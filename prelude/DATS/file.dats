(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
 * ATS - Unleashing the Potential of Types!
 *
 * Copyright (C) 2002-2008 Hongwei Xi, Boston University
 *
 * All rights reserved
 *
 * ATS is free software;  you can  redistribute it and/or modify it under
 * the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
 * Free Software Foundation; either version 2.1, or (at your option)  any
 * later version.
 * 
 * ATS is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
 * for more details.
 * 
 * You  should  have  received  a  copy of the GNU General Public License
 * along  with  ATS;  see the  file COPYING.  If not, please write to the
 * Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
 *
 *)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

// some basic IO operations

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no initialization is needed

(* ****** ****** *)

staload "prelude/SATS/file.sats"

(* ****** ****** *)

implement file_mode_lte_r_r = file_mode_lte_refl {r} ()
implement file_mode_lte_w_w = file_mode_lte_refl {w} ()

(* ****** ****** *)

%{^

typedef struct {
ats_char_type atslab_0 ; ats_ptr_type atslab_1 ;
} *charlst ;

static inline
ats_ptr_type string_make_charlst_rev
  (ats_int_type sz, ats_ptr_type cs) {
  char *s0, *s ; charlst cs_next ;
  s0 = (char*)ats_malloc_gc (sz + 1) ; s = s0 + sz ;
  *s = '\0' ; --s ;
  while (cs != (charlst)0) {
    *s = ((charlst)cs)->atslab_0 ; --s ;
    cs_next = ((charlst)cs)->atslab_1 ;
    ats_free_gc (cs) ; cs = cs_next ;
  }  
  return s0 ;
} /* string_make_charlst_rev */

%}

(* ****** ****** *)

#define i2c char_of_int

macdef EOF = $extval (int, "EOF")

extern fun feof0 (f: FILEref):<> int = "atslib_feof"
extern fun fgetc0_err (f: FILEref):<> int = "atslib_fgetc_err"
extern fun fclose0_exn (f: FILEref):<> void = "atslib_fclose_exn"

extern fun string_make_charlst_rev {n:nat}
  (sz: int n, cs: list_vt (char, n)):<> string
  = "string_make_charlst_rev"

(* ****** ****** *)

// if the last character is '\n', it is dropped
implement input_line (fil) = let
  fun loop {n:nat}
    (fil: FILEref, n: int n, cs: list_vt (char, n))
    : string = let
      val c = fgetc0_err (fil)
    in
      if c <> EOF then let
        val c = i2c c
      in
        if (c <> '\n') then loop (fil, n+1, list_vt_cons (c, cs))
        else string_make_charlst_rev (n, cs)
      end else begin
        string_make_charlst_rev (n, cs)
      end  // end of [if]
    end // end of [loop]
in
  loop (fil, 0, list_vt_nil ())
end // end of [input_line]

(* ****** ****** *)

extern fun fputc0_exn (c: char, fil: FILEref): void
  = "atslib_fputc_exn"

extern fun fputs0_exn (str: string, fil: FILEref): void
  = "atslib_fputs_exn"

extern fun fflush0_exn (fil: FILEref): void
  = "atslib_fflush_exn"

// the character '\n' is added at the end
implement output_line (fil, line) = begin
  fputs0_exn (line, fil); fputc0_exn ('\n', fil); fflush0_exn (fil)
end // end of [output_line]

(* ****** ****** *)

implement char_stream_make_file (fil) = let
  val c = fgetc0_err (fil)
in
  if c <> EOF then let
    val c = i2c c
  in
    $delay ($effmask_ref (stream_cons (c, char_stream_make_file fil)))
  end else begin
    let val () = fclose0_exn (fil) in $delay (stream_nil ()) end
  end // end of [if]
end // end of [char_stream_make_file]

(* ****** ****** *)

implement line_stream_make_file (fil) = begin
  if feof0 (fil) <> 0 then let
    val () = fclose0_exn fil in $delay (stream_nil ())
  end else let
    val l = input_line (fil)
  in
    $delay ($effmask_ref (stream_cons (l, line_stream_make_file fil)))
  end // end of [if]
end // end of [line_stream_make_file]

(* ****** ****** *)

%{$

ats_bool_type
atspre_test_file_exists (ats_ptr_type path) {
  int ret ;
  struct stat buf ;

  ret = stat ((char*)path, &buf) ;

  if (!ret) {
    return ats_true_bool ; // ret == 0
  } else {
    return ats_false_bool ; // ret == -1
  }
} /* test_file_exists */

ats_bool_type
atspre_test_file_isdir (ats_ptr_type path) {
  int ret ;
  struct stat buf ; mode_t mode ;

  ret = stat ((char*)path, &buf) ;

  if (!ret) { // ret == 0
    mode = buf.st_mode ;
    return S_ISDIR(mode) ? ats_true_bool : ats_false_bool ;
  } else { // ret == -1
    return ats_false_bool ;
  }
} /* atspre_test_file_dir */

%}

(* ****** ****** *)

(* end of [file.dats] *)
