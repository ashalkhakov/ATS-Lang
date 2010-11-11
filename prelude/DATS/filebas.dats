(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

//
// HX-2010-03-14: Here are some basic IO operations which
// are mostly for prototype implementation. If something more
// efficient is needed, please use the functions in declared
// in the following file: libc/SATS/stdio.sats
//

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no initialization is needed

(* ****** ****** *)

staload "prelude/SATS/filebas.sats"

(* ****** ****** *)

(*
//
// HX: implemented in [prelude/DATS/basics.dats]
//
implement file_mode_lte_r_r = file_mode_lte_refl {r} ()
implement file_mode_lte_w_w = file_mode_lte_refl {w} ()
implement file_mode_lte_rw_rw = file_mode_lte_refl {rw} ()
*)

(* ****** ****** *)

%{^

typedef struct {
  ats_char_type atslab_0 ; ats_ptr_type atslab_1 ;
} *charlst ; // end of [typedef]

ATSinline()
ats_ptr_type string_make_charlst_rev
  (ats_int_type sz, ats_ptr_type cs) {
  char *s0, *s ; charlst cs_next ;
  s0 = (char*)ATS_MALLOC (sz + 1) ; s = s0 + sz ;
  *s-- = '\0' ;
  while (cs != (charlst)0) {
    *s-- = ((charlst)cs)->atslab_0 ;
    cs_next = ((charlst)cs)->atslab_1 ;
    ATS_FREE (cs) ; cs = cs_next ;
  }  
  return s0 ;
} /* string_make_charlst_rev */

%} // end of [%{^]

(* ****** ****** *)

#define i2c char_of_int

macdef EOF = $extval (int, "EOF")

extern fun feof0 (f: FILEref):<> int = "atslib_feof"

extern
fun fgetc0_err (fil: FILEref):<> int = "atslib_fgetc_err"
// end of [fgetc0_err]

extern
fun fgetc1_err {m:file_mode} {l:addr}
  (pf_mod: file_mode_lte (m, r) | fil: &FILE m):<> int
  = "atslib_fgetc_err"
// end of [fgetc1_err]

extern
fun fclose0_exn (fil: FILEref):<!exn> void = "atslib_fclose_exn"
// end of [fclose0_exn]

extern
fun fclose1_exn {m:file_mode} {l:addr}
  (pf_fil: FILE m @ l | p_fil: ptr l):<!exn> void = "atslib_fclose_exn"
// end of [fclose1_exn]

(* ****** ****** *)

extern
fun string_make_charlst_rev {n:nat}
  (sz: int n, cs: list_vt (char, n)):<> String
  = "string_make_charlst_rev"
// end of [string_make_charlst_rev]

(* ****** ****** *)

// if the last character is '\n', it is dropped
implement input_line (fil) = let
  fun loop {n:nat} (
      fil: FILEref, n: int n, cs: list_vt (char, n)
    ) : Stropt = let
    val c = fgetc0_err (fil)
  in
    if c <> EOF then let
      val c = i2c c
    in
      if (c <> '\n') then
        loop (fil, n+1, list_vt_cons (c, cs))
      else 
        stropt_some (string_make_charlst_rev (n, cs))
      // end of [if]
    end else begin
      if n = 0 then let
        val+ ~list_vt_nil () = cs in stropt_none
      end else
        stropt_some (string_make_charlst_rev (n, cs))
      // end of [if]
    end  // end of [if]
  end // end of [loop]
in
  loop (fil, 0, list_vt_nil ())
end // end of [input_line]

(* ****** ****** *)

extern
fun fputc0_exn
  (c: char, fil: FILEref):<!exn> void = "atslib_fputc_exn"
// end of [fputc0_exn]

extern
fun fputs0_exn
  (str: string, fil: FILEref):<!exn> void = "atslib_fputs_exn"
// end of [fputs0_exn]

extern
fun fflush0_exn (fil: FILEref):<!exn> void = "atslib_fflush_exn"
// end of [fflush0_exn]

(* ****** ****** *)
//
// HX: the character '\n' is added at the end
//
implement output_line (fil, line) = begin
  fputs0_exn (line, fil); fputc0_exn ('\n', fil); fflush0_exn (fil)
end // end of [output_line]

(* ****** ****** *)

implement
char_stream_make_file
  (fil) = $delay (let
  val c = fgetc0_err (fil)
in
  if c <> EOF then let
    val c = i2c c in
    stream_cons (c, char_stream_make_file fil)
  end else begin
    let val () = fclose0_exn (fil) in stream_nil () end
  end // end of [if]
end : stream_con char
) (* end of [char_stream_make_file] *)

(* ****** ****** *)

implement
line_stream_make_file
  (fil) = $delay (let
  val line = $effmask_ref (input_line fil)
in
  if stropt_is_some line then let
    val line = stropt_unsome line in
    stream_cons (line, line_stream_make_file fil)
  end else let
    val () = fclose0_exn fil in stream_nil ()
  end // end of [if]
end : stream_con string // end of [let]
) (* end of [line_stream_make_file] *)

(* ****** ****** *)

implement
char_stream_vt_make_file
  {m} {l} (
  pf_mod, pf_fil | p_fil
) = $ldelay (
let
  val c = fgetc1_err (pf_mod | !p_fil)
in
  if c >= 0 then let // c <> EOF
    val c = char_of_int (c)
  in
    stream_vt_cons (
      c, char_stream_vt_make_file (pf_mod, pf_fil | p_fil)
    ) // end of [stream_vt_cons]
  end else let
    val () = fclose1_exn (pf_fil | p_fil) in stream_vt_nil ()
  end (* end of [if] *)
end : stream_vt_con char
,
fclose1_exn (pf_fil | p_fil) // HX: for cleanup
) // end of [char_stream_vt_make_file]

(* ****** ****** *)

implement
line_stream_vt_make_file
  {m} {l} (pf_mod, pf_fil | p_fil) = let
//
fun loop {n:nat} (
    pf_fil: FILE m @ l
  | p_fil: ptr l, n: int n, cs: list_vt (char, n)
  ) :<!laz> stream_vt_con (string) = let
  val c = fgetc1_err (pf_mod | !p_fil)
in
  if c >= 0 then let
    val c = char_of_int (c) // c <> EOF
  in
    if c <> '\n' then
      loop (pf_fil | p_fil, n+1, list_vt_cons (c, cs))
    else let
      val line = string_make_charlst_rev (n, cs) in
      stream_vt_cons (
        line, line_stream_vt_make_file (pf_mod, pf_fil | p_fil)
      ) // end of [stream_vt_cons]
    end // end of [if]
  end else let
    val () = fclose1_exn (pf_fil | p_fil)
  in
    if n > 0 then let
      val line = string_make_charlst_rev (n, cs)
    in
      stream_vt_cons (line, $ldelay stream_vt_nil)
    end else let
      val+ ~list_vt_nil () = cs in stream_vt_nil ()
    end // end of [if]
  end (* end of [if] *)
end (* end of [loop] *)
//
in
//
$ldelay (
loop (
  pf_fil | p_fil, 0, list_vt_nil ()
) // end of [loop]
,
fclose1_exn (pf_fil | p_fil) // HX: for cleanup
) // end of [$ldelay]
//
end // end of [char_stream_vt_make_file]

(* ****** ****** *)

%{$

ats_bool_type
atspre_test_file_exists
  (ats_ptr_type path) {
  int ret ;
  struct stat buf ;
  ret = stat ((char*)path, &buf) ;
  if (!ret) {
    return ats_true_bool ; // ret == 0
  } else {
    return ats_false_bool ; // ret == -1
  } // end of [if]
} /* test_file_exists */

ats_bool_type
atspre_test_file_isdir
  (ats_ptr_type path) {
  int ret ;
  struct stat buf ; mode_t mode ;
  ret = stat ((char*)path, &buf) ;
  if (!ret) { // ret == 0
    mode = buf.st_mode ;
    return (S_ISDIR(mode) ? ats_true_bool : ats_false_bool) ;
  } else { // ret == -1
    return ats_false_bool ;
  } // end of [if]
} /* atspre_test_file_dir */

%} // end of [%{$]

(* ****** ****** *)

(* end of [filebas.dats] *)
