(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Postiats - Unleashing the Potential of Types!
** Copyright (C) 2011-20?? Hongwei Xi, ATS Trustful Software, Inc.
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
** Free Software Foundation; either version 3, or (at  your  option)  any
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
//
// Author: Hongwei Xi (gmhwxi AT gmail DOT com)
// Start Time: July, 2011
//
(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/list.dats"
staload _(*anon*) = "prelude/DATS/list_vt.dats"
staload _(*anon*) = "prelude/DATS/reference.dats"

(* ****** ****** *)
//
staload
UN = "prelude/SATS/unsafe.sats"
//
staload
STDIO = "libc/SATS/stdio.sats"
sortdef fm = $STDIO.fm
stadef FILE_v = $STDIO.FILE_v
macdef SEEK_SET = $STDIO.SEEK_SET
macdef feof = $STDIO.feof
macdef fopen_err = $STDIO.fopen_err
macdef fclose_err = $STDIO.fclose_err
macdef fflush_err = $STDIO.fflush_err
macdef fgetc_err = $STDIO.fgetc_err
macdef fputc_err = $STDIO.fputc_err
macdef fseek_err = $STDIO.fseek_err
//
staload
FCNTL = "libc/SATS/fcntl.sats"
staload
STDLIB = "libc/SATS/stdlib.sats"
staload
UNISTD = "libc/SATS/unistd.sats"
staload
WAIT = "libc/sys/SATS/wait.sats"
//
(* ****** ****** *)

staload "libatsdoc/SATS/atsdoc_symbol.sats"
staload "libatsdoc/SATS/atsdoc_symmap.sats"

(* ****** ****** *)

staload "libatsdoc/SATS/atsdoc_text.sats"

(* ****** ****** *)

macdef neof (i) = (,(i) != EOF)

(* ****** ****** *)

local

staload
F = "libc/SATS/fcntl.sats"
stadef fildes_v = $F.fildes_v
staload S = "libc/SATS/stdlib.sats"
staload W = "libc/sys/SATS/wait.sats"
staload UNI = "libc/SATS/unistd.sats"

in // in of [local]

fn string2filename (
  pfx: string, content: string
) : strptr0 = let
  val tmp = sprintf ("%sXXXXXX", @(pfx))
  val [m,n:int] tmp = strbuf_of_strptr (tmp)
  prval () = __assert () where {
    extern prfun __assert (): [n >= 6] void
  }
  prval pfstr = tmp.1
  val (pfopt | fd) = $S.mkstemp !(tmp.2) // create it!
  prval () = tmp.1 := pfstr
in
//
if fd >= 0 then let
  prval $F.open_v_succ (pffil) = pfopt
  val [n:int] content = string1_of_string (content)
  val n = string1_length (content)
  val (pf, fpf | p) = __cast (content) where {
    extern castfn __cast {n:nat}
      (x: string n): [l:addr] (bytes(n)@l, bytes(n)@l -<lin,prf> void | ptr l)
    // end of [extern]
  }
  val () = $F.write_all_exn (pffil | fd, !p, n)
  prval () = fpf (pf)
  val () = $F.close_exn (pffil | fd)
in
  strptr_of_strbuf (tmp)
end else let
  prval $F.open_v_fail () = pfopt
  val () = strbufptr_free (tmp)
in
  strptr_null ()
end // end of [if]
//
end // end of [string2filename]

extern
fun file2strptr {fd:int} (
  pf: !fildes_v fd  | fd: int fd // [fd] is regular: no support for pipe
) : strptr0 = "atsdoc_file2strptr"
// end of [file2strptr]
%{$
ats_ptr_type
atsdoc_file2strptr
  (ats_int_type fd) {
  int err = 0 ;
  int nerr = 0 ;
  char* sbp = (char*)0 ;
//
  long int ofs_beg, ofs_end, nbyte ;
//
  ofs_beg = lseek (fd, 0L, SEEK_CUR) ;
  if (ofs_beg < 0) nerr += 1 ;
  ofs_end = lseek (fd, 0L, SEEK_END) ;
  if (ofs_end < 0) nerr += 1 ;
  ofs_beg = lseek (fd, ofs_beg, SEEK_SET) ;
  if (ofs_beg < 0) nerr += 1 ;
  nbyte = ofs_end - ofs_beg ;
//
  if (nerr == 0) { sbp = ATS_MALLOC(nbyte + 1) ; }
  if (sbp == NULL) nerr += 1 ;
//
  if (nerr == 0) {
    err = atslib_fildes_read_all_err (fd, sbp, nbyte) ;
  }
  if (err < 0) { nerr += 1 ; }
//
  if (nerr == 0) {
    sbp[ofs_end] = '\0'; return sbp ;
  }
//
  if (sbp) free (sbp) ; return NULL ;
} // end of [atsdoc_file2strptr]
%} // end of [%{$]

(* ****** ****** *)

implement{a}
tostring_fprint (prfx, fpr, x) = let
  val tmp = sprintf ("%sXXXXXX", @(prfx))
  val [m,n:int] tmp = strbuf_of_strptr (tmp)
  prval () = __assert () where {
    extern prfun __assert (): [n >= 6] void
  }
  prval pfstr = tmp.1
  val (pfopt | fd) = $S.mkstemp !(tmp.2) // create it!
  prval () = tmp.1 := pfstr
  val tmp = strptr_of_strbuf (tmp)
in
//
if fd >= 0 then let
  prval $F.open_v_succ (pffil) = pfopt
  val (fpf | out) = fdopen (pffil | fd, file_mode_w) where {
    extern fun fdopen {fd:nat} (
      pffil: !fildes_v fd | fd: int fd, mode: file_mode
    ) : (fildes_v fd -<lin,prf> void | FILEref) = "mac#fdopen"
  } // end of [out]
  val () = fpr (out, x)
  val _err = fflush_err (out)
  val _err = fseek_err (out, 0L, SEEK_SET)
  val res = file2strptr (pffil | fd)
  prval () = fpf (pffil)
  val _err = fclose_err (out)
  val _err = $UNI.unlink ($UN.castvwtp1 (tmp))
  val () = strptr_free (tmp)
in
  res (*strptr*)
end else let
  prval $F.open_v_fail () = pfopt; val () = strptr_free (tmp) in strptr_null ()
end // end of [if]
//
end // end of [tostring_fprint]

(* ****** ****** *)

implement
tostring_strsub (sub) = 
  tostring_fprint<string> ("atsdoc_tostring_strsub", fprint_strsub, sub)
// end of [tostring_strsub]

(* ****** ****** *)

implement
filename2text (path) = let
  val [l:addr] (pfopt | filp) = fopen_err (path, file_mode_r)
in
  if filp > null then let
    prval Some_v (pffil) = pfopt
    val filr = __cast (pffil | filp) where {
      extern castfn __cast {m:fm} (pffil: FILE_v (m, l) | p: ptr l): FILEref
    } // end of [val]
//
    fun fpr (out: FILEref, inp: FILEref): void = let
      val c = fgetc_err (inp)
    in
      if (c != EOF) then (fprint_char (out, (char_of_int)c); fpr (out, inp)) else ()
    end // end of [fpr]
//
    val res = tostring_fprint ("atsdoc_filename2text", fpr, filr)
    val _err = fclose_err (filr)
  in
    if strptr_isnot_null (res) then
      TEXTstrcst ((string_of_strptr)res)
    else let
      val () = strptr_free (res) in TEXTnil ()
    end (* end of [if] *)
  end else let
    prval None_v () = pfopt in TEXTnil ()
  end (* end of [if] *)
end // end of [filename2text]

(* ****** ****** *)

implement
atscode2xml_strcode
  (stadyn, code) = let
  val [l:addr] path =
    string2filename ("atsdoc_atscode2xmlinp", code)
  prval () = addr_is_gtez {l} ()
in
//
if strptr_isnot_null (path) then let
  val text = atscode2xml_filcode (stadyn, $UN.castvwtp1(path))
  val _err = $UNI.unlink ($UN.castvwtp1 (path))  
  val () = strptr_free (path)
in
  text
end else let
  val _null = strptr_free_null (path) in TEXTnil ()
end // end of [if]
//
end // end of [atscode2xml_strcode]

(* ****** ****** *)

extern
fun atsopt_get (): string = "atsopt_get"
implement
atsopt_get () = let
  val (fpf | x) = $S.getenv ("ATSHOME")
in
//
if strptr_isnot_null (x) then let
  val res = sprintf ("%s/bin/atsopt", @($UN.castvwtp1(x)))
  prval () = fpf (x)
in
  string_of_strptr (res)
end else let
  prval () = fpf (x) in "atsopt"
end (* end of [if] *)
//
end // end of [atsopt_get]

(* ****** ****** *)

extern
fun atscc_posmark_html_body_exec (
  stadyn: int, out: string, path: string
) : void = "atscc_posmark_html_body_exec"
// end of [atscc_posmark_html_body_exec]

(* ****** ****** *)

implement
atscode2xml_filcode
  (stadyn, path) = let
//
  val tmp = sprintf
    ("%sXXXXXX", @("atsdoc_atscode2xmlout"))
  // end of [val]
  val [m,n:int] tmp = strbuf_of_strptr (tmp)
  prval () = __assert () where {
    extern prfun __assert (): [n >= 6] void
  }
  prval pfstr = tmp.1
  val (pfopt | fd) = $S.mkstemp !(tmp.2) // create it!
  prval () = tmp.1 := pfstr
  val tmp = strptr_of_strbuf (tmp)
//
in
//
if fd >= 0 then let
  prval $F.open_v_succ (pffil) = pfopt
  val () = $F.close_exn (pffil | fd)
//
  val _tmp = $UN.castvwtp1(tmp)
  val cmd = lam (): void =<cloptr1>
    atscc_posmark_html_body_exec (stadyn, _tmp, path)
  // end of [val]
  val status = $UNI.fork_exec_and_wait_cloptr_exn (cmd)
//
  val status = int1_of_int (status)
  val (pf_ifexited | ifexited) = $W.WIFEXITED (status)
  val () = if ifexited then let
    val code = $W.WEXITSTATUS (pf_ifexited | status) in
    if code <> 0 then
      prerrf ("exit(ATS): [atscode2xml_filcode(%s)] failed.\n", @(path))
    // end of [if]
  end // end of [val]
  val () = if ~ifexited
    then prerr ("[atscode2xml_filcode] is sigtermed\n")
  // end of [if]
//
  val (pffil | fd) = $F.open_flag_exn ($UN.castvwtp1(tmp), $F.O_RDONLY)
  val res = file2strptr (pffil | fd)
  val () = $F.close_exn (pffil | fd)
  val _err = $UNI.unlink ($UN.castvwtp1(tmp)) // HX: may save it for debugging
  val () = strptr_free (tmp)
in
  if strptr_isnot_null (res) then let
    val res = string_of_strptr (res) in TEXTstrcst (res)
  end else let
    val () = strptr_free (res) in TEXTnil ()
  end // end of [if]
end else let
  prval $F.open_v_fail () = pfopt; val () = strptr_free (tmp) in TEXTnil ()
end // end of [if]
//
end // end of [atscode2xml_filcode]

end // end of [local]

(* ****** ****** *)

%{$

ats_void_type
atscc_posmark_html_body_exec (
  ats_int_type stadyn, ats_ptr_type out, ats_ptr_type path
) {
  ats_ptr_type atsopt = atsopt_get () ;
  char *stadynflg = (stadyn > 0 ? "--dynamic" : "--static") ;
  int err = execl (
    (char*)atsopt
  , (char*)atsopt
  , "--posmark_html_body"
  , "--output", out, stadynflg, (char*)path
  , (char*)0
  ) ; // end of [execl]
  if (err < 0) perror("atscc_posmark_html_body: [execl] failed: ") ;
  exit(EXIT_FAILURE) ;
} // end of [atscc_posmark_html_body]

%} // end of [%{$]

(* ****** ****** *)

local

viewtypedef
textmap = symmap (text)

val map0 = symmap_make_nil ()
val theTextMap = ref<textmap> (map0)

in // in of [local]

implement
theTextMap_search (s) = let
  val (vbox pf | p) = ref_get_view_ptr (theTextMap)
in
  symmap_search (!p, s)
end // end of [theTextMap_search]

implement
theTextMap_insert (s, x) = let
  val (vbox pf | p) = ref_get_view_ptr (theTextMap)
in
  symmap_insert (!p, s, x)
end // end of [theTextMap_insert]

implement
theTextMap_insert_str (s, x) = let
  val s = symbol_make_string (s) in theTextMap_insert (s, x)
end // end of [theTextMap_insert_str]

end // end of [local]

(* ****** ****** *)

implement
fprint_text (out, x) =
  case+ x of
//
  | TEXTnil () => ()
//
  | TEXTstrcst (str) => fprint_string (out, str)
  | TEXTstrsub (sub) => fprint_strsub (out, sub)
//
  | TEXTapptxt2 (x1, x2) => {
      val () = fprint_text (out, x1)
      val () = fprint_text (out, x2)
    }
  | TEXTappstr2 (x1, x2) => {
      val () = fprint_string (out, x1)
      val () = fprint_string (out, x2)
    }
//
  | TEXTapptxt3 (x1, x2, x3) => {
      val () = fprint_text (out, x1)
      val () = fprint_text (out, x2)
      val () = fprint_text (out, x3)
    }
  | TEXTappstr3 (x1, x2, x3) => {
      val () = fprint_string (out, x1)
      val () = fprint_string (out, x2)
      val () = fprint_string (out, x3)
    }
//
  | TEXTcontxt (xs) => fprint_textlst (out, xs)
  | TEXTcontxtsep (xs, sep) => fprint_textlst_sep (out, xs, sep)
//
(* end of [fprint_text] *)

implement
fprint_textlst (out, xs) =
  list_app_cloptr<text> (xs, lam (x) =<1> fprint_text (out, x))
// end of [fprint_textlst]

implement
fprint_textlst_sep (out, xs, sep) = let
  fun loop (xs: textlst, i: int):<cloref1> void =
    case+ xs of
    | list_cons (x, xs) => let
        val () = if i > 0 then fprint_text (out, sep)
        val () = fprint_text (out, x)
      in
        loop (xs, i+1)
      end // end of [list_cons]
    | list_nil () => ()
  // end of [loop]
in
  loop (xs, 0)
end // end of [fprint_textlst]

(* ****** ****** *)

staload "libatsdoc/SATS/atsdoc_reader.sats"
extern fun fprsub_reader (out: FILEref, inp: &reader): void

(* ****** ****** *)

local

#define i2c char_of_int

fun
IDENTFST_test
  (c: char): bool = case+ 0 of
  | _ when ('a' <= c andalso c <= 'z') => true
  | _ when ('A' <= c andalso c <= 'Z') => true
  | _ when c = '_' => true
  | _ => false
// end of [IDENTFST_test]

fun
IDENTRST_test
  (c: char): bool = case+ 0 of
  | _ when ('a' <= c andalso c <= 'z') => true
  | _ when ('A' <= c andalso c <= 'Z') => true
  | _ when ('0' <= c andalso c <= '9') => true
  | _ when c = '_' => true
  | _ when c = '\'' => true
  | _ => false
// end of [IDENTRST_test]

fun ident_get (
  inp: &reader, cur: &int
) : List_vt (char) = let
  val fst = (i2c)cur
in
  if IDENTFST_test (fst) then let
    val () = cur := reader_get_char (inp)
  in
    list_vt_cons (fst, identrst_get (inp, cur))
  end else
    list_vt_nil ()
  // end of [if]
end // end of [ident_get]

and identrst_get (
  inp: &reader, cur: &int
) : List_vt (char) = let
  val fst = (i2c)cur
in
  if IDENTRST_test (fst) then let
    val () = cur := reader_get_char (inp)
  in
    list_vt_cons (fst, identrst_get (inp, cur))
  end else
    list_vt_nil ()
  // end of [if]
end // end of [identrst_get]

fun fprsub_ident {n:nat} (
  out: FILEref, cs: list_vt (char, n)
) : void = let
  val n = list_vt_length (cs)
in
  if n > 0 then let
    val id = string_make_list_int ($UN.castvwtp1(cs), n)
    val id = string_of_strbuf (id)
    val sym = symbol_make_string (id)
    val ans = theTextMap_search (sym)
    val () = (case+ ans of
      | ~Some_vt xt => fprint_text (out, xt)
      | ~None_vt () => fprintf (out, "#%s$", @(id))
    ) // end of [val]
//
    val () = list_vt_free<char> (cs)
//
  in
    // nothing
  end else let
    val ~list_vt_nil () = cs in (*nothing*)
  end // end of [if]
end // end of [fprsub_ident]

in // in of [local]

implement
fprsub_reader (out, inp) = let
//
fun aux (
  out: FILEref, inp: &reader, cur: &int
) : void =
  if neof(cur) then let
    val fst = (i2c)cur
    val () = cur := reader_get_char (inp)
  in 
    case+ fst of
    | '#' => aux_SHARP (out, inp, cur)
    | '\\' => aux_SLASH (out, inp, cur)
    | _ => let
        val err = fputc_err (fst, out) in aux (out, inp, cur)
      end // end of [_]
  end // end of [if]
//
and aux_SHARP (
  out: FILEref, inp: &reader, cur: &int
) : void =
  if neof(cur) then let
    val cs = ident_get (inp, cur)
    val dlrend = (cur = int_of('$')): bool
//
    fun loop (out: FILEref, cs: List_vt (char)): void =
      case+ cs of
      | ~list_vt_cons (c, cs) => let
          val err = fputc_err (c, out) in loop (out, cs)
        end // end of [list_cons]
      | ~list_vt_nil () => ()
    (* end of [loop] *)
//
    val () =
      if dlrend then let
        val () = cur := reader_get_char (inp)
      in
        fprsub_ident (out, cs) // valid identifier
      end else let
        val err = fputc_err ('#', out) in loop (out, cs)
      end // end of [if]
    // end of [val]
  in
    aux (out, inp, cur)
  end else let
    val err = fputc_err ('#', out) in (*nothing*)
  end // end of [if]
//
and aux_SLASH (
  out: FILEref, inp: &reader, cur: &int
) : void =
  if neof(cur) then let
    val fst = (i2c)cur
    val () = cur := reader_get_char (inp)
  in
    case+ fst of
    | '#' => let
        val err = fputc_err ('#', out) in aux (out, inp, cur)
      end // end of ['#']
    | '\\' => let
        val err = fputc_err ('\\', out) in aux (out, inp, cur)
      end // end of ['\\']
    | _ => let
        val err = fputc_err ('\\', out); val err = fputc_err (fst, out)
      in
        aux (out, inp, cur)
      end // end of [_]
  end else let
    val err = fputc_err ('\\', out) in (*nothing*)
  end // end of [if]
//
  var cur: int
  val () = cur := reader_get_char (inp)
in
  aux (out, inp, cur)
end // end of [fprsub_reader]

end // end of [local]

(* ****** ****** *)

implement
fprint_strsub
  (out, sub) = let
  var inp: reader
  val () = reader_initialize_string (inp, sub)
  val () = fprsub_reader (out, inp)
  val () = reader_uninitialize (inp)
in
  // nothing
end // end of [fprint_strsub]

(* ****** ****** *)

implement
fprint_filsub (out, path) = let
  val (pfopt | filp) = fopen_err (path, file_mode_r)
in
//
if filp > null then let
  prval Some_v (pffil) = pfopt
  var inp: reader
  val () = reader_initialize_filp (file_mode_lte_r_r, pffil | inp, filp)
  val () = fprsub_reader (out, inp)
  val () = reader_uninitialize (inp)
in
  // nothing
end else let
  prval None_v () = pfopt
  val () = fprintf (stderr_ref, "fprint_filsub: [%s] cannot be opened!\n", @(path))
  val () = exit (1)
in
  // nothing
end // end of [if]
//
end // end of [

(* ****** ****** *)

(* end of [atsdoc_text.dats] *)
