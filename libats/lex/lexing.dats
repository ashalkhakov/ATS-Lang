(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Anairiats - Unleashing the Potential of Types!
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: July 2007
//
(* ****** ****** *)

%{^

#include "libc/CATS/stdio.cats"

%} // end of [%{^]

(* ****** ****** *)

staload "libats/lex/lexing.sats"
staload "libats/lex/unicode.sats"

(* ****** ****** *)

assume
position_t = '{
  line= int, loff= int, toff= lint
} // end of [position_t]

implement position_line (p) = p.line
implement position_loff (p) = p.loff
implement position_toff (p) = p.toff

implement lt_position_position (p1, p2) = p1.toff < p2.toff
implement lte_position_position (p1, p2) = p1.toff <= p2.toff
implement eq_position_position (p1, p2) = p1.toff = p2.toff
implement neq_position_position (p1, p2) = p1.toff <> p2.toff

extern fun position_make_int_int_lint
  (line: int, loff: int, toff: lint): position_t
  = "position_make_int_int_lint"

implement
position_make_int_int_lint
  (line, loff, toff) = '{
  line= line, loff= loff, toff= toff
} // end of [position_make_int_int_lint]

implement
fprint_position
  (pf | fil, pos) = fprintf1_exn (
  pf | fil, "%li(line=%i, offs=%i)", @(pos.toff+1L, pos.line+1, pos.loff+1)
) // end of [fprint_position]

implement print_position (pos) = print_mac (fprint_position, pos)
implement prerr_position (pos) = prerr_mac (fprint_position, pos)

(* ****** ****** *)

assume infile_t (v:view) = '{
  free= (v | (*none*)) -<cloptr1> void
, getc= (!v | (*none*)) -<cloptr1> int
} // end of [infile_t]

%{

// TODO: when compiler support is available, remove
typedef uint32_t char32_t ;

typedef struct {
  char32_t *buf_ptr ;
  int buf_size ;
  ats_ptr_type infile ;
  int fstpos ;
  int fstpos_line ; // line number
  int fstpos_loff ; // line offset
  long int fstpos_toff ; // total offset
  int lstpos ;
  int lstpos_line ; // line number
  int lstpos_loff ; // line offset
  long int lstpos_toff ; // total offset
  int curpos ;
  int curpos_line ; // line number
  int curpos_loff ; // line offset
  long int curpos_toff ; // total offset
  int endpos ;
} lexbuf ; // end of [typedef]

%}

(* ****** ****** *)

implement
infile_free (pf | infil) = let
  val '{free= free, getc= getc} = infil
  val () = free (pf | (*none*))
  val () = cloptr_free (getc)
  val () = cloptr_free (free)
in
  // nothing
end // end of [infile_free]

implement
infile_getc (pf | infil) = infil.getc (pf | (*none*))

(* ****** ****** *)

// FIXME: untested!
implement
infile_make_string
  (inp) = let
//
  val [n:int] str =
    string1_of_string (inp)
  // end of [val]
  val n = string_length (str)
//
  typedef T = @(sizeLte n, int, utf8_state)
  val [l:addr] (pf_gc, pf_at | p) = ptr_alloc_tsz {T} (sizeof<T>)
  viewdef V = (free_gc_v (T?, l), T @ l)
//
  fn _free (
    pf: V | (*none*)
  ) :<cloptr1> void = begin
     ptr_free {T?} (pf.0, pf.1 | p)
  end // end of [_free]
//
  fn _getc (
    pf: !V | (*none*)
  ) :<cloptr1> int = let
    prval pf_at = (pf.1: T @ l)
    fun loop {i:nat | i <= n} .<n-i>. (
      i: size_t i, p: &T, str: string n, n: size_t n
    ) :<> int =
      if i < n then let
	val res = utf8_decode_step (p.2, p.1, str[i])
      in
	case+ 0 of
	| _ when utf8_state_is_accept res => (p.0 := i+1; p.1)
	| _ when utf8_state_is_reject res => ~1 (*FIXME: more informative error*)
  	| _ => loop (i+1, p, str, n)
      end else ~1
    // end of [loop]
    val ans = loop (!p.0, !p, str, n)
  in
    pf.1 := pf_at; ans
  end // end of [_getc]
//
  val () = !p.0 := size1_of_int1 (0)
  val () = !p.1 := 0
  val () = !p.2 := UTF8init
//
in #[
  V | ( @(pf_gc, pf_at) | '{ free= _free, getc= _getc } )
] end // end of [infile_make_string]

(* ****** ****** *)

// FIXME: untested!
implement
infile_make_strptr
  (inp) = let
  stavar l1:addr; val inp = inp: strptr l1
  val [m,n:int] (pf_free, pf_sb | p_sb) = strbuf_of_strptr inp
  val n = strbuf_length (!p_sb)
//
  typedef T = @(sizeLte n, int, utf8_state)
  val [l2:addr] (pf_gc, pf_at | p) = ptr_alloc_tsz {T} (sizeof<T>)
//
  viewdef V = (
    freebyte_gc_v (m, l1), strbuf_v (m, n, l1), free_gc_v (T?, l2), T @ l2
  ) // end of [V]
  fn _free (
    pf: V | (*none*)
  ) :<cloptr1> void = let
    val () = strbufptr_free @(pf.0, pf.1 | p_sb)
  in
    ptr_free {T?} (pf.2, pf.3 | p)
  end // end of [_free]
//
  fn _getc (
    pf: !V | (*none*)
  ) :<cloptr1> int = let
    prval pf1_at = pf.1 
    prval pf2_at = pf.3
    fun loop {i:nat | i <= n} .<n-i>. (
      i: size_t i, p: &T, sb: &strbuf (m, n)
    ) :<> int =
      if ~strbuf_is_atend (sb, i) then let
	val c = strbuf_get_char_at (sb, i)
	val res = utf8_decode_step (p.2, p.1, c)
      in
	case+ 0 of
	| _ when utf8_state_is_accept res => (p.0 := i+1; p.1)
	| _ when utf8_state_is_reject res => ~1 (*FIXME: more informative error*)
  	| _ => loop (i+1, p, sb)
      end else ~1
    // end of [loop]
    val ans = loop (!p.0, !p, !p_sb)
  in
    pf.1 := pf1_at; pf.3 := pf2_at; ans
  end // end of [_getc]
//
  val () = !p.0 := size1_of_int1 (0)
  val () = !p.1 := 0
  val () = !p.2 := UTF8init
//
in #[
  V | ( @(pf_free, pf_sb, pf_gc, pf_at) | '{ free= _free, getc= _getc } )
] end // end of [infile_make_strptr]

(* ****** ****** *)

implement
lexbuf_curpos_prerr (lb) = lexbuf_curpos_fprint (stderr_ref, lb)

(* ****** ****** *)

local
//
// staload "libc/SATS/stdio.sats"
//
extern
fun fclose_exn
  {m:file_mode} {l:addr}
  (pf: FILE m @ l | p: ptr l): void = "mac#atslib_fclose_exn"
// end of [fclose_exn]

in // in of [local]

implement
infile_make_file
  {m} {l} (
  pf_fil, pf_mod | fil
) = let
  viewdef V = FILE m @ l
  fn _free (
    pf_fil: V | (*none*)
  ) :<cloptr1> void = fclose_exn (pf_fil | fil)
  fn _getc (
    pf_fil: !V | (*none*)
  ) :<cloptr1> int = utf8_decode (pf_mod, pf_fil | fil)
in #[
  V | (pf_fil | '{ free= _free, getc= _getc })
] end // end of [infile_make_file]

implement
infile_make_stdin () = let
  viewdef V = unit_v
  fn _free (pf: V | (*none*)):<cloptr1> void = () where {
     prval unit_v () = pf
  } // end of [_free]
  fn _getc (pf: !V | (*none*)):<cloptr1> int = let
    val (pf_stdin | ()) = stdin_view_get ()
    val res = utf8_decode (file_mode_lte_r_r, pf_stdin | stdin)
    val () = stdin_view_set (pf_stdin | (*empty*))
  in
    res
  end // end of [_getc]
in
  #[ V | (unit_v () | '{ free= _free, getc= _getc } ) ]
end // end of [infile_make_stdin]

end // end of [local]

(* ****** ****** *)

implement
lexing_engine_lexbuf
  (lxbf, transtbl, acctbl) = let
//
fun aux (
  lxbf: &lexbuf_t, irule: &int, state: int
) :<cloptr1> int =
  if state > 0 then let
    val irule_new = accept_table_get (acctbl, state)
(*
    val () = printf ("lexing_engine_lexbuf: aux: state = %i\n", @(state))
    val () = printf ("lexing_engine_lexbuf: aux: irule_new = %i\n", @(irule))
*)
    val () = if irule_new > 0 then begin
      lexbuf_lstpos_set (lxbf); irule := irule_new
    end // end of [val]
    val c: int = lexbuf_char_next (lxbf) // c >= -1
(*
    val () = printf ("lexing_engine_lexbuf: c = %i\n", @(c))
*)
    val state = transition_table_get (transtbl, state, c)
(*
    val () = printf ("lexing_engine_lexbuf: state = %i\n", @(state))
*)
  in
    aux (lxbf, irule, state)
  end else begin
    lexbuf_curpos_set (lxbf);
(*
    printf ("lexing_engine_lexbuf: end: irule = %i\n", @(irule));
*)
    irule
  end (* end of [if] *)
// end of [aux]
//
var irule = (0: int)
//
in
//
lexbuf_fstpos_set (lxbf); aux (lxbf, irule, 1)
//
end // end of [lexing_engine_lexbuf]

implement
lexing_engine (transtbl, acctbl) = let
  val (pf_lexbuf | lexbuf) = lexing_lexbuf_get ()
  val irule = lexing_engine_lexbuf (!lexbuf, transtbl, acctbl)
in
  lexing_lexbuf_set (pf_lexbuf | lexbuf); irule
end // end of [lexing_engine]

(* ****** ****** *)

implement
lexeme_get (i) = let
  val (
    pf_lexbuf | lexbuf
  ) = lexing_lexbuf_get ()
  val c = lexeme_get_lexbuf (!lexbuf, i)
in
  lexing_lexbuf_set (pf_lexbuf | lexbuf); c
end // end of [lexeme_get]

implement
lexeme_set (i, c) = let
  val (
    pf_lexbuf | lexbuf
  ) = lexing_lexbuf_get ()
  val () = lexeme_set_lexbuf (!lexbuf, i, c)
in
  lexing_lexbuf_set (pf_lexbuf | lexbuf)
end // end of [lexeme_set]

(* ****** ****** *)

implement
lexeme_strptr () = let
  val (
    pf_lexbuf | lexbuf
  ) = lexing_lexbuf_get ()
  val res = lexeme_strptr_lexbuf (!lexbuf)
  val () = lexing_lexbuf_set (pf_lexbuf | lexbuf); 
in
  res
end // end of [lexeme_strptr]
implement
lexeme_string () = let
  val res = lexeme_strptr () in string_of_strptr (res)
end // end of [lexeme_string]

(* ****** ****** *)

implement
lexing_is_eof () = let
  val (
    pf_lexbuf | lexbuf
  ) = lexing_lexbuf_get ()
  val b = lexbuf_is_eof (!lexbuf)
in
  lexing_lexbuf_set (pf_lexbuf | lexbuf); b
end // end of [lexing_is_of]

(* ****** ****** *)

implement
lexing_error () = $raise LexingErrorException

(* ****** ****** *)

val () = let // initialization
  val () = markroot () where {
    extern fun markroot (): void = "lexing_lexbuf_markroot"
  } // end of [val]
in
  // empty
end // end of [val]

(* ****** ****** *)

%{$

ats_ptr_type
lexbuf_fstpos_get (
  ats_ptr_type lxbf0
) {
  lexbuf *lxbf = (lexbuf*)lxbf0 ;
  return position_make_int_int_lint
    (lxbf->fstpos_line, lxbf->fstpos_loff, lxbf->fstpos_toff) ;
} // end of [lexbuf_fstpos_get]

ats_void_type
lexbuf_fstpos_set (
  ats_ptr_type lxbf0
) {
  lexbuf *lxbf = (lexbuf*)lxbf0 ;
  lxbf->fstpos = lxbf->curpos ;
  lxbf->fstpos_line = lxbf->curpos_line ;
  lxbf->fstpos_loff = lxbf->curpos_loff ;
  lxbf->fstpos_toff = lxbf->curpos_toff ;
  return ;
} // end of [lexbuf_fstpos_set]

//

ats_ptr_type
lexbuf_lstpos_get (
  ats_ptr_type lxbf0
) {
  lexbuf *lxbf = (lexbuf*)lxbf0 ;
  return position_make_int_int_lint
    (lxbf->lstpos_line, lxbf->lstpos_loff, lxbf->lstpos_toff) ;
} // end of [lexbuf_lstpos_get]

ats_void_type
lexbuf_lstpos_set (
  ats_ptr_type lxbf0
) {
  lexbuf *lxbf = (lexbuf*)lxbf0 ;
  lxbf->lstpos = lxbf->curpos ;
  lxbf->lstpos_line = lxbf->curpos_line ;
  lxbf->lstpos_loff = lxbf->curpos_loff ;
  lxbf->lstpos_toff = lxbf->curpos_toff ;
  return ;
} // end of [lexbuf_lstpos_set]

//

ats_ptr_type
lexbuf_curpos_get (
  ats_ptr_type lxbf0
) {
  lexbuf *lxbf = (lexbuf*)lxbf0 ;
  return position_make_int_int_lint
    (lxbf->curpos_line, lxbf->curpos_loff, lxbf->curpos_toff) ;
} // end of [lexbuf_curpos_get]

ats_void_type
lexbuf_curpos_set (
  ats_ptr_type lxbf0
) {
  lexbuf *lxbf ;
  lxbf = (lexbuf*)lxbf0 ;
  lxbf->curpos = lxbf->lstpos ;
  lxbf->curpos_line = lxbf->lstpos_line ;
  lxbf->curpos_loff = lxbf->lstpos_loff ;
  lxbf->curpos_toff = lxbf->lstpos_toff ;
  return ;
} // end of [lexbuf_curpos_set]

//

ats_int_type
lexbuf_size_get (
  ats_ptr_type lxbf0
) {
  int sz ; lexbuf *lxbf ;
  lxbf = (lexbuf*)lxbf0;
  sz = lxbf->lstpos - lxbf->fstpos ;
  if (sz < 0) { sz += lxbf->buf_size ; }
  return sz ;
} // end of [lexbuf_size_get]

/* ****** ****** */

#define BUF_SIZE 1024
#define BUF_RESIZE 256

/* ****** ****** */

ats_void_type
lexbuf_resize (lexbuf *lxbf) {
  int fstpos, curpos, lstpos, endpos ;
  int buf_size, buf_size_new ;
  char32_t *buf_ptr, *buf_ptr_new;
/*
  fprintf (stdout, "lexbuf_resize: before: buf_size = %i\n", lxbf->buf_size) ;
  fprintf (stdout, "lexbuf_resize: before: fstpos = %i\n", lxbf->fstpos) ;
  fprintf (stdout, "lexbuf_resize: before: curpos = %i\n", lxbf->curpos) ;
  fprintf (stdout, "lexbuf_resize: before: lstpos = %i\n", lxbf->lstpos) ;
  fprintf (stdout, "lexbuf_resize: before: endpos = %i\n", lxbf->endpos) ;
*/
  buf_ptr = lxbf->buf_ptr ;
  buf_size = lxbf->buf_size ;
  fstpos = lxbf->fstpos ;
  endpos = lxbf->endpos ;

  buf_size_new = buf_size + buf_size ;
  buf_ptr_new = ATS_MALLOC (sizeof(char32_t)*buf_size_new) ;

  lxbf->buf_ptr = buf_ptr_new ;
  lxbf->buf_size = buf_size_new ;

  lxbf->fstpos = 0 ;

  if (fstpos <= endpos) {
    memcpy(buf_ptr_new, buf_ptr+sizeof(char32_t)*fstpos, sizeof(char32_t)*(endpos-fstpos)) ;
    lxbf->endpos = endpos - fstpos ;
  } else {
    memcpy(buf_ptr_new, buf_ptr+sizeof(char32_t)*fstpos, sizeof(char32_t)*(buf_size-fstpos)) ;
    memcpy(buf_ptr_new+sizeof(char32_t)*(buf_size-fstpos), buf_ptr, sizeof(char32_t)*endpos) ;
    lxbf->endpos = buf_size + endpos - fstpos ;
  }

  curpos = lxbf->curpos ;

  if (fstpos <= curpos) {
    lxbf->curpos = curpos - fstpos ;
  } else {
    lxbf->curpos = buf_size + curpos - fstpos ;
  }

  lstpos = lxbf->lstpos ;

  if (fstpos <= lstpos) {
    lxbf->lstpos = lstpos - fstpos ;
  } else {
    lxbf->lstpos = buf_size + lstpos - fstpos ;
  }

/*
  fprintf (stdout, "lexbuf_resize: after: buf_size = %i\n", lxbf->buf_size) ;
  fprintf (stdout, "lexbuf_resize: after: fstpos = %i\n", lxbf->fstpos) ;
  fprintf (stdout, "lexbuf_resize: after: curpos = %i\n", lxbf->curpos) ;
  fprintf (stdout, "lexbuf_resize: after: lstpos = %i\n", lxbf->lstpos) ;
  fprintf (stdout, "lexbuf_resize: after: endpos = %i\n", lxbf->endpos) ;
*/

  ATS_FREE (buf_ptr) ;
} // end of [lexbuf_resize]

ats_void_type
lexbuf_resize_if
  (lexbuf *lxbf) {
//
  int fstpos, endpos ;
//
/*
  fprintf (stdout, "lexbuf_resize_if: buf_size = %i\n", lxbf->buf_size) ;
  fprintf (stdout, "lexbuf_resize_if: fstpos = %i\n", lxbf->fstpos) ;
  fprintf (stdout, "lexbuf_resize_if: curpos = %i\n", lxbf->curpos) ;
  fprintf (stdout, "lexbuf_resize_if: lstpos = %i\n", lxbf->lstpos) ;
  fprintf (stdout, "lexbuf_resize_if: endpos = %i\n", lxbf->endpos) ;
*/
//
  fstpos = lxbf->fstpos ;
  endpos = lxbf->endpos ;
//
  if (fstpos <= endpos) {
    if (endpos - fstpos + BUF_RESIZE > lxbf->buf_size) {
      lexbuf_resize(lxbf) ;
    }
  } else {
    if (endpos + BUF_RESIZE >= fstpos) {
      lexbuf_resize (lxbf) ;
    }
  } // end of [if]
//
  return ;
} // end of [lexbuf_resize_if]

ats_void_type
lexbuf_refill (
  ats_ptr_type lxbf0
) {
  lexbuf *lxbf ;
  char32_t *buf_ptr ;
  int c, fstpos, curpos, endpos ;

  lxbf = (lexbuf*)lxbf0 ;

  lexbuf_resize_if (lxbf) ;

  buf_ptr = lxbf->buf_ptr ;
  fstpos = lxbf->fstpos ;
  endpos = lxbf->endpos ;
/*
  fprintf (stdout, "lexbuf_refill: fstpos = %i\n", fstpos) ;
  fprintf (stdout, "lexbuf_refill: endpos = %i\n", endpos) ;
*/
  if (fstpos <= endpos) {
//
    while (endpos+1 < lxbf->buf_size) {
      c = lexing_infile_getc (lxbf->infile) ;
      if (c < 0) { lxbf->endpos = endpos ; return ; }
      buf_ptr[endpos] = c; ++endpos ;
    } // end of [while]
//
    if (fstpos == 0) { lxbf->endpos = endpos ; return ; }
//
    c = lexing_infile_getc (lxbf->infile) ;
    if (c < 0) { lxbf->endpos = endpos ; return ; }
    buf_ptr[endpos] = c; endpos = 0;
//
  } /* end of [if] */
  //
  while (endpos+1 < fstpos) {
    c = lexing_infile_getc (lxbf->infile) ;
    if (c < 0) { lxbf->endpos = endpos ; return ; }
    buf_ptr[endpos] = c; ++endpos ;
  } /* end of [while] */
//
  lxbf->endpos = endpos ;
//
  return ;
//
} /* end of [lexbuf_refill] */

/* ****** ****** */

ats_void_type
lexbuf_curpos_next (
  lexbuf *lxbf, int c
) {
//
  int curpos1 = lxbf->curpos + 1 ;
//
  if (curpos1 < lxbf->buf_size) {
    lxbf->curpos = curpos1;
  } else {
    lxbf->curpos = 0;
  } /* end of [if] */
//
  if (c == '\n') {
    lxbf->curpos_line += 1; lxbf->curpos_loff = 0; lxbf->curpos_toff += 1 ;
  } else {
    lxbf->curpos_loff += 1 ; lxbf->curpos_toff += 1 ;
  } /* end of [if] */
//
  return ;
} /* end of [lexbuf_curpos_next] */

/* ****** ****** */

ats_int_type
lexbuf_char_next (
  ats_ptr_type lxbf0
) {
  lexbuf *lxbf ;
  char32_t *buf_ptr ;
  int c, fstpos, curpos, endpos ;
//
  lxbf = (lexbuf*)lxbf0 ;
//
  buf_ptr = lxbf->buf_ptr ;
  curpos = lxbf->curpos ;
  endpos = lxbf->endpos ;
//
  if (curpos != endpos) {
    c = buf_ptr[curpos] ; lexbuf_curpos_next (lxbf, c); return c ;
  } // end of [if]
//
  lexbuf_refill (lxbf0) ;
//
  buf_ptr = lxbf->buf_ptr ;
  curpos = lxbf->curpos ;
  endpos = lxbf->endpos ;
/*
  fprintf (stdout, "lexbuf_char_next: refill: curpos = %i\n", curpos);
  fprintf (stdout, "lexbuf_char_next: refill: endpos = %i\n", endpos);
*/
  if (curpos != endpos) {
    c = buf_ptr[curpos] ; lexbuf_curpos_next (lxbf, c); return c ;
  } // end of [if]
//
  return -1 ; /* [-1] represents a special character */
//
} /* end of [lexbuf_char_next] */

/* ****** ****** */

ats_bool_type
lexbuf_is_eof (
  ats_ptr_type lxbf0
) {
  lexbuf *lxbf = (lexbuf*)lxbf0 ;
  if (lxbf->curpos != lxbf->endpos) return ats_false_bool ;
  lexbuf_refill (lxbf0) ;  
  if (lxbf->curpos != lxbf->endpos) {
    return ats_false_bool ;
  } else {
    return ats_true_bool ;
  } /* end of [if] */
} /* end of [lexbuf_is_eof] */

/* ****** ****** */

ats_ptr_type
lexing_fstpos_get () {
  ats_ptr_type pos ; lexbuf *lxbf ;
  lxbf = (lexbuf*)(lexing_lexbuf_get()) ;
  pos = lexbuf_fstpos_get (lxbf) ;
  lexing_lexbuf_set(lxbf) ;
  return pos;
} // end of [lexing_fstpos_get]

ats_ptr_type
lexing_lstpos_get () {
  ats_ptr_type pos ; lexbuf *lxbf ;
  lxbf = (lexbuf*)(lexing_lexbuf_get()) ;
  pos = lexbuf_lstpos_get (lxbf) ;
  lexing_lexbuf_set(lxbf) ;
  return pos;
} // end of [lexing_lstpos_get]

ats_ptr_type
lexing_curpos_get () {
  ats_ptr_type pos ; lexbuf *lxbf ;
  lxbf = (lexbuf*)(lexing_lexbuf_get()) ;
  pos = lexbuf_curpos_get (lxbf) ;
  lexing_lexbuf_set(lxbf) ;
  return pos;
} // end of [lexing_curpos_get]

/* ****** ****** */

ats_void_type
lexbuf_curpos_fprint (
  ats_ptr_type fil, ats_ref_type lxbf0
) {
  lexbuf *lxbf = (lexbuf*)lxbf0 ;
  fprintf ((FILE*)fil, "%li(line=%i, offset=%i)",
    lxbf->curpos_toff+1L, lxbf->curpos_line+1, lxbf->curpos_loff+1
  ) ; return ;
} // end of [lexbuf_curpos_fprint]

/* ****** ****** */

ats_ptr_type
lexbuf_make_infile (
  ats_ptr_type infile
) {
  lexbuf *lxbf ;
  char32_t *buf_ptr ;

  buf_ptr = ATS_MALLOC (sizeof(char32_t)*BUF_SIZE) ;
  lxbf = ATS_MALLOC (sizeof(lexbuf)) ;
  lxbf->buf_ptr = buf_ptr ;
  lxbf->buf_size = BUF_SIZE ;
  lxbf->infile = infile ;

  lxbf->fstpos = 0 ;
  lxbf->fstpos_line = 0 ;
  lxbf->fstpos_loff = 0 ;
  lxbf->fstpos_toff = 0 ;

  lxbf->lstpos = 0 ;
  lxbf->lstpos_line = 0 ;
  lxbf->lstpos_loff = 0 ;
  lxbf->lstpos_toff = 0 ;

  lxbf->curpos = 0 ;
  lxbf->curpos_line = 0 ;
  lxbf->curpos_loff = 0 ;
  lxbf->curpos_toff = 0 ;

  lxbf->endpos = 0 ;
/*
  printf ("lexbuf_make_infile: lxbf = %p\n", lxbf) ;
*/
  return lxbf ;
} // end of [lexbuf_make_infile]

ats_void_type
lexbuf_free (ats_ptr_type lxbf0) {
  lexbuf *lxbf ;
//
  lxbf = (lexbuf*)lxbf0 ;
  lexing_infile_free (lxbf->infile) ;
  ATS_FREE (lxbf->buf_ptr) ;
//
  return ;
} // end of [lexbuf_free]

/* ****** ****** */

ats_int_type
lexeme_get_lexbuf (
  ats_ptr_type lxbf0, ats_int_type i
) {
  int len, fstpos, lstpos, bufsz ;
  lexbuf *lxbf ;
//
  if (i < 0) {
    ats_exit_errmsg (
      1, "lexeme_get_lexbuf: index is out_of_bounds.\n"
    ) ;
  } /* end of [if] */
//
  lxbf = (lexbuf*)lxbf0 ;
//
  fstpos = lxbf->fstpos ;
  lstpos = lxbf->lstpos ;
  len = lstpos - fstpos ;
  bufsz = lxbf->buf_size ;
  if (len < 0) { len += bufsz ; }
//
  if (i > len) {
    ats_exit_errmsg (
      1, "lexeme_get_lexbuf: index is out_of_bounds.\n"
    ) ;
  } /* end of [if] */
//
  i = fstpos + i ;
  if (i >= bufsz) { i -= bufsz ; }
//
  return *((lxbf->buf_ptr) + i) ;
} // end of [lexeme_get_lexbuf]

ats_void_type
lexeme_set_lexbuf (
  ats_ptr_type lxbf0
, ats_int_type i, ats_int_type c
) {
  int len, fstpos, lstpos, bufsz ;
  lexbuf *lxbf ;
//
  if (i < 0) {
    ats_exit_errmsg (
      1, "lexeme_set_lexbuf: index is out_of_bounds.\n"
    ) ;
  } /* end of [if] */
//
  lxbf = (lexbuf*)lxbf0 ;
//
  fstpos = lxbf->fstpos ;
  lstpos = lxbf->lstpos ;
  len = lstpos - fstpos ;
  bufsz = lxbf->buf_size ;
  if (len < 0) { len += bufsz ; }
//
  if (i > len) {
    ats_exit_errmsg (
      1, "lexeme_set_lexbuf: index is out_of_bounds.\n"
    ) ;
  } // end of [if]
//
  i = fstpos + i ;
  if (i >= bufsz) { i -= bufsz ; }
//
  *((lxbf->buf_ptr) + i) = c ;
//
  return ;
} // end of [lexeme_set_lexbuf]

/* ****** ****** */

ats_ptr_type
lexeme_strptr_lexbuf
  (ats_ptr_type lxbf0) {
  int len, fstpos, lstpos ;
  char32_t *src, *dst0, *dst ;
  char *res ;
  lexbuf *lxbf ;

  lxbf = (lexbuf*)lxbf0 ;

  fstpos = lxbf->fstpos ;
  lstpos = lxbf->lstpos ;
  len = lstpos - fstpos ;
  if (len < 0) { len += lxbf->buf_size ; }

  src = (lxbf->buf_ptr) + fstpos ;
  dst0 = ATS_MALLOC ((len + 1) * sizeof(char32_t)) ;
  dst = dst0 ;

  if (lstpos < fstpos) {
    while (fstpos < lxbf->buf_size) {
      *dst = *src ; ++fstpos ; ++src ; ++dst ;
    }
    fstpos = 0 ; src = lxbf->buf_ptr ;
  }

  while (fstpos < lstpos) {
    *dst = *src ; ++fstpos ; ++src ; ++dst ;
  }

  *dst = 0 ;

  res = string_of_string32(dst0, len) ;
  ATS_FREE(dst0) ;

  return res ;
} // end of [lexeme_strptr_lexbuf]

/* ****** ****** */

static ats_ptr_type the_lexbuf = (lexbuf*)0 ;

ats_void_type
lexing_lexbuf_markroot () {
  ATS_GC_MARKROOT (&the_lexbuf, sizeof(ats_ptr_type)) ;
  return ;
} // end of [lexing_lexbuf_markroot]

static ats_int_type the_lexbuf_flag = 0 ;

ats_ptr_type
lexing_lexbuf_get () {
  if (the_lexbuf_flag == 0) {
    ats_exit_errmsg (
      1, "[lexeme_get] failed: The default lexbuf is not set!\n"
    );
  } // end of [if]
  the_lexbuf_flag = 0 ;
  return the_lexbuf ;
} // end of [lexing_lexbuf_get]

ats_void_type
lexing_lexbuf_set
  (ats_ptr_type lxbf) {
  if (the_lexbuf_flag == 1) {
    ats_exit_errmsg (
      1, "[lexeme_set] failed: The default lexbuf is already set!\n"
    );
  }
  the_lexbuf_flag = 1 ;
  the_lexbuf = lxbf ;
/*
  printf ("lexing_lexbuf_set: lxbf = %p\n", lxbf) ;
*/
  return ;
} // end of [lexing_lexbuf_set]

ats_void_type
lexing_lexbuf_free () {
  lexbuf_free (lexing_lexbuf_get ()) ; return ;
} // end of [lexing_lexbuf_free]

%} // end of [%{$]

(* ****** ****** *)

(* end of [lexing.dats] *)
