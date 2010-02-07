(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Anairiats - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
**
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

// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: December 2007

(* ****** ****** *)

%{^
#include "ats_intinf.cats"  /* only needed for [ATS/Geizella] */
%} // end of [%^]

(* ****** ****** *)

staload "libc_sats_gmp.sats"

(* ****** ****** *)

staload "ats_intinf.sats"

(* ****** ****** *)

assume intinf_t = ref (mpz_vt)

(* ****** ****** *)

implement intinf_make_int (i: int) = let
  val (pf_gc, pf | p) = ptr_alloc_tsz {mpz_vt} (sizeof<mpz_vt>)
  prval () = free_gc_elim {mpz_vt} (pf_gc)
  val () = mpz_init_set_int (!p, i);
  val (pfbox | ()) = vbox_make_view_ptr (pf | p)
in
  ref_make_view_ptr (pfbox | p)
end // end of [intinf_make_int]

extern fun intinf_set_string
  (x: &mpz_vt? >> mpz_vt, s: string): void
  = "ats_intinf_set_string"

implement intinf_make_string (s: string) = let
  val (pf_gc, pf | p) = ptr_alloc_tsz {mpz_vt} (sizeof<mpz_vt>)
  prval () = free_gc_elim {mpz_vt} (pf_gc)
  val () = intinf_set_string (!p, s)
  val (pfbox | ()) = vbox_make_view_ptr (pf | p)
in
  ref_make_view_ptr (pfbox | p)
end // end of [intinf_make_string]

extern fun intinf_set_stringsp
  (x: &mpz_vt? >> mpz_vt, s: string): void
  = "ats_intinf_set_stringsp"

implement intinf_make_stringsp (s: string) = let
  val (pf_gc, pf | p) = ptr_alloc_tsz {mpz_vt} (sizeof<mpz_vt>)
  prval () = free_gc_elim {mpz_vt} (pf_gc)
  val () = intinf_set_stringsp (!p, s)
  val (pfbox | ()) = vbox_make_view_ptr (pf | p)
in
  ref_make_view_ptr (pfbox | p)
end // end of [intinf_make_stringsp]

(* ****** ****** *)

implement fprint_intinf (pf | out, r) = let
  val (vbox pf_mpz | p) = ref_get_view_ptr r
in
  $effmask_ref (fprint_mpz (pf | out, !p))
end // end of [fprint_intinf]

implement print_intinf (r) = print_mac (fprint_intinf, r)
implement prerr_intinf (r) = prerr_mac (fprint_intinf, r)

(* ****** ****** *)

val () = intinf_initialize () where {
  extern fun intinf_initialize (): void = "ats_intinf_initialize"
} // end of [val]

(* ****** ****** *)

%{$

ats_void_type
ats_intinf_set_string (
  ats_mpz_ptr_type x, ats_ptr_type s0
) {
  char *s, *si, c0, c1 ;
  int i, base, err ;

  s = s0 ; c0 = s[0] ;

  if (c0 == '\000') {
    atspre_exit_prerrf(1, "exit(ATS): ats_intinf_set_str(%s)\n", s) ;
  } // end of [if]

  i = 0 ; base = 10 ;
  if (c0 == '~') { i = 1 ; c1 = s[1] ; } else { c1 = c0 ; }
  if (c1 == '0') {
    base = 8 ; i += 1 ; c1 = s[i] ;

    if (c1 == '\000') {
      mpz_init_set_si ((mpz_ptr)x, 0); return ;
    }

    if (c1 == 'x' || c1 == 'X') { base = 16 ; i += 1 ; }
  }

  if (c0 == '~') {
    i -= 1 ; si = s + i ; c1 = *si ; *si = '-' ;
    err = mpz_init_set_str((mpz_ptr)x, si, base) ;
    *si = c1 ;
  } else {
    si = s + i ;
    err = mpz_init_set_str((mpz_ptr)x, si, base) ;
  } // end of [if]

  if (err < 0) {
    atspre_exit_prerrf(1, "exit(ATS): mpz_init_set_str(%s)\n", s) ;
  } // end of [if]

  // mpz_out_str(stdout, 10, (mpz_ptr)x) ; fprintf (stdout, "\n") ;

  return ;
} /* end of [ats_intinf_set_string] */

/* ****** ****** */

ats_void_type
ats_intinf_set_stringsp (
  ats_mpz_ptr_type x, ats_ptr_type s0
) {
  char c, *s ;
  s = s0 ; while (c = *s) {
    if (strchr ("lLuU", c)) break ; else ++s ;
  } // end of [while]

  if (c) {
    *s = '\000' ;
    ats_intinf_set_string (x, s0) ;
    *s = c ;
  } else {
    ats_intinf_set_string (x, s0) ;
  } // end of [if]

  return ;
} /* end of [ats_intinf_set_stringsp] */

/* ****** ****** */

// This is necessary to prevent memory leak

static
void* ats_intinf_malloc
  (size_t sz) { return ATS_MALLOC (sz) ; }
// end of [ats_intinf_malloc]

static
void ats_intinf_free
  (void* ptr, size_t sz) { ATS_FREE (ptr) ; return ; }
// end of [ats_intinf_free]

static
void* ats_intinf_realloc (
  void* ptr, size_t sz_old, size_t sz_new
) {
  return ATS_REALLOC (ptr, sz_new) ;
} // end of [ats_intinf_realloc]

ats_void_type
ats_intinf_initialize () { mp_set_memory_functions (
    &ats_intinf_malloc, &ats_intinf_realloc, &ats_intinf_free
  ) ;
  return ;
} // end of [ats_intinf_initialize]

%} // end of [%{$]

(* ****** ****** *)

(* end of [ats_intinf.sats] *)
