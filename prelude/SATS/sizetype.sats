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

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then

#print "Loading [sizetype.sats] starts!\n"

#endif

(* ****** ****** *)

%{#

#include "prelude/CATS/sizetype.cats" ;

%}

(* ****** ****** *)

// unindexed size type

fun int_of_size (sz: size_t):<> int
  = "atspre_int_of_size"

fun size_of_int1 {i:nat} (i: int i):<> size_t
  = "atspre_size_of_int1"

fun add_size_size (sz1: size_t, sz2: size_t):<> size_t
  = "atspre_add_size_size"
overload + with add_size_size

fun mul_size_size (sz1: size_t, sz2: size_t):<> size_t
  = "atspre_mul_size_size"
overload * with mul_size_size

(* ****** ****** *)

symintr fprint_size

fun fprint0_size (out: FILEref, x: size_t):<!exnref> void
  = "atspre_fprint_size"

fun fprint1_size {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: size_t):<!exnref> void
  = "atspre_fprint_size"

overload fprint_size with fprint0_size
overload fprint_size with fprint1_size
overload fprint with fprint_size

(* ****** ****** *)

fun print_size (i: size_t):<!ref> void
  = "atspre_print_size"

and prerr_size (i: size_t):<!ref> void
  = "atspre_prerr_size"

overload print with print_size
overload prerr with prerr_size

(* ****** ****** *)

// indexed size type

(* ****** ****** *)

fun size1_of_size (i: size_t):<> [i:nat] size_t i
  = "atspre_size1_of_size"

(* ****** ****** *)

fun int1_of_size1 {i:nat} (sz: size_t i):<> int i
  = "atspre_int1_of_size1"

// ------ ------

fun size1_of_int1 {i:nat} (i: int i):<> size_t i
  = "atspre_size1_of_int1"

fun size1_of_ssize1 {i:nat} (_: ssize_t i):<> size_t i
  = "atspre_size1_of_ssize1"

fun size1_of_ptrdiff1 {i:nat} (_: ptrdiff_t i):<> size_t i
  = "atspre_size1_of_ptrdiff1"

(* ****** ****** *)

fun succ_size1 {i:nat} (i: size_t i):<> size_t (i+1)
  = "atspre_succ_size1"

and pred_size1 {i:pos} (i: size_t i):<> size_t (i-1)
  = "atspre_pred_size1"

overload succ with succ_size1
overload pred with pred_size1

// ------ ------

fun add_size1_size1 {i,j:nat}
  (i: size_t i, j: size_t j):<> size_t (i+j)
  = "atspre_add_size1_size1"
overload + with add_size1_size1

fun add_size1_int1 {i,j:nat}
  (i: size_t i, j: int j):<> size_t (i+j)
  = "atspre_add_size1_int1"
overload + with add_size1_int1

fun sub_size1_size1 {i,j:nat | j <= i}
  (i: size_t i, j: size_t j):<> size_t (i-j)
  = "atspre_sub_size1_size1"
overload - with sub_size1_size1

fun sub_size1_int1 {i,j:nat | j <= i}
  (i: size_t i, j: int j):<> size_t (i-j)
  = "atspre_sub_size1_int1"
overload - with sub_size1_int1

// ------ ------

fun mul_size1_size1
  {i,j:nat} (i: size_t i, j: size_t j):<> size_t (i*j)
  = "atspre_mul_size1_size1"
overload * with mul_size1_size1

symintr szmul1 szmul2; infixl ( * ) szmul1 szmul2

fun mul1_size1_size1
  {i,j:nat} (i: size_t i, j: size_t j):<> [p:nat] size_t p
  = "atspre_mul1_size1_size1"
overload szmul1 with mul1_size1_size1

fun mul2_size1_size1
  {i,j:nat} (i: size_t i, j: size_t j)
  :<> [p:int] (MUL (i, j, p) | size_t p)
  = "atspre_mul2_size1_size1"
overload szmul2 with mul2_size1_size1

// ------ ------

symintr szmod1; infixl szmod1

fun mod1_size1_size1 {i:nat;j:int | j > 0}
  (i: size_t i, j: size_t j):<> [r:nat | r < j] size_t r
  = "atspre_mod1_size1_size1"
overload szmod1 with mod1_size1_size1

// ------ ------

fun lt_size1_size1 {i,j:nat}
  (i: size_t i, j: size_t j):<> bool (i < j)
  = "atspre_lt_size1_size1"
overload < with lt_size1_size1

fun lt_int1_size1 {i,j:nat}
  (i: int i, j: size_t j):<> bool (i < j)
  = "atspre_lt_int1_size1"
overload < with lt_int1_size1

fun lt_size1_int1 {i,j:nat}
  (i: size_t i, j: int j):<> bool (i < j)
  = "atspre_lt_size1_int1"
overload < with lt_size1_int1

// ------ ------

fun lte_size1_size1 {i,j:nat}
  (i: size_t i, j: size_t j):<> bool (i <= j)
  = "atspre_lte_size1_size1"
overload <= with lte_size1_size1

fun lte_int1_size1 {i,j:nat}
  (i: int i, j: size_t j):<> bool (i < j)
  = "atspre_lte_int1_size1"
overload <= with lte_int1_size1

fun lte_size1_int1 {i,j:nat}
  (i: size_t i, j: int j):<> bool (i <= j)
  = "atspre_lte_size1_int1"
overload <= with lte_size1_int1

// ------ ------

fun gt_size1_size1 {i,j:nat}
  (i: size_t i, j: size_t j):<> bool (i > j)
  = "atspre_gt_size1_size1"
overload > with gt_size1_size1

fun gt_size1_int1 {i,j:nat}
  (i: size_t i, j: int j):<> bool (i > j)
  = "atspre_gt_size1_int1"
overload > with gt_size1_int1

// ------ ------

fun gte_size1_size1 {i,j:nat}
  (i: size_t i, j: size_t j):<> bool (i >= j)
  = "atspre_gte_size1_size1"
overload >= with gte_size1_size1

fun gte_size1_int1 {i,j:nat}
  (i: size_t i, j: int j):<> bool (i >= j)
  = "atspre_gte_size1_int1"
overload >= with gte_size1_int1

// ------ ------

fun eq_size1_size1 {i,j:nat}
  (i: size_t i, j: size_t j):<> bool (i == j)
  = "atspre_eq_size1_size1"
overload = with eq_size1_size1

fun eq_size1_int1 {i,j:nat}
  (i: size_t i, j: int j):<> bool (i == j)
  = "atspre_eq_size1_int1"
overload = with eq_size1_int1

// ------ ------

fun neq_size1_size1 {i,j:nat}
  (i: size_t i, j: size_t j):<> bool (i <> j)
  = "atspre_neq_size1_size1"
overload <> with neq_size1_size1

fun neq_size1_int1 {i,j:nat}
  (i: size_t i, j: int j):<> bool (i <> j)
  = "atspre_neq_size1_int1"
overload <> with neq_size1_int1

// ------ ------

fun max_size1_size1 {i,j:nat}
  (i: size_t i, j: size_t j):<> size_t (max (i, j))
  = "atspre_max_size1_size1"
and min_size1_size1 {i,j:nat}
  (i: size_t i, j: size_t j):<> size_t (min (i, j))
  = "atspre_min_size1_size1"

overload max with max_size1_size1
overload min with min_size1_size1

(* ****** ****** *)

// signed indexed size type

(* ****** ****** *)

fun int1_of_ssize1 {i:int} (sz: ssize_t i):<> int i
  = "atspre_int1_of_ssize1"

fun ssize1_of_int1 {i:int} (i: int i):<> ssize_t i
  = "atspre_ssize_of_int"

fun ssize1_of_size1 {i:nat} (sz: size_t i):<> ssize_t i
  = "atspre_ssize_of_size"

(* ****** ****** *)

fun lt_ssize1_int1 {i,j:int}
  (i: ssize_t i, j: int j):<> bool (i < j)
  = "atspre_lt_ssize1_int1"
overload < with lt_ssize1_int1

// ------ ------

fun lte_ssize1_int1 {i,j:int}
  (i: ssize_t i, j: int j):<> bool (i <= j)
  = "atspre_lt_ssize1_int1"
overload <= with lte_ssize1_int1

// ------ ------

fun gt_ssize1_int1 {i,j:int}
  (i: ssize_t i, j: int j):<> bool (i > j)
  = "atspre_gt_ssize1_int1"
overload > with gt_ssize1_int1

// ------ ------

fun gte_ssize1_int1 {i,j:int}
  (i: ssize_t i, j: int j):<> bool (i >= j)
  = "atspre_gte_ssize1_int1"
overload >= with gte_ssize1_int1

(* ****** ****** *)

fun eq_ssize1_ssize1 {i,j:int}
  (i: ssize_t i, j: ssize_t j):<> bool (i == j)
  = "atspre_eq_ssize1_ssize1"
overload = with eq_ssize1_ssize1

fun neq_ssize1_ssize1 {i,j:int}
  (i: ssize_t i, j: ssize_t j):<> bool (i <> j)
  = "atspre_neq_ssize1_ssize1"
overload <> with neq_ssize1_ssize1

(* ****** ****** *)

#if VERBOSE_PRELUDE #then

#print "Loading [sizetype.sats] finishes!\n"

#endif

(* end of [sizetype.sats] *)
