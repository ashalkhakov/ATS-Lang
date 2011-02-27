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
** Copyright (C) 2002-2011 Hongwei Xi, Boston University
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
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)
// Start Time: February, 2011
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0

(* ****** ****** *)

%{#
#include "contrib/kernel/CATS/utils.cats"
%} // end of [%{#]

(* ****** ****** *)

staload "contrib/kernel/basics.sats"

(* ****** ****** *)

fun{a:viewt@ype}
array_ptr_kalloc
  {n:nat} (
  asz: size_t n
) :<> [l:agz] (kfree_v (a, n, l), array_v (a?, n, l) | ptr l
) // end of [array_ptr_kalloc]

(*
// implemented in C
*)
fun array_ptr_kalloc_tsz
  {a:viewt@ype} {n:nat} (
  asz: size_t n, tsz: sizeof_t a
) :<> [l:agz] (kfree_v (a, n, l), array_v (a?, n, l) | ptr l)
  = "atsctrb_kernel_array_ptr_kalloc_tsz"
// end of [array_ptr_kalloc_tsz]

(* ****** ****** *)

(*
// implemented in C
*)
fun array_ptr_kfree
  {a:viewt@ype} {n:int} {l:addr} (
  pf_gc: kfree_v (a, n, l), pf_arr: array_v (a?, n, l) | p_arr: ptr l
) :<> void = "atsctrb_kernel_array_ptr_kfree"

(* ****** ****** *)

(* end of [array.sats] *)
