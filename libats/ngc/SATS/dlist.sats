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
// Start Time: March, 2011
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0 // there is no need for staloading at run-time

(* ****** ****** *)

sortdef t0p = t@ype
sortdef vt0p = viewt@ype

(* ****** ****** *)

absview
dlnode_v (
  a:viewt@ype+, l: addr, lp: addr, ln: addr
) // end of [dlnode_v]

prfun
dlnode_ptr_is_gtz
  {a:vt0p} {l,lp,ln:addr}
  (pf: !dlnode_v (a, l, lp, ln)): [l > null] void
// end of [dlnode_ptr_is_gtz]

(* ****** ****** *)

typedef
dlnode_get_prev_type
  (a:viewt@ype) = {l,lp,ln:addr} (
  !dlnode_v (a, l, lp, ln) | ptr l
) -<fun> ptr lp // end of [dlnode_get_prev_type]
fun{a:vt0p} dlnode_get_prev : dlnode_get_prev_type (a) // specific

typedef
dlnode_set_prev_type
  (a:viewt@ype) = {l,lp1,ln:addr} {lp2:addr} (
  !dlnode_v (a, l, lp1, ln) >> dlnode_v (a, l, lp2, ln) | ptr l, ptr lp2
) -<fun> void // end of [dlnode_set_prev_type]
fun{a:vt0p} dlnode_set_prev : dlnode_set_prev_type (a) // specific

(* ****** ****** *)

typedef
dlnode_get_next_type
  (a:viewt@ype) = {l,lp,ln:addr} (
  !dlnode_v (a, l, lp, ln) | ptr l
) -<fun> ptr ln // end of [dlnode_get_next_type]
fun{a:vt0p} dlnode_get_next : dlnode_get_next_type (a) // specific

typedef
dlnode_set_next_type
  (a:viewt@ype) = {l,lp,ln1:addr} {ln2:addr} (
  !dlnode_v (a, l, lp, ln1) >> dlnode_v (a, l, lp, ln2) | ptr l, ptr ln2
) -<fun> void // end of [dlnode_set_next_type]
fun{a:vt0p} dlnode_set_next : dlnode_set_next_type (a) // specific

(* ****** ****** *)

prfun
dlnode_v_takeout_val
  {a:vt0p} {l,lp,ln:addr}
  (pf: dlnode_v (a, l, lp, ln))
  : (a @ l, {a:vt0p} a @ l -<lin,prf> dlnode_v (a, l, lp, ln))
// end of [dlnode_v_takeout_val]

(* ****** ****** *)

typedef
dlnode_alloc_type
  (a:viewt@ype) =
  () -<fun> [l,lp,ln:addr] (
  option_v (dlnode_v (a?, l, lp, ln), l > null) | ptr l
) // end of [typedef]
fun{a:vt0p} dlnode_alloc : dlnode_alloc_type (a) // specific

typedef
dlnode_free_type (a:viewt@ype) =
  {l,lp,ln:addr} (dlnode_v (a?, l, lp, ln) | ptr l) -<fun> void
fun{a:vt0p} dlnode_free : dlnode_free_type (a) // specifc

(* ****** ****** *)

dataview
dlseg_v (
  a:viewt@ype+, int, addr, addr, addr, addr
) =
  | {n:nat}
    {l,lp:addr}
    {r,rn:addr}
    {ln:addr}
    dlseg_v_cons (a, n+1, l, lp, r, rn) of (
      dlnode_v (a, l, lp, ln), dlseg_v (a, n, ln, l, r, rn)
    ) // end of [dlseg_v_cons]
  | {l:addr}
    {r:addr}
    dlseg_v_nil (a, 0, l, r, r, l)
// end of [dlseg_v]

dataview
rdlseg_v (
  a:viewt@ype+, int, addr, addr, addr, addr
) =
  | {n:nat}
    {l,lp:addr}
    {r,rn:addr}
    {rp:addr}
    rdlseg_v_cons (a, n+1, l, lp, r, rn) of (
      rdlseg_v (a, n, l, lp, rp, r), dlnode_v (a, r, rp, rn)
    ) // end of [dlseg_v_cons]
  | {l:addr}
    {r:addr}
    rdlseg_v_nil (a, 0, l, r, r, l)
// end of [rdlseg_v]

(* ****** ****** *)

fun dlist_is_nil
  {a:vt0p} {n:int} {l,r:addr} (
  pf: !dlist_v (a, n, l, r) | p: ptr l
) :<> bool (n==0) = "atspre_ptr_is_null"

(* ****** ****** *)

absviewtype dlist (a:viewt@ype+, n:int)

prfun dlist_fold
  {a:vt0p} {n:int} {l,r:addr}
  (pflst: dlist_v (a, n, l, r) | p: !ptr l >> dlist (a, n)): void
// end of [dlist_fold]

prfun dlist_unfold
  {a:vt0p} {n:int}
  (xs: !dlist (a, n) >> ptr l):<> #[l,r:addr] (dlist_v (a, n, l, r) | void)
// end of [dlist_unfold]

castfn dlist_encode
  {a:vt0p} {n:int} {l,r:addr}
  (pflst: dlist_v (a, n, l, r) | p: ptr l):<> dlist (a, n)
// end of [dlist_encode]

castfn dlist_decode
  {a:vt0p} {n:int}
  (xs: dlist (a, n)):<> [l,r:addr] (dlist_v (a, n, l, r) | ptr l)
// end of [dlist_decode]

(* ****** ****** *)

(* end of [dlist.sats] *)
