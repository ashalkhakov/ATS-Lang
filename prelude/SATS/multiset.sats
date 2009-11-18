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

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then

#print "Loading [multiset.sats] starts!\n"

#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

datasort elt = // abstract // positive integer
sta lte_elt_elt_bool : (elt, elt) -> bool
stadef <= = lte_elt_elt_bool

(* ****** ****** *)

datasort eltlst =
  eltlst_nil of () | eltlst_cons of (elt, eltlst)
// end of [eltlst]

(* ****** ****** *)

datasort eltmset = // abstract // natural number

sta eltmset_nil : eltmset // the empty set

sta add_elt_eltmset_eltmset : (elt, eltmset) -> eltmset
stadef + = add_elt_eltmset_eltmset
sta add_eltmset_elt_eltmset : (eltmset, elt) -> eltmset
stadef + = add_eltmset_elt_eltmset
sta add_eltmset_eltmset_eltmset : (eltmset, eltmset) -> eltmset
stadef + = add_eltmset_eltmset_eltmset

(* ****** ****** *)

dataprop ELTLSTMSET (eltlst, eltmset) =
  | {x:elt} {xs:eltlst} {mst:eltmset}
    ELTLSTMSETcons (eltlst_cons (x, xs), x+mst) of ELTLSTMSET (xs, mst)
  | ELTLSTMSETnil (eltlst_nil, eltmset_nil)
// end of [ELTLSTMSET]

prfun ELTLSTMSET_istot {xs:eltlst} (): [xs1:eltmset] ELTLSTMSET (xs, xs1)
prfun ELTLSTMSET_isfun {xs:eltlst} {xs1,xs2:eltmset}
  (pf1: ELTLSTMSET (xs, xs1), pf2: ELTLSTMSET (xs, xs2)): [xs1==xs2] void

(* ****** ****** *)

dataprop ELTMSETSZ (eltmset, int) =
  | {x:elt} {xs:eltmset} {n:nat} ELTMSETSZcons (x+xs, n+1) of ELTMSETSZ (xs, n)
  | ELTMSETSZnil (eltmset_nil, 0)

prfun ELTMSETSZ_istot {xs:eltmset} (): [n:nat] ELTMSETSZ (xs, n)
prfun ELTMSETSZ_isfun {xs:eltmset} {n1,n2:nat}
  (pf1: ELTMSETSZ (xs, n1), pf2: ELTMSETSZ (xs, n2)): [n1==n2] void

(* ****** ****** *)

absprop ELTMSETLB (elt, eltmset)
absprop ELTMSETUB (eltmset, elt)

prfun ELTMSETLB_trans
  {l1,l2:elt | l1 <= l2} {xs:eltmset} (pf: ELTMSETLB (l2, xs)): ELTMSETLB (l1, xs)

prfun ELTMSETUB_trans
  {u1,u2:elt | u1 <= u2} {xs:eltmset} (pf: ELTMSETUB (xs, u1)): ELTMSETUB (xs, u2)

(* ****** ****** *)

#if VERBOSE_PRELUDE #then

#print "Loading [multiset.sats] finishes!\n"

#endif // end of [VERBOSE_PRELUDE]

(* end of [multiset.sats] *)
