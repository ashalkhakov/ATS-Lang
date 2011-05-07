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
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the  terms of the  GNU General Public License as published by the Free
** Software Foundation; either version 2.1, or (at your option) any later
** version.
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

(*
**
** A functional map implementation based on AVL trees
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: May, 2011 // based on a version done in October, 2008
**
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

abstype
set_t0ype_type (elt: t@ype+)
stadef set = set_t0ype_type

(* ****** ****** *)

typedef cmp (elt:t@ype) = (elt, elt) -<cloref> Sgn

fun{elt:t@ype}
compare_elt_elt (x1: elt, x2: elt, cmp: cmp elt):<> Sgn

(* ****** ****** *)

fun{} funset_make_nil {a:t@ype} ():<> set (a)
fun{a:t@ype} funset_make_sing (x0: a):<> set (a) // singleton set
fun{a:t@ype} funset_make_list (xs: List a, cmp: cmp a):<> set (a)

(* ****** ****** *)

fun{} funset_is_empty {a:t@ype} (xs: set a):<> bool
fun{} funset_isnot_empty {a:t@ype} (xs: set a):<> bool

(* ****** ****** *)
//
// HX: the time complexity of this function is O(n), where n is
fun{a:t@ype} funset_size (xs: set a):<> size_t // the size of the set

(* ****** ****** *)

fun{a:t@ype}
funset_is_member (xs: set a, x0: a, cmp: cmp a):<> bool
fun{a:t@ype}
funset_isnot_member (xs: set a, x0: a, cmp: cmp a):<> bool

(* ****** ****** *)

fun{elt:t@ype}
funset_insert (
  xs: &set (elt)
, x0: elt
, cmp: cmp elt
) :<> bool(*[x0] alreay exists in [xs]*) // end of [funset_insert]

(* ****** ****** *)

fun{elt:t@ype}
funset_remove (
  xs: &set (elt)
, x0: elt
, cmp: cmp elt
) :<> bool(*removed/not: true/false*)
// end of [funset_remove]

(* ****** ****** *)
//
// HX: choose an element in an unspecified manner
//
fun{elt:t@ype}
funset_choose (
  xs: set elt, x: &elt? >> opt (elt, b)
) : #[b:bool] bool (b) // end of [funset_choose]
//
// HX: take out an element in an unspecified manner
//
fun{elt:t@ype}
funset_takeout (
  xs: &set elt >> set elt, x: &elt? >> opt (elt, b)
) : #[b:bool] bool (b) // end of [funset_takeout]
//
(* ****** ****** *)

fun{elt:t@ype} funset_union
  (xs1: set elt, xs2: set elt, cmp: cmp elt):<> set (elt)
fun{elt:t@ype} funset_intersect
  (xs1: set elt, xs2: set elt, cmp: cmp elt):<> set (elt)
fun{elt:t@ype} funset_diff
  (xs1: set elt, xs2: set elt, cmp: cmp elt):<> set (elt)
fun{elt:t@ype} funset_symdiff
  (xs1: set elt, xs2: set elt, cmp: cmp elt):<> set (elt)

(* ****** ****** *)

fun{elt:t@ype}
funset_is_subset (xs1: set elt, xs2: set elt, cmp: cmp elt): bool

fun{elt:t@ype}
funset_is_equal (xs1: set elt, xs2: set elt, cmp: cmp elt): bool

(* ****** ****** *)

fun{elt:t@ype}
funset_foreach_funenv
  {v:view} {vt:viewtype} (
  pf: !v 
| xs: set (elt)
, f: (!v | elt, !vt) -<fun> void
, env: !vt
) :<> void // end of [funset_foreach_funenv]

fun{elt:t@ype}
funset_foreach_fun (
  xs: set (elt)
, f: (elt) -<fun> void
) :<> void // end of [funset_foreach_fun]

fun{elt:t@ype}
funset_foreach_clo {v:view} (
  pf: !v
| xs: set (elt)
, f: &(!v | elt) -<clo> void
) :<> void // end of [funset_foreach_clo]

fun{elt:t@ype}
funset_foreach_cloref (
  xs: set (elt)
, f: (elt) -<cloref> void
) :<!ref> void // end of [funset_foreach_cloref]

(* ****** ****** *)

fun{elt:t@ype}
funset_listize (xs: set (elt)):<> List_vt (elt)

(* ****** ****** *)

(* end of [funset_avltree.sats] *)
