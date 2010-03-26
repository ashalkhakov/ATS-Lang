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
** A map implementation based on AVL trees
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: March, 2010
**
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

absviewtype map_t0ype_viewt0ype (key:t@ype, itm:viewt@ype+)
stadef map = map_t0ype_viewt0ype

(* ****** ****** *)

typedef cmp (key:t@ype) = (key, key) -<cloref> Sgn
fun{key:t@ype} compare_key_key (x1: key, x2: key, cmp: cmp key):<> Sgn

(* ****** ****** *)

sortdef t0p = t@ype and vt0p = viewt@ype

(* ****** ****** *)

fun{} linmap_make_nil {key:t0p;itm:vt0p} ():<> map (key, itm)

(* ****** ****** *)

fun{} linmap_is_nil {key:t0p;itm:vt0p} (m: !map (key, itm)):<> bool
fun{} linmap_isnot_nil {key:t0p;itm:vt0p} (m: !map (key, itm)):<> bool

(* ****** ****** *)

// this function is O(n)-time and non-tail-recursive
fun{key,itm:t@ype} linmap_size (m: !map (key, itm)):<> Nat

// this function is O(1) // for gathering stats
fun{key,itm:t@ype} linmap_height (m: !map (key, itm)):<> Nat

(* ****** ****** *)

fun{key:t0p;itm:t0p}
linmap_search (
  m: !map (key, itm), k0: key, cmp: cmp key, res: &itm? >> opt (itm, b)
) :<> #[b:bool] bool b
// end of [linmap_search]

(* ****** ****** *)

//
// HX-2010-03-25:
// if [k0] occurs in [m], [x0] replaces the original value associated with [k0]
//
fun{key:t0p;itm:vt0p}
linmap_insert (
    m: &map (key, itm), k0: key, x0: itm, cmp: cmp key, res: &itm? >> opt (itm, b)
  ) :<> #[b:bool] bool b
// end of [linmap_insert]

(* ****** ****** *)

fun{key:t0p;itm:vt0p}
linmap_takeout (
  m: &map (key, itm), k0: key, cmp: cmp key, res: &itm? >> opt (itm, b)
) :<> #[b:bool] bool b
// end of [linmap_takeout]

fun{key:t0p;itm:t0p}
linmap_remove (m: &map (key, itm), k0: key, cmp: cmp key):<> bool
// end of [linmap_remove]

(* ****** ****** *)

fun{key:t0p;itm:vt0p}
linmap_foreach_main {v:view} {vt:viewtype}
  (pf: !v | m: !map (key, itm), f: (!v | key, &itm, !vt) -<fun> void, env: !vt):<> void
// end of [linmap_foreach_main]

fun{key:t0p;itm:vt0p}
linmap_foreach_clo {v:view}
  (pf: !v | m: !map (key, itm), f: &(!v | key, &itm) -<clo> void):<> void
// end of [linmap_foreach_clo]

fun{key:t0p;itm:vt0p}
linmap_foreach_cloref
  (m: !map (key, itm), f: (key, &itm) -<cloref> void):<!ref> void
// end of [linmap_foreach_cloref]

(* ****** ****** *)

fun{key:t0p;itm:t0p}
linmap_free (m: map (key, itm)):<> void

(* ****** ****** *)

(* end of [linmap_avltree.sats] *)
