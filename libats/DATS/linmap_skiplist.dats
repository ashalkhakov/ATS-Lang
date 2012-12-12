(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Postiats - Unleashing the Potential of Types!
** Copyright (C) 2011-2012 Hongwei Xi, ATS Trustful Software, Inc.
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

(* Author: Adam Udi *)
(* Authoremail: adamudi AT bu DOT edu *)

(* Author: Hongwei Xi *)
(* Authoremail: hwxi AT cs DOT bu DOT edu *)

(* Start time: December, 2012 *)

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

staload UN = "prelude/SATS/unsafe.sats"
staload _(*anon*) = "prelude/DATS/array.dats"

(* ****** ****** *)

staload "libats/SATS/linmap_skiplist.sats"

(* ****** ****** *)

abstype
node_type (
  key:t@ype, itm:viewt@ype+, l:addr, n:int
) // end of [node_type]

stadef node = node_type

typedef
node0 (
  key:t0p
, itm:vt0p
, n:int
) = [l:addr] node (key, itm, l, n)
typedef
node0 (
  key:t0p
, itm:vt0p
) = [l:addr;n:nat] node (key, itm, l, n)

typedef
node1 (
  key:t0p
, itm:vt0p
, n:int
) = [l:agz] node (key, itm, l, n)
typedef
node1 (
  key:t0p
, itm:vt0p
) = [l:agz;n:nat] node (key, itm, l, n)

(* ****** ****** *)

typedef
nodeGt0 (
  key:t0p, itm:vt0p, ni:int
) = [n:int | n > ni] node0 (key, itm, n)

typedef
nodeGte1 (
  key:t0p, itm:vt0p, ni:int
) = [n:int | n >= ni] node1 (key, itm, n)

(* ****** ****** *)

extern
castfn
node2ptr
  {key:t0p;itm:vt0p}
  {l:addr}{n:int} (nx: node (key, itm, l, n)):<> ptr (l)
// end of [node2ptr]

(* ****** ****** *)

fun{
} node_null
  {key:t0p;itm:vt0p}{n:nat} .<>.
  (n: int n):<> node (key, itm, null, n) = $UN.castvwtp0 (null)
// end of [node_null]

(* ****** ****** *)

extern
fun{
key:t0p;itm:vt0p
} node_make
  {lgN:int | lgN > 0}
  (k0: key, x0: itm, lgN: int lgN): node1 (key, itm, lgN)
// end of [node_make]

extern
fun{
key:t0p;itm:vt0p
} node_free
  {lgN:int | lgN > 0}
  (nx: node1 (key, itm, lgN), res: &itm? >> itm): void
// end of [node_free]

extern
fun{
key:t0p;itm:vt0p
} node_get_key (nx: node1 (key, itm)):<> key

extern
fun{
key:t0p;itm:vt0p
} node_getref_item (nx: node1 (key, itm)):<> Ptr1

(* ****** ****** *)

abstype
nodearr_type
  (key:t@ype, itm:viewt@ype+, int(*size*))
stadef nodearr = nodearr_type

extern
fun nodearr_get_at
  {key:t0p;itm:vt0p}
  {n:int}{i:nat | i < n}
  (nxa: nodearr (key, itm, n), i: int i):<> nodeGt0 (key, itm, i)
// end of [nodearr_get_at]

extern
fun nodearr_set_at
  {key:t0p;itm:vt0p}
  {n:int}{i:nat | i < n}
  (nxa: nodearr (key, itm, n), i: int i, nx0: nodeGt0 (key, itm, i)): void
// end of [nodearr_set_at]

extern
fun nodearr_make // HX: initized with nulls
  {key:t0p;itm:vt0p}{n:nat} (n: int n):<> nodearr (key, itm, n)
// end of [nodearr_make]

(* ****** ****** *)

implement
{key,itm}
node_make
  {lgN} (
  k0, x0, lgN
) = let
  viewtypedef VT = (key, itm, ptr, int)
  val (pfat, pfgc | p) = ptr_alloc<VT> ()
  val () = p->0 := k0
  val () = p->1 := $UN.castvwtp0{itm?}{itm}(x0)
  val () = p->2 := $UN.cast{ptr}(nodearr_make(lgN))
  val () = p->3 := lgN
in
  $UN.castvwtp0 {node1(key,itm,lgN)} @(pfat, pfgc | p)
end // end of [node_make]

implement
{key,itm}
node_free
  (nx, res) = let
//
  viewtypedef VT = (key, itm, ptr, int)
//
  prval (
    pfat, pfgc | p
  ) = __cast (nx) where {
    extern
    castfn __cast (
      nx: node1 (key, itm)
    ) :<> [l:addr] (VT @ l, free_gc_v (VT?, l) | ptr l)
  } // end of [prval]
//
  val () = res := p->1
//
  val () =
    __free (p->2) where {
    extern fun __free : ptr -> void = "atsruntime_free"
  } // end of [val]
//
  val () = ptr_free {VT?} (pfgc, pfat | p)
//
in
  // nothing
end // end of [node_free]

(* ****** ****** *)

#define i2sz size1_of_int1

(* ****** ****** *)

local

assume
nodearr_type
  (key:t0p, itm:vt0p, n:int) = array (ptr, n)
// end of [nodearr_type]

in // in of [local]

implement
nodearr_make (n) = let
  val asz = i2sz(n) in array_make_elt<ptr> (asz, null)
end // end of [nodearr]

implement
nodearr_get_at
  {key,itm}{i}
  (nxa, i) = let
  typedef T = nodeGt0 (key, itm, i)
  val nx0 = $effmask_ref (nxa[i]) in $UN.cast{T}(nx0)
end // end of [nodearr_get_at]

implement
nodearr_set_at
  {key,itm}{i}
  (nxa, i, nx0) = let
  val nx0 = $UN.cast{ptr} (nx0) in $effmask_ref (nxa[i] := nx0)
end // end of [nodearr_set_at]

end // end of [local]

(* ****** ****** *)

extern
fun{
key:t0p;itm:vt0p
} node_get_next
  {n:int}{ni:nat | ni < n}
  (nx: node1 (key, itm, n), ni: int ni):<> nodeGt0 (key, itm, ni)
// end of [node_get_next]

extern
fun{
key:t0p;itm:vt0p
} node_set_next
  {n,n1:int}{ni:nat | ni < n} (
  nx: node1 (key, itm, n), ni: int ni, nx0: nodeGt0 (key, itm, ni)
) :<> void // end of [node_set_next]

(* ****** ****** *)

#define lgMAX 40 // HX: it should be enough: 2^40 >= 10^12 :)

(* ****** ****** *)

dataviewtype
skiplist (
  key:t@ype, itm:viewt@ype+
) = // HX: [lgN] is the *current* highest level
  | {N:nat}{lgN:nat | lgN <= lgMAX}
    SKIPLIST (key, itm) of (size_t(N), int(lgN), nodearr(key, itm, lgMAX))
// end of [skiplist]

(* ****** ****** *)

assume
map_viewtype
  (key:t0p, itm:vt0p) = skiplist (key, itm)
// end of [map_viewtype]

(* ****** ****** *)

implement
linmap_make_nil () =
  SKIPLIST (i2sz(0), 0, nodearr_make (lgMAX))
// end of [linmap_make_nil]

(* ****** ****** *)

implement
linmap_is_nil (map) = let
  val SKIPLIST (!p_N, _, _) = map; val N = !p_N in fold@ (map); N = i2sz(0)
end // end of [linmap_is_nil]

implement
linmap_isnot_nil (map) = let
  val SKIPLIST (!p_N, _, _) = map; val N = !p_N in fold@ (map); N > i2sz(0)
end // end of [linmap_isnot_nil]

(* ****** ****** *)

implement
linmap_size (map) = let
  val SKIPLIST (!p_N, _, _) = map; val N = !p_N in fold@ (map); N
end // end of [linmap_size]

(* ****** ****** *)
//
// HX:
// for [node_search] to be called, k0 > the key contained in it
//
extern
fun{
key:t0p;itm:vt0p
} node_search {n:int} (
  nx: node1 (key, itm, n), k0: key, ni: natLte n, cmp: cmp key
) :<> node0 (key, itm) // end of [node_search]
extern
fun{
key:t0p;itm:vt0p
} nodearr_search {n:int} (
  nxa: nodearr (key, itm, n), k0: key, ni: natLte n, cmp: cmp key
) :<> node0 (key, itm) // end of [nodearr_search]

(* ****** ****** *)

implement
{key,itm}
node_search
  (nx, k0, ni, cmp) = let
in
//
if ni > 0 then let
  val ni1 = ni - 1
  val nx1 = node_get_next<key,itm> (nx, ni1)
  val p_nx1 = node2ptr (nx1)
in
//
if p_nx1 > null then let
  val k1 = node_get_key (nx1)
  val sgn = compare_key_key (k0, k1, cmp)
in
  if sgn < 0 then
    node_search<key,itm> (nx, k0, ni1, cmp)
  else if sgn > 0 then
    node_search<key,itm> (nx1, k0, ni, cmp)
  else nx1 // end of [if]
end else
  node_search<key,itm> (nx, k0, ni1, cmp)
// end of [if]
//
end else node_null (0)
//
end // end of [node_search]

implement
{key,itm}
nodearr_search
  (nxa, k0, ni, cmp) = let
in
//
if ni > 0 then let
  val ni1 = ni - 1
  val nx =
    nodearr_get_at {key,itm} (nxa, ni1)
  // end of [val]
  val p_nx = node2ptr (nx)
in
  if p_nx > null then let
    val k = node_get_key<key,itm> (nx)
    val sgn = compare_key_key (k0, k, cmp)
  in
    if sgn < 0 then
      nodearr_search<key,itm> (nxa, k0, ni1, cmp)
    else if sgn > 0 then
      node_search<key,itm> (nx, k0, ni, cmp)
    else nx // end of [if]
  end else
    nodearr_search<key,itm> (nxa, k0, ni1, cmp)
  // end of [if]
end else
  node_null (0)
// end of [if]
//
end // end of [nodearr_search]

(* ****** ****** *)

implement
{key,itm}
linmap_search_ref
  (map, k0, cmp) = let
in
//
case+ map of
| SKIPLIST
    (N, lgN, nxa) => let
    prval () = fold@ (map)
    val nx =
      nodearr_search<key,itm> (nxa, k0, lgN, cmp)
    val p_nx = node2ptr (nx)
  in 
    if p_nx > null
      then node_getref_item (nx) else null
    // end of [if]
  end // end of [SKIPLIST]
//
end // end of [linmap_search_ref]

(* ****** ****** *)
//
// HX:
// for [node_insert] to be called, k0 > the key contained in it
//
extern
fun{
key:t0p;itm:vt0p
} node_insert
  {n:int}{ni:nat | ni <= n} (
  nx: node1 (key, itm, n), k0: key, ni: int ni, nx0: nodeGte1 (key, itm, ni), cmp: cmp key
) : void // end of [node_insert]
extern
fun{
key:t0p;itm:vt0p
} nodearr_insert
  {n:int}{ni:nat | ni <= n} (
  nxa: nodearr (key, itm, n), k0: key, ni: int ni, nx0: nodeGte1 (key, itm, ni), cmp: cmp key
) : void // end of [nodearr_insert]

implement
{key,itm}
node_insert
  (nx, k0, ni, nx0, cmp) = let
in
//
if ni > 0 then let
  val ni1 = ni - 1
  val nx1 = node_get_next<key,itm> (nx, ni1)
  val p_nx1 = node2ptr (nx1)
in
  if p_nx1 > null then let
    val k1 = node_get_key<key,itm> (nx1)
    val sgn = compare_key_key (k0, k1, cmp)
  in
    if sgn <= 0 then let
      val () = node_set_next<key,itm> (nx, ni1, nx0)
      val () = node_set_next<key,itm> (nx0, ni1, nx1)
    in
      node_insert<key,itm> (nx, k0, ni1, nx0, cmp)
    end else
      node_insert<key,itm> (nx1, k0, ni, nx0, cmp)
    // end of [if]
  end else let
    val () = node_set_next<key,itm> (nx, ni1, nx0)
  in
    node_insert<key,itm> (nx, k0, ni1, nx0, cmp)
  end // end of [if]
end else (
  // nothing
) // end of [if]
//
end // end of [node_insert]

implement
{key,itm}
nodearr_insert
  (nxa, k0, ni, nx0, cmp) = let
in
//
if ni > 0 then let
  val ni1 = ni - 1
  val nx = nodearr_get_at {key,itm} (nxa, ni1)
  val p_nx = node2ptr (nx)
in
  if p_nx > null then let
    val k = node_get_key<key,itm> (nx)
    val sgn = compare_key_key (k0, k, cmp)
  in
    if sgn <= 0 then let
      val () = nodearr_set_at {key,itm} (nxa, ni1, nx0)
      val () = node_set_next<key,itm> (nx0, ni1, nx)
    in
      nodearr_insert<key,itm> (nxa, k0, ni1, nx0, cmp)
    end else
      node_insert<key,itm> (nx, k0, ni, nx0, cmp)
    // end of [if]
  end else let
    val () = nodearr_set_at {key,itm} (nxa, ni1, nx0)
  in
    nodearr_insert<key,itm> (nxa, k0, ni1, nx0, cmp)
  end // end of [if]
end else (
  // nothing
) // end of [if]
//
end // end of [nodearr_insert]

(* ****** ****** *)

implement
{key,itm}
linmap_insert
  (map, k0, x0, cmp, res) = let
//
val [l:addr]
  p_nx = linmap_search_ref<key,itm> (map, k0, cmp)
// end of [val]
//
in
//
if p_nx > null then let
  prval (pf, fpf) = __assert () where {
    extern praxi __assert : () -<prf> (itm @ l, itm @ l -<lin,prf> void)
  } // end of [prval]
  val () = res := !p_nx
  prval () = opt_some {itm} (res)
  val () = !p_nx := x0
  prval () = fpf (pf)
in
  true
end else let
  val () = linmap_insert_any<key,itm> (map, k0, x0, cmp)
  prval () = opt_none {itm} (res)
in
  false
end // end of [if]
//
end // end of [linmap_insert]

(* ****** ****** *)

implement
{key,itm}
linmap_insert_any
  (map, k0, x0, cmp) = let
//
val lgN0 =
  linmap_random_lgN (lgMAX)
val nx0 = node_make<key,itm> (k0, x0, lgN0)
//
in
//
case+ map of
| SKIPLIST
    (!p_N, !p_lgN, nxa) => let
    val N = !p_N
    val lgN = !p_lgN
    val () = !p_N := succ (N)
    val lgN = (
      if (lgN < lgN0) then lgN0 else lgN
    ) : natLte (lgMAX)
    val () = !p_lgN := lgN
    val () = nodearr_insert<key,itm> (nxa, k0, lgN0, nx0, cmp)
    prval () = fold@ (map)
  in
    // nothing
  end // end of [SKIPLIST]
//
end // end of [linmap_insert_any]

(* ****** ****** *)
//
// HX:
// for [node_takeout] to be called, k0 > the key contained in it
//
extern
fun{
key:t0p;itm:vt0p
} node_takeout
  {n:int}{ni:nat | ni <= n}
  (nx: node1 (key, itm, n), k0: key, ni: int ni, cmp: cmp key): nodeGt0 (key, itm, 0)
// end of [node_takeout]
extern
fun{
key:t0p;itm:vt0p
} nodearr_takeout
  {n:int}{ni:nat | ni <= n}
  (nxa: nodearr (key, itm, n), k0: key, ni: int ni, cmp: cmp key): nodeGt0 (key, itm, 0)
// end of [nodearr_takeout]

implement
{key,itm}
node_takeout
  (nx, k0, ni, cmp) = let
in
//
if ni > 0 then let
  val ni1 = ni - 1
  val nx1 = node_get_next<key,itm> (nx, ni1)
  val p_nx1 = node2ptr (nx1)
in
  if p_nx1 > null then let
    val k1 = node_get_key<key,itm> (nx1)
    val sgn = compare_key_key (k0, k1, cmp)
  in
    if sgn < 0 then
      node_takeout<key,itm> (nx, k0, ni1, cmp)
    else if sgn > 0 then
      node_takeout<key,itm> (nx1, k1, ni, cmp)
    else let // sgn = 0
      val () =
        node_set_next<key,itm> (nx, ni1, node_get_next<key,itm> (nx1, ni1))
      // end of [val]
    in
      if ni1 > 0 then node_takeout<key,itm> (nx, k0, ni1, cmp) else nx1
    end // end of [if]
  end else
    node_takeout<key,itm> (nx, k0, ni1, cmp)
  // end of [if]
end else
  node_null (1)
// end of [of]
//
end // end of [node_takeout]

implement
{key,itm}
nodearr_takeout
  (nxa, k0, ni, cmp) = let
in
//
if ni > 0 then let
  val ni1 = ni - 1
  val nx = nodearr_get_at {key,itm} (nxa, ni1)
  val p_nx = node2ptr (nx)
in
  if p_nx > null then let
    val k = node_get_key<key,itm> (nx)
    val sgn = compare_key_key (k0, k, cmp)
  in
    if sgn < 0 then
      nodearr_takeout<key,itm> (nxa, k0, ni1, cmp)
    else if sgn > 0 then
      node_takeout<key,itm> (nx, k0, ni, cmp)
    else let // sgn = 0
      val () = nodearr_set_at {key,itm} (nxa, ni1, node_get_next<key,itm> (nx, ni1))
    in
      if ni1 > 0 then nodearr_takeout<key,itm> (nxa, k0, ni1, cmp) else nx
    end
  end else
    nodearr_takeout<key,itm> (nxa, k0, ni1, cmp)
  // end of [if]
end else
  node_null (1)
// end of [if]
//
end // end of [nodearr_takeout]

(* ****** ****** *)

implement
{key,itm}
linmap_takeout
  (map, k0, cmp, res) = let
in
//
case+ map of
| SKIPLIST
    (!p_N, lgN, nxa) => let
    val N = !p_N
    val nx = nodearr_takeout<key,itm> (nxa, k0, lgN, cmp)
    val p_nx = node2ptr (nx)
  in
    if p_nx > null then let
      prval () = __assert (N) where {
        extern praxi __assert {N:int} (N: size_t N): [N>0] void
      } // end of [prval]
      val () = !p_N := pred (N)
      val () = node_free (nx, res)
      prval () = opt_some {itm} (res)
      prval () = fold@ (map)
    in
      true
    end else let
      prval () = fold@ (map)
      prval () = opt_none {itm} (res)
    in
      false
    end // end of [if]
  end // end of [SKIPLIST]
//
end // end of [linmap_takeout]

(* ****** ****** *)

implement
{key,itm}
linmap_remove
  (map, k0, cmp) = let
  var res: itm
  val takeout = linmap_takeout<key,itm> (map, k0, cmp, res)
  prval () = opt_clear (res)
in
  takeout(*removed*)
end // end of [linmap_remove]

(* ****** ****** *)

(*
fun{
key:t0p;itm:vt0p
}{
env:vt0p
} node_foreach_env (
  nx: nodeGt0 (key, itm, 0), env: &env
) : void = let
  val p_nx = node2ptr (nx)
in
//
if p_nx > null then let
  val k = node_get_key (nx)
  val [l:addr]
    p_i = node_getref_item (nx)
  prval (pf, fpf) = __assert () where {
    extern praxi __assert : () -<prf> (itm @ l, itm @ l -<lin,prf> void)
  } // end of [prval]
  val () = linmap_foreach$fwork<key,itm><env> (k, !p_i, env)
  prval () = fpf (pf)
  val nx1 = node_get_next<key,itm> (nx, 0)
in
  node_foreach_env (nx1, env)
end else () // end of [if]
//
end // end of [node_foreach_env]

implement
{key,itm}{env}
linmap_foreach_env
  (map, env) = let
in
//
case+ map of
| SKIPLIST
    (N, lgN, nxa) => let
    val nx = nodearr_get_at (nxa, 0)
  in
    node_foreach_env (nx, env)
  end // end of [SKIPLIST]
//
end // end of [linmap_foreach_env]
*)

(* ****** ****** *)

implement
{key,itm}
linmap_free_vt (m) = let
//
viewtypedef VT = map (key, itm)
val m1 =
  __cast (m) where {
  extern castfn __cast : (!VT >> VT?) -<> VT
}
//
in
//
case+ m1 of
| SKIPLIST
    (N, lgN, nxa) => let
  in
    if N = 0 then let
      val () = free@ {..}{0}{0} (m1)
      val () = __free (nxa) where {
        extern fun __free
          : {n:int} nodearr (key, itm, n) -<> void
      } // end of [val]
      prval () = opt_none {VT} (m)
    in
      false
    end else let
      prval () = fold@ (m1)
      prval () =
        __assert (m, m1) where {
        extern praxi __assert : (!VT? >> VT, VT) -<prf> void
      } // end of [val]
      prval () = opt_some {VT} (m)
    in
      true
    end // end of [if]
  end // end of [SKIPLIST]
//
end // end of [linmap_free_vt]

(* ****** ****** *)

(* end of [linmap_skiplist.dats] *)
