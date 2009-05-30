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

#define ATS_DYNLOADFLAG 0 // loaded by [ats_main_prelude]

(* ****** ****** *)

implement slseg_v_extend {a} (pf_seg, pf_gc, pf_at) = let
  prfun extend {l1,l2,l3:addr} {n:nat} .<n>. (
      pf_seg: slseg_v (a, l1, l2, n)
    , pf_gc: free_gc_v (@(a, ptr), l2)
    , pf_at: (a, ptr l3) @ l2
    ) :<prf> slseg_v (a, l1, l3, n+1) = begin case+ pf_seg of
    | slseg_v_cons (pf1_gc, pf1_at, pf1_seg) => begin
        slseg_v_cons (pf1_gc, pf1_at, slseg_v_extend (pf1_seg, pf_gc, pf_at))
      end // end of [slseg_v_cons]
    | slseg_v_nil () => slseg_v_cons (pf_gc, pf_at, slseg_v_nil ())
  end // end of [extend]
in
  extend (pf_seg, pf_gc, pf_at)
end // end of [slseg_v_extend]

(* ****** ****** *)

implement slseg_v_append {a} (pf1_seg, pf2_seg) = let
  prfun append {l1,l2,l3:addr} {n1,n2:nat} .<n1>. (
      pf1_seg: slseg_v (a, l1, l2, n1)
    , pf2_seg: slseg_v (a, l2, l3, n2)
    ) :<prf> slseg_v (a, l1, l3, n1+n2) = begin case+ pf1_seg of
    | slseg_v_cons (pf1_gc, pf1_at, pf1_seg) => begin
        slseg_v_cons (pf1_gc, pf1_at, slseg_v_append (pf1_seg, pf2_seg))
      end // end of [slseg_v_cons]
    | slseg_v_nil () => pf2_seg
  end // end of [append]
in
  append (pf1_seg, pf2_seg)
end // end of [slseg_v_append]

(* ****** ****** *)

implement{a} slseg_free
  (pf_seg | p, n) = _free (pf_seg | p, n) where {
  typedef T = @(a, ptr)
  fun _free {l1,l2:addr} {n:nat} .<n>. (
      pf_seg: slseg_v (a, l1, l2, n) | p: ptr l1, n: int n
    ) :<> void =
    if n > 0 then let
      prval slseg_v_cons (pf_gc, pf_at, pf1_seg) = pf_seg
      val p1 = p->1; val () = ptr_free {T} (pf_gc, pf_at | p)
    in
      _free (pf1_seg | p1, n-1)
    end else begin
      let prval slseg_v_nil () = pf_seg in () end
    end // end of [if]
  // end of [_free]
} (* end of [slseg_free] *)

(* ****** ****** *)

implement{a} slseg_foreach_clo
  {v} {l1,l2} {n} {f} (pf, pf_seg | p, n, f) = let
  fun loop {l1,l2:addr} {n:nat} .<n>. (
      pf: !v, pf_seg: !slseg_v (a, l1, l2, n)
    | p: ptr l1, n: int n, f: &(!v | &a) -<clo,f> void
    ) :<f> void =
  if n > 0 then let
    prval slseg_v_cons (pf_gc, pf_at, pf1_seg) = pf_seg
    val () = f (pf | p->0); val () = loop (pf, pf1_seg | p->1, n-1, f)
  in
    pf_seg := slseg_v_cons (pf_gc, pf_at, pf1_seg)
  end // end of [if]
in
  loop (pf, pf_seg | p, n, f)
end // end of [slseg_foreach_clo]
  
(* ****** ****** *)

%{^

static inline
ats_ptr_type
linqueuelst_list_vt_of_sllst (ats_ptr_type p) { return p ; }

%}

(* ****** ****** *)

// [slseg.sats] is already loaded by a call to [pervasive_load]
staload _(*anonymous*) = "prelude/SATS/slseg.sats" // this forces that the static
// loading function for [slseg.sats] is to be called at run-time
// this is really needed only if some datatypes are declared in [slseg.sats]

(* ****** ****** *)

(* end of [slseg_v.dats] *)
