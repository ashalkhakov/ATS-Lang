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

// some built-in static constants for linear list operations

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // loaded by [ats_main_prelude]

(* ****** ****** *)

implement list_vt_length_is_nonnegative (xs) = begin
  case+ xs of list_vt_cons _ => fold@ xs | list_vt_nil () => fold@ xs
end // end of [list_vt_length_is_nonnegative]

(* ****** ****** *)

fn list_vt_is_cons {a:viewt@ype} {n:nat} (xs: !list_vt (a, n)): bool (n>0) =
  case+ xs of list_vt_cons _ => (fold@ xs; true) | list_vt_nil () => (fold@ xs; false)
// end of [list_vt_is_cons]

(* ****** ****** *)

// A feature in this implementation is yet to be realized

viewtypedef List_vt = [a:viewt@ype] List_vt a

implement{a} list_vt_of_arraysize (arrsz) = let
  fun loop {n:nat} {l1,l2:addr} .<n>. (
      pf1: !array_v (a, n, l1) >> array_v (a?, n, l1)
    , pf2: !List_vt? @ l2 >> list_vt (a, n) @ l2
    | p_arr: ptr l1, res: ptr l2, n: int n
    ) :<> void =
    if n > 0 then let
      prval (pf11, pf12) = array_v_uncons {a} (pf1)
      val () = !res := list_vt_cons {a} {0} (!p_arr, ?)
      val list_vt_cons (_, !res_next) = !res
      val () = loop 
        (pf12, view@ (!res_next) | p_arr+sizeof<a>, res_next, n-1)
      // end of [val]
      prval () = pf1 := array_v_cons {a?} (pf11, pf12)
    in
      fold@ (!res)
    end else let
      prval () = array_v_unnil {a} (pf1)
      prval () = pf1 := array_v_nil {a?} ()        
    in
      !res := list_vt_nil {a} ()
    end // end of [if]
  // end of [loop]
  var res: List_vt?
  val (pf_gc, pf_arr | p_arr, sz) = arrsz
  val () = loop (pf_arr, view@ res | p_arr, &res, sz)
in
  array_ptr_free {a} (pf_gc, pf_arr | p_arr); res
end // end of [list_vt_of_arraysize]

(*

implement{a} list_vt_of_arraysize (arrsz) = let

// the loop goes from the end of an array to its beginning
fun loop {i,j:nat} {l:addr} {ofs:int} .<i>.
  (pf_mul: MUL (i, sizeof a, ofs), pf_arr: array_v (a, i, l) |
   i: int i, p: ptr (l+ofs), res: list_vt (a, j))
  :<> (array_v (a?, i, l) | list_vt (a, i+j)) =
  if i > 0 then let
    prval pf1_mul = mul_add_const {~1} (pf_mul)
    prval (pf1_arr, pf_lst) = array_v_unextend {a} (pf_mul, pf_arr)
    val p1 = p - sizeof<a>
    val x = ptr_get_vt (pf_lst | p1)
    val (pf1_arr | res) = loop (pf1_mul, pf1_arr | i-1, p1, cons (x, res))
    prval pf_arr = array_v_extend (pf1_mul, pf1_arr, pf_lst)
  in
    (pf_arr | res)    
  end else let
    prval () = array_v_unnone {a} (pf_arr)
  in
    (array_v_none {a?} () | res)
  end

val (pf_arr | p_arr, sz) = arrsz
val (pf_mul | ofs) = sz imul2 sizeof<a>
val (pf_arr | res) = loop (pf_mul, pf_arr | sz, p_arr+ofs, nil ())

in
  array_ptr_free {a} (pf_arr | p_arr); res
end // end of [list_vt_of_arraysize]

*)

(* ****** ****** *)

implement{a} list_vt_copy (xs0) = let
  fun loop {n:nat} .<n>.
    (xs: !list_vt (a, n), res: &List_vt a? >> list_vt (a, n))
    :<> void = case+ xs of
    | list_vt_cons (x, !p_xs1) => let
        val () = res := list_vt_cons {a} {0} (x, ?)
        val+ list_vt_cons (_, !p_res1) = res; val () = loop (!p_xs1, !p_res1)
      in
        fold@ xs; fold@ res
      end // end of [cons]
    | list_vt_nil () => (fold@ xs; res := list_vt_nil ())
  var res: List_vt a // uninitialized
in
  loop (xs0, res); res
end // end of [list_vt_copy]

(* ****** ****** *)

implement{a} list_vt_free (xs0) = let
  fun loop {n:nat} .<n>. (xs: list_vt (a, n)):<> void =
    case+ xs of ~list_vt_cons (_, xs) => loop xs | ~list_vt_nil () => ()
  // end of [list_vt_free]
in
  loop (xs0)
end // end of [list_vt_free]

(* ****** ****** *)

implement{a} list_vt_length (xs0) = loop (xs0, 0) where {
  fun loop {i,j:nat} .<i>.
    (xs: !list_vt (a, i), j: int j):<> int (i+j) = begin
    case+ xs of
    | list_vt_cons (_, !p_xs1) =>
        let val n = loop (!p_xs1, j+1) in fold@ xs; n end
    | list_vt_nil () => (fold@ xs; j)
  end // end of [loop]
} // end of [list_vt_length]

(* ****** ****** *)

implement{a} list_vt_append (xs0, ys0) = let
  var xs0 = xs0
  fun{a:viewt@ype} loop {m,n:nat} .<m>.
    (xs0: &list_vt (a, m) >> list_vt (a, m+n), ys0: list_vt (a, n))
    :<> void = begin case+ xs0 of
    | list_vt_cons (_, !p_xs) => (loop (!p_xs, ys0); fold@ xs0)
    | ~list_vt_nil () => (xs0 := ys0)
  end // end of [loop]
in
  loop (xs0, ys0); xs0
end // end of [list_vt_append]

(* ****** ****** *)

implement{a} list_vt_reverse (xs0) = let
  fun revapp {m,n:nat} .<m>.
    (xs: list_vt (a, m), ys: list_vt (a, n)):<> list_vt (a, m+n) =
    case+ xs of
    | list_vt_cons (_, !p_xs1) => let
        val xs1 = !p_xs1
      in
        !p_xs1 := ys; fold@ xs; revapp (xs1, xs)
      end // end of [val]
    | ~list_vt_nil () => ys
  // end of [revapp]
in
  revapp (xs0, list_vt_nil ())
end // end of [list_vt_reverse]

(* ****** ****** *)

implement{a} list_vt_tabulate__main
  {v} {vt} {n} {f} (pf | f, n, env) = let
  var res: List_vt a // uninitialized
  fun loop {i:nat | i <= n} .<n-i>. (
      pf: !v
    | n: int n, i: int i, f: (!v | natLt n, !vt) -<f> a, env: !vt
    , res: &(List_vt a)? >> list_vt (a, n-i)
    ) :<f> void =
    if i < n then let
      val () = (res := list_vt_cons {a} {0} (f (pf | i, env), ?))
      val+ list_vt_cons (_, !p) = res
    in
      loop (pf | n, i+1, f, env, !p); fold@ res
    end else begin
      res := list_vt_nil ()
    end // end of [if]
  // end of [loop]
in
  loop (pf | n, 0, f, env, res); res
end // end of [list_vt_tabulate__main]

implement{a} list_vt_tabulate_fun {n} {f:eff} (f, n) = let
  val f = coerce (f) where { extern castfn
    coerce (f: natLt n -<f> a):<> (!unit_v | natLt n, !Ptr) -<f> a
  } // end of [where]
  prval pf = unit_v ()
  val ans = list_vt_tabulate__main (pf | f, n, null)
  prval unit_v () = pf
in
  ans
end // end of [list_vt_tabulate_fun]

implement{a} list_vt_tabulate_clo {n} {f:eff} (f, n) = let
  typedef clo_t = natLt n -<clo,f> a
  stavar l_f: addr; val p_f: ptr l_f = &f
  viewdef V = clo_t @ l_f
  fn app (pf: !V | i: natLt n, p_f: !ptr l_f):<f> a = !p_f (i)
  prval pf = view@ f
  val ans = list_vt_tabulate__main<a> {V} {ptr l_f} (pf | app, n, p_f)
  prval () = view@ f := pf
in
  ans
end // end of [list_vt_tabulate_clo]

(* ****** ****** *)

implement{a} list_vt_foreach__main
  {v} {vt} {n} {f} (pf | xs0, f, env) = let
  viewtypedef fun_t = (!v | &a, !vt) -<f> void
  fun loop {i:nat} .<i>.
    (pf: !v | xs0: !list_vt (a, i), f: !fun_t, env: !vt):<f> void =
    case+ xs0 of
    | list_vt_cons (!p_x, !p_xs) => begin
        f (pf | !p_x, env); loop (pf | !p_xs, f, env); fold@ xs0
      end // end of [val]
    | list_vt_nil () => (fold@ xs0)
  // end of [loop]
in
  loop (pf | xs0, f, env)
end // end of [list_vt_foreach__main]

(* ****** ****** *)

implement{a} list_vt_iforeach__main
  {v} {vt} {n} {f} (pf | xs0, f, env) = let
  viewtypedef fun_t = (!v | natLt n, &a, !vt) -<f> void
  fun loop {i:nat | i <= n} .<n-i>.
    (pf: !v | i: int i, xs0: !list_vt (a, n-i), f: !fun_t, env: !vt):<f> void =
    case+ xs0 of
    | list_vt_cons (!p_x, !p_xs) => begin
        f (pf | i, !p_x, env); loop (pf | i+1, !p_xs, f, env); fold@ xs0
      end // end of [val]
    | list_vt_nil () => (fold@ xs0)
  // end of [loop]
in
  loop (pf | 0, xs0, f, env)
end // end of [list_vt_iforeach__main]

(* ****** ****** *)

// [list_vt.sats] is already loaded by a call to [pervasive_load]
staload _(*anonymous*) = "prelude/SATS/list_vt.sats" // this forces that the static
// loading function for [list_vt.sats] is to be called at run-time
// this is really needed only if some datatypes are declared in [list_vt.sats]

(* ****** ****** *)

(* end of [list_vt.dats] *)
