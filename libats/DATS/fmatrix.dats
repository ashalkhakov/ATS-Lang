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
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(*
**
** Fortran matrices: column-major representation
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Contributed by Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu)
**
** Time: Summer, 2009
**
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no need for dynamic loading

(* ****** ****** *)

staload "libats/SATS/genarrays.sats"
staload _(*anonymous*) = "libats/DATS/genarrays.dats"

(* ****** ****** *)

staload "libats/SATS/fmatrix.sats"

(* ****** ****** *)

implement{a}
fmatrix_ptr_alloc (m, n) = let
  val (pf_mn | mn) = mul2_size1_size1 (m, n)
  prval () = mul_nat_nat_nat (pf_mn)
  val (pf_gc, pf_arr | p_arr) = array_ptr_alloc_tsz {a} (mn, sizeof<a>)
  prval pf_fmat = fmatrix_v_of_array_v (pf_mn, pf_arr)
in
  (pf_gc, pf_mn, pf_fmat | p_arr)
end // end of [fmatrix_ptr_alloc]

(* ****** ****** *)

implement
fmatrix_ptr_free 
  (pf_gc, pf_mn, pf_fmat | p_fmat) = let
  prval (pf2_mn, pf_arr) = array_v_of_fmatrix_v (pf_fmat)
  prval () = mul_isfun (pf2_mn, pf_mn)
  val () = array_ptr_free (pf_gc, pf_arr | p_fmat)
in
  // nothing
end // end of [fmatrix_ptr_free]

(* ****** ****** *)

implement{a}
fmatrix_ptr_allocfree (m, n) = let
  val (pf_mn | mn) = mul2_size1_size1 (m, n)
  prval () = mul_nat_nat_nat (pf_mn)
  val [l:addr] (pf_gc, pf_arr | p_arr) = array_ptr_alloc_tsz {a} (mn, sizeof<a>)
  prval pf_fmat = fmatrix_v_of_array_v (pf_mn, pf_arr)
in #[l | (
  pf_fmat
| p_arr
, lam (pf_fmat | p_arr) =<lin> let
    prval (pf2_mn, pf_arr) = array_v_of_fmatrix_v (pf_fmat)
    prval () = mul_isfun (pf2_mn, pf_mn)
  in
    array_ptr_free (pf_gc, pf_arr | p_arr)
  end
) ] end // end of [fmatrix_ptr_allocfree]

(* ****** ****** *)

implement{a}
fmatrix_ptr_initialize_elt (base, m, n, x) = () where {
  prval pf_mat = view@ base
  prval (pf_mn1, pf_arr) = array_v_of_fmatrix_v (pf_mat)
  val (pf_mn2 | mn) = mul2_size1_size1 (m, n)
  prval () = mul_nat_nat_nat (pf_mn2)
  prval () = mul_isfun (pf_mn1, pf_mn2)
  var x: a = x
  val () = array_ptr_initialize_elt_tsz {a} (base, mn, x, sizeof<a>)
  prval () = view@ base := fmatrix_v_of_array_v (pf_mn1, pf_arr) 
} // end of [fmatrix_ptr_initialize]

(* ****** ****** *)

// initialization is done column by colmun
implement{a} // worth it???
fmatrix_ptr_initialize_clo
  {v} {m,n} (pf | base, m, n, f) = () where {
  prval pf_mat = view@ base
  prval (pf1_mn, pf_arr) = array_v_of_fmatrix_v (pf_mat)
  val [mn:int] (pf2_mn | mn) = mul2_size1_size1 (m, n)
  prval () = mul_nat_nat_nat (pf2_mn)
  prval () = mul_isfun (pf1_mn, pf2_mn)
//
  typedef clo_t = (!v | &(a?) >> a, sizeLt m, sizeLt n) -<clo> void
//
  fun loop_one {mi:nat | mi <= m} {l:addr} .<mi>. (
      pf_arr: !array_v (a?, mi, l) >> array_v (a, mi, l)
    , pf: !v
    | p: ptr l, f: &clo_t, mi: size_t mi, j: sizeLt n
    ) :<cloref> void = if mi > 0 then let
    prval (pf1_elt, pf2_arr) = array_v_uncons {a?} (pf_arr)
    val () = f (pf | !p, m - mi, j)
    val () = loop_one (pf2_arr, pf | p+sizeof<a>, f, mi - 1, j)
    prval () = pf_arr := array_v_cons {a} (pf1_elt, pf2_arr)
  in
    // nothing
  end else let
    prval () = array_v_unnil {a?} (pf_arr)
    prval () = pf_arr := array_v_nil {a} ()
  in
    // nothing
  end // end of [loop_one]
//
  fun loop_all
    {nj:nat | nj <= n} {p:int} {l:addr} .<nj>. (
    pf_mul: MUL (nj, m, p)
  , pf_arr: !array_v (a?, p, l) >> array_v (a, p, l)
  , pf: !v
  | p: ptr l
  , f: &clo_t
  , nj: size_t nj
  ) :<cloref> void = if nj > 0 then let
    prval () = mul_nat_nat_nat (pf_mul)
    prval pf1_mul = mul_add_const {~1} (pf_mul)
    prval () = mul_nat_nat_nat (pf1_mul)
    val [l1:addr] (pf1_arr, pf2_arr, fpf_arr | p1) =
      array_ptr_split_tsz {a?} (pf_arr | p, m, sizeof<a>)
    val () = loop_one (pf1_arr, pf | p, f, m, n-nj)
    val () = loop_all (pf1_mul, pf2_arr, pf | p1, f, nj-1)
    propdef fpf_p (a:viewt@ype) =
      (array_v (a, m, l), array_v (a, p-m, l1)) -<prf> array_v (a, p, l)
    prval fpf_arr = __cast fpf_arr where {
      extern prfun __cast (fpf: fpf_p (a?)):<prf> fpf_p (a)
    } // end of [prval]
    prval () = pf_arr := fpf_arr (pf1_arr, pf2_arr)
  in
    // nothing
  end else let
    prval MULbas () = pf_mul
    prval () = array_v_unnil {a?} (pf_arr)
    prval () = pf_arr := array_v_nil {a} ()
  in
    // nothing
  end // end of [loop_all]
//
  prval pf_nm = mul_commute (pf1_mn)
  val () = loop_all (pf_nm, pf_arr, pf | &base, f, n)
//
  prval () = view@ base := fmatrix_v_of_array_v (pf1_mn, pf_arr) 
} // end of [fmatrix_ptr_initialize_clo]

(* ****** ****** *)

local

//
// HX: implemented in [libats/CATS/fmatrix.cats]
//
extern
fun fmatrix_ptr_takeout_tsz {a:viewt@ype}
  {m,n:int} {i,j:nat | i < m; j < n} {l0:addr} (
    pf_mat: fmatrix_v (a, m, n, l0)
  | base: ptr l0, m: size_t m, i: size_t i, j: size_t j, tsz: sizeof_t a
  ) :<> [l:addr] (
    a @ l
  , a @ l -<lin,prf> fmatrix_v (a, m, n, l0)
  | ptr l
  ) = "atslib_fmatrix_ptr_takeout_tsz"
// end of [fmatrix_ptr_takeout_tsz]

in

implement{a} fmatrix_ptr_takeout
  (pf_mat | base, m, i, j) = begin
  fmatrix_ptr_takeout_tsz {a} (pf_mat | base, m, i, j, sizeof<a>)
end // end of [fmatrix_ptr_takeout]

implement{a} fmatrix_ptr_get_elt_at
  (base, m, i, j) = x where {
  prval pf_mat = view@ base
  val (pf_elt, fpf_mat | p_elt) =
    fmatrix_ptr_takeout_tsz {a} (pf_mat | &base, m, i, j, sizeof<a>)
  // end of [val]
  val x = !p_elt
  prval () = view@ base := fpf_mat (pf_elt)
} // end of [fmatrix_ptr_get_elt_at]

implement{a} fmatrix_ptr_set_elt_at
  (base, m, i, j, x) = () where {
  prval pf_mat = view@ base
  val (pf_elt, fpf_mat | p_elt) =
    fmatrix_ptr_takeout_tsz {a} (pf_mat | &base, m, i, j, sizeof<a>)
  // end of [val]
  val () = !p_elt := x
  prval () = view@ base := fpf_mat (pf_elt)
} // end of [fmatrix_ptr_set_elt_at]

end // end of [local]

(* ****** ****** *)

implement{a}
fmatrix_ptr_copy {m,n} (A, B, m, n) = let
  val [mn:int] (pf_mn | mn) = mul2_size1_size1 (m, n)
  prval () = mul_nat_nat_nat (pf_mn)
  prval (pf2_mn, pfA_arr) = array_v_of_fmatrix_v {a} {m,n} (view@ A)
  prval (pf3_mn, pfB_arr) = array_v_of_fmatrix_v {a?} {m,n} (view@ B)
  prval () = mul_isfun (pf_mn, pf2_mn)
  prval () = mul_isfun (pf_mn, pf3_mn)
  stavar lA: addr and lB: addr
  prval pfA_arr = pfA_arr: array_v (a, mn, lA)
  prval pfB_arr = pfB_arr: array_v (a?, mn, lB)
  val () = array_ptr_copy_tsz {a} {mn} (A, B, mn, sizeof<a>)
  prval () = view@ A := fmatrix_v_of_array_v {a} {m,n} {mn} (pf2_mn, pfA_arr)
  prval () = view@ B := fmatrix_v_of_array_v {a} {m,n} {mn} (pf3_mn, pfB_arr)
in
  // nothing
end // end of [fmatrix_ptr_copy]

(* ****** ****** *)

// loop proceeds column by column
implement
fmatrix_ptr_foreach_fun_tsz__main
  {a} {v} {vt} {ord} {m,n}
  (pf | M, f, ord, m, n, tsz, env) = if m > 0 then let
  prval (pf_mat, fpf) = GEMAT_v_of_fmatrix_v {a} (view@ M)
  val () = GEMAT_ptr_foreach_fun_tsz__main
    (pf | ORDERcol, M, f, ord, m, n, m, tsz, env)
  prval () = view@ M := fpf (pf_mat)
in
  // nothing
end (* end of [fmatrix_ptr_foreach_fun_tsz__main] *)

(* ****** ****** *)

implement{a}
fmatrix_ptr_foreach_fun {v} (pf | M, f, ord, m, n) = let
  val f = coerce (f) where { extern castfn
    coerce (f: (!v | &a) -<> void) :<> (!v | &a, !ptr) -<> void
  } // end of [where]
in
  fmatrix_ptr_foreach_fun_tsz__main {a} {v} {ptr} (pf | M, f, ord, m, n, sizeof<a>, null)
end // end of [fmatrix_ptr_foreach_fun]

(* ****** ****** *)

implement{a}
fmatrix_ptr_foreach_clo {v} (pf_v | M, f, ord, m, n) = let
  stavar l_f: addr
  val p_f: ptr l_f = &f
  typedef clo_t = (!v | &a) -<clo> void
  viewdef V = @(v, clo_t @ l_f)
  fn app (pf: !V | x: &a, p_f: !ptr l_f):<> void = let
    prval (pf1, pf2) = pf in !p_f (pf1 | x); pf := @(pf1, pf2)
  end // end of [app]
  prval pf = (pf_v, view@ f)
  val () = fmatrix_ptr_foreach_fun_tsz__main
    {a} {V} {ptr l_f} (pf | M, app, ord, m, n, sizeof<a>, p_f)
  prval (pf1, pf2) = pf
  prval () = (pf_v := pf1; view@ f := pf2)
in
  // empty
end // end of [fmatrix_ptr_foreach_clo_tsz]

(* ****** ****** *)

// loop proceeds column by column
implement
fmatrix_ptr_iforeach_fun_tsz__main
  {a} {v} {vt} {ord} {m,n}
  (pf | M, f, ord, m, n, tsz, env) = if m > 0 then let
  prval (pf_mat, fpf) = GEMAT_v_of_fmatrix_v {a} (view@ M)
  val () = GEMAT_ptr_iforeach_fun_tsz__main
    (pf | ORDERcol, M, f, ord, m, n, m, tsz, env)
  prval () = view@ M := fpf (pf_mat)
in
  // nothing
end (* end of [fmatrix_ptr_iforeach_fun_tsz__main] *)

(* ****** ****** *)

implement{a}
fmatrix_ptr_iforeach_fun {v} {ord} {m,n}
  (pf | M, f, ord, m, n) = if m > 0 then let
  prval (pf_mat, fpf) = GEMAT_v_of_fmatrix_v {a} (view@ M)
  val () = GEMAT_ptr_iforeach_fun<a> (pf | ORDERcol, M, f, ord, m, n, m)
  prval () = view@ M := fpf (pf_mat)
in
  // nothing
end (* end of [fmatrix_ptr_iforeach_fun] *)

(* ****** ****** *)

implement{a}
fmatrix_ptr_iforeach_clo {v} {ord} {m,n}
  (pf | M, f, ord, m, n) = if m > 0 then let
  prval (pf_mat, fpf) = GEMAT_v_of_fmatrix_v {a} (view@ M)
  val () = GEMAT_ptr_iforeach_clo<a> (pf | ORDERcol, M, f, ord, m, n, m)
  prval () = view@ M := fpf (pf_mat)
in
  // nothing
end (* end of [fmatrix_ptr_iforeach_clo] *)

(* ****** ****** *)

(* end of [fmatrix.dats] *)
