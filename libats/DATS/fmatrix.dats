(*
**
** An interface for ATS to interact with BLAS and LAPACK
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Contributed by Shivkumar Chandrasekaran (shiv AT ece DOT ucsb DOT edu)
**
** Time: Summer, 2009
**
*)

(* ****** ****** *)

staload "libats/SATS/fmatrix.sats"

(* ****** ****** *)

implement fmatrix_ptr_initialize_elt_tsz {a}
  (base, m, n, x, tsz) = () where {
  prval pf_mat = view@ base
  prval (pf_mn1, pf_arr) = array_v_of_fmatrix_v (pf_mat)
  val (pf_mn2 | mn) = op imul2 (m, n)
  prval () = mul_nat_nat_nat (pf_mn2)
  prval () = mul_isfun (pf_mn1, pf_mn2)
  val mn = size1_of_int1 (mn)
  val () = array_ptr_initialize_elt_tsz {a} (base, mn, x, tsz)
  prval () = view@ base := fmatrix_v_of_array_v (pf_mn1, pf_arr) 
} // end of [fmatrix_ptr_initialize]

(* ****** ****** *)

// initialization is done column by colmun
implement // worth it???
  fmatrix_ptr_initialize_clo_tsz {a} {v} {m,n}
  (pf | base, m, n, f, tsz) = () where {
  prval pf_mat = view@ base
  prval (pf1_mn, pf_arr) = array_v_of_fmatrix_v (pf_mat)
  val [mn:int] (pf2_mn | mn) = op imul2 (m, n)
  prval () = mul_nat_nat_nat (pf2_mn)
  prval () = mul_isfun (pf1_mn, pf2_mn)
//
  typedef clo_t = (!v | &(a?) >> a, natLt m, natLt n) -<clo> void
//
  fun loop_one {mi:nat | mi <= m} {l:addr} .<mi>. (
      pf_arr: !array_v (a?, mi, l) >> array_v (a, mi, l)
    , pf: !v
    | p: ptr l, f: &clo_t, mi: int mi, j: natLt n
    ) :<cloref> void = if mi > 0 then let
    prval (pf1_elt, pf2_arr) = array_v_uncons {a?} (pf_arr)
    val () = f (pf | !p, m - mi, j)
    val () = loop_one (pf2_arr, pf | p+tsz, f, mi - 1, j)
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
  val m_sz = size1_of_int1 m
  fun loop_all
    {nj:nat | nj <= n} {p:int} {l:addr} .<nj>. (
    pf_mul: MUL (nj, m, p)
  , pf_arr: !array_v (a?, p, l) >> array_v (a, p, l)
  , pf: !v
  | p: ptr l
  , f: &clo_t
  , nj: int (nj)
  ) :<cloref> void = if nj > 0 then let
    prval () = mul_nat_nat_nat (pf_mul)
    prval pf1_mul = mul_add_const {~1} (pf_mul)
    prval () = mul_nat_nat_nat (pf1_mul)
    val [l1:addr] (pf1_arr, pf2_arr, fpf_arr | p1) =
      array_ptr_split_tsz {a?} (pf_arr | p, m_sz, tsz)
    val () = loop_one (pf1_arr, pf | p, f, m, n-nj)
    val () = loop_all (pf1_mul, pf2_arr, pf | p1, f, nj-1)
    propdef fpf_p (a:t@ype) =
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
} // end of [fmatrix_ptr_initialize_clo_tsz]

(* ****** ****** *)

implement{a}
  fmatrix_ptr_takeout (pf_mat | base, i, m, j) = begin
  fmatrix_ptr_takeout_tsz {a} (pf_mat | base, i, m, j, sizeof<a>)
end // end of [fmatrix_ptr_takeout]

(* ****** ****** *)

implement{a}
  fmatrix_ptr_get_elt_at
  (base, i, m, j) = x where {
  prval pf_mat = view@ base
  val (pf_elt, fpf_mat | p_elt) =
    fmatrix_ptr_takeout_tsz {a} (pf_mat | &base, i, m, j, sizeof<a>)
  // end of [val]
  val x = !p_elt
  prval () = view@ base := fpf_mat (pf_elt)
} // end of [fmatrix_ptr_get_elt_at]

implement{a}
  fmatrix_ptr_set_elt_at
  (base, i, m, j, x) = () where {
  prval pf_mat = view@ base
  val (pf_elt, fpf_mat | p_elt) =
    fmatrix_ptr_takeout_tsz {a} (pf_mat | &base, i, m, j, sizeof<a>)
  // end of [val]
  val () = !p_elt := x
  prval () = view@ base := fpf_mat (pf_elt)
} // end of [fmatrix_ptr_set_elt_at]

(* ****** ****** *)

(* end of [fmatrix.dats] *)
