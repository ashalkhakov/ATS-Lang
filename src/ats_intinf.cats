/***********************************************************************/
/*                                                                     */
/*                        Applied Type System                          */
/*                                                                     */
/*                             Hongwei Xi                              */
/*                                                                     */
/***********************************************************************/

/*
** ATS/Anairiats - Unleashing the Potential of Types!
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
** Free Software Foundation; either version 3, or (at  your  option)  any
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
*/

/* ****** ****** */
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// July 2007
//
/* ****** ****** */

#ifndef ATS_SRC_INTINF_CATS
#define ATS_SRC_INTINF_CATS

/* ****** ****** */

#include "config.h" /* automatically generated by [configure] */

/* ****** ****** */

#include "ats_memory.h"
#include "ats_types.h"

/* ****** ****** */

// #undef HAVE_GMP_H // testing
#ifdef HAVE_GMP_H
#include "libc/CATS/gmp.cats"
#else
#include "ats_gmp.cats"
#endif

/* ****** ****** */
//
// HX: [mpz_ptr] is defined in [gmp.h]
//
/* ****** ****** */

ATSinline()
ats_int_type
atsopt_get_int (
  ats_mpz_ptr_type x
) {
  return mpz_get_si ((mpz_ptr)x) ;
} // end of [atsopt_get_int]

/* ****** ****** */

ATSinline()
ats_bool_type
atsopt_lt_intinf_int (
  ats_mpz_ptr_type x, ats_int_type y
) {
  if (mpz_cmp_si((mpz_ptr)x, y) < 0) return ats_true_bool ;
  return ats_false_bool ;
} // end of [atsopt_lt_intinf_int]

ATSinline()
ats_bool_type
atsopt_lt_intinf_intinf (
  ats_mpz_ptr_type x, ats_mpz_ptr_type y
) {
  if (mpz_cmp((mpz_ptr)x, (mpz_ptr)y) < 0) return ats_true_bool ;
  return ats_false_bool ;
} // end of [atsopt_lt_intinf_intinf]

ATSinline()
ats_bool_type
atsopt_lte_intinf_int (
  ats_mpz_ptr_type x, ats_int_type y
) {
  if (mpz_cmp_si((mpz_ptr)x, y) <= 0) return ats_true_bool ;
  return ats_false_bool ;
} // end of [atsopt_lte_intinf_int]

ATSinline()
ats_bool_type
atsopt_lte_intinf_intinf (
  ats_mpz_ptr_type x, ats_mpz_ptr_type y
) {
  if (mpz_cmp((mpz_ptr)x, (mpz_ptr)y) <= 0) return ats_true_bool ;
  return ats_false_bool ;
} // end of [atsopt_lte_intinf_intinf]

/* ****** ****** */

ATSinline()
ats_bool_type
atsopt_gt_intinf_int (
  ats_mpz_ptr_type x, ats_int_type y
) {
  if (mpz_cmp_si((mpz_ptr)x, y) > 0) return ats_true_bool ;
  return ats_false_bool ;
} // end of [atsopt_gt_intinf_int]

ATSinline()
ats_bool_type
atsopt_gt_intinf_intinf (
  ats_mpz_ptr_type x, ats_mpz_ptr_type y
) {
  if (mpz_cmp((mpz_ptr)x, (mpz_ptr)y) > 0) return ats_true_bool ;
  return ats_false_bool ;
} // end of [atsopt_gt_intinf_intinf]

ATSinline()
ats_bool_type
atsopt_gte_intinf_int (
  ats_mpz_ptr_type x, ats_int_type y
) {
  if (mpz_cmp_si((mpz_ptr)x, y) >= 0) return ats_true_bool ;
  return ats_false_bool ;
} // end of [atsopt_gte_intinf_int]

ATSinline()
ats_bool_type
atsopt_gte_intinf_intinf (
  ats_mpz_ptr_type x, ats_mpz_ptr_type y
) {
  if (mpz_cmp((mpz_ptr)x, (mpz_ptr)y) >= 0) return ats_true_bool ;
  return ats_false_bool ;
} // end of [atsopt_gte_intinf_intinf]

/* ****** ****** */

ATSinline()
ats_bool_type
atsopt_eq_intinf_int (
  ats_mpz_ptr_type x, ats_int_type y
) {
  if (mpz_cmp_si((mpz_ptr)x, (int)y) == 0) return ats_true_bool ;
  return ats_false_bool ;
} // end of [atsopt_eq_intinf_int]

ATSinline()
ats_bool_type
atsopt_eq_int_intinf (
  ats_int_type x, ats_mpz_ptr_type y
) {
  if (mpz_cmp_si((mpz_ptr)y, (int)x) == 0) return ats_true_bool ;
  return ats_false_bool ;
} // end of [atsopt_eq_int_intinf]

ATSinline()
ats_bool_type
atsopt_eq_intinf_intinf (
  ats_mpz_ptr_type x, ats_mpz_ptr_type y
) {
  if (mpz_cmp((mpz_ptr)x, (mpz_ptr)y) == 0) return ats_true_bool ;
  return ats_false_bool ;
} // end of [atsopt_eq_intinf_intinf]

ATSinline()
ats_bool_type
atsopt_neq_intinf_int (
  ats_mpz_ptr_type x, ats_int_type y
) {
  if (mpz_cmp_si((mpz_ptr)x, (int)y) != 0) return ats_true_bool ;
  return ats_false_bool ;
} // end of [atsopt_neq_intinf_int]

ATSinline()
ats_bool_type
atsopt_neq_intinf_intinf (
  ats_mpz_ptr_type x, ats_mpz_ptr_type y
) {
  if (mpz_cmp((mpz_ptr)x, (mpz_ptr)y) != 0) return ats_true_bool ;
  return ats_false_bool ;
} // end of [atsopt_neq_intinf_intinf]

/* ****** ****** */

ATSinline()
ats_int_type
atsopt_compare_intinf_intinf
  (ats_mpz_ptr_type x, ats_mpz_ptr_type y) {
  return mpz_cmp((mpz_ptr)x, (mpz_ptr)y) ;
} // end of [atsopt_compare_intinf_intinf]

/* ****** ****** */

ATSinline()
ats_mpz_ptr_type
atsopt_neg_intinf
  (ats_mpz_ptr_type x) {
  mpz_ptr ans = ATS_MALLOC (sizeof(ats_mpz_viewt0ype));
  mpz_init((mpz_ptr)ans);
  mpz_neg (ans, (mpz_ptr)x) ;
  return ans ;
} // end of [atsopt_neg_intinf]

ATSinline()
ats_mpz_ptr_type
atsopt_add_intinf_intinf (
  ats_mpz_ptr_type x, ats_mpz_ptr_type y
) {
  mpz_ptr ans = ATS_MALLOC (sizeof(ats_mpz_viewt0ype)) ;
  mpz_init((mpz_ptr)ans) ;
  mpz_add ((mpz_ptr)ans, (mpz_ptr)x, (mpz_ptr)y) ;
  return ans ;
} // end of [atsopt_add_intinf_intinf]

ATSinline()
ats_mpz_ptr_type
atsopt_sub_intinf_intinf (
  ats_mpz_ptr_type x, ats_mpz_ptr_type y
) {
  mpz_ptr ans = ATS_MALLOC (sizeof(ats_mpz_viewt0ype)) ;
  mpz_init((mpz_ptr)ans) ;
  mpz_sub ((mpz_ptr)ans, (mpz_ptr)x, (mpz_ptr)y) ;
  return ans ;
} // end of [atsopt_sub_intinf_intinf]

ATSinline()
ats_mpz_ptr_type
atsopt_mul_intinf_intinf (
  ats_mpz_ptr_type x, ats_mpz_ptr_type y
) {
  mpz_ptr ans = ATS_MALLOC (sizeof(ats_mpz_viewt0ype)) ;
  mpz_init((mpz_ptr)ans) ;
  mpz_mul ((mpz_ptr)ans, (mpz_ptr)x, (mpz_ptr)y) ;
  return ans;
} // end of [atsopt_mul_intinf_intinf]

/* ****** ****** */
//
// HX: declared in [prelude/CATS/integer.cats]
//
extern
ats_ptr_type
atspre_tostrptr_llint (ats_llint_type x) ;

ATSinline()
ats_ptr_type
atsopt_tostring_intinf
  (ats_mpz_ptr_type x) {
#ifdef HAVE_GMP_H
  return mpz_get_str((char*)0, 10/*base*/, (mpz_ptr)x) ;
#else // HAVE_GMP_H
/*
// HX: the following is in [ats_gmp.cats]:
// typedef ats_llint_type mpz ; typedef mpz *mpz_ptr ;
*/
  return atspre_tostrptr_llint (*(mpz_ptr)x) ;
#endif // HAVE_GMP_H
} // end of [atsopt_tostring_intinf]

/* ****** ****** */

#endif // ATS_SRC_INTINF_CATS

/* end of [ats_intinf.cats] */
