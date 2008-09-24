/***********************************************************************/
/*                                                                     */
/*                        Applied Type System                          */
/*                                                                     */
/*                             Hongwei Xi                              */
/*                                                                     */
/***********************************************************************/

/*
 * ATS/Anairiats - Unleashing the Potential of Types!
 *
 * Copyright (C) 2002-2008 Hongwei Xi.
 *
 * All rights reserved
 *
 * ATS is free software;  you can  redistribute it and/or modify it under
 * the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
 * Free Software Foundation; either version 3, or (at  your  option)  any
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
 */

/* ****** ****** */

// February 2008
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

/* ****** ****** */

/* ats_solver_fm: a solver based on the FM approach for linear constraints

/* ****** ****** */

#ifndef ATS_SRC_SOLVER_FM_CATS
#define ATS_SRC_SOLVER_FM_CATS

/* ****** ****** */

#include "ats_intinf.cats"

/* ****** ****** */

static inline
ats_int_type
ats_solver_fm_i0nt_of_int (ats_int_type i) {
  return (i) ;
}

static inline
ats_int_type
ats_solver_fm_i0nt_of_intinf (ats_mpz_ptr_type i) {
  return ats_intinf_get_int (i) ;
}

/* ****** ****** */

static inline
ats_bool_type
ats_solver_fm_gt_i0nt_int (ats_int_type i0, ats_int_type i) {
  return (i0 > i ? ats_true_bool : ats_false_bool) ;
}

static inline
ats_bool_type
ats_solver_fm_gte_i0nt_int (ats_int_type i0, ats_int_type i) {
  return (i0 >= i ? ats_true_bool : ats_false_bool) ;
}

static inline
ats_bool_type
ats_solver_fm_lt_i0nt_int (ats_int_type i0, ats_int_type i) {
  return (i0 < i ? ats_true_bool : ats_false_bool) ;
}

static inline
ats_bool_type
ats_solver_fm_lte_i0nt_int (ats_int_type i0, ats_int_type i) {
  return (i0 <= i ? ats_true_bool : ats_false_bool) ;
}

static inline
ats_bool_type
ats_solver_fm_eq_i0nt_int (ats_int_type i0, ats_int_type i) {
  return (i0 == i ? ats_true_bool : ats_false_bool) ;
}

static inline
ats_bool_type
ats_solver_fm_neq_i0nt_int (ats_int_type i0, ats_int_type i) {
  return (i0 != i ? ats_true_bool : ats_false_bool) ;
}

//

static inline
ats_bool_type
ats_solver_fm_gt_i0nt_i0nt (ats_int_type i1, ats_int_type i2) {
  return (i1 > i2 ? ats_true_bool : ats_false_bool) ;
}

static inline
ats_bool_type
ats_solver_fm_lt_i0nt_i0nt (ats_int_type i1, ats_int_type i2) {
  return (i1 < i2 ? ats_true_bool : ats_false_bool) ;
}

//

static inline
ats_int_type
ats_solver_fm_neg_i0nt (ats_int_type i) {
  return (-i) ;
}

static inline
ats_int_type
ats_solver_fm_add_i0nt_i0nt (ats_int_type i1, ats_int_type i2) {
  return (i1 + i2) ;
}

static inline
ats_int_type
ats_solver_fm_sub_i0nt_i0nt (ats_int_type i1, ats_int_type i2) {
  return (i1 - i2) ;
}

static inline
ats_int_type
ats_solver_fm_mul_i0nt_i0nt (ats_int_type i1, ats_int_type i2) {
  return (i1 * i2) ;
}

static inline
ats_int_type
ats_solver_fm_div_i0nt_i0nt (ats_int_type i1, ats_int_type i2) {
  return (i1 / i2) ;
}

//

static inline
ats_int_type
ats_solver_fm_succ_i0nt (ats_int_type i) {
  return (i + 1) ;
}

static inline
ats_int_type
ats_solver_fm_pred_i0nt (ats_int_type i) {
  return (i - 1) ;
}

//

static inline
ats_int_type
ats_solver_fm_mod_i0nt_i0nt (ats_int_type i1, ats_int_type i2) {
  return (i1 % i2) ;
}

static inline
ats_int_type
ats_solver_fm_gcd_i0nt_i0nt (ats_int_type i1, ats_int_type i2) {
  int tmp ;
  if (i1 < 0) i1 = -i1 ;
  if (i2 < 0) i2 = -i2 ;

  while (1) {
    if (i2 == 0) return i1; tmp = i1 % i2 ; i1 = i2 ; i2 = tmp ;
  }
  return 0 ; /* deadcode */
}

//

static inline
ats_void_type
ats_solver_fm_fprint_i0nt (ats_ptr_type out, ats_int_type i) {
  fprintf ((FILE *)out, "%i", i) ; return ;
}

/* ****** ****** */

static inline
ats_ptr_type
ats_solver_fm_intvecptr_make_view_ptr (ats_ptr_type p) {
  return p ;
}

static inline
ats_void_type
ats_solver_fm_intvecptr_free (ats_ptr_type p) {
  ATS_FREE (p) ; return ;
}

/* ****** ****** */

static inline
ats_ptr_type
ats_solver_fm_intvec_ptr_make (ats_int_type n) {
  int *p ;
  int nbytes = n * sizeof(ats_int_type) ;
  p = ATS_MALLOC (nbytes) ;
  return memset (p, 0, nbytes) ;
}

/* ****** ****** */

#endif // ATS_SRC_COUNTER_CATS

/* end of [ats_solver_fm.cats] */


