/************************************************************************/
/*                                                                      */
/*                         Applied Type System                          */
/*                                                                      */
/*                              Hongwei Xi                              */
/*                                                                      */
/************************************************************************/

/*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2008 Hongwei Xi.
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
*/

/* ****** ****** */

/* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) */

/* ****** ****** */

#ifndef ATS_PRELUDE_INTEGER_PTR_CATS
#define ATS_PRELUDE_INTEGER_PTR_CATS

/* ****** ****** */

#include <stdio.h>

/* ****** ****** */

#include "ats_exception.h"
#include "ats_memory.h"
#include "ats_types.h"

/* ****** ****** */

// intptr and uintptr

typedef ats_ptr_type ats_intptr_type ;
typedef ats_ptr_type ats_uintptr_type ;

/* ****** ****** */

// intptr_t: integers of one word size

static inline
ats_intptr_type
atspre_intptr_of_int (ats_int_type i) {
  return (ats_intptr_type)(intptr_t)i ;
}

// ------ ------

static inline
ats_intptr_type
atspre_abs_intptr (ats_intptr_type i) {
  return (i >= 0 ? i : (ats_intptr_type)(-(intptr_t)i)) ;
}

static inline
ats_intptr_type
atspre_neg_intptr (ats_intptr_type i) {
  return (ats_intptr_type)(-(intptr_t)i) ;
}

static inline
ats_intptr_type
atspre_succ_intptr (ats_intptr_type i) {
  return (ats_intptr_type)((intptr_t)i + 1) ;
}

static inline
ats_intptr_type
atspre_pred_intptr (ats_intptr_type i) {
  return (ats_intptr_type)((intptr_t)i - 1) ;
}

static inline
ats_intptr_type
atspre_add_intptr_intptr (ats_intptr_type i1, ats_intptr_type i2) {
  return (ats_intptr_type)((intptr_t)i1 + (intptr_t)i2) ;
}

static inline
ats_intptr_type
atspre_sub_intptr_intptr (ats_intptr_type i1, ats_intptr_type i2) {
  return (ats_intptr_type)((intptr_t)i1 - (intptr_t)i2) ;
}

static inline
ats_intptr_type
atspre_mul_intptr_intptr (ats_intptr_type i1, ats_intptr_type i2) {
  return (ats_intptr_type)((intptr_t)i1 * (intptr_t)i2) ;
}

static inline
ats_intptr_type
atspre_div_intptr_intptr (ats_intptr_type i1, ats_intptr_type i2) {
  return (ats_intptr_type)((intptr_t)i1 / (intptr_t)i2) ;
}

static inline
ats_intptr_type
atspre_mod_intptr_intptr (ats_intptr_type i1, ats_intptr_type i2) {
  return (ats_intptr_type)((intptr_t)i1 % (intptr_t)i2) ;
}

static inline
ats_bool_type
atspre_lt_intptr_intptr (ats_intptr_type i1, ats_intptr_type i2) {
  return ((intptr_t)i1 < (intptr_t)i2) ;
}

static inline
ats_bool_type
atspre_lte_intptr_intptr (ats_intptr_type i1, ats_intptr_type i2) {
  return ((intptr_t)i1 <= (intptr_t)i2) ;
}

static inline
ats_bool_type
atspre_gt_intptr_intptr (ats_intptr_type i1, ats_intptr_type i2) {
  return ((intptr_t)i1 > (intptr_t)i2) ;
}

static inline
ats_bool_type
atspre_gte_intptr_intptr (ats_intptr_type i1, ats_intptr_type i2) {
  return ((intptr_t)i1 >= (intptr_t)i2) ;
}

static inline
ats_bool_type
atspre_eq_intptr_intptr (ats_intptr_type i1, ats_intptr_type i2) {
  return ((intptr_t)i1 == (intptr_t)i2) ;
}

static inline
ats_bool_type
atspre_neq_intptr_intptr (ats_intptr_type i1, ats_intptr_type i2) {
  return ((intptr_t)i1 != (intptr_t)i2) ;
}

// print functions

static inline
ats_void_type
atspre_fprint_intptr (ats_ptr_type out, ats_intptr_type i) {
  int n ;
  n = fprintf ((FILE*)out, "%lli", (ats_llint_type)(intptr_t)i) ;
  if (n < 0) {
    ats_exit_errmsg (n, "Exit: [fprint_intptr] failed.\n") ;
  }
  return ;
}

static inline
ats_void_type
atspre_print_intptr (ats_intptr_type i) {
  atspre_stdout_view_get () ;
  atspre_fprint_intptr (stdout, i) ;
  atspre_stdout_view_set () ;
  return ;
}

static inline
ats_void_type
atspre_prerr_intptr (ats_intptr_type i) {
  atspre_stderr_view_get () ;
  atspre_fprint_intptr (stderr, i) ;
  atspre_stderr_view_set () ;
  return ;
}

/* ****** ****** */

// uintptr_t: unsigned integers of one word size

static inline
ats_uintptr_type
atspre_uintptr_of_int (ats_int_type i) {
  return (ats_uintptr_type)(uintptr_t)i ;
}

// ------ ------

static inline
ats_uintptr_type
atspre_succ_uintptr (ats_uintptr_type i) {
  return (ats_uintptr_type)((uintptr_t)i + 1) ;
}

static inline
ats_uintptr_type
atspre_pred_uintptr (ats_uintptr_type i) {
  return (ats_uintptr_type)((uintptr_t)i - 1) ;
}

static inline
ats_uintptr_type
atspre_add_uintptr_uintptr (ats_uintptr_type i1, ats_uintptr_type i2) {
  return (ats_uintptr_type)((uintptr_t)i1 + (uintptr_t)i2) ;
}

static inline
ats_uintptr_type
atspre_sub_uintptr_uintptr (ats_uintptr_type i1, ats_uintptr_type i2) {
  return (ats_uintptr_type)((uintptr_t)i1 - (uintptr_t)i2) ;
}

static inline
ats_uintptr_type
atspre_mul_uintptr_uintptr (ats_uintptr_type i1, ats_uintptr_type i2) {
  return (ats_uintptr_type)((uintptr_t)i1 * (uintptr_t)i2) ;
}

static inline
ats_uintptr_type
atspre_div_uintptr_uintptr (ats_uintptr_type i1, ats_uintptr_type i2) {
  return (ats_uintptr_type)((uintptr_t)i1 / (uintptr_t)i2) ;
}

static inline
ats_uintptr_type
atspre_mod_uintptr_uintptr (ats_uintptr_type i1, ats_uintptr_type i2) {
  return (ats_uintptr_type)((uintptr_t)i1 % (uintptr_t)i2) ;
}

static inline
ats_bool_type
atspre_lt_uintptr_uintptr (ats_uintptr_type i1, ats_uintptr_type i2) {
  return ((uintptr_t)i1 < (uintptr_t)i2) ;
}

static inline
ats_bool_type
atspre_lte_uintptr_uintptr (ats_uintptr_type i1, ats_uintptr_type i2) {
  return ((uintptr_t)i1 <= (uintptr_t)i2) ;
}

static inline
ats_bool_type
atspre_gt_uintptr_uintptr (ats_uintptr_type i1, ats_uintptr_type i2) {
  return ((uintptr_t)i1 > (uintptr_t)i2) ;
}

static inline
ats_bool_type
atspre_gte_uintptr_uintptr (ats_uintptr_type i1, ats_uintptr_type i2) {
  return ((uintptr_t)i1 >= (uintptr_t)i2) ;
}

static inline
ats_bool_type
atspre_eq_uintptr_uintptr (ats_uintptr_type i1, ats_uintptr_type i2) {
  return ((uintptr_t)i1 == (uintptr_t)i2) ;
}

static inline
ats_bool_type
atspre_neq_uintptr_uintptr (ats_uintptr_type i1, ats_uintptr_type i2) {
  return ((uintptr_t)i1 != (uintptr_t)i2) ;
}

// bitwise operations

static inline
ats_uintptr_type
atspre_lnot_uintptr
  (ats_uintptr_type x) {
  return (ats_uintptr_type)(~(uintptr_t)x) ;
}

static inline
ats_uintptr_type
atspre_land_uintptr_uintptr
  (ats_uintptr_type x, ats_uintptr_type y) {
  return (ats_uintptr_type)((uintptr_t)x & (uintptr_t)y) ;
}

static inline
ats_uintptr_type
atspre_lor_uintptr_uintptr
  (ats_uintptr_type x, ats_uintptr_type y) {
  return (ats_uintptr_type)((uintptr_t)x | (uintptr_t)y) ;
}

static inline
ats_uintptr_type
atspre_lxor_uintptr_uintptr
  (ats_uintptr_type x, ats_uintptr_type y) {
  return (ats_uintptr_type)((uintptr_t)x ^ (uintptr_t)y) ;
}

static inline
ats_uintptr_type
atspre_lsl_uintptr_int1
  (ats_uintptr_type x, ats_int_type n) {
  return (ats_uintptr_type)((uintptr_t)x << n) ;
}

static inline
ats_uintptr_type
atspre_lsr_uintptr_int1
  (ats_uintptr_type x, ats_int_type n) {
  return (ats_uintptr_type)((uintptr_t)x >> n) ;
}

// print functions

static inline
ats_void_type
atspre_fprint_uintptr (ats_ptr_type out, ats_uintptr_type u) {
  int n ;
  n = fprintf ((FILE*)out, "%llu", (ats_ullint_type)(uintptr_t)u) ;
  if (n < 0) {
    ats_exit_errmsg (n, "Exit: [fprint_uintptr] failed.\n") ;
  }
  return ;
}

static inline
ats_void_type
atspre_print_uintptr (ats_uintptr_type u) {
  atspre_stdout_view_get () ;
  atspre_fprint_uintptr (stdout, u) ;
  atspre_stdout_view_set () ;
  return ;
}

static inline
ats_void_type
atspre_prerr_uintptr (ats_uintptr_type u) {
  atspre_stderr_view_get () ;
  atspre_fprint_uintptr (stderr, u) ;
  atspre_stderr_view_set () ;
  return ;
}

/* ****** ****** */

#endif /* ATS_PRELUDE_INTEGER_PTR_CATS */

/* end of [integer.cats] */
