/************************************************************************/
/*                                                                      */
/*                         Applied Type System                          */
/*                                                                      */
/*                              Hongwei Xi                              */
/*                                                                      */
/************************************************************************/

/*
 * ATS - Unleashing the Potential of Types!
 *
 * Copyright (C) 2002-2009 Hongwei Xi.
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
 */

/* ****** ****** */

/* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) */

/* ****** ****** */

#ifndef ATS_PRELUDE_SIZETYPE_CATS
#define ATS_PRELUDE_SIZETYPE_CATS

/* ****** ****** */

#include <stdio.h>

/* ****** ****** */

#include "ats_types.h"

/* ****** ****** */

static inline
ats_int_type
atspre_int_of_size (ats_size_type sz) {
/*
  if (sz > INTMAX) {
    fprintf (stderr, "[ats_int_of_size(%li)] failed\n", sz) ; exit (1) ;
  }
*/
  return ((ats_int_type)sz) ;
} /* end of [atspre_int_of_size] */

/* ****** ****** */

static inline
ats_size_type
atspre_size1_of_size (ats_size_type sz) { return sz ; }

static inline
ats_size_type
atspre_size_of_int (ats_int_type i) { return (ats_size_type)i ; }

static inline
ats_size_type
atspre_size_of_ssize
  (ats_ssize_type x) { return (ats_size_type)x ; }

static inline
ats_size_type
atspre_size_of_ptrdiff
  (ats_ptrdiff_type x) { return (ats_size_type)x ; }

/* ****** ****** */

// print functions

static inline
ats_void_type
atspre_fprint_size (ats_ptr_type out, ats_size_type sz) {
  int n ;
  n = fprintf ((FILE*)out, "%ld", sz) ;
  if (n < 0) {
    ats_exit_errmsg (n, "exit(ATS): [fprint_size] failed.\n") ;
  } /* end of [if] */
  return ;
}

static inline
ats_void_type
atspre_print_size (ats_size_type sz) {
  atspre_stdout_view_get () ;
  atspre_fprint_size (stdout, sz) ;
  atspre_stdout_view_set () ;
  return ;
}

static inline
ats_void_type
atspre_prerr_size (ats_size_type sz) {
  atspre_stderr_view_get () ;
  atspre_fprint_int (stderr, sz) ;
  atspre_stderr_view_set () ;
  return ;
}

/* ****** ****** */

static inline
ats_size_type
atspre_succ_size (ats_size_type sz) { return (sz + 1) ; }

static inline
ats_size_type
atspre_pred_size (ats_size_type sz) { return (sz - 1) ; }

/* ****** ****** */

static inline
ats_size_type
atspre_add_size_size (ats_size_type sz1, ats_size_type sz2) {
  return (sz1 + sz2) ;
}

static inline
ats_size_type
atspre_add_size_int (ats_size_type sz1, ats_int_type i2) {
  return (sz1 + i2) ;
}

//

static inline
ats_size_type
atspre_sub_size_size (ats_size_type sz1, ats_size_type sz2) {
  return (sz1 - sz2) ;
}

static inline
ats_size_type
atspre_sub_size_int (ats_size_type sz1, ats_int_type i2) {
  return (sz1 - i2) ;
}

//

static inline
ats_size_type
atspre_mul_size_size (ats_size_type sz1, ats_size_type sz2) {
  return (sz1 * sz2) ;
}

#define atspre_mul1_size_size atspre_mul_size_size
#define atspre_mul2_size_size atspre_mul_size_size

//

static inline
ats_size_type
atspre_mod_size_size (ats_size_type sz1, ats_size_type sz2) {
  return (sz1 % sz2) ;
} /* end of [atspre_mod_size_size] */

#define atspre_mod1_size_size atspre_mod_size_size

/* ****** ****** */

static inline
ats_bool_type
atspre_lt_size_size
  (ats_size_type sz1, ats_size_type sz2) {
  return (sz1 < sz2 ? ats_true_bool : ats_false_bool) ;
} /* end of [atspre_lt_size_size] */

static inline
ats_bool_type
atspre_lte_size_size
  (ats_size_type sz1, ats_size_type sz2) {
  return (sz1 <= sz2 ? ats_true_bool : ats_false_bool) ;
} /* end of [atspre_lte_size_size] */

//

static inline
ats_bool_type
atspre_gt_size_size
  (ats_size_type sz1, ats_size_type sz2) {
  return (sz1 > sz2 ? ats_true_bool : ats_false_bool) ;
} /* end of [atspre_gt_size_size] */

static inline
ats_bool_type
atspre_gt_size_int
  (ats_size_type sz1, ats_int_type i2) {
  return (sz1 > i2 ? ats_true_bool : ats_false_bool) ;
} /* end of [atspre_gt_size_int] */

//

static inline
ats_bool_type
atspre_gte_size_size
  (ats_size_type sz1, ats_size_type sz2) {
  return (sz1 >= sz2 ? ats_true_bool : ats_false_bool) ;
} /* end of [atspre_gte_size_size] */

static inline
ats_bool_type
atspre_gte_size_int
  (ats_size_type sz1, ats_int_type i2) {
  return (sz1 >= i2 ? ats_true_bool : ats_false_bool) ;
} /* end of [atspre_gte_size_int] */

/* ****** ****** */

static inline
ats_bool_type
atspre_eq_size_size
  (ats_size_type sz1, ats_size_type sz2) {
  return (sz1 == sz2 ? ats_true_bool : ats_false_bool) ;
} /* end of [atspre_eq_size_size] */

static inline
ats_bool_type
atspre_neq_size_size
  (ats_size_type sz1, ats_size_type sz2) {
  return (sz1 != sz2 ? ats_true_bool : ats_false_bool) ;
} /* end of [atspre_neq_size_size] */


/* ****** ****** */

// signed size type

/* ****** ****** */

static inline
ats_ssize_type atspre_ssize_of_int (ats_int_type i) {
  return (ats_ssize_type)i ;
}

static inline
ats_ssize_type atspre_ssize_of_size (ats_size_type sz) {
  return (ats_ssize_type)sz ;
}

/* ****** ****** */

static inline
ats_bool_type
atspre_lt_ssize_int
  (ats_ssize_type sz2, ats_int_type i2) {
  return (sz2 < i2 ? ats_true_bool : ats_false_bool) ;
} /* end of [atspre_lt_ssize_int] */

static inline
ats_bool_type
atspre_lte_ssize_int
  (ats_ssize_type sz2, ats_int_type i2) {
  return (sz2 <= i2 ? ats_true_bool : ats_false_bool) ;
} /* end of [atspre_lte_ssize_int] */

/* ****** ****** */

static inline
ats_bool_type
atspre_gt_ssize_int
  (ats_ssize_type sz2, ats_int_type i2) {
  return (sz2 > i2 ? ats_true_bool : ats_false_bool) ;
} /* end of [atspre_gt_ssize_int] */

static inline
ats_bool_type
atspre_gte_ssize_int
  (ats_ssize_type sz2, ats_int_type i2) {
  return (sz2 >= i2 ? ats_true_bool : ats_false_bool) ;
} /* end of [atspre_gte_ssize_int] */

/* ****** ****** */

#endif /* ATS_PRELUDE_SIZETYPE_CATS */

/* end of [sizetype.cats] */
