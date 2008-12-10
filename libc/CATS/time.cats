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
 * Copyright (C) 2002-2008 Hongwei Xi.
 *
 * ATS is  free software;  you can redistribute it and/or modify it under
 * the  terms of the  GNU General Public License as published by the Free
 * Software Foundation; either version 2.1, or (at your option) any later
 * version.
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

#ifndef ATS_LIBC_TIME_CATS
#define ATS_LIBC_TIME_CATS

/* ****** ****** */

#include <time.h>

/* ****** ****** */

#include "ats_types.h"

typedef struct tm ats_tm_struct_type ;

/* ****** ****** */

#include "libc/sys/CATS/types.cats"

/* ****** ****** */

static inline
ats_lint_type
atslib_lint_of_time (time_t t) { return t ; }

static inline
ats_double_type
atslib_double_of_time (time_t t) { return t ; }

/* ****** ****** */

static inline
ats_int_type atslib_tm_sec_get (ats_ptr_type tm) {
  return ((struct tm *)tm)->tm_sec ;
}

static inline
ats_int_type atslib_tm_min_get (ats_ptr_type tm) {
  return ((struct tm *)tm)->tm_min ;
}

static inline
ats_int_type atslib_tm_hour_get (ats_ptr_type tm) {
  return ((struct tm *)tm)->tm_hour ;
}

static inline
ats_int_type atslib_tm_mday_get (ats_ptr_type tm) {
  return ((struct tm *)tm)->tm_mday ;
}

static inline
ats_int_type atslib_tm_mon_get (ats_ptr_type tm) {
  return ((struct tm *)tm)->tm_mon ;
}

static inline
ats_int_type atslib_tm_year_get (ats_ptr_type tm) {
  return ((struct tm *)tm)->tm_year ;
}

static inline
ats_int_type atslib_tm_wday_get (ats_ptr_type tm) {
  return ((struct tm *)tm)->tm_wday ;
}

static inline
ats_int_type atslib_tm_yday_get (ats_ptr_type tm) {
  return ((struct tm *)tm)->tm_yday ;
}

static inline
ats_int_type atslib_tm_isdst_get (ats_ptr_type tm) {
  return ((struct tm *)tm)->tm_isdst ;
}

//

/* ****** ****** */

static inline
ats_time_type
atslib_time_get () { return time((time_t*)0) ; }

static inline
ats_time_type
atslib_time_get_and_set (ats_ref_type p) {
  return time((time_t*)p) ;
}

/* ****** ****** */

static inline
ats_double_type
atslib_difftime (time_t finish, time_t start) {
  return difftime(finish, start) ;
}

/* ****** ****** */

static inline
ats_ptr_type // this function is not reentrant
atslib_ctime (ats_ref_type ntick) { return ctime((time_t*)ntick) ; }

static inline
ats_ptr_type // this function is reentrant
atslib_ctime_r (ats_ref_type ntick, ats_ptr_type p_buf) {
  return ctime_r ((time_t*)ntick, (char*)p_buf) ;
}

/* ****** ****** */

static inline
ats_ref_type
atslib_localtime (ats_ptr_type time) {
  return localtime ((time_t *)time) ;
}

static inline
ats_void_type
atslib_localtime_r (ats_ptr_type time, ats_ptr_type tm) {
  localtime_r ((time_t *)time, (struct tm *)tm) ; return ;
}

/* ****** ****** */

static inline
ats_lint_type
atslib_lint_of_clock (clock_t t) { return t ; }

static inline
ats_double_type
atslib_double_of_clock (clock_t t) { return t ; }

/* ****** ****** */

static inline
ats_clock_type
atslib_clock (void) { return clock (); }

/* ****** ****** */

#endif /* ATS_LIBC_TIME_CATS */

/* end of [time.cats] */
