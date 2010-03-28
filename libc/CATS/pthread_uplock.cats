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
** ATS is  free software;  you can redistribute it and/or modify it under
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
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*/

/* ****** ****** */

/* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) */

/* ****** ****** */

/*
** A linking error is issued if a user does not define
** [_ATS_MULTITHREAD] while triing to use them anyways
*/

/*
#ifndef _ATS_MULTITHREAD
#error "[pthread_uplock.cats]: _ATS_MULTITHREAD is undefined!"
#endif
*/

/* ****** ****** */

#ifndef ATS_LIBC_PTHREAD_UPLOCK_CATS
#define ATS_LIBC_PTHREAD_UPLOCK_CATS

/* ****** ****** */

#ifdef _ATS_MULTITHREAD

/* ****** ****** */

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

/* ****** ****** */

// locks and tickets for uploading

typedef struct {
  pthread_mutex_t mutex_res; /* for resource protection */
} ats_pthread_uplock_t ; /* linear lock uploading */

typedef ats_pthread_uplock_t ats_pthread_upticket_t ;

/* ****** ****** */

ATSinline()
ats_void_type
atslib_pthread_uplockopt_unnone (ats_ptr_type p) { return ; }

ATSinline()
ats_ptr_type
atslib_pthread_uplockopt_unsome (ats_ptr_type p) { return p ; }

ATSinline()
ats_bool_type
atslib_pthread_uplockopt_is_none
  (ats_ptr_type p) {
  return (p ? ats_false_bool : ats_true_bool) ;
} // end of [atslib_pthread_uplockopt_is_none]

ATSinline()
ats_bool_type
atslib_pthread_uplockopt_is_some
  (ats_ptr_type p) {
  return (p ? ats_true_bool : ats_false_bool) ;
} // end of [atslib_pthread_uplockopt_is_some]

/* ****** ****** */

ATSinline()
ats_ptr_type
atslib_pthread_uplock_create () {
  ats_pthread_uplock_t *p ;
  p = (ats_pthread_uplock_t*)ATS_MALLOC(sizeof(ats_pthread_uplock_t)) ;
  pthread_mutex_init (&p->mutex_res, NULL) ;
  return p ;
} // end of [atslib_pthread_uplock_create]

ATSinline()
ats_ptr_type
atslib_pthread_upticket_create
  (ats_ptr_type p) {
  pthread_mutex_lock (&((ats_pthread_upticket_t*)p)->mutex_res) ;
  return p ;
} // end of [atslib_pthread_upticket_create]

ATSinline()
ats_void_type
atslib_pthread_upticket_upload_and_destroy
  (ats_ptr_type p) {
  pthread_mutex_unlock (&((ats_pthread_upticket_t*)p)->mutex_res) ;
  return ;
} // end of [atslib_pthread_upticket_upload_and_destroy]

ATSinline()
ats_void_type
atslib_pthread_uplock_download
  (ats_ptr_type p) {
  pthread_mutex_lock (&((ats_pthread_uplock_t*)p)->mutex_res) ;
  pthread_mutex_destroy (&((ats_pthread_uplock_t*)p)->mutex_res) ;
  ATS_FREE(p) ;
  return ;
} // end of [atslib_pthread_uplock_download]

/* ****** ****** */

#endif // end of [#ifdef _ATS_MULTITHREAD]

/* ****** ****** */

#endif // end of [#ifndef ATS_LIBC_PTHREAD_UPLOCK_CATS]

/* ****** ****** */

/* end of [pthread_uplock.cats] */
