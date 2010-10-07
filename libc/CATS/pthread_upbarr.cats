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
** Copyright (C) 2002-2010 Hongwei Xi.
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
#error "[pthread_upbarr.cats]: _ATS_MULTITHREAD is undefined!"
#endif
*/

/* ****** ****** */

#ifndef ATS_LIBC_PTHREAD_UPBARR_CATS
#define ATS_LIBC_PTHREAD_UPBARR_CATS

/* ****** ****** */

#ifdef _ATS_MULTITHREAD

/* ****** ****** */

#include <pthread.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

/* ****** ****** */

// barrs and tickets for uploading

typedef struct {
  int count ; /* outstanding tickets */
  pthread_cond_t cond_eqz; /* for signaling [count==0] */
  pthread_mutex_t mutex_res; /* for resource protection */
} ats_pthread_upbarr_t ; /* linear barr uploading */

typedef ats_pthread_upbarr_t ats_pthread_upticket_t ;

/* ****** ****** */

ATSinline()
ats_ptr_type
atslib_pthread_upbarr_create () {
  ats_pthread_upbarr_t *p ;
  p = (ats_pthread_upbarr_t*)ATS_MALLOC(sizeof(ats_pthread_upbarr_t)) ;
  p->count = 0 ;
  pthread_cond_init (&p->cond_eqz, NULL) ;
  pthread_mutex_init (&p->mutex_res, NULL) ;
  return p ;
} // end of [atslib_pthread_upbarr_create]

ATSinline()
ats_ptr_type
atslib_pthread_upbarr_upticket_create
  (ats_ptr_type p) {
  pthread_mutex_lock (&((ats_pthread_upticket_t*)p)->mutex_res) ;
  ((ats_pthread_upticket_t*)p)->count += 1 ;
  pthread_mutex_unlock (&((ats_pthread_upticket_t*)p)->mutex_res) ;
  return p ;
} // end of [atslib_pthread_upbarr_upticket_create]

ATSinline()
ats_void_type
atslib_pthread_upbarr_upticket_upload_and_destroy
  (ats_ptr_type p) {
  int count1 ;
//
  pthread_mutex_lock (&((ats_pthread_upticket_t*)p)->mutex_res) ;
  count1 = ((ats_pthread_upticket_t*)p)->count - 1 ;
  ((ats_pthread_upticket_t*)p)->count = count1 ;
  pthread_mutex_unlock (&((ats_pthread_upticket_t*)p)->mutex_res) ;
//
  if (count1 == 0) {
    pthread_cond_signal (&((ats_pthread_upticket_t*)p)->cond_eqz) ;
  } // end of [if]
//
  return ;
} // end of [atslib_pthread_upbarr_upticket_upload_and_destroy]

/* ****** ****** */

ATSinline()
ats_void_type
atslib_pthread_upbarr_download
  (ats_ptr_type p) {
  int count ;
  pthread_cond_t *eqz = &((ats_pthread_upbarr_t*)p)->cond_eqz ;
  pthread_mutex_t *res = &((ats_pthread_upbarr_t*)p)->mutex_res ;
//
  pthread_mutex_lock (res) ;
//
  while (1) {
    count = ((ats_pthread_upbarr_t*)p)->count ;
    if (count == 0) break ;
    pthread_cond_wait (eqz, res) ;
  } // end of [while]
//
  pthread_mutex_unlock (res) ;
//
  return ;
} // end of [atslib_pthread_upbarr_download]

ATSinline()
ats_void_type
atslib_pthread_upbarr_destroy
  (ats_ptr_type p) {
  pthread_cond_destroy (&((ats_pthread_upbarr_t*)p)->cond_eqz) ;
  pthread_mutex_destroy (&((ats_pthread_upbarr_t*)p)->mutex_res) ;
  ATS_FREE(p) ;
} // end of [atslib_pthread_upbarr_destroy]

/* ****** ****** */

#endif // end of [#ifdef _ATS_MULTITHREAD]

/* ****** ****** */

#endif // end of [#ifndef ATS_LIBC_PTHREAD_UPBARR_CATS]

/* ****** ****** */

/* end of [pthread_upbarr.cats] */
