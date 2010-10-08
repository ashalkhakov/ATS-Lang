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
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
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
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

staload "libc/SATS/pthread_upbarr.sats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

implement
pthread_upbarr_elimunit {v} (barr) = let
  prfn fpf (pf: @(unit_v, v)): v = let prval unit_v () = pf.0 in pf.1 end
in
  pthread_upbarr_trans (fpf | barr)
end // end of [pthread_upbarr_elimunit]

(* ****** ****** *)

implement
pthread_upbarr_download_and_destroy
  (barr) = (pf | ()) where {
  val (pf | ()) = pthread_upbarr_download (barr)
  val () = pthread_upbarr_destroy (barr)
} // end of [pthread_upbarr_download_and_destroy]

(* ****** ****** *)

%{^

/* ****** ****** */

ats_ptr_type
atslib_pthread_upbarr_create () {
  ats_pthread_upbarr_t *p ;
  p = (ats_pthread_upbarr_t*)ATS_MALLOC(sizeof(ats_pthread_upbarr_t)) ;
  p->count = 0 ;
  if (pthread_cond_init (&p->cond_eqz, NULL)) goto FAIL ;
  if (pthread_mutex_init (&p->mutex_res, NULL)) goto FAIL ;
  return p ;
  FAIL: {
    ATS_FREE(p) ; ats_fatalerror() ; return (ats_ptr_type)0 ;
  } // end of [if]
} // end of [atslib_pthread_upbarr_create]

/* ****** ****** */

ats_ptr_type
atslib_pthread_upbarr_upticket_create
  (ats_ptr_type p) {
  if (pthread_mutex_lock(&((ats_pthread_upticket_t*)p)->mutex_res))
    ats_fatalerror() ;
  // end of [if]
  ((ats_pthread_upticket_t*)p)->count += 1 ;
  if (pthread_mutex_unlock(&((ats_pthread_upticket_t*)p)->mutex_res))
    ats_fatalerror() ;
  // end of [if]
  return p ;
} // end of [atslib_pthread_upbarr_upticket_create]

/* ****** ****** */

ats_void_type
atslib_pthread_upbarr_download
  (ats_ptr_type p) {
  int count ;
  pthread_cond_t *eqz = &((ats_pthread_upbarr_t*)p)->cond_eqz ;
  pthread_mutex_t *res = &((ats_pthread_upbarr_t*)p)->mutex_res ;
//
  if (pthread_mutex_lock(res)) ats_fatalerror() ;
//
  while (1) {
    count = ((ats_pthread_upbarr_t*)p)->count ;
    if (count == 0) break ;
    if (pthread_cond_wait (eqz, res)) ats_fatalerror() ;
  } // end of [while]
//
  if (pthread_mutex_unlock(res)) ats_fatalerror() ;
//
  return ;
} // end of [atslib_pthread_upbarr_download]

/* ****** ****** */

ats_void_type
atslib_pthread_upbarr_destroy
  (ats_ptr_type p) {
  pthread_cond_destroy (&((ats_pthread_upbarr_t*)p)->cond_eqz) ;
  pthread_mutex_destroy (&((ats_pthread_upbarr_t*)p)->mutex_res) ;
  ATS_FREE(p) ;
} // end of [atslib_pthread_upbarr_destroy]

/* ****** ****** */

ats_void_type
atslib_pthread_upbarr_upticket_upload_and_destroy
  (ats_ptr_type p) {
  int count1 ;
//
  if (pthread_mutex_lock(&((ats_pthread_upticket_t*)p)->mutex_res))
    ats_fatalerror() ;
  // end of [if]
  count1 = ((ats_pthread_upticket_t*)p)->count - 1 ;
  ((ats_pthread_upticket_t*)p)->count = count1 ;
  if (pthread_mutex_unlock(&((ats_pthread_upticket_t*)p)->mutex_res))
    ats_fatalerror() ;
  // end of [if]
//
  if (count1 == 0) {
    if (pthread_cond_signal(&((ats_pthread_upticket_t*)p)->cond_eqz))
      ats_fatalerror() ;
    // end of [if]
  } // end of [if]
//
  return ;
} // end of [atslib_pthread_upbarr_upticket_upload_and_destroy]

/* ****** ****** */

%} // end of [%{^]

(* ****** ****** *)

(* end of [pthread_upbarr.dats] *)
