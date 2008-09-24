(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
 * ATS/Anairiats - Unleashing the Power of Types!
 *
 * Copyright (C) 2002-2008 Hongwei Xi, Boston University
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
 *)

(* ****** ****** *)

// Time: June 2008
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

%{^

#include <stddef.h> // for [offsetof]
#include "gc.cats"

%}

(* ****** ****** *)

#include "gc.hats"

(* ****** ****** *)

staload "gc.sats"

(* ****** ****** *)

#define ATS_FUNCTION_NAME_PREFIX "gc_manops_"

(* ****** ****** *)

implement manmemlst_length (mms) = let
  fun aux {i,j:nat} (mms: manmemlst i, j: int j): int (i+j) = begin
    if manmemlst_is_cons mms then aux (manmemlst_next_get mms, j+1) else j
  end
in
  aux (mms, 0)
end // end of [manmemlst_length]

(* ****** ****** *)

%{^

static inline
ats_void_type gc_mark_the_manmemlst_aux
  (ats_ptr_type p0, ats_int_type sz) {
  int i ; freeitmlst *p_i ;

  p_i = (freeitmlst*)p0 ;
  for (i = 0; i < sz; i += 1, p_i += 1) gc_mark_ptr (*p_i) ;

  return ;
}

%}

implement gc_mark_the_manmemlst () = let
  fun auxlst (mms: manmemlst0): void =
    if manmemlst_is_cons (mms) then let
      val () = aux (
        manmemlst_data_get mms, manmemlst_itemwsz_get mms
      ) where {
        extern fun aux
          (itms: freeitmlst0, sz: int): void = "gc_mark_the_manmemlst_aux"
      } // end of [aux]
    in
      auxlst (manmemlst_next_get mms)
    end // end of [auxlst]
in
  auxlst (the_manmemlst_get ())
end

(* ****** ****** *)

%{$

extern manmemlst the_manmemlst ;
extern pthread_mutex_t the_manmemlst_lock ;

ats_ptr_type gc_man_malloc_bsz (ats_int_type bsz) {
  manmemlst mms ;

  mms = (manmemlst)malloc(sizeof(manmem) + bsz) ;

  if (!mms) {
    fprintf (stderr, "[gc_man_malloc_bsz]: [malloc] failed.\n") ;
    exit (1) ;
  } // end of [if]

  mms->itemwsz = bsz >> NBYTE_PER_WORD_LOG ;
  mms->prev = (manmemlst)0 ;

#ifdef _ATS_MULTITHREAD
  pthread_mutex_lock (&the_manmemlst_lock) ;
#endif

 if (the_manmemlst) { the_manmemlst->prev = mms ; }
 mms->next = the_manmemlst ; the_manmemlst = mms ;

#ifdef _ATS_MULTITHREAD
  pthread_mutex_unlock (&the_manmemlst_lock) ;
#endif

 return (mms->data) ;

} /* end of [gc_man_malloc_bsz] */

//

ats_ptr_type gc_man_calloc_bsz
  (ats_int_type n, ats_int_type bsz) {
  int nbsz = n * bsz ;
  ats_ptr_type _data = gc_man_malloc_bsz (nbsz) ;
  memset (_data, 0, nbsz) ;
  return (_data) ;
} /* end of [gc_man_calloc_bsz] */

//

ats_void_type gc_man_free (ats_ptr_type ptr) {
  manmemlst mms, mms_prev, mms_next ;

  mms = (manmemlst)((byte*)ptr - offsetof(manmem, data)) ;
  mms_prev = mms->prev ; mms_next = mms->next ;

#ifdef _ATS_MULTITHREAD
  pthread_mutex_lock (&the_manmemlst_lock) ;
#endif

  if (mms_next) {
    mms_next->prev = mms_prev ;
  } /* end of [if] */

  if (mms_prev) {
    mms_prev->next = mms_next ;
  } else {
    the_manmemlst = mms_next ;
  } /* end of [if] */

#ifdef _ATS_MULTITHREAD
  pthread_mutex_unlock (&the_manmemlst_lock) ;
#endif

  free (mms) ; return ;
} /* end of [gc_man_free] */

//

ats_ptr_type gc_man_realloc_bsz
  (ats_ptr_type ptr, ats_int_type bsz) {
  manmemlst mms, mms_prev, mms_next ;

  mms = (manmemlst)((byte*)ptr - offsetof(manmem, data)) ;
  mms_prev = mms->prev ; mms_next = mms->next ;
  mms = (manmemlst)realloc(mms, sizeof(manmem) + bsz) ;

  if (!mms) {
    fprintf (stderr, "[gc_man_realloc_bsz]: [realloc] failed.\n") ;
    exit (1) ;
  } // end of [if]

  mms->itemwsz = bsz >> NBYTE_PER_WORD_LOG ;

#ifdef _ATS_MULTITHREAD
  pthread_mutex_lock (&the_manmemlst_lock) ;
#endif

  if (mms_next) {
    mms_next->prev = mms ;
  } /* end of [if] */

  if (mms_prev) {
    mms_prev->next = mms ;
  } else {
    the_manmemlst = mms ;
  } /* end of [if] */

#ifdef _ATS_MULTITHREAD
  pthread_mutex_unlock (&the_manmemlst_lock) ;
#endif

  return (mms->data) ;

} /* end of [gc_man_realloc_bsz] */

%}

(* ****** ****** *)

(* end of [gc_top.dats] *)
