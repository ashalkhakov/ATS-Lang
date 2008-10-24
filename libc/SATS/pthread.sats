(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
 * ATS - Unleashing the Potential of Types!
 *
 * Copyright (C) 2002-2008 Hongwei Xi, Boston University
 *
 * All rights reserved
 *
 * ATS is free software;  you can  redistribute it and/or modify it under
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
 *)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

// History:
//
// Rui Shi and Hongwei Xi first did [pthread] in ATS/Proto, on which
// this version is primarily based.
//

(* ****** ****** *)

%{#

#include "libc/CATS/pthread.cats"

%}

(* ****** ****** *)

fun pthread_create_detached_cloptr
  (f: () -<lin,cloptr1> void): void // a linear closure!
  = "ats_pthread_create_detached_cloptr"

fun pthread_exit (): void // this function does not return
  = "ats_pthread_exit"

(* ****** ****** *)

absviewt@ype pthread_mutex_view_viewt0ype (v:view) =
  $extype "pthread_mutex_t"

stadef mutex_vt = pthread_mutex_view_viewt0ype

absviewtype pthread_mutexref_view_type (v:view) // a boxed type

(* ****** ****** *)

fun pthread_mutex_init_locked
  {v:view} (mut: &mutex_vt? >> mutex_vt v): void
  = "atslib_pthread_mutex_init_locked"

fun pthread_mutex_init_unlocked
  {v:view} (resource: v | mut: &mutex_vt? >> mutex_vt v): void
  = "atslib_pthread_mutex_init_unlocked"

(* ****** ****** *)

fun pthread_mutex_create_locked {v:view} {l:addr}
  (): [l:addr] @(free_gc_v l, mutex_vt v @ l | ptr l)
  = "atslib_pthread_mutex_create_locked"

fun pthread_mutex_create_unlocked {v:view} {l:addr}
  (resource: v | (*none*)): [l:addr] @(free_gc_v l, mutex_vt v @ l | ptr l)
  = "atslib_pthread_mutex_create_unlocked"

(* ****** ****** *)

fun pthread_mutexref_create_locked
  {v:view} (): mutex_ref_t (v)
  = "atslib_pthread_mutex_create_locked"

fun pthread_mutexref_create_unlocked
  {v:view} (resource: v | (*none*)): mutex_ref_t (v)
  = "atslib_pthread_mutex_create_unlocked"

fun pthread_mutexref_create_mutex
  {v:view} (pf: mutex_vt v @ l | p: ptr l): mutex_ref_t (v)
  = "atslib_pthread_mutexref_create_mutex"

(* ****** ****** *)

fun pthread_mutex_destroy {v:view} {l:addr}
  (pf: !mutex_vt v @ l >> mutex_vt? @ l | p: ptr l): (v | void)
  = "atslib_pthread_mutex_destroy"

(* ****** ****** *)

fun pthread_mutex_lock
  {v:view} (mutex: &mutex_vt v): (v | void)
  = "atslib_pthread_mutex_lock"

fun pthread_mutex_unlock {v:view}
  (resource: v | mutex: &mutex_vt v): void
  = "atslib_pthread_mutex_unlock"

(* ****** ****** *)

fun pthread_mutexref_lock
  {v:view} (mutex: mutex_ref_t v): (v | void)
  = "atslib_pthread_mutex_lock"

fun pthread_mutexref_unlock
  {v:view} (resource: v | mutex: mutex_ref_t v): void
  = "atslib_pthread_mutex_unlock"

(* ****** ****** *)

absviewt@ype pthread_cond_viewt0ype =
  $extype "pthread_cond_t"

stadef cond_vt = pthread_cond_viewt0ype

(* ****** ****** *)

fun pthread_cond_create ()
  : [l:addr] @(free_gc_v l, cond_vt @ l | ptr l)
  = "atslib_pthread_cond_create"

fun pthread_cond_wait_mutex {v:view}
  (resource: !v | cond: &cond_vt, p: &mutex_vt v) :<> void
  = "atslib_pthread_cond_wait_mutex"

fun pthread_cond_signal (cond: &cond_vt):<> void
  = "atslib_pthread_cond_signal"

fun pthread_cond_broadcast (cond: &cond_vt):<> void
  = "atslib_pthread_cond_broadcast"

(* ****** ****** *)

(* end of [pthread.sats] *)
