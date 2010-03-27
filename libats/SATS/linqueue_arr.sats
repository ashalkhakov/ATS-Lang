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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(*
**
** A array-based queue implementation
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: March, 2010 // based on a version done in October, 2008
**
*)

(* ****** ****** *)

//
// License: LGPL 3.0 (available at http://www.gnu.org/licenses/lgpl.txt)
//

(* ****** ****** *)

%{#
#include "libats/CATS/linqueue_arr.cats"
%} // end of [%{#]

(* ****** ****** *)

#define ATS_STALOADFLAG 0 // no static loading at run-time

(* ****** ****** *)

//
// a: item type
// m: maximal capacity
// n: current size
//
absviewt@ype QUEUE
  (a:viewt@ype+, m: int, n: int)
  = $extype "ats_libats_linqueue_arr_QUEUE_vt"
// end of [QUEUE]
typedef QUEUE0 (a:viewt@ype) = QUEUE (a, 0, 0)?

(* ****** ****** *)

// initializing to a queue of capacity [m]
fun{a:viewt@ype} queue_initialize {m:nat}
  (q: &QUEUE0 a >> QUEUE (a, m, 0), m: size_t m):<> void
// end of [linqueuearr_initialize]

(* ****** ****** *)

fun{a:viewt@ype}
queue_insert (*last*)
  {m,n:nat | m > n}
  (q: &QUEUE (a, m, n) >> QUEUE (a, m, n+1), x: a):<> void
// end of [queue_insert]

fun{a:viewt@ype}
queue_remove (*first*)
  {m,n:nat | n > 0} (q: &QUEUE (a, m, n) >> QUEUE (a, m, n-1)):<> a
// end of [queue_remove]

(* ****** ****** *)

fun queue_uninitialize {a:t@ype}
  {m,n:nat} {l:addr} (q: &QUEUE (a, m, n) >> QUEUE0 a):<> void
// end of [queue_uninitialize]

//
// uninitializeing an empty queue of capacity [m]
//
fun queue_uninitialize_vt {a:viewt@ype}
  {m:nat} {l:addr} (q: &QUEUE (a, m, 0) >> QUEUE0 a):<> void
// end of [queue_unintialize_vt]

(* ****** ****** *)

(* end of [linqueue_arr.sats] *)
