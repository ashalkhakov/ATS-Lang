(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Power of Types!
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

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)  *)

(* ****** ****** *)

%{#
#include "libc/sys/CATS/time.cats"
%} // end of [%{#]

(* ****** ****** *)

staload TYPES = "libc/sys/SATS/types.sats"
typedef time_t = $TYPES.time_t
typedef suseconds_t = $TYPES.suseconds_t

(* ****** ****** *)

typedef timeval_struct =
$extype_struct "ats_timeval_type" of { // = struct timeval
  tv_sec= time_t // seconds  
, tv_usec=  suseconds_t // microseconds
} // end of [timeval_struct]
typedef timeval = timeval_struct

(* ****** ****** *)

(*
// HX: these macros seem only available in BSD
fun timerisset (tv: &timeval):<> bool = "#atslib_timerisset"
fun timerclear (tv: &timeval >> timeval):<> bool = "#atslib_timerclear"
*)

(* ****** ****** *)

typedef timezone_struct =
$extype_struct "ats_timezone_type" of { // = struct timezone
  tz_minuteswest= int // minutes west of GMT
, tz_dsttime= int // nonzero if DST is ever in effect
} // end of [timezone_struct]
typedef timezone = timezone_struct

(* ****** ****** *)
//
symintr gettimeofday
//
fun gettimeofday_tv
  (tv: &timeval? >> opt (timeval, i==0))
  :<> #[i:int | i <= 0] int i = "#atslib_gettimeofday_tv"
overload gettimeofday with gettimeofday_tv
//
fun gettimeofday_tz
  (tz: &timezone? >> opt (timezone, i==0))
  :<> #[i:int | i <= 0] int i = "#atslib_gettimeofday_tz"
overload gettimeofday with gettimeofday_tz
//
(* ****** ****** *)
//
symintr settimeofday
//
fun settimeofday_tv
  (tv: &timeval):<> [i:int | i <= 0] int i = "#atslib_settimeofday_tv"
overload settimeofday with settimeofday_tv
//
fun settimeofday_tz
  (tz: &timezone):<> [i:int | i <= 0] int i = "#atslib_settimeofday_tz"
overload settimeofday with settimeofday_tz
//
fun settimeofday_tvtz
  (tv: &timeval, tz: &timezone):<> [i:int | i <= 0] int i = "#atslib_settimeofday_tvtz"
overload settimeofday with settimeofday_tvtz
//
(* ****** ****** *)

(* end of [time.sats] *)
