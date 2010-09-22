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
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
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

%{#
#include "libc/CATS/time.cats"
%} // end of [%{#]

(* ****** ****** *)

staload TYPES = "libc/sys/SATS/types.sats"

(* ****** ****** *)

typedef tm_struct =
  $extype_struct "ats_tm_struct_type" of {
  tm_sec= int (* seconds *)
, tm_min= int (* minutes *)
, tm_hour= int (* hours *)
, tm_mday= int (* day of the month *)
, tm_mon= int (* month *)
, tm_year= int (* year *)
, tm_wday= int (* day of the week *)
, tm_yday= int (* day in the year *)
, tm_isdst= int (* daylight saving time *)
} // end of [tm_struct]

(* ****** ****** *)

typedef time_t = $TYPES.time_t
typedef clock_t = $TYPES.clock_t

(* ****** ****** *)

fun lint_of_time
  (t: time_t):<> int_long_t0ype = "atslib_lint_of_time"
overload lint_of with lint_of_time

fun double_of_time
  (t: time_t):<> double_t0ype = "atslib_double_of_time"
overload double_of with double_of_time

(* ****** ****** *)

(*
** HX (2010-01-15):
** These functions are now kept for backward compatibility
*)
fun tm_sec_get (tm: &tm_struct): int
  = "atslib_tm_sec_get"

fun tm_min_get (tm: &tm_struct): int
  = "atslib_tm_min_get"

fun tm_hour_get (tm: &tm_struct): int
  = "atslib_tm_hour_get"

fun tm_mday_get (tm: &tm_struct): int
  = "atslib_tm_mday_get"

fun tm_mon_get (tm: &tm_struct): int
  = "atslib_tm_mon_get"

fun tm_year_get (tm: &tm_struct): int
  = "atslib_tm_year_get"

fun tm_wday_get (tm: &tm_struct): int
  = "atslib_tm_wday_get"

fun tm_yday_get (tm: &tm_struct): int
  = "atslib_tm_yday_get"

fun tm_isdst_get (tm: &tm_struct): int
  = "atslib_tm_isdst_get"

(* ****** ****** *)

symintr time

fun time_get (): time_t = "atslib_time_get"
overload time with time_get

fun time_get_and_set
  (p: &time_t? >> time_t): time_t = "atslib_time_get_and_set"
overload time with time_get_and_set

(* ****** ****** *)

// non-reentrant
fun ctime (t: &time_t):<!ref>
  [l:addr] (strptr l -<prf> void | strptr l) = "atslib_ctime"
// end of [ctime]

#define CTIME_BUFLEN 26
dataview ctime_v (m:int, l:addr, addr) =
  | {l>null} ctime_v_succ (m, l, l) of strbuf (m, CTIME_BUFLEN - 1) @ l
  | ctime_v_fail (m, l, null) of b0ytes (m) @ l
fun ctime_r // reentrant
  {m:int | m >= CTIME_BUFLEN} {l:addr} (
    pf: ! b0ytes (m) @ l >> ctime_v (m, l, l1)
  | t: &time_t, p_buf: ptr l
  ) :<> #[l1:addr] ptr l1 = "atslib_ctime_r"
// end of [ctime_r]

(* ****** ****** *)

fun difftime
  (finish: time_t, start: time_t):<> double = "atslib_difftime"
// end of [difftime]

(* ****** ****** *)

// [localtime] is non-reentrant
fun localtime (time: &time_t):<!ref> [l:addr] (
    option_v ((tm_struct @ l, tm_struct @ l -<prf> void), l>null) | ptr l
  ) = "#atslib_localtime"
// end of [localtime]

// [localtime_r] is reentrant
fun localtime_r (
    time: &time_t, tm: &tm_struct? >> opt (tm_struct, l > null)
  ) :<> #[l:addr] ptr l = "#atslib_localtime_r"
// end of [localtime_r]

(* ****** ****** *)

fun strftime {m:pos} {l:addr} (
    pf: !b0ytes m @ l >> strbuf (m, n) @ l
  | p: ptr l, m: size_t m, fmt: string, tm: &tm_struct
  ) :<> #[n:nat | n < m] size_t n
  = "#atslib_strftime" // this a macro!
// end of [strftime]

(* ****** ****** *)

fun lint_of_clock
  (c: clock_t):<> int_long_t0ype = "atslib_lint_of_clock"
overload lint_of with lint_of_clock

fun double_of_clock
  (c: clock_t):<> double_t0ype = "atslib_double_of_clock"
overload double_of with double_of_clock

//

macdef CLOCKS_PER_SEC = $extval (clock_t, "CLOCKS_PER_SEC")

//

fun clock (): clock_t = "atslib_clock"

(* ****** ****** *)

(* end of [time.sats] *)
