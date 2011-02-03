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
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
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

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [lazy_vt.sats] starts!\n"
#endif // end of [VERBOSE_PRELUDE]

(* ****** ****** *)

fun{a:viewt@ype} list_vt_of_stream_vt
  (xs: stream_vt a):<1,~ref> [n:nat] (int n, list_vt (a, n))
// end of [list_vt_of_stream_vt]

(* ****** ****** *)

fun{a:t@ype} stream_vt_free (xs: stream_vt a):<1,~ref> void

(* ****** ****** *)

fun{a:t@ype}
stream_vt_filter_fun
  (xs: stream_vt a, pred: (&a) -<1,~ref> bool):<1,~ref> stream_vt a
// end of [stream_vt_filter_fun]

fun{a:t@ype}
stream_vt_filter_cloptr
  (xs: stream_vt a, pred: (&a) -<cloptr1,~ref> bool):<1,~ref> stream_vt a
// end of [stream_vt_filter_cloptr]

(* ****** ****** *)

fun{a1,a2:t@ype}{b:t@ype}
stream_vt_map2_fun
  (xs1: stream_vt a1, xs2: stream_vt a2, f: (a1, a2) -<1,~ref> b)
  :<1,~ref> stream_vt b
// end of [stream_vt_map2_fun]

fun{a1,a2:t@ype}{b:t@ype}
stream_vt_map2_cloptr
  (xs1: stream_vt a1, xs2: stream_vt a2, f: (a1, a2) -<cloptr1,~ref> b)
  :<1,~ref> stream_vt b
// end of [stream_vt_map2_cloptr]

(* ****** ****** *)

#if VERBOSE_PRELUDE #then
#print "Loading [lazy_vt.sats] finishes!\n"
#endif // end of [VERBOSE_PRELUDE]

(* end of [lazy_vt.sats] *)
