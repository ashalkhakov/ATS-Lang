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
** Copyright (C) 2009-2010 Hongwei Xi, Boston University
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

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: May, 2010
//

(* ****** ****** *)
//
// source: curl/easy.h
//
(* ****** ****** *)

fun curl_easy_init (): CURLptr0 = "#atsctrb_curl_easy_init"

(* ****** ****** *)

fun curl_easy_setopt {l:agz} {ts:types}
  (curl: !CURLptr l, option: CURLoption, args: ts): [i:int] (CURLerr_v i | CURLcode i)
  = "#atsctrb_curl_easy_setopt"
// end of [curl_easy_setopt]

(*
fun curl_easy_setopt_exn {l:agz} {ts:types}
  (curl: !CURLptr l, option: CURLoption, args: ts): void
  = "atsctrb_curl_easy_setopt_exn" // this is a function
// end of [curl_easy_setopt_exn]
*)

(* ****** ****** *)

fun curl_easy_perform
  {l:agz} (curl: !CURLptr l): [i:int] (CURLerr_v i | CURLcode i)
  = "#atsctrb_curl_easy_perform"
// end of [curl_easy_perform]

(* ****** ****** *)

fun curl_easy_cleanup (curl: CURLptr1): void = "#atsctrb_curl_easy_cleanup"

(* ****** ****** *)

(* end of [easy.sats] *)
