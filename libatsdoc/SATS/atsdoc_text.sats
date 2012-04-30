(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Postiats - Unleashing the Potential of Types!
** Copyright (C) 2011-20?? Hongwei Xi, ATS Trustful Software, Inc.
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
** Free Software Foundation; either version 3, or (at  your  option)  any
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
//
// Author: Hongwei Xi (gmhwxi AT gmail DOT com)
// Start Time: July, 2011
//
(* ****** ****** *)

staload
SYM = "libatsdoc/SATS/atsdoc_symbol.sats"
typedef symbol = $SYM.symbol

(* ****** ****** *)

abstype atext_type
typedef atext = atext_type
typedef atextlst = List (atext)

(* ****** ****** *)

fun atext_nil (): atext
fun atext_strcst (x: string): atext
fun atext_strsub (x: string): atext
fun atext_apptxt2 (x1: atext, x2: atext): atext
fun atext_appstr2 (x1: string, x2: string): atext
fun atext_apptxt3 (x1: atext, x2: atext, x3: atext): atext
fun atext_appstr3 (x1: string, x2: string, x3: string): atext
fun atext_concatxt (xs: atextlst): atext
fun atext_concatxtsep (xs: atextlst, sep: atext): atext

val atext_newline : atext
fun atext_strptr (x: strptr1): atext

(* ****** ****** *)

fun filename2text (path: string): atext

(* ****** ****** *)

fun atscode2xml_strcode (stadyn: int, code: string): atext
fun atscode2xml_filcode (stadyn: int, path: string): atext

(* ****** ****** *)

fun theTextMap_search (s: symbol): Option_vt (atext)
fun theTextMap_insert (s: symbol, txt: atext): void
fun theTextMap_insert_str (s: string, txt: atext): void

(* ****** ****** *)

fun fprint_atext (out: FILEref, x: atext): void
fun fprint_atextlst (out: FILEref, xs: atextlst): void
fun fprint_atextlst_sep (out: FILEref, xs: atextlst, sep: atext): void

(* ****** ****** *)

fun fprint_strsub (out: FILEref, sub: string): void
fun fprint_filsub (out: FILEref, path: string): void

(* ****** ****** *)
//
// HX: this one is generic and should probably be moved into libats
//
fun{a:t@ype}
tostring_fprint
  (prfx: string, fpr: (FILEref, a) -> void, x: a): strptr0
// end of [tostring_fprint]

fun tostring_strsub (sub: string): strptr0 // HX: nullptr means error

(* ****** ****** *)

(* end of [atsdoc_atext.sats] *)
