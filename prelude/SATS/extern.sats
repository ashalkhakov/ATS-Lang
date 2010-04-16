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

// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: January 29, 2010

(* ****** ****** *)

// some common functions on pointers

(* ****** ****** *)

#include "prelude/params.hats"

(* ****** ****** *)

//
// this file is mostly used for building API's for external
// packages.
//

(* ****** ****** *)

//
// note that (vt1 \minus v2) roughly means that a ticket of
// [v2] is taken from [vt1]; the ticket must be returned before
// [vt1] is consumed.
//
absview minus_viewt0ype_view (vt1: viewt@ype, v2: view) = vt1
stadef minus = minus_viewt0ype_view
prfun minus_addback // [minus] is defined in basics_sta.sats
  {vt1:viewt@ype} {v2:view} (pf1: minus (vt1, v2), pf2: v2 | x: !vt1): void
// end of [minus_addback]

(* ****** ****** *)

absprop stamp (t: int)
absviewt@ype stamped (a:viewt@ype, t: int) = a

prfun stamped_encode {a:viewt@ype} {t:int} (v: !a >> stamped (a, t)): void
prfun stamped_decode {a:viewt@ype} {t:int} (v: !stamped (a, t) >> a): void

prfun stamp_forfeit
  {a:viewt@ype} {t:int} (s: stamp t, v: stamped (a, t)): void
// [stamp_forfeit]

(* ****** ****** *)

(* end of [extern.sats] *)
