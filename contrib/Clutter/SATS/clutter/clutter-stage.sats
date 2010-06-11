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
// Start Time: June, 2010
//
(* ****** ****** *)

fun clutter_stage_get_default ()
  : [l:agz] (ClutterStage_ref l -<lin,prf> void | ClutterStage_ref l)
  = "#atsctrb_clutter_stage_get_default"
// end of [clutter_stage_get_default]

fun clutter_stage_new (): [l:agz] ClutterStage_ref l = "#atsctrb_clutter_stage_new"

fun clutter_stage_is_default
  {c:cls | c <= ClutterStage} {l:agz} (stage: !gobjref (c, l)): gboolean
  = "#atsctrb_clutter_stage_is_default"
// end of [clutter_stage_get_is_default]

(* ****** ****** *)

fun clutter_stage_get_color
  {c:cls | c <= ClutterStage} {l:agz}
  (stage: !gobjref (c, l), color: &ClutterColor? >> ClutterColor): void
  = "#atsctrb_clutter_stage_get_color"
// end of [clutter_stage_get_color]

fun clutter_stage_set_color
  {c:cls | c <= ClutterStage} {l:agz}
  (stage: !gobjref (c, l), color: &ClutterColor): void
  = "#atsctrb_clutter_stage_set_color"
// end of [clutter_stage_set_color]

(* ****** ****** *)

fun clutter_stage_get_fullscreen
  {c:cls | c <= ClutterStage} {l:agz} (stage: !gobjref (c, l)): gboolean
  = "#atsctrb_clutter_stage_get_fullscreen"
// end of [clutter_stage_get_fullscreen]

fun clutter_stage_set_fullscreen
  {c:cls | c <= ClutterStage} {l:agz}
  (stage: !gobjref (c, l), fullscreen: gboolean): void
  = "#atsctrb_clutter_stage_set_fullscreen"
// end of [clutter_stage_set_fullscreen]

(* ****** ****** *)

fun clutter_stage_is_show_cursor
  {c:cls | c <= ClutterStage} {l:agz} (stage: !gobjref (c, l)): void
  = "#atsctrb_clutter_stage_is_show_cursor"
// end of [clutter_stage_get_is_show_cursor]

fun clutter_stage_is_hide_cursor
  {c:cls | c <= ClutterStage} {l:agz} (stage: !gobjref (c, l)): void
  = "#atsctrb_clutter_stage_is_hide_cursor"
// end of [clutter_stage_get_is_hide_cursor]

(* ****** ****** *)

(* end of [clutter-stage.sats] *)
