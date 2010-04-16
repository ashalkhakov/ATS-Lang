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
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: April, 2010
//

(* ****** ****** *)

abst@ype GdkEventType
macdef GDK_NOTHING = $extval (GdkEventType, "GDK_NOTHING")
macdef GDK_DELETE = $extval (GdkEventType, "GDK_DELETE")
macdef GDK_DESTROY = $extval (GdkEventType, "GDK_DESTROY")
macdef GDK_EXPOSE = $extval (GdkEventType, "GDK_EXPOSE")
macdef GDK_MOTION = $extval (GdkEventType, "GDK_MOTION")
macdef GDK_BUTTON = $extval (GdkEventType, "GDK_BUTTON")
macdef GDK_2BUTTON = $extval (GdkEventType, "GDK_2BUTTON")
macdef GDK_3BUTTON = $extval (GdkEventType, "GDK_3BUTTON")
macdef GDK_BUTTON = $extval (GdkEventType, "GDK_BUTTON")
macdef GDK_KEY = $extval (GdkEventType, "GDK_KEY")
macdef GDK_KEY = $extval (GdkEventType, "GDK_KEY")
macdef GDK_ENTER = $extval (GdkEventType, "GDK_ENTER")
macdef GDK_LEAVE = $extval (GdkEventType, "GDK_LEAVE")
macdef GDK_FOCUS = $extval (GdkEventType, "GDK_FOCUS")
macdef GDK_CONFIGURE = $extval (GdkEventType, "GDK_CONFIGURE")
macdef GDK_MAP = $extval (GdkEventType, "GDK_MAP")
macdef GDK_UNMAP = $extval (GdkEventType, "GDK_UNMAP")
macdef GDK_PROPERTY = $extval (GdkEventType, "GDK_PROPERTY")
macdef GDK_SELECTION = $extval (GdkEventType, "GDK_SELECTION")
macdef GDK_SELECTION = $extval (GdkEventType, "GDK_SELECTION")
macdef GDK_SELECTION = $extval (GdkEventType, "GDK_SELECTION")
macdef GDK_PROXIMITY = $extval (GdkEventType, "GDK_PROXIMITY")
macdef GDK_PROXIMITY = $extval (GdkEventType, "GDK_PROXIMITY")
macdef GDK_DRAG = $extval (GdkEventType, "GDK_DRAG")
macdef GDK_DRAG = $extval (GdkEventType, "GDK_DRAG")
macdef GDK_DRAG = $extval (GdkEventType, "GDK_DRAG")
macdef GDK_DRAG = $extval (GdkEventType, "GDK_DRAG")
macdef GDK_DROP = $extval (GdkEventType, "GDK_DROP")
macdef GDK_DROP = $extval (GdkEventType, "GDK_DROP")
macdef GDK_CLIENT = $extval (GdkEventType, "GDK_CLIENT")
macdef GDK_VISIBILITY = $extval (GdkEventType, "GDK_VISIBILITY")
macdef GDK_NO = $extval (GdkEventType, "GDK_NO")
macdef GDK_SCROLL = $extval (GdkEventType, "GDK_SCROLL")
macdef GDK_WINDOW = $extval (GdkEventType, "GDK_WINDOW")
macdef GDK_SETTING = $extval (GdkEventType, "GDK_SETTING")
macdef GDK_OWNER = $extval (GdkEventType, "GDK_OWNER")
macdef GDK_GRAB = $extval (GdkEventType, "GDK_GRAB")
macdef GDK_DAMAGE = $extval (GdkEventType, "GDK_DAMAGE")

(* ****** ****** *)

abst@ype GdkEventMask = $extype "GdkEventMask"

macdef GDK_EXPOSURE_MASK =
  $extval (GdkEventMask, "GDK_EXPOSURE_MASK")
macdef GDK_POINTER_MOTION_MASK =
  $extval (GdkEventMask, "GDK_POINTER_MOTION_MASK")
macdef GDK_POINTER_MOTION_HINT_MASK =
  $extval (GdkEventMask, "GDK_POINTER_MOTION_HINT_MASK")
macdef GDK_BUTTON_MOTION_MASK =
  $extval (GdkEventMask, "GDK_BUTTON_MOTION_MASK")
macdef GDK_BUTTON1_MOTION_MASK =
  $extval (GdkEventMask, "GDK_BUTTON1_MOTION_MASK")
macdef GDK_BUTTON2_MOTION_MASK =
  $extval (GdkEventMask, "GDK_BUTTON2_MOTION_MASK")
macdef GDK_BUTTON3_MOTION_MASK =
  $extval (GdkEventMask, "GDK_BUTTON3_MOTION_MASK")
macdef GDK_BUTTON_PRESS_MASK =
  $extval (GdkEventMask, "GDK_BUTTON_PRESS_MASK")
macdef GDK_BUTTON_RELEASE_MASK =
  $extval (GdkEventMask, "GDK_BUTTON_RELEASE_MASK")
macdef GDK_KEY_PRESS_MASK =
  $extval (GdkEventMask, "GDK_KEY_PRESS_MASK")
macdef GDK_KEY_RELEASE_MASK =
  $extval (GdkEventMask, "GDK_KEY_RELEASE_MASK")
macdef GDK_ENTER_NOTIFY_MASK =
  $extval (GdkEventMask, "GDK_ENTER_NOTIFY_MASK")
macdef GDK_LEAVE_NOTIFY_MASK =
  $extval (GdkEventMask, "GDK_LEAVE_NOTIFY_MASK")
macdef GDK_FOCUS_CHANGE_MASK =
  $extval (GdkEventMask, "GDK_FOCUS_CHANGE_MASK")
macdef GDK_STRUCTURE_MASK =
  $extval (GdkEventMask, "GDK_STRUCTURE_MASK")
macdef GDK_PROPERTY_CHANGE_MASK =
  $extval (GdkEventMask, "GDK_PROPERTY_CHANGE_MASK")
macdef GDK_VISIBILITY_NOTIFY_MASK =
  $extval (GdkEventMask, "GDK_VISIBILITY_NOTIFY_MASK")
macdef GDK_PROXIMITY_IN_MASK =
  $extval (GdkEventMask, "GDK_PROXIMITY_IN_MASK")
macdef GDK_PROXIMITY_OUT_MASK =
  $extval (GdkEventMask, "GDK_PROXIMITY_OUT_MASK")
macdef GDK_SUBSTRUCTURE_MASK =
  $extval (GdkEventMask, "GDK_SUBSTRUCTURE_MASK")
macdef GDK_SCROLL_MASK =
  $extval (GdkEventMask, "GDK_SCROLL_MASK")
macdef GDK_ALL_EVENTS_MASK =
  $extval (GdkEventMask, "GDK_ALL_EVENTS_MASK")

castfn gint_of_GdkEventMask (x: GdkEventMask):<> gint

fun lor_GdkEventMask_GdkEventMask
  (x1: GdkEventMask, x2: GdkEventMask): GdkEventMask
  = "atsctrb_lor_GdkEventMask_GdkEventMask"
overload lor with lor_GdkEventMask_GdkEventMask

(* ****** ****** *)

abst@ype GdkVisibilityState = $extype "GdkVisibilityState"
macdef GDK_VISIBILITY_UNOBSCURED =
  $extval (GdkVisibilityState, "GDK_VISIBILITY_UNOBSCURED")
macdef GDK_VISIBILITY_PARTIAL
  = $extval (GdkVisibilityState, "GDK_VISIBILITY_PARTIAL")
macdef GDK_VISIBILITY_FULLY_OBSCURED =
  $extval (GdkVisibilityState, "GDK_VISIBILITY_FULLY_OBSCURED")

(* ****** ****** *)

abst@ype GdkWindowState = $extype "GdkWindowState"
macdef GDK_WINDOW_STATE_WITHDRAWN =
  $extval (GdkWindowState, "GDK_WINDOW_STATE_WITHDRAWN")
macdef GDK_WINDOW_STATE_ICONIFIED =
  $extval (GdkWindowState, "GDK_WINDOW_STATE_ICONIFIED")
macdef GDK_WINDOW_STATE_MAXIMIZED =
  $extval (GdkWindowState, "GDK_WINDOW_STATE_MAXIMIZED")
macdef GDK_WINDOW_STATE_STICKY =
  $extval (GdkWindowState, "GDK_WINDOW_STATE_STICKY")
macdef GDK_WINDOW_STATE_FULLSCREEN =
  $extval (GdkWindowState, "GDK_WINDOW_STATE_FULLSCREEN")
macdef GDK_WINDOW_STATE_ABOVE =
  $extval (GdkWindowState, "GDK_WINDOW_STATE_ABOVE")
macdef GDK_WINDOW_STATE_BELOW =
  $extval (GdkWindowState, "GDK_WINDOW_STATE_BELOW")

(* ****** ****** *)

typedef GdkEvent = $extype_struct "GdkEvent" of {
  type= GdkEventType
/*
, window= GdkWindow
*/
, send_event= gint8
} // end of [GdkEvent]

(* ****** ****** *)

(* end of [gdk.sats] *)
