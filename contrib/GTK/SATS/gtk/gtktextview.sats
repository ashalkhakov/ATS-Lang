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

fun gtk_text_view_new (): GtkTextView_ptr1 = "#atsctrb_gtk_text_view_new"

//
// HX-2010-05-03: if [NULL] is passed, a new one is created automatically
//
fun gtk_text_view_new_with_buffer
  {c:cls | c <= GtkTextBuffer} {l:addr} (tb: !gobjptr (c, l)): GtkTextView_ptr1
  = "atsctrb_gtk_text_view_new_with_buffer"
// end of [gtk_text_view_new_with_buffer]
  
(* ****** ****** *)

fun gtk_text_view_set_buffer
  {c1,c2:cls | c1 <= GtkTextView; c2 <= GtkTextBuffer} {l1,l2:agz}
  (tv: !gobjptr (c1, l1), tb: !gobjptr (c2, l2)): void = "#atsctrb_gtk_text_view_set_buffer"
// end of [gtk_text_view_set_buffer]

//
// HX: this one should be called 'takeout' (instead of 'get')
//
fun gtk_text_view_get_buffer
  {c:cls | c <= GtkTextView} {l:agz}
  (tv: !gobjptr (c, l)):<> [l_buf:agz] (
    minus (gobjptr (c, l), gobjptr (GtkTextBuffer, l_buf)) | gobjptr (GtkTextBuffer, l_buf)
  ) = "#atsctrb_gtk_text_view_get_buffer"
// end of [gtk_text_view_get_buffer]

(* ****** ****** *)

fun gtk_text_view_get_editable
  {c:cls | c <= GtkTextView} {l:agz}
  (tv: !gobjptr (c, l)): gboolean = "#atsctrb_gtk_text_view_get_editable"
// end of [gtk_text_view_get_editable]

fun gtk_text_view_set_editable
  {c:cls | c <= GtkTextView} {l:agz}
  (tv: !gobjptr (c, l), editable: gboolean): void = "#atsctrb_gtk_text_view_set_editable"
// end of [gtk_text_view_set_editable]

(* ****** ****** *)

fun gtk_text_view_get_cursor_visible
  {c:cls | c <= GtkTextView} {l:agz}
  (tv: !gobjptr (c, l)): gboolean = "#atsctrb_gtk_text_view_get_cursor_visible"
// end of [gtk_text_view_get_cursor_visible]

fun gtk_text_view_set_cursor_visible
  {c:cls | c <= GtkTextView} {l:agz}
  (tv: !gobjptr (c, l), visible: gboolean): void = "#atsctrb_gtk_text_view_set_cursor_visible"
// end of [gtk_text_view_set_cursor_visible]

(* ****** ****** *)

(* end of [gtktextview.sats] *)
