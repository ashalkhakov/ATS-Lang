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

fun gtk_text_buffer_new_null ()
  : GtkTextBuffer_ref1 = "#atsctrb_gtk_text_buffer_new_null"
// end of [gtk_text_buffer_new_null]

(* ****** ****** *)

//
// HX-2010-05-06: this one is 'get0'
//
fun gtk_text_buffer_get_tag_table
  {c:cls | c <= GtkTextBuffer} {l:agz}
  (tb: !gobjref (c, l)):<> [l_tbl:agz] (
    minus (gobjref (c, l), gobjref (GtkTextTagTable, l_tbl)) | gobjref (GtkTextTagTable, l_tbl)
  ) = "#atsctrb_gtk_text_buffer_get_tag_table"
// end of [gtk_text_buffer_get_tag_table]

(* ****** ****** *)

fun gtk_text_buffer_get_line_count
  {c:cls | c <= GtkTextBuffer} {l:agz}
  (tb: !gobjref (c, l)): gint = "#atsctrb_gtk_text_buffer_get_line_count"
// end of [gtk_text_buffer_get_line_count]

fun gtk_text_buffer_get_char_count
  {c:cls | c <= GtkTextBuffer} {l:agz}
  (tb: !gobjref (c, l)): gint = "#atsctrb_gtk_text_buffer_get_char_count"
// end of [gtk_text_buffer_get_char_count]

(* ****** ****** *)

fun gtk_text_buffer_get_iter_at_offset
  {c:cls | c <= GtkTextBuffer} {l:agz}
  (tb: !gobjref (c, l), iter: &GtkTextIter? >> GtkTextIter, charofs: gint): void
  = "#atsctrb_gtk_text_buffer_get_iter_at_offset"
// end of [gtk_text_buffer_get_iter_at_offset]

(* ****** ****** *)

fun gtk_text_buffer_delete
  {c:cls | c <= GtkTextBuffer} {l:agz} (
    tb: !gobjref (c, l), _beg: &GtkTextIter, _end: &GtkTextIter
  ) : void = "#atsctrb_gtk_text_buffer_delete"
// end of [gtk_text_buffer_get_delete]

(* ****** ****** *)

fun gtk_text_buffer_insert
  {c:cls | c <= GtkTextBuffer} {l:agz} {n0,n1:nat | n0 >= n1}
  (tb: !gobjref (c, l), iter: &GtkTextIter, text: &(@[gchar][n0]), len: gint n1): void
  = "#atsctrb_gtk_text_buffer_insert"
// end of [gtk_text_buffer_insert]

fun gtk_text_buffer_insertall
  {c:cls | c <= GtkTextBuffer} {l1,l2:agz}
  (tb: !gobjref (c, l1), iter: &GtkTextIter, text: !gstring l2): void
  = "atsctrb_gtk_text_buffer_insertall" // function!
// end of [gtk_text_buffer_insert_all]

(* ****** ****** *)

fun gtk_text_buffer_insert_at_cursor
  {c:cls | c <= GtkTextBuffer} {l:agz} {n0,n1:nat | n0 >= n1}
  (tb: !gobjref (c, l), text: &(@[gchar][n0]), len: gint n1): void
  = "#atsctrb_gtk_text_buffer_insert_at_cursor"
// end of [gtk_text_buffer_insert_at_cursor]

fun gtk_text_buffer_insertall_at_cursor
  {c:cls | c <= GtkTextBuffer} {l1,l2:agz}
  (tb: !gobjref (c, l1), text: !gstring l2): void
  = "atsctrb_gtk_text_buffer_insertall_at_cursor" // function!
// end of [gtk_text_buffer_insertall_at_cursor]

(* ****** ****** *)

fun gtk_text_buffer_get_start_iter
  {c:cls | c <= GtkTextBuffer} {l:agz}
  (tb: !gobjref (c, l), iter: &GtkTextIter? >> GtkTextIter): void
  = "#atsctrb_gtk_text_buffer_get_start_iter"
// end of [gtk_text_buffer_get_start_iter]

fun gtk_text_buffer_get_end_iter
  {c:cls | c <= GtkTextBuffer} {l:agz}
  (tb: !gobjref (c, l), iter: &GtkTextIter? >> GtkTextIter): void
  = "#atsctrb_gtk_text_buffer_get_end_iter"
// end of [gtk_text_buffer_get_end_iter]

(* ****** ****** *)

fun gtk_text_buffer_get_bounds
  {c:cls | c <= GtkTextBuffer} {l:agz} (
    tb: !gobjref (c, l)
  , _beg: &GtkTextIter? >> GtkTextIter
  , _end: &GtkTextIter? >> GtkTextIter
  ) : void = "#atsctrb_gtk_text_buffer_get_bounds"
// end of [gtk_text_buffer_get_bounds]

(* ****** ****** *)

//
// HX-2010-05-04: yes, the return type is [gstring1]!
//
fun gtk_text_buffer_get_text
  {c:cls | c <= GtkTextBuffer} {l:agz} (
    tb: !gobjref (c, l)
  , _beg: &GtkTextIter, _end: &GtkTextIter
  , include_hidden_chars: gboolean
  ) : gstring1 = "#atsctrb_gtk_text_buffer_get_text"
// end of [gtk_text_buffer_get_get_text]

(* ****** ****** *)

fun gtk_text_buffer_set_text
  {c:cls | c <= GtkTextBuffer} {l:agz}
  {n0,n1:nat | n0 >= n1}
  (tb: !gobjref (c, l), text: &(@[gchar][n0]), len: gint n1): void
  = "#atsctrb_gtk_text_buffer_set_text"
// end of [gtk_text_buffer_set_text]

fun gtk_text_buffer_setall_text
  {c:cls | c <= GtkTextBuffer} {l1,l2:agz}
  (tb: !gobjref (c, l1), text: !gstring l2): void
  = "atsctrb_gtk_text_buffer_setall_text" // function!
// end of [gtk_text_buffer_setall_text]

(* ****** ****** *)

fun gtk_text_buffer_place_cursor
  {c:cls | c <= GtkTextBuffer} {l:agz}
  (tb: !gobjref (c, l), iter: &GtkTextIter): void
  = "#atsctrb_gtk_text_buffer_place_cursor"
// end of [gtk_text_buffer_place_cursor]

(* ****** ****** *)

(* end of [gtktextbuffer.sats] *)
