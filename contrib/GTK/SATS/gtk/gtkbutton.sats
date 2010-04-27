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

fun gtk_button_new
  (): GtkButton_ptr1 = "#atsctrb_gtk_button_new"

fun gtk_button_new_with_label
  (label: string): GtkButton_ptr1 = "#atsctrb_gtk_button_new_with_label"
// end of [gtk_button_new_with_label]

fun gtk_button_new_with_mnemonic
  (label: string): GtkButton_ptr1 = "#atsctrb_gtk_button_new_with_mnemonic"
// end of [gtk_button_new_with_mnemonic]

(* ****** ****** *)

fun gtk_button_new_from_stock
  (stock_id: string): GtkButton_ptr1 = "#atsctrb_gtk_button_new_from_stock"
// end of [gtk_button_new_from_stock]

(* ****** ****** *)

//
// HX-2010-04-26: the label string belongs to the widget!
//
fun gtk_button_get_label
  {c:cls | c <= GtkButton} {l:agz}
  (widget: !gobjptr (c, l)): [t:int] (stamp t | stamped (Stropt, t))
  = "#atsctrb_gtk_button_get_label"
// end of [gtk_button_get_label]

fun gtk_button_set_label
  {c:cls | c <= GtkButton} {l:agz}
  (widget: !gobjptr (c, l), label: Stropt): void
  = "#atsctrb_gtk_button_set_label"
// end of [gtk_button_set_label]

(* ****** ****** *)

(* end of [gtkbutton.sats] *)
