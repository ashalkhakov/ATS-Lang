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

// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: December, 2009

(* ****** ****** *)

%{#
#include "contrib/cairo/CATS/cairo.cats"
%}

(* ****** ****** *)

// a reference to cairo drawing context
absviewtype cairo_ref // cairo_t*

absviewtype cairo_surface_ref // cairo_surface*
absviewtype cairo_pattern_ref // cairo_pattern*

absviewtype cairo_font_face_ref // cairo_font_face*
absviewtype cairo_font_options_ptr // cairo_font_options_ptr

(* ****** ****** *)

abst@ype cairo_matrix_t = $extype "ats_cairo_matrix_type"

(* ****** ****** *)

// enum type
abst@ype cairo_status_t = $extype "ats_cairo_status_type"
castfn int_of_cairo_status_t (x: cairo_status_t):<> int

macdef CAIRO_STATUS_SUCCESS =
  $extval (cairo_status_t, "CAIRO_STATUS_SUCCESS")

macdef CAIRO_STATUS_NO_MEMORY =
  $extval (cairo_status_t, "CAIRO_STATUS_NO_MEMORY")

macdef CAIRO_STATUS_INVALID_RESTORE =
  $extval (cairo_status_t, "CAIRO_STATUS_INVALID_RESTORE")

macdef CAIRO_STATUS_INVALID_POP_GROUP =
  $extval (cairo_status_t, "CAIRO_STATUS_INVALID_POP_GROUP")

macdef CAIRO_STATUS_NO_CURRENT_POINT =
  $extval (cairo_status_t, "CAIRO_STATUS_NO_CURRENT_POINT")

macdef CAIRO_STATUS_INVALID_MATRIX =
  $extval (cairo_status_t, "CAIRO_STATUS_INVALID_MATRIX")

macdef CAIRO_STATUS_INVALID_STATUS =
  $extval (cairo_status_t, "CAIRO_STATUS_INVALID_STATUS")

macdef CAIRO_STATUS_NULL_POINTER =
  $extval (cairo_status_t, "CAIRO_STATUS_NULL_POINTER")

macdef CAIRO_STATUS_INVALID_STRING =
  $extval (cairo_status_t, "CAIRO_STATUS_INVALID_STRING")

macdef CAIRO_STATUS_INVALID_PATH_DATA =
  $extval (cairo_status_t, "CAIRO_STATUS_INVALID_PATH_DATA")

macdef CAIRO_STATUS_READ_ERROR =
  $extval (cairo_status_t, "CAIRO_STATUS_READ_ERROR")

macdef CAIRO_STATUS_WRITE_ERROR =
  $extval (cairo_status_t, "CAIRO_STATUS_WRITE_ERROR")

macdef CAIRO_STATUS_SURFACE_FINISHED =
  $extval (cairo_status_t, "CAIRO_STATUS_SURFACE_FINISHED")

macdef CAIRO_STATUS_SURFACE_TYPE_MISMATCH =
  $extval (cairo_status_t, "CAIRO_STATUS_SURFACE_TYPE_MISMATCH")

macdef CAIRO_STATUS_PATTERN_TYPE_MISMATCH =
  $extval (cairo_status_t, "CAIRO_STATUS_PATTERN_TYPE_MISMATCH")

macdef CAIRO_STATUS_INVALID_CONTENT =
  $extval (cairo_status_t, "CAIRO_STATUS_INVALID_CONTENT")

macdef CAIRO_STATUS_INVALID_FORMAT =
  $extval (cairo_status_t, "CAIRO_STATUS_INVALID_FORMAT")

macdef CAIRO_STATUS_INVALID_VISUAL =
  $extval (cairo_status_t, "CAIRO_STATUS_INVALID_VISUAL")

macdef CAIRO_STATUS_FILE_NOT_FOUND =
  $extval (cairo_status_t, "CAIRO_STATUS_FILE_NOT_FOUND")

macdef CAIRO_STATUS_INVALID_DASH =
  $extval (cairo_status_t, "CAIRO_STATUS_INVALID_DASH")

macdef CAIRO_STATUS_INVALID_DSC_COMMENT =
  $extval (cairo_status_t, "CAIRO_STATUS_INVALID_DSC_COMMENT")

fun eq_cairo_status_cairo_status
  (x1: cairo_status_t, x2: cairo_status_t): bool
  = "atsctrb_eq_cairo_status_cairo_status"
overload = with eq_cairo_status_cairo_status

(* ****** ****** *)

// enum type
abst@ype cairo_format_t = $extype "ats_cairo_format_type"
castfn int_of_cairo_format_t (x: cairo_format_t):<> int

macdef CAIRO_FORMAT_ARGB32 =
  $extval (cairo_format_t, "CAIRO_FORMAT_ARGB32")

macdef CAIRO_FORMAT_RGB24 =
  $extval (cairo_format_t, "CAIRO_FORMAT_RGB24")

macdef CAIRO_FORMAT_A8 =
  $extval (cairo_format_t, "CAIRO_FORMAT_A8")

macdef CAIRO_FORMAT_A1 =
  $extval (cairo_format_t, "CAIRO_FORMAT_A1")

(*
macdef CAIRO_FORMAT_RGB16_565 = // deprecated!
  $extval (cairo_format_t, "CAIRO_FORMAT_RGB16_565")
*)

(* ****** ****** *)

// enum type
abst@ype cairo_font_slant_t = $extype "ats_cairo_font_slant_type"
castfn int_of_cairo_font_slant_t (x: cairo_font_slant_t):<> int

macdef CAIRO_FONT_SLANT_NORMAL =
  $extval (cairo_font_slant_t, "CAIRO_FONT_SLANT_NORMAL")

macdef CAIRO_FONT_SLANT_ITALIC =
  $extval (cairo_font_slant_t, "CAIRO_FONT_SLANT_ITALIC")

macdef CAIRO_FONT_SLANT_OBLIQUE =
  $extval (cairo_font_slant_t, "CAIRO_FONT_SLANT_OBLIQUE")

// enum type
abst@ype cairo_font_weight_t = $extype "ats_cairo_font_weight_type"
castfn int_of_cairo_font_weight_t (x: cairo_font_weight_t):<> int

macdef CAIRO_FONT_WEIGHT_NORMAL =
  $extval (cairo_font_weight_t, "CAIRO_FONT_WEIGHT_NORMAL")

macdef CAIRO_FONT_WEIGHT_BOLD =
  $extval (cairo_font_weight_t, "CAIRO_FONT_WEIGHT_BOLD")

(* ****** ****** *)

absview cairo_save_v // abstract view generated by cairo_save
absview cairo_push_group_v // abstract view generated by cairo_push_group

(* ****** ****** *)

//
// contexts for drawing
//

(* ****** ****** *)

fun cairo_create
  (sf: !cairo_surface_ref): cairo_ref
  = "atsctrb_cairo_create"

fun cairo_reference (cr: !cairo_ref): cairo_ref
  = "atsctrb_cairo_reference"

fun cairo_destroy (cr: cairo_ref): void
  = "atsctrb_cairo_destroy"

//

fun cairo_status (cr: !cairo_ref): cairo_status_t
  = "atsctrb_cairo_status"

(* ****** ****** *)

fun cairo_save (cr: !cairo_ref): (cairo_save_v | void)
  = "atsctrb_cairo_save"

fun cairo_restore (pf: cairo_save_v | cr: !cairo_ref): void
  = "atsctrb_cairo_restore"

(* ****** ****** *)

fun cairo_get_target (cr: !cairo_ref): cairo_surface_ref
  = "atsctrb_cairo_get_target"

fun cairo_get_group_target
  (cr: !cairo_ref): cairo_surface_ref
  = "atsctrb_cairo_get_group_target"

(* ****** ****** *)

fun cairo_push_group
  (cr: !cairo_ref): (cairo_push_group_v | void)
  = "atsctrb_cairo_push_group"

// enum type
abst@ype cairo_content_t =
  $extype "ats_cairo_content_type"

macdef CAIRO_CONTENT_COLOR =
  $extval (cairo_content_t, "CAIRO_CONTENT_COLOR")

macdef CAIRO_CONTENT_ALPHA =
  $extval (cairo_content_t, "CAIRO_CONTENT_ALPHA")

macdef CAIRO_CONTENT_COLOR_ALPHA =
  $extval (cairo_content_t, "CAIRO_CONTENT_COLOR_ALPHA")

fun cairo_push_group_with_content (
    cr: !cairo_ref, content: cairo_content_t
  ) : (cairo_push_group_v | void)
  = "atsctrb_cairo_push_group_with_content"

fun cairo_pop_group
  (pf: cairo_push_group_v | cr: !cairo_ref): cairo_pattern_ref
  = "atsctrb_cairo_pop_group"

fun cairo_pop_group_to_source
  (pf: cairo_push_group_v | cr: !cairo_ref): void
  = "atsctrb_cairo_pop_group_to_source"

(* ****** ****** *)

fun cairo_set_source_rgb (
    cr: !cairo_ref
  , red: double, green: double, blue: double
  ) : void
  = "atsctrb_cairo_set_source_rgb"
// end of [cairo_set_source_rgb]

fun cairo_set_source_rgba (
    cr: !cairo_ref
  , red: double, green: double, blue: double, alpha: double
  ) : void
  = "atsctrb_cairo_set_source_rgba"
// end of [cairo_set_source_rgba]

fun cairo_get_source
  (cr: !cairo_ref): cairo_pattern_ref
  = "atsctrb_cairo_get_source"

fun cairo_set_source
  (cr: !cairo_ref, pat: !cairo_pattern_ref): void
  = "atsctrb_cairo_set_source"

fun cairo_set_source_surface (
    cr: !cairo_ref
  , sf: !cairo_surface_ref, x: double, y: double
  ) : void
  = "atsctrb_cairo_set_source_surface"

(* ****** ****** *)

abst@ype cairo_antialias_t =
  $extype "ats_cairo_antialias_type"

macdef CAIRO_ANTIALIAS_DEFAULT =
  $extval (cairo_antialias_t, "CAIRO_ANTIALIAS_DEFAULT")
macdef CAIRO_ANTIALIAS_NONE =
  $extval (cairo_antialias_t, "CAIRO_ANTIALIAS_NONE")
macdef CAIRO_ANTIALIAS_GRAY =
  $extval (cairo_antialias_t, "CAIRO_ANTIALIAS_GRAY")
macdef CAIRO_ANTIALIAS_SUBPIXEL =
  $extval (cairo_antialias_t, "CAIRO_ANTIALIAS_SUBPIXEL")

fun cairo_get_antialias
  (cr: !cairo_ref): cairo_antialias_t
  = "atsctrb_cairo_get_antialias"

fun cairo_set_antialias
  (cr: !cairo_ref, antialias: cairo_antialias_t): void
  = "atsctrb_cairo_set_antialias"

(* ****** ****** *)

fun cairo_get_dash_count (cr: !cairo_ref): int
  = "atsctrb_cairo_get_dash_count"

//
// note that [dashes] gets updated only if [n1 <= n]
//
fun cairo_get_dash {n:nat} (
    cr: !cairo_ref
  , dashes: &(@[double][n]), n: int n, offset: &double? >> double
  ) : [n1:nat] int n1
  = "atsctrb_cairo_get_dash"

fun cairo_set_dash {n:nat} (
    cr: !cairo_ref, dashes: &(@[double][n]), n: int n, offset: double
  ) : void
  = "atsctrb_cairo_set_dash"

(* ****** ****** *)

// enum type
abst@ype cairo_fill_rule_t =
  $extype "ats_cairo_fill_rule_type"

macdef CAIRO_FILL_RULE_WINDING =
  $extval (cairo_fill_rule_t, "CAIRO_FILE_RULE_WINDING")
macdef CAIRO_FILL_RULE_EVEN_ODD =
  $extval (cairo_fill_rule_t, "CAIRO_FILE_RULE_EVENODD")

fun cairo_get_fill_rule
  (cr: !cairo_ref): cairo_fill_rule_t
  = "atsctrb_cairo_get_fill_rule"

fun cairo_set_fill_rule
  (cr: !cairo_ref, fill_rule: cairo_fill_rule_t): void
  = "atsctrb_cairo_set_fill_rule"

(* ****** ****** *)

// enum type
abst@ype cairo_line_cap_t =
  $extype "ats_cairo_line_cap_type"

macdef CAIRO_LINE_CAP_BUTT =
  $extval (cairo_line_cap_t, "CAIRO_LINE_CAP_BUTT")

macdef CAIRO_LINE_CAP_ROUND =
  $extval (cairo_line_cap_t, "CAIRO_LINE_CAP_ROUND")

macdef CAIRO_LINE_CAP_SQUARE =
  $extval (cairo_line_cap_t, "CAIRO_LINE_CAP_SQUARE")

fun cairo_get_line_cap
  (cr: !cairo_ref): cairo_line_cap_t
  = "atsctrb_cairo_get_line_cap"

fun cairo_set_line_cap
  (cr: !cairo_ref, line_cap: cairo_line_cap_t): void
  = "atsctrb_cairo_set_line_cap"

(* ****** ****** *)

// enum type
abst@ype cairo_line_join_t =
  $extype "ats_cairo_line_join_type"

macdef CAIRO_LINE_JOIN_MITER =
  $extval (cairo_line_join_t, "CAIRO_LINE_JOIN_MITER")

macdef CAIRO_LINE_JOIN_ROUND =
  $extval (cairo_line_join_t, "CAIRO_LINE_JOIN_ROUND")

macdef CAIRO_LINE_JOIN_BEVEL =
  $extval (cairo_line_join_t, "CAIRO_LINE_JOIN_BEVEL")

fun cairo_get_line_join
  (cr: !cairo_ref): cairo_line_join_t
  = "atsctrb_cairo_get_line_join"

fun cairo_set_line_join
  (cr: !cairo_ref, line_join: cairo_line_join_t): void
  = "atsctrb_cairo_set_line_join"

(* ****** ****** *)

fun cairo_get_line_width (cr: !cairo_ref): double
  = "atsctrb_cairo_get_line_width"

fun cairo_set_line_width (cr: !cairo_ref, width: double): void
  = "atsctrb_cairo_set_line_width"
                                                         
(* ****** ****** *)

fun cairo_get_miter_limit (cr: !cairo_ref): double
  = "atsctrb_cairo_get_miter_limit"

fun cairo_set_miter_limit (cr: !cairo_ref, width: double): void
  = "atsctrb_cairo_set_miter_limit"
                                                         
(* ****** ****** *)

// enum type
abst@ype cairo_operator_t =
  $extype "ats_cairo_operator_type"
castfn int_of_cairo_operator_t (x: cairo_operator_t):<> int

macdef CAIRO_OPERATOR_CLEAR =
  $extval (cairo_operator_t, "CAIRO_OPERATOR_CLEAR")

macdef CAIRO_OPERATOR_SOURCE =
  $extval (cairo_operator_t, "CAIRO_OPERATOR_SOURCE")

macdef CAIRO_OPERATOR_OVER =
  $extval (cairo_operator_t, "CAIRO_OPERATOR_OVER")

macdef CAIRO_OPERATOR_IN =
  $extval (cairo_operator_t, "CAIRO_OPERATOR_IN")

macdef CAIRO_OPERATOR_OUT =
  $extval (cairo_operator_t, "CAIRO_OPERATOR_OUT")

macdef CAIRO_OPERATOR_ATOP =
  $extval (cairo_operator_t, "CAIRO_OPERATOR_ATOP")

macdef CAIRO_OPERATOR_DEST =
  $extval (cairo_operator_t, "CAIRO_OPERATOR_DEST")

macdef CAIRO_OPERATOR_DEST_OVER =
  $extval (cairo_operator_t, "CAIRO_OPERATOR_DEST_OVER")

macdef CAIRO_OPERATOR_DEST_IN =
  $extval (cairo_operator_t, "CAIRO_OPERATOR_DEST_IN")

macdef CAIRO_OPERATOR_DEST_OUT =
  $extval (cairo_operator_t, "CAIRO_OPERATOR_DEST_OUT")

macdef CAIRO_OPERATOR_DEST_ATOP =
  $extval (cairo_operator_t, "CAIRO_OPERATOR_DEST_ATOP")

macdef CAIRO_OPERATOR_XOR =
  $extval (cairo_operator_t, "CAIRO_OPERATOR_XOR")

macdef CAIRO_OPERATOR_AND =
  $extval (cairo_operator_t, "CAIRO_OPERATOR_AND")

macdef CAIRO_OPERATOR_SATURATE =
  $extval (cairo_operator_t, "CAIRO_OPERATOR_SATURATE")

fun cairo_get_operator
  (cr: !cairo_ref): cairo_operator_t
  = "atsctrb_cairo_get_operator"

fun cairo_set_operator
  (cr: !cairo_ref, operator: cairo_operator_t): void
  = "atsctrb_cairo_set_operator"

(* ****** ****** *)

fun cairo_get_tolerance (cr: !cairo_ref): double
  = "atsctrb_cairo_get_tolerance"

fun cairo_set_tolerance (cr: !cairo_ref, tolerance: double): void
  = "atsctrb_cairo_set_tolerance"

(* ****** ****** *)

typedef cairo_rectangle_t = @{
  x= double, y= double, width= double, height= double
} // end of [cairo_rectangel_t]

absviewtype // [n]: list length
cairo_rectangle_list_ref (n:int) // cairo_rectangle_list_t*

fun cairo_rectangle_list_destroy
  {n:nat} (lst: cairo_rectangle_list_ref (n)): void
  = "atsctrb_cairo_rectangle_list_destroy"

fun cairo_copy_clip_rectangle_list
  (cr: !cairo_ref): [n:nat] cairo_rectangle_list_ref (n)
  = "atsctrb_cairo_copy_clip_rectangle_list"

(* ****** ****** *)

fun cairo_clip (cr: !cairo_ref): void
  = "atsctrb_cairo_clip"

fun cairo_clip_preserve (cr: !cairo_ref): void
  = "atsctrb_cairo_clip_preserve"

fun cairo_clip_extents (
    cr: !cairo_ref
  , x1: &double? >> double
  , y1: &double? >> double
  , x2: &double? >> double
  , y2: &double? >> double
  ) : void
  = "atsctrb_cairo_clip_extents"

fun cairo_reset_clip (cr: !cairo_ref): void
  = "atsctrb_cairo_reset_clip"

(* ****** ****** *)

fun cairo_fill (cr: !cairo_ref): void
  = "atsctrb_cairo_fill"

fun cairo_fill_preserve (cr: !cairo_ref): void
  = "atsctrb_cairo_fill_preserve"

fun cairo_fill_extents (
    cr: !cairo_ref
  , x1: &double? >> double
  , y1: &double? >> double
  , x2: &double? >> double
  , y2: &double? >> double
  ) : void
  = "atsctrb_cairo_fill_extents"

// [cairo_bool_t] and [bool] are the same
fun cairo_in_fill (cr: !cairo_ref, x: double, y: double): bool
  = "atsctrb_cairo_in_fill"

(* ****** ****** *)

fun cairo_mask
  (cr: !cairo_ref, pattern: !cairo_pattern_ref): void
  = "atsctrb_cairo_mask"

fun cairo_mask_surface (
    cr: !cairo_ref
  , sf: !cairo_surface_ref, surface_x: double, surface_y: double
  ) : void
  = "atsctrb_cairo_mask_surface"

(* ****** ****** *)

fun cairo_paint (cr: !cairo_ref): void
  = "atsctrb_cairo_paint"

fun cairo_paint_with_alpha
  (cr: !cairo_ref, alpha: double): void
  = "atsctrb_cairo_paint_with_alpha"

(* ****** ****** *)

fun cairo_stroke (cr: !cairo_ref): void
  = "atsctrb_cairo_stroke"

fun cairo_stroke_preserve (cr: !cairo_ref): void
  = "atsctrb_cairo_stroke_preserve"

fun cairo_stroke_extents (
    cr: !cairo_ref
  , x1: &double? >> double
  , y1: &double? >> double
  , x2: &double? >> double
  , y2: &double? >> double
  ) : void
  = "atsctrb_cairo_stroke_extents"

// [cairo_bool_t] and [bool] are the same
fun cairo_in_stroke (cr: !cairo_ref, x: double, y: double): bool
  = "atsctrb_cairo_in_stroke"

(* ****** ****** *)

fun cairo_get_reference_count (cr: !cairo_ref): uint
  = "atsctrb_cairo_get_reference_count"

(* ****** ****** *)

fun cairo_copy_page (cr: !cairo_ref): void
  = "atsctrb_cairo_copy_page"

fun cairo_show_page (cr: !cairo_ref): void
  = "atsctrb_cairo_show_page"

(* ****** ****** *)

//
// drawing paths
//

(* ****** ****** *)

absviewtype // [n]: path length
cairo_path_ref (n:int) // cairo_path_t*

fun cairo_copy_path (cr: !cairo_ref): [n:nat] cairo_path_ref n
  = "atsctrb_cairo_copy_path"

fun cairo_copy_path_flat (cr: !cairo_ref): [n:nat] cairo_path_ref n
  = "atsctrb_cairo_copy_path_flat"

fun cairo_append_path {n:nat}
  (cr: !cairo_ref, path: !cairo_path_ref n): void
  = "atsctrb_cairo_append_path"

fun cairo_path_destroy {n:nat} (path: cairo_path_ref n): void
  = "atsctrb_cairo_path_destroy"

(* ****** ****** *)

// [cairo_bool_t] and [bool] are the same
fun cairo_has_current_point (cr: !cairo_ref): bool
  = "atsctrb_cairo_has_current_point"

fun cairo_get_current_point (
    cr: !cairo_ref
  , x: &double? >> double, y: &double? >> double
  ) : void
  = "atsctrb_cairo_get_current_point"

(* ****** ****** *)

fun cairo_new_path (cr: !cairo_ref): void
  = "atsctrb_cairo_new_path"

fun cairo_new_sub_path (cr: !cairo_ref): void
  = "atsctrb_cairo_new_path"

fun cairo_close_path (cr: !cairo_ref): void
  = "atsctrb_cairo_close_path"

(* ****** ****** *)

fun cairo_arc (
    cr: !cairo_ref
  , xc: double, yc: double
  , rad: double, angle1: double, angle2: double
  ) : void
  = "atsctrb_cairo_arc"

fun cairo_arc_negative (
    cr: !cairo_ref
  , xc: double, yc: double
  , rad: double, angle1: double, angle2: double
  ) : void
  = "atsctrb_cairo_arc_negative"

fun cairo_curve_to (
    cr: !cairo_ref
  , x1: double, y1: double
  , x2: double, y2: double
  , x3: double, y3: double
  ) : void
  = "atsctrb_cairo_curve_to"

fun cairo_line_to
  (cr: !cairo_ref, x: double, y: double): void
  = "atsctrb_cairo_line_to"

fun cairo_move_to
  (cr: !cairo_ref, x: double, y: double): void
  = "atsctrb_cairo_move_to"

fun cairo_rectangle (
    cr: !cairo_ref
  , x: double, y: double, width: double, height: double
  ) : void
  = "atsctrb_cairo_rectangle"

(* ****** ****** *)

fun cairo_rel_curve_to (
    cr: !cairo_ref
  , dx1: double, dy1: double
  , dx2: double, dy2: double
  , dx3: double, dy3: double
  ) : void
  = "atsctrb_cairo_rel_curve_to"

fun cairo_rel_line_to
  (cr: !cairo_ref, dx: double, dy: double): void
  = "atsctrb_cairo_rel_line_to"

fun cairo_rel_move_to
  (cr: !cairo_ref, dx: double, dy: double): void
  = "atsctrb_cairo_rel_move_to"

(* ****** ****** *)

fun cairo_path_extents (
    cr: !cairo_ref
  , x1: &double? >> double
  , y1: &double? >> double
  , x2: &double? >> double
  , y2: &double? >> double
  ) : void
  = "atsctrb_cairo_path_extents"

(* ****** ****** *)

// drawing texts

fun cairo_select_font_face (
    cr: !cairo_ref
  , name: string (* family *)
  , slnt: cairo_font_slant_t
  , wght: cairo_font_weight_t
  ) : void
  = "atsctrb_cairo_select_font_face"

fun cairo_set_font_size
  (cr: !cairo_ref, size: double): void
  = "atsctrb_cairo_set_font_size"

(* ****** ****** *)

fun cairo_set_font_matrix
  (cr: !cairo_ref, mat: &cairo_matrix_t): void
  = "atsctrb_cairo_set_font_matrx"

fun cairo_get_font_matrix
  (cr: !cairo_ref, mat: &cairo_matrix_t? >> cairo_matrix_t): void
  = "atsctrb_cairo_get_font_matrx"

(* ****** ****** *)

fun cairo_get_font_face
  (cr: !cairo_ref): cairo_font_face_ref
  = "atsctrb_cairo_get_font_face"

fun cairo_set_font_face
  (cr: !cairo_ref, face: !cairo_font_face_ref): void
  = "atsctrb_cairo_set_font_face"

(* ****** ****** *)

(*
abst@ype cairo_font_extents_t = $extype "cairo_font_extents_t"
*)
typedef cairo_font_extents_t = @{
  ascent= double
, descent= double
, height= double
, max_x_advance= double
, max_y_advance= double
} // end of [cairo_font_extents_t]

(*
abst@ype cairo_text_extents_t = $extype "cairo_text_extents_t"
*)
typedef cairo_text_extents_t = @{
  x_bearing= double, y_bearing= double
, width= double, height= double
, x_advance= double, y_advance= double
} // end of [cairo_text_extents_t]

fun cairo_font_extents (
    cr: !cairo_ref
  , extents: &cairo_font_extents_t? >> cairo_font_extents_t
  ) : void
  = "atsctrb_cairo_font_extents"

fun cairo_text_extents (
    cr: !cairo_ref, utf8: string
  , extents: &cairo_text_extents_t? >> cairo_text_extents_t
  ) : void
  = "atsctrb_cairo_text_extents"

(* ****** ****** *)

fun cairo_show_text (cr: !cairo_ref, utf8: string): void
  = "atsctrb_cairo_show_text"

(* ****** ****** *)

// transformations for drawing

fun cairo_translate
  (cr: !cairo_ref, x: double, y: double): void
  = "atsctrb_cairo_translate"

fun cairo_scale (cr: !cairo_ref, sx: double, sy: double): void
  = "atsctrb_cairo_scale"

fun cairo_rotate (cr: !cairo_ref, angle: double): void
  = "atsctrb_cairo_rotate"

fun cairo_transform (cr: !cairo_ref, mat: &cairo_matrix_t): void
  = "atsctrb_cairo_transform"
  
(* ****** ****** *)

fun cairo_get_matrix (
    cr: !cairo_ref
  , mat: &cairo_matrix_t? >> cairo_matrix_t
  ) : void
  = "atsctrb_cairo_get_matrix"
  
fun cairo_set_matrix (cr: !cairo_ref, mat: &cairo_matrix_t): void
  = "atsctrb_cairo_set_matrix"
  
fun cairo_identity_matrix (cr: !cairo_ref): void
  = "atsctrb_cairo_identity_matrix"
  
(* ****** ****** *)

fun cairo_user_to_device
  (cr: !cairo_ref, x: &double, y: &double) : void
  = "atsctrb_cairo_user_to_device"

fun cairo_user_to_device_distance
  (cr: !cairo_ref, dx: &double, dy: &double) : void
  = "atsctrb_cairo_user_to_device_distance"

fun cairo_devide_to_user
  (cr: !cairo_ref, x: &double, y: &double) : void
  = "atsctrb_cairo_devide_to_user"

fun cairo_devide_to_user_distance
  (cr: !cairo_ref, dx: &double, dy: &double) : void
  = "atsctrb_cairo_devide_to_user_distance"

(* ****** ****** *)

//
// fonts for drawing
//

fun cairo_font_face_reference
  (font_face: !cairo_font_face_ref): cairo_font_face_ref
  = "atsctrb_cairo_font_face_reference"

fun cairo_font_face_destroy
  (font_face: cairo_font_face_ref): void
  = "atsctrb_cairo_font_face_destroy"

fun cairo_font_face_status
  (font_face: !cairo_font_face_ref): cairo_status_t
  = "atsctrb_cairo_font_face_status"

abst@ype cairo_font_type_t =
  $extype "ats_cairo_font_type_type"

macdef CAIRO_FONT_TYPE_TOY =
  $extval (cairo_font_type_t, "CAIRO_FONT_TYPE_TOY")
macdef CAIRO_FONT_TYPE_FT =
  $extval (cairo_font_type_t, "CAIRO_FONT_TYPE_FT")
macdef CAIRO_FONT_TYPE_WIN32 =
  $extval (cairo_font_type_t, "CAIRO_FONT_TYPE_WIN32")
macdef CAIRO_FONT_TYPE_QUARTZ =
  $extval (cairo_font_type_t, "CAIRO_FONT_TYPE_QUARTZ")
macdef CAIRO_FONT_TYPE_USER =
  $extval (cairo_font_type_t, "CAIRO_FONT_TYPE_USER")

fun cairo_font_face_get_type
  (font_face: !cairo_font_face_ref): cairo_font_type_t
  = "atsctrb_cairo_font_face_get_type"

fun cairo_font_face_get_reference_count
  (font_face: !cairo_font_face_ref): uint
  = "atsctrb_cairo_font_face_get_reference_count"

(* ****** ****** *)

// scaled fonts

absviewtype cairo_scaled_font_ref // cairo_scaled_font_t*

fun cairo_scaled_font_reference
  (font: !cairo_scaled_font_ref): cairo_scaled_font_ref
  = "atsctrb_cairo_scaled_font_reference"

fun cairo_scaled_font_destroy
  (font: cairo_scaled_font_ref): void
  = "atsctrb_cairo_scaled_font_destroy"

fun cairo_scaled_font_status
  (font: !cairo_scaled_font_ref): cairo_status_t
  = "atsctrb_cairo_scaled_font_status"

fun cairo_scaled_font_get_reference_count
  (font: !cairo_scaled_font_ref): uint
  = "atsctrb_cairo_scaled_font_reference_count"

fun cairo_scaled_font_extents (
    font: !cairo_scaled_font_ref
  , extents: &cairo_font_extents_t? >> cairo_font_extents_t
  ) : void
  = "atsctrb_cairo_scaled_font_extents"

fun cairo_scaled_font_text_extents (
    font: !cairo_scaled_font_ref
  , utf8: string, extents: &cairo_text_extents_t? >> cairo_text_extents_t
  ) : void
  = "atsctrb_cairo_scaled_font_text_extents"

fun cairo_scaled_font_get_font_face
  (font: !cairo_scaled_font_ref) : cairo_font_face_ref
  = "atsctrb_cairo_scaled_font_get_font_face"

fun cairo_scaled_font_get_font_options (
    font: !cairo_scaled_font_ref, options: !cairo_font_options_ptr
  ) : void
  = "atsctrb_cairo_scaled_font_get_font_options"

fun cairo_scaled_font_get_font_matrix (
    font: !cairo_scaled_font_ref
  , font_matrix: &cairo_matrix_t? >> cairo_matrix_t
  ) : void
  = "atsctrb_cairo_scaled_font_get_font_matrix"

fun cairo_scaled_font_get_ctm (
    font: !cairo_scaled_font_ref
  , ctm: &cairo_matrix_t? >> cairo_matrix_t
  ) : void
  = "atsctrb_cairo_scaled_font_get_ctm"

fun cairo_scaled_font_get_scale_matrix (
    font: !cairo_scaled_font_ref
  , scale_matrix: &cairo_matrix_t? >> cairo_matrix_t
  ) : void
  = "atsctrb_cairo_scaled_font_get_scale_matrix"

fun cairo_scaled_font_get_type
  (font: !cairo_scaled_font_ref): cairo_font_type_t
  = "atsctrb_cairo_scaled_font_get_type"

(* ****** ****** *)

// font options

fun cairo_font_options_create (): cairo_font_options_ptr
  = "atsctrb_cairo_font_options_create"

fun cairo_font_options_copy
  (options: !cairo_font_options_ptr): cairo_font_options_ptr
  = "atsctrb_cairo_font_options_copy"

fun cairo_font_options_destroy (options: cairo_font_options_ptr): void
  = "atsctrb_cairo_font_options_copy"

fun cairo_font_options_status
  (options: !cairo_font_options_ptr): cairo_status_t
  = "atsctrb_cairo_font_options_status"

fun cairo_font_options_merge (
    options: !cairo_font_options_ptr, other: !cairo_font_options_ptr
  ) : void
  = "atsctrb_cairo_font_options_merge"

fun cairo_font_options_hash (options: !cairo_font_options_ptr): ulint
  = "atsctrb_cairo_font_options_hash"

fun cairo_font_options_equal (
    options: !cairo_font_options_ptr, other: !cairo_font_options_ptr
  ) : bool // cairo_bool_t
  = "atsctrb_cairo_font_options_equal"

fun cairo_font_options_get_antialias
  (options: !cairo_font_options_ptr): cairo_antialias_t
  = "atsctrb_cairo_font_options_get_antialias"

fun cairo_set_font_options_antialias
  (options: !cairo_font_options_ptr, antialias: cairo_antialias_t): void
  = "atsctrb_cairo_font_options_set_antialias"

(* ****** ****** *)

//
// surfaces for drawing
//

(* ****** ****** *)

fun cairo_surface_destroy
  (sf: cairo_surface_ref): void
  = "atsctrb_cairo_surface_destroy"

(* ****** ****** *)

//
// image surface
//

(*
// how to handle this:
#define CAIRO_HAS_IMAGE_SURFACE
*)

fun cairo_format_stride_for_width
  (format: cairo_format_t, width: int): int
  = "atsctrb_cairo_format_stride_for_width"

fun cairo_image_surface_create (
    format: cairo_format_t, width: int, height: int
  ) : cairo_surface_ref
  = "atsctrb_cairo_image_surface_create"

(*
cairo_surface_t*
cairo_image_surface_create_for_data (
  unsigned char *data,
  cairo_format_t format,
  int width, int height, int stride
) ;

unsigned char*
cairo_image_surface_get_data (cairo_surface_t *surface);
*)

fun cairo_image_surface_get_format
  (sf: !cairo_surface_ref): cairo_format_t
  = "atsctrb_cairo_image_surface_get_format"

fun cairo_image_surface_get_width (sf: !cairo_surface_ref): int
  = "atsctrb_cairo_image_surface_get_width"

fun cairo_image_surface_get_height (sf: !cairo_surface_ref): int
  = "atsctrb_cairo_image_surface_get_height"

fun cairo_image_surface_get_stride (sf: !cairo_surface_ref): int
  = "atsctrb_cairo_image_surface_get_stride"

fun cairo_image_surface_create_from_png
  (filename: string): cairo_surface_ref
  = "atsctrb_cairo_image_surface_create_from_png"

fun cairo_image_surface_create_from_png_stream
  {v:view} {vt:viewtype} (
    pf: !v
  | read_func: (!v | !vt, string, uint) -<fun1> cairo_status_t
  , env: !vt
  ) : cairo_surface_ref
  = "atsctrb_cairo_image_surface_create_from_png"

fun cairo_image_surface_get_width
  (image: !cairo_surface_ref): double
  = "atsctrb_cairo_image_surface_get_width"

fun cairo_image_surface_get_height
  (image: !cairo_surface_ref): double
  = "atsctrb_cairo_image_surface_get_height"

fun cairo_image_surface_get_stride
  (image: !cairo_surface_ref): double
  = "atsctrb_cairo_image_surface_get_stride"

(* ****** ****** *)

fun cairo_surface_write_to_png
  (sf: !cairo_surface_ref, filename: string): cairo_status_t
  = "atsctrb_cairo_surface_write_to_png"

fun cairo_surface_write_to_png_stream
  {v:view} {vt:viewtype} (
    pf: !v
  | sf: !cairo_surface_ref
  , write_func: (!v | !vt, string, uint) -<fun1> cairo_status_t
  , env: !vt
  ) : cairo_status_t
  = "atsctrb_cairo_surface_write_to_png_stream"

(* ****** ****** *)

//
// PDF surface
//

(*
#define CAIRO_HAS_PDF_SURFACE
*)

fun cairo_pdf_surface_create (
    filename: string, width_in_points: double, height_in_points: double
  ) : cairo_surface_ref
  = "atsctrb_cairo_pdf_surface_create"

fun cairo_pdf_surface_create_null (
    width_in_points: double, height_in_points: double
  ) : cairo_surface_ref
  = "atsctrb_cairo_pdf_surface_create_null"

(*
** note that [pf] and [env] can be freed only after the
** returned surface is destroyed by a call to[cairo_surface_destroy]
*)
fun cairo_pdf_surface_create_for_stream
  {v:view} {vt:viewtype} (
    pf: !v
  | write_func: (!v | !vt, string, uint) -<fun1> cairo_status_t
  , env: !vt
  , width_in_points: double
  , height_in_points: double
  ) : cairo_surface_ref
  = "atsctrb_cairo_pdf_surface_create_for_stream"

fun cairo_pdf_surface_set_size (
    sf: !cairo_surface_ref
  , width_in_points: double, height_in_points: double
  ) : void 
  = "atsctrb_cairo_pdf_surface_set_size"

(* ****** ****** *)

//
// PS surface
//

(*
#define CAIRO_HAS_PS_SURFACE
*)

fun cairo_ps_surface_create (
    filename: string, width_in_points: double, height_in_points: double
  ) : cairo_surface_ref
  = "atsctrb_cairo_ps_surface_create"

fun cairo_ps_surface_create_null (
    width_in_points: double, height_in_points: double
  ) : cairo_surface_ref
  = "atsctrb_cairo_ps_surface_create_null"

(*
** note that [pf] and [env] can be freed only after the
** returned surface is destroyed by a call to[cairo_surface_destroy]
*)
fun cairo_ps_surface_create_for_stream
  {v:view} {vt:viewtype} (
    pf: !v
  | write_func: (!v | !vt, string, uint) -<fun1> cairo_status_t
  , env: !vt
  , width_in_points: double
  , height_in_points: double
  ) : cairo_surface_ref
  = "atsctrb_cairo_ps_surface_create_for_stream"

(*
// enum
abst@ype cairo_ps_level_t =
  $extype "ats_cairo_ps_level_type"

macdef CAIRO_PS_LEVEL_2 =
  $extval (cairo_ps_level_t, "CAIRO_PS_LEVEL_2")
macdef CAIRO_PS_LEVEL_3 =
  $extval (cairo_ps_level_t, "CAIRO_PS_LEVEL_3")

(*
void cairo_ps_get_levels
  (cairo_ps_level_t const **levels, int *num_levels);
*)
fun cairo_ps_get_levels
  (num_levels: &int? >> int n): #[n:nat] array (cairo_ps_level_t, n)
  = "atsctrb_cairo_ps_get_levels"
// end of [cairo_ps_get_levels]

// a null string is returned if [level] is invalid
fun cairo_ps_level_to_string (level: cairo_ps_level_t): Stropt
*)

fun cairo_ps_surface_set_size (
    sf: !cairo_surface_ref
  , width_in_points: double, height_in_points: double
  ) : void 
  = "atsctrb_cairo_ps_surface_set_size"

fun cairo_ps_surface_dsc_begin_setup
  (sf: !cairo_surface_ref) : void
  = "atsctrb_cairo_ps_surface_dsc_begin_setup"

fun cairo_ps_surface_dsc_begin_page_setup
  (sf: !cairo_surface_ref) : void
  = "atsctrb_cairo_ps_surface_dsc_begin_page_setup"

fun cairo_ps_surface_dsc_comment
  (sf: !cairo_surface_ref, comment: string): void
  = "atsctrb_cairo_ps_surface_dsc_comment"

(* ****** ****** *)

//
// utilities for drawing
//

(* ****** ****** *)

// generic matrix operations

fun cairo_matrix_init (
    matrix: &cairo_matrix_t? >> cairo_matrix_t
  , xx: double, yx: double, xy: double, yy: double
  , x0: double, y0: double
  ) : void
  = "cairo_matrix_init"

(* ****** ****** *)

fun cairo_matrix_init_identity
  (matrix: &cairo_matrix_t? >> cairo_matrix_t): void
  = "cairo_matrix_init_identity"

fun cairo_matrix_init_translate (
    matrix: &cairo_matrix_t? >> cairo_matrix_t, tx: double, ty: double
  ) : void
  = "cairo_matrix_init_translate"

fun cairo_matrix_init_scale (
    matrix: &cairo_matrix_t? >> cairo_matrix_t, sx: double, sy: double
  ) : void
  = "cairo_matrix_init_scale"

fun cairo_matrix_init_rotate (
    matrix: &cairo_matrix_t? >> cairo_matrix_t, angle: double
  ) : void
  = "cairo_matrix_init_rotate"

(* ****** ****** *)

fun cairo_matrix_translate (
    matrix: &cairo_matrix_t >> cairo_matrix_t, tx: double, ty: double
  ) : void
  = "cairo_matrix_translate"

fun cairo_matrix_scale (
    matrix: &cairo_matrix_t >> cairo_matrix_t, sx: double, sy: double
  ) : void
  = "cairo_matrix_scale"

fun cairo_matrix_rotate (
    matrix: &cairo_matrix_t >> cairo_matrix_t, angle: double
  ) : void
  = "cairo_matrix_rotate"

(* ****** ****** *)

fun cairo_matrix_invert (
    matrix: &cairo_matrix_t >> cairo_matrix_t
  ) : cairo_status_t
  = "cairo_matrix_invert"

fun cairo_matrix_multiply (
    result: &cairo_matrix_t? >> cairo_matrix_t
  , amatrix: &cairo_matrix_t (*read*)
  , bmatrix: &cairo_matrix_t (*read*)
  ) : void
  = "cairo_matrix_multiply"

fun cairo_matrix_transform_distance (
    matrix: &cairo_matrix_t (*read*), dx: &double, dy: &double
  ) : void
  = "cairo_matrix_transform_distance"

fun cairo_matrix_transform_point (
    matrix: &cairo_matrix_t (*read*), x: &double, y: &double
  ) : void
  = "cairo_matrix_transform_point"

(* ****** ****** *)

// error handling

// all error strings are statically allocated
fun cairo_status_to_string (status: cairo_status_t):<> string
  = "atsctrb_cairo_status_to_string"

fun cairo_debug_reset_static_data (): void
  = "atsctrb_cairo_debug_reset_static_data"

(* ****** ****** *)

(* end of [cairo.sats] *)