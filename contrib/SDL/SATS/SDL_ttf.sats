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
// Starting time: January, 2010

(* ****** ****** *)

%{#
#include "contrib/SDL/CATS/SDL_ttf.cats"
%} // end of [%{#]

(* ****** ****** *)

staload "contrib/SDL/SATS/SDL.sats"

(* ****** ****** *)

// is this type reference counted?
absviewtype TTF_Font_ref (l:addr) // TTF_Font* or null
viewtypedef TTF_Font_ref0 = [l:addr] TTF_Font_ref l
viewtypedef TTF_Font_ref1 = [l:addr | l <> null] TTF_Font_ref l

castfn TTF_Font_ref_null (p: ptr null): TTF_Font_ref null

castfn TTF_Font_ref_free_null (sf: TTF_Font_ref null): ptr
// overload ref_free_null with TTF_Font_ref_free_null

fun TTF_Font_ref_is_null
  {l:addr} (x: !TTF_Font_ref l):<> bool (l == null)
  = "atsctrb_SDL_ref_is_null"
// overload ref_is_null with TTF_Font_ref_is_null

fun TTF_Font_ref_is_notnull
  {l:addr} (x: !TTF_Font_ref l):<> bool (l <> null)
  = "atsctrb_SDL_ref_is_notnull"
// overload ref_is_notnull with TTF_Font_ref_is_notnull

(* ****** ****** *)

fun TTF_Init (): int (*err*) = "atsctrb_TTF_Init"

(* ****** ****** *)

(*
extern DECLSPEC TTF_Font * SDLCALL TTF_OpenFont(const char *file, int ptsize);
extern DECLSPEC TTF_Font * SDLCALL TTF_OpenFontIndex(const char *file, int ptsize, long index);
extern DECLSPEC TTF_Font * SDLCALL TTF_OpenFontRW(SDL_RWops *src, int freesrc, int ptsize);
extern DECLSPEC TTF_Font * SDLCALL TTF_OpenFontIndexRW(SDL_RWops *src, int freesrc, int ptsize, long index);
*)

fun TTF_OpenFont (filename: string, ptsize: int): TTF_Font_ref0
  = "atsctrb_TTF_OpenFont"

(* ****** ****** *)

macdef TTF_STYLE_NORMAL = $extval (int, "TTF_STYLE_NORMAL")
macdef TTF_STYLE_BOLD = $extval (int, "TTF_STYLE_BOLD")
macdef TTF_STYLE_ITALIC = $extval (int, "TTF_STYLE_ITALIC")
macdef TTF_STYLE_UNDERLINE = $extval (int, "TTF_STYLE_UNDERLINE")

fun TTF_GetFontStyle {l:anz} (font: !TTF_Font_ref l): int(*style*)
  = "atsctrb_TTF_GetFontStyle"
  
fun TTF_SetFontStyle {l:anz} (font: !TTF_Font_ref l, style: int): void
  = "atsctrb_TTF_SetFontStyle"

fun TTF_FontHeight {l:anz} (font: !TTF_Font_ref l): int(*height*)
  = "atsctrb_TTF_FontHeight"

fun TTF_FontAscent {l:anz} (font: !TTF_Font_ref l): int(*ascent*)
  = "atsctrb_TTF_FontAscent"

fun TTF_FontDescent {l:anz} (font: !TTF_Font_ref l): int(*descent*)
  = "atsctrb_TTF_FontDescent"

fun TTF_FontLineSkip {l:anz} (font: !TTF_Font_ref l): int(*lineskip*)
  = "atsctrb_TTF_FontLineSkip"

fun TTF_FontFaces {l:anz} (font: !TTF_Font_ref l): lint(*number of faces*)
  = "atsctrb_TTF_FontFaces"

(* ****** ****** *)

fun TTF_FontFaceIsFixedWidth {l:anz} (font: !TTF_Font_ref l): int
  = "atsctrb_TTF_FontFaceIsFixedWidth"

fun TTF_FontFaceFamilyName {l:anz} (font: !TTF_Font_ref l): string
  = "atsctrb_TTF_FontFaceFamilyName"

fun TTF_FontFaceStyleName {l:anz} (font: !TTF_Font_ref l): string
  = "atsctrb_FontFaceStyleName"

(* ****** ****** *)

fun TTF_SizeText {l:anz}
  (font: !TTF_Font_ref l, txt: string, w: &int? >> int, h: &int? >> int): int(*err*)
  = "atsctrb_TTF_SizeText"

fun TTF_SizeUTF8 {l:anz}
  (font: !TTF_Font_ref l, txt: string, w: &int? >> int, h: &int? >> int): int(*err*)
  = "atsctrb_TTF_SizeUTF8"

(*
fun TTF_SizeUTF16 {l:anz}
  (font: !TTF_Font_ref l, txt: wstring, w: &int? >> int, h: &int? >> int): int
  = "atsctrb_TTF_SizeUTF16"
*)

(* ****** ****** *)

fun TTF_GlyphMetrics {l:anz} (
    font: !TTF_Font_ref l, ch: Uint16
  , minx: &int? >> int, maxx: &int? >> int
  , miny: &int? >> int, maxy: &int? >> int
  , advance: &int? >> int
  ) : int (*err*)
  = "atsctrb_TTF_GlyphMetrics"

(* ****** ****** *)

fun TTF_RenderText_Solid {l:anz}
  (font: !TTF_Font_ref l, txt: string, fg: &SDL_Color): SDL_Surface_ref0
  = "atsctrb_TTF_RenderText_Solid"

fun TTF_RenderUTF8_Solid {l:anz}
  (font: !TTF_Font_ref l, txt: string, fg: &SDL_Color): SDL_Surface_ref0
  = "atsctrb_TTF_RenderUTF8_Solid"

(*
fun TTF_RenderUTF16_Solid {l:anz}
  (font: !TTF_Font_ref l, txt: wstring, fg: &SDL_Color): SDL_Surface_ref0
  = "atsctrb_TTF_RenderUTF16_Solid"
*)

(* ****** ****** *)

fun TTF_RenderText_Shaded {l:anz}
  (font: !TTF_Font_ref l, txt: string, fg: &SDL_Color, bg: &SDL_Color): SDL_Surface_ref0
  = "atsctrb_TTF_RenderText_Shaded"

fun TTF_RenderUTF8_Shaded {l:anz}
  (font: !TTF_Font_ref l, txt: string, fg: &SDL_Color, bg: &SDL_Color): SDL_Surface_ref0
  = "atsctrb_TTF_RenderUTF8_Shaded"

(*
fun TTF_RenderUTF16_Shaded {l:anz}
  (font: !TTF_Font_ref l, txt: wstring, fg: &SDL_Color, bg: &SDL_Color): SDL_Surface_ref0
  = "atsctrb_TTF_RenderUTF16_Shaded"
*)

(* ****** ****** *)

fun TTF_RenderGlyph_Solid {l:anz}
  (font: !TTF_Font_ref l, ch: Uint16, fg: &SDL_Color): SDL_Surface_ref0
  = "atsctrb_TTF_RenderGlyph_Solid"

(* ****** ****** *)

fun TTF_CloseFont {l:anz}
  (font: TTF_Font_ref l): void = "atsctrb_TTF_CloseFont"
// end of [TTF_CloseFont]

(* ****** ****** *)

fun TTF_Quit (): void = "atsctrb_TTF_Quit"
fun TTF_WasInit (): int = "atsctrb_TTF_WasInit"

(* ****** ****** *)

(* end of [SDL_ttf.sats] *)
