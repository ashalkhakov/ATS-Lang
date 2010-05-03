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

// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: Summer, 2008

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no need for dynamic loading

(* ****** ****** *)

staload "contrib/GL/SATS/glut.sats"

(* ****** ****** *)

implement glutStrokeString (font, s) = let
  val [n:int] s = string1_of_string s
  var i: sizeLte n = size1_of_int1 (0)
in
  while (string_isnot_at_end (s, i)) let
    val () = glutStrokeCharacter (font, s[i]) in i := i+1
  end // end of [while]
end // end of [glutStrokeString]

(* ****** ****** *)

implement glutBitmapString (font, s) = let
  val [n:int] s = string1_of_string s
  var i: sizeLte n = size1_of_int1 (0)
in
  while (string_isnot_at_end (s, i)) let
    val () = glutBitmapCharacter (font, s[i]) in i := i+1
  end // end of [while]
end // end of [glutBitmapString]

(* ****** ****** *)

(* end of [glut.dats] *)
