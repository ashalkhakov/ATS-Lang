(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
 * ATS - Unleashing the Potential of Types!
 *
 * Copyright (C) 2002-2008 Hongwei Xi, Boston University
 *
 * All rights reserved
 *
 * ATS is free software;  you can  redistribute it and/or modify it under
 * the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
 * Free Software Foundation; either version 3, or (at  your  option)  any
 * later version.
 * 
 * ATS is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
 * for more details.
 * 
 * You  should  have  received  a  copy of the GNU General Public License
 * along  with  ATS;  see the  file COPYING.  If not, please write to the
 * Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
 *
 *)

(* ****** ****** *)

// author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

(* some coommonly used macro definitions *)

(* ****** ****** *)

#include "prelude/params.hats"

#if VERBOSE_PRELUDE #then

#print "Loading [macrodef.ats] starts!\n"

#endif

(* ****** ****** *)

(* macros in short form *)
macdef min_mac x y = if ,(x) <= ,(y) then ,(x) else ,(y)
macdef max_mac x y = if ,(x) <= ,(y) then ,(y) else ,(x)

(* ****** ****** *)

(* macros in short form *)
// [orelse] and [andalso] are not defined in the curried
// form as they are infix operators
macdef orelse (x, y) = if ,(x) then true else ,(y)
macdef andalso (x, y) = if ,(x) then ,(y) else false

(* ****** ****** *)

(* only a macro in long form can be defined recursively *)
macrodef rec power_mac x(*base:code*) n(*exponent:int*) =
  if n > 1 then `(,(x) * ,(power_mac x (n-1))) else (if n > 0 then x else `(1))

macdef square_mac (x) = ,(power_mac x 2) and cube_mac (x)  = ,(power_mac x 3)

(* ****** ****** *)

macdef print_mac (fprint, x) = let
  val (pf_stdout | ptr_stdout) = stdout_get ()
in
  ,(fprint) (file_mode_lte_w_w | !ptr_stdout, ,(x));
  stdout_view_set (pf_stdout | (*none*))
end // end of [print_mac]

macdef prerr_mac (fprint, x) = let
  val (pf_stderr | ptr_stderr) = stderr_get ()
in
  ,(fprint) (file_mode_lte_w_w | !ptr_stderr, ,(x));
  stderr_view_set (pf_stderr | (*none*))
end // end of [prerr_mac]

(* ****** ****** *)

symintr is_nil; symintr is_cons
symintr tup_head; symintr tup_tail

local

macrodef rec printstar_rec args =
  if is_nil args then `() else `(
    print ,(tup_head args) ; ,(printstar_rec (tup_tail args))
  ) // end of [if]

macrodef rec prerrstar_rec args =
  if is_nil args then `() else `(
    prerr ,(tup_head args) ; ,(prerrstar_rec (tup_tail args))
  ) // end of [if]

in // in of [local]

macdef printstar args = ,(printstar_rec args)
macdef printstarln args = begin ,(printstar_rec args); print_newline () end

macdef prerrstar args = ,(prerrstar_rec args)
macdef prerrstarln args = begin ,(prerrstar_rec args); prerr_newline () end

end // end of [local]

(* ****** ****** *)

#if VERBOSE_PRELUDE #then

#print "Loading [macrodef.ats] finishes!\n"

#endif

(* ****** ****** *)

(* end of [macrodef.ats] *)
