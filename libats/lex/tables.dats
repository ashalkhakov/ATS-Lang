(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Anairiats - Unleashing the Potential of Types!
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
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

//
// Author: Hongwei Xi (* hwxi AT cs DOT bu DOT edu *)
// Time: July 2007
//
// Author: Artyom Shalkhakov (* artyom DOT shalkhakov AT gmail DOT com *)
// Time: March 2013
//

(* ****** ****** *)

staload "libats/lex/lexing.sats"

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/array.dats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

assume accept_table_t = [n:nat] '(array (int, n), int n(*ntot*))

implement
accept_table_of_array {n} {l} (pf_arr | p_arr, ntot) = '(
  array_make_view_ptr (pf_arr | p_arr), ntot
) // end of [accept_table_of_array]

implement
accept_table_get
  (tbl, nstate) = let
  val '(arr, ntot) = tbl
  val nstate = int1_of_int nstate
in
  if nstate >= 0 && nstate < ntot then arr[nstate]
  else exit_errmsg (1, "lexing: accept_table_get: state number is illegal\n")
end // end of [accept_table_get]

(* ****** ****** *)

assume transition_table_t = [n:nat] '(array (List @(int, int, int), n), int n(*ntot*))

(* ****** ****** *)

implement
transition_table_of_array {n} {l} (pf_arr | p_arr, ntot) = '(
  array_make_view_ptr {List @(int, int, int)} (pf_arr | p_arr), ntot
) // end of [transition_table_of_array]

implement
transition_table_get (tbl, nstate, c) = let
  fun loop (xs: List @(int, int, int), c: int): int =
    case+ xs of
    | list_cons (@(c1, c2, n), xs) => if c1 <= c && c <= c2 then n else loop (xs, c)
    | list_nil () => 0(*no transition: error*)
  // end of [loop]
(*
  val () = printf (
    "transition_table_get: state = %i and c = %i\n", @(nstate, c)
  ) // end of [val]
*)
  val '(arr, ntot) = tbl
  val nstate = nstate - 1 // we don't store transitions for state 0, which is an error state
  val nstate = int1_of_int nstate
  val ans = begin
    if nstate >= 0 && nstate < ntot then loop (arr[nstate], c)
    else 0
  end // end of [val]
(*
  val () = begin
    prerr "transition_table_get: ans = "; prerr ans; prerr_newline ()
  end // end of [val]
*)
in
  ans
end // end of [transition_table_get]

(* ****** ****** *)

(* end of [tables.dats] *)
