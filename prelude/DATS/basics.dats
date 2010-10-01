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

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading

(* ****** ****** *)

(*
** prfun verify_constraint {p:bool | p} (): [p] void // verify and add
*)
implement verify_constraint () = ()

(* ****** ****** *)

// file_mode_lte_r_r : declared in basic_dyn.ats
implement file_mode_lte_r_r = file_mode_lte_refl {r} ()

// file_mode_lte_w_w : declared in basic_dyn.ats
implement file_mode_lte_w_w = file_mode_lte_refl {w} ()

(* ****** ****** *)

%{^

/*
** various functions for exits
*/

ats_void_type // external
ats_exit
  (ats_int_type status) { exit(status) ; return ; }
// end of [ats_exit]

ats_void_type // external
ats_exit_errmsg (
  ats_int_type status
, ats_ptr_type errmsg
) {
  fprintf(stderr, "%s", (char*)errmsg) ; exit(status) ;
  return ; // deadcode
} /* end of [ats_exit_errmsg] */

%} // end of [%{^]

(* ****** ****** *)

%{^

/*
** various functions for asserts
*/

ats_void_type
atspre_assert (
  ats_bool_type assertion
) {
  if (!assertion) {
    fprintf (stderr, "exit(ATS): [assert] failed\n") ; exit(1) ;
  } // end of [if]
  return ;
} /* end of [atspre_assert] */

ats_void_type
atspre_assert_errmsg (
  ats_bool_type assertion, ats_ptr_type errmsg
) {
  if (!assertion) {
    fprintf (stderr, "exit(ATS)%s\n", (char*)errmsg) ; exit(1) ;
  } // end of [if]
  return ;
} /* end of [atspre_assert_errmsg] */

%} // end of [%{]

(* ****** ****** *)

(* end of [basics.dats] *)
