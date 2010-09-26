(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
**
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

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

%{#
#include "libc/CATS/grp.cats"
%} // end of [%{#]

(* ****** ****** *)

staload T = "libc/sys/SATS/types.sats"
typedef gid_t = $T.gid_t

(* ****** ****** *)

(*
struct group {
  char *gr_name;		/* Group name.	*/
  char *gr_passwd;		/* Password.	*/
  __gid_t gr_gid;		/* Group ID.	*/
  char **gr_mem;		/* Member list.	*/ // null-terminated
} ;
*)
abst@ype
group_rest // unknown quantity
typedef group_struct =
$extype_struct "ats_group_type" of {
  gr_gid= gid_t
, _rest= group_rest
} // end of [group]
typedef group = group_struct

(* ****** ****** *)

fun group_get_gr_name
  (grp: &group): [l:addr] (strptr l -<lin,prf> void | strptr l)
  = "atslib_group_get_gr_name" // fun!
// end of [group_get_gr_name]

fun group_get_gr_passwd
  (grp: &group): [l:addr] (strptr l -<lin,prf> void | strptr l)
  = "atslib_group_get_gr_passwd" // fun!
// end of [group_get_gr_passwd]

//
// HX: please use with caution!
//
fun group_get_gr_mem
  (grp: &group): ptr = "atslib_group_get_gr_mem" // fun!
// end of [group_get_gr_mem]

(* ****** ****** *)

// HX: non-reentrant
fun getgrnam (nam: string):<!ref>
  [l:addr] (ptroutopt (group, l) | ptr l) = "#atslib_getgrnam"
// end of [getgrnam]

// HX: non-reentrant
fun getgrgid (gid: gid_t):<!ref>
  [l:addr] (ptroutopt (group, l) | ptr l) = "#atslib_getgrgid"
// end of [getgrgid]

(* ****** ****** *)

(* end of [grp.sats] *)
