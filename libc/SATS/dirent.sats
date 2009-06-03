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

staload TYPES = "libc/sys/SATS/types.sats" // for [off_t]

typedef off_t = $TYPES.off_t

(* ****** ****** *)

%{#

#include "libc/CATS/dirent.cats"

%}

(* ****** ****** *)

// defined in dirent.cats
abst@ype DIR = $extype "ats_DIRype" // = DIR
abst@ype dirent_t = $extype "ats_dirent_type" // = struct dirent

(* ****** ****** *)

fun dirent_d_name_get (ent: &dirent_t): string
  = "atslib_dirent_d_name_get"

(* ****** ****** *)

fun closedir_err {l_dir:addr} (pf: DIR @ l_dir | p: ptr l_dir):<> int
  = "atslib_closedir_err"

fun closedir_exn {l_dir:addr} (pf: DIR @ l_dir | p: ptr l_dir):<!exn> void
  = "atslib_closedir_exn"

(* ****** ****** *)

fun opendir_err (s: string)
  : [l_dir:addr] (option_v (DIR @ l_dir, l_dir <> null) | ptr l_dir)
  = "atslib_opendir_err"

fun opendir_exn (s: string)
  : [l_dir:addr] (DIR @ l_dir | ptr l_dir)
  = "atslib_opendir_exn"

(* ****** ****** *)

fun readdir_err (dir: &DIR)
  :<> [l_ent:addr] (option_v (vbox (dirent_t @ l_ent), l_ent <> null) | ptr l_ent)
  = "atslib_readdir_err"

// this function is nonreentrant
fun readdir_exn (dir: &DIR):<!exn> [l_ent:addr] (vbox (dirent_t @ l_ent) | ptr l_ent)
  = "atslib_readdir_exn"

(* ****** ****** *)

fun rewinddir (dir: &DIR): void = "atslib_rewinddir"

fun telldir (dir: &DIR): off_t = "atslib_telldir"

fun seekdir (dir: &DIR, off: off_t): void = "atslib_seekdir"

(* ****** ****** *)

// [dirent_stream_vt_make_DIR] is reentrant
fun dirent_stream_vt_make_DIR {l_dir:addr}
  (pf: DIR @ l_dir | p: ptr l_dir):<1,~ref> stream_vt (dirent_t)

viewtypedef direntptr_gc =
  [l:addr] (free_gc_v (dirent_t, l), dirent_t @ l | ptr l)

// [direntptr_stream_vt_make_DIR] is reentrant
fun direntptr_stream_vt_make_DIR {l_dir:addr}
  (pf: DIR @ l_dir | p: ptr l_dir):<1,~ref> stream_vt (direntptr_gc)

(* ****** ****** *)

(* end of [dirent.sats] *)
