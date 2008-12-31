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
**
*)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

%{#

#include "libc/CATS/fcntl.cats"

%}

(* ****** ****** *)

datasort open_flag =
  | open_flag_rd (* read *)
  | open_flag_wr (* write *)
  | open_flag_rdwr (* read and write *)

stadef rd = open_flag_rd
stadef wr = open_flag_wr
stadef rdwr = open_flag_rdwr

(* ****** ****** *)

abst@ype flag_t (open_flag) = $extype "ats_int_type"

macdef O_RDONLY = $extval (flag_t rd, "O_RDONLY")
macdef O_WRONLY = $extval (flag_t wr, "O_WRONLY")
macdef O_RDWR   = $extval (flag_t rdwr, "O_RDWR")

abst@ype orflag_t = $extype "ats_int_type"

macdef O_CREAT = $extval (orflag_t, "O_CREAT")

(*
macdef O_EXCL
macdef O_NOCTTY
macdef O_TRUNC
macdef O_APPEND
macdef O_NONBLOCK
macdef O_NDELAY
macdef O_SYNC
macdef O_NOFOLLOW
macdef O_DIRECTORY
macdef O_DIRECT
macdef O_ASYNC
macdef O_LARGEFILE
*)

fun or_flag_orflag {f:open_flag}
  (f: flag_t f, orf: orflag_t): flag_t f = "atslib_or_flag_orflag"

overload || with or_flag_orflag

(* ****** ****** *)

absview fildes_v (int, open_flag) // file descriptor view
dataview fildes_opt_v (int, open_flag) =
  | {fd:nat} {flag:open_flag}
    fildes_some (fd, flag) of fildes_v (fd, flag)
  | {flag:open_flag} fildes_none (~1, flag) of ()

(* ****** ****** *)

dataview open_v (int, open_flag) =
  | {i:nat} {f:open_flag} open_v_succ (i, f) of fildes_v (i, f)
  | {f:open_flag} open_v_fail (~1, f) of ()

fun open_path_flag_err {f:open_flag}
  (path: string, flag: flag_t f): [i: int] (fildes_opt_v (i, f) | int i)
  = "atslib_open_path_flag_err"

(* ****** ****** *)

dataview close_v (fd: int, flag: open_flag, int) =
  | close_v_succ (fd, flag,  0) of ()
  | close_v_fail (fd, flag, ~1) of fildes_v (fd, flag)

fun close_err {fd:int} {flag: open_flag}
  (pf: fildes_v (fd, flag) | fd: int fd)
  : [i:int] (close_v (fd, flag, i) | int i)
  = "atslib_close_err"

// implemented in [libc/DATS/fcntl.dats]
fun close_exn {fd:int} {flag: open_flag}
  (pf: fildes_v (fd, flag) | fd: int fd): void
  = "atslib_close_exn"

(* ****** ****** *)

(* end of [fcntl.sats] *)
