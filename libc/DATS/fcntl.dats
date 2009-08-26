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
*)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

staload "libc/SATS/errno.sats"
staload "libc/SATS/stdio.sats"

(* ****** ****** *)

staload "libc/SATS/fcntl.sats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // no dynamic loading is needed

(* ****** ****** *)

macdef errno_is_EINTR () = (errno_get () = EINTR)

(* ****** ****** *)

#define i2sz size1_of_int1
#define ssz2sz size1_of_ssize1
#define i2ssz ssize1_of_int1
#define sz2ssz ssize1_of_size1

(* ****** ****** *)

implement close_loop_err
  {fd} {flag} (pf_fd | fd) =
  $effmask_all (loop (pf_fd | fd)) where {
  fun loop
    (pf_fd: fildes_v (fd, flag) | fd: int fd)
    : [i:int] (close_v (fd, flag, i) | int i) = let
    val (pf_err | i) = close_err (pf_fd | fd)
  in
    if i >= 0 then (pf_err | i) else begin
      if errno_is_EINTR () then let
        prval close_v_fail pf_fd = pf_err in loop (pf_fd | fd)
      end else (pf_err | i) // end of [if]
    end // end of [if]
  end (* end of [loop] *)
} // end of [close_loop_err]

implement close_loop_exn (pf_fd | fd) = let
  val (pf_err | i) = close_loop_err (pf_fd | fd)
in
  if (i >= 0) then let
    prval close_v_succ () = pf_err in (*empty*)
  end else let
    prval close_v_fail pf_fd = pf_err
    val () = exit_main {void} {..} {unit_v} (pf_fd | 1)
    prval unit_v () = pf_fd
  in
    // empty
  end // end of [if]
end // end of [close_loop_exn]

(* ****** ****** *)

extern praxi bytes_v_split {n,i:nat | i <= n}
  {l:addr} (pf: bytes n @ l): @(bytes i @ l, bytes (n-i) @ l + i)

extern praxi bytes_v_unsplit {n1,n2:nat}
  {l:addr} (pf1: bytes n1 @ l, pf2: bytes n2 @ l + n1): bytes (n1+n2) @ l

implement fildes_read_loop_err
  {fd} {flag} {n,sz} (pf_lte, pf_fd | fd, buf, ntotal) = let
  fun loop {nleft:nat | nleft <= n} {l:addr} (
      pf_fd: !fildes_v (fd, flag)
    , pf_buf: !bytes (sz-n+nleft) @ l
    | fd: int fd, p_buf: ptr l, nleft: size_t nleft, err: &int
    ) : sizeLte n =
    if nleft > 0 then let
      val [nread:int] nread = fildes_read_err (pf_lte, pf_fd | fd, !p_buf, nleft)
    in
      if nread > 0 then let
        val nread = ssz2sz nread
        prval @(pf1_buf, pf2_buf) = bytes_v_split {sz-n+nleft,nread} (pf_buf)
        val nleft2 = loop (pf_fd, pf2_buf | fd, p_buf + nread, nleft - nread, err)
        prval () = pf_buf := bytes_v_unsplit (pf1_buf, pf2_buf)
      in
        nleft2
      end else let // nread <= 0
        val retry = begin
          if nread < 0 then errno_is_EINTR () else false (*EOF*)
        end : bool
      in        
        if retry then loop (pf_fd, pf_buf | fd, p_buf, nleft, err)
        else (if nread < 0 then err := 1; nleft)
      end // end of [if]
    end else begin
      i2sz 0 // all bytes are read
    end // end of [if]
  // end of [loop]
  var err: int = 0; val nleft = loop (pf_fd, view@ buf | fd, &buf, ntotal, err)
in
  if err = 0 then sz2ssz (ntotal - nleft) else i2ssz (~1)
end // end of [fildes_read_loop_err]

implement fildes_read_loop_exn
  (pf_lte, pf_fd | fd, buf, ntotal) = let
  val nread = fildes_read_loop_err (pf_lte, pf_fd | fd, buf, ntotal)
in
  if nread >= 0 then ssz2sz (nread) else (perror "fildes_read: "; exit 1)
end // end of [fildes_read_loop_exn]

(* ****** ****** *)

implement fildes_write_loop_err
  {fd} {flag} {n,sz} (pf_lte, pf_fd | fd, buf, ntotal) = let
  fun loop {nleft:nat | nleft <= n} {l:addr} (
      pf_fd: !fildes_v (fd, flag)
    , pf_buf: !bytes (sz-n+nleft) @ l
    | fd: int fd, p_buf: ptr l, nleft: size_t nleft, err: &int
    ) : sizeLte n =
    if nleft > 0 then let
      val [nwrit:int] nwrit = fildes_write_err (pf_lte, pf_fd | fd, !p_buf, nleft)
    in
      if nwrit > 0 then let
        val nwrit = ssz2sz (nwrit)
        prval @(pf1_buf, pf2_buf) = bytes_v_split {sz-n+nleft,nwrit} (pf_buf)
        val nleft2 = loop (pf_fd, pf2_buf | fd, p_buf + nwrit, nleft - nwrit, err)
        prval () = pf_buf := bytes_v_unsplit (pf1_buf, pf2_buf)
      in
        nleft2
      end else let
        val retry = (if nwrit < 0 then errno_is_EINTR () else true): bool
      in
        if retry then loop (pf_fd, pf_buf | fd, p_buf, nleft, err) else (err := 1; nleft)
      end // end of [if]
    end else begin
      i2sz 0 // all bytes are written
    end // end of [if]
  // end of [loop]
  var err: int = 0; val nleft = loop (pf_fd, view@ buf | fd, &buf, ntotal, err)
in
  if err = 0 then sz2ssz (ntotal - nleft) else i2ssz (~1)
end // end of [fildes_write_loop_err]

//

implement fildes_write_loop_exn
  (pf_lte, pf_fd | fd, buf, ntotal) = let
  var err: int = 1
  val nwrit = fildes_write_loop_err (pf_lte, pf_fd | fd, buf, ntotal)
  val () = if nwrit >= 0 then let
    val nwrit = ssz2sz (nwrit) in if nwrit = ntotal then (err := 0)
  end // end of [val]
in  
  if err > 0 then (perror "fildes_write: "; exit 1)
end // end of [fildes_write_loop_exn]

(* ****** ****** *)

(* end of [fcntl.dats] *)
