//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Summer, 2009
//

(* ****** ****** *)

// book: AUP (2nd edition), pages 108 - 110

(* ****** ****** *)

staload TYPES = "libc/sys/SATS/types.sats"
typedef off_t = $TYPES.off_t

staload STDIO = "libc/SATS/stdio.sats"

(* ****** ****** *)

staload "libc/SATS/errno.sats"
staload "libc/SATS/fcntl.sats"
staload "libc/SATS/unistd.sats"

(* ****** ****** *)

extern fun backward2 (path: string): void

(* ****** ****** *)

%{

ats_ptr_type __buf_tail
  (ats_ptr_type p_buf, ats_int_type n) { return ((char*)p_buf) + n ; }
/* end of [__buf_tail] */

%}

(* ****** ****** *)

#define BUFSZ 256

fun loop_main {fd:int} {flag:open_flag} (
    pf_lte: open_flag_lte (flag, rd), pf_fil: !fildes_v (fd, flag)
  | fd: int fd
  , buf: &bytes BUFSZ, n: natLt BUFSZ
  , bufc: &bytes 1
  , ofs: lint
  , err: &int
  ) : void = if ofs >= 0L then let
  val nread = fildes_pread_err (pf_lte, pf_fil | fd, bufc, 1, $TYPES.off_of_lint ofs)
  val nread = int1_of_ssize1 (nread)
in
  case+ 0 of
  | _ when nread = 1 => let
      val b = bufc.[0]
      val c = char_of_byte (b)
      val ofs1 = ofs - 1L
    in
      if c <> '\n' then begin
        if n = 0 then (errno_set (E2BIG); err := err + 1) 
        else (
          buf.[n-1] := b; loop_main (pf_lte, pf_fil | fd, buf, n-1, bufc, ofs1, err)
        ) // end of [if]
      end else let // c = '\n'
        extern fun __buf_tail (buf: &bytes BUFSZ, n: natLt BUFSZ): string
          = "__buf_tail"
        val () = printf ("%s", @(__buf_tail (buf, n)))
        val n = BUFSZ - 1
      in
        buf.[n-1] := b; loop_main (pf_lte, pf_fil | fd, buf, n-1, bufc, ofs1, err)
      end // end of [if]
    end (* end of [_ when nread = 1] *)
  | _ when nread = ~1 => begin
      err := err + 1
    end // end of [_ when ...] 
  | _ => begin
      errno_reset (); err := err + 1
    end // end of [_]
end else begin
  // loop exits successfully
end // end of [loop_main] 

(* ****** ****** *)

implement backward2 (path) = let
  val (pf_fil | fd) = open_flag_exn (path, O_RDONLY)
  var !p_buf with pf_buf = @[byte][BUFSZ]()
  prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
  var !p_bufc with pf_bufc = @[byte][1]()
  prval () = pf_bufc := bytes_v_of_b0ytes_v (pf_bufc)
  val () = p_buf->[BUFSZ-1] := byte_of_int (0)
  val ofs0 = $TYPES.off_of_lint (0L)
  val fsize = fildes_lseek_exn (pf_fil | fd, ofs0, $TYPES.SEEK_END)
  val fsize1 = $TYPES.lint_of_off (fsize) - 1L
  var err: int = 0
  val () = loop_main (open_flag_lte_rd_rd, pf_fil | fd, !p_buf, BUFSZ-1, !p_bufc, fsize1, err)
// (*
  val () = if (err > 0) then begin
    prerrf ("err = %i\n", @(err)); $STDIO.perror "backward2"; 
  end // end of [val]  
// *)
  val () = close_exn (pf_fil | fd)
in
  // empty
end // end of [backward2] 

(* ****** ****** *)

implement main (argc, argv) = let
  val () = assert (argc >= 2)
  val () = backward2 (argv.[1])
in
  // nothing
end // end of [main]

(* ****** ****** *)

(* end of [AUP_2_14_0.dats] *)
