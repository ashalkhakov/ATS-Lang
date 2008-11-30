(*
**
** An introductory example to UNIX socket programming in ATS
**
** The following code implements a client socket for sending a line to
** a server that echos it back.
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: November, 2008
**
*)

(* ****** ****** *)

staload "libc/SATS/stdio.sats"
staload "libc/sys/SATS/socket.sats"
staload "libc/netinet/SATS/in.sats"
staload "libc/arpa/SATS/inet.sats"

(* ****** ****** *)

#define MAXLINE 128
#define SERVPORT 9877

fn prerr_usage (cmd: string): void = begin
  prerrf ("usage: %s <IPaddress>", @(cmd)); prerr_newline ()
end // end of [print_usage]

(* ****** ****** *)

extern fun client_loop {fd:int}
  (pf_sock: !socket_v (fd, conn) | sockfd: int fd): void

(* ****** ****** *)

implement client_loop {fd:int} (pf_sock | sockfd) = let
  #define M MAXLINE
  val [l_buf_send:addr] (pf_gc_send, pf_buf_send | p_buf_send) = malloc_gc (M)
  val [l_buf_recv:addr] (pf_gc_recv, pf_buf_recv | p_buf_recv) = malloc_gc (M)
  fun loop {m:file_mode} (
      pf_sock: !socket_v (fd, conn)
    , pf_buf_send: !bytes M @ l_buf_send
    , pf_buf_recv: !bytes M @ l_buf_recv
    , pf_mod: file_mode_lte (m, r)
    | fil: &FILE m
    ) :<cloref1> void = let
    prval pf_buf_send0 = pf_buf_send
    val (pf_fgets | p) = fgets_err (pf_mod, pf_buf_send0 | p_buf_send, M, fil)
  in
    if p <> null then let
      prval fgets_v_succ (pf_buf_send1) = pf_fgets
      val nsend = strbuf_length (!p_buf_send)
      prval () = pf_buf_send := bytes_v_of_strbuf_v (pf_buf_send1)
      val nwritten = socket_write_exn (pf_sock, pf_buf_send | sockfd, p_buf_send, nsend)
      val nread = socket_read_exn (pf_sock, pf_buf_recv | sockfd, p_buf_recv, nsend)
      val (pf_stdout | p_stdout) = stdout_get ()
      val () = fwrite_byte_exn (file_mode_lte_w_w, pf_buf_recv | p_buf_recv, nread, !p_stdout)
      val () = stdout_view_set (pf_stdout | (*none*))
    in
      loop (pf_sock, pf_buf_send, pf_buf_recv, pf_mod | fil)
    end else let
      prval fgets_v_fail (pf_buf_send1) = pf_fgets
      prval () = pf_buf_send := pf_buf_send1
    in
      // loop exists
    end // end of [if]
  end // end of [loop]
  val (pf_stdin | p_stdin) = stdin_get ()
  val () = loop (pf_sock, pf_buf_send, pf_buf_recv, file_mode_lte_r_r | !p_stdin)
  val () = stdin_view_set (pf_stdin | (*none*))
  val () = free_gc (pf_gc_send, pf_buf_send | p_buf_send)
  val () = free_gc (pf_gc_recv, pf_buf_recv | p_buf_recv)
in
  // empty
end // end of [client_loop]

(* ****** ****** *)

implement main (argc, argv) = let
  val () = if (argc <> 2) then prerr_usage (argv.[0])
  val () = assert (argc = 2) // redundant at run-time
  val servname = argv.[1]
  var inp: in_addr_struct_t // uninitialized
  val () = inet_aton_exn (servname, inp)
  var servaddr: sockaddr_in_struct_t // uninitialized
  val () = sockaddr_ipv4_init
    (servaddr, AF_INET, in_addr_struct_s_addr_get inp, in_port_nbo_of_int SERVPORT)
  val [fd:int] (pf_sock | sockfd) = socket_family_type_exn (AF_INET, SOCK_STREAM)
  val () = connect_ipv4_exn (pf_sock | sockfd, servaddr)
  val () = client_loop (pf_sock | sockfd)
  val () = socket_close_exn (pf_sock | sockfd)
in
  exit (0)
end // end of [main]

(* ****** ****** *)

(* end of [echo_tcpclient.dats] *)