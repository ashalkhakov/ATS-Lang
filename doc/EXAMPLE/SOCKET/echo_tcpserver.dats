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
staload "libc/SATS/unistd.sats"
staload "libc/sys/SATS/socket.sats"
staload "libc/netinet/SATS/in.sats"
staload "libc/arpa/SATS/inet.sats"

(* ****** ****** *)

#define LISTENQ 5
#define MAXLINE 128
#define SERVPORT_DEFAULT 9877

(* ****** ****** *)

extern fun server_action {fd_c:int}
  (pf_sock_c: !socket_v (fd_c, conn) | fd_c: int fd_c): void

implement server_action {fd_c} (pf_sock_c | fd_c) = let
  #define M MAXLINE
  val [l_buf:addr] (pf_gc, pf_buf | p_buf) = malloc_gc (M)
  val () = loop (pf_sock_c, pf_buf | (*none*)) where {
    fun loop (
        pf_sock_c: !socket_v (fd_c, conn)
      , pf_buf: !bytes M @ l_buf
      | (*none*)
      ) :<cloref1> void = let
     val nread = socket_read_exn (pf_sock_c, pf_buf | fd_c, p_buf, MAXLINE)
   in
     if nread > 0 then let
       val nwritten = socket_write_exn (pf_sock_c, pf_buf | fd_c, p_buf, nread)
     in
       loop (pf_sock_c, pf_buf | (*none*))
     end else begin
       // loop exits
     end // end of [if]
   end // end of [loop]
 } // end of [where]
in
  free_gc (pf_gc, pf_buf | p_buf)
end // end of [server_action]

(* ****** ****** *)

extern fun server_loop {fd_s:int}
  (pf_sock_s: !socket_v (fd_s, listen) | fd_s: int fd_s): void

implement server_loop {fd_s} (pf_sock_s | fd_s) = let
  fun loop (pf_sock_s: !socket_v (fd_s, listen) | fd_s: int fd_s): void = let
    val [fd_c:int] (pf_sock_c | fd_c) = accept_null_exn (pf_sock_s | fd_s)
    viewdef V = @(socket_v (fd_s, listen), socket_v (fd_c, conn))
    prval pf = @(pf_sock_s, pf_sock_c)
    val f_child = lam (pf: V | (*none*)): void =<cloptr1> let
      prval @(pf_sock_s, pf_sock_c) = pf
      val () = socket_close_exn (pf_sock_s | fd_s)
      val () = server_action (pf_sock_c | fd_c)
      val () = socket_close_exn (pf_sock_c | fd_c)
    in
      // empty
    end // f_child
    val () = fork_exec_cloptr_exn {V} (pf | f_child)
    prval () = pf_sock_s := pf.0
    prval () = pf_sock_c := pf.1
    val () = socket_close_exn (pf_sock_c | fd_c)
  in
    loop (pf_sock_s | fd_s)
  end // end of [loop]
in
  loop (pf_sock_s | fd_s)
end // end of [server_loop]

(* ****** ****** *)

implement main (argc, argv) = let
  val nport = (if argc > 1 then int_of argv.[1] else SERVPORT_DEFAULT): int
  val [fd_s:int] (pf_sock_s | fd_s) = socket_family_type_exn (AF_INET, SOCK_STREAM)
  var servaddr: sockaddr_in_struct_t // uninitialized
  val servport = in_port_nbo_of_int (nport)
  val in4addr_any = in_addr_nbo_of_hbo (INADDR_ANY)
  val () = sockaddr_ipv4_init (servaddr, AF_INET, in4addr_any, servport)
  val () = bind_ipv4_exn (pf_sock_s | fd_s, servaddr)
  val () = listen_exn (pf_sock_s | fd_s, LISTENQ) 
  val () = server_loop (pf_sock_s | fd_s)
  val () = socket_close_exn (pf_sock_s | fd_s)
in
  exit (0)
end // end of [main]

(* ****** ****** *)

(* end of [echo_tcpserver.dats] *)
