(*
**
** An introductory example to UNIX socket programming in ATS
**
** The following code implements a client socket for requesting a server
** to send the current time.
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

#define MAXLINE 1024

#define TIME_SERVER_NAME_DEFAULT "192.43.244.18"
#define TIME_SERVER_PORT_DEFAULT 13
  
implement main (argc, argv) = let
  val servname = (
    if argc > 1 then argv.[1] else TIME_SERVER_NAME_DEFAULT
  ) : string
  val nport = (
    if argc > 2 then int_of_string (argv.[2]) else TIME_SERVER_PORT_DEFAULT
  ) : int
  val servport = in_port_nbo_of_int (nport)
  var inp: in_addr_struct // uninitialized
  // turning a name into an ipv4 address in the network-byte-order
  val () = inet_aton_exn (servname, inp)
  var servaddr: sockaddr_in_struct // uninitialized
  // [sockaddr_ipv4_init] is implemented in [libc/sys/CATS/socket.cats];
  // it initializes an ipv4 socket address with an ipv4 address and a port
  // (represented in the network-byte-order)
  val () = sockaddr_ipv4_init
    (servaddr, AF_INET, in_addr_struct_get_s_addr inp, servport)
  // [socket_family_type_exn] creates a socket of a given family and a given type
  val [fd:int] (pf_sock | sockfd) = socket_family_type_exn (AF_INET, SOCK_STREAM)
  // [connect_ipv4_exn] connects to a server assigned an ipv4 socket address
  val () = connect_ipv4_exn (pf_sock | sockfd, servaddr)
  typedef buf_t = @[byte][MAXLINE]
  val b0 = byte_of_int (0)
  var !p_buf = @[byte][MAXLINE](b0) // allocation on stack
  val () = loop (pf_sock | !p_buf) where {
    fun loop (pf_sock: !socket_v (fd, conn) | buf: &buf_t)
      :<cloref1> void = let
      val n = socket_read_exn (pf_sock | sockfd, buf, MAXLINE)
    in
      if n > 0 then let
        val (pf_stdout | p_stdout) = stdout_get ()
        val () = fwrite_byte_exn (file_mode_lte_w_w | buf, n, !p_stdout)
        val () = stdout_view_set (pf_stdout | (*none*))
      in
        loop (pf_sock | buf)
      end else begin
        // connection is closed by the server
      end // end of [if]
    end // end of [loop]
  } // end of [val]
  val () = socket_close_exn (pf_sock | sockfd) // closing the socket
in
  // empty
end // end of [main]

(* ****** ****** *)

(* end of [daytimetcpclient.dats] *)
