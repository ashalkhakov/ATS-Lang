(*
** some testing code for functions declared in
** prelude/SATS/integer.sats
*)
(* ****** ****** *)
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: April, 2012
//
(* ****** ****** *)
//
// staload "prelude/SATS/integer.sats"
//
(* ****** ****** *)

implement
main () = {
  var i: int
  val () = fprintf (stdout_ref, "Please input an integer:\n", @())
  val () = fscan_int_exn (stdin_ref, i)
  val () = fprintf (stdout_ref, "The input integer is [%d]\n", @(i))
} // end of [main]

(* ****** ****** *)

(* end of [prelude_integer.dats] *)
