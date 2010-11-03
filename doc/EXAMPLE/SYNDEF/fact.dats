(*
** some code for testing syndef
*)

(* ****** ****** *)
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: November, 2010
//
(* ****** ****** *)

fun fact
  {n:pos} .<>.
  (n: int n): int = let
  var n: int = n
  var res: int = 1
  val () = `do` {
    val () = res := n * res; val () = n := n-1
  } `while` (n >= 1)
in
  res
end // end of [f]

implement main () = let
  val ans = fact (10) in print "10! = "; print ans; print_newline ()
end // end of [main]

(* ****** ****** *)

(* end of [fact.dats] *)