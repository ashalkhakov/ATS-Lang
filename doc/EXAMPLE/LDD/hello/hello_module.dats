//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: February, 2011
//
(* ****** ****** *)
//
// How to compile:
//   atscc -IATS $ATSHOME/contrib/linux -cc hello_module.dats
//
(* ****** ****** *)

#define ATS_DYNLOADFLAG 0

(* ****** ****** *)

staload "linux/SATS/kernel.sats"

(* ****** ****** *)

staload "linux/SATS/init.sats"
staload "linux/SATS/module.sats"

(* ****** ****** *)

staload "hello_module.sats"

(* ****** ****** *)

%{^
MODULE_LICENSE("Dual BSD/GPL") ;
%} // end of [%{^]

(* ****** ****** *)

extern
fun hello_init (): int = "hello_init"
extern
fun hello_exit (): void = "hello_exit"

(*
%{$
module_init (hello_init) ;
module_exit (hello_exit) ;
%} // end [%{$]
*)

(* ****** ****** *)

implement hello_init () = let
  val () = printk (KERN_ALERT "Hello, world\n", @()) in 0
end // end of [hello_init]

implement hello_exit () = let
  val () = printk (KERN_ALERT "Goodbye, cruel world\n", @()) in ()
end // end of [hello_exit]

(* ****** ****** *)

implement
main () = () where {
  val _ = hello_init ()
  val () = hello_exit ()
}

(* ****** ****** *)

(* end of [hello_module.dats] *)
