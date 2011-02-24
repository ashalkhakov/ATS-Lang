//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: February, 2011
//
(* ****** ****** *)
//
// How to compile:
//   atscc -IATS $ATSHOME -cc hello_mod.dats
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0
#define ATS_DYNLOADFLAG 0

(* ****** ****** *)

staload "hello_mod.sats"

(* ****** ****** *)

staload "linux/SATS/kernel.sats"

(* ****** ****** *)

staload "linux/SATS/init.sats"
staload "linux/SATS/module.sats"

(* ****** ****** *)

%{^
MODULE_LICENSE("Dual BSD/GPL") ;
%} // end of [%{^]

(* ****** ****** *)

extern
fun hello_init (): int = "hello_init"
extern
fun hello_exit (): void = "hello_exit"

%{$
module_init (hello_init) ;
module_exit (hello_exit) ;
%} // end [%{$]

(* ****** ****** *)

implement hello_init () = let
  val () = printk (KERN_INFO "Hello, world\n", @()) in 0
end // end of [hello_init]

implement hello_exit () = let
  val () = printk (KERN_INFO "Goodbye, cruel world\n", @()) in ()
end // end of [hello_exit]

(* ****** ****** *)

(* end of [hello_mod.dats] *)