(*
** some testing code for functions declared in
** libc/SATS/stdlib.sats
*)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: May, 2009
//

(* ****** ****** *)

staload "libc/SATS/dlfcn.sats"

(* ****** ****** *)

typedef ftrig_t = (double) -> double
fun ftrig_get {l:addr} .<>. (
    pf_lib: !dlopen_v l | p_lib: ptr l, name: string
  ) : ftrig_t = let
  val _msg = dlerror () // clearing any existing error
  val _fcos = dlsym (pf_lib | p_lib, name)
  val _msg = dlerror () // see if there is any error
  val () = assert_errmsg (stropt_is_none _msg, #LOCATION)
in
  __cast (_fcos) where { extern castfn __cast (x: ptr):<> ftrig_t }
end // end of [fcos]

(* ****** ****** *)

#define PI 3.1415926535898

implement main () = () where {
  val (pf_lib | p_lib) = dlopen_exn ("libm.so", RTLD_LAZY)
  val fsin = ftrig_get (pf_lib | p_lib, "sin")
  val sin00 = fsin (0.0)
  val () = (print "sin00 = "; print sin00; print_newline ())
  val sin30 = fsin (PI / 6)
  val () = (print "sin30 = "; print sin30; print_newline ())
  val sin45 = fsin (PI / 4)
  val () = (print "sin45 = "; print sin45; print_newline ())
  val sin60 = fsin (PI / 3)
  val () = (print "sin60 = "; print sin60; print_newline ())
  val sin90 = fsin (PI / 2)
  val () = (print "sin90 = "; print sin90; print_newline ())
//
  val fcos = ftrig_get (pf_lib | p_lib, "cos")
  val cos00 = fcos (0.0)
  val () = (print "cos00 = "; print cos00; print_newline ())
  val cos30 = fcos (PI / 6)
  val () = (print "cos30 = "; print cos30; print_newline ())
  val cos45 = fcos (PI / 4)
  val () = (print "cos45 = "; print cos45; print_newline ())
  val cos60 = fcos (PI / 3)
  val () = (print "cos60 = "; print cos60; print_newline ())
  val cos90 = fcos (PI / 2)
  val () = (print "cos90 = "; print cos90; print_newline ())
//
  val _err = dlclose (pf_lib | p_lib)
} // end of [main]

(* ****** ****** *)

(* end of [libc_dlfcn.dats] *)
