(*
**
** A simple CAIRO example: Koch curve
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: December, 2009
**
*)

(* ****** ****** *)

staload "libc/SATS/math.sats"
staload "contrib/cairo/SATS/cairo.sats"

(* ****** ****** *)

stadef dbl = double
stadef cr = cairo_ref

(* ****** ****** *)

val PI3 = M_PI / 3
val sin60 = sin (PI3)

fn koch0
  (cr: !cr, x: dbl): void = let
  val () = cairo_move_to (cr, 0.0, 0.0)
  val () = cairo_line_to (cr, x / 3, 0.0)
  val () = cairo_line_to (cr, x / 2, ~x / 3 * sin60)
  val () = cairo_line_to (cr, 2 * x / 3, 0.0)
  val () = cairo_line_to (cr, x, 0.0)
in
  // nothing
end // end of [koch0]

fun koch {n:nat} .<n>.
  (cr: !cr, n: int n, x: dbl): void =
  if n > 0 then let
    val () = koch (cr, n-1, x / 3)
//
    val (pf | ()) = cairo_save (cr)
    val () = cairo_translate (cr, x / 3, 0.0)
    val () = cairo_rotate (cr, ~PI3)
    val () = koch (cr, n-1, x / 3)
    val () = cairo_restore (pf | cr)
//
    val (pf | ()) = cairo_save (cr)
    val () = cairo_translate (cr, x / 2, ~x / 3 * sin60)
    val () = cairo_rotate (cr, PI3)
    val () = koch (cr, n-1, x / 3)
    val () = cairo_restore (pf | cr)
//
    val (pf | ()) = cairo_save (cr)
    val () = cairo_translate (cr, 2 * x / 3, 0.0)
    val () = koch (cr, n-1, x / 3)
    val () = cairo_restore (pf | cr)
  in
    // nothing
  end else //
    koch0 (cr, x)
  // end of [if]
// end of [koch]

(* ****** ****** *)

implement main () = () where {
  val surface =
    cairo_image_surface_create (CAIRO_FORMAT_ARGB32, 300, 300)
  val cr = cairo_create (surface)
  val x0 = 50.0 and y0 = 150.0
  val () = cairo_translate (cr, x0, y0)
  val () = koch (cr, 3, 200.0)
  val () = cairo_stroke (cr)
//
  val status = cairo_surface_write_to_png (surface, "koch.png")
  val () = cairo_surface_destroy (surface)
  val () = cairo_destroy (cr)
//
  val () = if status = CAIRO_STATUS_SUCCESS then begin
    print "The image is written to the file [koch.png].\n"
  end else begin
    print "exit(ATS): [cairo_surface_write_to_png] failed"; print_newline ()
  end // end of [if]
} // end of [main]

(* ****** ****** *)

(* end of [cairo-test4.dats] *)
