(*
**
** A simple CAIRO example: an illusion of circular motion
** see Kitaoka's page: http://www.ritsumei.ac.jp/~akitaoka/
**
** This is a variant of cairo-test8
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: December, 2009
**
*)

(*
** how to compile:
   atscc -o test9 \
     `pkg-config --cflags --libs cairo` \
     $ATSHOME/contrib/cairo/atsctrb_cairo.o \
     cairo-test9.dats

** how ot test:
   ./test9
   'gthumb' can be used to view the generated image file 'cairo-test9.png'
*)

(* ****** ****** *)

staload "libc/SATS/math.sats"
staload "contrib/cairo/SATS/cairo.sats"

(* ****** ****** *)

#define PI M_PI

(* ****** ****** *)

stadef dbl = double
stadef cr (l:addr) = cairo_ref l

(* ****** ****** *)

fn bw_set {l:addr} (cr: !cr l, bw: int): void =
  if bw > 0 then
    cairo_set_source_rgb (cr, 0.0, 0.0, 0.0)
  else
    cairo_set_source_rgb (cr, 1.0, 1.0, 1.0)
  // end of [if]
// end of [rb_set]

fn rb_set {l:addr} (cr: !cr l, rb: int): void =
  if rb > 0 then
    cairo_set_source_rgb (cr, 1.0, 0.75, 0.0)
  else
    cairo_set_source_rgb (cr, 0.0, 0.0, 1.0)
  // end of [if]
// end of [rb_set]

(* ****** ****** *)

fn draw_ring
  {l:addr} {n:int | n >= 2} (
    cr: !cr l
  , bw: int, rb: int
  , rad1: dbl, rad2: dbl
  , n: int n
  ) : void = let
  val alpha =  (1.0 - rad2/rad1) / 1.5
  val delta = 2 * PI / n
//
  fun loop1 {i:nat | i <= n} .<n-i>.
    (cr: !cr l, angle: double, i: int i, bw: int)
    :<cloref1> void = let
    val x2 = rad2 * cos angle
    and y2 = rad2 * sin angle
//
    val angle_nxt = angle + delta
    val () = cairo_move_to (cr, x2, y2)
    val () = cairo_arc (cr, 0., 0., rad1, angle, angle_nxt)
    val () = cairo_arc_negative (cr, 0., 0., rad2, angle_nxt, angle)
    val () = bw_set (cr, bw)
    val () = cairo_fill (cr)
  in
    if i < n then loop1 (cr, angle_nxt, i+1, 1-bw)
  end // end of [loop1]
  val () = loop1 (cr, 0.0, 1, bw)
  fun loop2 {i:nat | i <= n} .<n-i>.
    (cr: !cr l, angle: double, i: int i, rb: int)
    :<cloref1> void = let
    val radm = (rad1 + rad2) / 2
    val drad = rad1 - rad2
    val xm = radm * cos angle
    and ym = radm * sin angle
    val (pf | ()) = cairo_save (cr)
    val () = cairo_translate (cr, xm, ym)
    val () = cairo_rotate (cr, angle)
    // drawing an oval shape:
    val () = cairo_scale (cr, drad/2, drad/3.6)
    val () = cairo_arc (cr, 0., 0., 1., 0., 2*PI)
    val () = cairo_restore (pf | cr)
    val () = rb_set (cr, rb)
    val () = cairo_fill (cr)
  in
    if i < n then loop2 (cr, angle+delta, i+1, 1-rb)
  end // end of [loop2]
  val () = loop2 (cr, 0.0, 1, rb)
in
  // nothing
end // end of [draw_ring]

(* ****** ****** *)

#define SHRINKAGE 0.80
fun draw_rings
  {l:addr} {n:int | n >= 2} (
    cr: !cr l
  , bw: int, rb: int
  , rad_beg: dbl, rad_end: dbl
  , n: int n
  ) : void =
  if rad_beg <= rad_end then let
    val () = cairo_set_source_rgb (cr, 0.0, 0.0, 0.0)
    val () = cairo_arc (cr, 0.0, 0.0, rad_beg, 0.0, 2*PI)
    val () = cairo_fill (cr)
  in
    // loop exits
  end else let
    val rad_beg_nxt = SHRINKAGE * rad_beg
    val () = draw_ring (cr, bw, rb, rad_beg, rad_beg_nxt, n)
  in
    draw_rings (cr, 1-bw, 1-rb, rad_beg_nxt, rad_end, n)
  end // end of [if]
// end of [draw_rings]

(* ****** ****** *)

staload "contrib/X11/SATS/X.sats"
staload "contrib/X11/SATS/Xlib.sats"

(* ****** ****** *)

symintr uint
overload uint with uint_of_int // no-op casting

(* ****** ****** *)

implement main () = () where {
  val wd = 512 and ht = 512
  val margin = 10
(*
  val surface = cairo_image_surface_create
    (CAIRO_FORMAT_ARGB32, wd+margin, ht+margin)
*)
//
  val [l_dpy:addr] dpy = XOpenDisplay (stropt_none)
  val () = assert_errmsg (Display_ptr_isnot_null dpy, #LOCATION)
//
  val screen_num = XDefaultScreen (dpy)
//
  val mywin = XCreateSimpleWindow (
    dpy, parent, x, y, width, height, border_width, w_pix, b_pix
  ) where {
    val parent = XRootWindow (dpy, screen_num)
    val x = 0 and y = 0
    val width = (uint)wd and height = (uint)ht
    val border_width = (uint)4
    val w_pix = XWhitePixel (dpy, screen_num)
    val b_pix = XBlackPixel (dpy, screen_num)
  }
//
  val (pf_minus | visual) = XDefaultVisual (dpy, screen_num)
  val surface = cairo_xlib_surface_create (dpy, (Drawable)mywin, visual, wd, ht)
  prval () = minus_addback (pf_minus, visual | dpy)
//
  val () = XMapWindow(dpy, mywin)
//
  val cr = cairo_create (surface)
  val wd = double_of wd and ht = double_of ht
  val margin = double_of margin
//
  val () = cairo_translate (cr, margin/2, margin/2)
  var i : int = 0 and j : int = 0
  val () = (
    for (i := 0; i < 3; i := i + 1) (
    for (j := 0; j < 3; j := j + 1) let
      val (pf | ()) = cairo_save (cr)
      val () = cairo_translate (cr, i*wd/2, j*ht/2)
      val () = draw_rings (cr, 0, 0, 128.0, 4.0, 40)
      val () = cairo_restore (pf | cr)
    in
      // nothing
    end // end of [for]
    ) // end of [for]
  ) // end of [val]
//
  val () = (
    for (i := 0; i < 2; i := i + 1) (
    for (j := 0; j < 2; j := j + 1) let
      val (pf | ()) = cairo_save (cr)
      val () =
        cairo_translate (cr, i*wd/2+wd/4, j*ht/2+ht/4)
      // end of [val]
      val () = draw_rings (cr, i, 0, 128.0, 4.0, 40)
      val () = cairo_restore (pf | cr)
    in
      // nothing
    end // end of [for]
    ) // end of [for]
  ) // end of [val]
//
  val () = XSelectInput (dpy, mywin, flag) where {
    val flag = ExposureMask lor KeyPressMask lor ButtonPressMask lor StructureNotifyMask
  } // end of [val]
//
  var report: XEvent? // uninitialized
  val () = while (true) let
    val () = XNextEvent (dpy, report)
    val type = report.type
  in
    case+ 0 of
    | _ when (type = KeyPress) => (break)
    | _ => () // ignored
  end // end of [val]
//
(*
  val status = cairo_surface_write_to_png (surface, "cairo-test9.png")
*)
  val () = cairo_surface_destroy (surface)
  val () = cairo_destroy (cr)
  val () = XCloseDisplay (dpy)
//
(*
  val () = if status = CAIRO_STATUS_SUCCESS then begin
    print "The image is written to the file [cairo-test9.png].\n"
  end else begin
    print "exit(ATS): [cairo_surface_write_to_png] failed"; print_newline ()
  end // end of [if]
*)
} // end of [main]

(* ****** ****** *)

(* end of [cairo-test9.dats] *)
