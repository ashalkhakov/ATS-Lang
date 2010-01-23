(*
**
** A simple Xlib example
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: January, 2010
**
*)

(* ****** ****** *)

staload "contrib/Xlib/SATS/Xlib.sats"

(* ****** ****** *)

implement main () = () where {
  val dpy = XOpenDisplay ("localhost:10.0")
  val () = assert_errmsg (Display_ptr_isnot_null dpy, #LOCATION)
  val bpxl = XBlackPixel (dpy, 0)
  val () = (print "bpxl = "; print bpxl; print_newline ())
  val wpxl = XWhitePixel (dpy, 0)
  val () = (print "wpxl = "; print wpxl; print_newline ())
  val nconn = XConnectionNumber (dpy)
  val () = (print "nconn = "; print nconn; print_newline ())
  val colormap = XDefaultColormap (dpy, 0)
  val () = let
    val xid = __cast (colormap) where {
      extern castfn __cast (x: Colormap):<> ulint
    }
  in
    print "colormap = "; print xid; print_newline ()
  end // end of [val]
  val depth = XDefaultDepth (dpy, 0)
  val () = (print "depth = "; print depth; print_newline ())
//
  var asz: int = 0
  val (pf_free, pf_arr | p_arr) = XListDepths (dpy, 0, asz)
  val () = (print "asz = "; print asz; print_newline ())
  val () = (print "p_arr = "; print p_arr; print_newline ())
  val () = XFree (pf_free, pf_arr | p_arr)
//
  val gc = XDefaultGC (dpy, 0)
//
  val window = XDefaultRootWindow (dpy)
  val () = let
    val xid = __cast (window) where {
      extern castfn __cast (x: Window):<> ulint
    }
  in
    print "window = "; print xid; print_newline ()
  end // end of [val]
//
  val nscr = XDefaultScreen (dpy)
  val () = (print "nscr = "; print nscr; print_newline ())
//
  val ncell = XDisplayCells (dpy, 0)
  val () = (print "ncell = "; print ncell; print_newline ())
//
  val nplane = XDisplayPlanes (dpy, 0)
  val () = (print "nplane = "; print nplane; print_newline ())
//
  val name = XDisplayString (dpy)
  val () = (print "name = "; print name; print_newline ())
//
  val last = XLastKnownRequestProcessed (dpy)
  val () = (print "last = "; print last; print_newline ())
  val next = XNextRequest (dpy)
  val () = (print "next = "; print next; print_newline ())
//
  val nque = XQLength (dpy)
  val () = (print "nque = "; print nque; print_newline ())
//
  val rtwindow = XRootWindow (dpy, 0)
  val () = let
    val xid = __cast (rtwindow) where {
      extern castfn __cast (x: Window):<> ulint
    }
  in
    print "rtwindow = "; print xid; print_newline ()
  end // end of [val]
//
  val scrcnt = XScreenCount (dpy)
  val () = (print "scrcnt = "; print scrcnt; print_newline ())
//
  val ht = XDisplayHeight (dpy, 0)
  val () = (print "ht = "; print ht; print_newline ())
  val ht_mm = XDisplayHeightMM (dpy, 0)
  val () = (print "ht_mm = "; print ht_mm; print_newline ())
  val wd = XDisplayWidth (dpy, 0)
  val () = (print "wd = "; print wd; print_newline ())
  val wd_mm = XDisplayWidthMM (dpy, 0)
  val () = (print "wd_mm = "; print wd_mm; print_newline ())
//
  val () = XCloseDisplay (dpy)
} // end of [main]

(* ****** ****** *)

(* end of [Xlib-test01.dats] *)
