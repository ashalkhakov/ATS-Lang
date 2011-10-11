(*
**
** A simple OpenGL example
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: Summer, 2008 // originally done
** Time: October, 2011 // adapted to gtkglext-1.0
**
*)

(* ****** ****** *)

staload UN = "prelude/SATS/unsafe.sats"
staload _(*anon*) = "prelude/DATS/reference.dats"

(* ****** ****** *)

staload "libc/SATS/math.sats"
staload "libc/SATS/time.sats"
staload "libc/SATS/random.sats"
macdef PI = M_PI
macdef _2PI = 2 * M_PI

(* ****** ****** *)

staload "contrib/GL/SATS/gl.sats"
staload "contrib/GL/SATS/glu.sats"
staload "contrib/cairo/SATS/cairo.sats"

(* ****** ****** *)

staload "contrib/glib/SATS/glib.sats"
staload "contrib/glib/SATS/glib-object.sats"
staload "contrib/GTK/SATS/gdk.sats"
staload "contrib/GTK/SATS/gtk.sats"
staload "contrib/GTK/SATS/gtkclassdec.sats"

(* ****** ****** *)

staload "contrib/gtkglext/SATS/gdk.sats"
staload "contrib/gtkglext/SATS/gtk.sats"

(* ****** ****** *)

staload
"contrib/atspslide/SATS/atspslide.sats"
dynload "contrib/atspslide/dynloadall.dats"

(* ****** ****** *)

local

val theSlideCount_ref = ref_make_elt<int> (0)

in // in of [local]

fun theSlideCount_get (): int = !theSlideCount_ref

fun theSlideCount_inc (): void = let
  val n = !theSlideCount_ref in !theSlideCount_ref := n+1
end // end of [theSlideCount_inc]

fun theSlideCount_dec (): void = let
  val n = !theSlideCount_ref in !theSlideCount_ref := n-1
end // end of [theSlideCount_dec]

end // end of [local]

(* ****** ****** *)

#define theSlideWidth 16
#define theSlideHeight 9

val theSlideAspect = 1.0 * theSlideHeight / theSlideWidth

(* ****** ****** *)

%{^
GtkWidget *theDrawingArea = NULL;
ats_ptr_type
theDrawingArea_get () {
  g_object_ref (G_OBJECT(theDrawingArea)); return theDrawingArea ;
} // end of [theDrawingArea_get]
ats_void_type
theDrawingArea_initset (ats_ptr_type x) {
  g_object_ref(G_OBJECT(x)) ;
  if (theDrawingArea) g_object_unref (G_OBJECT(theDrawingArea));
  theDrawingArea = x ;
  return ;
} // end of [theDrawingArea_initset]
ats_void_type
theDrawingArea_set_gl_capability
  (ats_ptr_type gl_config) {
  gboolean gl_capability ;
  gl_capability = gtk_widget_set_gl_capability
    (theDrawingArea, gl_config, NULL, TRUE, GDK_GL_RGBA_TYPE) ;
  // end of [gl_capability]
  if (!gl_capability) g_assert_not_reached () ;
  return ;
} // end of [theDrawingArea_set_gl_capability]
%} // end of [%{^] 
//
extern
fun theDrawingArea_get (): GtkDrawingArea_ref1 = "theDrawingArea_get"
extern
fun theDrawingArea_initset (x: !GtkDrawingArea_ref1): void = "theDrawingArea_initset"
//
extern
fun theDrawingArea_set_gl_capability {l:addr}
  (glconfig: !GdkGLConfig_ref (l)): void = "theDrawingArea_set_gl_capability"
// end of [theDrawingArea_set_gl_capability]

(* ****** ****** *)

extern
fun initialize (): void = "initialize"
implement initialize () = let
//
  val () = glClearColor (0.75, 0.75, 0.75, 0.0)
  val () = glMatrixMode (GL_PROJECTION)
  val () = glLoadIdentity ()
//
(*
//
// HX: [r] should be greater than sqrt(2)/2 for [glOrtho]
//
  val r = 1.41422 / 2
  val w = r and h = r
  val () = glOrtho (~1.0*w, 1.0*w, ~1.0*h, 1.0*h, 1.0, 10.0)
*)
  val r = 1.0 / 2 // HX: for [glFrustum]
  val w = r and h = r
  val znear = 2.0; val zfar = znear + 2.0
  val () = glFrustum (~1.0*w, 1.0*w, ~1.0*h, 1.0*h, znear, zfar)
  val () = glMatrixMode (GL_MODELVIEW)
  val () = glLoadIdentity ()
  val zdelta = 1.0
  val () = gluLookAt (0.0, 0.0, znear+zdelta, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0)
//
in
  // empty
end // end of [initialize]

(* ****** ****** *)

extern
fun frealize {l:agz} (
  darea: !GtkDrawingArea_ref l, data: gpointer
) : void = "frealize"
implement
frealize (darea, data) = let
  val (fpf1 | glcontext) = gtk_widget_get_gl_context (darea)
  val (fpf2 | gldrawable) = gtk_widget_get_gl_drawable (darea)
  val (pfbeg | can_begin) = gdk_gl_drawable_gl_begin (gldrawable, glcontext)
  prval () = minus_addback (fpf1, glcontext | darea)
in
  if (can_begin) then let
    val () = initialize ()
    val () = gdk_gl_drawable_gl_end (pfbeg | gldrawable)
    prval () = minus_addback (fpf2, gldrawable | darea)
  in
    // nothing
  end else let
    prval () = gdkgl_begin_false_elim (pfbeg)
    prval () = minus_addback (fpf2, gldrawable | darea)
  in
    // nothing
  end // end of [if]
end // end of [frealize]

(* ****** ****** *)

extern
fun funrealize {l:agz} (
  darea: !GtkDrawingArea_ref l, data: gpointer
) : void = "funrealize"
implement
funrealize (darea, data) = let
(*
  val () = println! ("funrealize: enter")
*)
in 
  // nothing
end // end of [funrealize]

(* ****** ****** *)

extern
fun fconfigure {l:agz} (
  darea: !GtkDrawingArea_ref l, event: &GdkEvent
) : gboolean = "fconfigure"
implement
fconfigure (darea, event) = let
  val (fpf1 | glcontext) = gtk_widget_get_gl_context (darea)
  val (fpf2 | gldrawable) = gtk_widget_get_gl_drawable (darea)
  val (pfbeg | can_begin) = gdk_gl_drawable_gl_begin (gldrawable, glcontext)
  prval () = minus_addback (fpf1, glcontext | darea)
in
  if (can_begin) then let
    val (pf, fpf | p) = gtk_widget_getref_allocation (darea)
    val () = gtk_widget_queue_draw_area (darea, (gint)0, (gint)0, p->width, p->height)
//
    val daw = (int_of)p->width
    val dah = (int_of)p->height
    val uw = theSlideWidth
    val uh = theSlideHeight
    val w = 1.0 * daw / uw
    val h = 1.0 * dah / uh
    val wh = min (w, h)
    val vpw = int_of(wh * uw)
    val vph = int_of(wh * uh)
    val () = glViewport ((daw-vpw)/2, (dah-vph)/2, vpw, vph)
    prval () = minus_addback (fpf, pf | darea)
    val () = initialize ()
//
    val () = gdk_gl_drawable_gl_end (pfbeg | gldrawable)
    prval () = minus_addback (fpf2, gldrawable | darea)
  in
    GTRUE
  end else let
    prval () = gdkgl_begin_false_elim (pfbeg)
    prval () = minus_addback (fpf2, gldrawable | darea)
  in
    GFALSE
  end // end of [if]
end (* end of [fconfigure] *)

(* ****** ****** *)

extern
fun cairodraw_slide
  {l:agz} (cr: !cairo_ref (l), count: int): void
// end of [cairodraw_slide]

fun
cairodraw_slide_relative
  {l:agz} (
  cr: !cairo_ref (l), count: int
) : void =
  cairodraw_slide (cr, theSlideCount_get () + count)
// end of [cairodraw_slide_relative]

fun
cairodraw_slide_count
  {l:agz} (
  cr: !cairo_ref (l), count: int
) : void = let
  val (pf | ()) = cairo_save (cr)
  val () = cairo_rectangle (cr, 0.0, 0.0, 1.0, 1.0)
  val () = cairo_set_source_rgb (cr, 0.5, 0.5, 0.5) // midgray color
  val () = cairo_fill (cr)
  val () = cairodraw_circnum (cr, count)
  val () = cairo_restore (pf | cr)
in
  // nothing
end // end of [cairodraw_slide_count]

fun
cairodraw_slide_pngfile
  {l:agz} (
  cr: !cairo_ref (l), path: string
) : void = let
  val wsf = 1.0
  val hsf = 1.0
  val img = cairo_image_surface_create_from_png (path)
  val wimg = cairo_image_surface_get_width (img)
  and himg = cairo_image_surface_get_height (img)
  val (pf | ()) = cairo_save (cr)
  val () = cairo_scale (cr, 1.0*wsf/wimg, 1.0*hsf/himg)
  val () = cairo_set_source_surface (cr, img, 0.0, 0.0)
  val () = cairo_paint (cr)
  val () = cairo_restore (pf | cr)
  val () = cairo_surface_destroy (img)
in
  // nothing
end // end of [cairodraw_slide_count]

local

#define PREFIX "review"

fun
slidename_get_by_count
  (count: int): strptr1 =
  sprintf ("data/%s_%i.png", @(PREFIX, count))
// end of [slidename_get_by_count]

in // in of [local]

implement
cairodraw_slide
  (cr, count) = let
  val [l:addr] path = slidename_get_by_count (count)
  val path1 = $UN.castvwtp1 {string} {strptr(l)} (path)
  val isexi = test_file_exists (path1)
in
  if isexi then let
    val () = cairodraw_slide_pngfile (cr, path1)
  in
    strptr_free (path)
  end else let
    val () = cairodraw_slide_count (cr, count)
  in
    strptr_free (path)
  end // end of [if]
end // end of [cairodraw_slide]

end // end of [local]

(* ****** ****** *)

#define
ACTpresent 0 // default
#define ACTrotate 1

local
//
val theActState_ref = ref<int> (ACTpresent)
//
in
//
fun theActState_get () = !theActState_ref
fun theActState_set (act: int): void = !theActState_ref := act
//
end // end of [val]

val theDelta = 5.0
val theAlpha_ref = ref<double> (0.0)
val theRotateknd_ref = ref_make_elt<int> (0)

(* ****** ****** *)

fun fexpose_present
  (vpw: int, vph: int): void = let
  val surface =
    cairo_image_surface_create (CAIRO_FORMAT_ARGB32, vpw, vph)
  val vpw = (double_of)vpw
  and vph = (double_of)vph
//
  val [l:addr] cr = cairo_create (surface)
//
  val (pf_save | ()) = cairo_save (cr)
  val () = cairo_scale (cr, vpw, vph)
  val () = cairodraw_slide_relative (cr, 0) // current one
  val () = cairodraw_clock01 (cr) // HX: a translucent clock layover
  val () = cairo_restore (pf_save | cr)
  val gltext = glTexture_make_cairo_ref (GL_BGRA_format, cr)
//
  val () = cairo_destroy (cr)
  val () = cairo_surface_destroy (surface)
//
  val () = glClear (GL_COLOR_BUFFER_BIT)
  val () = glColor3d (0.0, 0.0, 0.0) // black color
//
  val (pfmat | ()) = glPushMatrix ()
  val () = glTranslated (~0.5, ~0.5, 0.5)
  val () = glTexture_mapout_rect (gltext, 1.0, 1.0, 1(*down*))
  val () = glPopMatrix (pfmat | (*none*))
//
  val () = glDeleteTexture (gltext)
//
in
  // nothing
end // end of [fexpose_present]

fun fexpose_rotate
  (vpw: int, vph: int): void = let
  val surface =
    cairo_image_surface_create (CAIRO_FORMAT_ARGB32, vpw, vph)
  val vpw = (double_of)vpw
  and vph = (double_of)vph
//
  val [l:addr] cr = cairo_create (surface)
//
  val (pf_save | ()) = cairo_save (cr)
  val () = cairo_scale (cr, vpw, vph)
  val () = cairodraw_slide_relative (cr, 0) // current one
  val () = cairodraw_clock01 (cr) // HX: a translucent clock layover
  val () = cairo_restore (pf_save | cr)
  val gltext1 = glTexture_make_cairo_ref (GL_BGRA_format, cr)
//
  val (pf_save | ()) = cairo_save (cr)
  val () = cairo_scale (cr, vpw, vph)
  val () = cairodraw_slide_relative (cr, 1) // next one
  val () = cairodraw_clock01 (cr) // HX: a translucent clock layover
  val () = cairo_restore (pf_save | cr)
  val gltext2 = glTexture_make_cairo_ref (GL_BGRA_format, cr)
//
  val () = cairo_destroy (cr)
  val () = cairo_surface_destroy (surface)
//
  val () = glClear (GL_COLOR_BUFFER_BIT)
  val () = glColor3d (0.0, 0.0, 0.0) // black color
//
  val knd12 = !theRotateknd_ref; val knd16 = 1 - knd12
//
  val (pfmat | ()) = glPushMatrix ()
  val () = () where {
    val alpha = !theAlpha_ref
    val () = if knd12 > 0 then glRotated (~alpha, 0.0, 1.0, 0.0)
    val () = if knd16 > 0 then glRotated (~alpha, 1.0, 0.0, 0.0)
  } // end of [val]
  val () = glTranslated (~0.5, ~0.5, 0.5)
//
  val () = glEnable (GL_CULL_FACE)
  val () = glCullFace (GL_BACK) // HX: prevent transparency!
  val () = if knd12 > 0 then
    glTexture_mapout_rect12 (gltext1, gltext2, 1.0, 1.0, 1(*down*))
  val () = if knd16 > 0 then
    glTexture_mapout_rect16 (gltext1, gltext2, 1.0, 1.0, 1(*down*))
  val () = glDisable (GL_CULL_FACE)
//
  val () = glPopMatrix (pfmat | (*none*))
//
  val () = glDeleteTexture (gltext1)
  val () = glDeleteTexture (gltext2)
//
in
  // nothing
end // end of [fexpose_rotate]

(* ****** ****** *)

extern
fun fexpose {l:agz} (
  darea: !GtkDrawingArea_ref l, event: &GdkEvent
) : gboolean = "fexpose"

implement
fexpose (darea, event) = let
(*
  val () = println! ("fexpose: enter")
*)
  val (fpf_glcontext | glcontext) = gtk_widget_get_gl_context (darea)
  val (fpf_gldrawable | gldrawable) = gtk_widget_get_gl_drawable (darea)
  val is_double_buffered = gdk_gl_drawable_is_double_buffered (gldrawable)
  val (pfbeg | can_begin) = gdk_gl_drawable_gl_begin (gldrawable, glcontext)
  prval () = minus_addback (fpf_glcontext, glcontext | darea)
in
  if (can_begin) then let
//
    var alloc: GtkAllocation
    val () = gtk_widget_get_allocation (darea, alloc)
//
    val daw = (int_of)alloc.width
    and dah = (int_of)alloc.height
    val uw = theSlideWidth
    val uh = theSlideHeight
    val w = 1.0 * daw / uw
    val h = 1.0 * dah / uh
    val wh = min (w, h)
    val vpw = int_of(wh * uw)
    val vph = int_of(wh * uh)
//
    val theActState = theActState_get ()
    val () = (
      case+ theActState of
      | 0 => fexpose_present (vpw, vph)
      | 1 => fexpose_rotate (vpw, vph)
      | _ => () // HX: should it do something?
    ) : void // end of [val]
//
    val () = if ((bool_of)is_double_buffered) then
      gdk_gl_drawable_swap_buffers (gldrawable) else glFlush ()
    // end of [if]
    val () = gdk_gl_drawable_gl_end (pfbeg | gldrawable)
    prval () = minus_addback (fpf_gldrawable, gldrawable | darea)
  in
    GTRUE
  end else let
    prval () = gdkgl_begin_false_elim (pfbeg)
    prval () = minus_addback (fpf_gldrawable, gldrawable | darea)
  in
    GFALSE
  end // end of [if]
end // end of [fexpose]

(* ****** ****** *)

%{$
//
guint timeout_id = 0 ;
//
#define TIMEOUT_INTERVAL 100
//
void timeout_add () {
  if (timeout_id == 0) {
    timeout_id = g_timeout_add (TIMEOUT_INTERVAL, (GSourceFunc)ftimeout, NULL);
  } ; return ;
} // end of [timeout_add]
//
void timeout_remove () {
  if (timeout_id != 0) {
    g_source_remove (timeout_id); timeout_id = 0;
  } ; return ;
} // end of [timeout_remove]
//
%} // end of [%{$]

extern fun timeout_add (): void = "timeout_add"
extern fun timeout_remove (): void = "timeout_remove"

(* ****** ****** *)

val rotate_ref = ref_make_elt<int> (0)

(* ****** ****** *)

extern
fun ftimeout (): gboolean = "ftimeout"
implement
ftimeout () = let
//
  val act = theActState_get ()
  val () = if
    (act = ACTrotate) then {
    val alpha = !theAlpha_ref + theDelta
//
    val isRot = alpha <= 90.0
    val () = if isRot then let
      // more rotation is needed
    in
      !theAlpha_ref := alpha
    end // end of [val]
    val () = if ~(isRot) then let
      // rotation is completed by now
      val () = !rotate_ref := 0
      val () = !theRotateknd_ref := randint (2)
      val () = theSlideCount_inc ()
      val () = theActState_set (ACTpresent)
(*
      val () = timeout_remove ()
*)
    in
      !theAlpha_ref := 0.0
    end // end of [val]
  } // end of [if]
  val darea = theDrawingArea_get ()
  val (fpf_win | win) = gtk_widget_get_window (darea)
  var alloc: GtkAllocation
  val () = gtk_widget_get_allocation (darea, alloc)
  prval pf = GtkAllocation2GdkRectangle (view@ (alloc))
  val () = gdk_window_invalidate_rect (win, alloc, GFALSE)
  prval () = view@ (alloc) := GdkRectangle2GtkAllocation (pf)
  val () = gdk_window_process_updates (win, GFALSE)
  prval () = minus_addback (fpf_win, win | darea)
  val () = g_object_unref (darea)
in
  GTRUE
end // end of [ftimeout]

(* ****** ****** *)

extern
fun fprev (): void
implement fprev () = let
//
  val () = theSlideCount_dec ()
//
  var alloc: GtkAllocation
  val darea = theDrawingArea_get ()
  val () = gtk_widget_get_allocation (darea, alloc)
  prval pf = GtkAllocation2GdkRectangle (view@ (alloc))
  val (fpf_win | win) = gtk_widget_get_window (darea)
  val () = gdk_window_invalidate_rect (win, alloc, GFALSE)
  prval () = view@ (alloc) := GdkRectangle2GtkAllocation (pf)
  val () = gdk_window_process_updates (win, GFALSE)
  prval () = minus_addback (fpf_win, win | darea)
  val () = g_object_unref (darea)
in
  // nothing
end // end of [fprev]

(* ****** ****** *)

extern
fun fnext (): void
implement
fnext () = let
//
  val x = !rotate_ref
  val () = !rotate_ref := 1 - x
//
in
//
if (x = 0) then let
  val () = theActState_set (ACTrotate)
  val () = timeout_add () in (*nothing*)
end else let
  val () = timeout_remove ()
  var alloc: GtkAllocation
  val darea = theDrawingArea_get ()
  val () = gtk_widget_get_allocation (darea, alloc)
  prval pf = GtkAllocation2GdkRectangle (view@ (alloc))
  val (fpf_win | win) = gtk_widget_get_window (darea)
  val () = gdk_window_invalidate_rect (win, alloc, GFALSE)
  prval () = view@ (alloc) := GdkRectangle2GtkAllocation (pf)
  val () = gdk_window_process_updates (win, GFALSE)
  prval () = minus_addback (fpf_win, win | darea)
  val () = g_object_unref (darea)
//
in
  // nothing
end // end of [if]
//
end // end of [fnext]

(* ****** ****** *)

fun
map_event (): gboolean = let
(*
  val () = println! ("map_event: enter")
*)
  val () = if (!rotate_ref = 1) then timeout_add () in GTRUE
end // end of [map_event]

fun
unmap_event (): gboolean = let
(*
  val () = println! ("unmap_event: enter")
*)
  val () = if (!rotate_ref = 1) then timeout_remove () in GTRUE
end // end of [unmap_event]

fun
visibility_notify_event (
  widget: gpointer, event: &GdkEventVisibility
) : gboolean = let
(*
  val () = println! ("visibility_notify_event: enter")
*)
  val () = if (!rotate_ref = 1) then
    if (event.state = GDK_VISIBILITY_FULLY_OBSCURED) then timeout_remove () else timeout_add ()
  // end of [if]
in
  GTRUE
end // end of [visibility_notify_event]

(* ****** ****** *)

macdef gs = gstring_of_string
overload gint with gint_of_GtkResponseType

(* ****** ****** *)

fun
cb_btn_close_clicked {l:agz}
  (btn: ptr, win: !GtkWindow_ref (l)): gboolean = GTRUE where {
(*
  val () = (print (#LOCATION + ": cb_btn_close_clicked"); print_newline ())
*)
  val flags = GTK_DIALOG_DESTROY_WITH_PARENT
  val _type = GTK_MESSAGE_QUESTION
  val buttons = GTK_BUTTONS_YES_NO
//
  val (fpf_x | x) = (gs)"Quit?"
  val dialog = gtk_message_dialog_new0 (flags, _type, buttons, x)
  prval () = fpf_x (x)
  val (fpf_x | x) = (gs)"Confirmation"
  val () = gtk_window_set_title (dialog, x)
  prval () = fpf_x (x)
//
  val () = gtk_window_set_transient_for (dialog, win(*parent*))
//
  val response = gtk_dialog_run (dialog)
  val () = gtk_widget_destroy (dialog)
//
  val () = case+ 0 of
    | _ when response = (gint)GTK_RESPONSE_YES => gtk_main_quit () // many things to do here!
    | _ => () // quit is not confirmed
  // end of [val]
} // end of [cb_file_quit_activate]

(* ****** ****** *)

extern
fun main1 (): void = "main1"
implement
main1 () = () where {
//
val () = srand48_with_time ()
//
val glconfig = gdk_gl_config_new_by_mode (
  GDK_GL_MODE_RGB lor GDK_GL_MODE_ALPHA lor GDK_GL_MODE_DEPTH lor GDK_GL_MODE_DOUBLE
) // end of [glconfig]
//
val window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
val () = gtk_window_set_default_size (window, (gint)400, (gint)400)
val (fpf_x | x) = (gs)"gtkglSlidePresent"
val () = gtk_window_set_title (window, x)
prval () = fpf_x (x)
val (fpf_window | window_) = g_object_vref (window)
val _sid = g_signal_connect0
(window_, (gsignal)"delete_event", G_CALLBACK (gtk_widget_destroy), (gpointer)null)
val _sid = g_signal_connect1
(window, (gsignal)"destroy", G_CALLBACK (gtk_main_quit), (gpointer)null)
//
val vbox0 = gtk_vbox_new (GFALSE(*homo*), (gint)10(*spacing*))
val () = gtk_container_add (window, vbox0)
val darea = gtk_drawing_area_new ()
//
val () = theDrawingArea_initset (darea)
val () = theDrawingArea_set_gl_capability (glconfig)
val () = g_object_unref (glconfig)
//
val () = gtk_box_pack_start (vbox0, darea, GTRUE, GTRUE, (guint)0)
val _sid = g_signal_connect_after
  (darea, (gsignal)"realize", G_CALLBACK (frealize), (gpointer)null)
val _sid = g_signal_connect
  (darea, (gsignal)"configure_event", G_CALLBACK (fconfigure), (gpointer)null)
val _sid = g_signal_connect
  (darea, (gsignal)"expose_event", G_CALLBACK (fexpose), (gpointer)null)
val _sid = g_signal_connect
  (darea, (gsignal)"unrealize", G_CALLBACK (funrealize), (gpointer)null)

val _sig = g_signal_connect
  (darea, (gsignal)"map_event", G_CALLBACK (map_event), (gpointer)null)
val _sig = g_signal_connect
  (darea, (gsignal)"unmap_event", G_CALLBACK (unmap_event), (gpointer)null)
val _sig = g_signal_connect
  (darea, (gsignal)"visibility_notify_event", G_CALLBACK (visibility_notify_event), (gpointer)null)
//
val () = gtk_widget_show_unref (darea)
//
val hsep = gtk_hseparator_new ()
val () = gtk_box_pack_start (vbox0, hsep, GFALSE, GTRUE, (guint)0)
val () = gtk_widget_show_unref (hsep)
//
val hbox1 = gtk_hbox_new (GFALSE(*homo*), (gint)5(*spacing*))
val () = gtk_box_pack_start (vbox0, hbox1, GFALSE, GTRUE, (guint)10)
//
val (fpf_x | x) = (gs)"_Close"
val btn_close = gtk_button_new_with_mnemonic (x)
prval () = fpf_x (x)
val _sid = g_signal_connect
  (btn_close, (gsignal)"clicked", G_CALLBACK(cb_btn_close_clicked), (gpointer_vt)window)
// end of [val]
val () = gtk_box_pack_end (hbox1, btn_close, GFALSE, GFALSE, (guint)10)
val () = gtk_widget_show_unref (btn_close)
//
val (fpf_x | x) = (gs)"_Prev"
val btn_prev = gtk_button_new_with_mnemonic (x)
prval () = fpf_x (x)
val _sid = g_signal_connect
  (btn_prev, (gsignal)"clicked", G_CALLBACK(fprev), (gpointer)null)
// end of [val]
val () = gtk_box_pack_start (hbox1, btn_prev, GFALSE, GFALSE, (guint)4)
val () = gtk_widget_show_unref (btn_prev)
//
val (fpf_x | x) = (gs)"_Next"
val btn_next = gtk_button_new_with_mnemonic (x)
prval () = fpf_x (x)
val _sid = g_signal_connect
  (btn_next, (gsignal)"clicked", G_CALLBACK(fnext), (gpointer)null)
val () = gtk_box_pack_start (hbox1, btn_next, GFALSE, GFALSE, (guint)4)
val () = gtk_widget_show_unref (btn_next)
//
val () = gtk_widget_show_unref (hbox1)
//
val () = gtk_widget_show_unref (vbox0)
//
val () = gtk_widget_show (window)
prval () = fpf_window (window)
//
val () = if !rotate_ref = 1 then timeout_add () // start the clock
//
} // end of [main1]

(* ****** ****** *)

%{^
extern
ats_void_type mainats (ats_int_type argc, ats_ptr_type argv) ;
%} // end of [%{^]
implement main_dummy () = ()

(* ****** ****** *)

%{$

ats_void_type
mainats (
  ats_int_type argc, ats_ptr_type argv
) {
  int gtkglcheck ;
  GdkGLConfig *glconfig ;
//
  gtk_init ((int*)&argc, (char***)&argv) ;
//
  gtkglcheck =
    gtk_gl_init_check ((int*)&argc, (char***)&argv) ;
  // end of [gtkglcheck]
  if (!gtkglcheck) {
    fprintf (stderr, "[gtk_gl_init] failded!\n"); exit (1) ;
  } // end of [if]
//
  main1 () ;
//
  gtk_main () ;
//
  return ; /* deadcode */
} /* end of [mainats] */

%} // end of[%{$]

(* ****** ****** *)

(* end of [gtkglCS112Intro.dats] *)
