//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: April, 2010
//

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/list.dats"
staload _(*anon*) = "prelude/DATS/list_vt.dats"
staload _(*anon*) = "prelude/DATS/reference.dats"

(* ****** ****** *)

staload MATH = "libc/SATS/math.sats"
staload RAND = "libc/SATS/random.sats"

(* ****** ****** *)

staload "contrib/glib/SATS/glib.sats"
staload "contrib/glib/SATS/glib-object.sats"

(* ****** ****** *)

staload "contrib/GTK/SATS/gdk.sats"
staload "contrib/GTK/SATS/gtk.sats"

(* ****** ****** *)

datatype exp =
  | Num of double
  | Add of (exp, exp)
  | Sub of (exp, exp)
  | Mul of (exp, exp)
  | Div of (exp, exp)
// end of [exp]

(* ****** ****** *)

extern
fun eq_exp_exp (x1: exp, x2: exp):<> bool
overload = with eq_exp_exp

implement eq_exp_exp (x1, x2) =
  case+ (x1, x2) of
  | (Num d1, Num d2) =>  (d1 = d2)
  | (Add (x11, x12), Add (x21, x22)) =>
      (x11 = x21 andalso x12 = x22) orelse (x11 = x22 andalso x12 = x21)
  | (Sub (x11, x12), Sub (x21, x22)) => (x11 = x21) andalso (x12 = x22)
  | (Mul (x11, x12), Mul (x21, x22)) =>
      (x11 = x21 andalso x12 = x22) orelse (x11 = x22 andalso x12 = x21)
  | (Div (x11, x12), Div (x21, x22)) => (x11 = x21) andalso (x12 = x22)
  | (_, _) => false
// end of [eq_exp_exp]

fun explst_remdup
  (xs: List exp): List exp = case+ xs of
  | list_cons (x, xs) => let
      var !p_clo = @lam (pf: !unit_v | x1: exp): bool =<clo> ~(x = x1)
      prval pf = unit_v ()
      val xs = list_filter_clo<exp> {unit_v} (pf | xs, !p_clo)
      prval unit_v () = pf
      val xs = list_of_list_vt (xs)
    in
      list_cons (x, explst_remdup xs)
    end // end of [list_cons]
  | list_nil () => list_nil ()
// end of [explst_remdup]

(* ****** ****** *)

fun eval_exp
  (e: exp): double = case+ e of
  | Num (a) => a
  | Add (e1, e2) => eval_exp e1 + eval_exp e2
  | Sub (e1, e2) => eval_exp e1 - eval_exp e2  
  | Mul (e1, e2) => eval_exp e1 * eval_exp e2
  | Div (e1, e2) => eval_exp e1 / eval_exp e2
// end of [eval_exp]

fun priority_mac (e: exp): Nat = (case+ e of
  | Num _ => 0 | Add _ => 2 | Sub _ => 2 | Mul _ => 1 | Div _ => 1
) : Nat // end of [priority]

(* ****** ****** *)

fun g_print_exp {l:agz}
  (gs: !GString_ptr l, e: exp): void = begin
  case+ e of
  | Num r => g_string_append_printf (gs, "%.0f", @(r))
  | Add (e1, e2) => let
      val p1 = priority_mac e1 and p2 = priority_mac e2
      val () = g_print_exp_ (gs, 2, p1, e1)
      val _ = g_string_append_c (gs, (gchar)'+')
      val () = g_print_exp_ (gs, 2, p2, e2)
    in
      // nothing
    end // end of [Add]
  | Sub (e1, e2) => let
      val p1 = priority_mac e1 and p2 = priority_mac e2
      val () = g_print_exp_ (gs, 2, p1, e1)
      val _ = g_string_append_c (gs, (gchar)'-')
      val () = g_print_exp_ (gs, 2, p2, e2)
    in
      // nothing
    end // end of [Sub]
  | Mul (e1, e2) => let
      val p1 = priority_mac e1 and p2 = priority_mac e2
      val () = g_print_exp_ (gs, 1, p1, e1)
      val _ = g_string_append_c (gs, (gchar)'*')
      val () = g_print_exp_ (gs, 1, p2, e2)
    in
      // nothing
    end // end of [Mul]
  | Div (e1, e2) => let
      val p1 = priority_mac e1 and p2 = priority_mac e2
      val () = g_print_exp_ (gs, 1, p1, e1)
      val _ = g_string_append_c (gs, (gchar)'/')
      val () = g_print_exp_ (gs, 1, p2, e2)
    in
      // nothing
    end // end of [Div]
end // end of [print_exp]

and g_print_exp_ {l:agz}
  (gs: !GString_ptr l, p0: Nat, p: Nat, e: exp): void =
  if p < p0 then g_print_exp (gs, e) else let
    val _ = g_string_append_c (gs, (gchar)'\(')
    val () = g_print_exp (gs, e)
    val _ = g_string_append_c (gs, (gchar)')')
  in
    // nothing
  end // end of [if]
// end of [print_exp_]

fun print_exp (e: exp): void = let
  val gs = g_string_new ()
  val () = g_print_exp (gs, e)
  val ptr = g_string_get_str (gs)
  val () = print (string) where {
    val string = __cast (ptr) where { extern castfn __cast (x: ptr): string }
  } // end of [val]
in
  g_string_free_true (gs)
end // end of [print_exp]
  
(* ****** ****** *)

typedef explst (n:int) = list (exp, n)

#define EPSILON 0.000001
fn iseq
  (a1: double, a2: double): bool = abs (a1 - a2) < EPSILON
// end of [isZero]

fn explst_add_exp_exp (
    ans: double, res: List exp, x1: exp, x2: exp
  ) : List exp = res where {
  var res: List exp = res
  val a1 = eval_exp x1 and a2 = eval_exp x2
  val () = if iseq (a1 + a2, ans) then res := list_cons (Add (x1, x2), res)
  val () = if iseq (a1 - a2, ans) then res := list_cons (Sub (x1, x2), res)
  val () = if iseq (a2 - a1, ans) then res := list_cons (Sub (x2, x1), res)
  val () = if iseq (a1 * a2, ans) then res := list_cons (Mul (x1, x2), res)
  val () = if iseq (a1 / a2, ans) then res := list_cons (Div (x1, x2), res)
  val () = if iseq (a2 / a1, ans) then res := list_cons (Div (x2, x1), res)
} // end of [explst_add_exp_exp]

macdef list_revapp = list_reverse_append

fun play {n:int | n >= 2} (
    ans: double, n: int n, xs: explst n, res: List exp
  ) : List exp =
  if n > 2 then let
    fun aux {n1,n3,n4:nat | n1+1+n3+n4 == n} .<n3+n4,n4>. (
        xs1: explst n1
      , x2: exp, xs2: explst (n3+n4)
      , xs3: explst n3, xs4: explst n4
      , res: List exp
      ) :<cloref1> List exp =
      case+ xs4 of
      | list_cons (x4, xs4) => let
          val ys = list_revapp (xs3, xs4)
          val ys = list_revapp (xs1, ys)
          val ys1 = list_cons (Add (x2, x4), ys)
          val res = play (ans, n-1, ys1, res)
          val ys1 = list_cons (Sub (x2, x4), ys)
          val res = play (ans, n-1, ys1, res)
          val ys1 = list_cons (Sub (x4, x2), ys)
          val res = play (ans, n-1, ys1, res)
          val ys1 = list_cons (Mul (x2, x4), ys)
          val res = play (ans, n-1, ys1, res)
          val ys1 = list_cons (Div (x2, x4), ys)
          val res = play (ans, n-1, ys1, res)
          val ys1 = list_cons (Div (x4, x2), ys)
          val res = play (ans, n-1, ys1, res)
        in
          aux (xs1, x2, xs2, list_cons (x4, xs3), xs4, res)
        end // end of [list_cons]
      | list_nil () => begin case+ xs2 of
        | list_cons (x21, xs21) => let
            val xs1 = list_cons (x2, xs1) in aux (xs1, x21, xs21, list_nil, xs21, res)
          end // end of [list_cons]
        | list_nil () => res
        end // end of [list_nil]
    val+ list_cons (x, xs) = xs
  in
    aux (list_nil, x, xs, list_nil, xs, res)
  end else let // n = 2
    val+ list_cons (x1, xs) = xs
    val+ list_cons (x2, xs) = xs
  in
    explst_add_exp_exp (ans, res, x1, x2)
  end (* end of [if] *)
// end of [play]

(* ****** ****** *)

%{^
ats_ptr_type
answering_gtk_dialog_new () {
  GtkWidget *widget ;
  widget = gtk_dialog_new_with_buttons (
    "Game-of-24 Answer Dialog"
  , NULL
  , GTK_DIALOG_MODAL
  , "_Close", 0
  , NULL
  ) ;
  g_object_ref_sink(G_OBJECT(widget)) ; // removing floating reference!
  return widget ;
}
%} // end of [%{^]
extern
fun answering_gtk_dialog_new (): GtkDialog_ptr1 = "answering_gtk_dialog_new"

overload gint with gint_of_GtkResponseType

fun answering
  (xs: List exp): void = () where {
  val dialog = answering_gtk_dialog_new ()
//
  val (fpf_vbox0 | vbox0) = gtk_dialog_takeout_vbox (dialog)
//
  val hbox1 = gtk_hbox_new (GFALSE, (gint)0)
  val () = gtk_widget_show (hbox1)
  val () = gtk_box_pack_start (vbox0, hbox1, GTRUE, GTRUE, guint(10))
  val () = (case+ xs of
    | list_cons _ => let
//
        val frame = gtk_frame_new ("Solution(s) found:")
        val () = gtk_box_pack_start (hbox1, frame, GTRUE, GFALSE, guint(10))
        val () = gtk_widget_show (frame)
        val [l_box:addr] vbox2 = gtk_vbox_new (GTRUE, gint(2))
        val () = gtk_container_add (frame, vbox2)
        val () = gtk_widget_show (vbox2)
//
        val [l_str:addr] gs  = g_string_new ()
        val () = loop (vbox2, gs, xs) where {
         fun loop (
             vbox2: !gobjptr (GtkVBox, l_box), gs: !GString_ptr l_str, xs: List exp
           ) : void = case+ xs of
           | list_cons (x, xs) => let
               val _ptr = g_string_truncate (gs, gsize(0))
               val () = g_print_exp (gs , x)
               val () = g_string_append_printf (gs, " = 24", @())
               val ptr = g_string_get_str (gs)
               val label_msg = gtk_label_new (msg) where {
                 val msg = __cast (ptr) where { extern castfn __cast (x: ptr): string }
               } // end of [val]
              val () = gtk_widget_show (label_msg)
              val () = gtk_box_pack_start (vbox2, label_msg, GFALSE, GTRUE, guint(0))
              val () = g_object_unref (label_msg)
            in
              loop (vbox2, gs, xs)
            end // end of [list_cons]
          | list_nil () => ()
        }
        val () = g_string_free_true (gs)
        val () = g_object_unref (vbox2)
        val () = g_object_unref (frame)
      in
        // nothing
      end // end of [if]
    | list_nil _ => let
        val label_ans = gtk_label_new ("No solution found!")
        val () = gtk_widget_show (label_ans)
        val () = gtk_box_pack_start (hbox1, label_ans, GTRUE, GFALSE, guint(10))
        val () = g_object_unref (label_ans)
      in
        // nothing
      end // end of [if]
  ) : void // end of [val]
  val () = g_object_unref (hbox1)
//
  prval () = fpf_vbox0 (vbox0)
  val () = gtk_widget_show (dialog)
//
  val () = while (true) let
    val response = gtk_dialog_run (dialog)
    // val () = (print "response = "; print ((int_of)response); print_newline ())
  in
    case+ 0 of
    | _ when response = (gint)0 => break
    | _ when response = (gint)GTK_RESPONSE_DELETE_EVENT => break
    | _ => ()
  end // end of [val]
//
  val () = gtk_widget_destroy (dialog)
} // end of [answering]

(* ****** ****** *)

fun play24 {n:nat}
  (ns: list_vt (int, n)) = let
  val xs = loop (ns, list_nil) where {
    fun loop {i,j:nat} .<i>.
      (ns: list_vt (int, i), xs: explst j): explst (i+j) =
      case+ ns of
      | ~list_vt_cons (n, ns) => let
          val x = Num (double_of(n)) in loop (ns, list_cons (x, xs))
        end // end of [list_vt_cons]
      | ~list_vt_nil () => xs
    // end of [loop]
  } // end of [val]
  val n = list_length (xs)
  val () = assert_errmsg (n >= 2, #LOCATION)
  val ans = 24.0
  val res = play (ans, n, xs, list_nil)
  val res = explst_remdup (res)
  val () = answering (res)
(*
  val () = loop (res) where {
    fun loop (xs: List exp): void =
      case+ xs of
      | list_cons (x, xs) => loop (xs) where {
          val () = (print_exp x; print " = "; print 24; print_newline ())
        } // end of [list_cons]
      | list_nil () => ()
    // end of [loop]
  } // end of [val]
*)
in
  // nothing
end // end of [play24]

(* ****** ****** *)

fun suit_spinner_gen
  (): GtkSpinButton_ptr1 = let
  val adj = gtk_adjustment_new
    (1.0, 1.0, 13.0, 1.0, 0.0(*ignored*), 0.0(*ignored*))
  val spinner = gtk_spin_button_new (adj, (gdouble)0.0, (guint)0)
  val () = gtk_widget_show (spinner)
  val () = gtk_spin_button_set_numeric (spinner, GTRUE)
  val () = gtk_spin_button_set_wrap (spinner, GTRUE)
  val () = g_object_unref (adj)
in
  spinner
end // end of [suit_spinner_gen]

(* ****** ****** *)

viewtypedef
suitSpinnerLst = List_vt (GtkSpinButton_ptr1)

val theSuitSpinnerLst =
  ref_make_elt<suitSpinnerLst> (list_vt_nil)
// end of [val]

fun theSuitSpinnerLst_add
  (x: !GtkSpinButton_ptr1): void = () where {
  val (vbox pf_xs | p_xs) = ref_get_view_ptr (theSuitSpinnerLst)
  val x1 = $effmask_ref (g_object_ref (x))
  val () = !p_xs := list_vt_cons (x1, !p_xs)
} // end of [theSuitSpinnerLst_add]

(* ****** ****** *)

fun inputapp (): void = () where {
  fun loop (xs: !suitSpinnerLst): void =
    case+ xs of
    | list_vt_cons (!p_x, !p_xs) => let
        var min: gdouble and max: gdouble
        val () = gtk_spin_button_get_range (!p_x, min, max)
        val max = double_of(max) and min = double_of(min)
        val v = $MATH.floor (min + (max + 1 - min) * $RAND.drand48 ())
        val v = gtk_spin_button_set_value (!p_x, (gdouble)v)
        val () = loop (!p_xs)
        prval () = fold@ (xs)
      in
        // nothing
      end // end of [list_vt_cons]
    | list_vt_nil () => (fold@ xs)
  // end of [loop]
  val () = $effmask_ref (loop (!p_xs)) where {
    val (vbox pf_xs | p_xs) = ref_get_view_ptr (theSuitSpinnerLst)
  }
} // end of [inputapp]

(* ****** ****** *)

fun evalapp (): void = let
  fun loop (xs: !suitSpinnerLst): List_vt int =
    case+ xs of
    | list_vt_cons (!p_x, !p_xs) => let
        val v = gtk_spin_button_get_value_as_int (!p_x)
        val v = int_of(v)
        val vs = loop (!p_xs)
        val () = fold@ (xs)
      in
        list_vt_cons (v, vs)
      end // end of [list_vt_cons]
    | list_vt_nil () => (fold@ xs; list_vt_nil)
  // end of [loop]
  val (vbox pf_xs | p_xs) = ref_get_view_ptr (theSuitSpinnerLst)
  val vs = $effmask_ref (loop (!p_xs))
  val () = $effmask_ref (play24 (vs))
in
  // nothing
end // end of [evalapp]

(* ****** ****** *)
staload PRINTF = "libc/SATS/printf.sats"

fun suit_spinnerlst_hbox_gen
  {n:nat} (n: int n): GtkHBox_ptr1 = let
  val hbox = gtk_hbox_new (GTRUE(*homo*), (gint)10(*spacing*))
  val () = gtk_widget_show (hbox)
  val () = loop (hbox, n, 1) where {
    fun loop
      {c:cls | c <= GtkBox}
      {l:agz} {n:nat} .<n>. (
        box: !gobjptr (c, l), n: int n, i: int
      ) : void =
      if n > 0 then let
        #define BUFSZ 16
        var !p_buf with pf_buf = @[byte][BUFSZ]()
        val vbox = gtk_vbox_new (GFALSE, (gint)0)
        val () = gtk_widget_show (vbox)
        val () = gtk_box_pack_start (box, vbox, GTRUE, GTRUE, (guint)20)
        val _ = $PRINTF.snprintf (pf_buf | p_buf, BUFSZ, "Card %d:", @(i))
        val label = gtk_label_new (txt) where {
          val txt = __cast (p_buf) where { extern castfn __cast (x: ptr): string }           
        } // end of [val]
        prval () = pf_buf := bytes_v_of_strbuf_v (pf_buf)
        val () = gtk_widget_show (label)        
        val () = gtk_box_pack_start (vbox, label, GFALSE, GTRUE, (guint)2)
        val spinner = suit_spinner_gen ()
        val () = theSuitSpinnerLst_add (spinner)
        val () = gtk_box_pack_start (vbox, spinner, GFALSE, GTRUE, (guint)2)
        val () = g_object_unref (label)
        val () = g_object_unref (spinner)
        val () = g_object_unref (vbox)
      in
        loop (box, n-1, i+1)
      end // end of [if]
    // end of [loop]
  } // end of [val]
in
  hbox
end // end of [suit_spinnerlst_hbox_gen]

(* ****** ****** *)

fun quitapp
  {c:cls | c <= GtkWidget} {l:agz}
  (widget: !gobjptr (c, l), event: &GdkEvent, _: gpointer): gboolean = let
  val () = gtk_main_quit ()
in
  GFALSE // delivered!
end // end of [quitapp]

(* ****** ****** *)

%{^
extern
ats_void_type
mainats (ats_int_type argc, ats_ptr_type argv) ;
%}

(* ****** ****** *)

extern fun main1 (): void = "main1"

implement main1 () = () where {
//
  val () = $RAND.srand48_with_time ()
//
  val window = gtk_window_new (GTK_WINDOW_TOPLEVEL)
  val (fpf_window | window_) = g_object_vref (window)
  val _sig = g_signal_connect0
    (window_, (gsignal)"destroy", G_CALLBACK(gtk_widget_destroy), (gpointer)null)
  val _sig = g_signal_connect1
    (window, (gsignal)"delete_event", G_CALLBACK(quitapp), (gpointer)null)
  val () = gtk_window_set_title (window, "Game-of-24")
//
  val vbox0 = gtk_vbox_new (GFALSE(*homo*), (gint)0)
  val () = gtk_widget_show (vbox0)
//
  val label_title = gtk_label_new ("Game-of-24")
  val () = gtk_widget_show (label_title)
  val () = gtk_box_pack_start (vbox0, label_title, GTRUE, GTRUE, (guint)10)
  val () = g_object_unref (label_title)
//
  val hsep = gtk_hseparator_new ()
  val () = gtk_widget_show (hsep)
  val () = gtk_box_pack_start (vbox0, hsep, GTRUE, GTRUE, (guint)0)
  val () = g_object_unref (hsep)
//
  val hbox_suits = suit_spinnerlst_hbox_gen (4)
  val () = gtk_box_pack_start (vbox0, hbox_suits, GTRUE, GTRUE, (guint)10)
  val () = g_object_unref (hbox_suits)
//
  val hsep = gtk_hseparator_new ()
  val () = gtk_widget_show (hsep)
  val () = gtk_box_pack_start (vbox0, hsep, GTRUE, GTRUE, (guint)0)
  val () = g_object_unref (hsep)
//
  val hbox = gtk_hbox_new (GFALSE, (gint)0)
  val () = gtk_widget_show (hbox)
  val () = gtk_box_pack_start (vbox0, hbox, GTRUE, GTRUE, (guint)10)

  val () = () where { // adding the [input] button
    val button = gtk_button_new_with_label ("Random Input")
    val _sid = g_signal_connect
      (button, (gsignal)"clicked", G_CALLBACK(inputapp), (gpointer)null)
    val () = gtk_widget_show (button)
    val () = gtk_box_pack_start (hbox, button, GTRUE, GTRUE, (guint)10)
    val () = g_object_unref (button)
  } // end of [val]

  val () = () where { // adding the [eval] button
    val button = gtk_button_new_with_label ("Eval")
    val _sid = g_signal_connect
      (button, (gsignal)"clicked", G_CALLBACK(evalapp), (gpointer)null)
    val () = gtk_widget_show (button)
    val () = gtk_box_pack_start (hbox, button, GTRUE, GTRUE, (guint)10)
    val () = g_object_unref (button)
  } // end of [val]

  val () = g_object_unref (hbox)
//
  val hsep = gtk_hseparator_new ()
  val () = gtk_widget_show (hsep)
  val () = gtk_box_pack_start (vbox0, hsep, GTRUE, GTRUE, (guint)0)
  val () = g_object_unref (hsep)
//
  val hbox = gtk_hbox_new (GFALSE, (gint)0)
  val () = gtk_widget_show (hbox)
  val button = gtk_button_new_with_label ("Quit")
  val _sid = g_signal_connect_swapped
    (button, (gsignal)"clicked", G_CALLBACK(quitapp), window)
  val () = gtk_widget_show (button)
  val () = gtk_box_pack_start (hbox, button, GTRUE, GTRUE, (guint)10)
  val () = g_object_unref (button)
  val () = gtk_box_pack_start (vbox0, hbox, GTRUE, GTRUE, (guint)10)
  val () = g_object_unref (hbox)
//
  val () = gtk_container_add (window, vbox0)
  val () = g_object_unref (vbox0)
//
  val () = gtk_widget_show (window)
  prval () = fpf_window (window)
  val () = gtk_main ()
} // end of [main]

(* ****** ****** *)

implement main_dummy () = ()

(* ****** ****** *)

%{$
ats_void_type
mainats (
  ats_int_type argc, ats_ptr_type argv
) {
  gtk_init ((int*)&argc, (char***)&argv) ;
  main1 () ;
  return ;
} // end of [mainats]
%} // end of [%{^]

(* ****** ****** *)

(* end of [GameOf24.dats] *)
