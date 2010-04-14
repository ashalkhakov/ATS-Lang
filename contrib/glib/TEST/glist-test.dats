(*
** some testing code for the quicksort function declared in
** contrib/glib/SATS/glist.sats
*)

(* ****** ****** *)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: February, 2010
//

(* ****** ****** *)

staload "contrib/glib/SATS/glib.sats"
staload _(*anon*) = "contrib/glib/DATS/glib.dats"

(* ****** ****** *)

%{^
#define N 10
//
static
int test_nums[N] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 } ;
#define test_ptr(i) (&test_nums[i])
//
static
int more_nums[N] = { 8, 9, 7, 0, 3, 2, 5, 1, 4, 6 } ;
#define more_ptr(i) (&more_nums[i])
//
#define ptr_read(p) (*(int*)(p))
%} // end of [%{^]

extern fun ptr_read (p: ptr):<> int = "#ptr_read"
extern fun test_ptr {i:nat} (i: int i): ptr = "#test_ptr"
extern fun more_ptr {i:nat} (i: int i): ptr = "#more_ptr"

implement main () = () where {
  val () = begin
    print ("glist-test: starts"); print_newline ()
  end // end of [va]
//
  #define N 10
//
  val glist = loop (glist, 0) where {
    val glist = g_list_new_nil ()
    fun loop {i:nat | i <= N} .<N-i>.
      (xs: GList_ptr (ptr, i), i: int i): GList_ptr (ptr, N) =
      if i < N then let
        val x = test_ptr i
        val xs = g_list_append {ptr} (xs, x) in loop (xs, i+1)
      end else xs
    // end of [loop]
  } // end of [val]
  val () = assert_errmsg ((gint)N = g_list_length (glist), #LOCATION)
  val () = loop (glist, 0) where {
    fun loop {i:nat | i <= N}
      (xs: !GList_ptr (ptr, N), i: int i): void =
      if i < N then let
        val x = g_list_nth_data {ptr} (xs, i)
        val () = assert_errmsg (ptr_read(x) = i, #LOCATION)
      in
        loop (xs, i+1)
      end // end of [if]
  } // end of [val]
  val () = g_list_free {ptr} (glist)
//
  val glist = loop (glist, 0) where {
    val glist = g_list_new_nil ()
    fun loop {i:nat | i <= N} .<N-i>.
      (xs: GList_ptr (ptr, i), i: int i): GList_ptr (ptr, N) =
      if i < N then let
        val x = test_ptr i
        val xs = g_list_prepend {ptr} (xs, x) in loop (xs, i+1)
      end else xs
    // end of [loop]
  } // end of [val]
  val () = assert_errmsg ((gint)N = g_list_length (glist), #LOCATION)
  val () = loop (glist, 0) where {
    fun loop {i:nat | i <= N}
      (xs: !GList_ptr (ptr, N), i: int i): void =
      if i < N then let
        val x = g_list_nth_data {ptr} (xs, i)
        val () = assert_errmsg (ptr_read(x) = N-1-i, #LOCATION)
      in
        loop (xs, i+1)
      end // end of [if]
  } // end of [val]
  val () = g_list_free {ptr} (glist)
//
  val glist = loop (glist, 0) where {
    val glist = g_list_new_nil ()
    val cmp = lam (x1: !ptr, x2: !ptr)
      : gint =<fun> (gint)(ptr_read x1 - ptr_read x2)
    // end of [val]
    fun loop {i:nat | i <= N} .<N-i>.
      (xs: GList_ptr (ptr, i), i: int i):<cloref1> GList_ptr (ptr, N) =
      if i < N then let
        val x = test_ptr i
        val xs = g_list_insert_sorted {ptr} (xs, x, cmp) in loop (xs, i+1)
      end else xs
    // end of [loop]
  } // end of [val]
  val () = assert_errmsg ((gint)N = g_list_length (glist), #LOCATION)
  val glist = g_list_reverse (glist)
  val () = assert_errmsg ((gint)N = g_list_length (glist), #LOCATION)
  val () = loop (glist, 0) where {
    fun loop {i:nat | i <= N}
      (xs: !GList_ptr (ptr, N), i: int i): void =
      if i < N then let
        val x = g_list_nth_data {ptr} (xs, i)
        val () = assert_errmsg (ptr_read(x) = N-1-i, #LOCATION)
      in
        loop (xs, i+1)
      end // end of [if]
  } // end of [val]
  val () = g_list_free {ptr} (glist)
//
  val () = begin
    print ("glist-test: finishes"); print_newline ()
  end // end of [va]
} // end of [main]

(* ****** ****** *)

(* end of [glist-test.dats] *)
