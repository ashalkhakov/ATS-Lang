(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
 * ATS - Unleashing the Potential of Types!
 *
 * Copyright (C) 2002-2008 Hongwei Xi, Boston University
 *
 * All rights reserved
 *
 * ATS is free software;  you can  redistribute it and/or modify it under
 * the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
 * Free Software Foundation; either version 2.1, or (at your option)  any
 * later version.
 * 
 * ATS is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
 * for more details.
 * 
 * You  should  have  received  a  copy of the GNU General Public License
 * along  with  ATS;  see the  file COPYING.  If not, please write to the
 * Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
 *
 *)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0 // loaded by [ats_main_prelude]

(* ****** ****** *)

// list0 implementation

#define nil list0_nil
#define cons list0_cons
#define :: list0_cons

(* ****** ****** *)

implement{a} list0_make_arraysize (arrsz) =
  list0_of_list1 (list_of_arraysize<a> arrsz)

(* ****** ****** *)

implement{a} list0_append (xs, ys) = let
  val xs = list1_of_list0 xs and ys = list1_of_list0 ys
in
  list0_of_list1 (list_append (xs, ys))
end

(* ****** ****** *)

implement{a} list0_exists_fun (xs, f) =
  list_exists_fun (list1_of_list0 xs, f)

implement{a} list0_exists_cloref (xs, f) =
  list_exists_cloref (list1_of_list0 xs, f)

(* ****** ****** *)

implement{a} list0_forall_fun (xs, f) =
  list_forall_fun (list1_of_list0 xs, f)

implement{a} list0_forall_cloref (xs, f) =
  list_forall_cloref (list1_of_list0 xs, f)

(* ****** ****** *)

implement{a} list0_foreach_fun (xs, f) = let
  fn f (pf: !unit_v | x: a): void = f (x)
  prval pf = unit_v ()
  val () = list_foreach_fun<a> {unit_v} (pf | list1_of_list0 xs, f)
  prval unit_v () = pf
in
  // empty
end // end of [list0_foreach_fun]
  
implement{a} list0_foreach_cloref (xs, f) = let
  val () = list_foreach_cloref<a> (list1_of_list0 xs, f)
in
  // empty
end // end of [list0_foreach_cloref]

(* ****** ****** *)

implement{a} list0_head_exn (xs) = begin case+ xs of
  | list0_cons (x, xs) => x | list0_nil () => $raise ListSubscriptException
end // end of [list0_head_exn]

(* ****** ****** *)

implement{a} list0_length (xs) =
  list_length (list1_of_list0 xs)

(* ****** ****** *)

implement{a,b} list0_map_fun (xs, f) =
  list0_of_list1 (list_map_fun (list1_of_list0 xs, f))
// end of [list0_map_fun]

implement{a,b} list0_map_cloref (xs, f) =
  list0_of_list1 (list_map_cloref (list1_of_list0 xs, f))
// end of [list0_map_cloref]

(* ****** ****** *)

implement{a1,a2,b} list0_map2_fun (xs1, xs2, f) = let
  fun loop {n1,n2:nat} .<n1>.
    (xs1: list (a1, n1), xs2: list (a2, n2), res: &list0 b? >> list0 b)
    :<cloref1> void =
    case+ (xs1, xs2) of
    | (list_cons (x1, xs1), list_cons (x2, xs2)) => let
        val y = f (x1, x2)
        val () = res := cons (y, ?)
        val+ cons (_, !p_ys) = res
        val () = loop (xs1, xs2, !p_ys)
      in
        fold@ (res)
      end // end of [list_cons _, list_cons _]
    | (_, _) => let
        val () = res := nil () in ()
      end // end of [_, _]
  // end of [loop]
  var res: list0 b // uninitialized
  val () = loop (xs1, xs2, res) where {
    val xs1 = list1_of_list0 (xs1) and xs2 = list1_of_list0 (xs2)
  } // end of [val]
in
  res
end // end of [list0_map2_fun]

implement{a1,a2,b} list0_map2_cloref (xs1, xs2, f) = let
  fun loop {n1,n2:nat} .<n1>.
    (xs1: list (a1, n1), xs2: list (a2, n2), res: &list0 b? >> list0 b)
    :<cloref1> void =
    case+ (xs1, xs2) of
    | (list_cons (x1, xs1), list_cons (x2, xs2)) => let
        val y = f (x1, x2)
        val () = res := cons (y, ?)
        val+ cons (_, !p_ys) = res
        val () = loop (xs1, xs2, !p_ys)
      in
        fold@ (res)
      end // end of [list_cons _, list_cons _]
    | (_, _) => let
        val () = res := nil () in ()
      end // end of [_, _]
  // end of [loop]
  var res: list0 b // uninitialized
  val () = loop (xs1, xs2, res) where {
    val xs1 = list1_of_list0 (xs1) and xs2 = list1_of_list0 (xs2)
  } // end of [val]
in
  res
end // end of [list0_map2_cloref]

(* ****** ****** *)

implement{a} list0_nth_exn (xs, i) = let
  fun aux (xs: list0 a, i: Nat): a = begin case+ xs of
    | cons (x, xs) => if i > 0 then aux (xs, i-1) else x
    | nil () => $raise ListSubscriptException
  end // end of [aux]
  val i = int1_of_int i
in
  if i >= 0 then aux (xs, i) else $raise ListSubscriptException
end // end of [list0_nth_exn]

implement{a} list0_nth_opt (xs, i) = begin try
  let val x = list0_nth_exn<a> (xs, i) in Some x end
with
  | ~ListSubscriptException () => None ()
end // end of [try]

(* ****** ****** *)

implement{a} list0_reverse (xs) =
  list0_reverse_append (xs, list0_nil ())

implement{a} list0_reverse_append (xs, ys) = let
  val xs = list1_of_list0 xs and ys = list1_of_list0 ys
in
  list0_of_list1 (list_reverse_append (xs, ys))
end // end of [list0_reverse_append]

(* ****** ****** *)

implement{a} list0_tail_exn (xs) = begin case+ xs of
  | list0_cons (x, xs) => xs | list0_nil () => $raise ListSubscriptException
end // end of [list0_tail_exn]

(* ****** ****** *)

// [list0.sats] is already loaded by a call to [pervasive_load]
staload _(*anonymous*) = "prelude/SATS/list0.sats" // this forces that the static
// loading function for [list0.sats] is to be called at run-time
// this is really needed only if some datatypes are declared in [list0.sats]

(* ****** ****** *)

(* end of [list0.dats] *)
