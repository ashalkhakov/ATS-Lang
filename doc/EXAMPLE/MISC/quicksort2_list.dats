(*
//
// A verified implementation of quicksort on lists:
// the returned output list is guaranteed to be a permutation
// of the original input
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: Tuesday, October 5, 2010
//
*)

(* ****** ****** *)

staload "libats/SATS/ilistp.sats"

(* ****** ****** *)

abst@ype T (x:int)

extern
fun lte_elt_elt {x,y:nat} (x: T x, y: T y):<> bool (x <= y)
overload <= with lte_elt_elt

datatype list (ilist) =
  | nil (ilist_nil)
  | {x:pos} {xs: ilist} cons (ilist_cons (x, xs)) of (T (x), list (xs))
// end of [list]

(* ****** ****** *)

extern
fun quicksort {xs:ilist}
  (xs: list (xs)): [ys:ilist] (PERMUTE (xs, ys) | list (ys))
// end of [quicksort]

(* ****** ****** *)

extern
fun append {xs1,xs2:ilist}
  (xs1: list xs1, xs2: list xs2)
  : [xs3: ilist] (MUNION (xs1, xs2, xs3) | list xs3)
// end of [append]

implement
append {xs1, xs2}
  (xs1, xs2) = case+ xs1 of
  | cons {x1} (x1, xs11) => let
      val [xs31:ilist] (fpf1 | xs31) = append (xs11, xs2)
      stadef xs3 = ilist_cons (x1, xs31)
      prval fpf = lam {x0:int} {n1,n2:nat} (
        pf1: MSETCNT (x0, xs1, n1), pf2: MSETCNT (x0, xs2, n2)
      ) : MSETCNT (x0, xs3, n1+n2) =<prf> let
        prval MSETCNTcons (pf11) = pf1 in MSETCNTcons (fpf1 (pf11, pf2))
      end // end of [prval]
    in
      (fpf | cons (x1, xs31))
    end // end of [cons]
  | nil () => let
      stadef xs3 = xs2
      prval fpf = lam {x0:int} {n1,n2:nat} (
        pf1: MSETCNT (x0, xs1, n1), pf2: MSETCNT (x0, xs2, n2)
      ) : MSETCNT (x0, xs3, n1+n2) =<prf> let
        prval MSETCNTnil () = pf1 in pf2
      end // end of [prval]
    in
      (fpf | xs2)
    end // end of [nil]
// end of [append]  

(* ****** ****** *)

propdef PART (
  x: int, xs0: ilist, xs1: ilist, xs2: ilist, xs: ilist
) = {x0:int} {n0,n1,n2:nat} (
  MSETCNT (x0, xs0, n0), MSETCNT (x0, xs1, n1), MSETCNT (x0, xs2, n2)
) -<prf> MSETCNT (x0, xs, n0+n1+n2+b2i(x0==x))
// end of [PART]

fun qsrt {xs:ilist}
  (xs: list xs): [ys:ilist] (PERMUTE (xs, ys) | list (ys)) =
  case+ xs of
  | cons {x} {xs1} (x, xs1) => let
      val [ys:ilist] (fpf | ys) = part (x, xs1, nil (), nil ())
      prval fpf = lam {x0:int} {n:nat}
        (pf: MSETCNT (x0, xs, n)): MSETCNT (x0, ys, n) =<prf> let
        prval MSETCNTcons pf = pf
      in
         fpf (pf, MSETCNTnil, MSETCNTnil)
      end // end of [prval]
    in
      (fpf | ys)
    end // end of [cons]
  | nil () => (permute_refl {ilist_nil} () | nil ())
// end // end of [qsrt]

and part {x:pos} {xs,xs1,xs2:ilist} (
  x: T x, xs: list xs, xs1: list xs1, xs2: list xs2
) : [ys:ilist] (PART (x, xs, xs1, xs2, ys) | list ys) =
  case xs of
  | cons (x_, xs_) => (
      if (x_ <= x) then let
        val [ys:ilist] (fpf | ys) = part (x, xs_, cons (x_, xs1), xs2)
        prval fpf = lam {x0:int} {n0,n1,n2:nat}
          (pf0: MSETCNT (x0, xs, n0), pf1: MSETCNT (x0, xs1, n1), pf2: MSETCNT (x0, xs2, n2))
          : MSETCNT (x0, ys, n0+n1+n2+b2i(x0==x)) =<prf> let
          prval MSETCNTcons pf0 = pf0
          prval pf1 = MSETCNTcons (pf1)
        in
          fpf (pf0, pf1, pf2)
        end // end of [prval]
      in
        (fpf | ys)
      end else let
        val [ys:ilist] (fpf | ys) = part (x, xs_, xs1, cons (x_, xs2))
        prval fpf = lam {x0:int} {n0,n1,n2:nat}
          (pf0: MSETCNT (x0, xs, n0), pf1: MSETCNT (x0, xs1, n1), pf2: MSETCNT (x0, xs2, n2))
          : MSETCNT (x0, ys, n0+n1+n2+b2i(x0==x)) =<prf> let
          prval MSETCNTcons pf0 = pf0
          prval pf2 = MSETCNTcons (pf2)
        in
          fpf (pf0, pf1, pf2)
        end // end of [prval]
      in
        (fpf | ys)
      end // end of [let]
    ) // end of [cons]
  | nil () => let
      val [ys1:ilist] (fpf1 | ys1) = qsrt (xs1)
      val [ys2:ilist] (fpf2 | ys2) = qsrt (xs2)
      val [ys:ilist] (fpf3 | ys) = append (ys1, cons (x, ys2))
      prval fpf = lam {x0:int} {n0,n1,n2:nat}
        (pf0: MSETCNT (x0, xs, n0), pf1: MSETCNT (x0, xs1, n1), pf2: MSETCNT (x0, xs2, n2))
        : MSETCNT (x0, ys, n0+n1+n2+b2i(x0==x)) =<prf> let
        prval MSETCNTnil () = pf0
        prval pf1 = fpf1 (pf1) // pf1 : MSETCNT (x0, ys1, n1)
        prval pf2 = fpf2 (pf2) // pf2 : MSETCNT (x0, ys2, n2)
      in
        fpf3 (pf1, MSETCNTcons (pf2))
      end // end of [prval]
    in
      (fpf | ys)
    end // end of [nil]
// end of [part]

(* ****** ****** *)

implement quicksort (xs) = qsrt (xs)

(* ****** ****** *)

(* end of [quicksort2_list.dats] *)
