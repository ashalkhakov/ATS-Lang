(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
 * ATS/Anairiats - Unleashing the Potential of Types!
 *
 * Copyright (C) 2002-2008 Hongwei Xi, Boston University
 *
 * All rights reserved
 *
 * ATS is free software;  you can  redistribute it and/or modify it under
 * the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
 * Free Software Foundation; either version 3, or (at  your  option)  any
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

// Time: December 2007
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

(* Mainly for testing exhaustiveness of pattern mattching  *)

(* ****** ****** *)

%{^

#include "ats_intinf.cats"

%}

(* ****** ****** *)

staload Err = "ats_error.sats"
staload IntInf = "ats_intinf.sats"
staload Lst = "ats_list.sats"
staload SDC = "ats_stadyncst2.sats"

(* ****** ****** *)

staload "ats_staexp2.sats"
staload "ats_dynexp2.sats"

(* ****** ****** *)

staload "ats_patcst2.sats"

(* ****** ****** *)

#define nil list_nil
#define :: list_cons
#define cons list_cons

macdef singleton (x) = cons (,(x), nil ())

(* ****** ****** *)

overload < with $IntInf.lt_intinf_intinf
overload = with $IntInf.eq_intinf_intinf

(* ****** ****** *)

assume intinfset_t = List intinf_t // of increasing order

implement intinflst_of_intinfset (xs) = xs

fun intinfset_add
  (xs0: intinfset_t, x0: intinf_t, err: &int): intinfset_t =
  case+ xs0 of
  | cons (x, xs) => begin
      if x < x0 then x :: intinfset_add (xs, x0, err)
      else begin
        if x0 < x then x0 :: xs0 else (err := 1; xs0)
      end
    end
  | nil () => cons (x0, nil ())

fun intinfset_sing (x: intinf_t): intinfset_t = cons (x, nil ())

(* ****** ****** *)

fn p2atcst_lst (p2tcs0: p2atcstlst): p2atcst = let
(*
  val () = begin
    prerr "p2atcst_lst: p2tcs0 = "; prerr p2tcs0; prerr_newline ()
  end
*)
  fun aux
    (d2c_cons: d2con_t, d2c_nil: d2con_t, p2tcs: p2atcstlst)
    : p2atcst = case+ p2tcs of
    | p2tc_elt :: p2tcs => let
        val p2tc_lst = aux (d2c_cons, d2c_nil, p2tcs)
      in
        P2TCcon (d2c_cons, '[p2tc_elt, p2tc_lst])
      end
    | nil () => P2TCcon (d2c_nil, '[])
  val d2c_nil = $SDC.d2conref_con_get ($SDC.List_nil)
  val d2c_cons = $SDC.d2conref_con_get ($SDC.List_cons)
  val p2tcs1 = aux (d2c_cons, d2c_nil, p2tcs0)
(*
  val () = begin
    prerr "p2atcst_lst: p2tcs1 = "; prerr p2tcs1; prerr_newline ()
  end
*)
in
  p2tcs1
end // end of [p2atcst_lst]

fun labp2atcstlst_of_labp2atlst (lp2ts: labp2atlst): labp2atcstlst =
  case+ lp2ts of
  | LABP2ATLSTcons (l, p2t, lp2ts) => let
      val p2tc = p2atcst_of_p2at p2t
      val lp2tcs = labp2atcstlst_of_labp2atlst lp2ts
    in
      LABP2ATCSTLSTcons (l, p2tc, lp2tcs)
    end
  | _ => LABP2ATCSTLSTnil ()

implement p2atcst_of_p2at p2t0 = begin
  case+ p2t0.p2at_node of
  | P2Tann (p2t, _) => p2atcst_of_p2at p2t
  | P2Tany () => P2TCany ()
  | P2Tas (_, _, p2t) => p2atcst_of_p2at p2t
  | P2Tbool b => P2TCbool b
  | P2Tchar c => P2TCchar c
  | P2Tcon (_, d2c, _, _, _, p2ts) => begin
      P2TCcon (d2c, p2atcstlst_of_p2atlst p2ts)
    end
  | P2Tempty () => P2TCempty ()
  | P2Texist (_, p2t) => p2atcst_of_p2at p2t
  | P2Tfloat f(*string*) => P2TCfloat f
  | P2Tint (s(*string*), i(*intinf*)) => P2TCint i
  | P2Tlist _ => begin
      prerr "Internal Error: p2atcst_of_p2at: P2Tlist";
      prerr_newline ();
      $Err.abort {p2atcst} ()
    end
  | P2Tlst p2ts => p2atcst_lst (p2atcstlst_of_p2atlst p2ts)
  | P2Trec (recknd, _(*npf*), lp2ts) => begin
      P2TCrec (recknd, labp2atcstlst_of_labp2atlst lp2ts)
    end
  | P2Tstring s => P2TCstring s
  | P2Tvar _ => P2TCany ()
  | P2Tvbox _ => P2TCany ()
end // end of [p2atcst_of_p2at]

implement p2atcstlst_of_p2atlst (p2ts) = case+ p2ts of
  | cons (p2t, p2ts) => begin
      cons (p2atcst_of_p2at p2t, p2atcstlst_of_p2atlst p2ts)
    end
  | nil () => nil ()

(* ****** ****** *)

fun fprint_intinfset {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, xs: intinfset_t)
  : void = let
  fun aux (out: &FILE m, i: int, xs: intinfset_t)
    : void = begin case+ xs of
    | x :: xs => begin
        if i > 0 then fprint (pf | out, ", ");
        $IntInf.fprint_intinf (pf | out, x); aux (out, i+1, xs)
      end
    | nil () => ()
  end // end of [aux]
in
  aux (out, 0, xs)
end // end of [fprint_intinfset]

implement fprint_p2atcst (pf | out, p2tc) = case+ p2tc of
  | P2TCany () => fprint (pf | out, '_')
  | P2TCbool b => begin
      if b then fprint (pf | out, "true") else fprint (pf | out, "false")
    end
  | P2TCchar c => begin
      fprint (pf | out, '\''); fprint (pf | out, c); fprint (pf | out, '\'')
    end
  | P2TCcon (d2c, p2tcs) => begin
      fprint (pf | out, d2c);
      fprint (pf | out, "(");
      fprint (pf | out, p2tcs);
      fprint (pf | out, ")")
    end
  | P2TCempty () => fprint (pf | out, "()")
  | P2TCfloat float => fprint (pf | out, float)
  | P2TCint int => $IntInf.fprint_intinf (pf | out, int)
  | P2TCintc ints => begin
      fprint (pf | out, "[^");
      fprint_intinfset (pf | out, ints);
      fprint (pf | out, "]")
    end
  | P2TCrec (recknd, lp2tcs) => begin
      if recknd > 0 then fprint (pf | out, '\'') else fprint (pf | out, '@');
      fprint (pf | out, "{");
      fprint (pf | out, lp2tcs);
      fprint (pf | out, "}")
    end
  | P2TCstring s => begin
      fprint (pf | out, '"'); fprint (pf | out, s); fprint (pf | out, '"')
    end

implement fprint_p2atcstlst {m} (pf | out, p2tcs) = let
  fun aux (out: &FILE m, i: int, p2tcs: p2atcstlst)
    : void = begin case+ p2tcs of
    | cons (p2tc, p2tcs) => begin
        if i > 0 then fprint (pf | out, ", ");
        fprint_p2atcst (pf | out, p2tc);
        aux (out, i+1, p2tcs)
      end
    | nil () => ()
  end // end of [aux]
in
  aux (out, 0, p2tcs)
end // end of [fprint_p2atcstlst]

implement fprint_labp2atcstlst {m} (pf | out, lp2tcs) = let
  fun aux (out: &FILE m, i: int, lp2tcs: labp2atcstlst): void =
    case+ lp2tcs of
    | LABP2ATCSTLSTcons (l, p2tc, lp2tcs) => begin
        if i > 0 then fprint (pf | out, ", ");
        $Lab.fprint_label (pf | out, l); fprint (pf | out, "= ");
        fprint_p2atcst (pf | out, p2tc);
        aux (out, i + 1, lp2tcs)
      end
    | LABP2ATCSTLSTnil () => ()
in
  aux (out, 0, lp2tcs)
end // end of [fprint_labp2atcstlst]

implement fprint_p2atcstlstlst {m} (pf | out, p2tcss) = let
  fun aux (out: &FILE m, p2tcss: p2atcstlstlst): void =
    case+ p2tcss of
    | cons (p2tcs, p2tcss) => begin
        fprint_p2atcstlst (pf | out, p2tcs);
        fprint_newline (pf | out);
        aux (out, p2tcss)
      end
    | nil () => ()
in
  aux (out, p2tcss)
end // end of [fprint_p2atcstlstlst]

(* ****** ****** *)

implement print_p2atcst (p2tc) = print_mac (fprint_p2atcst, p2tc)
implement prerr_p2atcst (p2tc) = prerr_mac (fprint_p2atcst, p2tc)

implement print_p2atcstlst (p2tcs) = print_mac (fprint_p2atcstlst, p2tcs)
implement prerr_p2atcstlst (p2tcs) = prerr_mac (fprint_p2atcstlst, p2tcs)

implement print_p2atcstlstlst (p2tcss) = print_mac (fprint_p2atcstlstlst, p2tcss)
implement prerr_p2atcstlstlst (p2tcss) = prerr_mac (fprint_p2atcstlstlst, p2tcss)

(* ****** ****** *)

fn p2atcst_con_complement
  (d2c0: d2con_t, d2cs: d2conlst, p2tcs: p2atcstlst)
  : p2atcstlst = let
  fun aux (d2cs: d2conlst, res: p2atcstlst_vt):<cloptr1> p2atcstlst = begin
    case+ d2cs of
    | D2CONLSTcons (d2c, d2cs) => begin
        if d2c0 = d2c then let
          val res =
            aux1 (d2c, p2atcstlst_complement p2tcs, res) where {
            fun aux1
              (d2c: d2con_t, p2tcss: p2atcstlstlst, res: p2atcstlst_vt)
              : p2atcstlst_vt = case+ p2tcss of
              | p2tcs :: p2tcss => begin
                  aux1 (d2c, p2tcss, list_vt_cons (P2TCcon (d2c, p2tcs), res))
                end
              | nil () => res
          } // end [where]
        in
          aux (d2cs, res)
        end else let
          val p2tcs_any =
            aux2 (d2con_arity_full_get d2c, nil ()) where {
            fun aux2 (n: int, res: p2atcstlst): p2atcstlst =
              if n > 0 then aux2 (n-1, cons (P2TCany (), res))
              else res
          } // end of [where]
          val res = list_vt_cons (P2TCcon (d2c, p2tcs_any), res)
        in
          aux (d2cs, res)
        end
      end // end of [D2CONLSTcons]
    | D2CONLSTnil () => $Lst.list_vt_reverse_list (res)
  end // end of [aux]
in
  aux (d2cs, list_vt_nil ())
end

implement p2atcst_complement (p2tc0) = case+ p2tc0 of
  | P2TCany () => nil ()
  | P2TCbool b => cons (P2TCbool ~b, nil ())
  | P2TCchar _ => singleton (P2TCany ()) // conservative estimation
  | P2TCcon (d2c0, p2tcs) => let
      val tag = d2con_tag_get d2c0
    in
      if tag >= 0 then let
        val s2c0 = d2con_scst_get d2c0
        val d2cs: d2conlst = case+ s2cst_conlst_get s2c0 of
          | Some d2cs => d2cs
          | None _ => begin
              prerr "Internal Error: p2atcst_complement: s2c0 = ";
              prerr s2c0;
              prerr_newline ();
              $Err.abort {d2conlst} ()
            end
        in
          p2atcst_con_complement (d2c0, d2cs, p2tcs)
        end
      else begin // associated with a extensible sum
        singleton (P2TCany ())
      end
    end // end of [P2TCcon]
  | P2TCempty () => nil ()
  | P2TCint x => singleton (P2TCintc (intinfset_sing x))
  | P2TCintc xs => aux xs where {
      fun aux (xs: List intinf_t): p2atcstlst = case+ xs of
        cons (x, xs) => P2TCint x :: aux (xs) | nil () => nil ()
    } // end of [where]
  | P2TCrec (knd, lp2tcs) => let
      val lp2tcss = labp2atcstlst_complement lp2tcs
    in
      $Lst.list_map_cloptr (lp2tcss, lam lp2tcs =<cloptr> P2TCrec (knd, lp2tcs))
    end
  | P2TCstring _ => singleton (P2TCany ())  // conservative estimation
  | _ => begin
      prerr "Internal Error: p2atcst_complement: p2tc0 = ";
      prerr p2tc0;
      prerr_newline ();
      $Err.abort {p2atcstlst} ()
    end

implement p2atcstlst_complement {n} (p2tcs0) = begin
  case+ p2tcs0 of
  | p2tc1 :: p2tcs1 => let
      val p2tcs1_any = aux p2tcs1 where {
        fun aux {n:nat} (p2tcs: p2atcstlst n): p2atcstlst n =
          case+ p2tcs of
          | _ :: p2tcs => cons (P2TCany (), aux p2tcs) | nil () => nil ()
      } // end of [where]
      val p2tcss = aux (p2atcstlst_complement p2tcs1) where {
        fun aux {n:nat}
          (p2tcss: p2atcstlstlst n):<cloptr1> p2atcstlstlst (n+1) =
          case+ p2tcss of
          | p2tcs :: p2tcss => cons (p2tc1 :: p2tcs, aux p2tcss)
          | nil () => nil ()
      } // end of [where]
      val p2tcss = aux (p2atcst_complement p2tc1) where {
        fun aux (p2tcs: p2atcstlst):<cloptr1> p2atcstlstlst (n) =
        case+ p2tcs of
        | p2tc :: p2tcs => cons (p2tc :: p2tcs1_any, aux p2tcs)
        | nil () => p2tcss
      } // end of [where]
    in
      p2tcss
    end
  | nil () => nil ()
end // end of [p2atcstlst_commplement]

implement labp2atcstlst_complement (lp2tcs0) = begin
  case+ lp2tcs0 of
  | LABP2ATCSTLSTcons (l1, p2tc1, lp2tcs1) => let
      val lp2tcs1_any = aux lp2tcs1 where {
        fun aux (lp2tcs: labp2atcstlst): labp2atcstlst =
          case+ lp2tcs of
          | LABP2ATCSTLSTcons (l, _, lp2tcs) => begin
              LABP2ATCSTLSTcons (l, P2TCany (), aux lp2tcs)
            end
          | LABP2ATCSTLSTnil () => LABP2ATCSTLSTnil ()
      } // end of [where]
      val lp2tcss = aux (labp2atcstlst_complement lp2tcs1) where {
        fun aux (lp2tcss: labp2atcstlstlst):<cloptr1> labp2atcstlstlst =
          case+ lp2tcss of
          | lp2tcs :: lp2tcss => begin
              cons (LABP2ATCSTLSTcons (l1, p2tc1, lp2tcs), aux lp2tcss)
            end
          | nil () => nil ()
      } // end of [where]
      val lp2tcss = aux (p2atcst_complement p2tc1) where {
        fun aux (p2tcs: p2atcstlst):<cloptr1> labp2atcstlstlst =
        case+ p2tcs of
        | cons (p2tc, p2tcs) => begin
            cons (LABP2ATCSTLSTcons (l1, p2tc, lp2tcs1_any), aux p2tcs)
          end
        | nil () => lp2tcss
      } // end of [where]
    in
      lp2tcss
    end
  | LABP2ATCSTLSTnil () => nil ()
end // end of [labp2atcstlst_commplement]

(* ****** ****** *)

implement c2lau_pat_complement (c2l) = begin
  case+ c2l.c2lau_gua of
  | list_cons _ => let // the most conservative estimation
      fun aux {i,j:nat} .<i>.
        (p2ts: p2atlst i, res: p2atcstlst j): p2atcstlst (i+j) =
        case+ p2ts of
        | cons (_, p2ts) => aux (p2ts, cons (P2TCany (), res))
        | nil () => res
      val p2tcs = aux (c2l.c2lau_pat, nil ())
    in
      cons (p2tcs, nil ())
    end
  | list_nil _ => begin
      p2atcstlst_complement (p2atcstlst_of_p2atlst c2l.c2lau_pat)
    end
end // end of [c2lau_pat_complement]

(* ****** ****** *)

implement p2atcst_intersect_test (p2tc1, p2tc2) = begin
  case+ (p2tc1, p2tc2) of
  | (P2TCany (), _) => true
  | (_, P2TCany ()) => true
  | (P2TCbool b1, P2TCbool b2) => b1 = b2
  | (P2TCchar c1, P2TCchar c2) => c1 = c2
  | (P2TCcon (d2c1, p2tcs1), P2TCcon (d2c2, p2tcs2)) => begin
      if d2c1 = d2c2 then let
        val sgn = $Lst.list_length_compare (p2tcs1, p2tcs2)
        // run-time check needed for type-checking
        val () = assert_errmsg_bool1 (sgn = 0, "p2atcst_intersect_test")
      in
        p2atcstlst_intersect_test (p2tcs1, p2tcs2)
      end else begin
        false
      end
    end
  | (P2TCempty (), P2TCempty ()) => true
  | (P2TCint i1, P2TCint i2) => i1 = i2
  | (P2TCint i, P2TCintc xs) => aux (i, xs) where {
      fun aux (i: intinf_t, xs: intinfset_t): bool = case+ xs of
        | list_cons (x, xs) => begin
            if i < x then true else (if x < i then aux (i, xs) else false)
          end
        | list_nil () => true
    } // end of [where]
  | (P2TCintc xs, P2TCint i) => aux (xs, i) where {
      fun aux (xs: intinfset_t, i: intinf_t): bool = case+ xs of
        | list_cons (x, xs) => begin
            if i < x then true else (if x < i then aux (xs, i) else false)
          end
        | list_nil () => true
    } // end of [where]
  | (P2TCintc _, P2TCintc _) => true
  | (_, _) => false
end // end of [p2atcst_intersect_test]

implement p2atcstlst_intersect_test (p2tcs1, p2tcs2) =
  case+ p2tcs1 of
  | cons (p2tc1, p2tcs1) => let
      val+ cons (p2tc2, p2tcs2) = p2tcs2
    in
      if p2atcst_intersect_test (p2tc1, p2tc2) then
        p2atcstlst_intersect_test (p2tcs1, p2tcs2)
      else false
    end
  | nil () => true

(* ****** ****** *)

implement p2atcst_difference (p2tc1, p2tc2) =
  case+ (p2tc1, p2tc2) of
  | (_, P2TCany ()) => nil ()
  | (P2TCany (), _) => p2atcst_complement p2tc2
  | (P2TCbool b1, P2TCbool b2) => begin
      if b1 = b2 then nil () else cons (p2tc1, nil ())
    end
  | (P2TCchar c1, P2TCchar c2) => begin
      if c1 = c2 then nil () else cons (p2tc1, nil ())
    end
  | (P2TCcon (d2c1, p2tcs1), P2TCcon (d2c2, p2tcs2)) =>
    if d2c1 = d2c2 then let
      fun aux (d2c: d2con_t, p2tcss: p2atcstlstlst): p2atcstlst =
        case+ p2tcss of
        | p2tcs :: p2tcss => cons (P2TCcon (d2c, p2tcs), aux (d2c, p2tcss))
        | nil () => nil ()
      val sgn = $Lst.list_length_compare (p2tcs1, p2tcs2)
      // run-time check needed for type-checking
      val () = assert_errmsg_bool1 (sgn = 0, "p2atcst_difference: P2TCcon")
    in
      aux (d2c1, p2atcstlst_difference (p2tcs1, p2tcs2))
    end else begin
      cons (p2tc1, nil ())
    end
  | (P2TCempty (), P2TCempty ()) => nil ()
  | (P2TCint i1, P2TCint i2) => begin
      if i1 = i2 then nil () else cons (p2tc1, nil ())
    end
  | (P2TCstring s1, P2TCstring s2) => begin
      if s1 = s2 then nil () else cons (p2tc1, nil ())
    end
  | (P2TCintc xs, P2TCint x) => let
      var err: int = 0
      val xs = intinfset_add (xs, x, err)
    in
      if err > 0 then cons (p2tc1, nil ()) else cons (P2TCintc xs, nil ())
    end
  | (_, _) => begin
      prerr "Internal Error: p2atcst_difference";
      prerr_newline ();
      $Err.abort {p2atcstlst} ()
    end

implement p2atcstlst_difference {n} (p2tcs1, p2tcs2) = begin
  case+ p2tcs1 of
  | cons (p2tc1, p2tcs1) => let
      val+ cons (p2tc2, p2tcs2) = p2tcs2
      val p2tcss: List (list (p2atcst, n)) =
        aux (p2atcstlst_difference (p2tcs1, p2tcs2)) where {
        fun aux (p2tcss: List (list (p2atcst, n-1)))
          :<cloptr1> List (list (p2atcst, n)) = case+ p2tcss of
          | cons (p2tcs, p2tcss) => cons (p2tc1 :: p2tcs, aux p2tcss)
          | nil () => nil ()
      } // end of [where]
      val p2tcss: List (list (p2atcst, n)) =
        aux (p2atcst_difference (p2tc1, p2tc2)) where {
        fun aux (p2tcs: p2atcstlst)
          :<cloptr1> List (list (p2atcst, n)) = case+ p2tcs of
          | cons (p2tc, p2tcs) => cons (p2tc :: p2tcs1, aux p2tcs)
          | nil () => p2tcss
      } // end of [where]
    in
      p2tcss
    end
  | nil () => nil ()
end // end of [p2atcstlst_difference]

(* ****** ****** *)

(* end of [ats_patcst2.dats] *)
