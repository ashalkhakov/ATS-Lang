(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Power of Types!
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)

// July 2007
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

%{^

#include "prelude/CATS/array.cats"

%}

(* ****** ****** *)

staload "top.sats"

staload _ = "prelude/DATS/array.dats"
staload _ = "prelude/DATS/list_vt.dats" // DEBUG

staload "libats/lex/unicode.sats"

typedef intset = intset_t

// datatype for marked regular expressions
// augmented with EOF symbol
dataviewtype regex1_node =
  | REG1alt of (regex1, regex1)
  | REG1chars of (int(*position*), charset_t)
  | REG1nil (* for matching empty string *)
  | REG1end of int(*position*) // rule end
  | REG1opt of regex1
  | REG1plus of regex1
  | REG1seq of (regex1, regex1)
  | REG1star of regex1
(*
from "Scanner generation for modular regular grammars":

the predicate [nullable] determines whether a regular expression
can recognize the empty string

the function [firstpos] maps a marked regular expression to the
set of positions that can match the first symbol of an input string

the function [lastpos] maps a marked regular expression to the
set of positions that can matach the last symbol of an input string

the function [followpos] maps a position in a marked regular expression
[e] to the set of positions that can follow it, i.e., if [p] is a position
with symbol [c] and [p] matches the symbol [c] in some legal input string,
...ab..., then [b] will be matched by some position in [followpos(p)]

an accepting sequence of positions for a marked regular express [e] can
be defined as a sequence of positions p1,...,pn, such that p1 is in firstpos(e),
pn is in lastpos(e), and p(i+1) is in followpos(pi), i=1,...,n-1

from Berry/Sethi (4. fast algorithm):

Since each continuation is uniquely determined by an input symbol,
a regular expression with /n/ marked symbols will lead to an
automaton with /n+1/ states--a start state and a state for each
symbol. Instead of complete continuations, the second step of
the algorithm needs only the set of leading symbols in strings
generated by the continuations.

Definition 4.1
first(E) = {a | av in L(E)}
follow(E,a) = {b | uabv in L(E)}
*)
where regex1: viewtype = '{
  node= regex1_node
  // [true] iff epsilon is in language of [node]
, null= bool
  // set of positions in [node] that correspond to the first
  // symbol of at least one string in language of [node]
, fstpos= intset
  // set of positions in [node] that correspond to the last
  // symbol of at least one string in language of [node]
, lstpos= intset
}

(* ****** ****** *)

exception Fatal

(* ****** ****** *)

// for constant-time access to character set or irule
// of a tree node by its position:
// - end of rule is assigned an irule (action to perform
//   once the prefix of the input string matched the rule)
// - character sets are put as-is
datatype CSI = CSI_cs of charset_t | CSI_i of int(*irule*)
viewtypedef CSIlst (n:int) = list_vt (CSI, n)

viewtypedef T = [n:nat] @{ lst= CSIlst n, len= int n }

(* ****** ****** *)

fn regex1_alt (
  r1: regex1, r2: regex1
) : regex1 = let
  val null: bool = if r1.null then true else r2.null
  val fstpos = r1.fstpos + r2.fstpos
  val lstpos = r1.lstpos + r2.lstpos
in '{
  node= REG1alt (r1, r2)
, null= null
, fstpos= fstpos
, lstpos= lstpos
} end // end of [regex1_alt]

//

fn regex1_chars (
  x: &T, cs: charset_t
) : regex1 = let
  val x_len = x.len
  val () = x.len := x_len + 1
  val x_lst = x.lst
  val () = x.lst := list_vt_cons (CSI_cs cs, x_lst)
in '{
  node= REG1chars (x_len, cs)
, null= false
, fstpos = intset_singleton x_len
, lstpos = intset_singleton x_len
} end // end of [regex1_chars]

//

fn regex1_end (
  x: &T, irule: int
) : regex1 = let
  val x_len = x.len
  val () = x.len := x_len + 1
  val x_lst = x.lst
  val () = x.lst := list_vt_cons (CSI_i irule, x_lst)
in '{
  node= REG1end x_len
, null= false
, fstpos = intset_singleton x_len
, lstpos = intset_singleton x_len
} end // end of [regex1_end]

//

fn regex1_nil (): regex1 = '{
  node= REG1nil ()
, null= true
, fstpos = intset_nil
, lstpos = intset_nil
}

//

fn regex1_null (): regex1 = '{
  node= REG1nil ()
, null= false
, fstpos = intset_nil
, lstpos = intset_nil
}

//

fn regex1_seq (
  r1: regex1, r2: regex1
) : regex1 = let
  val null: bool = if r1.null then r2.null else false
  val fstpos: intset =
    if r1.null then r1.fstpos + r2.fstpos else r1.fstpos
  val lstpos: intset =
    if r2.null then r1.lstpos + r2.lstpos else r2.lstpos
in '{
  node= REG1seq (r1, r2)
, null= null
, fstpos= fstpos
, lstpos= lstpos
} end // end of [regex1_seq]

//

fn regex1_opt
  (r: regex1): regex1 = let
  val fstpos = r.fstpos and lstpos = r.lstpos
in '{
  node= REG1opt r
, null= true
, fstpos= fstpos
, lstpos= lstpos
} end // end of [regex1_opt]

fn regex1_plus
  (r: regex1): regex1 = let
  val null = r.null and fstpos = r.fstpos and lstpos = r.lstpos
in '{
  node= REG1plus r
, null= null
, fstpos= fstpos
, lstpos= lstpos
} end // end of [regex1_plus]

fn regex1_star
  (r: regex1): regex1 = let
  val fstpos = r.fstpos and lstpos = r.lstpos
in '{
  node= REG1star r
, null= true
, fstpos= fstpos
, lstpos= lstpos
} end // end of [regex1_star]

(* ****** ****** *)

fn redef_find (
  env: !redef, id0: string
) : Option_vt regex = let
  fun loop (env: !redef, id0: string): Option_vt regex = begin
    case+ env of
    | redef_cons (id, r, !p_env) =>
        if (id0 = id) then (fold@ env; Some_vt {regex} (r))
        else let
          val res = loop (!p_env, id0) in
          fold@ env; res
        end // end of [if]
    | redef_nil () => (fold@ env; None_vt {regex} ())
  end // end of [loop]
in
  loop (env, id0)
end // end of [redef_find]

(* ****** ****** *)

fn array_of_CSIlst {n:nat} (
  lst: CSIlst n, n: int n
) :<> [l:addr] (
  free_gc_v (CSI?, n, l), array_v (CSI, n, l) | ptr l
) = let
  // TODO: move into an auxiliary function template by generalizing
  // CSI to a type parameter, please see also
  // [string32_make_charlst_rev_int] in [token.dats]
  val nsz = size1_of_int1 n
  val tsz = sizeof<CSI>
  val (pf_gc, pf_arr | p_arr) = array_ptr_alloc_tsz {CSI} (nsz, tsz)
  val (pfmul | ofs) = mul2_size1_size1 (nsz, tsz)
  fun loop {m,n:nat} {ofs:int} {l:addr} .<m>. (
    pfmul: MUL (m, sizeof CSI, ofs)
  , pf1: array_v (CSI?, m, l)
  , pf2: array_v (CSI, n, l+ofs)
  | p: ptr (l+ofs), lst: CSIlst m
  ) :<> (array_v (CSI, m+n, l) | void) = let
    prval () = mul_nat_nat_nat (pfmul)
  in
    case+ lst of
    | ~list_vt_nil () => let
        prval MULbas () = pfmul
        prval () = array_v_unnil {CSI?} (pf1)
      in
        (pf2 | ())
      end // end of [let]
    | ~list_vt_cons (x, lst) => let
        prval (pf11, pf1at) = array_v_unextend {CSI?} (pfmul, pf1)
        val p = p-sizeof<CSI>
        val () = !p := x
        prval () = mul_nat_nat_nat (pfmul)
        prval MULind pf1mul = pfmul
        prval pf22 = array_v_cons {CSI} (pf1at, pf2)
      in
        loop (pf1mul, pf11, pf22 | p, lst)
      end // end of [let]
  end // end of [loop]
  val (pf1_arr | ()) = loop (pfmul, pf_arr, array_v_nil {CSI} () | p_arr+ofs, lst)
in
  (pf_gc, pf1_arr | p_arr)
end // end of [array_of_CSIlst]

(* ****** ****** *)

fun regex_mark_str
  {i,l:nat | i <= l} .<l-i>. (
  env: !redef, x0: &T, i: size_t i, l: size_t l, s: string32_vt l, r1e: regex1
) : regex1 = begin
  if i < l then let
    val cs = charset_singleton (s[i])
    val r1e = regex1_seq (r1e, regex1_chars (x0, cs))
  in
    regex_mark_str (env, x0, i+1, l, s, r1e)
  end else r1e
end // end of [regex_mark_str]

(* ****** ****** *)

fun regex_mark (
  env: !redef, x0: &T, r0e: regex
) : regex1 = begin case+ r0e of
  | REGalt (r0e1, r0e2) => let
      val r1e1 = regex_mark (env, x0, r0e1)
      val r1e2 = regex_mark (env, x0, r0e2)
    in
      regex1_alt (r1e1, r1e2)
    end
  | REGchars (cs) => regex1_chars (x0, cs)
  | REGnil () => regex1_nil ()
  | REGid id => begin
      case+ redef_find (env, id) of
        | ~Some_vt r0e0 => regex_mark (env, x0, r0e0)
        | ~None_vt () => begin
          prerrf ("Undefined identifier: %s\n", @(id));
          $raise Fatal ()
        end
    end (* end of [REGid] *)
  | REGopt (r0e0) => regex1_opt (regex_mark (env, x0, r0e0))
  | REGplus (r0e0) => regex1_plus (regex_mark (env, x0, r0e0))
  | REGrep (r0e0, i) =>
      if i > 0 then let
        val r1e0 = regex_mark (env, x0, r0e0)
      in
        regex_mark_rep (env, x0, i-1, r0e0, r1e0)
      end else regex1_nil ()
    // end of [REGrep]
  | REGseq (r0e1, r0e2) => let
      val r1e1 = regex_mark (env, x0, r0e1)
      val r1e2 = regex_mark (env, x0, r0e2)
    in
      regex1_seq (r1e1, r1e2)
    end // end of [REGseq]
  | REGstar (r0e0) => regex1_star (regex_mark (env, x0, r0e0))
  | REGstr @(n, str) => regex_mark_str (env, x0, 0, n, str, regex1_nil ())
end // end of [regex_mark]

and regex_mark_rep (
  env: !redef, x0: &T, i: int, r0e: regex, r1e: regex1
) : regex1 =
  if i > 0 then let
    val r1e' = regex_mark (env, x0, r0e)
  in
    regex_mark_rep (env, x0, i-1, r0e, regex1_seq (r1e, r1e'))
  end else r1e // end of [if]
// end of [regex_mark_rep]

(* ****** ****** *)

// for a position p, followpos(p) is the set of positions q such that:
// - assuming x = a_1a_2...a_n in L(R#) such that
// - for some i, there is a way to explain membership of x in in L(R#)
//   by matching a_i to position p and a_i+1 to position q
//
// compute followpos for every position in the tree
fun followpos
  {n:nat} (
  n0: int n, r: regex1
) : [l:addr] (
  free_gc_v (intset?, n, l), array_v (intset, n, l) | ptr l
) = let
  fun aux {l:addr}
    (pf: !array_v (intset, n, l) | A: ptr l, n0: int n, r: regex1): void = let
    val r_node = r.node
    prval () = cleanup_top {regex1} (__cast r) where {
      typedef regex1clr = '{
        node= regex1_node?, null= bool
      , fstpos= intset, lstpos= intset
      } // end of [regex1clr]
      extern
      castfn __cast (x: regex1clr):<> regex1?
    }
  in
    aux_node (pf | A, n0, r_node)
  end // end of [aux]

  and aux_node{l:addr}
    (pf: !array_v (intset, n, l) | A: ptr l, n0: int n, r_node: regex1_node)
    : void =
    case+ r_node of
    | ~REG1alt (r1, r2) => begin
        aux (pf | A, n0, r1); aux (pf | A, n0, r2);
      end // end of [REG1alt]
    | ~REG1chars (n, cs) => ()
    | ~REG1end n => ()
    | ~REG1nil () => ()
    | ~REG1opt r0 => aux (pf | A, n0, r0)
    | ~REG1plus r0 => let
        val r0_fstpos = r0.fstpos
        var !p_f = @lam (
          pf: !array_v (intset, n, l) | i: int
        ) : void =<clo1> let
          val i = int1_of_int i
(*
          val () = prerrf ("followpos: i = %i and n = %i\n", @(i,n0))
*)
          val () = assert (0 <= i)
          val () = assert (i < n0)
        in
          A[i] := A[i] + r0_fstpos
        end // end of [f]
        val () = foreach_intset (pf | !p_f, r0.lstpos)
      in
        aux (pf | A, n0, r0)
      end // end of [REG1plus]
    | ~REG1seq (r1, r2) => let
        val r2_fstpos = r2.fstpos
        var !p_f = @lam (
          pf: !array_v (intset, n, l) | i: int
        ) : void =<clo1> let
          val i = int1_of i
(*
          val () = prerrf ("followpos: i = %i and n = %i\n", @(i,n0))
*)
          val () = assert (0 <= i)
          val () = assert (i < n0)
        in
          A[i] := A[i] + r2_fstpos
        end // end of [f]
        val () = foreach_intset (pf | !p_f, r1.lstpos)
      in
        aux (pf | A, n0, r1); aux (pf | A, n0, r2)
      end // end of [REG1seq]
    | ~REG1star r0 => let
        val r0_fstpos = r0.fstpos
        var !p_f = @lam (
          pf: !array_v (intset, n, l) | i: int
        ) : void =<clo1> let
          val i = int1_of i
(*
          val () = prerrf ("followpos: i = %i and n = %i\n", @(i,n0))
*)
          val () = assert (0 <= i)
          val () = assert (i < n0)
        in
          A[i] := A[i] + r0_fstpos
        end // end of [f]
        val () = foreach_intset (pf | !p_f, r0.lstpos)
      in
        aux (pf | A, n0, r0)
      end // end of [REG1star]
  // end of [aux] and [aux_node]
  val n0_sz = size1_of_int1 n0
  val tsz = sizeof<intset>
  val (pf_gc, pf_arr | p_arr) =
    array_ptr_alloc_tsz {intset} (n0_sz, tsz)
  val () = begin
    array_ptr_initialize_elt_tsz {intset} (!p_arr, n0_sz, ini, tsz)
  end where {
    var ini: intset = intset_nil
  } // end of [val]
  val () = aux (pf_arr | p_arr, n0, r)
in
  (pf_gc, pf_arr | p_arr)
end // end of [followpos]

(* ****** ****** *)

fn rules_mark (
  env: !redef, x0: &T, rls: !rules
) : regex1 = let
  fun aux (
    env: !redef, x0: &T, irule: int, rls: !rules
  ) : regex1 =
    case+ rls of
    | rules_cons (r0e, act, !p_rls) => let
        val r1e1 = regex_mark (env, x0, r0e)
        val r1e2 = regex1_end (x0, irule)
        val r1e_seq = regex1_seq (r1e1, r1e2)
        val res = aux (env, x0, irule+1, !p_rls)
      in
        fold@ rls; regex1_alt (r1e_seq, res)
      end
    | rules_nil () => (fold@ rls; regex1_null ())
  // end of [aux]
in
   aux (env, x0, 1, rls) // irule starts from 1!
end (* rules_mark *)

(* ****** ****** *)
// construction of a DFA from regular expression

viewtypedef acclst = List_vt @(int(*state*), int(*irule*))

dataviewtype
intlst = intlst_nil | intlst_cons of (int, intlst)
// end of [dataviewtype]

dataviewtype statelst = // list of untagged DFA states
  | statelst_nil of () | statelst_cons of (intset(*set of positions*), statelst)
// end of [statelst]

viewtypedef
trans_vt = List_vt @(charset_t, int) // all charsets are pairwise disjoint

dataviewtype
translst (int) = // list of transitions
  | translst_nil (0) of ()
    // one transition (arc in the DFA transition function graph)
  | {n:nat} translst_cons (n+1) of (
      int(*tag of state*)
    , trans_vt
    , translst n
    ) // end of [translst_cons]
// end of [translst]

viewtypedef Translst = [n:nat] translst n

(* ****** ****** *)

// returns union of followpos(p) for all p in st,
// provided that p corresponds to a character set cs
// such that c is in cs
fn transition_charset
  {n:nat} {l_csi,l_pos:addr} (
  pf1: !array_v (CSI, n, l_csi)
, pf2: !array_v (intset, n, l_pos)
| A_csi: ptr l_csi
, A_pos: ptr l_pos
, n: int n, st: intset, cs: charset_t
) : intset = let
(*
  val () = prerr ("transition_charset:\n")
  val () = let
      val (pf_stdout | ()) = stdout_view_get ()
    in
      fprint_charset (file_mode_lte_w_w | !stdout, cs);
      stdout_view_set (pf_stdout | (*empty*))
    end // end of [val]
*)
  var st_res: intset = intset_nil
  viewdef V = (
    array_v (CSI, n, l_csi), array_v (intset, n, l_pos), intset @ st_res
  ) // end of [viewdef]
  var !p_f = @lam (
    pf: !V | i: int
  ) : void =<clo1> let
    prval (pf1, pf2, pf3) = pf
    val i = int1_of_int i
(*
    val () = prerrf ("transition_char: i = %i and n = %i\n", @(i, n))
    val () = (prerr "transition_char: st_res = "; prerr_intset st_res; prerr_newline ())
*)
    val () = assert (0 <= i); val () = assert (i < n)
    val () = (case+ A_csi[i] of
      | CSI_cs cs1 => begin
          if charset_is_joint (cs, cs1) then st_res := st_res + A_pos[i]
        end // end of [CSI_cs]
      | _ => ()
    ) : void // end of [val]
(*
    val () = (prerr "transition_char: st_res = "; prerr_intset st_res; prerr_newline ())
*)
  in
     pf := (pf1, pf2, pf3)
  end // end of [!p_f]
  prval pf = (pf1, pf2, view@ st_res)
  val () = foreach_intset {V} (pf | !p_f, st)
  prval () = (pf1 := pf.0; pf2 := pf.1; view@ st_res := pf.2)
in
  st_res
end // end of [transition_charset]

fun transition_one {n:nat} {l_csi,l_pos:addr}
  (pf1: !array_v (CSI, n, l_csi),
   pf2: !array_v (intset, n, l_pos) |
   A_csi: ptr l_csi, A_pos: ptr l_pos, n: int n,
   nst_r: &int, sts: &states_t(*tagged states*), stlst: &statelst(*untagged states*),
   st0: intset, c: List_vt charset_t, ns: List_vt @(charset_t, int)(*transitions*)): List_vt @(charset_t, int) =
  case+ c of
  | ~list_vt_cons (cs, c) => let
      val st = transition_charset (pf1, pf2 | A_csi, A_pos, n, st0, cs)
      val nst = states_find (sts, st)
      val nst =
        if nst < 0 then let // not found
          val nst = nst_r
          val () = nst_r := nst+1
          val () = states_insert (sts, nst, st)
          val () = stlst := statelst_cons (st, stlst)
        in
          nst
        end else begin
          nst (* found *)
        end // end of [if]
      val ns = list_vt_cons (@(cs, nst), ns)
    in
      transition_one (
        pf1, pf2 | A_csi, A_pos, n, nst_r, sts, stlst, st0, c, ns
      ) // end of [transition_one]
    end // end of [let]
  | ~list_vt_nil () => ns
// end of [transition_one]

// list of all non-empty character sets associated with
// positions in [st]
fun charsetlst {n:nat} {l_csi:addr} (
  pf1: !array_v (CSI, n, l_csi)
| A_csi: ptr l_csi, n: int n
, st: intset
) : List_vt charset_t = let
  var res = list_vt_nil ()
  viewdef V = [i:nat] (array_v (CSI, n, l_csi), list_vt (charset_t, i) @ res)
  var !p_f = @lam (
    pf: !V | i: int
  ) : void =<clo1> let
    prval (pf1, pf2) = pf
    val i = int1_of_int i
    val () = assert (0 <= i); val () = assert (i < n)
  in
    case+ A_csi[i] of
      | CSI_cs cs => (res := list_vt_cons (cs, res); pf := (pf1, pf2))
      | _ => pf := (pf1, pf2)
    // end of [case+]
  end // end of [!p_f]
  prval pf = (pf1, view@ res)
  val () = foreach_intset {V} (pf | !p_f, st)
  prval () = (pf1 := pf.0; view@ res := pf.1)
in
  res
end // end of [charsetlst]

(*
fun prerr_charsetlst (csl: !List_vt charset_t): void = let
  fun f (cs: &charset_t): void = (prerr_charset cs; prerr ";")
in
  prerr "charsetlst(";
  list_vt_foreach_fun<charset_t> (csl, f);
  prerr ")"
end // end of [prerr_charsetlst]
*)

// given a list of possibly overlapping charsets,
// yields a sorted, duplicate-free list of pairwise disjoint
// charsets
fun split (ys: List_vt charset_t): List_vt charset_t = let
  fun loop (xs: List_vt charset_t, cs: charset_t): List_vt charset_t =
    case+ xs of
    | list_vt_nil () => (fold@ xs; xs)
    | ~list_vt_cons (cs1, xs1) => let
        val a = charset_intersect (cs1, cs)
      in
        if ~charset_is_nil a then let
          val b = charset_difference (cs1, cs)
          val c = charset_difference (cs, cs1)
          var res = list_vt_cons (a, loop (xs1, cs))
        in
          if :(res: List_vt charset_t) => ~charset_is_nil b then
            res := list_vt_cons{charset_t} (b, res);
          if :(res: List_vt charset_t) => ~charset_is_nil c then
            res := list_vt_cons (c, res);
          res
        end else begin
          list_vt_cons (cs1, loop (xs1, cs))
        end // end of [if]
      end // end of [loop]
  fun aux (
    all: charset_t, xs: List_vt charset_t, ys: List_vt charset_t
  ) : List_vt charset_t = case+ ys of
    | ~list_vt_nil () => xs
    | ~list_vt_cons (y, ys1) =>
      if charset_is_joint (all, y) then let
        val xs1 = loop (xs, y)
      in
        aux (charset_union (y, all), xs1, ys1)
      end else begin
        aux (charset_union (y, all), list_vt_cons (y, xs), ys1)
      end // end of [aux]
  // remove duplicates
  fun aux_dup {n:nat} .<n>. (xs: list_vt (charset_t, n)): List_vt charset_t = case+ xs of
    | list_vt_nil () => (fold@ xs; xs)
    | ~list_vt_cons (cs, xs1) => aux_dup1 (cs, xs1)
  and aux_dup1 {n:nat} .<n>. (cs: charset_t, xs: list_vt (charset_t, n)): List_vt charset_t =
    case+ xs of
    | list_vt_nil () => (fold@ xs; list_vt_cons (cs, xs))
    | ~list_vt_cons (cs1, xs1) => begin
        if compare_charset_charset (cs, cs1) = 0 then aux_dup1 (cs, xs1)
        else list_vt_cons (cs, aux_dup1 (cs1, xs1))
      end // end of [aux_dup1]
in
  case+ ys of
  | ~list_vt_cons (y, ys1) => let
      var xs = aux (y, list_vt_sing y, ys1)
      var !p_clo = @lam (x: &charset_t, y: &charset_t): int =<clo>
        $effmask_all (compare_charset_charset (x, y))
      // end of [var]
      val xs = list_vt_mergesort (xs, !p_clo)
    in
      aux_dup xs
    end // end of [let]
  | _ => ys // ys is nil!
end // end of [split]

fun transition_all {n:nat} {l_csi,l_pos:addr}
  (pf1: !array_v (CSI, n, l_csi),
   pf2: !array_v (intset, n, l_pos) |
   A_csi: ptr l_csi, A_pos: ptr l_pos, n: int n,
   nst_r: &int, sts: &states_t, stlst: statelst,
   ans: Translst): Translst = begin
  case+ stlst of
  | ~statelst_cons (st, stlst) => let
      val nst = states_find (sts, st)
      var stlst = stlst
      //
      var csets = charsetlst (pf1 | A_csi, n, st)
      // FIXME: necessary?
//      val () = csets := list_vt_cons (charset_eof, csets)
      val csets = split csets
      val ns = transition_one (
        pf1, pf2
      | A_csi, A_pos, n, nst_r, sts, stlst, st, csets, list_vt_nil ()
      ) // transition_one
    in
      transition_all (
        pf1, pf2
      | A_csi, A_pos, n, nst_r, sts, stlst, translst_cons (nst, ns, ans)
      ) // end of [transition_all]
    end
  | ~statelst_nil () => ans
end // end of [transition_all]

(* ****** ****** *)

fun accept_one
  {n:nat} {l_csi:addr} (
  pf1: !array_v (CSI, n, l_csi)
| A_csi: ptr l_csi, n: int n, st: intset
) : int = let
  var irule = (0: int)
  viewdef V = (array_v (CSI, n, l_csi), int @ irule)
  var !p_f = @lam
    (pf: !V | nst: int): void =<clo1> let
    prval (pf1, pf2) = pf
    val nst = int1_of_int nst
(*
    val () = prerrf ("accept_one: i = %i and n = %i\n", @(nst,n))
*)
    val () = assert (0 <= nst)
    val () = assert (nst < n)
    val () = case+ A_csi[nst] of
      | CSI_i i => begin
          if irule > 0 then (if i < irule then irule := i) else (irule := i)
        end // end of [CSI_i]
      | _ => ()
    // end of [val]
  in
    pf := (pf1, pf2)
  end // end of [!p_f]
  prval pf = (pf1, view@ irule)
  val () = foreach_intset {V} (pf | !p_f, st)
  prval () = pf1 := pf.0; prval () = view@ irule := pf.1
in
  irule
end // end of [accept_one]

fun accept_all
  {n:nat} {l_csi:addr} (
  pf1: !array_v (CSI, n, l_csi)
| A_csi: ptr l_csi, n: int n, sts: states_t
) : acclst = let
  var ans: acclst = list_vt_nil ()
  viewdef V = (array_v (CSI, n, l_csi), acclst @ ans)
  var !p_f = @lam (
    pf: !V | tag: int, st: intset
  ) : void =<clo1> let
    prval (pf1, pf2) = pf
    val irule = accept_one (pf1 | A_csi, n, st)
  in
    if irule > 0 then (ans := list_vt_cons (@(tag, irule), ans); pf := (pf1, pf2))
    else pf := (pf1, pf2)
  end // end of [f]
  prval pf = (pf1, view@ ans)
  val () = states_foreach_and_free {V} (pf | !p_f, sts)
  prval () = pf1 := pf.0; prval () = view@ ans := pf.1
in
  ans
end // end of [accept_all]

(* ****** ****** *)

extern
fun translst_length
  {n:nat} (lst: !translst n): int n = "translst_length"
// end of [translst_length]

implement
translst_length (lst) = let
  fun loop {i,j:nat}
    (lst: !translst i, j: int j): int(i+j) =
    case+ lst of
    | translst_cons (_, _, !lst_r) =>
      let val n = loop (!lst_r, j+1) in fold@ lst; n end
    | translst_nil () => (fold@ lst; j)
  // end of [loop]
in
  loop (lst, 0)
end // end of [translst_length]

(* ****** ****** *)

extern
fun translst_uncons {n:pos} (
  lst: &translst n >> translst (n-1), tag: &(int?) >> int, ns: &trans_vt? >> trans_vt
) : void = "translst_uncons"

implement
translst_uncons
  (lst, tag, ns) = let
  val+ ~translst_cons (tag_v, ns_v, lst_v) = lst
in
  tag := tag_v; ns := ns_v; lst := lst_v
end // end of [translst_uncons]

(* ****** ****** *)

extern
fun fprint_irule
  {m:file_mode} (
  pf_mod: file_mode_lte (m, w) | fil: &FILE m, i: int
) : void = "fprint_irule"

implement
fprint_irule (pf_mod | fil, i) = fprint_int (pf_mod | fil, i)

extern
fun fprint_state
  {m:file_mode} (
  pf_mod: file_mode_lte (m, w) | fil: &FILE m, n: int
) : void = "fprint_state"

implement
fprint_state (pf_mod | fil, n) = fprint_int (pf_mod | fil, n)

extern
fun fprint_trans_vt {m:file_mode} (
  pf_mod: file_mode_lte (m, w) | fil: &FILE m, ns: trans_vt
) : void = "fprint_trans_vt"
//
// printing as well as freeing
//
implement
fprint_trans_vt {m}
  (pf_mod | fil, ns) = let
  fun fprint_int_ATS (fil: &FILE m, i: int): void =
    if i < 0 then (fprint_char (pf_mod | fil, '~'); fprint_int (pf_mod | fil, abs i))
    else fprint_int (pf_mod | fil, i)
  // end of [fprint_int_ATS]
  fun loop {n:nat} (
    fil: &FILE m, n: int n, xs: list_vt (@(int, int, int), n)
  ) : void = case+ xs of
    | ~list_vt_cons (@(c1, c2, tag), xs) => (
        fprint_string (pf_mod | fil, "@(");
        fprint_int_ATS (fil, c1);
        fprint_string (pf_mod | fil, ", ");
        fprint_int_ATS (fil, c2);
        fprint_string (pf_mod | fil, ", ");
        fprint_int (pf_mod | fil, tag);
        fprint_char (pf_mod | fil, ')');
        if list_vt_is_cons xs then fprint_char (pf_mod | fil, ',');
        loop (fil, n-1, xs)
      ) // end of [list_vt_cons]
    | ~list_vt_nil () => ()
  // end of [loop]
  // TODO: sort entries prior to printing?
  // safe to merge entries, because all character sets are pairwise disjoint
  fun aux (fil: &FILE m, ns: trans_vt) : void =
    case+ ns of
    | ~list_vt_cons (@(cs, tag), ns) => let
        val (n1, cs1) = list_vt_of_charset (cs, tag)
      in
        loop (fil, n1, cs1);
        if list_vt_is_cons ns then fprint_char (pf_mod | fil, ',');
        aux (fil, ns)
      end // end of [let]
    | ~list_vt_nil () => ()
  // end of [aux]
in
  fprint_string (pf_mod | fil, "$lst {@(int,int,int)} (");
  aux (fil, ns);
  fprint_char (pf_mod | fil, ')')
end // end of [fprint_trans_vt]

(* ****** ****** *)

extern typedef "instlst" = intlst
extern typedef "trans_vt" = trans_vt

// printing as well as freeing
extern
fun fprint_translst {m:file_mode} (
  pf_mod: file_mode_lte (m, w) | fil: &FILE m, lst: Translst
) : void = "fprint_translst"

%{

ats_void_type
fprint_translst (
  ats_ptr_type fil
, ats_ptr_type lst
) {
  int i, n, tag ; trans_vt ns, *A;

  n = translst_length (lst) ;

  // this is really just radix sorting
  A = ats_malloc_ngc((n+1)*sizeof(trans_vt)) ; // A[0] is unused

  for (i = 1; i <= n; ++i) {
    translst_uncons (&lst, &tag, &ns) ;
/*
    fprintf (stderr, "fprint_translst: i = %i and tag = %i\n", i, tag);
*/
    A[tag] = ns ;
  } // end of [for]

  for (i = 1; i <= n; ++i) {
    fprint_trans_vt (fil, A[i]) ;
    if (i+1 <= n) {
      fprintf ((FILE*)fil, ", ") ;
    }
    fprintf ((FILE*)fil, "\n") ;
  } // end of [for]

  ats_free_ngc(A) ;

  return ;
} // end of [fprint_translst]

%}
//
// HX: freeing as well
//
fun fprint_acclst {m:file_mode} (
  pf_mod: file_mode_lte (m, w) | fil: &FILE m, lst: acclst, ntot: int
) : void = let
  // print in sparse form
  fun loop1 (
    fil: &FILE m, ntot: int, lst: acclst, n0: int, nstate: int, irule: int
  ) : void =
    if n0 < nstate then begin
      fprint_int (pf_mod | fil, 0);
      if n0+1 < ntot then fprint_char (pf_mod | fil, ',');
      loop1 (fil, ntot, lst, n0+1, nstate, irule)
    end else begin // n0 = nstate
      fprint_int (pf_mod | fil, irule);
      if list_vt_is_cons lst then fprint_char (pf_mod | fil, ',');
      loop2 (fil, ntot, lst, nstate+1)
    end // end of [loop1]

  and loop2 (fil: &FILE m, ntot: int, lst: acclst, n0: int): void =
    case+ lst of
    | ~list_vt_cons (@(nstate, irule), lst) => loop1 (fil, ntot, lst, n0, nstate, irule)
    | ~list_vt_nil () => ()
  // end of [loop2]
  var !p_clo = @lam (x: &(int, int), y: &(int, int)): int =<clo>
    compare (x.0, y.0)
  // end of [var]
  val xs = list_vt_mergesort (lst, !p_clo)
in
  loop2 (fil, ntot, xs, 0(*incl. error state*));
  fprint_newline (pf_mod | fil)
end // end of [fprint_acclst]

(* ****** ****** *)

fun fprint_header
  {m:file_mode} (
  pf_mod: file_mode_lte (m, w)
| fil: &FILE m, id: string, arg: string
) : void = let
  val isreent = atslex_get_reentrant ()
in
  fprintf (pf_mod | fil, "implement %s (", @(id));
  if isreent then begin
    fprint_string (pf_mod | fil, "mylexbuf");
    if string_isnot_empty arg then fprint_string (pf_mod | fil, ", ");
  end; // end of [if]
  fprintf (pf_mod | fil, "%s) =\n", @(arg));
  if ~isreent then begin
    fprint_string (pf_mod | fil, "case+ lexing_engine (__")
  end else begin
    fprint_string (pf_mod | fil, "case+ lexing_engine_lexbuf (mylexbuf, __");
  end; // end of [if]
  fprint_string (pf_mod | fil, id);
  fprint_string (pf_mod | fil, "_transition_table, __");
  fprint_string (pf_mod | fil, id);
  fprint_string (pf_mod | fil, "_accept_table) of");
  fprint_newline (pf_mod | fil)
end // end of [fprint_header]

(* ****** ****** *)

fun fprint_rules
  {m:file_mode} (
  pf_mod: file_mode_lte (m, w)
| fil: &FILE m, id: string, arg: string, rls: !rules
) : void = let
  val isreent = atslex_get_reentrant ()
  fun loop (
    fil: &FILE m, rls: !rules, irule: int
  ) : void =
    case+ rls of
    | rules_cons (r, code, !p_rls) => begin
        fprintf (pf_mod | fil, "  | %i => ( %s )\n", @(irule, code));
        loop (fil, !p_rls, irule + 1);
        fold@ rls
      end // end of [rules_cons]
    | rules_nil () => fold@ rls
  // end of [loop]
in
  loop (fil, rls, 1);
  fprintf (pf_mod | fil, "  | _ => %s_lexing_error (", @(id));
  if isreent then begin
    fprint_string (pf_mod | fil, "mylexbuf");
    if string_isnot_empty arg then fprint_string (pf_mod | fil, ", ");
  end; // end of [if]
  fprintf (pf_mod | fil, "%s)\n", @(arg))
end // end of [fprint_rules]

(* ****** ****** *)

extern
fun fprint_DFA
  {m:file_mode} (
  pf_mod: file_mode_lte (m, w)
| fil: &FILE m, env: !redef, id: string, arg: string, rls: !rules
) : void // end of [fprint_DFA]

val regex_eof = REGchars charset_eof
val regex_base_char = REGchars charset_base_char
val regex_ideographic = REGchars charset_ideographic
val regex_combining_char = REGchars charset_combining_char
val regex_digit = REGchars charset_digit
val regex_extender = REGchars charset_extender
val regex_blank = REGchars charset_blank
val regex_letter = REGchars charset_letter
val regex_tr8876_ident_char = REGchars charset_tr8876_ident_char

implement
fprint_DFA (
  pf_mod | fil, env, id, arg, rls
) = let
//
var x0: T = @{ lst= list_vt_nil (), len= 0 }
val root_regex1 = rules_mark (env, x0, rls)
val root_fstpos = root_regex1.fstpos
//
(*
val () = fprint_string (pf_mod | fil, "root_fstpos = ")
val () = fprint_intset (pf_mod | fil, root_fstpos)
val () = fprint_newline (pf_mod | fil)
*)
//
val npos = x0.len
//
val (pf_csi_gc, pf_csi | A_csi) = array_of_CSIlst (x0.lst, npos)
val (pf_pos_gc, pf_pos | A_pos) = followpos (npos, root_regex1)
//
var nst_r: int // uninitialized
var sts = states_nil ()
//
// state 0 is special: it is the error state
val _ = states_insert (sts, 0, intset_nil)
// state 1 is special: it is the start state
val _ = states_insert (sts, 1, root_fstpos)
val () = nst_r := (2: int)
//
var stlst = statelst_nil ()
val () = stlst := statelst_cons (root_fstpos, stlst)
//
val dfa_transtbl = transition_all (
  pf_csi, pf_pos | A_csi, A_pos, npos, nst_r, sts, stlst, translst_nil ()
) // end of [val]
//
val () = array_ptr_free {intset?} (pf_pos_gc, pf_pos | A_pos)
//
val dfa_acctbl = accept_all (pf_csi | A_csi, npos, sts: states_t)
val dfa_nfinal = list_vt_length dfa_acctbl (* number of final states *)
//
val () = array_ptr_free {CSI?} (pf_csi_gc, pf_csi | A_csi)
//
val dfa_nstate = nst_r - 1
//
in
//
// transition table
//
  fprint_string (pf_mod | fil, "local
val (pf_gc, pf_arr | p_arr, n) = $arrpsz(\n");
  fprint_translst (pf_mod | fil, dfa_transtbl);
  fprint_string (pf_mod | fil, ") // end of [val]\n");
  fprintf (pf_mod | fil, "prval () = free_gc_elim pf_gc
in
val __%s_nstate = n
val __%s_transition_table = transition_table_of_array (pf_arr | p_arr, int1_of_size1 n)
end\n\n", @(id, id));
//
// accepting states
//
  fprint_string (pf_mod | fil, "local\n
val (pf_gc, pf_arr | p_arr, n) = $arrpsz(\n");
  // end of [fprint_string ...]
  fprint_acclst (pf_mod | fil, dfa_acctbl, dfa_nfinal);
  fprint_string (pf_mod | fil, ") // end of [val]\n");
  fprintf (pf_mod | fil, "prval () = free_gc_elim pf_gc
in
val __%s_accept_table = accept_table_of_array (pf_arr | p_arr, int1_of_size1 n)
end\n\n", @(id));
//
// function for lexical analysis
//
  fprint_header (pf_mod | fil, id, arg);
  fprint_rules (pf_mod | fil, id, arg, rls);
  fprint_newline (pf_mod | fil);
//
end // end of [fprint_DFA]

(* ****** ****** *)

implement
fprint_lexfns {m}
  (pf_mod | fil, env, lfs) = let
  fun loop (
    fil: &FILE m, env: &redef, lfs: !lexfns
  ) : void =
    case+ lfs of
    | lexfns_cons (id, arg, !p_rls, !p_lfs) => begin
        fprint_DFA (pf_mod | fil, env, id, arg, !p_rls);
        loop (fil, env, !p_lfs);
        fold@ lfs
      end // end of [lexfns_cons]
    | lexfns_nil () => (fold@ lfs)
  // end of [loop]
in
  // these are pre-defined and cannot be overwritten
  env := redef_cons ("xml_letter", regex_letter, env);
  env := redef_cons ("xml_digit", regex_digit, env);
  env := redef_cons ("xml_extender", regex_extender, env);
  env := redef_cons ("xml_base_char", regex_base_char, env);
  env := redef_cons ("xml_ideographic", regex_ideographic, env);
  env := redef_cons ("xml_combining_char", regex_combining_char, env);
  env := redef_cons ("xml_blank", regex_blank, env);
  env := redef_cons ("tr8876_ident_char", regex_tr8876_ident_char, env);
  env := redef_cons ("EOF", regex_eof, env);
  loop (fil, env, lfs)
end // end of [fprint_lexfns]

(* ****** ****** *)

(* end of [lexgen.dats] *)
