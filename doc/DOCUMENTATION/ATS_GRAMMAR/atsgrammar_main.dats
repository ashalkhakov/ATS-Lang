(*
**
** For documenting the grammar of ATS/Anairiats
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Contributed by Sylvain Nahas (sylvain.nahas AT googlemail DOT com)
**
** Time: November, 2010
**
*)

(* ****** ****** *)

staload "atsgrammar.sats"

(* ****** ****** *)

local
//
assume symbol_open_v (s:int) = unit_v
//
in
//
implement
symbol_open (sym) = (unit_v () | ())
//
implement
symbol_close
  (pf | sym) = let
  prval unit_v () = pf
  val grs1 = theGrmrulelst_get ()
  val grs2 = symbol_get_grmrulelst (sym)
  val grs = revapp (grs1, grs2) where {
    fun revapp (
      grs1: grmrulelst_vt, grs2: grmrulelst
    ) : grmrulelst = case+ grs1 of
      | ~list_vt_cons (gr, grs1) => revapp (grs1, list_cons (gr, grs2))
      | ~list_vt_nil () => grs2
    // end of [revapp]
  } // end of [val]
in
  symbol_set_grmrulelst (sym, grs)
end // end of [symbol_close]
//
end // end of [local]

(* ****** ****** *)
//
val LITERAL_char = symbol_make "LITERAL_char"
val LITERAL_extcode = symbol_make "LITERAL_extcode"
val LITERAL_float = symbol_make "LITERAL_float"
val LITERAL_floatsp = symbol_make "LITERAL_floatsp"
val LITERAL_int = symbol_make "LITERAL_int"
val LITERAL_intsp = symbol_make "LITERAL_intsp"
val LITERAL_string = symbol_make "LITERAL_string"
//
val IDENTIFIER_alp = symbol_make "IDENTIFIER_alp"
val () = symbol_set_printname
  (IDENTIFIER_alp, "ALPHANUMERIC_IDENTIFIER")
//
val IDENTIFIER_sym = symbol_make "IDENTIFIER_sym"
val () = symbol_set_printname (IDENTIFIER_sym, "SYMBOLIC_IDENTIFIER")
//
val IDENTIFIER_arr = symbol_make "IDENTIFIER_arr"
val () = symbol_set_printname (IDENTIFIER_arr, "ARRAY_IDENTIFIER")
val IDENTIFIER_tmp = symbol_make "IDENTIFIER_tmp"
val () = symbol_set_printname (IDENTIFIER_tmp, "TEMPLATE_IDENTIFIER")
val IDENTIFIER_ext = symbol_make "IDENTIFIER_ext"
val () = symbol_set_printname (IDENTIFIER_ext, "EXTERNAL_IDENTIFIER")
//
val IDENTIFIER_dlr = symbol_make "IDENTIFIER_dlr"
val IDENTIFIER_srp = symbol_make "IDENTIFIER_srp"
//
(* ****** ****** *)
//
val EQ = symbol_make ("EQ")
val () = symbol_set_printname (EQ, "\"=\"")
//
val GT = symbol_make ("GT")
val () = symbol_set_printname (GT, "\">\"")
//
val LT = symbol_make ("LT")
val () = symbol_set_printname (LT, "\"<\"")
//
(* ****** ****** *)
//
val ABSPROP = symbol_make "ABSPROP"
val () = symbol_set_printname (ABSPROP, "\"absprop\"")
//
val ABSTYPE = symbol_make "ABSTYPE"
val () = symbol_set_printname (ABSTYPE, "\"abstype\"")
//
val ABST0YPE = symbol_make "ABST@YPE"
val () = symbol_set_printname (ABST0YPE, "\"abst@ype\"")
//
val ABSVIEW = symbol_make "ABSVIEW"
val () = symbol_set_printname (ABSVIEW, "\"absview\"")
//
val ABSVIEWTYPE = symbol_make "ABSVIEWTYPE"
val () = symbol_set_printname (ABSVIEWTYPE, "\"absviewtype\"")
//
val ABSVIEWT0YPE = symbol_make "ABSVIEWT@YPE"
val () = symbol_set_printname (ABSVIEWT0YPE, "\"absviewt@ype\"")
//
val AND = symbol_make "AND"
val () = symbol_set_printname (AND, "\"and\"")
//
val AS = symbol_make "AS"
val () = symbol_set_printname (AS, "\"as\"")
//
val ASSUME = symbol_make "ASSUME"
val () = symbol_set_printname (ASSUME, "\"assume\"")
//
val CASTFN = symbol_make "CASTFN"
val () = symbol_set_printname (CASTFN, "\"castfn\"")
//
val FUN = symbol_make "FUN"
val () = symbol_set_printname (FUN, "\"fun\"")
//
val PRAXI = symbol_make "PRAXI"
val () = symbol_set_printname (PRAXI, "\"praxi\"")
//
val PRFUN = symbol_make "PRFUN"
val () = symbol_set_printname (PRFUN, "\"prfun\"")
//
val PRVAL = symbol_make "PRVAL"
val () = symbol_set_printname (PRVAL, "\"prval\"")
//
val VAL = symbol_make "VAL"
val () = symbol_set_printname (VAL, "\"val\"")
//
(* ****** ****** *)

val abskind = symbol_make "abskind"
val dcstkind = symbol_make "dcstkind"

(* ****** ****** *)

val i0de = symbol_make "i0de"
val i0deseq = symbol_make "i0deseq"

(* ****** ****** *)

val d0ec_sta = symbol_make "d0ec_sta"
val d0ecseq_sta = symbol_make "d0ecseq_sta"
val d0ec_dyn = symbol_make "d0ec_dyn"
val d0ecseq_dyn = symbol_make "d0ecseq_dyn"
val d0ecseq_dyn_rev = symbol_make "d0ecseq_dyn_rev"

(* ****** ****** *)

(*
abskind =
    "absprop"
  | "abstype"
  | "abst@ype"
  | "absview"
  | "absviewtype"
  | "absviewt@ype"
  ;
*)
fun abskind_proc
  (): void = () where {
//
val (pf | ()) = symbol_open (abskind)
//
val () = grmrule_append (ABSPROP)
val () = grmrule_append (ABSTYPE)
val () = grmrule_append (ABST0YPE)
val () = grmrule_append (ABSVIEW)
val () = grmrule_append (ABSVIEWTYPE)
val () = grmrule_append (ABSVIEWT0YPE)
//
val () = symbol_close (pf | abskind)
} // end of [abskind_proc]

(* ****** ****** *)

(*
dcstkind =
    "fun"
  | "val"
  | "praxi"
  | "prfun"
  | "prval"
  | "castfn"
  ;
*)
fun dcstkind_proc
  (): void = () where {
//
val (pf | ()) = symbol_open (dcstkind)
//
val () = grmrule_append (FUN)
val () = grmrule_append (VAL)
val () = grmrule_append (PRAXI)
val () = grmrule_append (PRFUN)
val () = grmrule_append (PRVAL)
val () = grmrule_append (CASTFN)
//
val () = symbol_close (pf | dcstkind)
} // end of [dcstkind_proc]

(* ****** ****** *)

fun i0deseq_proc
  (): void = () where {
//
val (pf | ()) = symbol_open (i0deseq)
//
val () = grmrule_append ()
val () = grmrule_append ($lst_t {symbol} (tupz! i0de i0deseq))
//
val () = symbol_close (pf | i0deseq)
//
} // end of [i0deseq_proc]

(* ****** ****** *)

(*
d0ecseq_dyn_rev /* tail-recursive */
  : /* empty */                         { $$ = d0ecllst_nil() ; }
  | d0ecseq_dyn_rev d0ec_dyn semicolonseq
                                        { $$ = d0ecllst_cons($1, $2) ; }
;
*)
fun d0ecseq_dyn_rev_proc
  (): void = () where {
//
val (pf | ()) = symbol_open (d0ecseq_dyn_rev)
//
val () = grmrule_append ()
val () = grmrule_append ($lst_t {symbol} (tupz! d0ecseq_dyn_rev d0ec_dyn))
//
val () = symbol_close (pf | d0ecseq_dyn_rev)
//
} // end of [d0ecseq_dyn_proc]

(*
d0ecseq_dyn
  : d0ecseq_dyn_rev                     { $$ = d0ecllst_reverse($1) ; }
;
*)
fun d0ecseq_dyn_proc
  (): void = () where {
//
val (pf | ()) = symbol_open (d0ecseq_dyn)
//
val () = grmrule_append (d0ecseq_dyn_rev)
//
val () = symbol_close (pf | d0ecseq_dyn)
//
} // end of [d0ecseq_dyn_proc]

(* ****** ****** *)

extern fun atsgrammar_main (): void

(* ****** ****** *)

implement
atsgrammar_main
  () = () where {
  val () = abskind_proc ()
  val () = dcstkind_proc ()
  val () = i0deseq_proc ()
  val () = d0ecseq_dyn_rev_proc () // reversed dynamic declaration sequence
  val () = d0ecseq_dyn_proc ()
} // end of [atsgrammar_main]

(* ****** ****** *)

(* end of [atsgrammar_main.dats] *)
