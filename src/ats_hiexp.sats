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

// Time: March 2008
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

(* high-level intermediate representation *)

(* ****** ****** *)

staload Syn = "ats_syntax.sats"

(* ****** ****** *)

staload "ats_staexp2.sats"
staload "ats_dynexp2.sats"

(* ****** ****** *)

datatype hityp_node =
  | HITextype of (* external named type *)
      string
  | HITfun of (* function type *)
      ($Syn.funclo, hityplst, hityp)
  | HITrefarg of (* reference argument *)
      (int(*refval*), hityp)
  | HITs2var of s2var_t
  | HITtyarr of (* array type *)
      (hityp (*element*), s2explstlst(*dimension*))
  | HITtyrec of (* boxed record type *)
      (int(*fltboxknd*), labhityplst) (* knd: flt/box: 0/1 *)
  | HITtyrectemp of (* boxed record type in template *)
      (int(*fltboxknd*), labhityplst) (* knd: flt/box: 0/1 *)
  | HITtyrecsin of (* flat singluar record type *)
      hityp
  | HITtysum of (* constructor type *)
      (d2con_t, hityplst)
  | HITtysumtemp of (* constructor type in template *)
      (d2con_t, hityplst)
  | HITunion of (* union type *)
      labhityplst
  | HITvararg (* variable argument *)

and labhityplst =
  | LABHITYPLSTcons of (lab_t, hityp, labhityplst)
  | LABHITYPLSTnil

and hityp_name = HITNAM of (int(*1/0: ptr/non*), string)

where hityp = '{
  hityp_name= hityp_name, hityp_node= hityp_node
}

and hityplst = List hityp
and hityplstlst = List hityplst

(* ****** ****** *)

fun fprint_hityp {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, hit: hityp): void
overload fprint with fprint_hityp

fun fprint_hityplst {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, hits: hityplst): void
overload fprint with fprint_hityplst

fun fprint_hityplstlst {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, hitss: hityplstlst): void
overload fprint with fprint_hityplstlst

//

fun print_hityp (hit: hityp): void
fun prerr_hityp (hit: hityp): void

overload print with print_hityp
overload prerr with prerr_hityp

//

fun print_hityplst (hits: hityplst): void
fun prerr_hityplst (hits: hityplst): void

overload print with print_hityplst
overload prerr with prerr_hityplst

(* ****** ****** *)

val hityp_abs : hityp
val hityp_bool : hityp
val hityp_char : hityp
val hityp_clo : hityp
val hityp_clo_ptr : hityp
val hityp_clo_ref : hityp
val hityp_double : hityp
val hityp_float : hityp
val hityp_int : hityp
val hityp_ptr : hityp
val hityp_string : hityp
val hityp_tysum_ptr : hityp
val hityp_var : hityp
val hityp_vararg : hityp
val hityp_void : hityp

(* ****** ****** *)

fun hityp_extype (name: string): hityp
fun hityp_fun (fc: $Syn.funclo, _arg: hityplst, _res: hityp): hityp
fun hityp_refarg (refvar: int, _arg: hityp): hityp
fun hityp_s2var (s2v: s2var_t): hityp

fun hityp_tyarr (hit_elt: hityp, s2ess_dim: s2explstlst): hityp

fun hityp_tyrec (fltboxknd: int, name: string, lhits: labhityplst): hityp
fun hityp_tyrectemp (fltboxknd: int, lhits: labhityplst): hityp
fun hityp_tyrecsin (hit: hityp): hityp

fun hityp_tysum (name: string, d2c: d2con_t, _arg: hityplst): hityp
fun hityp_tysumtemp (d2c: d2con_t, _arg: hityplst): hityp

fun hityp_union (name: string, lhits: labhityplst): hityp

(* ****** ****** *)

fun hityp_is_void (hit: hityp): bool
fun hityp_fun_is_void (hit: hityp): bool

fun hityp_is_vararg (hit: hityp): bool
fun hityp_fun_is_vararg (hit: hityp): bool

fun hityp_is_tyrecbox (hit: hityp): bool
fun hityp_is_tyrecsin (hit: hityp): bool

(* ****** ****** *)

abstype tmpvar_t // boxed type
typedef tmpvarlst = List (tmpvar_t)
datatype tmpvaropt = TMPVAROPTsome of tmpvar_t | TMPVAROPTnone

(* ****** ****** *)

datatype hipat_node =
  | HIPann of (* pattern with type ascription *)
      (hipat, hityp)
  | HIPany (* wildcard *)
  | HIPas of (* referenced pattern *)
      (int(*refkind*), d2var_t, hipat)
  | HIPbool of (* boolean pattern *)
      bool
  | HIPchar of (* character pattern *)
      char
  | HIPcon of (* constructor pattern *)
      (int (*freeknd*), d2con_t, hipatlst, hityp(*sum*))
  | HIPcon_any of (* constructor pattern with unused arg *)
      (int(*freeknd*), d2con_t)
  | HIPempty (* empty pattern *)
  | HIPfloat of string (* float point pattern *)
  | HIPint of (* integer pattern *)
      (string, intinf_t)
  | HIPlst of (* list pattern *)
      (hipatlst, hityp(*element*))
  | HIPrec of (* record pattern *)
      (int (*knd*), labhipatlst, hityp(*rec*))
  | HIPstring of (* string pattern *)
      string 
  | HIPvar of (* variable pattern *)
      (int(*refknd*), d2var_t)

and labhipatlst =
  | LABHIPATLSTcons of (lab_t, hipat, labhipatlst)
  | LABHIPATLSTdot
  | LABHIPATLSTnil

where hipat = '{
  hipat_loc= loc_t
, hipat_node= hipat_node
, hipat_typ= hityp
// a variable for storing the value that matches the pattern
, hipat_asvar= d2varopt
} // end of [hipat]

and hipatlst = List hipat

and hipatopt = Option hipat

(* ****** ****** *)

fun fprint_hipat {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, hip: hipat): void
overload fprint with fprint_hipat

fun fprint_hipatlst {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, hips: hipatlst): void
overload fprint with fprint_hipatlst

fun fprint_labhipatlst {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, lhips: labhipatlst): void
overload fprint with fprint_labhipatlst

//

fun print_hipat (hip: hipat): void
fun prerr_hipat (hip: hipat): void

overload print with print_hipat
overload prerr with prerr_hipat

fun print_hipatlst (hips: hipatlst): void
fun prerr_hipatlst (hips: hipatlst): void

overload print with print_hipatlst
overload prerr with prerr_hipatlst

(* ****** ****** *)

fun hipat_ann (_: loc_t, _: hityp, _: hipat, ann: hityp): hipat
fun hipat_any (_: loc_t, _: hityp): hipat
fun hipat_as (_: loc_t, _: hityp, refknd: int, _: d2var_t, _: hipat): hipat
fun hipat_bool (_: loc_t, _: hityp, _: bool): hipat
fun hipat_char (_: loc_t, _: hityp, _: char): hipat

fun hipat_con (
  _: loc_t, _: hityp, freeknd: int, _: d2con_t, _arg: hipatlst, _sum: hityp)
  : hipat

fun hipat_con_any (_: loc_t, _: hityp, freeknd: int, _: d2con_t): hipat

fun hipat_empty (_: loc_t, _: hityp): hipat
fun hipat_int (_: loc_t, _: hityp, str: string, int: intinf_t): hipat
fun hipat_lst (_: loc_t, _lst: hityp, _: hipatlst, _elt: hityp): hipat

fun hipat_rec
  (_: loc_t, _: hityp, knd: int, _: labhipatlst, _rec: hityp): hipat

fun hipat_string (_: loc_t, _: hityp, _: string): hipat

fun hipat_var (_: loc_t, _: hityp, refknd: int, d2v: d2var_t): hipat

(* ****** ****** *)

fun hipatlst_is_unused (hips: hipatlst): bool

(* ****** ****** *)

fun hipat_asvar_set (hip: hipat, od2v: d2varopt): void
  = "ats_hiexp_hipat_asvar_set"

(* ****** ****** *)

datatype hidec_node =
  | HIDlist of hideclst
  | HIDsaspdec of s2aspdec
  | HIDdcstdec of (* dynamic constant *)
      ($Syn.dcstkind, d2cstlst)
  | HIDdatdec of (* datatype declaration *)
      ($Syn.datakind, s2cstlst)
  | HIDexndec of (* exception constructor *)
      d2conlst
  | HIDextype of (* external type *)
      (string (*name*), hityp (*definition*))
  | HIDextval of (* external value *)
      (string (*name*), hiexp (*definition*))
  | HIDextern of (* external code *)
      (int (*position: 0/1/2 : top/?/end*), string (*code*))
  | HIDfundecs of (* function *)
      (s2qualst(*decarg*), $Syn.funkind, hifundeclst)
  | HIDvaldecs of (* value *)
      ($Syn.valkind, hivaldeclst)
  | HIDvaldecs_par of (* parallel value declaration *)
      hivaldeclst
  | HIDvaldecs_rec of (* recursive value declaration *)
      hivaldeclst
  | HIDvardecs of (* variable *)
      hivardeclst
  | HIDimpdec of (* implementation *)
      hiimpdec
  | HIDlocal of (* local declaration *)
      (hideclst (*head*), hideclst (*body*))
  | HIDstaload of (* static loading *)
      fil_t
  | HIDdynload of (* dynamic loading *)
      fil_t

and hiexp_node =
  | HIEapp of (* dynamic application *)
      (hityp, hiexp, hiexplst)
  | HIEarrinit of (* array construction *)
      (hityp(*eltyp*), hiexpopt(*asz*), hiexplst(*elt*))
  | HIEarrsize of (* arraysize construction *)
      (hityp(*eltyp*), hiexplst(*elt*))
  | HIEassgn_ptr of (* assignment to a pointer with offsets *)
      (hiexp, hilablst, hiexp)
  | HIEassgn_var of (* assignment to a variable with ofsets *)
      (d2var_t, hilablst, hiexp)
  | HIEbool of (* boolean constant *)
      bool
  | HIEcaseof of (* case expression *)
      (int(*casknd*), hiexplst, hiclaulst)
  | HIEchar of (* character constant *)
      char
  | HIEcon of (* constructor *)
      (hityp, d2con_t, hiexplst)
  | HIEcst of (* dynamic constant *)
      d2cst_t
  | HIEdynload of (* dynamic loading *)
      fil_t (* filename *)
  | HIEempty (* no operation *)
  | HIEextval of (* external value *)
      string
  | HIEfix of (* fixed-point expression *)
      (d2var_t, hiexp)
  | HIEfloat of (* double precison floating point *)
      string
  | HIEfloatsp of (* specified floating point *)
      string
  | HIEfreeat of (* memory deallocation *)
      hiexp
  | HIEif of (* conditional *)
      (hiexp, hiexp, hiexp)
  | HIEint of (* integer constant *)
      (string, intinf_t)
  | HIEintsp of (* specified integer constant *)
      (string, intinf_t)
  | HIElam of (* lambda-abstraction *)
      (hipatlst, hiexp)
  | HIElazy_delay of (* delayed computation *)
      (int(*lin*), hiexp)
  | HIElazy_force of (* lazy value evaluation *)
      (int(*linearity*), hiexp)
  | HIElet of (* let-expression *)
      (hideclst, hiexp)
  | HIEloop of (* for-loop *)
      (hiexpopt(*init*), hiexp(*test*), hiexpopt(*post*), hiexp(*body*))
  | HIEloopexn of (* local jump *)
      int (* break: 0 and continue: 1 *)
  | HIElst of (* list expression *)
      (int(*lin*), hityp(*element*), hiexplst)
  | HIEptrof_ptr of (* address-of *)
      (hiexp, hilablst)
  | HIEptrof_var of (* address-of *)
      (d2var_t, hilablst)
  | HIEraise of (* raising exception *)
      hiexp
  | HIErec of (* record construction *)
      (int(*knd*), hityp, labhiexplst)
  | HIErefarg of (* call-by-refval argument *)
      (int(*refval*), int(*freeknd*), hiexp)
  | HIEsel of (* path selection *)
      (hiexp, hilablst)
  | HIEsel_ptr of (* path selection for pointer *)
      (hiexp, hilablst)
  | HIEsel_var of (* path selection for variable *)
      (d2var_t, hilablst)
  | HIEseq of (* sequencing *)
      hiexplst
  | HIEsizeof of (* size of type *)
      hityp
  | HIEspawn of (* spawned evaluation *)
      hiexp
  | HIEstring of (* string constant *)
      (string, int(*length*))
  | HIEtmpcst of (* template constant *)
      (d2cst_t, hityplstlst)
  | HIEtmpvar of (* template variable *)
      (d2var_t, hityplstlst)
  | HIEtop (* uninitialized value *)
  | HIEtrywith of (* exception handling *)
      (hiexp, hiclaulst)
  | HIEvar of (* variable *)
      d2var_t

and labhiexplst =
  | LABHIEXPLSTcons of (lab_t, hiexp, labhiexplst)
  | LABHIEXPLSTnil

and hilab_node =
  | HILlab of (* record selection *)
      (lab_t, hityp (*record*))
  | HILind of (* array subscription *)
      (hiexplstlst (*index*), hityp (*element*))

(* ****** ****** *)

where hidec = '{
  hidec_loc= loc_t, hidec_node= hidec_node
}

and hideclst = List hidec

and hiexp = '{ 
  hiexp_loc= loc_t, hiexp_node= hiexp_node, hiexp_typ= hityp
}

and hiexplst = List hiexp
and hiexpopt = Option hiexp
and hiexplstlst = List hiexplst

and hilab = '{
  hilab_loc= loc_t, hilab_node= hilab_node
}

and hilablst = List hilab

and himat = '{
  himat_loc= loc_t, himat_exp= hiexp, himat_pat= Option (hipat)
}

and himatlst = List himat

and hiclau = '{ (* type for clauses *)
  hiclau_loc= loc_t (* location information *)
, hiclau_pat= hipatlst (* pattern list *)
, hiclau_gua= himatlst (* clause guard *)
, hiclau_exp= hiexp (* expression body *)
}

and hiclaulst = List hiclau

(* ****** ****** *)

and hifundec = '{
  hifundec_loc= loc_t
, hifundec_var= d2var_t
, hifundec_def= hiexp
}

and hifundeclst = List hifundec

and hivaldec = '{
  hivaldec_loc= loc_t
, hivaldec_pat= hipat
, hivaldec_def= hiexp
}

and hivaldeclst = List hivaldec

and hivardec = '{
  hivardec_loc= loc_t
, hivardec_knd= int
, hivardec_ptr= d2var_t
, hivardec_ini= hiexpopt
}

and hivardeclst = List hivardec

and hiimpdec = '{ (* implementation *)
  hiimpdec_loc= loc_t
, hiimpdec_cst= d2cst_t
, hiimpdec_tmp= int
, hiimpdec_decarg= s2qualst, hiimpdec_tmparg= hityplstlst
, hiimpdec_def= hiexp
}

(* ****** ****** *)

fun fprint_hilab {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, hil: hilab): void
overload fprint with fprint_hilab

fun fprint_hilablst {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, hils: hilablst): void
overload fprint with fprint_hilablst

//

fun fprint_hiexp {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, hie: hiexp): void
overload fprint with fprint_hiexp

fun fprint_hiexplst {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, hies: hiexplst): void
overload fprint with fprint_hiexplst

fun fprint_hiexplstlst {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, hiess: hiexplstlst): void
overload fprint with fprint_hiexplstlst

fun fprint_labhiexplst {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, lhies: labhiexplst): void
overload fprint with fprint_labhiexplst

//

fun print_hiexp (hie: hiexp): void
fun prerr_hiexp (hie: hiexp): void

overload print with print_hiexp
overload prerr with prerr_hiexp

fun print_hiexplst (hies: hiexplst): void
fun prerr_hiexplst (hies: hiexplst): void

overload print with print_hiexplst
overload prerr with prerr_hiexplst

(* ****** ****** *)

fun hiexp_is_empty (hie: hiexp): bool

fun hiexp_is_value (hie: hiexp): bool

fun hiexp_let_simplify
  (_: loc_t, _: hityp, _: hideclst, _: hiexp): hiexp

fun hiexp_seq_simplify
  (_: loc_t, _: hityp, _: hiexplst): hiexp

(* ****** ****** *)
 
fun hiexp_app
  (_: loc_t, _app: hityp, _fun: hityp, hie: hiexp, hies: hiexplst)
  : hiexp

fun hiexp_arrinit
  (_: loc_t, _: hityp, hit_elt: hityp, ohie_asz: hiexpopt, hies_elt: hiexplst)
  : hiexp

fun hiexp_arrsize
  (_: loc_t, _: hityp, hit_elt: hityp, hies: hiexplst): hiexp

fun hiexp_assgn_ptr
  (_: loc_t, _: hityp, _ptr: hiexp, hils: hilablst, _val: hiexp)
  : hiexp

fun hiexp_assgn_var
  (_: loc_t, _: hityp, _ptr: d2var_t, hils: hilablst, _val: hiexp)
  : hiexp

fun hiexp_bool (_: loc_t, _: hityp, _: bool): hiexp

fun hiexp_caseof
  (_: loc_t, _: hityp, knd: int, hies: hiexplst, hicls: hiclaulst)
  : hiexp

fun hiexp_caseof_if
  (loc0: loc_t, hit0: hityp, knd: int, hies: hiexplst, hicls: hiclaulst)
  : hiexp

fun hiexp_char (_: loc_t, _: hityp, _: char): hiexp

fun hiexp_con
  (_: loc_t, _: hityp, _sum: hityp, _: d2con_t, _arg: hiexplst)
  : hiexp

fun hiexp_cst (_: loc_t, _: hityp, d2c: d2cst_t): hiexp

fun hiexp_dynload (_: loc_t, _: hityp, _: fil_t): hiexp

fun hiexp_empty (_: loc_t, _: hityp): hiexp

fun hiexp_extval (_: loc_t, _: hityp, code: string): hiexp

fun hiexp_fix
  (_: loc_t, _: hityp, _fun: d2var_t, _body: hiexp): hiexp

fun hiexp_float (_: loc_t, _: hityp, _: string): hiexp
fun hiexp_floatsp (_: loc_t, _: hityp, _: string): hiexp

fun hiexp_freeat (_: loc_t, _: hityp, _: hiexp): hiexp

fun hiexp_if
  (_: loc_t, _: hityp, _cond: hiexp, _then: hiexp, _else: hiexp)
  : hiexp

fun hiexp_int
  (_: loc_t, _: hityp, str: string, int: intinf_t): hiexp

fun hiexp_intsp
  (_: loc_t, _: hityp, str: string, int: intinf_t): hiexp

fun hiexp_lam
  (_: loc_t, _: hityp, _arg: hipatlst, _body: hiexp): hiexp

(* ****** ****** *)

fun hiexp_lazy_delay
  (_: loc_t, _body: hityp, lin: int, _body: hiexp): hiexp

fun hiexp_lazy_force
  (_: loc_t, _val: hityp, lin: int, _lazyval: hiexp): hiexp

(* ****** ****** *)

fun hiexp_let (_: loc_t, _: hityp, _: hideclst, _: hiexp): hiexp

(* ****** ****** *)

fun hiexp_loop (
    _: loc_t
  , _: hityp
  , _init: hiexpopt
  , _test: hiexp
  , _post: hiexpopt
  , _body: hiexp
  ) : hiexp

fun hiexp_loopexn (_: loc_t, _: hityp, i: int): hiexp

(* ****** ****** *)

fun hiexp_lst
  (_: loc_t, _lst: hityp, lin: int, _elt: hityp, _: hiexplst)
  : hiexp

fun hiexp_ptrof_ptr
  (_: loc_t, _: hityp, _ptr: hiexp, hils: hilablst): hiexp

fun hiexp_ptrof_var
  (_: loc_t, _: hityp, _ptr: d2var_t, hils: hilablst): hiexp

fun hiexp_raise (_: loc_t, _: hityp, _: hiexp): hiexp

fun hiexp_rec
  (_: loc_t, _: hityp, knd: int, _rec: hityp, _: labhiexplst)
  : hiexp

fun hiexp_refarg
  (_: loc_t, hit: hityp, refval: int, freeknd: int, _: hiexp): hiexp

fun hiexp_sel
  (_: loc_t, _: hityp, hie: hiexp, hils: hilablst): hiexp

fun hiexp_sel_ptr
  (_: loc_t, _: hityp, _ptr: hiexp, hils: hilablst): hiexp

fun hiexp_sel_var
  (_: loc_t, _: hityp, _ptr: d2var_t, hils: hilablst): hiexp

fun hiexp_seq (_: loc_t, _: hityp, _: hiexplst): hiexp

fun hiexp_sizeof (_: loc_t, _: hityp, _arg: hityp): hiexp

fun hiexp_spawn (_: loc_t, _: hityp, _: hiexp): hiexp

fun hiexp_string (_: loc_t, _: hityp, _: string, _: int): hiexp

fun hiexp_tmpcst
  (_: loc_t, _: hityp, d2c: d2cst_t, _: hityplstlst): hiexp

fun hiexp_tmpvar
  (_: loc_t, _: hityp, d2v: d2var_t, _: hityplstlst): hiexp

fun hiexp_top (_: loc_t, _: hityp): hiexp

fun hiexp_trywith
  (_: loc_t, _: hityp, _: hiexp, _: hiclaulst): hiexp

fun hiexp_var (_: loc_t, _: hityp, _: d2var_t): hiexp

(* ****** ****** *)

fun hilab_lab (_: loc_t, _lab: lab_t, _rec: hityp): hilab
fun hilab_ind (_: loc_t, _ind: hiexplstlst, _elt: hityp): hilab

(* ****** ****** *)

fun himat_make (_: loc_t, _: hiexp, _: hipatopt): himat

fun hiclau_make
  (_: loc_t, hips: hipatlst, gua: himatlst, body: hiexp): hiclau

(* ****** ****** *)

fun hidec_list (_: loc_t, _: hideclst): hidec

fun hidec_saspdec (_: loc_t, _: s2aspdec): hidec

fun hidec_dcstdec (_: loc_t, _: $Syn.dcstkind, _: d2cstlst): hidec

fun hidec_datdec (_: loc_t, _: $Syn.datakind, _: s2cstlst): hidec
fun hidec_exndec (_: loc_t, _: d2conlst): hidec

fun hidec_extern (_: loc_t, position: int, code: string): hidec
fun hidec_extype (_: loc_t, name: string, _def: hityp): hidec
fun hidec_extval (_: loc_t, name: string, _def: hiexp): hidec

fun hifundec_make (_: loc_t, d2v: d2var_t, hie: hiexp): hifundec
fun hidec_fundecs
  (_: loc_t, decarg: s2qualst, knd: $Syn.funkind, hids: hifundeclst)
  : hidec

fun hivaldec_make (_: loc_t, hip: hipat, hie: hiexp): hivaldec
fun hidec_valdecs
  (_: loc_t, knd: $Syn.valkind, hids: hivaldeclst): hidec
fun hidec_valdecs_par (_: loc_t, hids: hivaldeclst): hidec
fun hidec_valdecs_rec (_: loc_t, hids: hivaldeclst): hidec

fun hivardec_make
  (_: loc_t, knd: int, d2v: d2var_t, ini: hiexpopt): hivardec
fun hidec_vardecs (_: loc_t, hids: hivardeclst): hidec

(* ****** ****** *)

fun hiimpdec_make (
    _: loc_t
  , d2c: d2cst_t, tmp: int
  , decarg: s2qualst, tmparg: hityplstlst,
  _def: hiexp
  ) : hiimpdec
fun hidec_impdec (_: loc_t, hid: hiimpdec): hidec

(* ****** ****** *)

fun hidec_local (_: loc_t, _head: hideclst, _body: hideclst): hidec

fun hidec_staload (_: loc_t, _: fil_t): hidec
fun hidec_dynload (_: loc_t, _: fil_t): hidec

(* ****** ****** *)

abstype hityp_t // boxed type
abstype hityplst_t // boxed type
abstype hityplstlst_t // boxed type

fun hityp_encode (hit: hityp): hityp_t
fun hityp_decode (hit: hityp_t): hityp

fun hityp_t_s2var (s2v: s2var_t): hityp_t
fun hityp_t_name_get (hit: hityp_t): hityp_name

fun hityp_t_is_void (hit: hityp_t): bool
fun hityp_t_fun_is_void (hit: hityp_t): bool
fun hityp_t_is_tyrecbox (hit: hityp_t): bool
fun hityp_t_is_tyrecsin (hit: hityp_t): bool

fun hityplst_encode (hits: hityplst): hityplst_t
fun hityplst_decode (hits: hityplst_t): hityplst

fun hityplst_is_nil (hits: hityplst_t): bool
fun hityplst_is_cons (hits: hityplst_t): bool

fun hityplstlst_encode (hits: hityplstlst): hityplstlst_t
fun hityplstlst_decode (hits: hityplstlst_t): hityplstlst

fun hityplstlst_is_nil (hitss: hityplstlst_t): bool
fun hityplstlst_is_cons (hitss: hityplstlst_t): bool

//

fun print_hityp_t (hit: hityp_t): void
fun prerr_hityp_t (hit: hityp_t): void

overload print with print_hityp_t
overload prerr with prerr_hityp_t

//

fun d2cst_hityp_get (_: d2cst_t): Option (hityp_t)
fun d2cst_hityp_set (_: d2cst_t, _: Option hityp_t): void
  = "ats_dynexp2_d2cst_hityp_set"

fun d2cst_hityp_get_some (_: d2cst_t): hityp_t

(* ****** ****** *)

abstype vartyp_t // boxed type

fun vartyp_make (d2v: d2var_t, hit: hityp_t): vartyp_t

fun vartyp_typ_get (vtp: vartyp_t):<> hityp_t
fun vartyp_var_get (vtp: vartyp_t):<> d2var_t

fun eq_vartyp_vartyp (_: vartyp_t, _: vartyp_t):<> bool
overload = with eq_vartyp_vartyp

fun compare_vartyp_vartyp (_: vartyp_t, _: vartyp_t):<> Sgn
overload compare with compare_vartyp_vartyp

fun fprint_vartyp {m:file_mode}
   (pf: file_mode_lte (m, w) | out: &FILE m, vtp: vartyp_t): void

fun print_vartyp (vtp: vartyp_t): void
fun prerr_vartyp (vtp: vartyp_t): void

overload print with print_vartyp
overload prerr with prerr_vartyp

(* ****** ****** *)

typedef strlst = List string

datatype labstrlst =
  | LABSTRLSTcons of (lab_t, string, labstrlst) | LABSTRLSTnil

datatype typkey =
  | TYPKEYrec of labstrlst // record
  | TYPKEYsum of (int, strlst) // sum
  | TYPKEYuni of labstrlst // union

// implemented in [ats_ccomp_env.dats]
fun typdefmap_find (tk: typkey): string

// implemented in [ats_hiexp_util.dats]
fun hityp_tyrec_make (fltboxknd: int, lhits: labhityplst): hityp_t
fun hityp_tysum_make (d2c: d2con_t, hits_arg: hityplst): hityp_t

fun hityp_normalize (hit: hityp): hityp_t
fun hityplst_normalize (hits: hityplst): hityplst_t
fun hityplstlst_normalize (hitss: hityplstlst): hityplstlst_t

// implemented in [ats_ccomp_trans_temp.dats]
fun hityp_s2var_normalize (s2v: s2var_t): Option_vt (hityp_t)

(* ****** ****** *)

abstype tmpdef_t

// implemented in [ats_hiexp_util.dats]
fun tmpdef_make (decarg: s2qualst, def: hiexp): tmpdef_t
fun tmpdef_arg_get (def: tmpdef_t): s2qualst
fun tmpdef_exp_get (def: tmpdef_t): hiexp

// implemented in [ats_hiexp_util.dats]
fun tmpcstmap_add (d2c: d2cst_t, decarg: s2qualst, def: hiexp): void
fun tmpcstmap_find (d2c: d2cst_t): Option_vt tmpdef_t

// implemented in [ats_hiexp_util.dats]
fun tmpvarmap_add (d2v: d2var_t, decarg: s2qualst, def: hiexp): void
fun tmpvarmap_find (d2v: d2var_t): Option_vt tmpdef_t

(* ****** ****** *)

(* end of [ats_hiexp.dats] *)
