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

staload "ats_staexp2.sats"
staload "ats_dynexp2.sats"

(* ****** ****** *)

staload "ats_hiexp.sats"

(* ****** ****** *)

abstype tmplab_t // boxed type

fun tmplab_make (): tmplab_t
fun tmplab_stamp_get (tl: tmplab_t): stamp_t

fun fprint_tmplab {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, tl: tmplab_t): void

fun print_tmplab (tl: tmplab_t): void
fun prerr_tmplab (tl: tmplab_t): void

overload print with print_tmplab
overload prerr with prerr_tmplab

(* ****** ****** *)

abstype funlab_t // boxed type
typedef funlablst = List funlab_t
viewtypedef funlablst_vt = List_vt (funlab_t)

abstype funentry_t
typedef funentrylst = List funentry_t
typedef funentryopt = Option funentry_t

//

fun fprint_funlab {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, fl: funlab_t): void
overload fprint with fprint_funlab

fun print_funlab (fl: funlab_t): void
fun prerr_funlab (fl: funlab_t): void

overload print with print_funlab
overload prerr with prerr_funlab

fun fprint_funlablst {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, fls: funlablst): void
overload fprint with fprint_funlablst

fun print_funlablst (fls: funlablst): void
fun prerr_funlablst (fls: funlablst): void

overload print with print_funlablst
overload prerr with prerr_funlablst

//

fun eq_funlab_funlab (fl1: funlab_t, fl2: funlab_t):<> bool
overload = with eq_funlab_funlab

fun compare_funlab_funlab (fl1: funlab_t, fl2: funlab_t):<> Sgn
overload compare with compare_funlab_funlab

//

fun funlab_make_typ (hit: hityp_t): funlab_t

fun funlab_make_nam_typ (name: string, hit: hityp_t): funlab_t

fun funlab_make_cst_typ
  (d2c: d2cst_t, tmparg: hityplstlst, hit: hityp_t): funlab_t

fun funlab_make_var_typ (d2v: d2var_t, hit: hityp_t): funlab_t

//

fun funlab_make_cst_trmck (d2c: d2cst_t): funlab_t

//

fun funlab_name_get (fl: funlab_t): string

fun funlab_lev_get (fl: funlab_t): int

fun funlab_typ_get (fl: funlab_t): hityp_t
fun funlab_typ_arg_get (fl: funlab_t): hityplst_t
fun funlab_typ_res_get (fl: funlab_t): hityp_t
fun funlab_funclo_get (fl: funlab_t): $Syn.funclo

fun funlab_qua_get (fl: funlab_t): d2cstopt
fun funlab_qua_set (fl: funlab_t, _: d2cstopt): void
  = "ats_ccomp_funlab_qua_set"

fun funlab_tailjoined_get (fl: funlab_t): tmpvarlst
fun funlab_tailjoined_set (fl: funlab_t, tmps: tmpvarlst): void
  = "ats_ccomp_funlab_tailjoined_set"

fun funlab_entry_get (fl: funlab_t): funentryopt
fun funlab_entry_set (fl: funlab_t, _: funentryopt): void
  = "ats_ccomp_funlab_entry_set"

fun funlab_entry_get_some (fl: funlab_t): funentry_t

fun funlab_trmck_get (fl: funlab_t): int

(* ****** ****** *)

fun fprint_tmpvar {m:file_mode}
   (pf: file_mode_lte (m, w) | out: &FILE m, tmp: tmpvar_t): void
overload fprint with fprint_tmpvar

fun print_tmpvar (tmp: tmpvar_t): void
fun prerr_tmpvar (tmp: tmpvar_t): void

overload print with print_tmpvar
overload prerr with prerr_tmpvar

fun fprint_tmpvarlst {m:file_mode}
   (pf: file_mode_lte (m, w) | out: &FILE m, tmps: tmpvarlst): void
overload fprint with fprint_tmpvarlst

fun print_tmpvarlst (tmps: tmpvarlst): void
fun prerr_tmpvarlst (tmps: tmpvarlst): void

overload print with print_tmpvarlst
overload prerr with prerr_tmpvarlst

//

fun eq_tmpvar_tmpvar (tmp1: tmpvar_t, tmp2: tmpvar_t): bool
overload = with eq_tmpvar_tmpvar

fun compare_tmpvar_tmpvar (tmp1: tmpvar_t, tmp2: tmpvar_t):<> Sgn
overload compare with compare_tmpvar_tmpvar

//

fun tmpvar_make (hit: hityp_t): tmpvar_t
fun tmpvar_make_ret (hit: hityp_t): tmpvar_t
fun tmpvar_make_root (tmp: tmpvar_t): tmpvar_t

fun tmpvarlst_make (hit: hityplst_t): tmpvarlst

//

fun tmpvar_ret_get (tmp: tmpvar_t): int
fun tmpvar_root_get (tmp: tmpvar_t): tmpvaropt
fun tmpvar_stamp_get (tmp: tmpvar_t): stamp_t
fun tmpvar_typ_get (tmp: tmpvar_t): hityp_t

fun tmpvar_is_mutable (tmp: tmpvar_t): bool
fun tmpvar_is_void (tmp: tmpvar_t): bool
fun tmpvar_is_nonvoid (tmp: tmpvar_t): bool

(* ****** ****** *)

abstype envmap_t // boxed type

(* ****** ****** *)

datatype valprim_node =
  | VParg of int
  | VParg_ref of int (* call-by-reference *)
  | VPbool of bool
  | VPchar of char
  | VPclo of (int(*knd*), funlab_t, envmap_t)
  | VPcst of d2cst_t
  | VPenv of vartyp_t
  | VPext of string
  | VPfloat of string
  | VPfun of funlab_t
  | VPint of intinf_t
  | VPintsp of (string, intinf_t)
  | VPptrof of valprim
  | VPptrof_ptr_offs of (valprim, offsetlst)
  | VPptrof_var_offs of (valprim, offsetlst)
  | VPsizeof of hityp_t
  | VPstring of (string, int(*length*))
  | VPtmp of tmpvar_t
  | VPtmp_ref of tmpvar_t
  | VPtop
  | VPvoid

and offset =
  | OFFSETlab of (lab_t, hityp_t(*rec*))
  | OFFSETind of (valprimlstlst(*ind*), hityp_t(*elt*))

where valprim = '{ // primitive values
  valprim_node= valprim_node, valprim_typ= hityp_t // type erasure
}

and valprimlst = List valprim
and valprimlstlst = List valprimlst
and valprimlst_vt = List_vt valprim

and offsetlst = List offset

(* ****** ****** *)

datatype labvalprimlst =
  | LABVALPRIMLSTcons of (lab_t, valprim, labvalprimlst)
  | LABVALPRIMLSTnil

(* ****** ****** *)

fun fprint_valprim {m:file_mode}
   (pf: file_mode_lte (m, w) | out: &FILE m, vp: valprim): void

fun fprint_valprimlst {m:file_mode}
   (pf: file_mode_lte (m, w) | out: &FILE m, vps: valprimlst): void

fun fprint_labvalprimlst {m:file_mode}
   (pf: file_mode_lte (m, w) | out: &FILE m, lvps: labvalprimlst): void

//

fun fprint_offset {m:file_mode}
   (pf: file_mode_lte (m, w) | out: &FILE m, off: offset): void

fun fprint_offsetlst {m:file_mode}
   (pf: file_mode_lte (m, w) | out: &FILE m, offs: offsetlst): void

//

fun print_valprim (vp: valprim): void
fun prerr_valprim (vp: valprim): void

overload print with print_valprim
overload prerr with prerr_valprim

fun print_valprimlst (vps: valprimlst): void
fun prerr_valprimlst (vps: valprimlst): void

overload print with print_valprimlst
overload prerr with prerr_valprimlst

(* ****** ****** *)

fun valprim_is_const (vp: valprim): bool
fun valprim_is_mutable (vp: valprim): bool

(* ****** ****** *)

fun valprim_arg (i: int, hit: hityp_t): valprim
fun valprim_arg_ref (i: int, hit: hityp_t): valprim
fun valprim_bool (b: bool): valprim
fun valprim_char (c: char): valprim
fun valprim_clo (knd: int, fl: funlab_t, map: envmap_t): valprim
fun valprim_cst (d2c: d2cst_t, hit: hityp_t): valprim
fun valprim_env (vtp: vartyp_t, hit: hityp_t): valprim
fun valprim_ext (code: string, hit: hityp_t): valprim
fun valprim_float (f: string): valprim
fun valprim_fun (fl: funlab_t): valprim

fun valprim_int (int: intinf_t): valprim
fun valprim_intsp (str: string, int: intinf_t): valprim

fun valprim_ptrof (vp: valprim): valprim
fun valprim_ptrof_ptr_offs (vp: valprim, offs: offsetlst): valprim
fun valprim_ptrof_var_offs (vp: valprim, offs: offsetlst): valprim

fun valprim_sizeof (hit: hityp_t): valprim
fun valprim_string (str: string, len: int): valprim

fun valprim_tmp (tmp: tmpvar_t): valprim
fun valprim_tmp_ref (tmp: tmpvar_t): valprim

fun valprim_top (hit: hityp_t): valprim
fun valprim_void (): valprim

(* ****** ****** *)

fun valprim_funclo_make (fl: funlab_t): valprim

(* ****** ****** *)

fun valprim_is_void (vp: valprim): bool

(* ****** ****** *)

fun envmap_find (map: envmap_t, d2v: d2var_t): Option_vt (valprim)

(* ****** ****** *)

datatype patck =
  | PATCKbool of bool
  | PATCKchar of char
  | PATCKcon of d2con_t
  | PATCKexn of d2con_t
  | PATCKfloat of string
  | PATCKint of intinf_t
  | PATCKstring of string

typedef patcklst = List patck

//

fun fprint_patck {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, _: patck): void
overload fprint with fprint_patck

fun fprint_patcklst {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, _: patcklst): void
overload fprint with fprint_patcklst

(* ****** ****** *)

abstype matpnt_t

typedef matpntlst = List matpnt_t
typedef matpntopt = Option matpnt_t

datatype kont =
  | KONTnone
  | KONTtmplab of tmplab_t
  | KONTtmplabint of (tmplab_t, int)
  | KONTcaseof_fail
  | KONTfunarg_fail of funlab_t
  | KONTraise of valprim
  | KONTmatpnt of matpnt_t

typedef kontlst = List kont

fun fprint_kont {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, _: kont): void
overload fprint with fprint_kont

fun fprint_kontlst {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, _: kontlst): void
overload fprint with fprint_kontlst

fun matpnt_kont_get (mpt: matpnt_t): kont
fun matpnt_kont_set (mpt: matpnt_t, _: kont): void
  = "ats_ccomp_matpnt_kont_set"

(* ****** ****** *)

fun emit_kont {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, k: kont): void

fun emit_matpnt {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, mpt: matpnt_t): void

(* ****** ****** *)

datatype instr =
  | INSTRarr_heap of (* heap array allocation *)
      (tmpvar_t, int(*size*), hityp_t(*element type*))
  | INSTRarr_stack of (* stack array allcation *)
      (tmpvar_t, valprim(*size*), hityp_t(*element type*))
  | INSTRassgn_arr of (* array initialization *)
      (valprim(*arr*), valprim(*asz*), tmpvar_t(*elt*), valprim(*tsz*))
  | INSTRassgn_clo of (* closure initialization *)
      (valprim(*clo*), funlab_t, envmap_t)
  | INSTRcall of (* function call *)
      (tmpvar_t, hityp_t, valprim, valprimlst)
  | INSTRcall_tail of (* function tail-call *)
      funlab_t
  | INSTRcond of (valprim, instrlst, instrlst) // conditional
  | INSTRdefine_clo of (d2cst_t, funlab_t)
  | INSTRdefine_fun of (d2cst_t, funlab_t)
  | INSTRdefine_val of (d2cst_t, valprim)
  | INSTRextern of string // external instruction
  | INSTRextval of (string(*name*), valprim)
  | INSTRfreeptr of valprim
  | INSTRfunction of ( // inlined function
      tmpvar_t(*result*), valprimlst(*arg*), instrlst(*body*), tmpvar_t(*ret*)
    ) // end of [INSTRfunction]
  | INSTRfunlab of funlab_t

  | INSTRdynload_file of fil_t
  
  // load instructions
  | INSTRload_ptr of (tmpvar_t, valprim)
  | INSTRload_ptr_offs of (tmpvar_t, valprim, offsetlst)
  | INSTRload_var of (tmpvar_t, valprim)
  | INSTRload_var_offs of (tmpvar_t, valprim, offsetlst)

  | INSTRloop of ( // loop
      tmplab_t(*init*)
    , tmplab_t(*fini*)
    , tmplab_t(*cont*)
    , instrlst(*init*)
    , valprim(*test*), instrlst(*test*)
    , instrlst(*post*)
    , instrlst(*body*)
    ) // end of [INSTRloop]
  | INSTRloopexn of
      (int (*0/1: break/continue*), tmplab_t)

  | INSTRmove_arg of (int, valprim)
  | INSTRmove_con of
      (tmpvar_t, hityp_t, d2con_t, valprimlst(*arg*))
  | INSTRmove_lazy_delay of
      (tmpvar_t, int (*lin*), hityp_t, valprim(*clo*))
  | INSTRmove_lazy_force of
      (tmpvar_t, int (*lin*), hityp_t, valprim(*lazy*))
  | INSTRmove_rec_box of
      (tmpvar_t, hityp_t, labvalprimlst(*arg*))
  | INSTRmove_rec_flt of
      (tmpvar_t, hityp_t, labvalprimlst(*arg*))
  | INSTRmove_ref of (tmpvar_t, valprim)
  | INSTRmove_val of (tmpvar_t, valprim)

  | INSTRpar_spawn of // parallel spawning
      (tmpvar_t(*ret*), valprim(*clo*))
  | INSTRpar_synch of // parallel synchronization
      tmpvar_t (*ret*)

  | INSTRpatck of (valprim, patck, kont) // pattern check
  
  | INSTRraise of valprim // raising an exception

  | INSTRselect of // label selection
      (tmpvar_t, valprim, offsetlst)
  | INSTRselcon of // sum value selection
      (tmpvar_t, valprim, hityp_t, int)
  | INSTRselcon_ptr of // sum value selection
      (tmpvar_t, valprim, hityp_t, int)
  | INSTRswitch of branchlst // switch statement

  // store instructions
  | INSTRstore_ptr of
      (valprim(*ptr*), valprim(*val*))
  | INSTRstore_ptr_offs of
      (valprim(*ptr*), offsetlst, valprim(*val*))
  | INSTRstore_var of
      (valprim(*var*), valprim(*val*))
  | INSTRstore_var_offs of
      (valprim(*var*), offsetlst, valprim(*val*))

  | INSTRtmplabint of (tmplab_t, int)

  | INSTRtrmck_beg of d2cst_t // beg of termination check
  | INSTRtrmck_end of d2cst_t // end of termination check
  | INSTRtrmck_tst of d2cst_t // test a given dynamic constant

  | INSTRtrywith of (instrlst, tmpvar_t, branchlst)

  | INSTRvardec of tmpvar_t

where instrlst = List instr

and instrlst_vt = List_vt instr

and branch = '{
  branch_lab= tmplab_t, branch_inss= instrlst
}

and branchlst = List branch

//

fun fprint_instr {m:file_mode}
   (pf: file_mode_lte (m, w) | out: &FILE m, ins: instr): void
overload fprint with fprint_instr

fun fprint_instrlst {m:file_mode}
   (pf: file_mode_lte (m, w) | out: &FILE m, inss: instrlst): void
overload fprint with fprint_instrlst

fun fprint_branch {m:file_mode}
   (pf: file_mode_lte (m, w) | out: &FILE m, br: branch): void

fun fprint_branchlst {m:file_mode}
   (pf: file_mode_lte (m, w) | out: &FILE m, brs: branchlst): void

fun print_instr (ins: instr): void
fun prerr_instr (ins: instr): void

overload print with print_instr
overload prerr with prerr_instr

fun print_instrlst (inss: instrlst): void
fun prerr_instrlst (inss: instrlst): void

overload print with print_instrlst
overload prerr with prerr_instrlst

(* ****** ****** *)

fun instr_add_arr_heap (
    res: &instrlst_vt
  , tmp: tmpvar_t, asz: int
  , hit_elt: hityp_t
  ) : void

fun instr_add_arr_stack (
    res: &instrlst_vt
  , tmp: tmpvar_t
  , vp_asz: valprim
  , hit_elt: hityp_t
  ) : void

(* ****** ****** *)

fun instr_add_assgn_arr (
    res: &instrlst_vt
  , vp_arr: valprim, vp_asz: valprim, tmp_elt: tmpvar_t, vp_tsz: valprim
  ) : void

fun instr_add_assgn_clo (
    res: &instrlst_vt, vp_clo: valprim, fl: funlab_t, env: envmap_t
  ) : void

(* ****** ****** *)

fun instr_add_call (
    res: &instrlst_vt
  , tmp: tmpvar_t
  , hit_fun: hityp_t
  , vp_fun: valprim
  , vp_arg: valprimlst
  ) : void

fun instr_add_call_tail (res: &instrlst_vt, fl: funlab_t): void

//

fun instr_add_define_clo
  (res: &instrlst_vt, d2c: d2cst_t, fl: funlab_t): void

fun instr_add_define_fun
  (res: &instrlst_vt, d2c: d2cst_t, fl: funlab_t): void

fun instr_add_define_val
  (res: &instrlst_vt, d2c: d2cst_t, vp: valprim): void

fun instr_add_extval
  (res: &instrlst_vt, name: string, vp: valprim): void

fun instr_add_freeptr (res: &instrlst_vt, vp: valprim): void

fun instr_add_patck
  (res: &instrlst_vt, _: valprim, _: patck, _: kont): void

//

fun instr_add_dynload_file (res: &instrlst_vt, fil: fil_t): void

//

fun instr_add_load_ptr_offs
  (res: &instrlst_vt, tmp: tmpvar_t, vp: valprim, offs: offsetlst): void

fun instr_add_load_var_offs
  (res: &instrlst_vt, tmp: tmpvar_t, vp: valprim, offs: offsetlst): void

//

fun instr_add_loop (
    res: &instrlst_vt
  , lab_init: tmplab_t
  , lab_fini: tmplab_t
  , lab_cont: tmplab_t
  , inss_init: instrlst
  , vp_test: valprim
  , inss_test: instrlst
  , inss_post: instrlst
  , inss_body: instrlst
  ) : void

fun instr_add_loopexn (res: &instrlst_vt, knd: int, tl: tmplab_t): void

//

fun instr_add_move_arg (res: &instrlst_vt, arg: int, vp: valprim): void

fun instr_add_move_con (
    res: &instrlst_vt
  , tmp_res: tmpvar_t
  , hit_sum: hityp_t
  , d2c: d2con_t
  , vps_arg: valprimlst
  ) : void

fun instr_add_move_lazy_delay (
    res: &instrlst_vt
  , tmp_res: tmpvar_t
  , lin: int
  , hit_body: hityp_t
  , vp_clo: valprim
  ) : void

fun instr_add_move_lazy_force (
    res: &instrlst_vt
  , tmp_res: tmpvar_t
  , lin: int
  , hit_elt: hityp_t
  , vp_lazy: valprim
  ) : void

fun instr_add_move_rec (
    res: &instrlst_vt
  , tmp_res: tmpvar_t
  , recknd: int
  , hit_sum: hityp_t
  , lvps: labvalprimlst
  ) : void

fun instr_add_move_ref
  (res: &instrlst_vt, tmp_res: tmpvar_t, vp_val: valprim): void

fun instr_add_move_val
  (res: &instrlst_vt, tmp_res: tmpvar_t, vp_val: valprim): void

//

fun instr_add_raise (res: &instrlst_vt, vp_exn: valprim): void

//

fun instr_add_select
  (res: &instrlst_vt, tmp_res: tmpvar_t, vp: valprim, offs: offsetlst): void

fun instr_add_selcon
  (res: &instrlst_vt, tmp_res: tmpvar_t, vp: valprim, hit: hityp_t, i: int): void

fun instr_add_selcon_ptr
  (res: &instrlst_vt, tmp_res: tmpvar_t, vp: valprim, hit: hityp_t, i: int): void

//

fun instr_add_store_ptr_offs
  (res: &instrlst_vt, vp_ptr: valprim, offs: offsetlst, vp_val: valprim): void

fun instr_add_store_var_offs
  (res: &instrlst_vt, vp_mut: valprim, offs: offsetlst, vp_val: valprim): void

//

fun instr_add_switch (res: &instrlst_vt, brs: branchlst): void

fun instr_add_tmplabint (res: &instrlst_vt, tl: tmplab_t, i: int): void

fun instr_add_trywith
  (res: &instrlst_vt, res_try: instrlst, tmp_exn: tmpvar_t, brs: branchlst)
  : void

fun instr_add_vardec (res: &instrlst_vt, tmp: tmpvar_t): void

(* ****** ****** *)

// implemented in [ats_ccomp_trans.dats]
fun instr_add_valprimlst_free (res: &instrlst_vt): void

(* ****** ****** *)

(* implemented in [ats_ccomp_emit.dats] *)

fun emit_label {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, l: $Lab.label_t): void

fun emit_filename {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, fil: $Fil.filename_t): void
  = "ats_ccomp_emit_filename"

fun emit_d2con {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, d2c: d2con_t): void

fun emit_d2cst {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, d2c: d2cst_t): void

fun emit_funlab {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, fl: funlab_t): void

fun emit_tmplab {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, tl: tmplab_t): void

fun emit_tmplabint {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, tl: tmplab_t, i: int): void

fun emit_tmpvar {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, tmp: tmpvar_t): void

(* ****** ****** *)

fun emit_hityp {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, hit: hityp_t): void

fun emit_hityplst {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, hits: hityplst_t): void

fun emit_hityp_ptr {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, hit: hityp_t): void

(* ****** ****** *)

fun emit_valprim_tmpvar {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, tmp: tmpvar_t): void

(* ****** ****** *)

fun emit_valprim {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, vp: valprim): void

fun emit_valprimlst {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, vps: valprimlst): void

(* ****** ****** *)

fun emit_instr {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, ins: instr): void

fun emit_instrlst {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, inss: instrlst): void

(* ****** ****** *)

absviewtype tmpvarmap_vt

fun tmpvarmap_nil(): tmpvarmap_vt
fun tmpvarmap_free (tmps: tmpvarmap_vt): void

fun instr_tmpvarmap_add (m: &tmpvarmap_vt, ins: instr): void
fun instrlst_tmpvarmap_add (m: &tmpvarmap_vt, inss: instrlst): void

fun funentry_tmpvarmap_gen (entry: funentry_t): tmpvarmap_vt

// implemented in [ats_ccomp_util.dats]
fun emit_tmpvarmap_dec_local {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, tmps: !tmpvarmap_vt): int

// implemented in [ats_ccomp_util.dats]
fun emit_tmpvarmap_dec_static {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, tmps: !tmpvarmap_vt): int

// implemented in [ats_ccomp_util.dats]
fun emit_tmpvarmap_markroot {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, tmps: !tmpvarmap_vt): int

(* ****** ****** *)

fun emit_funentry {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, entry: funentry_t): void

fun emit_funentry_prototype {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, entry: funentry_t): void

(* ****** ****** *)

fun emit_mainfun {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, fil: fil_t): void

(* ****** ****** *)

fun ccomp_patck (
  res: &instrlst_vt, vp: valprim, hip: hipat, fail: kont
) : void

fun ccomp_match (
  res: &instrlst_vt, level: int, vp: valprim, hip0: hipat
) : void

//

fun ccomp_exp_arg_body_funlab (
    loc_fun: loc_t, prolog: instrlst
  , hips_arg: hipatlst, hie_body: hiexp
  , fl: funlab_t
  ) : funentry_t

//

fun ccomp_exp (res: &instrlst_vt, hie0: hiexp): valprim

fun ccomp_explst (res: &instrlst_vt, hies: hiexplst): valprimlst

fun ccomp_exp_tmpvar
  (res: &instrlst_vt, hie0: hiexp, tmp: tmpvar_t): void

(* ****** ****** *)

fun tmpnamtbl_add (fullname: string, vp_funclo: valprim): void
fun tmpnamtbl_find (fullname: string): Option_vt (valprim)

(* ****** ****** *)

fun template_name_make (basename: string, hitss: hityplstlst_t): string
fun template_cst_name_make (d2c: d2cst_t, hitss: hityplstlst_t): string
fun template_var_name_make (d2v: d2var_t, hitss: hityplstlst_t): string

fun ccomp_exp_template_cst (
  loc: loc_t, res: &instrlst_vt, hit0: hityp_t, d2c: d2cst_t, hitss: hityplstlst
) : valprim

fun ccomp_exp_template_var (
  loc: loc_t, res: &instrlst_vt, hit0: hityp_t, d2v: d2var_t, hitss: hityplstlst
) : valprim

(* ****** ****** *)

fun ccomp_hiclaulst (
    level: int
  , vps: valprimlst
  , hicls: hiclaulst
  , tmp_res: tmpvar_t
  , fail: kont
  ) : branchlst

(* ****** ****** *)

fun ccomp_tailjoin_funentrylst (loc_all: loc_t, fs: funentrylst): void

(* ****** ****** *)

fun ccomp_declst (res: &instrlst_vt, hids: hideclst): void

(* ****** ****** *)

fun ccomp_main {m:file_mode} (
    pf: file_mode_lte (m, w)
  | flag: int
  , out: &FILE m
  , fil: fil_t
  , hids: hideclst
  ) : void

(* ****** ****** *)

(* end of [ats_ccomp.sats] *)
