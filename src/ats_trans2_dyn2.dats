(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Anairiats - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
** Free Software Foundation; either version 3, or (at  your  option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: November 2007

(* ****** ****** *)

staload Deb = "ats_debug.sats"
staload Err = "ats_error.sats"
staload Lst = "ats_list.sats"
staload NS = "ats_namespace.sats"
staload PM = "ats_posmark.sats"
staload Sym = "ats_symbol.sats"

staload SymEnv = "ats_symenv.sats"
stadef symmap_t = $SymEnv.symmap_t 
typedef symmapref (itm:t@ype) = $SymEnv.symmapref (itm)

staload Syn = "ats_syntax.sats"

(* ****** ****** *)

staload "ats_staexp1.sats"
staload "ats_dynexp1.sats"
staload "ats_staexp2.sats"
staload "ats_dynexp2.sats"
staload "ats_stadyncst2.sats"
staload "ats_trans2_env.sats"

(* ****** ****** *)

staload "ats_trans2.sats"

(* ****** ****** *)

staload "ats_reference.sats"
staload _(*anonymous*) = "ats_reference.dats"

(* ****** ****** *)

staload _(*anonymous*) = "ats_map_lin.dats"
staload _(*anonymous*) = "ats_symenv.dats"

(* ****** ****** *)

#define THISFILENAME "ats_trans2_dyn2.dats"

(* ****** ****** *)

#define nil list_nil
#define cons list_cons
#define :: list_cons

(* ****** ****** *)

overload prerr with $Syn.prerr_d0ynq
overload prerr with $Sym.prerr_symbol

(* ****** ****** *)

fn prerr_loc_error2 (loc: loc_t): void =
  ($Loc.prerr_location loc; prerr ": error(2)")
// end of [prerr_loc_error2]

(* ****** ****** *)

fn dyncstimploc_posmark
  (loc: loc_t, d2c: d2cst_t): void = let
  val loc_d2c = d2cst_loc_get (d2c)
  val loc_begoff = $Loc.location_begpos_toff loc
  val () = $PM.posmark_insert_dyncstimp_beg (loc_begoff, loc_d2c)
  val loc_endoff = $Loc.location_endpos_toff loc
  val () = $PM.posmark_insert_dyncstimp_end (loc_endoff, loc_d2c)
in
  // empty
end // end of [dyncstimploc_posmark]

(* ****** ****** *)

fn symintr_tr (ids: i0delst): void = let
  fun aux (ids: i0delst): void = case+ ids of
    | cons (id, ids) => aux ids where {
        val () = the_d2expenv_add (id.i0de_sym, D2ITEMsym nil)
      } // end of [cons]
    | list_nil () => ()
  // end of [aux]
in
  aux ids
end // end of [symintr_tr]

fn symelim_tr (ids: i0delst): void = let
  fun aux (ids: i0delst): void = case+ ids of
    | cons (id, ids) => aux ids where {
        val () = the_d2expenv_add (id.i0de_sym, D2ITEMsym nil)
      } // end of [cons]
    | nil () => ()
  // end of [aux]
in
  aux ids
end // end of [symelim_tr]

(* ****** ****** *)

fn overload_tr (id: $Syn.i0de, qid: $Syn.dqi0de): void = let
(*
  val () = begin
    prerr "overload_tr: id = ";
    prerr id.i0de_sym; prerr_newline ();
    prerr "overload_tr: qid = ";
    prerr qid.dqi0de_qua; prerr qid.dqi0de_sym; prerr_newline ();
  end // end of [val]
*)
  val ans = 
    the_d2expenv_find_qua (qid.dqi0de_qua, qid.dqi0de_sym)
  // end of [val]
  val d2i = (case+ ans of
    | ~Some_vt d2i => d2i
    | ~None_vt () => begin
        prerr_loc_error2 qid.dqi0de_loc;
        $Deb.debug_prerrf (": %s: overload_tr", @(THISFILENAME));
        prerr ": the dynamic identifier [";
        prerr qid.dqi0de_qua; prerr qid.dqi0de_sym;
        prerr "] is unrecognized.";
        prerr_newline ();
        $Err.abort {d2item} ()
      end // end of [None_vt]
  ) : d2item
  var is_current: bool = false
  val ans = ans where {
    val id_sym = id.i0de_sym
    val ans = the_d2expenv_current_find id_sym
    val ans = (case+ ans of
      | Some_vt _ => (is_current := true; fold@ ans; ans)
      | ~None_vt () => the_d2expenv_pervasive_find id_sym
    ) : d2itemopt_vt
  } // end of [val]
  val d2is = (case+ ans of
    | ~Some_vt d2i => begin case+ d2i of
      | D2ITEMsym d2is => d2is | _ => begin
          prerr_loc_error2 id.i0de_loc;
          $Deb.debug_prerrf (": %s: overload_tr", @(THISFILENAME));
          prerr ": the identifier [";
          prerr id.i0de_sym;
          prerr "] should refer to a symbol but it does not.";
          prerr_newline ();
          $Err.abort {d2itemlst} ()          
        end // end of [_]
      end (* end of [Some_vt] *)
    | ~None_vt () => begin
        prerr_loc_error2 id.i0de_loc;
        $Deb.debug_prerrf (": %s: overload_tr", @(THISFILENAME));
        prerr ": the identifier [";
        prerr id.i0de_sym;
        prerr "] is unrecognized.";
        prerr_newline ();
        $Err.abort {d2itemlst} ()
      end // end of [None_vt]
  ) : d2itemlst
(*
  val () = begin
    prerr "overload_tr: d2is := "; prerr_d2itemlst d2is; prerr_newline ()
  end // end of [val]
*)
  val d2i_new = D2ITEMsym (d2i :: d2is)
in
  if is_current then begin
    the_d2expenv_add (id.i0de_sym, d2i_new)
  end else begin
    the_d2expenv_pervasive_replace (id.i0de_sym, d2i_new)
  end // end of [if]
end (* end of [overload_tr] *)

fn overload_tr_if
  (id: $Syn.i0de, qid: $Syn.dqi0de): void = let
  val level = staload_level_get_level ()
  val topknd = staload_level_get_topkind ()
in
  if (level + topknd) <= 1 then overload_tr (id, qid) else ()
end // end of [overload_tr_if]

(* ****** ****** *)

fn witht1ype_tr
  (w1t: witht1ype): s2expopt = case+ w1t of
  | WITHT1YPEnone () => None ()
  | WITHT1YPEprop s1e => Some (s1exp_tr_dn (s1e, s2rt_prop))
  | WITHT1YPEtype s1e => Some (s1exp_tr_dn (s1e, s2rt_t0ype))
  | WITHT1YPEview s1e => Some (s1exp_tr_dn (s1e, s2rt_view))
  | WITHT1YPEviewtype s1e => Some (s1exp_tr_dn (s1e, s2rt_viewt0ype))
// end of [witht1ype_tr]

(* ****** ****** *)

fn m1thdec_tr (
    r_map: ref mtdmap_t, s2e_self: s2exp, mtd: m1thdec
  ) : m2thdec =
  case+ mtd of
  | M1THDECmtd (loc, sym, _(*dummy*), def) => let
      val d2v_self = d2var_make (loc, $Sym.symbol_SELF)
//
      val def = (case+ def of
        | Some d1e => Some d2e where {
            val (pf1_token | ()) = the_d2expenv_push ()
            val () = the_d2expenv_swap (r_map)
            val (pf2_token | ()) = the_d2expenv_push ()
            val () = the_d2expenv_add_dvar (d2v_self)
            val d2e = d1exp_tr (d1e)
            val () = the_d2expenv_pop (pf2_token | (*none*))
            val () = the_d2expenv_swap (r_map)
            val () = the_d2expenv_pop (pf1_token | (*none*))
          } // end of [Some]
        | None () => None ()
      ) : d2expopt // end of [val]
//
      val () = d2var_typ_set (d2v_self, Some s2e_self)
    in
      M2THDECmtd (loc, sym, d2v_self, def)
    end // end of [M1THDECmtd]
  | M1THDECval (loc, sym, res, def) => let
      val res = s1exp_tr_dn_impredicative res; val def = d1expopt_tr def
    in
      M2THDECval (loc, sym, res, def)
    end // end of [M2THDECval]
  | M1THDECvar (loc, sym, res, def) => let
      val res = s1exp_tr_dn_impredicative res; val def = d1expopt_tr def
    in
      M2THDECvar (loc, sym, res, def)
    end // end of [M2THDECvar]
  | M1THDECimp (loc, sym, def) => let
      val ans = $SymEnv.symmap_ref_search<d2item> (r_map, sym)
      val d2m = (case+ ans of
        | ~Some_vt d2i => let
            val- D2ITEMmtd d2m = d2i in d2m
          end // end of [Some_vt]
        | ~None_vt () => begin
            prerr_loc_error2 (loc);
            prerr ": the dynamic identifier ["; prerr sym; prerr "] is unrecognized.";
            prerr_newline ();
            $Err.abort {d2mtd_t} ()
          end (* end of [None_vt] *)
      ) : d2mtd_t // end of [val]
      val def = d1exp_tr def in M2THDECimp (loc, d2m, def)
    end // end of [M2THDECimp]
// end of [m1thdec_tr]
  
implement m1thdeclst_tr
  (r_map, s2e_self, mtds) = case+ mtds of
  | list_cons (mtd, mtds) => let
      val mtd = m1thdec_tr (r_map, s2e_self, mtd)
    in
      list_cons (mtd, m1thdeclst_tr (r_map, s2e_self, mtds))
    end // end of [list_cons]
  | list_nil () => list_nil ()
// end of [m1thdeclst_tr]

(* ****** ****** *)

fn c1lassdec_tr (
    clsknd: int // mod/obj : 0/1
  , decarg: s2qualst
  , d1c_cls: c1lassdec
  , d1cs_def: s1expdeflst
  ) : c2lassdec = d2c_cls where {
  val loc = d1c_cls.c1lassdec_loc
  val s2vss = s1arglstlst_var_tr (d1c_cls.c1lassdec_arg)
  val s2c_cls =
    s2cst_make_cls (d1c_cls.c1lassdec_sym, loc, s2vss)
  (* end of [val] *)
//
  val s2v_mycls =
    s2var_make_id_srt ($Sym.symbol_MYCLS, s2cst_srt_get s2c_cls)
  (* end of [val] *)
  val () = the_s2expenv_add_svar (s2v_mycls)
//
  val s2e_mycls = aux (s2v_mycls, s2vss) where {
    fun aux (s2v_fun: s2var_t, s2vss_arg: s2varlstlst): s2exp =
      case+ s2vss_arg of
      | list_cons (s2vs_arg, s2vss_arg) => let
          val+ s2e_fun = aux (s2v_fun, s2vss_arg)
          val- S2RTfun (_, s2t_res) = s2e_fun.s2exp_srt
          val+ s2es_arg = $Lst.list_map_fun (s2vs_arg, s2exp_var)
        in
          s2exp_app_srt (s2t_res, s2e_fun, s2es_arg)
        end // end of [val]
      | list_nil () => s2exp_var (s2v_fun)
  } // end of [val]
  val s2e_self = (
    if clsknd > 0 then s2exp_obj_cls_t0ype s2e_mycls // object
                  else s2exp_objmod_cls_type s2e_mycls // module
  ) : s2exp // end of [val]
// (*
  val () = begin
    prerr "c1lassdec_tr: s2e_self = "; prerr s2e_self; prerr_newline ()
  end // end of [val
// *)
//
  val () = aux (d1cs_def) where {
    fun aux (d1cs: s1expdeflst): void = begin
      case+ d1cs of
      | list_cons (d1c, d1cs) => aux (d1cs) where {
          val s2c = s1expdef_tr (None (), d1c)
          val () = the_s2expenv_add_scst (s2c)
        } // end of [cons]
      | list_nil () => ()
    end (* end of [aux] *)
  } // end of [val]
//
  val supclss =
    s1explst_tr_dn_cls (d1c_cls.c1lassdec_suplst)
  (* end of [val] *)
//
  val mtdlst = d1c_cls.c1lassdec_mtdlst
//
  typedef itm = d2item
  var mtdmap: mtdmap_t = $SymEnv.symmap_make {itm} ()
  fun aux0 (
      decarg: s2qualst
    , s2c: s2cst_t, ts2ess: tmps2explstlst
    , mtdmap: !mtdmap_t
  ) : void = let
    val d2c_cls = d2c where {
      val- Some d2c = s2cst_clsdec_get (s2c)
    } // end of [val]
    val d2c_cls = c2lassdec_of_c2lassdec_t (d2c_cls)
    val kis = $SymEnv.symmap_list_inf (!p1) where {
      val r_mtdmap1 = d2c_cls.c2lassdec_mtdmap
      val (vbox pf1 | p1) = ref_get_view_ptr (r_mtdmap1)
    } // end of [val]
    fun loop (
        kis: List_vt @(sym_t, itm), mtdmap: !mtdmap_t
      ) :<cloref1> void = case+ kis of
      | ~list_vt_cons (ki, kis) => let
          val- D2ITEMmtd d2m1 = ki.1
          val loc1 = d2mtd_loc_get (d2m1)
          val sym1 = d2mtd_sym_get (d2m1)
          val knd1 = d2mtd_knd_get (d2m1)
          val decarg1 = d2mtd_decarg_get (d2m1)
          val sublst1 = d2mtd_sublst_get (d2m1)
          val sub = @(decarg1, ts2ess)
          val sublst = list_cons (sub, sublst1)
          val typ1 = d2mtd_typ_get (d2m1)
          val d2m = d2mtd_make (loc1, sym1, knd1, decarg, sublst, typ1)
          val () = $SymEnv.symmap_insert (mtdmap, ki.0, D2ITEMmtd d2m)
        in
          loop (kis, mtdmap)
        end // end of [list_vt_cons]
      | ~list_vt_nil () => ()
    // end of [loop]
  in
    loop (kis, mtdmap)    
  end // end of [aux0]
  fun aux1 (
      decarg: s2qualst
    , supclss: s2explst
    , mtdmap: !mtdmap_t
    ) : void = begin case+ supclss of
    | list_cons (supcls, supclss) => let
        val s2e_head = s2exp_head_get (supcls)
        var ts2ess: tmps2explstlst = TMPS2EXPLSTLSTnil ()
        val s2c = (case+ s2e_head.s2exp_node of
          | S2Ecst s2c => s2c
          | S2Etmpid (s2c, ts2ess1) => (ts2ess := ts2ess1; s2c)
          | _ => let
              val () = prerr "INTERNAL ERROR"
              val () = (
                prerr ": c1lassdec_tr: aux1: s2e_head = "; prerr s2e_head
              ) // end of [val]
              val () = prerr_newline ()
            in
              $Err.abort {s2cst_t} ()
            end // end of [_]
        ) : s2cst_t
        val () = aux0 (decarg, s2c, ts2ess, mtdmap)
      in
        aux1 (decarg, supclss, mtdmap)
      end // end of [list_cons]
    | list_nil () => ()
  end // end of [aux1]
  val () = aux1 (decarg, supclss, mtdmap)
  fun aux2 (
      decarg: s2qualst
    , mtds: m1thdeclst
    , mtdmap: !mtdmap_t
    ) : void = begin case+ mtds of
    | list_cons (mtd, mtds) => let
        val () = (case+ mtd of
          | M1THDECmtd (loc, sym, def_dummy, def) => let
              val def_dummy = d1exp_tr (def_dummy)
              val mtd_typ = d2exp_typ_syn (def_dummy)
              val d2m = d2mtd_make
                (loc, sym, MTDKINDmtd, decarg, list_nil, mtd_typ)
            in
              $SymEnv.symmap_insert<itm> (mtdmap, sym, D2ITEMmtd d2m)
            end // end of [M1THDECmtd]
          | M1THDECval (loc, sym, typ, def) => let
              val mtd_typ = s1exp_tr_dn_t0ype (typ)
              val d2m = d2mtd_make
                (loc, sym, MTDKINDval, decarg, list_nil, mtd_typ)
            in
              $SymEnv.symmap_insert<itm> (mtdmap, sym, D2ITEMmtd d2m)
            end // end of [M1THDECval]
          | M1THDECvar (loc, sym, typ, def) => let
              val mtd_typ = s1exp_tr_dn_viewt0ype (typ)
              val d2m = d2mtd_make
                (loc, sym, MTDKINDvar, decarg, list_nil, mtd_typ)
            in
              $SymEnv.symmap_insert<itm> (mtdmap, sym, D2ITEMmtd d2m)
            end // end of [M1THDECvar]
          | M1THDECimp _ => ()
        ) : void
      in
        aux2 (decarg, mtds, mtdmap)
      end // end of [list_cons]
    | list_nil () => ()
  end // end of [aux2]
  val () = aux2 (decarg, mtdlst, mtdmap)
  val r_mtdmap = ref_make_elt<mtdmap_t> (mtdmap)
//
  val mtdlst = m1thdeclst_tr (r_mtdmap, s2e_self, mtdlst)
//
  val d2c_cls = c2lassdec_make
    (loc, clsknd, s2c_cls, supclss, mtdlst, r_mtdmap)
  (* end of [val] *)
//
  val d2c1_cls = c2lassdec_t_of_c2lassdec d2c_cls
  val () = s2cst_clsdec_set (s2c_cls, Some d2c1_cls)
//
} // end of [c1lassdec_tr]

(* ****** ****** *)

fn v1aldec_tr (d1c: v1aldec, p2t: p2at): v2aldec = let
  val loc = d1c.v1aldec_loc
  val def = d1exp_tr (d1c.v1aldec_def)
  val ann = witht1ype_tr (d1c.v1aldec_ann)
in
  v2aldec_make (loc, p2t, def, ann)
end // end of [v1aldec_tr]

fn v1aldeclst_tr {n:nat}
  (isrec: bool, d1cs: list (v1aldec, n)): v2aldeclst = let
  fun aux1 {n:nat} (d1cs: list (v1aldec, n)): list (p2at, n) =
    case+ d1cs of
    | cons (d1c, d1cs) => cons (p1at_tr d1c.v1aldec_pat, aux1 d1cs)
    | nil () => nil ()
  fun aux2 {n:nat}
    (d1cs: list (v1aldec, n), p2ts: list (p2at, n)): v2aldeclst =
    case+ d1cs of
    | cons (d1c, d1cs) => let
        val+ cons (p2t, p2ts) = p2ts
      in
        cons (v1aldec_tr (d1c, p2t), aux2 (d1cs, p2ts))
      end
    | nil () => nil ()
  val p2ts: list (p2at, n) = aux1 d1cs
  val s2vs = s2varlst_of_s2varlstord (p2atlst_svs_union p2ts)
  val d2vs = d2varlst_of_d2varlstord (p2atlst_dvs_union p2ts)
in
  if isrec then let
    val () = the_d2expenv_add_dvarlst d2vs
    val d2cs = aux2 (d1cs, p2ts)
  in
    the_s2expenv_add_svarlst s2vs; d2cs
  end else let
    val d2cs = aux2 (d1cs, p2ts)
    val () = the_d2expenv_add_dvarlst d2vs
  in
    the_s2expenv_add_svarlst s2vs; d2cs
  end // end of [if]
end (* end of [v1aldeclst_tr] *)

(* ****** ****** *)

fn f1undec_tr (
    level: int
  , decarg: s2qualst
  , d2v: d2var_t
  , d1c: f1undec
  ) : f2undec = let
  val () = d2var_lev_set (d2v, level)
  val () = d2var_decarg_set (d2v, decarg)
  val def = d1exp_tr (d1c.f1undec_def)
(*
  val () = begin
    prerr "f1undec_tr: d2v = "; prerr d2v; prerr_newline ()
    prerr "f1undec_tr: def = "; prerr def; prerr_newline ()
  end
*)
  val ann = witht1ype_tr (d1c.f1undec_ann)
in
  f2undec_make (d1c.f1undec_loc, d2v, def, ann)
end // end of [f1undec_tr]

fn f1undeclst_tr {n:nat} (
    fk: $Syn.funkind
  , level: int
  , decarg: s2qualst
  , d1cs: list (f1undec, n)
  ) : f2undeclst = let
  val isprf = $Syn.funkind_is_proof fk
  val isrec = $Syn.funkind_is_recursive fk
  val d2vs: list (d2var_t, n) = aux1 (isprf, d1cs) where {
    fun aux1 {n:nat}
      (isprf: bool, d1cs: list (f1undec, n))
      : list (d2var_t, n) = begin case+ d1cs of
      | cons (d1c, d1cs) => let
          val d2v = d2var_make (d1c.f1undec_sym_loc, d1c.f1undec_sym)
          val () = d2var_isfix_set (d2v, true)
          val () = d2var_isprf_set (d2v, isprf)
        in
          cons (d2v, aux1 (isprf, d1cs))
        end
      | nil () => nil ()
    end // end of [aux1]
  } // end of [where]
  fun aux2 {n:nat} (
      level: int
    , decarg: s2qualst
    , d2vs: list (d2var_t, n)
    , d1cs: list (f1undec, n)
    ) : list (f2undec, n) =
    case+ d2vs of
    | cons (d2v, d2vs) => let
        val+ cons (d1c, d1cs) = d1cs
        val d2c = f1undec_tr (level, decarg, d2v, d1c)
        val d2cs = aux2 (level, decarg, d2vs, d1cs)
      in
        cons (d2c, d2cs)
      end
    | nil () => nil ()
  val () = if isrec then the_d2expenv_add_dvarlst (d2vs) else ()
  val d2cs = aux2 (level, decarg, d2vs, d1cs)
  val () = if isrec then () else the_d2expenv_add_dvarlst (d2vs)
in
  d2cs
end // end of [f1undeclst_tr]

(* ****** ****** *)

fn v1ardec_tr (d1c: v1ardec): v2ardec = let
  val knd = d1c.v1ardec_knd
  val id = d1c.v1ardec_sym
  val loc_id = d1c.v1ardec_sym_loc
  val d2v_ptr = d2var_make (loc_id, id)
  val s2v_ptr = s2var_make_id_srt (id, s2rt_addr)
  val os2e_ptr = Some (s2exp_var s2v_ptr)
  val () = d2var_addr_set (d2v_ptr, os2e_ptr)
  val typ = (
    case+ d1c.v1ardec_typ of
    | Some s1e => Some (s1exp_tr_dn_impredicative s1e)
    | None () => None ()
  ) : s2expopt
  val wth = (case+ d1c.v1ardec_wth of
    | Some (i0de) => let
        val d2v = d2var_make (i0de.i0de_loc, i0de.i0de_sym)
      in
        D2VAROPTsome d2v
      end // end of [Some]
    | None () => D2VAROPTnone ()
  ) : d2varopt // end of [val]
  val ini = d1expopt_tr d1c.v1ardec_ini
in
  v2ardec_make (d1c.v1ardec_loc, knd, d2v_ptr, s2v_ptr, typ, wth, ini)
end // end of [v1ardec_tr]

fn v1ardeclst_tr (d1cs: v1ardeclst): v2ardeclst = let
  val d2cs = aux d1cs where {
    fun aux (d1cs: v1ardeclst): v2ardeclst =
      case+ d1cs of
      | cons (d1c, d1cs) => cons (v1ardec_tr d1c, aux d1cs)
      | nil () => nil ()
  } // end of [where]
  val () = aux d2cs where {
    fun aux (d2cs: v2ardeclst): void =
      case+ d2cs of
      | cons (d2c, d2cs) => let
          val () = the_s2expenv_add_svar (d2c.v2ardec_svar)
          val () = the_d2expenv_add_dvar (d2c.v2ardec_dvar)
          val () = case+ d2c.v2ardec_wth of
            | D2VAROPTsome d2v => the_d2expenv_add_dvar d2v
            | D2VAROPTnone () => ()
          // end of [val]
        in
          aux d2cs
        end // end of [cons]
      | nil () => ()
  } // end of [where]
in
  d2cs
end // end of [v2ardeclst_tr]

(* ****** ****** *)

fn s1arglst_bind_svarlst
  (loc0: loc_t, s1as: s1arglst, s2vs: s2varlst, sub: &stasub_t)
  : s2varlst = let
  fun aux {n:nat} (
      s1as: list (s1arg, n)
    , s2vs: list (s2var_t, n)
    , sub: &stasub_t
    ) : list (s2var_t, n) = case+ s1as of
    | cons (s1a, s1as) => let
        val+ cons (s2v, s2vs) = s2vs
        val s2v_new = s1arg_var_tr_srt (s1a, s2var_srt_get s2v)
        val () =
          if ~(s2var_srt_get s2v <= s2var_srt_get s2v_new) then begin
            prerr_loc_error2 s1a.s1arg_loc;
            $Deb.debug_prerrf (": %s: s1arglst_bind_svarlst", @(THISFILENAME));
            prerr ": the ascribed sort for the static variable [";
            prerr s1a.s1arg_sym;
            prerr "] is incorrect.";
            prerr_newline ();
            $Err.abort {void} ()
          end
        val s2e_new = s2exp_var (s2v_new)
        val () = sub := stasub_add (sub, s2v, s2e_new)
      in
        cons (s2v_new, aux (s1as, s2vs, sub))
      end // end of [cons]
    | nil () => nil ()
  // end of [aux]
  val ns1as = $Lst.list_length s1as and ns2vs = $Lst.list_length s2vs
in
  if ns1as <> ns2vs then begin
    prerr_loc_error2 loc0;
    if ns1as < ns2vs then prerr ": more static arguments should be given.";
    if ns1as > ns2vs then prerr ": less static arguments should be given.";
    prerr_newline ();
    $Err.abort {s2varlst} ()
  end else begin
    aux (s1as, s2vs, sub)
  end // end of [if]
end (* end of [s1arglst_bind_svarlst] *)
      
(* ****** ****** *)

fn s1explst_bind_svarlst
  (loc0: loc_t, s1es: s1explst, s2vs: s2varlst, sub: &stasub_t)
  : s2explst = let
  fun aux {n:nat} (
      s1es: list (s1exp, n)
    , s2vs: list (s2var_t, n)
    , sub: &stasub_t
    ) : s2explst = begin case+ s1es of
    | cons (s1e, s1es) => let
        val+ cons (s2v, s2vs) = s2vs; val s2e = s1exp_tr_up (s1e)
        val s2t_s2v = s2var_srt_get s2v and s2t_s2e = s2e.s2exp_srt
        val () = if ~(s2t_s2e <= s2t_s2v) then begin
          prerr_loc_error2 s1e.s1exp_loc;
          $Deb.debug_prerrf (": %s: s1explst_bind_svarlst", @(THISFILENAME));
          prerr ": the sort of the static expression ["; prerr s1e;
          prerr "] is expected to be ["; prerr s2t_s2v;
          prerr "], but it is ["; prerr s2t_s2e; prerr "] instead.";
          prerr_newline ();
          $Err.abort {void} ()
        end // end of [val]
        val () = sub := stasub_add (sub, s2v, s2e)
      in
        list_cons (s2e, aux (s1es, s2vs, sub))
      end // end of [cons]
    | nil () => nil ()
  end // end of [aux]
  val ns1es = $Lst.list_length s1es and ns2vs = $Lst.list_length s2vs
in
  if ns1es <> ns2vs then begin
    prerr_loc_error2 loc0;
    if ns1es < ns2vs then prerr ": more template arguments should be given.";
    if ns1es > ns2vs then prerr ": less template arguments should be given.";
    prerr_newline ();
    $Err.abort {s2explst} ()
  end else begin
    aux (s1es, s2vs, sub)
  end // end of [if]
end (* end of [s1explst_bind_svarlst] *)

(* ****** ****** *)

fun d1exp_tr_ann (d1e0: d1exp, s2e0: s2exp): d2exp = begin
  case+ s2e0.s2exp_node of
  | S2Euni (s2vs, s2ps, s2e) => begin
    case+ d1e0.d1exp_node of
    | D1Elam_sta_ana (loc_arg, arg, body) => let
        var sub: stasub_t = stasub_nil
        val s2vs = s1arglst_bind_svarlst (loc_arg, arg, s2vs, sub)
        val (pf_s2expenv | ()) = the_s2expenv_push ()
        val () = the_s2expenv_add_svarlst s2vs
        val s2ps = s2explst_subst (sub, s2ps)
        val s2e = s2exp_subst (sub, s2e)
        val body = d1exp_tr_ann (body, s2e)
        val () = the_s2expenv_pop (pf_s2expenv | (*none*))
      in
        d2exp_lam_sta (d1e0.d1exp_loc, s2vs, s2ps, body)
      end // end of [D1Elam_sta_ana]
    | _ => let
        val d2e0 = d1exp_tr_ann (d1e0, s2e)
      in
        d2exp_lam_sta (d1e0.d1exp_loc, s2vs, s2ps, d2e0)
      end // end of [_]
    end (* end of [S2Euni] *)
  | S2Efun (fc, lin1, s2fe, npf1, s2es_arg, s2e_res) => begin
    case+ d1e0.d1exp_node of
    | D1Elam_dyn (lin2, p1t_arg, d1e_body) => let
        val @(p2ts_arg, d2e_body) = d1exp_arg_body_tr_ann (
          d1e0, fc, lin1, s2fe, npf1, s2es_arg, s2e_res, lin2, p1t_arg, d1e_body
        ) // end of [val]
      in
        d2exp_lam_dyn (d1e0.d1exp_loc, lin1, npf1, p2ts_arg, d2e_body)
      end // end of [D2Elam_dyn]
    | D1Elaminit_dyn (lin2, p1t_arg, d1e_body) => let
        val @(p2ts_arg, d2e_body) = d1exp_arg_body_tr_ann (
          d1e0, fc, lin1, s2fe, npf1, s2es_arg, s2e_res, lin2, p1t_arg, d1e_body
        ) // end of [val]
      in
        d2exp_laminit_dyn (d1e0.d1exp_loc, lin1, npf1, p2ts_arg, d2e_body)
      end // end of [D2Elam_dyn]
    | _ => d2exp_ann_type (d1e0.d1exp_loc, d1exp_tr d1e0, s2e0)
    end // end of [S2Efun]
  | _ => d2exp_ann_type (d1e0.d1exp_loc, d1exp_tr d1e0, s2e0)
end // end of [d1exp_tr_ann]

and d1exp_arg_body_tr_ann (
    d1e0: d1exp
  , fc: $Syn.funclo
  , lin1: int
  , s2fe: s2eff
  , npf1: int
  , s2es_arg: s2explst
  , s2e_res: s2exp
  , lin2: int
  , p1t_arg: p1at
  , d1e_body: d1exp
  ) : @(p2atlst, d2exp) = let
  val () = case+ fc of
    | $Syn.FUNCLOclo knd when knd = 0 => begin
        prerr_loc_error2 d1e0.d1exp_loc;
        prerr ": function is given an unboxed closure type.";
        $Err.abort {void} ()
      end // end of [FUNCLOclo when ...]
    | _ => ()
  // end of [val]
  val () = if lin1 <> lin2 then begin
    prerr_loc_error2 d1e0.d1exp_loc;
    $Deb.debug_prerrf (": %s: d1exp_tr_ann", @(THISFILENAME));
    if lin1 < lin2 then prerr ": linear function is given a nonlinear type.";
    if lin1 > lin2 then prerr ": nonlinear function is given a linear type.";
    prerr_newline ();
    $Err.abort {void} ()
  end // end of [val]
  var wths1es = WTHS1EXPLSTnil ()
  val p2t_arg = p1at_arg_tr (p1t_arg, wths1es)
  val () = // check for refval types
    if wths1explst_is_none wths1es then () else begin
      prerr_loc_error2 p1t_arg.p1at_loc;
      prerr ": the function argument cannot be ascribed refval types.";
      prerr_newline ();
      $Err.abort {void} ()
    end
  // end of [val]
  var npf2: int = 0
  val p2ts_arg = (
    case+ p2t_arg.p2at_node of
    | P2Tlist (npf, p2ts) => (npf2 := npf; p2ts)
    | _ => cons (p2t_arg, nil ())
  ) : p2atlst
  val () = if npf1 <> npf2 then begin // check for pfarity match
    prerr_loc_error2 d1e0.d1exp_loc;
    $Deb.debug_prerrf (": %s: d1exp_tr_ann", @(THISFILENAME));
    if npf1 < npf2 then prerr ": less proof arguments are expected.";
    if npf1 > npf2 then prerr ": more proof arguments are expected.";
    prerr_newline ();
    $Err.abort {void} ()
  end // end of [val]
  val p2ts_arg = let
    val ns2es = $Lst.list_length s2es_arg
    val np2ts = $Lst.list_length p2ts_arg
    fun aux {n:nat}
      (p2ts: list (p2at, n), s2es: list (s2exp, n)): list (p2at, n) =
      case+ p2ts of
      | cons (p2t, p2ts) => let
          val+ cons (s2e, s2es) = s2es
        in
          cons (p2at_ann (p2t.p2at_loc, p2t, s2e), aux (p2ts, s2es))
        end
      | nil () => nil ()            
  in
    if ns2es <> np2ts then begin
      prerr_loc_error2 d1e0.d1exp_loc;
      $Deb.debug_prerrf (": %s: d1exp_tr_ann", @(THISFILENAME));
      if ns2es < np2ts then prerr ": less arguments are expected.";
      if ns2es > np2ts then prerr ": more arguments are expected.";
      prerr_newline ();
      $Err.abort {p2atlst} ()
    end else begin
      aux (p2ts_arg, s2es_arg)
    end // end of [if]
  end : p2atlst // end of [val]
  val (pf_env2 | ()) = trans2_env_push ()
  val () = let
    val s2vs = s2varlst_of_s2varlstord p2t_arg.p2at_svs
  in
    the_s2expenv_add_svarlst s2vs
  end // end of [val]
  val () = let
    val d2vs = d2varlst_of_d2varlstord p2t_arg.p2at_dvs
  in
    the_d2expenv_add_dvarlst d2vs
  end // end of [val]
  val d2e_body = d1exp_tr_ann (d1e_body, s2e_res)
  val () = trans2_env_pop (pf_env2 | (*none*))
  val loc_body = d2e_body.d2exp_loc
  val d2e_body = d2exp_ann_seff (loc_body, d2e_body, s2fe)
  val d2e_body = d2exp_ann_funclo (loc_body, d2e_body, fc)
in
  @(p2ts_arg, d2e_body)
end // end of [d2exp_tr_arg_body_ann]

(* ****** ****** *)

fn m1acdef_tr
  (knd: int, d2m: d2mac_t, d1c: m1acdef): void = let
  val loc = d1c.m1acdef_loc and name = d1c.m1acdef_sym
  val (pf_d2expenv | ()) = the_d2expenv_push ()
  val () = aux (d2mac_arglst_get d2m) where {
    fun aux (args: macarglst): void = begin case+ args of
      | cons (arg, args) => let
          val () = case+ arg of
            | MACARGone (d2v) => the_d2expenv_add_dmac_var (d2v)
            | MACARGlst (_(*n*), d2vs) => the_d2expenv_add_dmac_varlst (d2vs)
        in
          aux args
        end // end of [cons]
      | nil () => ()
    end // end of [aux]
  } // end of [where]
  val () = if knd > 0 then macro_level_dec (loc)
  val def = d1exp_tr (d1c.m1acdef_def)
  val () = if knd > 0 then macro_level_inc (loc)
  val () = the_d2expenv_pop (pf_d2expenv | (*none*))
  val () = d2mac_def_set (d2m, def)
in
  // empty
end // end of [m1acdef_tr]

fun m1acdeflst_tr (knd: int, d1cs: m1acdeflst): void = let
  // knd: 0/1/2 => short/long/long rec
  fn aux1 (d1c: m1acdef):<cloptr1> d2mac_t = let
    val args = auxarglst d1c.m1acdef_arg where {
      fun auxarg (arg: $Syn.m0acarg): macarg = let
        fn f (x: $Syn.i0de): d2var_t = d2var_make (x.i0de_loc, x.i0de_sym)
      in
        case+ arg of
        | $Syn.M0ACARGone (x) => MACARGone (f x)
        | $Syn.M0ACARGlst (xs) => let
            val d2vs = $Lst.list_map_fun (xs, f); val n = $Lst.list_length d2vs
          in
            MACARGlst (n, d2vs)
          end
      end
      fun auxarglst (args: $Syn.m0acarglst): macarglst = begin case+ args of
        | cons (arg, args) => cons (auxarg arg, auxarglst args) | nil () => nil ()
      end // end of [auxarglst]
    } // end of [where]
    val def = d2exp_empty ($Loc.location_none)
    val d2m = d2mac_make (
      d1c.m1acdef_loc, d1c.m1acdef_sym, knd, args, def
    ) // end of [d2mac_make]
  in
    // [knd > 1] : recursive
    if knd > 1 then the_d2expenv_add_dmac_def (d2m); d2m
  end // end of [aux1]
  fun aux2 {n:nat}
    (d2ms: list (d2mac_t, n), d1cs: list (m1acdef, n))
    : void = begin case+ d2ms of
    | cons (d2m, d2ms) => let
        val+ cons (d1c, d1cs) = d1cs
        val knd = d2mac_kind_get (d2m)
      in
        m1acdef_tr (knd, d2m, d1c);
        // [knd <= 1] : non-recursive
        if knd <= 1 then the_d2expenv_add_dmac_def (d2m);
        aux2 (d2ms, d1cs)
      end
    | nil () => ()
  end // end of [aux2]
  val d2ms = $Lst.list_map_cloptr (d1cs, aux1)
in
  aux2 (d2ms, d1cs)
end // end of [m1acdeflst_tr]

(* ****** ****** *)

viewtypedef d2cstlst_vt = List_vt d2cst_t

fun d1exp_arity_check (d1e: d1exp, ns: List int): bool = let
  fn* aux1 (d1e: d1exp, ns: List int): bool = begin
    case+ ns of list_cons (n, ns) => aux2 (d1e, n, ns) | list_nil () => true
  end // end of [aux1]

  and aux2 (d1e: d1exp, n: int, ns: List int): bool = begin
(*
    prerr "d1exp_arith_check: n = "; prerr n; prerr_newline ();
    prerr "d1exp_arith_check: d1e = "; prerr_d1exp d1e; prerr_newline ();
*)
    case+ d1e.d1exp_node of
    | D1Elam_dyn (_(*lin*), p1t, d1e) => let
        val narg = (case+ p1t.p1at_node of
          | P1Tlist (_(*npf*), p1ts) => $Lst.list_length (p1ts) | _ => 1
        ) : int
      in
        if (n = narg) then aux1 (d1e, ns) else false
      end // end of [D1Elam_dyn]
    | D1Elam_met (_(*loc*), _(*met*), d1e) => aux2 (d1e, n, ns)
    | D1Elam_sta_ana (_(*loc*), _(*s1as*), d1e) => aux2 (d1e, n, ns)
    | D1Elam_sta_syn (_(*loc*), _(*s1qs*), d1e) => aux2 (d1e, n, ns)
    | _ => false
  end // end of [aux2]
in
  aux1 (d1e, ns)
end // end of [d1exp_arity_check]

(* ****** ****** *)

fn i1mpdec_tr_d2cst_select
  (d1c: i1mpdec, d2is: d2itemlst): d2cst_t = let
  fun aux (d2is: d2itemlst)
    :<cloptr1> d2cstlst_vt = begin case+ d2is of
    | list_cons (d2i, d2is) => begin case+ d2i of
      | D2ITEMcst d2c => let
          val ns = d2cst_arilst_get (d2c)
          val ismat = d1exp_arity_check (d1c.i1mpdec_def, ns)
        in 
          if (ismat) then list_vt_cons (d2c, aux d2is) else aux d2is
        end
      | _ => aux d2is
      end // end of [list_cons]
    | list_nil () => list_vt_nil ()
  end // end of [aux]
  val d2cs = aux (d2is)
in
  case+ d2cs of
  | ~list_vt_cons (d2c1, d2cs) => begin case+ d2cs of
    | ~list_vt_nil () => d2c1
    | ~list_vt_cons (d2c2, d2cs) => let
        val qid = d1c.i1mpdec_qid
        val q = qid.impqi0de_qua and id = qid.impqi0de_sym
      in
        prerr_loc_error2 d1c.i1mpdec_loc;
        prerr ": the dynamic constants [";
        prerr d2c1; prerr "] and [";
        prerr d2c2; prerr "] cannot be resolved for [";
        $Syn.prerr_d0ynq q; $Sym.prerr_symbol id; prerr "].";
        prerr_newline ();
        $Lst.list_vt_free__boxed (d2cs);
        $Err.abort {d2cst_t} ()
      end // end of [list_vt_cons]
    end // end of [list_vt_cons]
  | ~list_vt_nil () => let
      val qid = d1c.i1mpdec_qid
      val q = qid.impqi0de_qua and id = qid.impqi0de_sym
    in
      prerr_loc_error2 d1c.i1mpdec_loc;
      prerr ": no dynamic constant can be found for [";
      $Syn.prerr_d0ynq q; $Sym.prerr_symbol id; prerr "].";
      prerr_newline ();
      $Err.abort {d2cst_t} ()
    end // end of [list_vt_nil]
end // end of [i1mpdec_tr_d2cst_select]

fn i1mpdec_tr
  (loc0: loc_t, i1mparg: s1arglstlst, d1c: i1mpdec): i2mpdec = let
  val t1mparg = d1c.i1mpdec_tmparg
  val () = case+ (i1mparg, t1mparg) of
    | (cons _, cons _) => begin
        prerr_loc_error2 loc0;
        prerr ": template implementation and instantiation may not be combined.";
        prerr_newline ();
        $Err.abort {void} ()
      end // end of [cons, cons]
    | (_, _) => ()
  // end of [val]
  val qid = d1c.i1mpdec_qid
  val q = qid.impqi0de_qua and id = qid.impqi0de_sym
  val d2c = begin
    case+ the_d2expenv_find_qua (q, id) of
    | ~Some_vt d2i => begin case+ d2i of
      | D2ITEMcst d2c => d2c
      | D2ITEMsym (d2is) => i1mpdec_tr_d2cst_select (d1c, d2is)
      | _ => begin
          prerr_loc_error2 d1c.i1mpdec_loc;
          $Deb.debug_prerrf (": %s: i1mpdec_tr", @(THISFILENAME));
          prerr ": the identifier [";
          prerr q; prerr id;
          prerr "] should refer to a dynamic constant but it does not.";
          prerr_newline ();
          $Err.abort {d2cst_t} ()
        end // end of [_]
      end (* end of [Some_vt] *)
    | ~None_vt () => begin
        prerr_loc_error2 d1c.i1mpdec_loc;
        $Deb.debug_prerrf (": %s: i1mpdec_tr", @(THISFILENAME));
        prerr ": the dynamic identifier [";
        prerr q; prerr id;
        prerr "] is unrecognized.";
        prerr_newline ();
        $Err.abort {d2cst_t} ()
      end // end of [None_vt]
  end // end of [val]
//
  val () = dyncstimploc_posmark (qid.impqi0de_loc, d2c)
//
(*
  // automatic instantiation is not supported because it can otherwise readily
  // lead to confusion as to whether an implementation is actually compiled.
  fun aux1
    (s2vpss: s2qualst, s2e: s2exp, out: &s2qualst): s2exp = begin
    case+ s2vpss of
    | cons (s2vps, s2vpss) => let
        val (sub, s2vs) = stasub_extend_svarlst (stasub_nil, s2vps.0)
        val s2ps = s2explst_subst (sub, s2vps.1)
        val s2e = s2exp_subst (sub, s2e)
      in
        out := @(s2vs, s2ps) :: out; aux1 (s2vpss, s2e, out)
      end
    | nil () => (out := s2qualst_reverse out; s2e)
  end // end of [aux1]
*)
  fun aux2_imp (
      loc0: loc_t
    , args: s1arglstlst
    , s2vpss: s2qualst
    , s2e: s2exp
    , out_imp: &s2qualst
    ) :<cloptr1> s2exp = begin case+ args of
    | cons (arg, args) => begin case+ s2vpss of
      | cons (s2vps, s2vpss) => let
          var sub: stasub_t = stasub_nil
          val s2vs = s1arglst_bind_svarlst (loc0, arg, s2vps.0, sub)
          val () = the_s2expenv_add_svarlst s2vs
          val s2ps = s2explst_subst (sub, s2vps.1)
          val s2e = s2exp_subst (sub, s2e)
          val () = out_imp := @(s2vs, s2ps) :: out_imp
        in
          aux2_imp (loc0, args, s2vpss, s2e, out_imp)
        end // end of [cons]
      | nil () => begin
          prerr_loc_error2 loc0;
          $Deb.debug_prerrf (": %s: i1mpdec_tr: aux2_imp", @(THISFILENAME));
          prerr ": the implementation for [";
          prerr q; prerr id;
          prerr "] should be applied to less template arguments.";
          prerr_newline ();
          $Err.abort {s2exp} ()
        end // end of [nil]
      end // end of [cons]
    | nil () => let
        val () = case+ s2vpss of
          | cons _ => begin
              prerr_loc_error2 loc0;
              $Deb.debug_prerrf (": %s: i1mpdec_tr: aux2_imp", @(THISFILENAME));
              prerr ": the implementation for [";
              prerr q; prerr id;
              prerr "] should be applied to more template arguments.";
              prerr_newline ();
              $Err.abort {void} ()
            end // end of [cons]
          | nil () => ()
      in
        s2e // no automatic instantiation
      end // end of [nil]
  end // end of [aux2_imp]
  fun aux2_tmp (
      loc0: loc_t
    , args: s1explstlst
    , s2vpss: s2qualst
    , s2e: s2exp
    , out_tmparg: &s2explstlst
    , out_tmpgua: &s2explstlst
    ) :<cloptr1> s2exp = begin case+ args of
    | cons (arg, args) => begin case+ s2vpss of
      | cons (s2vps, s2vpss) => let
          var sub: stasub_t = stasub_nil
          val s2es = s1explst_bind_svarlst (loc0, arg, s2vps.0, sub)
          val s2ps = s2explst_subst (sub, s2vps.1)
          val s2e = s2exp_subst (sub, s2e)
          val () = out_tmparg := s2es :: out_tmparg
          val () = out_tmpgua := s2ps :: out_tmpgua
        in
          aux2_tmp (loc0, args, s2vpss, s2e, out_tmparg, out_tmpgua)
        end // end of [cons]
      | nil () => begin
          prerr_loc_error2 loc0;
          $Deb.debug_prerrf (": %s: i1mpdec_tr: aux2_tmp", @(THISFILENAME));
          prerr ": the implementation for [";
          prerr q; prerr id;
          prerr "] should be applied to less template arguments.";
          prerr_newline ();
          $Err.abort {s2exp} ()
        end // end of [nil]
      end // end of [cons]
    | nil () => let
        val () = case+ s2vpss of
          | cons _ => begin
              prerr_loc_error2 loc0;
              $Deb.debug_prerrf (": %s: i1mpdec_tr: aux2_tmp", @(THISFILENAME));
              prerr ": the implementation for [";
              prerr q; prerr id;
              prerr "] should be applied to more template arguments.";
              prerr_newline ();
              $Err.abort {void} ()
            end // end of [cons]
          | nil () => ()
      in
        s2e // no automatic instantiation
      end // end of [nil]
  end // end of [aux2_tmp]
  val loc_id = qid.impqi0de_loc
  val decarg = d2cst_decarg_get d2c and s2e_d2c = d2cst_typ_get d2c
  val () = begin case+ decarg of
    | cons _ => begin case+ (i1mparg, t1mparg) of
      | (nil (), nil ()) => begin
          prerr_loc_error2 loc0;
          prerr ": the dynamic constant [";
          prerr d2c; prerr "] requires a template implementation";
          prerr_newline ();
          $Err.abort {void} ()
        end // end of [nil, nil]
      | (_, _) => ()
      end // end of [cons]
    | _ => ()
  end // end of [val]
  var out_imp: s2qualst = nil ()
  var out_tmparg: s2explstlst = nil ()
  var out_tmpgua: s2explstlst = nil ()
  val s2e = s2e_d2c
  val (pf_s2expenv | ()) = the_s2expenv_push ()
  val () = begin
    case+ decarg of cons _ => template_level_inc () | nil _ => ()
  end // end of [val]
  val s2e = (case+ i1mparg of
    | cons _ => aux2_imp (loc_id, i1mparg, decarg, s2e, out_imp)
    | nil () => s2e
  ) : s2exp
  val s2e = (case+ t1mparg of
    | cons _ => aux2_tmp
        (loc_id, t1mparg, decarg, s2e, out_tmparg, out_tmpgua)
    | nil () => s2e
  ) : s2exp        
  // val out_imp = $Lst.list_reverse (out_imp) // a serious bug!!!
  val out_imp = s2qualst_reverse (out_imp)
  val () = s2qualst_tmplev_set (out_imp, template_level_get ())
  val out_tmparg = $Lst.list_reverse (out_tmparg: s2explstlst)
  val out_tmpgua = $Lst.list_reverse (out_tmpgua: s2explstlst)
  val d2e = d1exp_tr_ann (d1c.i1mpdec_def, s2e)
  val () = begin
    case+ decarg of cons _ => template_level_dec () | nil _ => ()
  end // end of [val]
  val () = the_s2expenv_pop (pf_s2expenv | (*none*))
  val () = d2cst_def_set (d2c, Some d2e)
in
  i2mpdec_make (
    d1c.i1mpdec_loc, loc_id, d2c, out_imp, out_tmparg, out_tmpgua, d2e
  ) // end of [i2mpdec_make]
end // end of [i1mpdec_tr]

(* ****** ****** *)

fn s1taload_tr
  (loc0: loc_t,
   idopt: symopt_t, fil: fil_t, loaded: int,
   d1cs: d1eclst)
  : d2ec = let
(*
  val () = prerr "s1taload_tr: staid = "
  val () = case+ idopt of
    | Some id => prerr id | None () => prerr "None"
  // end of [val]
  val () = prerr_newline ()
  val () = begin
    prerr "s1taload_tr: filename = "; $Fil.prerr_filename fil;
    prerr_newline ()
  end // end of [val]
*)
  var d2cs_loaded: d2eclst = list_nil ()
  val fil_sym = $Fil.filename_full_sym fil
  val staloadknd = (
    case+ idopt of Some _ => 1 | None _ => 0 (*opened*)
  ) : int // end of [val]
  val (pf_token | ()) = staload_level_push (staloadknd)
  val ans = d2eclst_namespace_find fil_sym
  val od2cs = (case+ ans of
    | ~Some_vt d2cs => let
        val () = d2cs_loaded := d2cs in None ()
      end // end of [Some_vt]
    | ~None_vt _ => let
        val () = trans2_env_save ()
//
        val flag = $PM.posmark_pause_get ()
        val d2cs = d1eclst_tr d1cs
        val () = $PM.posmark_resume_set (flag)
//
        val () = trans2_env_namespace_add_topenv (fil_sym)
        val () = trans2_env_restore ()
        val () = d2eclst_namespace_add (fil_sym, d2cs)
      in
        Some d2cs
      end // end of [None_vt]
  ) : Option d2eclst
  val () = case+ idopt of
    | Some id => the_s2expenv_add (id, S2ITEMfil fil)
    | None () => begin
        $NS.the_namespace_add fil_sym (* opened file *)
      end // end of [None]
  // end of [val]
  val () = (case+ d2cs_loaded of
    | list_nil () => () | list_cons _ => let
        val level = staload_level_get_level ()
        val topknd = staload_level_get_topkind ()
      in
        if (level + topknd <= 1) then loop (d2cs_loaded)
      end // end of [list_cons]
  ) where {
    fun loop (d2cs: d2eclst)
      : void = begin case+ d2cs of
      | list_cons (d2c, d2cs) => loop d2cs where {
          val () = case+ d2c.d2ec_node of
            | D2Csymintr ids => symintr_tr (ids)
            | D2Csymelim ids => symelim_tr (ids)
            | D2Coverload (id, qid) => overload_tr (id, qid)
            | _ => ()
          // end of [val]
        } // end of [list_cons]
      | list_nil () => ()
    end // end of [loop]
  } // end of [val]
  val () = staload_level_pop (pf_token | (*none*))
in
  d2ec_staload (loc0, fil, od2cs)
end // end of [s1taload_tr]

(* ****** ****** *)

implement d1ec_tr (d1c0) = begin
  case+ d1c0.d1ec_node of
  | D1Cnone () => d2ec_none (d1c0.d1ec_loc)
  | D1Clist d1cs => begin
      d2ec_list (d1c0.d1ec_loc, d1eclst_tr d1cs)
    end // end of [D1Clist]
  | D1Cinclude d1cs => let
      val flag = $PM.posmark_pause_get ()
      val d2cs = d1eclst_tr d1cs
      val () = $PM.posmark_resume_set (flag)
    in
      d2ec_include (d1c0.d1ec_loc, d2cs)
    end // end of [D1Cinclude]
  | D1Csymintr ids => let
      val () = symintr_tr (ids) in
      d2ec_symintr (d1c0.d1ec_loc, ids)
    end // end of [D1Csymintr]
  | D1Csymelim ids => let
      val () = symelim_tr (ids) in
      d2ec_symelim (d1c0.d1ec_loc, ids)
    end // end of [D1Csymelim]
  | D1Ce1xpdef (id, def) => begin
      the_s2expenv_add (id, S2ITEMe1xp def);
      the_d2expenv_add (id, D2ITEMe1xp def);
      d2ec_none (d1c0.d1ec_loc)
    end // end of [D1Ce1xpdef]
  | D1Cdatsrts (para, d1cs) => begin
      d1atsrtdeclst_tr d1cs; d2ec_none (d1c0.d1ec_loc)
    end // end of [D1Cdatsrts]
  | D1Csrtdefs d1cs => begin
      s1rtdeflst_tr d1cs; d2ec_none (d1c0.d1ec_loc)
    end // end of [D1Csrtdefs]
  | D1Cstacons (absknd, d1cs) => begin
      s1taconlst_tr (absknd, d1cs); d2ec_none (d1c0.d1ec_loc)
    end // end of [D1Cstacons]
  | D1Cstacsts d1cs => begin
      s1tacstlst_tr d1cs; d2ec_none (d1c0.d1ec_loc)
    end // end of [D1Cstacsts]
  | D1Cstavars d1cs => let
      val d2cs = s1tavarlst_tr d1cs in
      d2ec_stavars (d1c0.d1ec_loc, d2cs)
    end // end of [D1Cstavars]
  | D1Csexpdefs (os1t, d1cs) => d2c where {
      val () = s1expdeflst_tr (s1rtopt_tr os1t, d1cs)
      val d2c = d2ec_none (d1c0.d1ec_loc)
    } // end of [D1Csexpdefs]
  | D1Csaspdec (d1c) => begin
      d2ec_saspdec (d1c0.d1ec_loc, s1aspdec_tr d1c)
    end // end of [D1Csaspdec]
  | D1Cdcstdecs (dck, decarg, d1cs) => let
      val (pf_s2expenv | ()) = the_s2expenv_push ()
      val s2vpss = s1qualstlst_tr (decarg)
      val d2cs = d1cstdeclst_tr (dck, s2vpss, d1cs)
      val () = the_s2expenv_pop (pf_s2expenv | (*none*))
    in
      d2ec_dcstdec (d1c0.d1ec_loc, dck, d2cs)
    end // end of [D1Cdcstdecs]
  | D1Cdatdecs (dtk, d1cs_dat, d1cs_def) => let
      val s2cs = d1atdeclst_tr (dtk, d1cs_dat, d1cs_def)
    in
      d2ec_datdec (d1c0.d1ec_loc, dtk, s2cs)
    end // end of [D1Cdatdecs]
  | D1Cexndecs (d1cs) => let
      val d2cs = e1xndeclst_tr d1cs in
      d2ec_exndec (d1c0.d1ec_loc, d2cs)
    end // end of [D1Cexndecs]
  | D1Cclassdec
      (clsknd, decarg, d1c_cls, d1cs_def) => let
      val (pf_s2expenv | ()) = the_s2expenv_push ()
      val () = begin
        case+ decarg of cons _ => template_level_inc () | nil _ => ()
      end // end of [val]
      val s2vpss = s1qualstlst_tr (decarg)
      val () = s2qualst_tmplev_set (s2vpss, template_level_get ())
      val d2c_cls = c1lassdec_tr (clsknd, s2vpss, d1c_cls, d1cs_def)
      val () = the_s2expenv_pop (pf_s2expenv | (*none*))
      val () = begin
        case+ decarg of cons _ => template_level_dec () | nil _ => ()
      end // end of [val]
      val () = the_s2expenv_add_scst (d2c_cls.c2lassdec_cst)
    in
      d2ec_classdec (d1c0.d1ec_loc, d2c_cls)
    end // end of [D1Cfundecs]
  | D1Coverload (id, qid) => let
      val () = overload_tr_if (id, qid) in
      d2ec_overload (d1c0.d1ec_loc, id, qid)
    end // end of [D1Coverload]
  | D1Cextype (name, s1e_def) => let
      val s2e_def = s1exp_tr_dn_viewt0ype s1e_def
    in
      d2ec_extype (d1c0.d1ec_loc, name, s2e_def)
    end // end of [D1Cextype]
  | D1Cextval (name, d1e_def) => begin
      d2ec_extval (d1c0.d1ec_loc, name, d1exp_tr d1e_def)
    end // end of [D1Cextval]
  | D1Cextcode (pos, code) => begin
      d2ec_extcode (d1c0.d1ec_loc, pos, code)
    end // end of [D1Cextcode]
  | D1Cvaldecs (valknd, d1cs) => let
      val d2cs = v1aldeclst_tr (false(*isrec*), d1cs)
    in
      d2ec_valdecs (d1c0.d1ec_loc, valknd, d2cs)
    end // end of [D1Cvaldecs]
  | D1Cvaldecs_par (d1cs) => let
      val d2cs = v1aldeclst_tr (false(*isrec*), d1cs)
    in
      d2ec_valdecs_par (d1c0.d1ec_loc, d2cs)
    end // end of [D1Cvaldecs_par]
  | D1Cvaldecs_rec (d1cs) => let
      val d2cs = v1aldeclst_tr (true(*isrec*), d1cs)
    in
      d2ec_valdecs_rec (d1c0.d1ec_loc, d2cs)
    end // end of [D1Cvaldecs_rec]
  | D1Cfundecs (funknd, decarg, d1cs) => let
      val (pf_s2expenv | ()) = the_s2expenv_push ()
      val () = begin
        case+ decarg of cons _ => template_level_inc () | nil _ => ()
      end // end of [val]
      val s2vpss = s1qualstlst_tr (decarg)
      val () = s2qualst_tmplev_set (s2vpss, template_level_get ())
      val level = d2var_current_level_get ()
      val d2cs = f1undeclst_tr (funknd, level, s2vpss, d1cs)
      val () = the_s2expenv_pop (pf_s2expenv | (*none*))
      val () = begin
        case+ decarg of cons _ => template_level_dec () | nil _ => ()
      end // end of [val]
    in
      d2ec_fundecs (d1c0.d1ec_loc, s2vpss, funknd, d2cs)
    end // end of [D1Cfundecs]
  | D1Cvardecs (d1cs) => let
      val d2cs = v1ardeclst_tr d1cs
    in
      d2ec_vardecs (d1c0.d1ec_loc, d2cs)
    end // end of [D1Cvardecs]
  | D1Cmacdefs (knd, d1cs) => begin
       // knd: 0/1/2 => short/long/long rec
       m1acdeflst_tr (knd, d1cs); d2ec_none (d1c0.d1ec_loc)
    end // end of [D1Cmacdefs]
  | D1Cimpdec (i1mparg, d1c) => let
      val loc0 = d1c0.d1ec_loc
      val d2c = i1mpdec_tr (loc0, i1mparg, d1c)
    in
      d2ec_impdec (loc0, d2c)
    end // end of [D1Cimpdec]
  | D1Clocal (d1cs_head, d1cs_body) => let
      val (pf1_env | ()) = trans2_env_push ()
      val d2cs_head = d1eclst_tr d1cs_head
      val (pf2_env | ()) = trans2_env_push ()
      val d2cs_body = d1eclst_tr d1cs_body
      val () = trans2_env_localjoin (pf1_env, pf2_env | (*none*))
    in
      d2ec_local (d1c0.d1ec_loc, d2cs_head, d2cs_body)
    end // end of [D1Clocal]
  | D1Cdynload (fil) => d2ec_dynload (d1c0.d1ec_loc, fil)
  | D1Cstaload (idopt, fil, loaded, d1cs) => begin
      s1taload_tr (d1c0.d1ec_loc, idopt, fil, loaded, d1cs)
    end // end of [D1Cstaload]
(*
  | _ => begin
      prerr_loc_error2 d1c0.d1ec_loc;
      prerr ": d1ec_tr: not available yet.\n";
      $Err.abort {d2ec} ()
    end // end of [_]
*)
end // end of [d1ec_tr]

(* ****** ****** *)

// [list_map_fun] is tail-recursive!
implement d1eclst_tr (d1cs) = $Lst.list_map_fun (d1cs, d1ec_tr)

(* ****** ****** *)

(* end of [ats_trans2_dyn2.dats] *)
