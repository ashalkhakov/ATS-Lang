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
 *)

(* ****** ****** *)

// Time: April 2008
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

staload Err = "ats_error.sats"
staload IntInf = "ats_intinf.sats"
staload Lab = "ats_label.sats"
staload Lst = "ats_list.sats"
staload Map = "ats_map_lin.sats"

(* ****** ****** *)

staload "ats_staexp2.sats"
staload "ats_dynexp2.sats"
staload "ats_stadyncst2.sats"
staload "ats_trans2_env.sats"

(* ****** ****** *)

staload "ats_hiexp.sats"
staload "ats_trans4.sats"
staload "ats_ccomp.sats"
staload "ats_ccomp_env.sats"

(* ****** ****** *)

staload "ats_reference.sats"
staload _(*anonymous*) = "ats_reference.dats"

(* ****** ****** *)

staload _(*anonymous*) = "ats_map_lin.dats"

(* ****** ****** *)

extern fun cloenv_make (): envmap_t
extern fun cloenv_make_dynctx (ctx: !dynctx_vt): envmap_t

local

assume envmap_t = ref ($Map.map_vt (d2var_t, valprim))

in // in of [local]

implement envmap_find (map, d2v) = let
  val (pfbox | p_map) = ref_get_view_ptr (map)
  prval vbox pf = pfbox
in
  $Map.map_search (!p_map, d2v)
end // end of [envmap_find]

implement cloenv_make_dynctx (ctx) = let
  viewtypedef map_vt = $Map.map_vt (d2var_t, valprim)
  var res: map_vt = $Map.map_make {d2var_t, valprim} (compare_d2var_d2var)
  fn f (pf: !map_vt @ res | d2v: d2var_t, vp: valprim, p_res: !ptr res): void =
    if d2var_lev_get (d2v) > 0 then begin
      // [d2v] is recorded only if it is not at the top level
      $Map.map_insert (!p_res, d2v, vp)
    end // end of [if]
  // end of [f]
  val () = dynctx_foreach_main {map_vt @ res} (view@ res | ctx, f, &res)
in
  ref_make_elt<map_vt> (res)
end // end of [cloenv_make_dynctx]

end // end of [local]

(* ****** ****** *)

local

viewtypedef dynctx = $Map.map_vt (d2var_t, valprim)
viewtypedef dynctxlst = List_vt (dynctx)

assume dynctx_vt = dynctx

fn dynctx_nil ():<> dynctx = $Map.map_make (compare_d2var_d2var)
val the_dynctx = ref_make_elt<dynctx> (dynctx_nil ())
val the_dynctxlst = ref_make_elt<dynctxlst> (list_vt_nil ())

dataviewtype dynmarklst =
  | DYNMARKLSTcons of (d2var_t, dynmarklst)
  | DYNMARKLSTmark of dynmarklst
  | DYNMARKLSTnil

assume dynctx_mark_token = unit_v
assume dynctx_push_token = unit_v

val the_dynmarklst = ref_make_elt<dynmarklst> (DYNMARKLSTnil ())

in // in of [local]

implement the_dynctx_add (d2v, vp) = let
  val () = let
    val (pfbox | p) = ref_get_view_ptr (the_dynctx)
    prval vbox pf = pfbox
  in
    $Map.map_insert<d2var_t,valprim> (!p, d2v, vp)
  end
  val () = let
    val (pfbox | p) = ref_get_view_ptr (the_dynmarklst)
    prval vbox pf = pfbox
  in
    !p := DYNMARKLSTcons (d2v, !p)
  end
in
  // empty
end // end of [the_dynctx_add]

//

implement the_dynctx_mark () = let
  val () = let
    val (pfbox | p) = ref_get_view_ptr (the_dynmarklst)
    prval vbox pf = pfbox
  in
    !p := DYNMARKLSTmark (!p)
  end
in
  (unit_v () | ())
end // end of [the_dynctx_mark]

//

fn the_dynctx_del (d2v: d2var_t): void = let
  val ans = let
    val (pfbox | p) = ref_get_view_ptr (the_dynctx)
    prval vbox pf = pfbox
  in
    $Map.map_remove<d2var_t,valprim> (!p, d2v)
  end
in
  case+ ans of
  | ~Some_vt _ => () | ~None_vt () => begin
      prerr "Internal Error: the_dynctx_del: d2v = ";
      prerr d2v; prerr_newline ();
      $Err.abort {void} ()
    end
end // end of [the_dynctx_del]

implement the_dynctx_unmark (pf_mark | (*none*)) = let
  prval unit_v () = pf_mark
  fun aux (vms: dynmarklst)
    : dynmarklst = begin case+ vms of
    | ~DYNMARKLSTcons (d2v, vms) =>
        (the_dynctx_del (d2v); aux (vms))
    | ~DYNMARKLSTmark (vms) => vms
    | ~DYNMARKLSTnil () => begin
        prerr "Internal Error: the_dynctx_unmark: aux";
        prerr_newline ();
        $Err.abort {dynmarklst} ()
      end
  end // end of [aux]
  val () = let
    val (pfbox | p) = ref_get_view_ptr (the_dynmarklst)
    prval vbox pf = pfbox
  in
    !p := $effmask_all (aux (!p))
  end // end of [val]
in
  // empty  
end // end of [the_dynctx_unmark]

//

implement the_dynctx_free () = let
  fun aux (vms: dynmarklst): void = begin case+ vms of
    | ~DYNMARKLSTcons (d2v, vms) => (the_dynctx_del d2v; aux vms)
    | ~DYNMARKLSTmark (vms) => aux (vms)
    | ~DYNMARKLSTnil () => ()
  end // end of [aux]
  val () = let
    val (pfbox | p) = ref_get_view_ptr (the_dynmarklst)
    prval vbox pf = pfbox
    val () = $effmask_all (aux (!p))
  in
    !p := DYNMARKLSTnil ()
  end // end of [val]
in
  // empty  
end // end of [the_dynctx_free]

//

implement the_dynctx_find (d2v) = let
  val ans = let
    val (pfbox | p) = ref_get_view_ptr (the_dynctx)
    prval vbox pf = pfbox
  in
    $Map.map_search<d2var_t,valprim> (!p, d2v)
  end // end of [val]
in
  case+ ans of
  | ~Some_vt vp => vp | ~None_vt () => begin
      $Loc.prerr_location (d2var_loc_get d2v);
      prerr ": Internal Error: ats_ccomp_trans: the_dynctx_find: d2v = ";
      prerr d2v; prerr_newline ();
      $Err.abort {valprim} ()
    end // end of [None_vt]
end // end of [the_dynctx_find]

//

implement the_dynctx_pop (pf_push | (*none*)) = let
  prval unit_v () = pf_push
  var err: int = 0; val x = let
    val (vbox pf | p) = ref_get_view_ptr (the_dynctxlst)
  in
    case+ !p of
    | ~list_vt_cons (x, xs) => begin
        let val () = !p := (xs: dynctxlst) in x end
      end
    | list_vt_nil () => begin
        fold@ (!p); err := 1; dynctx_nil ()
      end
  end : dynctx // end of [val]
  val () = // error checking
    if err > 0 then begin
      prerr "Internal Error; ats-ccomp_trans: the_dynctx_pop";
      prerr_newline ();
      $Err.abort {void} ()
    end
  val () = the_dynctx_unmark (unit_v () | (*none*))
  val (vbox pf | p) = ref_get_view_ptr (the_dynctx)
in
  $Map.map_free (!p); !p := (x: dynctx)
end // end of [the_dynctx_pop]

implement the_dynctx_push () = let
  val x = let
    val (vbox pf | p) = ref_get_view_ptr (the_dynctx)
    val x = !p
  in
    !p := dynctx_nil (); x
  end
  val (pf_mark | ()) = the_dynctx_mark ()
  prval unit_v () = pf_mark
  val (vbox pf | p) = ref_get_view_ptr (the_dynctxlst)
  val () = !p := list_vt_cons (x, !p)
in
  (unit_v () | ())
end // end of [the_dynctx_push]

//

implement dynctx_foreach_main
  (pf | ctx, f, env) = $Map.map_foreach_pre (pf | ctx, f, env)

(* ****** ****** *)

implement cloenv_make () = let
  val (pfbox | p) = ref_get_view_ptr (the_dynctx)
  prval vbox pf = pfbox
in
  $effmask_ref (cloenv_make_dynctx (!p))
end // end of [cloenv_make]

end // end of [local]

(* ****** ****** *)

local

val the_glocstlst = ref_make_elt<glocstlst> (GLOCSTLSTnil ())

fn glocstlst_reverse (xs: glocstlst): glocstlst = let
  fun aux (xs: glocstlst, ys: glocstlst)
    : glocstlst = begin case+ xs of
    | GLOCSTLSTcons_clo (_(*d2c*), !xs1) => let
        val xs1_v = !xs1; val () = (!xs1 := ys; fold@ (xs))
      in
        aux (xs1_v, xs)
      end // end of [GLOCSTLSTcons_clo]
    | GLOCSTLSTcons_fun (_(*d2c*), !xs1) => let
        val xs1_v = !xs1; val () = (!xs1 := ys; fold@ (xs))
      in
        aux (xs1_v, xs)
      end // end of [GLOCSTLSTcons_fun]
    | GLOCSTLSTcons_val (_(*d2c*), _(*vp*), !xs1) => let
        val xs1_v = !xs1; val () = (!xs1 := ys; fold@ (xs))
      in
        aux (xs1_v, xs)
      end // end of [GLOCSTLSTcons_val]
    | ~GLOCSTLSTnil () => ys
  end // end of [aux]
in
  aux (xs, GLOCSTLSTnil ())
end // end of [glocstlst_reverse]

in // in of [local]

implement the_glocstlst_add_clo (d2c) = let
  val (vbox pf | p) = ref_get_view_ptr (the_glocstlst)
in
  !p := GLOCSTLSTcons_clo (d2c, !p)
end // end of [the_glocstlst_add_clo]

implement the_glocstlst_add_fun (d2c) = let
  val (vbox pf | p) = ref_get_view_ptr (the_glocstlst)
in
  !p := GLOCSTLSTcons_fun (d2c, !p)
end // end of [the_glocstlst_add_fun]

implement the_glocstlst_add_val (d2c, vp) = let
  val (vbox pf | p) = ref_get_view_ptr (the_glocstlst)
in
  !p := GLOCSTLSTcons_val (d2c, vp, !p)
end // end of [the_glocstlst_add_val]

implement the_glocstlst_get () = let
  val xs = let
    val (vbox pf | p) = ref_get_view_ptr (the_glocstlst)
    val xs = !p
  in
    !p := GLOCSTLSTnil (); xs
  end
in
  glocstlst_reverse (xs)
end // end of [the_glocstlst_get]

end // end of [local]

(* ****** ****** *)

local

viewtypedef cstctx = $Map.map_vt (d2cst_t, valprim)

fn cstctx_nil (): cstctx = $Map.map_make (compare_d2cst_d2cst)

val the_topcstctx = ref_make_elt<cstctx> (cstctx_nil ())

in // in of [local]

implement the_topcstctx_add (d2c, vp) = let
  val (pfbox | p) = ref_get_view_ptr (the_topcstctx)
  prval vbox pf = pfbox
in
  $Map.map_insert (!p, d2c, vp)
end // end of [the_topcstctx_add]

implement the_topcstctx_find (d2c) = let
  val (pfbox | p) = ref_get_view_ptr (the_topcstctx)
  prval vbox pf = pfbox
in
  $Map.map_search (!p, d2c)
end // end of [the_topcstctx_find]

end // end of [local]

(* ****** ****** *)

local

val the_valprimlst_free = ref_make_elt<valprimlst_vt> (list_vt_nil ())

in // in of [local]

implement the_valprimlst_free_add (vp) = let
  val (vbox pf | p) = ref_get_view_ptr (the_valprimlst_free)
in
  !p := list_vt_cons (vp, !p)
end // end of [the_valprimlst_free_add]

implement the_valprimlst_free_get () = let
  val (vbox pf | p) = ref_get_view_ptr (the_valprimlst_free)
  val vps = !p; val () = !p := list_vt_nil ()
in
  $Lst.list_vt_reverse (vps)
end // end of [the_valprimlst_free_get]

implement instr_add_valprimlst_free (res) = let
  fun aux_free (res: &instrlst_vt, vps: valprimlst_vt)
    : void = begin case+ vps of
    | ~list_vt_cons (vp, vps) => begin
        instr_add_freeptr (res, vp); aux_free (res, vps)
      end
    | ~list_vt_nil () => ()
  end // end of [aux_free]
in
  aux_free (res, the_valprimlst_free_get ())
end // end of [instr_add_valprimlst_free]

end // end of [local]

(* ****** ****** *)

extern fun ccomp_patck_rec (
    res: &instrlst_vt
  , vp: valprim
  , lhips: labhipatlst
  , hit_rec: hityp_t
  , fail: kont
  ) : void

extern fun ccomp_patck_sum (
    res: &instrlst_vt
  , vp: valprim
  , d2c: d2con_t
  , hips: hipatlst
  , hit_sum: hityp_t
  , fail: kont
  ) : void

implement ccomp_patck_rec
  (res, vp_rec, lhips, hit_rec, fail) = let
  fun aux (res: &instrlst_vt, l: lab_t, hip: hipat)
    :<cloptr1> void = begin case+ hip.hipat_node of
    | HIPann (hip, _(*ann*)) => aux (res, l, hip)
    | HIPany _ => ()
    | HIPas (_(*knd*), _(*d2v*), hip) => aux (res, l, hip)
    | HIPvar _ => ()
    | _ => let
        val d2v = (
          case+ hip.hipat_asvar of
          | D2VAROPTnone () => let
              val d2v = d2var_make_any (hip.hipat_loc)
              val () = hipat_asvar_set (hip, D2VAROPTsome d2v)
            in
              d2v
            end // end of [D2VAROPTnone]
          | D2VAROPTsome d2v => d2v // may happen in template compilation
        ) : d2var_t
        val hit = hityp_normalize (hip.hipat_typ)
        val tmp = tmpvar_make (hit)
        val off = OFFSETlab (l, hit_rec)
        val () = instr_add_select (res, tmp, vp_rec, '[off])
        val vp = valprim_tmp tmp
        val () = the_dynctx_add (d2v, vp)
      in
        ccomp_patck (res, vp, hip, fail)
      end // end of [_]
  end // end of [aux]
  fun auxlst (res: &instrlst_vt, lhips: labhipatlst)
    :<cloptr1> void = begin case+ lhips of
    | LABHIPATLSTcons (l, hip, lhips) =>
        (aux (res, l, hip); auxlst (res, lhips))
    | _ => () // [LABHIPATLSTdot] and [LABHIPATLSTnil]
  end // end of [auxlst]
in
  auxlst (res, lhips)
end // end of [ccomp_patck_rec]

implement ccomp_patck_sum
  (res, vp_sum, d2c, hips_arg, hit_sum, fail) = let
  fun aux (res: &instrlst_vt, hip: hipat, i: int)
    :<cloptr1> void = begin case+ hip.hipat_node of
    | HIPann (hip, _(*ann*)) => aux (res, hip, i)
    | HIPany _ => ()
    | HIPas (_(*knd*), _(*d2v*), hip) => aux (res, hip, i)
    | HIPvar _ => ()
    | _ => let
        val d2v = (
          case+ hip.hipat_asvar of
          | D2VAROPTnone () => let
              val d2v = d2var_make_any (hip.hipat_loc)
              val () = hipat_asvar_set (hip, D2VAROPTsome d2v)
            in
              d2v
            end // end of [D2VAROPTnone]
          | D2VAROPTsome d2v => d2v // may happen in template compialtion
        ) : d2var_t
        val () = hipat_asvar_set (hip, D2VAROPTsome d2v)
        val hit = hityp_normalize (hip.hipat_typ)
        val tmp = tmpvar_make (hit)
        val () = instr_add_selcon (res, tmp, vp_sum, hit_sum, i)
        val vp = valprim_tmp tmp
        val () = the_dynctx_add (d2v, vp)
      in
        ccomp_patck (res, vp, hip, fail)
      end // end of [_]
  end // end of [aux]
  fun auxlst
    (res: &instrlst_vt, hips: hipatlst, i: int)
    :<cloptr1> void = begin case+ hips of
    | list_cons (hip, hips) =>
        (aux (res, hip, i); auxlst (res, hips, i+1))
    | list_nil () => ()
  end // end of [auxlst]
in
  auxlst (res, hips_arg, 0)
end // end of [ccomp_patck_sum]

(* ****** ****** *)

implement ccomp_patck (res, vp0, hip0, fail) = let
(*
  val () = begin
    prerr "ccomp_patck: vp0 = "; prerr vp0; prerr_newline ()
  end
  val () = begin
    prerr "ccomp_patck: vp0.typ = "; prerr vp0.valprim_typ; prerr_newline ()
  end
  val () = begin
    prerr "ccomp_patck: hip0 = "; prerr hip0; prerr_newline ()
  end
  val () = begin
    prerr "ccomp_patck: hip0.typ = "; prerr hip0.hipat_typ; prerr_newline ()
  end
*)
in
  case+ hip0.hipat_node of
  | HIPann (hip, _(*ann*)) => begin
      ccomp_patck (res, vp0, hip, fail)
    end
  | HIPany () => ()
  | HIPas (_(*knd*), _(*d2v*), hip) => ccomp_patck (res, vp0, hip, fail)
  | HIPbool b => begin
      instr_add_patck (res, vp0, PATCKbool b, fail)
    end
  | HIPchar c => begin
      instr_add_patck (res, vp0, PATCKchar c, fail)
    end
  | HIPcon (_(*freeknd*), d2c, hips_arg, hit_sum) => let
(*
      val () = begin
        prerr "ccomp_patck: HIPcon: hit_sum = "; prerr hit_sum;
        prerr_newline ()
      end
*)
      val () = the_dynconset_add d2c
      val isexn = d2con_is_exn d2c
      val patck = (
        if isexn then PATCKexn d2c else PATCKcon d2c
      ) : patck
      val () = instr_add_patck (res, vp0, patck, fail)
      val hit_sum = hityp_normalize hit_sum
    in
      ccomp_patck_sum (res, vp0, d2c, hips_arg, hit_sum, fail)
    end // end of [HIPcon]
  | HIPcon_any (_(*freeknd*), d2c) => let
      val () = the_dynconset_add d2c
      val isexn = d2con_is_exn d2c
      val patck = (
        if isexn then PATCKexn d2c else PATCKcon d2c
      ) : patck
    in
      instr_add_patck (res, vp0, patck, fail)
    end // end of [HIPcon_any]
  | HIPfloat f(*string*) => begin
      instr_add_patck (res, vp0, PATCKfloat f, fail)
    end // end of [HIPfloat]
  | HIPempty () => ()
  | HIPint (str, int) => begin
      instr_add_patck (res, vp0, PATCKint (int), fail)
    end // end of [HIPint]
  | HIPrec (_(*knd*), lhips, hit_rec) => let
      val hit_rec = hityp_normalize (hit_rec)
    in
      ccomp_patck_rec (res, vp0, lhips, hit_rec, fail)
    end // end of [HIPrec]
  | HIPstring str => begin
      instr_add_patck (res, vp0, PATCKstring str, fail)
    end // end of [HIPstring]
  | HIPvar _ => ()
  | _ => begin
      prerr "ccomp_patck: not implemented yet: hip0 = ";
      prerr hip0; prerr_newline ();
      $Err.abort {void} ()
    end // end of [_]
end // end of [ccomp_patck]

(* ****** ****** *)

fn ccomp_match_rec (
    res: &instrlst_vt
  , level: int
  , vp_rec: valprim
  , lhips: labhipatlst
  , hit_rec: hityp_t
  ) : void = let
  fun aux (
      res: &instrlst_vt
    , lhips: labhipatlst
    ) :<cloptr1> void = begin case+ lhips of
    | LABHIPATLSTcons (l, hip, lhips) => let
        val vp = (
          case+ hip.hipat_asvar of
          | D2VAROPTnone () => let
              val hit = hityp_normalize (hip.hipat_typ)
              val tmp = tmpvar_make (hit)
              val off = OFFSETlab (l, hit_rec)
              val () = instr_add_select (res, tmp, vp_rec, '[off])
            in
              valprim_tmp tmp
            end // end of [D2VAROPTnone]
          | D2VAROPTsome d2v => the_dynctx_find (d2v)
        ) : valprim
        val () = ccomp_match (res, level, vp, hip)
      in
        aux (res, lhips)
      end // end of [LABHIPATLSTcons]
    | _ => () // [LABHIPATLSTnil] and [LABHIPATLSTdot]
  end // end of [aux]
in
  aux (res, lhips)
end // end of [ccomp_match_rec]

fn ccomp_match_sum (
    res: &instrlst_vt
  , level: int
  , vp_sum: valprim
  , freeknd: int
  , d2c: d2con_t
  , hips_arg: hipatlst
  , hit_sum: hityp_t
  ) : void = let

  fun aux_var (
      res: &instrlst_vt
    , level: int
    , i: int
    , hip0: hipat
    , refknd: int
    , d2v: d2var_t
    ) :<cloptr1> void = let
    val () = d2var_lev_set (d2v, level)
  in
    case+ 0 of
    | _ when d2var_count_get d2v = 0 => () // [d2v] is unused
    | _ when refknd > 0 => let
        val hit = hityp_encode (hityp_ptr)
        val tmp_ptr = tmpvar_make (hit)
        val () = instr_add_selcon_ptr (res, tmp_ptr, vp_sum, hit_sum, i)
        val vp_ptr = valprim_tmp (tmp_ptr)
        val () = the_dynctx_add (d2v, vp_ptr)
      in
        // empty
      end // end of [_ when refknd > 0]
    | _ => let
        val hit = hityp_normalize (hip0.hipat_typ)
        val tmp = tmpvar_make (hit)
        val () = instr_add_selcon (res, tmp, vp_sum, hit_sum, i)
        val vp = valprim_tmp tmp
        val () = the_dynctx_add (d2v, vp)
      in
        // empty
      end // end of [_]
  end // end of [aux_var]

  fun aux_pat
    (res: &instrlst_vt, level: int, i: int, hip0: hipat)
    :<cloptr1> void = begin case+ hip0.hipat_node of
    | HIPann (hip, __(*hit_ann*)) => aux_pat (res, level, i, hip)
    | HIPany () => ()
    | HIPas (refknd, d2v, hip) => begin
        aux_var (res, level, i, hip0, refknd, d2v); aux_pat (res, level, i, hip)
      end // end of [HIPas]
    | HIPvar (refknd, d2v) => aux_var (res, level, i, hip0, refknd, d2v)
    | _ => let
        val vp = (
          case+ hip0.hipat_asvar of
          | D2VAROPTsome d2v => the_dynctx_find d2v
          | D2VAROPTnone () => begin
              prerr "ccomp_match_sum: aux: hip0 = "; prerr hip0; prerr_newline ();
              $Err.abort {valprim} ()
            end
        ) : valprim
      in
        ccomp_match (res, level, vp, hip0)
      end // end of [_]
  end // end of [aux_pat]

  fun auxlst_pat
    (res: &instrlst_vt, level: int, i: int, hips: hipatlst)
    :<cloptr1> void = begin case+ hips of
    | list_cons (hip, hips) => let
        val () = aux_pat (res, level, i, hip)
      in
        auxlst_pat (res, level, i+1, hips)
      end
    | list_nil () => ()
  end // end of [auxlst_pat]
in
  auxlst_pat (res, level, 0, hips_arg)
end // end of [ccomp_match_sum]

(* ****** ****** *)

implement ccomp_match (res, level, vp0, hip0) = let
(*
  val () = begin
    prerr "ccomp_match: level = "; prerr level; prerr_newline ()
  end
  val () = begin
    prerr "ccomp_match: vp0 = "; prerr vp0; prerr_newline ()
  end
  val () = begin
    prerr "ccomp_match: hip0 = "; prerr hip0; prerr_newline ()
  end
*)
  fun aux_var (
      res: &instrlst_vt
    , level: int
    , vp0: valprim
    , d2v: d2var_t
    ) : void = let
    val () = d2var_lev_set (d2v, level)
  in
    case+ d2v of
    | _ when d2var_count_get d2v > 0 => let
        val ismove = (
          case+ vp0.valprim_node of
          | VPclo _ => true
          | _ when d2var_is_mutable d2v => false
          | _ => valprim_is_mutable vp0
        ) : bool
      in
        if ismove then let
          val tmp = tmpvar_make (vp0.valprim_typ)
          val () = instr_add_move_val (res, tmp, vp0)
          val () = the_dynctx_add (d2v, valprim_tmp tmp)
        in
          // empty
        end else let
          val () = the_dynctx_add (d2v, vp0)
        in
          // empty
        end // end of [ismove]
      end // end of [d2var_count_get (d2v) > 0]
    | _ => () // the variable [d2v] is unused
  end // end of [aux_var]
in
  case+ hip0.hipat_node of
  | HIPann (hip, _(*ann*)) =>
      ccomp_match (res, level, vp0, hip)
  | HIPany _ => ()
  | HIPas (_(*refknd*), d2v, hip) => let
      val () = aux_var (res, level, vp0, d2v)
    in
      ccomp_match (res, level, vp0, hip)
    end // end of [HIPas]
  | HIPbool _ => ()
  | HIPchar _ => ()
  | HIPcon (freeknd, d2c, hips_arg, hit_sum) => let
      val hit_sum = hityp_normalize (hit_sum)
      val () = ccomp_match_sum
        (res, level, vp0, freeknd, d2c, hips_arg, hit_sum)
    in
      if freeknd < 0 then the_valprimlst_free_add vp0
    end // end of [HIPcon]
  | HIPcon_any (freeknd, d2c) => begin
      if freeknd < 0 then the_valprimlst_free_add vp0
    end // end of [HIPcon_any]
  | HIPempty _ => ()
  | HIPint _ => ()
  | HIPfloat _ => ()
  | HIPrec (_(*knd*), lhips, hit_rec) => let
      val hit_rec = hityp_normalize (hit_rec)
    in
      ccomp_match_rec (res, level, vp0, lhips, hit_rec)
    end // end of [HIPrec]
  | HIPstring _ => ()
  | HIPvar (_(*refknd*), d2v) => aux_var (res, level, vp0, d2v)
  | _ => begin
      $Loc.prerr_location hip0.hipat_loc;
      prerr ": Internal Error";
      prerr ": ccomp_match: hip0 = "; prerr hip0; prerr_newline ();
      $Err.abort {void} ()
    end
end // end of [ccomp_match]

(* ****** ****** *)

extern fun ccomp_explstlst
  (res: &instrlst_vt, hies: hiexplstlst): valprimlstlst

extern fun ccomp_labexplst
  (res: &instrlst_vt, lhies: labhiexplst): labvalprimlst

(* ****** ****** *)

extern fun ccomp_exp_var (d2v: d2var_t): valprim

(* ****** ****** *)

extern fun ccomp_hilab (res: &instrlst_vt, hil: hilab): offset

extern fun ccomp_hilablst (res: &instrlst_vt, hils: hilablst): offsetlst

(* ****** ****** *)

extern fun ccomp_dec (res: &instrlst_vt, hid: hidec): void

(* ****** ****** *)

fn hiexp_refarg_tr (
    res: &instrlst_vt
  , level: int
  , vps_free: &valprimlst_vt
  , hie0: hiexp
  ) : hiexp = begin case+ hie0.hiexp_node of
  | HIErefarg (refval, freeknd, hie) when freeknd > 0 => let
      val loc = hie.hiexp_loc
      val d2v_any = let
        val d2v_any = d2var_make_any (loc)
        val d2v_any_view = d2var_make_any (loc)
        val () = d2var_lev_set (d2v_any, level)
        val () = begin
          d2var_view_set (d2v_any, D2VAROPTsome d2v_any_view)
        end
      in
        d2v_any
      end // end of [val]
      val hit = hie.hiexp_typ
      val tmp = tmpvar_make (hityp_normalize hit)
      val () = instr_add_vardec (res, tmp)
      val vp = valprim_tmp_ref tmp
      val () = (vps_free := list_vt_cons (vp, vps_free))
      val () = the_dynctx_add (d2v_any, vp)
      val hie_assgn = hiexp_assgn_var
        (loc, hityp_void, d2v_any, list_nil (), hie)
      val hie_var = hiexp_var (loc, hit, d2v_any)
      val hie_res = begin
        hiexp_refarg (loc, hit, refval, freeknd, hie_var)
      end
    in
      hiexp_seq (loc, hit, '[hie_assgn, hie_res])
    end // end of [HIErefarg]
  | _ => hie0
end (* end of [hiexp_refarg_tr] *)

fun hiexplst_refarg_tr (
    res: &instrlst_vt
  , level: int
  , vps_free: &valprimlst_vt
  , hies: hiexplst
  ) : hiexplst = begin case+ hies of
  | list_cons (hie, hies) => let
      val hie = begin
        hiexp_refarg_tr (res, level, vps_free, hie)
      end
      val hies = begin
        hiexplst_refarg_tr (res, level, vps_free, hies)
      end
    in
      list_cons (hie, hies)
    end
  | list_nil () => list_nil ()
end (* end of [hiexplst_refarg_tr] *)

(* ****** ****** *)

fn ccomp_exp_assgn_ptr (
    res: &instrlst_vt
  , hie_ptr: hiexp
  , hils: hilablst
  , hie_val: hiexp)
  : void = let
  val vp_ptr = ccomp_exp (res, hie_ptr)
  val offs = ccomp_hilablst (res, hils)
  val vp_val = ccomp_exp (res, hie_val)
in
  instr_add_store_ptr_offs (res, vp_ptr, offs, vp_val)
end // end of [ccomp_exp_assgn_ptr]

fn ccomp_exp_assgn_var (
    res: &instrlst_vt
  , d2v_mut: d2var_t
  , hils: hilablst
  , hie_val: hiexp)
  : void = let
  val vp_mut = ccomp_exp_var (d2v_mut)
  val offs = ccomp_hilablst (res, hils)
  val vp_val = ccomp_exp (res, hie_val)
  val ins = INSTRstore_ptr_offs (vp_mut, offs, vp_val)
in
  instr_add_store_var_offs (res, vp_mut, offs, vp_val)
end // end of [ccomp_exp_assgn_var] 

(* ****** ****** *)

fn ccomp_exp_freeat
  (res: &instrlst_vt, hie: hiexp): void = begin
  instr_add_freeptr (res, ccomp_exp (res, hie))
end // end of [ats_ccomp_trans]

(* ****** ****** *)

fn funarg_valprim_make
  (n: int, hit0: hityp): valprim = begin
  case+ hit0.hityp_node of
  | HITrefarg (refval, hit) => let
      val hit = hityp_normalize (hit)
    in
      if refval > 0 then valprim_arg_ref (n, hit)
      else valprim_arg (n, hit)
    end
  | _ => valprim_arg (n, hityp_normalize hit0)
end // end of [funarg_valprim_make]

fn ccomp_funarg (
    res: &instrlst_vt
  , level: int
  , hips_arg: hipatlst
  , fl: funlab_t
  ) : void = let
  fun aux_patck {n:nat} (
      res: &instrlst_vt
    , i: int
    , hips: list (hipat, n)
    , fail: kont
    ) : list_vt (valprim, n) = begin case+ hips of
    | list_cons (hip, hips) => let
        val hit = hip.hipat_typ
(*
        val () = begin
          prerr "ccomp_funarg: aux_patck: hip = "; prerr hip; prerr_newline ();
          prerr "ccomp_funarg: aux_patck: hit = "; prerr hit; prerr_newline ();
        end
*)
        val vp = funarg_valprim_make (i, hit)
        val () = ccomp_patck (res, vp, hip, fail)
      in
        list_vt_cons (vp, aux_patck (res, i+1, hips, fail))
      end // end of [list_cons]
    | list_nil () => list_vt_nil ()
  end // end of [aux_patck]
  fun aux_match {n:nat} (
      res: &instrlst_vt
    , level: int
    , vps: list_vt (valprim, n)
    , hips: list (hipat, n)
    ) : void = begin case+ vps of
    | ~list_vt_cons (vp, vps) => let
        val list_cons (hip, hips) = hips
        val () = ccomp_match (res, level, vp, hip)
      in
        aux_match (res, level, vps, hips)
      end // end of [list_vt_cons]
    | ~list_vt_nil () => ()
  end // end of [aux_match]
  val vps_arg = aux_patck (res, 0, hips_arg, KONTfunarg_fail fl)
in
  aux_match (res, level, vps_arg, hips_arg)
end // end of [ccomp_funarg]

implement ccomp_exp_lam_funlab
  (loc_fun, prolog, hips_arg, hie_body, fl: funlab_t) = let
(*
  val () = begin
    prerr "ccomp_exp_lam_funlab: fl = "; prerr_funlab fl; prerr_newline ()
  end // end of [val]
*)
  var res: instrlst_vt = list_vt_nil ()

  val () = aux (res, prolog) where {
    fun aux (res: &instrlst_vt, inss: instrlst)
      : void = begin case+ inss of
      | list_cons (ins, inss) => begin
          res := list_vt_cons (ins, res); aux (res, inss)
        end // end of [list_cons]
      | list_nil () => ()
    end // end of [aux]
  } // end of [where]

  val (pf_level | level) = d2var_current_level_inc_and_get ()
  val () = the_funlabset_push () and () = the_vartypset_push ()

  val (pf_dynctx_mark | ()) = the_dynctx_mark ()

  val () = ccomp_funarg (res, level, hips_arg, fl)
  val hit_body = hityp_normalize (hie_body.hiexp_typ)
  val tmp_ret = tmpvar_make_ret (hit_body)

  val (pf_funlab_mark | ()) = funlab_push (fl)
  val () = ccomp_exp_tmpvar (res, hie_body, tmp_ret)
  val () = funlab_pop (pf_funlab_mark | (*none*))

  val () = the_dynctx_unmark (pf_dynctx_mark | (*none*))

  val fls = the_funlabset_pop () and vtps = the_vartypset_pop ()
  val level = d2var_current_level_dec_and_get (pf_level | (*none*))
  // function label propogation
  val () = funlabset_foreach_cloptr (fls, aux) where {
    fun aux (fl: funlab_t):<cloptr1> void = begin
      if funlab_lev_get fl < level then the_funlabset_add fl
    end
  } // end of [where]
  // environment variable propogation
  val () = vartypset_foreach_cloptr (vtps, aux) where {
    fun aux (vtp: vartyp_t):<cloptr1> void = let
      val d2v = vartyp_var_get (vtp)
    in
      if d2var_lev_get d2v < level then the_vartypset_add vtp
    end
  } // end of [where]
  val res = $Lst.list_vt_reverse_list (res)
(*
  val () = begin
    prerr "ccomp_exp_lam_funlab: fls = "; prerr_funlabset fls; prerr_newline ()
  end // end of [val]
*)
(*
  val () = begin
    prerr "ccomp_exp_lam_funlab: body = "; prerr_instrlst res; prerr_newline ()
  end // end of [val]
*)
  val entry = begin
    funentry_make (loc_fun, fl, level, fls, vtps, tmp_ret, res)
  end // end of [val]
in
  funentry_lablst_add (fl); funentry_associate (entry); entry
end // end of [ccomp_exp_lam_funlab]

(* ****** ****** *)

fn ccomp_exp_lam (
    loc0: loc_t
  , hit0: hityp
  , hips_arg: hipatlst
  , hie_body: hiexp
  ) : valprim = let
  val hit0 = hityp_normalize hit0
  val fl = funlab_make_typ (hit0)
  val (pf_tailcallst_mark | ()) = the_tailcallst_mark ()
  val () = the_tailcallst_add (fl, list_nil ())
  val _(*funentry*) = ccomp_exp_lam_funlab
    (loc0, prolog, hips_arg, hie_body, fl) where {
      val prolog = '[INSTRfunlab fl]
    } // end of [where]
  val () = the_tailcallst_unmark (pf_tailcallst_mark | (*none*))
in
  valprim_funclo_make (fl)
end // end of [ccomp_exp_lam]

(* ****** ****** *)

fn ccomp_exp_lazy_delay
  (loc0: loc_t, hie_eval: hiexp): valprim = let
  val funclo = $Syn.FUNCLOclo (~1) // cloref
  val hit_fun = hityp_fun (funclo, '[], hie_eval.hiexp_typ)
in
  ccomp_exp_lam (loc0, hit_fun, '[], hie_eval)
end // end of [ccomp_exp_lazy_delay]

fn ccomp_exp_lazy_vt_delay
  (loc0: loc_t, hie_eval: hiexp, hie_free: hiexp): valprim = let
  val funclo = $Syn.FUNCLOclo ( 1) // cloptr
  val hit_eval = hie_eval.hiexp_typ
  val d2v_arg = d2var_make_any (loc0)
  val () = d2var_count_inc (d2v_arg)
  val hie_cond = hiexp_var (loc0, hityp_bool, d2v_arg)
  val hie_if = hiexp_if (loc0, hit_eval, hie_cond, hie_eval, hie_free)
  val hip_arg = hipat_var (loc0, hityp_bool, 0(*refknd*), d2v_arg)
  val hit_fun = hityp_fun (funclo, '[hityp_bool], hit_eval)
in
  ccomp_exp_lam (loc0, hit_fun, '[hip_arg], hie_if)
end // end of [ccomp_exp_lazy_vt_delay]

(* ****** ****** *)

fn ccomp_exp_ptrof_ptr
  (res: &instrlst_vt, hie_ptr: hiexp, hils: hilablst)
  : valprim = let
(*
  val () = begin
    prerr "ccomp_exp_ptrof_ptr: hie_ptr = "; prerr_hiexp hie_ptr;
    prerr_newline ()
  end // end of [val]
*)
  val vp_ptr = ccomp_exp (res, hie_ptr)
  val offs = ccomp_hilablst (res, hils)
in
  case+ offs of
  | list_cons _ => valprim_ptrof_ptr_offs (vp_ptr, offs)
  | list_nil () => vp_ptr
end // end of [ccomp_exp_ptrof_ptr]

fn ccomp_exp_ptrof_var
  (res: &instrlst_vt, d2v_mut: d2var_t, hils: hilablst)
  : valprim = let
  var vp_mut: valprim = the_dynctx_find (d2v_mut)
  val () = let
    val lev_d2v = d2var_lev_get (d2v_mut)
    val level = d2var_current_level_get ()
(*
    val () = begin
      prerr "ccomp_exp_ptrof_var: lev_d2v = "; prerr lev_d2v; prerr_newline ()
    end
    val () = begin
      prerr "ccomp_exp_ptrof_var: level = "; prerr level; prerr_newline ()
    end
*)
  in
    case+ 0 of
    | _ when lev_d2v < level => begin
        if lev_d2v > 0 then let
          val hit = vp_mut.valprim_typ
          val vtp = vartyp_make (d2v_mut, hit)
          val () = the_vartypset_add vtp
        in
          vp_mut := valprim_env (vtp, hit)
        end else begin
          () // [d2v_mut] is at the top level
        end
      end // end of [_ when ...]
    | _ => () // [d2v_mut] is at the current level
  end
  val offs = ccomp_hilablst (res, hils)
in
  valprim_ptrof_var_offs (vp_mut, offs)
end (* end of [ccomp_exp_ptrof_var] *)

(* ****** ****** *)

fn ccomp_exp_refarg (
    res: &instrlst_vt
  , refval: int
  , hie: hiexp
  ) : valprim = begin case+ 0 of
  | _ when refval = 0 => ccomp_exp (res, hie)
  | _ (*call-by-ref*) => begin case+ hie.hiexp_node of
    | HIEvar d2v_mut =>
        ccomp_exp_ptrof_var (res, d2v_mut, list_nil ())
    | HIEsel_ptr (hie_ptr, hils) =>
        ccomp_exp_ptrof_ptr (res, hie_ptr, hils)
    | HIEsel_var (d2v_mut, hils) =>
        ccomp_exp_ptrof_var (res, d2v_mut, hils)
    | _ => begin
        $Loc.prerr_location (hie.hiexp_loc);
        prerr ": Internal Error";
        prerr ": ccomp_exp_refarg: hie = ";
        prerr_hiexp hie; prerr_newline ();
        $Err.abort {valprim} ()
      end
  end // end of [_]
end // end of [ccomp_exp_refarg]

(* ****** ****** *)

fn ccomp_exp_seq
  (res: &instrlst_vt, hies: hiexplst): valprim = let
  fun aux (res: &instrlst_vt, hie0: hiexp, hies: hiexplst)
    : valprim = begin case+ hies of
    | list_cons (hie, hies) => let
        val _(*void*) = ccomp_exp (res, hie0)
      in
        aux (res, hie, hies)
      end
    | list_nil () => begin
        ccomp_exp (res, hie0)
      end
  end // end of [aux]
in
  case+ hies of
  | list_cons (hie, hies) => aux (res, hie, hies)
  | list_nil () => valprim_void ()
end // end of [ccomp_exp_seq]

(* ****** ****** *)

implement ccomp_exp_var (d2v) = let
  var vp: valprim = the_dynctx_find (d2v)
  val d2v_lev = d2var_lev_get (d2v)
  val level = d2var_current_level_get ()
(*
  val () = begin
    prerr "ccomp_exp_var: d2v = "; prerr d2v; prerr_newline ()
  end
  val () = begin
    prerr "ccomp_exp_var: d2v_lev = "; prerr d2v_lev; prerr_newline ()
  end
  val () = begin
    prerr "ccomp_exp_var: level = "; prerr level; prerr_newline ()
  end
*)
  val () = case+ 0 of
    | _ when d2v_lev < level => begin
        if d2v_lev > 0 then begin case+ vp.valprim_node of
          | _ when valprim_is_const vp => ()
          | VPclo (_(*knd*), fl, _(*env*)) => the_funlabset_add fl
          | _ => let
              val hit = vp.valprim_typ
              val vtp = vartyp_make (d2v, hit)
              val () = the_vartypset_add vtp
            in
              vp := valprim_env (vtp, hit)
            end
        end else begin
          () // [d2v] is at the top level
        end
      end // end of [_ when ...]
    | _ => () // [d2v] is at the current level
in
  vp
end // end of [ccomp_exp_var]

(* ****** ****** *)

fn ccomp_exp_loop (
    res: &instrlst_vt
  , ohie_init: hiexpopt
  , hie_test: hiexp
  , ohie_post: hiexpopt
  , hie_body: hiexp
  ) : void = let
  var res_init : instrlst_vt = list_vt_nil ()
  val () = begin case+ ohie_init of
    | Some hie => begin
        let val _(*void*) = ccomp_exp (res_init, hie) in () end
      end // end of [Some]
    | None () => ()
  end // end of [val]
  val res_init = $Lst.list_vt_reverse_list res_init where {
    val res_init = (res_init: instrlst_vt) // handling a complaint by [ATS/Geizella]
  } // end of [where]
  val lab_init = tmplab_make () and lab_fini = tmplab_make ()
  val lab_cont = (
    case+ ohie_post of | Some _ => tmplab_make () | _ => lab_init
  ) : tmplab_t
  val () = loopexnlablst_push (lab_init, lab_fini, lab_cont)
  var res_test : instrlst_vt = list_vt_nil ()
  val vp_test = ccomp_exp (res_test, hie_test)
  val res_test = $Lst.list_vt_reverse_list res_test
  var res_post : instrlst_vt = list_vt_nil ()
  val () = begin case+ ohie_post of
    | Some hie => begin
        let val _(*void*) = ccomp_exp (res_post, hie) in () end
      end // end of [Some]
    | None () => ()
  end // end of [val]
  val res_post = $Lst.list_vt_reverse_list res_post where {
    val res_post = (res_post: instrlst_vt) // handling a complaint by [ATS/Geizella]
  } // end of [where]
  var res_body : instrlst_vt = list_vt_nil ()
  val _(*void*) = ccomp_exp (res_body, hie_body)
  val res_body = $Lst.list_vt_reverse_list res_body
  val () = loopexnlablst_pop ()
in
  instr_add_loop (
    res, lab_init, lab_fini, lab_cont, res_init, vp_test, res_test, res_post, res_body
  ) // end of [instr_add_loop]
end // end of [ccomp_exp_loop]

(* ****** ****** *)

implement valprim_funclo_make (fl) = let
  val fc = funlab_funclo_get (fl)
in
  case+ fc of
  | $Syn.FUNCLOclo knd => let
      val env = cloenv_make ()
    in
      valprim_clo (knd, fl, env)
    end
  | $Syn.FUNCLOfun () => valprim_fun (fl)
end // end of [valprim_funclo_make]

implement ccomp_exp (res, hie0) = let
(*
  val () = begin
    prerr "ccomp_exp: hie0 = "; prerr hie0; prerr_newline ()
  end
  val () = begin
    prerr "ccomp_exp: hit0 = "; prerr hie0.hiexp_typ; prerr_newline ()
  end
*)
in
  case+ hie0.hiexp_node of
  | HIEassgn_ptr (hie_ptr, hils, hie_val) => let
      val () = ccomp_exp_assgn_ptr (res, hie_ptr, hils, hie_val)
    in
      valprim_void ()
    end
  | HIEassgn_var (d2v_mut, hils, hie_val) => let
      val () = ccomp_exp_assgn_var (res, d2v_mut, hils, hie_val)
    in
      valprim_void ()
    end
  | HIEbool b => valprim_bool b
  | HIEchar c => valprim_char c
  | HIEcst d2c => begin case+ 0 of
    | _ when d2cst_is_proof d2c => begin
        $Loc.prerr_location (hie0.hiexp_loc);
        prerr ": error(ccomp)";
        prerr ": ["; prerr d2c; prerr "] is a proof constant";
        prerr ", which must not occur at run-time.";
        prerr_newline ();
        $Err.abort {valprim} ()
      end // end of [_ when ...]
    | _ => let
        val () = the_dyncstset_add_if (d2c)
        val hit0 = hityp_normalize (hie0.hiexp_typ)
      in
        valprim_cst (d2c, hit0)
      end // end of [_]
    end // end of [HIEcst]
  | HIEdynload fil => let
      val () = instr_add_dynload_file (res, fil)
    in
      valprim_void ()
    end
  | HIEempty () => valprim_void ()
  | HIEextval code => let
      val hit0 = hityp_normalize (hie0.hiexp_typ)
    in
      valprim_ext (code, hit0)
    end // end of [HIEextval]
  | HIEfloat f(*string*) => valprim_float f
  | HIEfreeat hie => let
      val () = ccomp_exp_freeat (res, hie)
    in
      valprim_void ()
    end // end of [HIEfreeat]
  | HIEint (_(*str*), int) => valprim_int (int)
  | HIEintsp (str, int) => valprim_intsp (str, int)
  | HIElam (hips_arg, hie_body) => begin
      ccomp_exp_lam (hie0.hiexp_loc, hie0.hiexp_typ, hips_arg, hie_body)
    end // end if [HIElam]
  | HIElet (hids, hie) => let
      val (pf_mark | ()) = the_dynctx_mark ()
      val () = ccomp_declst (res, hids)
      val vp = ccomp_exp (res, hie)
      val () = the_dynctx_unmark (pf_mark | (*none*))
    in
      vp // the return value
    end // end of [HIElet]
  | HIEloopexn (knd) => let
      val () = instr_add_loopexn (res, knd, loopexnlablst_get knd)
    in
      valprim_void ()
    end // end of [HIEloopexn]
  | HIEptrof_ptr (hie_ptr, hils) => begin
      ccomp_exp_ptrof_ptr (res, hie_ptr, hils)
    end // end of [HIEptrof_ptr]
  | HIEptrof_var (d2v_mut, hils) => begin
      ccomp_exp_ptrof_var (res, d2v_mut, hils)
    end // end of [HIEptrof_var]
  | HIErefarg (refval, _(*freeknd*), hie) => begin
      ccomp_exp_refarg (res, refval, hie)
    end // end of [HIErefarg]
  | HIEseq hies => ccomp_exp_seq (res, hies)
  | HIEsizeof hit => begin
      valprim_sizeof (hityp_normalize hit)
    end // end of [HIEsizeof]
  | HIEstring (str, len)=> valprim_string (str, len)
  | HIEtmpcst (d2c, hitss) => let
      val hit0 = hityp_normalize (hie0.hiexp_typ)
    in
      ccomp_exp_template_cst (hie0.hiexp_loc, res, hit0, d2c, hitss)
    end // end of [HIEtmpcst]
  | HIEtmpvar (d2v, hitss) => let
      val hit0 = hityp_normalize (hie0.hiexp_typ)
    in
      ccomp_exp_template_var (hie0.hiexp_loc, res, hit0, d2v, hitss)
    end // end of [HIEtmpvar]
  | HIEtop () => let
      val hit0 = hityp_normalize (hie0.hiexp_typ)
    in
      valprim_top (hit0)
    end // end of [HIEtop]
  | HIEvar d2v => begin case+ 0 of
    | _ when d2var_isprf_get d2v => begin
        $Loc.prerr_location (hie0.hiexp_loc);
        prerr ": error(ccomp)";
        prerr ": ["; prerr d2v; prerr "] is a proof variable";
        prerr ", which must not occur at run-time.";
        prerr_newline ();
        $Err.abort {valprim} ()
      end // end of [_ when ...]
    | _ => ccomp_exp_var (d2v)
    end // end of [HIEvar]
  | HIEloop (ohie_init, hie_test, ohie_post, hie_body) => let
      val () = ccomp_exp_loop (res, ohie_init, hie_test, ohie_post, hie_body)
    in
      valprim_void ()
    end // end of [HIEloop]
  | _ => let
      val hit0 = hityp_normalize (hie0.hiexp_typ)
(*
      val () = begin
        prerr "ccomp_exp: hit0 = "; prerr hit0; prerr_newline ();
        prerr "ccomp_exp: hie0 = "; prerr hie0; prerr_newline ();
      end
*)
      val tmp_res = tmpvar_make (hit0)
(*
      val () = begin
        prerr "ccomp_exp: tmp_res = "; prerr tmp_res; prerr_newline ();
      end
*)
      val () = ccomp_exp_tmpvar (res, hie0, tmp_res)
    in
      valprim_tmp tmp_res
    end
end // end of [ccomp_exp]

(* ****** ****** *)

implement ccomp_explst (res, hies) = begin
  case+ hies of
  | list_cons (hie, hies) => let
      val vp = ccomp_exp (res, hie)
    in
      list_cons (vp, ccomp_explst (res, hies))
    end
  | list_nil () => list_nil ()
end // end of [ccomp_explst]

implement ccomp_explstlst (res, hiess) = begin
  case+ hiess of
  | list_cons (hies, hiess) => let
      val vps = ccomp_explst (res, hies)
    in
      list_cons (vps, ccomp_explstlst (res, hiess))
    end
  | list_nil () => list_nil ()
end // end of [ccomp_explstlst]

implement ccomp_labexplst (res, lhies) = begin
  case+ lhies of
  | LABHIEXPLSTcons (l, hie, lhies) => let
      val vp = ccomp_exp (res, hie)
    in
      LABVALPRIMLSTcons (l, vp, ccomp_labexplst (res, lhies))
    end
  | LABHIEXPLSTnil () => LABVALPRIMLSTnil ()
end // end of [ccomp_labexplst]

(* ****** ****** *)

fun instrlst_add_freeptr
  (res: &instrlst_vt, vps: valprimlst_vt): void = begin
  case+ vps of
  | ~list_vt_cons (vp, vps) => begin
      instr_add_freeptr (res, vp); instrlst_add_freeptr (res, vps)
    end
  | ~list_vt_nil () => ()
end // end of [instrlst_add_freeptr]

//

fn tailcall_arg_move (
    res: &instrlst_vt
  , knd: int // 0/1: self/other
  , tmps_arg: tmpvarlst
  , vps_arg: valprimlst
  ) : void = let
  fun valprim_mov
    (res: &instrlst_vt, vp: valprim): valprim = let
    val tmp = tmpvar_make (vp.valprim_typ)
    val () = instr_add_move_val (res, tmp, vp)
  in
    valprim_tmp (tmp)
  end // end of [valmov]

  fun aux1_arg (res: &instrlst_vt, i: int, vps: valprimlst)
    : valprimlst_vt = begin case+ vps of
    | list_cons (vp, vps) => let
        val vp = (
          case+ vp.valprim_node of
          | VParg i_arg when i_arg < i => valprim_mov (res, vp)
          | VParg_ref i_arg when i_arg < i => valprim_mov (res, vp)
          | VPclo _ => valprim_mov (res, vp)
          | _ => vp
        ) : valprim
      in
        list_vt_cons (vp, aux1_arg (res, i+1, vps))
      end // end of [list_vt_cons]
    | list_nil () => list_vt_nil ()
  end // end of [aux1_arg]

  fun aux2_arg
    (res: &instrlst_vt, vps: valprimlst)
    : valprimlst_vt = begin case+ vps of
    | list_cons (vp, vps) => begin
        list_vt_cons (vp, aux2_arg (res, vps))
      end
    | list_nil () => list_vt_nil ()
  end // end of [aux2_arg]

  val vps_arg = (
    case+ knd of
    | 0 => aux1_arg (res, 0, vps_arg) // a call to self
    | _ => aux2_arg (res, vps_arg)
  ) : valprimlst_vt

  fun aux1_mov (
      res: &instrlst_vt
    , i: int
    , vps: valprimlst_vt
    ) : void = begin case+ vps of
    | ~list_vt_cons (vp, vps) => let
        val () = instr_add_move_arg (res, i, vp)
      in
        aux1_mov (res, i+1, vps)
      end
    | ~list_vt_nil () => ()
  end // end of [aux1_mov]

  fun aux2_mov (
      res: &instrlst_vt
    , tmps: tmpvarlst
    , vps: valprimlst_vt
    ) : void = begin case+ vps of
    | ~list_vt_cons (vp, vps) => begin case+ tmps of
      | list_cons (tmp, tmps) => let
          val () = instr_add_move_val (res, tmp, vp)
        in
          aux2_mov (res, tmps, vps)
        end
      | list_nil () => aux2_mov (res, tmps, vps) // deadcode
      end
    | ~list_vt_nil () => ()
  end // end of [aux2_mov]
in
  case+ tmps_arg of
  | list_nil () => aux1_mov (res, 0, vps_arg)
  | list_cons _ => aux2_mov (res, tmps_arg, vps_arg)
end // end of [tailcall_arg_move]

//

fn ccomp_exp_app_tmpvar (
    res: &instrlst_vt
  , level: int
  , hit_fun: hityp_t
  , hie_fun: hiexp
  , hies_arg: hiexplst
  , tmp_res: tmpvar_t
  ) : void = let

  var vps_free: valprimlst_vt = list_vt_nil {valprim} ()

  val vp_fun = (
    case+ hie_fun.hiexp_node of
    | HIErefarg
       (refval, freeknd, hie) when freeknd > 0 => vp
      where {
        val vp = ccomp_exp (res, hie)
        val () = case+ vp.valprim_node of
          | VPclo _ => () | VPfun _ => () | _ => (
              vps_free := list_vt_cons (vp, vps_free)
            )
      } // end of [where]
    | _ => ccomp_exp (res, hie_fun)
  ) : valprim
(*
  val () = begin
    prerr "ccomp_exp_app_tmpvar: vp_fun = "; prerr vp_fun; prerr_newline ()
  end
*)
  val vps_arg =
    ccomp_explst (res, hies_arg) where {
    val hies_arg = begin
      hiexplst_refarg_tr (res, level, vps_free, hies_arg)
    end
  } // end of [where]

  val vps_free = $Lst.list_vt_reverse {valprim} (vps_free)

  val ofl = (case+ vp_fun.valprim_node of
    | VPfun fl => Some_vt fl
    | VPclo (_(*knd*), fl, _(*env*)) => Some_vt fl
    | VPcst d2c => let
        val ovp = the_topcstctx_find (d2c)
      in
        case+ ovp of
        | ~Some_vt vp => begin
          case+ vp.valprim_node of
          | VPfun fl => Some_vt fl | _ => None_vt ()
          end // end of [Some_vt]
        | ~None_vt () => None_vt ()
      end // end of [VPcst]
    | _ => None_vt ()
  ) : Option_vt (funlab_t)

  var istail: int = 0
  var tmps_arg: tmpvarlst = list_nil ()
  val () = begin
    case+ ofl of // handling tail-call
    | ~Some_vt (fl) => let
(*
        val () = begin
          prerr "ccomp_exp_app_tmpvar: fl = "; prerr fl; prerr_newline ()
        end
*)
        val () = case+ vps_free of
          | list_vt_nil _ => begin
              if tmpvar_ret_get tmp_res > 0 then istail := 1;
              fold@ vps_free
            end
          | list_vt_cons _ => (fold@ vps_free)
(*
        val () = begin
          prerr "ccomp_exp_app_tmpvar: istail = "; prerr istail; prerr_newline ()
        end
*)
        val () =
          if istail > 0 then let
            val otmps = the_tailcallst_find (fl)
          in
            case+ otmps of
            | ~Some_vt (tmps) => (tmps_arg := tmps)
            | ~None_vt () => (istail := 0)
          end
(*
        val () = begin
          prerr "ccomp_exp_app_tmpvar: istail = "; prerr istail; prerr_newline ()
        end
*)
      in
        if istail > 0 then let
          val fl0 = funlab_top (); val knd =
            (if eq_funlab_funlab (fl, fl0) then 0 else 1): int
          val () = tailcall_arg_move (res, knd, tmps_arg, vps_arg)
        in
          instr_add_call_tail (res, fl)
        end // end of [list_vt_cons]
      end // end of [Some_vt]
    | ~None_vt () => ()
  end // end of [val]

  val () = // handling non-tail-call
    if istail = 0 then begin
      instr_add_call (res, tmp_res, hit_fun, vp_fun, vps_arg)
    end

  val () = instrlst_add_freeptr (res, vps_free)
in
  // empty
end // end of [ccomp_exp_app_tmpvar]

(* ****** ****** *)

fn ccomp_exp_arr1asgn (
    res: &instrlst_vt
  , vp_arr: valprim, hit_elt: hityp_t
  , hies_elt: hiexplst
  ) : void = let
  fun aux (res: &instrlst_vt, i: int, hies: hiexplst):<cloref1> void =
    case+ hies of
    | list_cons (hie, hies) => let
        val vp = ccomp_exp (res, hie)
        macdef list_sing (x) = list_cons (,(x), list_nil ())
        val int = $IntInf.intinf_make_int (i)
        val ind = list_sing (list_sing (valprim_int int))
        val off = OFFSETind (ind, hit_elt)
        val () = instr_add_store_ptr_offs (res, vp_arr, '[off], vp)
      in
        aux (res, i+1, hies)
      end // end of [aux]
    | list_nil () => ()
  // end of [aux]
in
  aux (res, 0, hies_elt)
end // end of [ccomp_exp_arr1asgn]

(* ****** ****** *)

fn ccomp_exp_arrinit_tmpvar (
    res: &instrlst_vt
  , hit_elt: hityp_t
  , ohie_asz: hiexpopt
  , hies_elt: hiexplst
  , tmp_arr: tmpvar_t
  ) : void = let
  val vp_arr = valprim_tmp (tmp_arr)
  val vp_asz = (case+ ohie_asz of
    | Some hie_asz => ccomp_exp (res, hie_asz)
    | None () => let
        val n = $Lst.list_length (hies_elt)
      in
        valprim_int ($IntInf.intinf_make_int n)
      end // end of [None]
  ) : valprim
  val () = instr_add_arr_stack (res, tmp_arr, vp_asz, hit_elt)
in
  case+ ohie_asz of
  | Some _ => begin
    case+ hies_elt of
    | list_cons (hie_elt, _) => let
        val tmp_elt = tmpvar_make (hit_elt)
        val () = ccomp_exp_tmpvar (res, hie_elt, tmp_elt)
        val vp_tsz = valprim_sizeof (hit_elt)
      in
        instr_add_arr1asgn (res, vp_arr, vp_asz, tmp_elt, vp_tsz)
      end // end of [list_cons]
    | list_nil () => ()
    end // end of [Some]
  | None () => begin
      ccomp_exp_arr1asgn (res, vp_arr, hit_elt, hies_elt)
    end // end of [None]
end // end of [ccomp_exp_arrinit]

(* ****** ****** *)

fn ccomp_exp_arrsize_tmpvar (
    res: &instrlst_vt
  , hit_elt: hityp_t
  , hies_elt: hiexplst
  , tmp_res: tmpvar_t
  ) : void = let
  val asz = $Lst.list_length hies_elt
  val () = instr_add_arr_heap (res, tmp_res, asz, hit_elt)
  val tmp_arr = tmpvar_make (hityp_encode hityp_ptr)
  val vp_arr = valprim_tmp (tmp_arr)
  val () = let
    val vp_res = valprim_tmp (tmp_res)
    val hit_arrsz = tmpvar_typ_get tmp_res
(*
viewtypedef
arraysize_viewt0ype_int_viewt0ype (a: viewt@ype, n:int) =
  [l:addr | l <> null]
    (free_gc_v (a, n, l), @[a][n] @ l | ptr l(*arr*), int n(*size*))
*)
    val off = OFFSETlab ($Lab.label_make_int 2(*arr*), hit_arrsz)
  in
    instr_add_load_var_offs (res, tmp_arr, vp_res, '[off])
  end // end of [var]
in
  ccomp_exp_arr1asgn (res, vp_arr, hit_elt, hies_elt)
end // end of [ccomp_exp_arrsize_tmpvar]

(* ****** ****** *)

fun ccomp_exp_lst_tmpvar_rest (
    res: &instrlst_vt
  , d2c_nil: d2con_t
  , hit_nil: hityp_t
  , d2c_cons: d2con_t
  , hit_cons: hityp_t
  , hies: hiexplst
  , vp_top: valprim
  , offs: offsetlst
  , tmp_fst: tmpvar_t
  , vp_fst: valprim
  , tmp_nxt: tmpvar_t
  , vp_nxt: valprim
  ) : void = begin case+ hies of
  | list_cons (hie, hies) => let
      val vp = ccomp_exp (res, hie)
      val () = instr_add_move_con
        (res, tmp_nxt, hit_cons, d2c_cons, '[vp, vp_top])
      val () = instr_add_store_ptr_offs (res, vp_fst, '[], vp_nxt)
      val () = instr_add_move_val
        (res, tmp_fst, valprim_ptrof_ptr_offs (vp_nxt, offs))
    in
      ccomp_exp_lst_tmpvar_rest (
        res
      , d2c_nil, hit_nil, d2c_cons, hit_cons
      , hies, vp_top, offs
      , tmp_fst, vp_fst, tmp_nxt, vp_nxt
      )
    end // end of [list_cons]
  | list_nil () => let
      val () = instr_add_move_con (res, tmp_nxt, hit_nil, d2c_nil, '[])
      val () = instr_add_store_ptr_offs (res, vp_fst, '[], vp_nxt)
    in
      // empty
    end // end of [list_nil]
end // end of [ccomp_exp_lst_tmpvar_rest]

fn ccomp_exp_lst_tmpvar (
    res: &instrlst_vt
  , knd: int
  , hit_elt: hityp_t
  , hies: hiexplst
  , tmp_res: tmpvar_t
  ) : void = let
  val d2conref_nil = (
    if knd > 0 then List_vt_nil else List_nil
  ) : d2conref_t
  val d2c_nil = d2conref_con_get (d2conref_nil)
  val () = the_dynconset_add d2c_nil
  val hit_nil = hityp_tysum_make (d2c_nil, '[])
in
  case+ hies of
  | list_cons (hie, hies) => let
      val d2conref_cons = (
        if knd > 0 then List_vt_cons else List_cons
      ) : d2conref_t
      val d2c_cons = d2conref_con_get (d2conref_cons)
      val () = the_dynconset_add d2c_cons
      val hit_elt = hityp_decode (hit_elt)
      val hit_cons = hityp_tysum_make (d2c_cons, '[hit_elt, hityp_ptr])
      val vp = ccomp_exp (res, hie)
      val hityp_t_ptr = hityp_encode hityp_ptr
      val vp_top = valprim_top (hityp_t_ptr)
      val () = instr_add_move_con
        (res, tmp_res, hit_cons, d2c_cons, '[vp, vp_top])
      val vp_res = valprim_tmp tmp_res
      val hit_cons_flt = let
        val hit_cons = hityp_decode hit_cons
        val HITNAM (_(*knd=1*), name) = hit_cons.hityp_name
      in
        hityp_encode (hityp_extype name)
      end
      val off = OFFSETlab ($Lab.label_make_int 1(*tail*), hit_cons_flt)
      val offs = '[off]
      val tmp_fst = tmpvar_make hityp_t_ptr; val vp_fst = valprim_tmp tmp_fst
      val () = instr_add_move_val (
        res, tmp_fst, valprim_ptrof_ptr_offs (vp_res, offs)
      )
      val tmp_nxt = tmpvar_make hityp_t_ptr; val vp_nxt = valprim_tmp tmp_nxt
    in
      ccomp_exp_lst_tmpvar_rest (
        res
      , d2c_nil, hit_nil, d2c_cons, hit_cons
      , hies, vp_top, offs
      , tmp_fst, vp_fst, tmp_nxt, vp_nxt
      )
    end // end of [list_cons]
  | list_nil () => begin
      instr_add_move_con (res, tmp_res, hit_nil, d2c_nil, '[])
    end // end of [list_nil]
end // end of [ccomp_exp_lst_with_tmpvar]

(* ****** ****** *)

fn ccomp_exp_seq_tmpvar (
    res: &instrlst_vt
  , hies: hiexplst
  , tmp_res: tmpvar_t
  ) : void = let
  fun aux (
      res: &instrlst_vt
    , hie0: hiexp
    , hies: hiexplst
    , tmp_res: tmpvar_t
    ) : void = begin case+ hies of
    | list_cons (hie, hies) => let
        val _(*void*) = ccomp_exp (res, hie0)
      in
        aux (res, hie, hies, tmp_res)
      end
    | list_nil () => begin
        ccomp_exp_tmpvar (res, hie0, tmp_res)
      end
  end // end of [aux]
in
  case+ hies of
  | list_cons (hie, hies) => aux (res, hie, hies, tmp_res)
  | list_nil () => ()
end // end of [ccomp_exp_seq_tmpvar]

(* ****** ****** *)

implement ccomp_exp_tmpvar (res, hie0, tmp_res) = let
(*
  val () = begin
    prerr "ccomp_exp_tmpvar: hie0 = "; prerr hie0; prerr_newline ()
  end
  val () = begin
    prerr "ccomp_exp_tmpvar: hit0 = "; prerr hie0.hiexp_typ; prerr_newline ()
  end
  val () = begin
    prerr "ccomp_exp_tmpvar: tmp_res = "; prerr tmp_res; prerr_newline ()
  end
*)
in
  case+ hie0.hiexp_node of
  | HIEapp (hit_fun, hie_fun, hies_arg) => let
      val level = d2var_current_level_get ()
      val hit_fun = hityp_normalize (hit_fun)
    in
      ccomp_exp_app_tmpvar (
        res, level, hit_fun, hie_fun, hies_arg, tmp_res
      )
    end
  | HIEarrsize (hit_elt, hies) => let
      val hit_elt = hityp_normalize (hit_elt)
    in
      ccomp_exp_arrsize_tmpvar (res, hit_elt, hies, tmp_res)
    end // end of [HIElst]
  | HIEassgn_ptr (hie_ptr, hils, hie_val) =>
      ccomp_exp_assgn_ptr (res, hie_ptr, hils, hie_val)
  | HIEassgn_var (d2v_mut, hils, hie_val) =>
      ccomp_exp_assgn_var (res, d2v_mut, hils, hie_val)
  | HIEbool b => instr_add_move_val (res, tmp_res, valprim_bool b)
  | HIEchar c => instr_add_move_val (res, tmp_res, valprim_char c)
  | HIEcaseof (knd, hies, hicls) => let
      val level = d2var_current_level_get ()
      val vps = ccomp_explst (res, hies)
      val fail = (
        if knd > 0 then KONTnone () else KONTcaseof_fail ()
      ) : kont
      val (pf_mark | ()) = the_dynctx_mark ()
      val branchlst = begin
        ccomp_hiclaulst (level, vps, hicls, tmp_res, fail)
      end
      val () = the_dynctx_unmark (pf_mark | (*none*))
    in
      instr_add_switch (res, branchlst)
    end // end of [HIEcaseof]
  | HIEcon (hit_sum, d2c, hies_arg) => let
(*
      val () = begin
        prerr "ccomp_exp_tmpvar: HIEcon: hit_sum = ";
        prerr hit_sum;
        prerr_newline ()
      end
*)
      val () = if (d2con_is_proof d2c) then begin
        $Loc.prerr_location (hie0.hiexp_loc);
        prerr ": error(ccomp)";
        prerr ": ["; prerr d2c; prerr "] is a proof constructor";
        prerr ", which must not occur at run-time.";
        prerr_newline ();
        $Err.abort {void} ()
      end // end of [val]
        
      val () = the_dynconset_add d2c
      val hit_sum = hityp_normalize (hit_sum)
      val vps = ccomp_explst (res, hies_arg)
    in
      instr_add_move_con (res, tmp_res, hit_sum, d2c, vps)
    end // end of [HIEcon]
  | HIEcst _ => begin
      instr_add_move_val (res, tmp_res, ccomp_exp (res, hie0))
    end // end of [HIEcst]
  | HIEdynload fil => instr_add_dynload_file (res, fil)
  | HIEempty () => ()
  | HIEextval code => let
      val hit0 = hityp_normalize (hie0.hiexp_typ)
    in
      instr_add_move_val (res, tmp_res, valprim_ext (code, hit0))
    end // end of [HIEextval]
  | HIEfloat f(*string*) => begin
      instr_add_move_val (res, tmp_res, valprim_float f)
    end
  | HIEif (hie_cond, hie_then, hie_else) => let
      val vp_cond = ccomp_exp (res, hie_cond)
      val tmp_res_then = tmpvar_make_root (tmp_res)
      var res_then: instrlst_vt = list_vt_nil ()
      val () = ccomp_exp_tmpvar (res_then, hie_then, tmp_res_then)
      val tmp_res_else = tmpvar_make_root (tmp_res)
      var res_else: instrlst_vt = list_vt_nil ()
      val () = ccomp_exp_tmpvar (res_else, hie_else, tmp_res_else)
      val res_then = $Lst.list_vt_reverse_list res_then
      val res_else = $Lst.list_vt_reverse_list res_else
      val ins = INSTRcond (vp_cond, res_then, res_else)
    in
      res := list_vt_cons (ins, res)
    end // end of [HIEif]
  | HIEint (_(*str*), int) => begin
      instr_add_move_val (res, tmp_res, valprim_int int)
    end
  | HIEintsp (str, int) => begin
      instr_add_move_val (res, tmp_res, valprim_intsp (str, int))
    end
  | HIElam (hips_arg, hie_body) => let
      val vp_lam = ccomp_exp_lam
        (hie0.hiexp_loc, hie0.hiexp_typ, hips_arg, hie_body)
    in
      instr_add_move_val (res, tmp_res, vp_lam)
    end // end of [HIElam]
  | HIElazy_delay (hie_eval) => let
      val hit_eval = hityp_normalize (hie_eval.hiexp_typ)
      val vp_clo = ccomp_exp_lazy_delay (hie0.hiexp_loc, hie_eval)
    in
      instr_add_move_lazy_delay (res, tmp_res, 0(*lin*), hit_eval, vp_clo)
    end // end of [HIElazy_delay]
  | HIElazy_vt_delay (hie_eval, hie_free) => let
      val hit_eval = hityp_normalize (hie_eval.hiexp_typ)
      val vp_clo = ccomp_exp_lazy_vt_delay (hie0.hiexp_loc, hie_eval, hie_free)
    in
      instr_add_move_lazy_delay (res, tmp_res, 1(*lin*), hit_eval, vp_clo)
    end // end of [HIElazy_vt_delay]
  | HIElazy_force (lin, hie) => let
      val vp_lazy = ccomp_exp (res, hie)
      val hit_val = hityp_normalize (hie0.hiexp_typ)
    in
      instr_add_move_lazy_force (res, tmp_res, lin, hit_val, vp_lazy)
    end // end of [HIElazy_force]
  | HIElet (hids, hie) => let
      val (pf_mark | ()) = the_dynctx_mark ()
      val () = ccomp_declst (res, hids)
      val () = ccomp_exp_tmpvar (res, hie, tmp_res)
      val () = the_dynctx_unmark (pf_mark | (*none*))
    in
      // empty
    end // end of [HIElet]
  | HIEloop (ohie_init, hie_test, ohie_post, hie_body) => begin
      ccomp_exp_loop (res, ohie_init, hie_test, ohie_post, hie_body)
    end // end of [HIEloop]
  | HIEloopexn (knd) => begin
      instr_add_loopexn (res, knd, loopexnlablst_get knd)
    end // end of [HIEloopexn]
  | HIElst (knd, hit_elt, hies) => let
      val hit_elt = hityp_normalize (hit_elt)
    in
      ccomp_exp_lst_tmpvar (res, knd, hit_elt, hies, tmp_res)
    end // end of [HIElst]
  | HIEraise hie_exn => let
      val vp_exn = ccomp_exp (res, hie_exn)
    in
      instr_add_raise (res, vp_exn)
    end // end of [HIEraise]
  | HIErec (recknd, hit_rec, lhies) => let
      val hit_rec = hityp_normalize (hit_rec)
    in
      case+ lhies of
      | LABHIEXPLSTcons (l, hie, LABHIEXPLSTnil ())
          when hityp_t_is_tyrecsin hit_rec => begin
          ccomp_exp_tmpvar (res, hie, tmp_res)
        end
      | _ => let
          val lvps = ccomp_labexplst (res, lhies)
        in
          instr_add_move_rec (res, tmp_res, recknd, hit_rec, lvps)
        end
    end // end of [HIErec]
  | HIEsel (hie, hils) => let
      val vp = ccomp_exp (res, hie)
      val offs = ccomp_hilablst (res, hils)
    in
      instr_add_select (res, tmp_res, vp, offs)
    end // end of [HIEsel]
  | HIEsel_ptr (hie_ptr, hils) => let
      val vp_ptr = ccomp_exp (res, hie_ptr)
      val offs = ccomp_hilablst (res, hils)
    in
      instr_add_load_ptr_offs (res, tmp_res, vp_ptr, offs)
    end // end of [HIEsel_ptr]
  | HIEsel_var (d2v_mut, hils) => let
      val vp_mut = ccomp_exp_var (d2v_mut)
      val offs = ccomp_hilablst (res, hils)
    in
      instr_add_load_var_offs (res, tmp_res, vp_mut, offs)
    end // end of [HIEsel_var]
  | HIEseq (hies) => ccomp_exp_seq_tmpvar (res, hies, tmp_res)      
  | HIEstring (str, len) => begin
      instr_add_move_val (res, tmp_res, valprim_string (str, len))
    end
  | HIEtop () => let
      val hit0 = hityp_normalize (hie0.hiexp_typ)
    in
      instr_add_move_val (res, tmp_res, valprim_top (hit0))
    end // end of [HIEtop]
  | HIEtrywith (hie_try, hicls) => let
      val level = d2var_current_level_get ()
      var res_try: instrlst_vt = list_vt_nil ()
      val () = ccomp_exp_tmpvar (res_try, hie_try, tmp_res)
      val res_try: instrlst = $Lst.list_vt_reverse_list res_try
      val hit_exn = hityp_encode (hityp_ptr)
      val tmp_exn = tmpvar_make hit_exn; val vp_exn = valprim_tmp tmp_exn
      val vps = '[vp_exn]; val fail = KONTraise vp_exn
      val brs = ccomp_hiclaulst (level, vps, hicls, tmp_res, fail)
    in
      instr_add_trywith (res, res_try, tmp_exn, brs)
    end // end of [HIEtrywith]
  | HIEvar d2v => begin
      instr_add_move_val (res, tmp_res, ccomp_exp_var (d2v))
    end // end of [HIEvar]
  | _ => begin
      $Loc.prerr_location (hie0.hiexp_loc);
      prerr ": ccomp_exp_tmpvar: not implemented: hie0 = ";
      prerr hie0; prerr_newline ();
      $Err.abort {void} ()
    end // end of [_]
end // end of [ccomp_exp_tmpvar]

(* ****** ****** *)

implement ccomp_hilab (res, hil) = begin
  case+ hil.hilab_node of
  | HILind (hiess_ind, hit_elt) => let
      val hit_elt = hityp_normalize (hit_elt)
      val vpss_ind = ccomp_explstlst (res, hiess_ind)
    in
      OFFSETind (vpss_ind, hit_elt)
    end
  | HILlab (l, hit_rec) => let
      val hit_rec = hityp_normalize hit_rec
    in
      OFFSETlab (l, hit_rec)
    end
end // end of [ccomp_hilab]

implement ccomp_hilablst (res, hils) = begin
  case+ hils of
  | list_cons (hil, hils) => let
      val off = ccomp_hilab (res, hil)
    in
      list_cons (off, ccomp_hilablst (res, hils))
    end
  | list_nil () => list_nil ()
end // end of [ccomp_hilablst]

(* ****** ****** *)

fn d2var_typ_ptr_get (d2v: d2var_t): s2exp = let
  val d2v_view = (
    case+ d2var_view_get d2v of
    | D2VAROPTsome d2v_view => d2v_view
    | D2VAROPTnone () => begin
        prerr "Internal Error: d2var_typ_ptr_get: d2v = ";
        prerr d2v; prerr_newline ();
        $Err.abort {d2var_t} ()
      end
  ) : d2var_t
  val s2e_view = (
    case+ d2var_mastyp_get (d2v_view) of
    | Some s2e_view => s2e_view | None () => begin
        prerr "Internal Error: d2var_typ_ptr_get: d2v_view = ";
        prerr d2v_view; prerr_newline ();
        $Err.abort {s2exp} ()
      end
  ) : s2exp
  val s2e_elt = (
    case+ un_s2exp_at_viewt0ype_addr_view (s2e_view) of
    | ~Some_vt (s2es2e) => s2es2e.0 | ~None_vt () => begin
      prerr "Internal Error: d2var_typ_ptr_get: s2e_view = ";
      prerr s2e_view; prerr_newline ();
      $Err.abort {s2exp} ()
    end
  ) : s2exp
in
  s2e_elt
end // end of [d2var_typ_ptr_get]

(* ****** ****** *)

fun ccomp_fundeclst_init {n:nat}
  (level: int, fundecs: list (hifundec, n))
  : list_vt (funlab_t, n) = begin
  case+ fundecs of
  | list_cons (fundec, fundecs) => let
      val d2v = fundec.hifundec_var
      val () = d2var_lev_set (d2v, level)
      val s2e = d2var_mastyp_get_some (fundec.hifundec_loc, d2v)
      val hit = hityp_normalize (s2exp_tr (1(*deep*), s2e))
      val fl = funlab_make_var_typ (d2v, hit)
      val vp = valprim_funclo_make (fl)
      val () = the_dynctx_add (d2v, vp)
    in
      list_vt_cons (fl, ccomp_fundeclst_init (level, fundecs))
    end
  | list_nil () => list_vt_nil ()
end // end of [ccomp_fundeclst_init]

//

fn ccomp_fntdeclst_main {n:nat} (
    loc0: loc_t
  , fundecs: list (hifundec, n)
  , fls: list_vt (funlab_t, n)
  ) : void = let
  val (pf_tailcallst_mark | ()) = the_tailcallst_mark ()
  val () = auxlst_push (fls) where {
    fn aux_push (fl: funlab_t): void = let
      val tmps = tmpvarlst_make (funlab_typ_arg_get fl)
      val () = funlab_tailjoined_set (fl, tmps)
    in
      the_tailcallst_add (fl, tmps)
    end
    fun auxlst_push {n:nat}
      (fls: !list_vt (funlab_t, n)): void = begin
      case+ fls of
      | list_vt_cons (fl, !fls1) => begin
          aux_push fl; auxlst_push (!fls1); fold@ fls
        end
      | list_vt_nil () => fold@ fls
    end
  }
  val entrylst = auxlst_ccomp (fundecs, fls) where {
    fn aux_ccomp (fundec: hifundec, fl: funlab_t): funentry_t = let
      val prolog = '[INSTRfunlab fl]
      val hie_def = fundec.hifundec_def
    in
      case+ hie_def.hiexp_node of
      | HIElam (hips_arg, hie_body) => ccomp_exp_lam_funlab
          (fundec.hifundec_loc, prolog, hips_arg, hie_body, fl)
      | _ => begin
          prerr "Internal Error: ccomp_fntdeclst_main; aux_ccomp: hie_def = ";
          prerr hie_def;
          prerr_newline ();
          $Err.abort {funentry_t} ()
        end
    end // end of [aux_ccomp]
    fun auxlst_ccomp {n:nat}
      (fundecs: list (hifundec, n), fls: list_vt (funlab_t, n))
      : list (funentry_t, n) = begin case+ fls of
      | ~list_vt_cons (fl, fls) => let
          val+ list_cons (fundec, fundecs) = fundecs
          val entry = aux_ccomp (fundec, fl)
        in
          list_cons (entry, auxlst_ccomp (fundecs, fls))
        end
      | ~list_vt_nil () => list_nil ()
    end // end of [auxlst_ccomp]
  }
  val () = the_tailcallst_unmark (pf_tailcallst_mark | (*none*))
in
  ccomp_tailjoin_funentrylst (loc0, entrylst)
end // end of [ccomp_fntdeclst_main]

//

fun ccomp_fundeclst_main {n:nat} (
    fundecs: list (hifundec, n), fls: list_vt (funlab_t, n)
  ) : void = begin case+ fls of
  | ~list_vt_cons (fl, fls) => let
      val+ list_cons (fundec, fundecs) = fundecs
      val hie_def = fundec.hifundec_def
      val () = begin case+ hie_def.hiexp_node of
        | HIElam (hips_arg, hie_body) => let
            val (pf_tailcallst_mark | ()) = the_tailcallst_mark ()
            val () = the_tailcallst_add (fl, list_nil ())
            val prolog = '[INSTRfunlab fl]
            val _(*funentry*) = ccomp_exp_lam_funlab
              (fundec.hifundec_loc, prolog, hips_arg, hie_body, fl)
            val () = the_tailcallst_unmark (pf_tailcallst_mark | (*none*))
          in
            // empty
          end
        | _ => begin
            prerr "Internal Error: ccomp_fundeclst_main: hie_def = ";
            prerr hie_def;
            prerr_newline ();
            $Err.abort {void} ();
          end
      end // end of [val]
    in
      ccomp_fundeclst_main (fundecs, fls)
    end // end of [list_vt_cons]
  | ~list_vt_nil () => ()
end // end of [ccomp_fundeclst_main]

(* ****** ****** *)

fn ccomp_valdeclst (
    res: &instrlst_vt
  , level: int
  , valdecs: hivaldeclst
  , fail: kont
  ) : void = let
  fun aux (res: &instrlst_vt, valdecs: hivaldeclst)
    :<cloptr1> void = begin case+ valdecs of
    | list_cons (valdec, valdecs) => let
        val vp = ccomp_exp (res, valdec.hivaldec_def)
        val hip = valdec.hivaldec_pat
        val () = ccomp_patck (res, vp, hip, fail)
        val () = ccomp_match (res, level, vp, hip)
        val () = instr_add_valprimlst_free (res)
      in
        aux (res, valdecs)
      end // end of [list_cons]
    | list_nil () => ()
  end // end of [aux]
in
  aux (res, valdecs)
end // end of [ccomp_valdeclst]

(* ****** ****** *)

fn ccomp_valdeclst_rec (
    res: &instrlst_vt, level: int, valdecs: hivaldeclst
  ) : void = () where {
  val tmps = aux1 (res, valdecs) where {
    fun aux1 {n:nat} .<n>.
      (res: &instrlst_vt, valdecs: list (hivaldec, n))
      :<cloref1> list (tmpvar_t, n) = case+ valdecs of
      | list_cons (valdec, valdecs) => let
          val hip = valdec.hivaldec_pat
          val hit = hityp_normalize (hip.hipat_typ)
          val tmp = tmpvar_make (hit)
          val vp = valprim_tmp (tmp)
          val () = ccomp_match (res, level, vp, hip)
        in
          list_cons (tmp, aux1 (res, valdecs))
        end // end of [list_cons]
      | list_nil () => list_nil ()
    // end of [aux1]
  } // end of [val]
  val () = aux2 (res, valdecs, tmps) where {
    fun aux2 {n:nat} (
        res: &instrlst_vt
      , valdecs: list (hivaldec, n)
      , tmps: list (tmpvar_t, n)
      ) : void = case+ valdecs of
      | list_cons (valdec, valdecs) => let
          val+ list_cons (tmp, tmps) = tmps
          val () = ccomp_exp_tmpvar (res, valdec.hivaldec_def, tmp)
        in
          aux2 (res, valdecs, tmps)
        end // end of [list_cons]
      | list_nil () => ()
    // end of [aux]
  } // end of [va;]
} // end of [ccomp_valdeclst_rec]

(* ****** ****** *)

fn ccomp_vardec_sta
  (res: &instrlst_vt, level: int, vardec: hivardec)
  : void = let
  val d2v = vardec.hivardec_ptr
  val () = d2var_lev_set (d2v, level)
  val s2e = d2var_typ_ptr_get d2v
  val hit = s2exp_tr (0(*deep*), s2e)
  val tmp = tmpvar_make (hityp_normalize hit)
  val () = instr_add_vardec (res, tmp)
  val () = the_dynctx_add (d2v, valprim_tmp_ref tmp)
in
  case+ vardec.hivardec_ini of
  | Some hie => ccomp_exp_tmpvar (res, hie, tmp) | None () => ()
end // end of [ccomp_vardec_sta]

fn ccomp_vardec_dyn
  (res: &instrlst_vt, level: int, vardec: hivardec)
  : void = let
  val d2v = vardec.hivardec_ptr
  val () = d2var_lev_set (d2v, level)
  val hit_ptr = s2exp_tr (0(*deep*), s2e) where {
    // [s2e] must a pointer type
    val s2e = d2var_typ_get_some (d2var_loc_get d2v, d2v)
  } // end of [val]
  val tmp_ptr = tmpvar_make (hityp_normalize hit_ptr)
  val () = instr_add_vardec (res, tmp_ptr)
  val () = the_dynctx_add (d2v, valprim_tmp tmp_ptr)
  val hie_ini = (case+ vardec.hivardec_ini of
    | Some hie => hie | None => begin
        $Loc.prerr_location (vardec.hivardec_loc);
        prerr ": Internal Error: ccomp_vardec_dyn: no initialization.";
        prerr_newline (); $Err.abort {hiexp} ()
      end // end of [None]
  ) : hiexp
in
  case+ hie_ini.hiexp_node of
  | HIEarrinit (hit_elt, ohie_asz, hies_elt) => let
      val hit_elt = hityp_normalize hit_elt
    in
      ccomp_exp_arrinit_tmpvar (res, hit_elt, ohie_asz, hies_elt, tmp_ptr)
    end // end of [HIEarrinit]
  | _ => begin
      prerr "Internal Error";
      prerr ": ccomp_vardec_dyn: unsupported initialization form";
      prerr ": hie_ini = "; prerr hie_ini; prerr_newline ();
      $Err.abort {void} ()
    end // end of [_]
end // end of [ccomp_vardec_dyn]

fn ccomp_vardec
  (res: &instrlst_vt, level: int, vardec: hivardec): void = let
  val knd = vardec.hivardec_knd
in
  case+ 0 of
  | _ when (knd = 0) => ccomp_vardec_sta (res, level, vardec)
  | _ (* dynamic allocation *) => ccomp_vardec_dyn (res, level, vardec)
end // end of [ccomp_vardec]

(* ****** ****** *)

fn ccomp_vardeclst (
    res: &instrlst_vt
  , level: int
  , vardecs: hivardeclst
  ) : void = let
  fun aux
    (res: &instrlst_vt, vardecs: hivardeclst):<cloptr1> void =
    case+ vardecs of
    | list_cons (vardec, vardecs) => let
        val () = ccomp_vardec (res, level, vardec) in aux (res, vardecs)
      end // end of [list_cons]
    | list_nil () => ()
  // end of [aux]
in
  aux (res, vardecs)
end // end of [ccomp_vardeclst]

(* ****** ****** *)

fn ccomp_impdec
  (res: &instrlst_vt, impdec: hiimpdec): void = let
  fun aux (
      res: &instrlst_vt
    , d2c: d2cst_t
    , tmparg: hityplstlst (* not yet normalized *)
    , hie: hiexp
    ) : void = begin case+ hie.hiexp_node of
    | HIElam (hips_arg, hie_body) => let
        val hit = hityp_normalize (hie.hiexp_typ)
        val fl = funlab_make_cst_typ (d2c, tmparg, hit)
        val fc = funlab_funclo_get (fl)
(*
        val () = begin
          prerr "ccomp_impdec: aux: fl = "; prerr fl; prerr_newline ()
        end
*)
        val vp_lam = valprim_funclo_make (fl)
        val () = the_topcstctx_add (d2c, vp_lam)

        val (pf_tailcallst_mark | ()) = the_tailcallst_mark ()
        val () = the_tailcallst_add (fl, list_nil ())
        val _(*funentry*) = ccomp_exp_lam_funlab
          (hie.hiexp_loc, prolog, hips_arg, hie_body, fl) where {
            val prolog = '[INSTRfunlab fl]
          } // end of [where]
        val () = the_tailcallst_unmark (pf_tailcallst_mark | (*none*))

        val () = case+ 0 of
          | _ when $Lst.list_is_cons tmparg => let
              val name = funlab_name_get fl in tmpnamtbl_add (name, vp_lam)
            end // end of [_ when ...]
          | _ => begin case+ d2cst_kind_get d2c of
            | $Syn.DCSTKINDval () => begin case+ fc of
              | $Syn.FUNCLOclo _ => begin
                  the_glocstlst_add_clo d2c; instr_add_define_clo (res, d2c, fl)
                end // end of [FUNCLOclo]
              | $Syn.FUNCLOfun _ => begin
                  the_glocstlst_add_fun d2c; instr_add_define_fun (res, d2c, fl)
                 end
              end // end of [FUNCLOfun]
            | _ => ()
          end // end of [_]
      in
        // empty
      end // end of [HIElam]
    | HIEfix (d2v_fix, hie_def) => let
        // should it require that [tmparg] be empty?
        val hit = hityp_normalize (hie.hiexp_typ)
        val vp_cst = valprim_cst (d2c, hit)
        val () = the_dynctx_add (d2v_fix, vp_cst)
      in
        aux (res, d2c, tmparg, hie_def)
      end // end of [HIEfix]
    | _ => let
        val vp = ccomp_exp (res, hie)
        val () = the_topcstctx_add (d2c, vp)
        val () = the_glocstlst_add_val (d2c, vp)
      in
        instr_add_define_val (res, d2c, vp)
      end
  end // end of [aux]
  val d2c = impdec.hiimpdec_cst
(*
  val () = begin
    prerr "ccomp_impdec: d2c = "; prerr d2c; prerr_newline ()
  end
*)
in
  case+ 0 of
  | _ when d2cst_is_proof d2c => let
      val () = the_topcstctx_add (d2c, valprim_void ())
    in
      // proof is not compiled
    end
  | _ => begin case+ impdec.hiimpdec_decarg of
    | list_cons _ => () // template is not compiled
    | list_nil () => begin
        aux (res, d2c, impdec.hiimpdec_tmparg, impdec.hiimpdec_def)
      end // end of [list_nil]
  end // end of [if]
end // end of [ccomp_impdec]

(* ****** ****** *)

implement ccomp_dec (res, hid0) = let
(*
  val () = (prerr "ccomp_dec: enter"; prerr_newline ())
*)
in
  case+ hid0.hidec_node of
  | HIDsaspdec _(*s2aspdec*) => () // run-time checking?
  | HIDdcstdec (knd, d2cs) => begin case+ 0 of
    | _ when $Syn.dcstkind_is_proof knd => ()
    | _ => $Lst.list_foreach_fun (d2cs, the_dyncstset_add_if)
    end // end of [HIDdcstdec]
  | HIDdatdec (datknd, s2cs) => begin case+ 0 of
    | _ when $Syn.datakind_is_proof (datknd) => ()
    | _ => the_datcstlst_adds (s2cs)
    end // end of [HIDdatdec]
  | HIDexndec (d2cs) => the_exnconlst_adds (d2cs)
  | HIDextype (name, hit_def) => let
      val hit_def = hityp_normalize (hit_def)
    in
      the_extypelst_add (name, hit_def)
    end // end of [HIDextype]
  | HIDextval (name, hie_def) => let
      val vp_def = ccomp_exp (res, hie_def)
      val () = the_extvallst_add (name, vp_def)
      val () = instr_add_extval (res, name, vp_def)
    in
      // empty
    end // end of [extval]
  | HIDextern (pos, code) => the_extcodelst_add (pos, code)
  | HIDfundecs (decarg, knd, fundecs) => let
      val level = d2var_current_level_get () in case+ decarg of
      | list_nil () => let
          val fls = ccomp_fundeclst_init (level, fundecs)
        in
          case+ fundecs of
          | list_cons (_, list_cons (_, _)) // mutual recursion
              when $Syn.funkind_is_tailrecur knd => begin
              ccomp_fntdeclst_main (hid0.hidec_loc, fundecs, fls)
            end
          | _ => ccomp_fundeclst_main (fundecs, fls)
        end // end of [list_nil]
      | list_cons _ => () // template
    end // end of [HIDfundecs]
  | HIDvaldecs (valknd, valdecs) => let
      val level = d2var_current_level_get ()
      val fail = (case+ valknd of
        | $Syn.VALKINDvalplus () => KONTnone () | _ => KONTcaseof_fail ()
      ) : kont
    in
      ccomp_valdeclst (res, level, valdecs, fail)
    end // end of [HIDvaldecs]
  | HIDvaldecs_rec (valdecs) => let
      val level = d2var_current_level_get ()
    in
      ccomp_valdeclst_rec (res, level, valdecs)
    end // end of [HIDvaldecs_rec]
  | HIDvardecs (vardecs) => let
      val level = d2var_current_level_get ()
    in
      ccomp_vardeclst (res, level, vardecs)
    end // end of [HIDvardecs]
  | HIDimpdec impdec => ccomp_impdec (res, impdec)
  | HIDimpdec_prf d2c => let
      val vp = valprim_void () in the_topcstctx_add (d2c, vp)
    end // end of [HIDimpdec_prf]
  | HIDlocal (hids_head, hids_body) => let
      val () = ccomp_declst (res, hids_head)
    in
      ccomp_declst (res, hids_body)
    end // end of [HIDlocal]
  | HIDdynload fil => begin
      the_dynfilelst_add (fil); instr_add_dynload_file (res, fil)
    end // end of [HIDdynload]
  | HIDstaload (fil) => begin the_stafilelst_add (fil) end
  | HIDlist hids => ccomp_declst (res, hids)
  | _ => begin
      $Loc.prerr_location (hid0.hidec_loc);
      prerr ": ccomp_dec: not implemented yet."; prerr_newline ();
      $Err.abort {void} ()
    end // end of [_]
end // end of [ccomp_dec]

implement ccomp_declst (res, hids) = begin
  case+ hids of
  | list_cons (hid, hids) => begin
      ccomp_dec (res, hid); ccomp_declst (res, hids)
    end
  | list_nil () => ()
end // end of [ccomp_dec]

(* ****** ****** *)

(* end of [ats_ccomp_trans.dats] *)
