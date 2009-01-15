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

// Time: October 2007
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

%{^

#include "ats_counter.cats" /* only needed for [ATS/Geizella] */
#include "ats_intinf.cats"  /* only needed for [ATS/Geizella] */

%}

(* ****** ****** *)

staload Err = "ats_error.sats"
staload IntInf = "ats_intinf.sats"
staload Lst = "ats_list.sats"

(* ****** ****** *)

staload "ats_staexp2.sats"
staload "ats_stadyncst2.sats"
staload "ats_trans3_env.sats"

(* ****** ****** *)

#define nil list_nil
#define :: list_cons
#define cons list_cons

(* ****** ****** *)

overload = with $Lab.eq_label_label

overload prerr with $Lab.prerr_label
overload prerr with $Loc.prerr_location

(* ****** ****** *)

fun s2exp_link_remove_flag (s2e0: s2exp, flag: &int): s2exp = let
(*
  val () = begin
    print "s2exp_link_remove_flag: s2e0 = "; print s2e0; print_newline ()
  end // end of [val]
*)
in
  case+ s2e0.s2exp_node of
  | S2Ecst s2c => begin
      if s2cst_isrec_get s2c then s2e0
      else begin case+ s2cst_def_get s2c of
        | Some s2e => begin
            flag := flag + 1; s2exp_link_remove_flag (s2e, flag)
          end
        | None () => s2e0
      end
    end // end of [S2Ecst]
    // the link of s2V should not be updated!!!
  | S2EVar s2V => begin case+ s2Var_link_get (s2V) of
    | Some s2e => begin
        flag := flag + 1; s2exp_link_remove_flag (s2e, flag)
      end
    | None () => s2e0
    end // end of [S2EVar]
  | _ => s2e0
end // end of [s2exp_link_remove_flag]

implement s2exp_link_remove (s2e0) = begin
  let var flag: int = 0 in s2exp_link_remove_flag (s2e0, flag) end
end // end of [s2exp_link_remove]

(* ****** ****** *)

fun labs2explst_readize (_v: s2exp, ls2es: labs2explst): labs2explst =
  case+ ls2es of
  | LABS2EXPLSTcons (l, s2e, ls2es) => begin
      LABS2EXPLSTcons (l, s2exp_readize (_v, s2e), labs2explst_readize (_v, ls2es))
    end
  | LABS2EXPLSTnil () => LABS2EXPLSTnil ()

implement s2exp_readize (_v, s2e) = let
  val s2t_s2e = s2e.s2exp_srt
in
  if s2rt_is_linear s2t_s2e then let
    val s2t = s2rt_readize s2t_s2e
  in
    case+ s2e.s2exp_node of
    | S2Etyrec (knd, npf, ls2es) => let
        val ls2es = labs2explst_readize (_v, ls2es)
      in
        s2exp_tyrec_srt (s2t, knd, npf, ls2es)
      end
    | S2Eunion (stamp, s2i, ls2es) => let
        val ls2es = labs2explst_readize (_v, ls2es)
      in
        s2exp_union_srt (s2t, stamp, s2i, ls2es)
      end
    | _ => s2exp_read_srt (s2t, _v, s2e)
  end else begin
    s2e // [r@ead] is identity on nonlinear types
  end
end // end of [s2exp_readize]

implement s2expopt_readize (_v, os2e) = begin
  case+ os2e of
  | Some s2e => begin
      if s2exp_is_linear s2e then Some (s2exp_readize (_v, s2e))
      else os2e
    end
  | None () => os2e
end // end of [s2expopt_readize]

(* ****** ****** *)

fun labs2explst_topize
  (knd: int, ls2es: labs2explst): labs2explst = begin
  case+ ls2es of
  | LABS2EXPLSTcons (l, s2e, ls2es) => let
      val s2e = s2exp_topize (knd, s2e)
    in
      LABS2EXPLSTcons (l, s2e, labs2explst_topize (knd, ls2es))
    end
  | LABS2EXPLSTnil () => LABS2EXPLSTnil ()
end // end of [labs2explst_topize]

implement s2exp_topize (knd, s2e) = let
  val s2t = s2e.s2exp_srt
  val isdone: bool = begin
    if knd > 0 (*typization*) then
      if s2rt_is_linear s2t then false else true
    else false
  end
  val s2e = s2exp_whnf s2e
in
  if isdone then begin 
    s2e // there is no need for any change
  end else let
    val s2t_new = (
      if s2rt_is_proof s2t then s2rt_prop else
        (if s2rt_is_boxed s2t then s2rt_type else s2rt_t0ype)
    ) : s2rt
  in
    case+ s2e.s2exp_node of
    | S2Etyarr (s2e_elt, s2ess_dim) => let
        val s2e_elt = s2exp_topize (knd, s2e_elt)
      in
        s2exp_tyarr (s2e_elt, s2ess_dim)
      end
    | S2Etyrec (recknd, npf, ls2es) => begin
      case+ recknd of
      | TYRECKINDbox () => s2exp_top_srt (s2t_new, knd, s2e)
      | _ (* flat record *) => let
          val ls2es = labs2explst_topize (knd, ls2es)
        in
          s2exp_tyrec_srt (s2t_new, recknd, npf, ls2es)
        end
      end // end of [S2Etyrec]
    | S2Eunion (stamp, _(*ind*), ls2es) => let
        val s2i = s2exp_int (~1)
      in
        s2exp_union_srt (s2t_new, stamp, s2i, ls2es)
      end
    | _ => s2exp_top_srt (s2t_new, knd, s2e)
  end // end of [if]
end // end of [s2exp_topize]

implement s2exp_topize_0 (s2e) = s2exp_topize (0(*knd*), s2e)
implement s2exp_topize_1 (s2e) = s2exp_topize (1(*knd*), s2e)

(* ****** ****** *)

fun s2exp_whnf_flag
  (s2e0: s2exp, flag: &int): s2exp = let
  val s2e0 = s2exp_link_remove_flag (s2e0, flag)
in
  case+ s2e0.s2exp_node of
  | S2Eapp (s2e_fun, s2es_arg) => let
      val flag0 = flag; val s2e_fun = s2exp_whnf_flag (s2e_fun, flag)
    in
      case+ s2e_fun.s2exp_node of
      | S2Elam (s2vs_arg, s2e_body) => let
          fun aux (s2vs: s2varlst, s2es: s2explst): stasub_t =
            case+ (s2vs, s2es) of
            | (cons (s2v, s2vs), cons (s2e, s2es)) =>
                stasub_add (aux (s2vs, s2es), s2v, s2e)
            | (nil _, nil _) => stasub_nil
            | (_, _) => begin
                prerr "Internal Error: s2exp_whnf: S2Eapp: arity error";
                prerr_newline ();
                $Err.abort {stasub_t} ()
              end
          val sub = aux (s2vs_arg, s2es_arg)
          val () = flag := flag + 1
        in
          s2exp_whnf_flag (s2exp_subst (sub, s2e_body), flag)
        end
      | _ => begin
          if flag > flag0 then begin
            s2exp_app_srt (s2e0.s2exp_srt, s2e_fun, s2es_arg)
          end else begin
            s2e0 // there is no change
          end
        end
    end // end of [S2Eapp]
  | S2Eclo (knd, s2e) => let
      val flag0 = flag; val s2e = s2exp_whnf_flag (s2e, flag)
    in
      case+ s2e.s2exp_node of
      | S2Efun (_(*fc*), lin, s2fe, npf, s2es_arg, s2e_res) => let
          val fc1 = $Syn.FUNCLOclo knd
          val () = flag := flag + 1
          val s2t1: s2rt =
            if knd = 0 then
              if lin > 0 then s2rt_viewt0ype else s2rt_t0ype
            else begin
              s2e0.s2exp_srt
            end
        in
          s2exp_fun_srt (s2t1, fc1, lin, s2fe, npf, s2es_arg, s2e_res)
        end
      | _ => begin
          if flag > flag0 then s2exp_clo_srt (s2e.s2exp_srt, knd, s2e) else s2e0
        end
    end // end of [S2Eclo]
  | S2Ecrypt (s2e) => let
      val flag0 = flag; val s2e = s2exp_whnf_flag (s2e, flag)
    in
      case+ s2e.s2exp_node of
      | S2Eexi (s2vs, s2ps, s2e_scope) => begin
          flag := flag + 1; s2exp_exi (s2vs, s2ps, s2exp_crypt s2e_scope)
        end
      | S2Euni (s2vs, s2ps, s2e_scope) => begin
          flag := flag + 1; s2exp_uni (s2vs, s2ps, s2exp_crypt s2e_scope)
        end
      | _ => s2e0
    end // end of [S2Ecrypt]
  | S2Eread (_v, s2e) => let
      val flag0 = flag; val s2e = s2exp_whnf_flag (s2e, flag)
    in
      case+ s2e.s2exp_node of
      | S2Etyrec (knd, npf, ls2es) => let
          val ls2es = labs2explst_readize (_v, ls2es)
        in
          flag := flag + 1; s2exp_tyrec_srt (s2e0.s2exp_srt, knd, npf, ls2es)
        end
      | S2Eunion (stamp, s2i, ls2es) => let
          val ls2es = labs2explst_readize (_v, ls2es)
        in
          flag := flag + 1; s2exp_union_srt (s2e0.s2exp_srt, stamp, s2i, ls2es)
        end
      | _ => begin
          if flag > flag0 then s2exp_read_srt (s2e0.s2exp_srt, _v, s2e) else s2e0
        end
    end // end of [S2Eread]
  | S2Esel (s2e_tup, ind) => let
      val flag0 = flag; val s2e_tup = s2exp_whnf s2e_tup
    in
      case+ s2e_tup.s2exp_node of
      | S2Etup s2es => let
          fun aux (s2es: s2explst, n: int): s2exp = // the [nth] function
            case+ s2es of
            | cons (s2e, s2es) => (if n > 0 then aux (s2es, n-1) else s2e)
            | nil () => begin
                prerr "Internal Error: s2exp_whnf: S2Etup: subscript error";
                prerr_newline ();
                $Err.abort {s2exp} ()
              end
        in
          flag := flag + 1; s2exp_whnf_flag (aux (s2es, ind), flag)
        end
      | _ => begin
          if flag > flag0 then begin
            s2exp_sel_srt (s2e0.s2exp_srt, s2e_tup, ind)
          end else s2e0 // there is no change
        end
    end // end of [S2Esel]
  | S2Esizeof s2e => let
      val () = flag := flag + 1; val s2ze = s2zexp_make_s2exp s2e
    in
      s2exp_size s2ze
    end
  | S2Etop (knd, s2e) => let
      val flag0 = flag; val s2e = s2exp_whnf_flag (s2e, flag)
    in
      case+ s2e.s2exp_node of
      | S2Etop (knd1, s2e1) => let
          val () = flag := flag + 1
          val knd1: int = if knd > 0 then knd1 else 0
        in
          s2exp_topize (knd1, s2e1)
        end
      | S2Etyarr (s2e_elt, s2ess_dim) => let
          val s2e_elt = s2exp_topize (knd, s2e_elt)
        in
          flag := flag + 1; s2exp_tyarr (s2e_elt, s2ess_dim)
        end
      | S2Etyrec (recknd, npf, ls2es) => begin case+ recknd of
        | TYRECKINDbox () => begin
            if flag > flag0 then s2exp_top_srt (s2e0.s2exp_srt, knd, s2e)
            else s2e0
          end
        | _ (* flat record *) => let
            val ls2es = labs2explst_topize (knd, ls2es)
          in
            flag := flag + 1;
            s2exp_tyrec_srt (s2e0.s2exp_srt, recknd, npf, ls2es)
          end
        end // end of [S2Etyrec]
      | S2Eunion (stamp, _(*ind*), ls2es) => let
          val s2i = s2exp_int (~1)
        in
          flag := flag + 1;
          s2exp_union_srt (s2e0.s2exp_srt, stamp, s2i, ls2es)
        end
      | _ => let
          val cond =
            if knd > 0 then (*typization*)
              if s2exp_is_linear s2e then false else true
            else false
        in
          if cond then begin
            flag := flag + 1; s2exp_whnf_flag (s2e, flag)
          end else begin
            if flag > flag0 then
              s2exp_top_srt (s2e0.s2exp_srt, knd, s2e)
            else s2e0
          end
        end // end of [_]
    end // end of [S2Etop]
  | S2Evar s2v => begin case+ the_s2varbindmap_find s2v of
    | ~Some_vt s2e => (flag := flag + 1; s2exp_whnf_flag (s2e, flag))
    | ~None_vt () => s2e0
    end // end of [S2Evar]
  | _ => s2e0
end // end of [s2exp_whnf_flag]

and s2explst_whnf_flag
  (s2es0: s2explst, flag: &int): s2explst = begin
  case+ s2es0 of
  | cons (s2e, s2es) => let
      val flag0 = flag
      val s2e = s2exp_whnf_flag (s2e, flag)
      val s2es = s2explst_whnf_flag (s2es, flag)
    in
      if flag > flag0 then cons (s2e, s2es) else s2es0
    end
  | nil () => nil ()
end // end of [s2explst_whnf_flag]

implement s2exp_whnf (s2e) = begin
  let var flag: int = 0 in s2exp_whnf_flag (s2e, flag) end
end // end of [s2exp_whnf]

implement s2explst_whnf (s2es) =
  $Lst.list_map_fun (s2es, s2exp_whnf)

(* ****** ****** *)

local

assume s2exp_whnf_t = s2exp

in

implement s2exp_of_s2exp_whnf (s2e) = s2e
implement s2exp_whnf_of_s2exp (s2e) = s2e

end // end of [local]

(* ****** ****** *)

fun s2eff_syneq (s2fe1: s2eff, s2fe2: s2eff): bool = begin
  case+ (s2fe1, s2fe2) of
  | (S2EFFall (), S2EFFall ()) => true
  | (S2EFFnil (), S2EFFnil ()) => true
  | (S2EFFset (efs1, s2es1), S2EFFset (efs2, s2es2)) => begin
      $Eff.eq_effset_effset (efs1, efs2) andalso s2explst_syneq (s2es1, s2es2)
    end
  | (_, _) => false
end // end of [s2eff]

fun labs2explst_syneq
  (ls2es1: labs2explst, ls2es2: labs2explst): bool = begin
  case+ (ls2es1, ls2es2) of
  | (LABS2EXPLSTcons (l1, s2e1, ls2es1),
     LABS2EXPLSTcons (l2, s2e2, ls2es2)) => begin
      l1 = l2 andalso
      s2exp_syneq (s2e1, s2e2) andalso
      labs2explst_syneq (ls2es1, ls2es2)
    end
  | (LABS2EXPLSTnil (), LABS2EXPLSTnil ()) => true
  | (_, _) => false
end // end of [labs2explst_syneq]

implement s2exp_syneq (s2e10, s2e20) = let
  val s2e10 = s2exp_whnf s2e10 and s2e20 = s2exp_whnf s2e20
(*
  val () = begin
    print "s2exp_syneq: s2e10 = "; print s2e10; print_newline ();
    print "s2exp_syneq: s2e20 = "; print s2e20; print_newline ();
  end
*)
in
  case+ s2e10.s2exp_node of
  | _ when eqref_s2exp_s2exp (s2e10, s2e20) => true
  | S2Eapp (s2e11, s2es12) => begin case+ s2e20.s2exp_node of
    | S2Eapp (s2e21, s2es22) => begin
        s2exp_syneq (s2e11, s2e21) andalso s2explst_syneq (s2es12, s2es22)
      end
    | _ => false
    end
  | S2Echar c1 => begin case+ s2e20.s2exp_node of
    | S2Echar c2 => eq_char_char (c1, c2) | _ => false
    end
  | S2Eclo (knd1, s2e1) => begin case+ s2e20.s2exp_node of
    | S2Eclo (knd2, s2e2) => knd1 = knd2 andalso s2exp_syneq (s2e1, s2e2)
    | _ => false
    end
  | S2Ecst s2c1 => begin case+ s2e20.s2exp_node of
    | S2Ecst s2c2 => eq_s2cst_s2cst (s2c1, s2c2) | _ => false
    end
  | S2Edatconptr (d2c1, s2es1) => begin case+ s2e20.s2exp_node of
    | S2Edatconptr (d2c2, s2es2) => begin
        eq_d2con_d2con (d2c1, d2c2) andalso s2explst_syneq (s2es1, s2es2)
      end
    | _ => false
    end
  | S2Edatcontyp (d2c1, s2es1) => begin case+ s2e20.s2exp_node of
    | S2Edatcontyp (d2c2, s2es2) => begin
        eq_d2con_d2con (d2c1, d2c2) andalso s2explst_syneq (s2es1, s2es2)
      end
    | _ => false
    end
  | S2Eeff s2fe1 => begin case+ s2e20.s2exp_node of
      | S2Eeff s2fe2 => s2eff_syneq (s2fe1, s2fe2) | _ => false
    end
  | S2Eeqeq (s2e11, s2e12) => begin case+ s2e20.s2exp_node of
    | S2Eeqeq (s2e21, s2e22) => begin
        s2exp_syneq (s2e11, s2e21) andalso s2exp_syneq (s2e12, s2e22)
      end
    | _ => false
    end
  | S2Eextype name1 => begin case+ s2e20.s2exp_node of
    | S2Eextype name2 => eq_string_string (name1, name2) | _ => false
    end
  | S2Efun (fc1, lin1, s2fe1, npf1, s2es1_arg, s2e1_res) => begin
      case+ s2e20.s2exp_node of
      | S2Efun (fc2, lin2, s2fe2, npf2, s2es2_arg, s2e2_res) => begin
          $Syn.eq_funclo_funclo (fc1, fc2) andalso
          lin1 = lin2 andalso
          s2eff_syneq (s2fe1, s2fe2) andalso
          npf1 = npf2 andalso
          s2explst_syneq (s2es1_arg, s2es2_arg) andalso
          s2exp_syneq (s2e1_res, s2e2_res)
        end
      | _ => false
    end
  | S2Eint i1 => begin case+ s2e20.s2exp_node of
    | S2Eint i2 => eq_int_int (i1, i2)
    | S2Eintinf i2 => $IntInf.eq_int_intinf (i1, i2)
    | _ => false
    end
  | S2Eintinf i1 => begin case+ s2e20.s2exp_node of
    | S2Eint i2 => $IntInf.eq_intinf_int (i1, i2)
    | S2Eintinf i2 => $IntInf.eq_intinf_intinf (i1, i2)
    | _ => false
    end
  | S2Eout s2e1 => begin case+ s2e20.s2exp_node of
    | S2Eout s2e2 => s2exp_syneq (s2e1, s2e2) | _ => false
    end
  | S2Eproj (s2e1(*root*), s2l1) => begin case+ s2e20.s2exp_node of
    | S2Eproj (s2e2(*root*), s2l2) => begin
        s2exp_syneq (s2e1, s2e2) andalso s2lab_syneq (s2l1, s2l2)
      end
    | _ => false
    end
  | S2Eread (_v1, s2e1) => begin
    case+ s2e20.s2exp_node of
    | S2Eread (_v2, s2e2) => begin
        s2exp_syneq (_v1, _v2) andalso s2exp_syneq (s2e1, s2e2)
      end
    | _ => false
    end // end of [S2Eread]
  | S2Erefarg (refval1, s2e1) => begin
    case+ s2e20.s2exp_node of
    | S2Erefarg (refval2, s2e2) => begin
        refval1 = refval2 andalso s2exp_syneq (s2e1, s2e2)
      end
    | _ => false
    end // end of [S2Erefarg]
  | S2Esel (s2e1, i1) => begin case+ s2e20.s2exp_node of
    | S2Esel (s2e2, i2) => (i1 = i2 andalso s2exp_syneq (s2e1, s2e2))
    | _ => false
    end
  | S2Esize s2ze1 => begin case+ s2e20.s2exp_node of 
    | S2Esize s2ze2 => s2zexp_syneq (s2ze1, s2ze2) | _ => false
    end
  | S2Esizeof s2e1 => begin case+ s2e20.s2exp_node of
    | S2Esizeof s2e2 => s2exp_syneq (s2e1, s2e2) | _ => false
    end
  | S2Etop (knd1, s2e1) => begin case+ s2e20.s2exp_node of
    | S2Etop (knd2, s2e2) => (knd1 = knd2 andalso s2exp_syneq (s2e1, s2e2))
    | _ => false
    end
  | S2Etup s2es1 => begin case+ s2e20.s2exp_node of
    | S2Etup s2es2 => s2explst_syneq (s2es1, s2es2) | _ => false
    end
  | S2Etylst s2es1 => begin case+ s2e20.s2exp_node of
    | S2Etylst s2es2 => s2explst_syneq (s2es1, s2es2) | _ => false
    end
  | S2Etyarr (s2e1_elt, s2ess1_ind) => begin case+ s2e20.s2exp_node of
    | S2Etyarr (s2e2_elt, s2ess2_ind) => begin
        s2exp_syneq (s2e1_elt, s2e2_elt) andalso
        s2explstlst_syneq (s2ess1_ind, s2ess2_ind)
      end
    | _ => false
    end
  | S2Etyleq (_(*knd*), s2e11, s2e12) => begin
    case+ s2e20.s2exp_node of
    | S2Etyleq (_(*knd*), s2e21, s2e22) => begin
        s2exp_syneq (s2e11, s2e21) andalso s2exp_syneq (s2e12, s2e22)
      end
    | _ => false
    end
  | S2Etyrec (knd1, npf1, ls2es1) => begin case+ s2e20.s2exp_node of
    | S2Etyrec (knd2, npf2, ls2es2) => begin
        knd1 = knd2 andalso npf1 = npf2 andalso
        labs2explst_syneq (ls2es1, ls2es2)
      end
    | _ => false
    end
  | S2Eunion (s1, s2e1_ind, ls2es1_body) => begin case+ s2e20.s2exp_node of
    | S2Eunion (s2, s2e2_ind, ls2es2_body) => begin
        $Stamp.eq_stamp_stamp (s1, s2) andalso
        s2exp_syneq (s2e1_ind, s2e2_ind) andalso
        labs2explst_syneq (ls2es1_body, ls2es2_body)
      end
    | _ => false
    end
  | S2Evar s2v1 => begin case+ s2e20.s2exp_node of
    | S2Evar s2v2 => eq_s2var_s2var (s2v1, s2v2) | _ => false
    end
  | S2EVar s2V1 => begin case+ s2e20.s2exp_node of
    | S2EVar s2V2 => eq_s2Var_s2Var (s2V1, s2V2) | _ => false
    end
  | _ => false (* test accuracy may be increased ... *)
end // end of [s2exp_syneq]

implement s2explst_syneq (s2es10, s2es20) = begin
  case+ (s2es10, s2es20) of
  | (s2e1 :: s2es1, s2e2 :: s2es2) => begin
      s2exp_syneq (s2e1, s2e2) andalso s2explst_syneq (s2es1, s2es2)
    end
  | (nil (), nil ()) => true
  | (_, _) => false
end // end of [s2explst_syneq]

implement s2explstlst_syneq (s2ess10, s2ess20) = begin
  case+ (s2ess10, s2ess20) of
  | (s2es1 :: s2ess1, s2es2 :: s2ess2) => begin
      s2explst_syneq (s2es1, s2es2) andalso s2explstlst_syneq (s2ess1, s2ess2)
    end
  | (nil (), nil ()) => true
  | (_, _) => false
end // end of [s2explstlst_syneq]

(* ****** ****** *)

fun labs2zexplst_syneq
  (ls2zes1: labs2zexplst, ls2zes2: labs2zexplst): bool = begin
  case+ (ls2zes1, ls2zes2) of
  | (LABS2ZEXPLSTcons (l1, s2ze1, ls2zes1),
     LABS2ZEXPLSTcons (l2, s2ze2, ls2zes2)) => begin
      l1 = l2 andalso
      s2zexp_syneq (s2ze1, s2ze2) andalso
      labs2zexplst_syneq (ls2zes1, ls2zes2)
    end
  | (LABS2ZEXPLSTnil (), LABS2ZEXPLSTnil ()) => true
  | (_, _) => false
end // end of [labs2zexplst_syneq]

fun s2zexplst_syneq (s2zes1: s2zexplst, s2zes2: s2zexplst): bool =
  case+ (s2zes1, s2zes2) of
  | (cons (s2ze1, s2zes1), cons (s2ze2, s2zes2)) => begin
      if s2zexp_syneq (s2ze1, s2ze2) then s2zexplst_syneq (s2zes1, s2zes2)
      else false
    end
  | (nil (), nil ()) => true
  | (_, _) => false

implement s2zexp_syneq (s2ze10, s2ze20) = begin
  case+ s2ze10 of
  | S2ZEapp (s2ze1, s2zes1) => begin case+ s2ze20 of
    | S2ZEapp (s2ze2, s2zes2) => begin
        if s2zexp_syneq (s2ze1, s2ze2) then s2zexplst_syneq (s2zes1, s2zes2)
        else false
      end
    | _ => false
    end
  | S2ZEbot () => false // [S2ZEbot <> S2ZEbot]!
  | S2ZEbyte i1 => begin case+ s2ze20 of
    | S2ZEbyte i2 => (i1 = i2) | _ => false
    end
  | S2ZEcst s2c1 => begin case+ s2ze20 of
    | S2ZEcst s2c2 => eq_s2cst_s2cst (s2c1, s2c2) | _ => false
    end
  | S2ZEextype name1 => begin case+ s2ze20 of
    | S2ZEextype name2 => (name1 = name2) | _ => false
    end
  | S2ZEtyarr (s2ze1_elt, s2ess1_dim) => begin case+ s2ze20 of
    | S2ZEtyarr (s2ze2_elt, s2ess2_dim) => begin
        s2zexp_syneq (s2ze1_elt, s2ze2_elt) andalso
        s2explstlst_syneq (s2ess1_dim, s2ess2_dim)
      end
    | _ => false
    end
  | S2ZEtyrec (knd1, ls2zes1) => begin case+ s2ze20 of
    | S2ZEtyrec (knd2, ls2zes2) => begin
        knd1 = knd2 andalso labs2zexplst_syneq (ls2zes1, ls2zes2)
      end
    | _ => false
    end
  | S2ZEunion (s1, ls2zes1) => begin case+ s2ze20 of
    | S2ZEunion (s2, ls2zes2) => begin
        $Stamp.eq_stamp_stamp (s1, s2) andalso
        labs2zexplst_syneq (ls2zes1, ls2zes2)
      end
    | _ => false
    end
  | S2ZEvar s2v1 => begin case+ s2ze20 of
    | S2ZEvar s2v2 => eq_s2var_s2var (s2v1, s2v2) | _ => false
    end
  | S2ZEword i1 => begin case+ s2ze20 of
    | S2ZEword i2 => (i1 = i2) | _ => false
    end
end // end of [s2zexp_syneq]

(* ****** ****** *)

implement s2exp_projlst (s2e, s2ls) = begin case+ s2ls of
  | cons (s2l, s2ls) => s2exp_projlst (s2exp_proj (s2e, s2l), s2ls)
  | nil () => s2e
end // end of [s2exp_projlst]

implement s2exp_addr_normalize (s2e0) = let
  fun aux (s2e0: s2exp, s2ls: s2lablst): @(s2exp, s2lablst) =
    case+ s2e0.s2exp_node of
    | S2Eproj (s2e0, s2l) => aux (s2e0, cons (s2l, s2ls))
    | _ => (s2e0, s2ls)
in
  aux (s2e0, nil ())
end // end of [s2exp_addr_normalize]

(* ****** ****** *)

fun s2exp_prenexing
  (isexi: bool, s2e0: s2exp,
   s2vs_r: &s2varlst, s2ps_r: &s2explst, flag: &int): s2exp = let
  val s2e0 = s2exp_whnf s2e0 in case+ s2e0.s2exp_node of
    | S2Eapp (s2e_fun, s2es_arg) => begin
        if s2cstref_exp_equ (At_viewt0ype_addr_view, s2e_fun) then
          case+ s2es_arg of
          | cons (s2e_at, cons (s2l, nil ())) => let
              val flag0 = flag
              val s2e_at = s2exp_prenexing (isexi, s2e_at, s2vs_r, s2ps_r, flag)
            in
              if flag > flag0 then s2exp_app_srt
                (s2rt_view, s2e_fun, cons (s2e_at, cons(s2l, nil ())))
              else s2e0
            end
          | _ => begin
              prerr "Internal Error: s2exp_prenexing: At_viewt0ype_addr_view";
              prerr_newline ();
              $Err.abort {s2exp} ()
            end
        else s2e0
      end
    | S2Eexi (s2vs, s2ps, s2e_body) => begin
        if isexi then
          s2exp_prenexing_main (isexi, s2vs, s2ps, s2e_body, s2vs_r, s2ps_r, flag)
        else s2e0
      end
    | S2Etyrec (knd, npf, ls2es) => let
        val flag0 = flag
        val ls2es = labs2explst_prenexing (isexi, ls2es, s2vs_r, s2ps_r, flag)
      in
        if flag > flag0 then
          s2exp_tyrec_srt (s2e0.s2exp_srt, knd, npf, ls2es)
        else s2e0
      end
    | S2Euni (s2vs, s2ps, s2e_body) => begin
        if isexi then s2e0 else
          s2exp_prenexing_main (isexi, s2vs, s2ps, s2e_body, s2vs_r, s2ps_r, flag)
      end
    | _ => s2e0
end // end of [s2exp_prenexing]

and s2exp_prenexing_main
  (isexi: bool, s2vs: s2varlst, s2ps: s2explst, s2e_body: s2exp,
   s2vs_r: &s2varlst, s2ps_r: &s2explst, flag: &int): s2exp = let
  val @(sub, s2vs) = stasub_extend_svarlst (stasub_nil, s2vs)
  val s2ps = s2explst_subst (sub, s2ps)
  val s2e_body = s2exp_subst (sub, s2e_body)
  val () = flag := flag + 1
  val () = s2vs_r := $Lst.list_revapp (s2vs, s2vs_r);
  val () = s2ps_r := $Lst.list_revapp (s2ps, s2ps_r);
  val s2e_body = s2exp_prenexing (isexi, s2e_body, s2vs_r, s2ps_r, flag)
in
  s2e_body
end

and s2explst_prenexing
  (isexi: bool, s2es0: s2explst,
   s2vs_r: &s2varlst, s2ps_r: &s2explst, flag: &int): s2explst = begin
  case+ s2es0 of
  | cons (s2e, s2es) => let
      val flag0 = flag
      val s2e = s2exp_prenexing (isexi, s2e, s2vs_r, s2ps_r, flag)
      val s2es = s2explst_prenexing (isexi, s2es, s2vs_r, s2ps_r, flag)
    in
      if flag > flag0 then cons (s2e, s2es) else s2es0
    end
  | nil () => nil ()
end // end of [s2explst_prenexing]

and labs2explst_prenexing
  (isexi: bool, ls2es0: labs2explst,
   s2vs_r: &s2varlst, s2ps_r: &s2explst, flag: &int): labs2explst = begin
  case+ ls2es0 of
  | LABS2EXPLSTcons (l, s2e, ls2es) => let
      val flag0 = flag
      val s2e = s2exp_prenexing (isexi, s2e, s2vs_r, s2ps_r, flag)
      val ls2es = labs2explst_prenexing (isexi, ls2es, s2vs_r, s2ps_r, flag)
    in
      if flag > flag0 then LABS2EXPLSTcons (l, s2e, ls2es) else ls2es0
    end
  | LABS2EXPLSTnil () => LABS2EXPLSTnil ()
end // end of [labs2explst_prenexing]

(* ****** ****** *)

implement s2exp_absuni (s2e) = let
  var flag: int = 0
  var s2vs_r: s2varlst = nil () and s2ps_r: s2explst = nil ()
  val s2e = s2exp_prenexing (false, s2e, s2vs_r, s2ps_r, flag)
in
  ($Lst.list_reverse s2vs_r, $Lst.list_reverse s2ps_r, s2e)
end

implement s2exp_opnexi (s2e) = let
  var flag: int = 0
  var s2vs_r: s2varlst = nil () and s2ps_r: s2explst = nil ()
  val s2e = s2exp_prenexing (true, s2e, s2vs_r, s2ps_r, flag)
in
  ($Lst.list_reverse s2vs_r, $Lst.list_reverse s2ps_r, s2e)
end

implement s2explst_opnexi (s2es) = let
  var flag: int = 0
  var s2vs_r: s2varlst = nil () and s2ps_r: s2explst = nil ()
  val s2es = s2explst_prenexing (true, s2es, s2vs_r, s2ps_r, flag)
in
  ($Lst.list_reverse s2vs_r, $Lst.list_reverse s2ps_r, s2es)
end


(* ****** ****** *)

implement labs2explst_lab_get (ls2es, l0) = let
  fun aux_get (ls2es: labs2explst):<cloptr1> s2expopt_vt =
    case+ ls2es of
    | LABS2EXPLSTcons (l, s2e, ls2es) => begin
        if l0 = l then Some_vt s2e else aux_get ls2es
      end
    | LABS2EXPLSTnil () => None_vt ()
in
  aux_get ls2es
end // end of [labs2explst_lab_get]

implement labs2explst_lab_set (ls2es, l0, s2e0) = let
  fun aux_set (ls2es: labs2explst, s2e0: s2exp)
    :<cloptr1> Option_vt labs2explst = begin
    case+ ls2es of
    | LABS2EXPLSTcons (l, s2e, ls2es) => begin
        if l0 = l then begin
          Some_vt (LABS2EXPLSTcons (l0, s2e0, ls2es))
        end else begin case+ aux_set (ls2es, s2e0) of
        | ~Some_vt ls2es => Some_vt (LABS2EXPLSTcons (l, s2e, ls2es))
        | ~None_vt () => None_vt ()
        end
      end
    | LABS2EXPLSTnil () => None_vt ()
  end // end of [aux_set]
in
  aux_set (ls2es, s2e0)
end // end of [labs2explst_lab_set]

(* ****** ****** *)

implement labs2zexplst_lab_get (ls2zes, l0) = let
  fun aux_get (ls2zes: labs2zexplst):<cloptr1> s2zexpopt_vt =
    case+ ls2zes of
    | LABS2ZEXPLSTcons (l, s2ze, ls2zes) => begin
        if l0 = l then Some_vt s2ze else aux_get ls2zes
      end
    | LABS2ZEXPLSTnil () => None_vt ()
in
  aux_get ls2zes
end // end of [labs2zexplst_lab_get]

(* ****** ****** *)

fn labs2explst_lab_get_ind
  (ls2es: labs2explst, l0: lab_t, i: &(int?) >> int)
  : s2expopt_vt = let
  fun aux_get
    (i: &int, ls2es: labs2explst):<cloptr1> s2expopt_vt = begin
    case+ ls2es of
    | LABS2EXPLSTcons (l, s2e, ls2es) => begin
        if l0 = l then Some_vt s2e else (i := i+1; aux_get (i, ls2es))
      end
    | LABS2EXPLSTnil () => (i := ~1; None_vt ())
  end // end of [aux_get]
  val () = (i := 0)
  val os2e = aux_get (i, ls2es)
in
  os2e
end // end of [labs2explst_lab_get_ind]

fn labs2explst_lab_set_ind
  (ls2es: labs2explst, l0: lab_t, s2e0: s2exp, i: &(int?) >> int)
  : Option_vt (labs2explst) = let
  fun aux_set (ls2es: labs2explst, s2e0: s2exp, i: &int)
    :<cloptr1> Option_vt labs2explst = begin case+ ls2es of
    | LABS2EXPLSTcons (l, s2e, ls2es) => begin
        if l0 = l then begin
          Some_vt (LABS2EXPLSTcons (l0, s2e0, ls2es))
        end else begin case+ aux_set (ls2es, s2e0, i) of
        | ~Some_vt ls2es => begin
            i := i + 1; Some_vt (LABS2EXPLSTcons (l, s2e, ls2es))
          end
        | ~None_vt () => None_vt ()
        end
      end
    | LABS2EXPLSTnil () => (i := ~1; None_vt ())
  end // end of [aux_set]
  val () = (i := 0)
in
  aux_set (ls2es, s2e0, i)
end // end of [labs2explst_lab_set_ind]

(* ****** ****** *)

implement s2exp_lab_get_restlin_cstr
  (loc0, s2e0, l0, restlin, cstr) = let
  val s2e0 = s2exp_whnf s2e0
  fn err1 (loc0: loc_t, s2e0: s2exp, l0: lab_t): s2exp = begin
    prerr loc0;
    prerr ": error(3)";
    prerr ": the label [";
    prerr l0;
    prerr "] is not found in [";
    prerr s2e0;
    prerr "].";
    prerr_newline ();
    $Err.abort {s2exp} ()
  end // end of [err]
  fn err2 {a:viewt@ype} (loc0: loc_t, s2e0: s2exp): a = begin
    prerr loc0;
    prerr ": error(3)";
    prerr ": the type [";
    prerr s2e0;
    prerr "] is expected to be a record or union but it is not.";
    prerr_newline ();
    $Err.abort ()
  end // end of [err2]
  fun aux_restlin
    (i: int, ls2es: labs2explst, restlin: &int): void =
    case+ ls2es of
    | LABS2EXPLSTcons (l, s2e, ls2es) => let
        val () =
          if i <> 0 then begin
            (if s2exp_is_linear s2e then restlin := restlin + 1)
          end
      in
        aux_restlin (i-1, ls2es, restlin)
      end
    | LABS2EXPLSTnil () => ()
in
  case+ s2e0.s2exp_node of
  | S2Etyrec (_(*knd*), _(*npf*), ls2es) => let
      var i: int (* uninitialized *)
      val os2e = labs2explst_lab_get_ind (ls2es, l0, i)
    in
      case+ os2e of
      | ~Some_vt s2e => (aux_restlin (i, ls2es, restlin); s2e)
      | ~None_vt () => err1 (loc0, s2e0, l0)
    end
  | S2Eunion (_(*stamp*), s2i, ls2es) => let
      var i: int (* uninitialized *)
      val os2e = labs2explst_lab_get_ind (ls2es, l0, i)
      val s2e = case+ os2e of
        | ~Some_vt s2e => s2e | ~None_vt () => err1 (loc0, s2e0, l0)
      val () = aux_restlin (i, ls2es, restlin)
      val s2p = s2exp_eqeq (s2i, s2exp_int i)
    in
      cstr := cons (s2p, cstr); s2e
    end
  | _ => err2 {s2exp} (loc0, s2e0)
end // end of [s2exp_lab_get_restlin_cstr]

(* ****** ****** *)

implement s2exp_slablst_get_restlin_cstr
  (loc0, s2e0, s2ls, restlin, cstr) = let
  fun aux {i,j:nat} .<i>. (
      s2e0: s2exp
    , s2ls: list (s2lab, i)
    , restlin: &int
    , cstr: &s2explst
    , s2ls_vt: list_vt (s2lab, j)
    ) :<cloptr1> @(s2exp, list (s2lab, i+j)) = begin
    case+ s2ls of
    | cons (s2l, s2ls) => let
        val l = case+ s2l of
          | S2LAB0lab l => l
          | S2LAB1lab (l, _) => l // should not happen!
          | _ => begin
              prerr loc0;
              prerr ": error(3)";
              prerr ": the use of array subscription is not supported.";
              prerr_newline ();
              $Err.abort {lab_t} ()
            end
        val s2e0_prj = begin
          s2exp_lab_get_restlin_cstr (loc0, s2e0, l, restlin, cstr)
        end
        val s2ls_vt = list_vt_cons (S2LAB1lab (l, s2e0), s2ls_vt)
      in
        aux (s2e0_prj, s2ls, restlin, cstr, s2ls_vt)
      end
    | nil () => @(s2e0, $Lst.list_vt_reverse_list s2ls_vt)
    end // end of [aux]
in
  aux (s2e0, s2ls, restlin, cstr, list_vt_nil ())
end // end of [s2exp_slablst_get_restlin_cstr]

(* ****** ****** *)

fn s2exp_is_int_0 (s2e: s2exp): bool =
  case+ s2e.s2exp_node of S2Eint i => i = 0 | _ => false

fn un_s2exp_int (s2e: s2exp): Option_vt int =
  case+ s2e.s2exp_node of S2Eint i => Some_vt i | _ => None_vt ()

fun labs2explst_nth_get (ls2es: labs2explst, i: int): @(lab_t, s2exp) =
  case+ ls2es of
  | LABS2EXPLSTcons (l, s2e, ls2es) => begin
      if i > 0 then labs2explst_nth_get (ls2es, i-1) else @(l, s2e)
    end
  | LABS2EXPLSTnil () => begin
      prerr "labs2explst_nth_get: Internal Error: i = "; prerr i;
      prerr_newline ();
      $Err.abort ()
    end

fun labs2explst_nth_set
  (ls2es: labs2explst, i: int, s2e0: s2exp): labs2explst = begin
  case+ ls2es of
  | LABS2EXPLSTcons (l, s2e, ls2es) => begin
      if i > 0 then begin
        LABS2EXPLSTcons (l, s2e, labs2explst_nth_set (ls2es, i-1, s2e0))
      end else begin
        LABS2EXPLSTcons (l, s2e0, ls2es)
      end
    end
  | LABS2EXPLSTnil () => begin
      prerr "labs2explst_nth_set: Internal Error: i = "; prerr i;
      prerr_newline ();
      $Err.abort ()
    end
end // end of [labs2explst_nth_set]

fn array_ind_dim_check_err (
    s2ess_ind: s2explstlst, s2ess_dim: s2explstlst, cstr: &s2explst, err: &int
  ) : void = let
  fun auxlst
    (s2es_ind: s2explst, s2es_dim: s2explst, cstr: &s2explst, err: &int)
    : void =
    if err > 0 then () else begin case+ (s2es_ind, s2es_dim) of
    | (nil _, nil _) => ()
    | (cons (s2e_ind, s2es_ind), cons (s2e_dim, s2es_dim)) => let
        val s2p = s2exp_btw_int_int_int_bool (s2exp_int_0, s2e_ind, s2e_dim)
        val () = cstr := cons (s2p, cstr)
      in
        auxlst (s2es_ind, s2es_dim, cstr, err)
      end
    | (_, _) => (err := 1)
    end // end of [if]
  fun auxlstlst
    (s2ess_ind: s2explstlst, s2ess_dim: s2explstlst, cstr: &s2explst, err: &int)
    : void =
    if err > 0 then () else begin case+ (s2ess_ind, s2ess_dim) of
    | (nil _, nil _) => ()
    | (cons (s2es_ind, s2ess_ind), cons (s2es_dim, s2ess_dim)) => let
        val () = auxlst (s2es_ind, s2es_dim, cstr, err)
      in
        auxlstlst (s2ess_ind, s2ess_dim, cstr, err)
      end
    | (_, _) => (err := 1)
    end // end of [if]
in
  auxlstlst (s2ess_ind, s2ess_dim, cstr, err)
end // end of [s2explstlst_btw_int_int_int_bool_err]

(* ****** ****** *)

// [s2e0] is assumed to have been normalized
implement s2exp_lab_linget_cstr (loc0, s2e0, l0, cstr) = let
  fn label_not_found_errmsg ():<cloptr1> s2exp = begin
    prerr loc0;
    prerr ": error(3)";
    prerr ": the lable [";
    prerr l0;
    prerr "] is not found in [";
    prerr s2e0;
    prerr_newline ();
    $Err.abort {s2exp} ()
  end
in
  case+ s2e0.s2exp_node of
  | S2Etyrec (knd, npf, ls2es) => begin
    case+ labs2explst_lab_get (ls2es, l0) of
      | ~Some_vt s2e => s2e | ~None_vt () => label_not_found_errmsg ()
    end // end of [S2Etyrec]
  | S2Eunion (stamp, s2i, ls2es) => let
      var i: int; var isint: int = 0
      val s2e =
        case+ labs2explst_lab_get_ind (ls2es, l0, i) of
        | ~Some_vt s2e => begin case+ un_s2exp_int s2i of
          | ~Some_vt i0 when i0 = i => (isint := 1; s2e)
          | ~Some_vt i0 when i0 >= 0 => let
              val ls2e = labs2explst_nth_get (ls2es, i0)
              val () = // linearity checking
                if s2exp_is_linear (ls2e.1) then begin
                  prerr loc0;
                  prerr ": error(3)";
                  prerr ": the linear union component with the label [";
                  prerr l0;
                  prerr "] is abandoned";
                  prerr_newline ();
                  $Err.abort {void} ()
                end
            in
              isint := 1; s2exp_topize (0(*knd*), s2e)
            end
          | ~Some_vt i0 (* i0 = ~1 *) => begin
              isint := 1; s2exp_topize (0(*knd*), s2e)
            end
          | ~None_vt () => s2e
          end // end of [Some_vt]
        | ~None_vt () => label_not_found_errmsg ()
      val () =
        if isint = 0 then let
          val s2i_new = s2exp_int i; val s2p = s2exp_eqeq (s2i, s2i_new)
        in
          cstr := cons (s2p, cstr)
        end
    in
      s2e
    end // end of [S2Eunion]
  | _ => begin
      prerr loc0;
      prerr ": error(3)";
      prerr ": the type [";
      prerr s2e0;
      prerr "] is expected to be a record or union but it is not.";
      prerr_newline ();
      $Err.abort ()
    end
end // end of [s2exp_lab_linget_cstr]

// [s2e0] is assumed to have been normalized
implement s2exp_ind_linget_cstr (loc0, s2e0, s2ess_ind, cstr) = begin
  case+ s2e0.s2exp_node of
  | S2Etyarr (s2e_elt, s2ess_dim) => let
      var err: int = 0
      val () = array_ind_dim_check_err (s2ess_ind, s2ess_dim, cstr, err)
      val () =
        if err > 0 then begin
          prerr loc0;
          prerr ": error(3)";
          prerr ": array index/dimension mismatch.";
          prerr_newline ();
          $Err.abort {void} ()
        end
    in
      s2e_elt
    end
  | _ => begin
      prerr loc0;
      prerr ": error(3)";
      prerr ": the type [";
      prerr s2e0;
      prerr "] is expected to be an array but it is not.";
      prerr_newline ();
      $Err.abort ()
    end
end // end of [s2exp_ind_linget_cstr]

(* ****** ****** *)

// [s2e0] is assumed to have been normalized
implement s2exp_slab_linget_cstr (loc0, s2e0, s2l, cstr) = begin
  case+ s2l of
  | S2LAB0lab l => let
      val s2e_prj = s2exp_lab_linget_cstr (loc0, s2e0, l, cstr)
    in
      @(s2e_prj, S2LAB1lab (l, s2e0))
    end
  | S2LAB0ind (s2ess_ind) => let
      val s2e_elt = s2exp_ind_linget_cstr (loc0, s2e0, s2ess_ind, cstr)
    in
      @(s2e_elt, S2LAB1ind (s2ess_ind, s2e_elt))
    end
  | _ => begin
      prerr loc0;
      prerr ": Internal Error: s2exp_select_slab_get: S2LAB1lab or S2LAB1ind";
      prerr_newline ();
      $Err.abort ()
    end
end // end of [s2exp_slab_linget_cstr]

(* ****** ****** *)

// [s2e0] is assumed to have been normalized
implement s2exp_lab_linset (loc0, s2e0, l0, s2e_new) = let
  fn err {a:type} (loc0: loc_t): a = begin
    prerr loc0;
    prerr ": Internal Error: s2exp_slab_set";
    prerr_newline ();
    $Err.abort {a} ()
  end // end of [err]
in
  case+ s2e0.s2exp_node of
  | S2Etyrec (knd, npf, ls2es) => begin       
    case+ labs2explst_lab_set (ls2es, l0, s2e_new) of
    | ~Some_vt ls2es => begin case+ knd of
      | TYRECKINDflt0 () => s2exp_tyrec (0, npf, ls2es)
      | _ => begin
          s2exp_tyrec_srt (s2e0.s2exp_srt, knd, npf, ls2es)
        end
      end // end of [Some_vt]
    | ~None_vt () => err (loc0)
    end // end of [S2Etyrec]
  | S2Eunion (stamp, s2i, ls2es) => let
      val s2t0 = s2e0.s2exp_srt
      val isbox = s2rt_is_boxed s2t0
      val istop = begin
        case+ s2e_new.s2exp_node of | S2Etop _ => true | _ => false
      end
    in
      if istop then let
        val s2i = s2exp_int (~1)
        val s2t0: s2rt = if isbox then s2rt_type else s2rt_t0ype
      in
        s2exp_union_srt (s2t0, stamp, s2i, ls2es)
      end else let
        var i: int
        val ls2es: labs2explst = case+ 
          labs2explst_lab_set_ind (ls2es, l0, s2e_new, i) of
          | ~Some_vt ls2es => ls2es | ~None_vt () => err (loc0)
        val s2i = case+ un_s2exp_int s2i of
          | ~Some_vt _ => s2i | ~None_vt () => s2exp_int i
        val s2t0: s2rt =
          if s2exp_is_linear s2e_new then begin
            if isbox then s2rt_viewtype else s2rt_viewt0ype
          end else begin
            if isbox then s2rt_type else s2rt_t0ype
          end
      in
        s2exp_union_srt (s2t0, stamp, s2i, ls2es)
      end
    end
  | _ => err (loc0)
end // end of [s2exp_lab_linset]

(* ****** ****** *)

implement s2exp_slablst_lintry_cstr (loc0, s2e0, s2ls, cstr) = let
  fun aux {n:nat}
    (loc0: loc_t, s2e0: s2exp, s2ls: list (s2lab, n), cstr: &s2explst)
    : list (s2lab, n) = begin case+ s2ls of
    | cons (s2l, s2ls) => let
        val s2e0 = s2exp_whnf s2e0
        val (s2e1, s2l) = s2exp_slab_linget_cstr (loc0, s2e0, s2l, cstr)
        val s2ls = aux (loc0, s2e1, s2ls, cstr)
      in
        cons (s2l, s2ls)
      end
    | nil () => nil ()
  end // end of [aux]
in
  aux (loc0, s2e0, s2ls, cstr)
end // end of [s2exp_slablst_lintry_cstr]

implement s2exp_slablst_linget_cstr (loc0, s2e0, s2ls, cstr) = let
  fun aux {n:nat}
    (loc0: loc_t, s2e0: s2exp, s2ls: list (s2lab, n), cstr: &s2explst)
    : (s2exp, s2expopt_vt, list (s2lab, n)) = begin case+ s2ls of
    | cons (s2l, s2ls) => let
        val s2e0 = s2exp_whnf s2e0
        val (s2e1, s2l) = s2exp_slab_linget_cstr (loc0, s2e0, s2l, cstr)
        val (s2e_prj, os2e1, s2ls) = aux (loc0, s2e1, s2ls, cstr)
        val os2e0 = (
          case+ os2e1 of
          | ~Some_vt s2e1 => begin case+ s2l of
            | S2LAB1lab (l, _) => begin
                Some_vt (s2exp_lab_linset (loc0, s2e0, l, s2e1))
              end
            | S2LAB1ind (_, s2e_elt) => let
                val s2p = s2exp_tyleq (1(*knd*), s2e1, s2e_elt)
              in
                cstr := cons (s2p, cstr); None_vt ()
              end
            | _ => begin
                prerr loc0;
                prerr "Internal Error: s2exp_slablst_linget_cstr: aux";
                prerr_newline ();
                $Err.abort {s2expopt_vt} ()
              end
            end // end of [Some_vt]
          | ~None_vt () => None_vt ()
        ) : s2expopt_vt
      in
        (s2e_prj, os2e0, cons (s2l, s2ls))
      end
    | nil () => let
        val os2e0 = (
          if s2exp_is_linear s2e0 then Some_vt (s2exp_topize (1(*knd*), s2e0))
          else None_vt ()
        ) : s2expopt_vt
      in
        (s2e0, os2e0, nil ())
      end
  end // end of [aux]
  val (s2e_prj, os2e0, s2ls) = aux (loc0, s2e0, s2ls, cstr)
  val s2e0 = (
    case+ os2e0 of ~Some_vt s2e0 => s2e0 | ~None_vt () => s2e0
  ) : s2exp
in
  (s2e_prj, s2e0, s2ls)
end // end of [s2exp_slablst_linget_cstr]

implement s2exp_slablst_linset_cstr
  (loc0, s2e0, s2ls, s2e_new, cstr) = let
  fun aux {n:nat} (
      loc0: loc_t
    , s2e0: s2exp
    , s2ls: list (s2lab, n)
    , s2e_new: s2exp
    , cstr: &s2explst
    ) : @(s2exp, s2expopt_vt, list (s2lab, n)) = begin case+ s2ls of
    | cons (s2l, s2ls) => let
        val s2e0 = s2exp_whnf s2e0
        val (s2e1, s2l) = s2exp_slab_linget_cstr (loc0, s2e0, s2l, cstr)
        val (s2e_old, os2e1, s2ls) = aux (loc0, s2e1, s2ls, s2e_new, cstr)
        val os2e0 = (
          case+ os2e1 of
          | ~Some_vt s2e1 => begin case+ s2l of
            | S2LAB1lab (l, _) => Some_vt (s2exp_lab_linset (loc0, s2e0, l, s2e1))
            | S2LAB1ind (_, s2e_elt) => let
                val s2p = s2exp_tyleq (1(*knd*), s2e1, s2e_elt)
              in
                cstr := cons (s2p, cstr); None_vt ()
              end
            | _ => begin
                prerr loc0;
                prerr "Internal Error: s2exp_slablst_linset_cstr: aux";
                prerr_newline ();
                $Err.abort {s2expopt_vt} ()
              end
            end // end of [Some_vt]
          | ~None_vt () => None_vt ()
        ) : s2expopt_vt
      in
        (s2e_old, os2e0, cons (s2l, s2ls))
      end
    | nil () => (s2e0, Some_vt s2e_new, nil ())
  end // end of [aux]
  val (s2e_old, os2e0, s2ls) = aux (loc0, s2e0, s2ls, s2e_new, cstr)
  val s2e0 = (
    case+ os2e0 of ~Some_vt s2e0 => s2e0 | ~None_vt () => s2e0
  ) : s2exp
in
  (s2e_old, s2e0, s2ls)
end // end of [s2exp_slablst_linset_cstr]

implement s2exp_slablst_lindel_cstr (loc0, s2e0, s2ls, cstr) = let
  fun aux {n:nat} (
      loc0: loc_t
    , s2e0: s2exp
    , s2ls: list (s2lab, n)
    , cstr: &s2explst
    ) : @(s2exp(*part*), s2exp(*whole*), list (s2lab, n)) = begin
    case+ s2ls of
    | cons (s2l, s2ls) => let
        val () = case+ s2l of
          | S2LAB0ind _ => begin
              prerr loc0;
              prerr ": error(3)";
              prerr ": array subscription is not supported (for view extraction).";
              prerr_newline ();
              $Err.abort {void} ()
            end
          | _ => ()
        val s2e0 = s2exp_whnf s2e0
        val (s2e1, s2l) = s2exp_slab_linget_cstr (loc0, s2e0, s2l, cstr)
        val (s2e_out, s2e1, s2ls) = aux (loc0, s2e1, s2ls, cstr)
        val s2e0 = (
          case+ s2l of
          | S2LAB1lab (l, _) => s2exp_lab_linset (loc0, s2e0, l, s2e1)
          | _ => begin
              prerr loc0;
              prerr "Internal Error: s2exp_slablst_linset_cstr: aux";
              prerr_newline ();
              $Err.abort {s2exp} ()
            end
        ) : s2exp
      in
        (s2e_out, s2e0, cons (s2l, s2ls))
      end
    | nil () => (s2e0, s2exp_out s2e0, nil ())
  end // end of [aux]
in
  aux (loc0, s2e0, s2ls, cstr)
end // end of [s2exp_slablst_lindel_cstr]

(* ****** ****** *)

(* end of [ats_staexp2_util2.dats] *)
