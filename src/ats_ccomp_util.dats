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

// Time: April 2008
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

staload Map = "ats_map_lin.sats"

(* ****** ****** *)

staload "ats_hiexp.sats"
staload "ats_ccomp.sats"
staload "ats_ccomp_env.sats"

(* ****** ****** *)

staload _(*anonymous*)= "ats_map_lin.dats"

(* ****** ****** *)

viewtypedef tmpvarmap = $Map.map_vt (tmpvar_t, int)

fn tmpvarmap_add
  (m: &tmpvarmap, tmp: tmpvar_t): void = begin
  case+ $Map.map_search (m, tmp) of
  | ~Some_vt _ => () | ~None_vt () => $Map.map_insert (m, tmp, 0)
end // end of [tmpvarmap_add]

fn tmpvarmap_add_root
  (m: &tmpvarmap, tmp: tmpvar_t): void = let
  val tmp = (
    case+ tmpvar_root_get tmp of
    | TMPVAROPTsome tmp => tmp | TMPVAROPTnone () => tmp
  ) : tmpvar_t
in
  case+ $Map.map_search (m, tmp) of
  | ~Some_vt _ => () | ~None_vt () => $Map.map_insert (m, tmp, 0)
end // end of [tmpvarmap]

(* ****** ****** *)

local

assume tmpvarmap_vt = tmpvarmap

dataviewtype ENV (l:addr, i:addr) = ENVcon (l, i) of (ptr l, ptr i, int)

fn _emit_tmpvarmap_dec {m:file_mode} {l:addr} (
    pf_mod: file_mode_lte (m, w)
  , pf_fil: !FILE m @ l
  | l: ptr l, knd: int (*local/static*)
  , tmps: !tmpvarmap_vt
  ) : int = let
  var i: int = 0
  viewdef V = (FILE m @ l, int @ i)
  viewtypedef VT = ENV (l, i)
  fn f (pf: !V | tmp: tmpvar_t, _: int, env: !VT): void = let
    prval @(pf_fil, pf_int) = pf
    val+ ENVcon (p_l, p_i, knd)= env
    val () = (!p_i := !p_i + 1)
  in
    case+ 0 of
    | _ when tmpvar_is_void (tmp) => let
        val () = if knd = 0 then fprint (pf_mod | !p_l, "ATSlocal_void (")
        val () = if knd = 1 then fprint (pf_mod | !p_l, "ATSstatic_void (")
        val () = emit_tmpvar (pf_mod | !p_l, tmp)
        val () = fprint (pf_mod | !p_l, ") ;\n")
      in
        pf := @(pf_fil, pf_int); fold@ env
      end // end of [tmpvar_is_void]
    | _ => let
        val () = if knd = 0 then fprint (pf_mod | !p_l, "ATSlocal (")
        val () = if knd = 1 then fprint (pf_mod | !p_l, "ATSstatic (")
        val () = emit_hityp (pf_mod | !p_l, tmpvar_typ_get tmp)
        val () = fprint (pf_mod | !p_l, ", ")
        val () = emit_tmpvar (pf_mod | !p_l, tmp)
        val () = fprint (pf_mod | !p_l, ") ;\n")
      in
        pf := @(pf_fil, pf_int); fold@ env
      end // end of [_]
  end // end of [f]
  val env = ENVcon (l, &i, knd)
  prval pf = @(pf_fil, view@ i)
  val () = $Map.map_foreach_pre {V} {VT} (pf | tmps, f, env)
  prval () = (pf_fil := pf.0; view@ i := pf.1)
  val+ ~ENVcon (_, _, _) = env
in
  i // the number of tmpvars
end // end of [_emit_tmpvarmap_dec]

//

dataviewtype ENV (l:addr, i:addr) = ENVcon (l, i) of (ptr l, ptr i)

fn _emit_tmpvarmap_markroot {m:file_mode} {l:addr} (
    pf_mod: file_mode_lte (m, w), pf_fil: !FILE m @ l
  | l: ptr l, tmps: !tmpvarmap_vt
  ) : int = let
  var i: int = 0
  viewdef V = (FILE m @ l, int @ i)
  viewtypedef VT = ENV (l, i)
  fn f (pf: !V | tmp: tmpvar_t, _: int, env: !VT): void = let
    prval @(pf_fil, pf_int) = pf
    val+ ENVcon (p_l, p_i)= env
    val () = (!p_i := !p_i + 1)
    val () = case+ 0 of
      | _ when tmpvar_is_void (tmp) => () | _ => let
          val () = fprint (pf_mod | !p_l, "ATS_GC_MARKROOT(&")
          val () = emit_tmpvar (pf_mod | !p_l, tmp)
          val () = fprint (pf_mod | !p_l, ", sizeof(")
          val () = emit_hityp (pf_mod | !p_l, tmpvar_typ_get tmp)
          val () = fprint (pf_mod | !p_l, ")) ;\n")
        in
          // empty
        end // end of [_]
  in
    pf := @(pf_fil, pf_int); fold@ env
  end // end of [f]
  val env = ENVcon (l, &i)
  prval pf = @(pf_fil, view@ i)
  val () = $Map.map_foreach_pre {V} {VT} (pf | tmps, f, env)
  prval () = (pf_fil := pf.0; view@ i := pf.1)
  val+ ~ENVcon (_, _) = env
in
  i // the number of tmpvars
end // end of [_emit_tmpvarmap_markroot]

in // in of [local]

implement tmpvarmap_nil () =
  $Map.map_make {tmpvar_t,int} (compare_tmpvar_tmpvar)

implement tmpvarmap_free (tmps) = $Map.map_free (tmps)

implement instr_tmpvarmap_add (m, ins) = let
  fun aux_branchlst (m: &tmpvarmap, brs: branchlst)
    : void = begin case+ brs of
    | list_cons (br, brs) => let
        val () = instrlst_tmpvarmap_add (m, br.branch_inss)
      in
        aux_branchlst (m, brs)
      end // end of [list_cons]
    | list_nil () => ()
  end // end of [aux_branchlst]
in
  case+ ins of
  | INSTRarr (tmp, _, _) => tmpvarmap_add_root (m, tmp)
  | INSTRcall (tmp, _, _, _) => tmpvarmap_add_root (m, tmp)
(*
  | INSTRcall_tail fl => ()
*)
  | INSTRcond (_, inss_then, inss_else) => begin
      instrlst_tmpvarmap_add (m, inss_then);
      instrlst_tmpvarmap_add (m, inss_else);
    end
  | INSTRfunction (tmp_ret_all, vps_arg, inss_body, tmp_ret) => begin
      tmpvarmap_add_root (m, tmp_ret_all);
      instrlst_tmpvarmap_add (m, inss_body);
      tmpvarmap_add_root (m, tmp_ret);
    end
  | INSTRload_ptr (tmp, _) => tmpvarmap_add_root (m, tmp)
  | INSTRload_ptr_offs (tmp, _, _) => tmpvarmap_add_root (m, tmp)
  | INSTRload_var (tmp, _) => tmpvarmap_add_root (m, tmp)
  | INSTRload_var_offs (tmp, _, _) => tmpvarmap_add_root (m, tmp)
  | INSTRmove_con (tmp, _, _, _) => tmpvarmap_add_root (m, tmp)
  | INSTRmove_rec_box (tmp, _, _) => tmpvarmap_add_root (m, tmp)
  | INSTRmove_rec_flt (tmp, _, _) => tmpvarmap_add_root (m, tmp)
  | INSTRmove_ref (tmp, _) => tmpvarmap_add_root (m, tmp)
  | INSTRmove_val (tmp, _) => tmpvarmap_add_root (m, tmp)
  | INSTRselect (tmp, _, _) => tmpvarmap_add_root (m, tmp)
  | INSTRselcon (tmp, _, _, _) => tmpvarmap_add_root (m, tmp)
  | INSTRselcon_ptr (tmp, _, _, _) => tmpvarmap_add_root (m, tmp)
  | INSTRpar_spawn (tmp, _) => tmpvarmap_add_root (m, tmp)
  | INSTRpar_synch (tmp) => tmpvarmap_add_root (m, tmp)
  | INSTRswitch (brs) => aux_branchlst (m, brs)
  | INSTRtrywith (inss_try, tmp_exn, brs) => let
      val () = instrlst_tmpvarmap_add (m, inss_try)
    in
      tmpvarmap_add_root (m, tmp_exn); aux_branchlst (m, brs)
    end // end of [INSTRtrywith]
  | INSTRvardec (tmp) => tmpvarmap_add (m, tmp)
  | INSTRwhile (_, _, _, inss_test, inss_body) => begin
      instrlst_tmpvarmap_add (m, inss_test);
      instrlst_tmpvarmap_add (m, inss_body);
    end
  | _ => ()
end // end of [instr_tmpvarmap_add]

implement instrlst_tmpvarmap_add (m, inss) = begin
  case+ inss of
  | list_cons (ins, inss) => begin
      instr_tmpvarmap_add (m, ins); instrlst_tmpvarmap_add (m, inss)
    end
  | list_nil () => ()
end // end of [instrlst_tmpvarmap_add]

//

implement emit_tmpvarmap_dec_local (pf | out, tmps) =
  _emit_tmpvarmap_dec (pf, view@ out | &out, 0(*local*), tmps)

implement emit_tmpvarmap_dec_static (pf | out, tmps) =
  _emit_tmpvarmap_dec (pf, view@ out | &out, 1(*static*), tmps)

//

implement emit_tmpvarmap_markroot (pf | out, tmps) =
  _emit_tmpvarmap_markroot (pf, view@ out | &out, tmps)

//

implement funentry_tmpvarmap_gen (entry) = let
  var tmps = tmpvarmap_nil ()
  val () = instrlst_tmpvarmap_add (tmps, funentry_body_get entry)
  val () = tmpvarmap_add_root (tmps, funentry_ret_get entry)
in
  tmps
end // end of [funentry_tmpvarmap_gen]

end // end of [local]

(* ****** ****** *)

(* end of [ats_ccomp_util.dats] *)
