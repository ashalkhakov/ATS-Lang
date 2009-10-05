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

// Time: January 2008
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

staload Eff = "ats_effect.sats"
staload Lab = "ats_label.sats"

(* ****** ****** *)

staload "ats_staexp2.sats"
staload "ats_dynexp2.sats"
staload "ats_dynexp3.sats"

(* ****** ****** *)

#define nil list_nil
#define cons list_cons
#define :: list_cons

(* ****** ****** *)

macdef fprint_label = $Lab.fprint_label

(* ****** ****** *)

implement fprint_p3at (pf | out, p3t) = let
  macdef prstr (s) = fprint1_string (pf | out, ,(s))
in
  case+ p3t.p3at_node of
  | P3Tann (p3t, s2e) => begin
      prstr "P3Tann(";
      fprint_p3at (pf | out, p3t);
      prstr "; ";
      fprint_s2exp (pf | out, s2e);
      prstr ")"
    end
  | P3Tany d2v => begin
      prstr "P3Tany("; fprint_d2var (pf | out, d2v); prstr ")"
    end
  | P3Tas (refknd, d2v, p3t) => begin
      prstr "P3Tas(";
      if (refknd > 0) then prstr "!";
      fprint_d2var (pf | out, d2v);
      prstr "; ";
      fprint_p3at (pf | out, p3t);
      prstr ")"
    end // end of [P3Tas]
  | P3Tbool b => begin
      prstr "P3Tbool("; fprint1_bool (pf | out, b); prstr ")"
    end
  | P3Tchar c => begin
      prstr "P3Tchar("; fprint1_char (pf | out, c); prstr ")"
    end
  | P3Tcon (refknd, d2c, npf, p3ts) => begin
      prstr "P3Tcon(";
      fprint1_int (pf | out, refknd);
      prstr "; ";
      fprint_d2con (pf | out, d2c);
      prstr "; ";
      fprint1_int (pf | out, npf);
      prstr "; ";
      fprint_p3atlst (pf | out, p3ts);
      prstr ")"
    end // end of [P3Tcon]
  | P3Tempty () => begin
      fprint1_string (pf | out, "P3Tempty()")
    end
  | P3Texist (s2vs, p3t) => begin
      prstr "P3Texist(";
      fprint_s2varlst (pf | out, s2vs);
      prstr "; ";
      fprint_p3at (pf | out, p3t);
      prstr ")"
    end // end of [P3Texist]
  | P3Tfloat str => begin
      fprintf1_exn (pf | out, "P3Tfloat(%s)", @(str))
    end
  | P3Tint (str, int) => begin
      fprintf1_exn (pf | out, "P3Tint(%s)", @(str))
    end
  | P3Tlst (s2e, p3ts) => begin
      prstr "P3Tlst(";
      fprint_s2exp (pf | out, s2e);
      prstr "; ";
      fprint_p3atlst (pf | out, p3ts);
      prstr ")"
    end // end of [P3Tlst]
  | P3Trec (recknd, npf, lp3ts) => begin
      fprint1_string (pf | out, "P3Trec(...)")
    end // end of [P3Trec]
  | P3Tstring str => begin
      fprintf1_exn (pf | out, "P3Tstring(\"%s\")", @(str))
    end // end of [P3Tstring]
  | P3Tvar (refknd, d2v) => begin
      prstr "P3Tvar(";
      if (refknd > 0) then prstr "!";
      fprint_d2var (pf | out, d2v);
      prstr ")"
    end
  | P3Tvbox (d2v) => begin
      prstr "P3Tvbox("; fprint_d2var (pf | out, d2v); prstr ")"
    end
(*
  | _ => begin
      prstr "fprint_p3at: not implemented yet"; fprint_newline (pf | out);
      exit (1)
    end
*)
end // end of [fprint_p3at]

implement fprint_p3atlst {m} (pf | out, p3ts) = let
  fun aux (out: &FILE m, i: int, p3ts: p3atlst)
    : void = begin case+ p3ts of
    | cons (p3t, p3ts) => begin
        if (i > 0) then fprint1_string (pf | out, ", ");
        fprint_p3at (pf | out, p3t); aux (out, i + 1, p3ts)
      end
    | nil () => ()
  end // end of [aux]
in
  aux (out, 0, p3ts)
end // end of [fprint_p3atlst]

(* ****** ****** *)

implement print_p3at (p3t) = print_mac (fprint_p3at, p3t)
implement prerr_p3at (p3t) = prerr_mac (fprint_p3at, p3t)

(* ****** ****** *)

implement fprint_d3exp (pf | out, d3e) = let
  macdef prstr (s) = fprint1_string (pf | out, ,(s))
in
  case+ d3e.d3exp_node of
  | D3Eann_type (d3e, s2e) => begin
      prstr "D3Eann_type(";
      fprint_d3exp (pf | out, d3e);
      prstr "; ";
      fprint_s2exp (pf | out, s2e);
      prstr ")"
    end
  | D3Eapp_dyn (d3e_fun, npf, d3es_arg) => begin
      prstr "D3Eapp_dyn(";
      fprint_d3exp (pf | out, d3e_fun);
      prstr "; ";
      fprint1_int (pf | out, npf);
      prstr "; ";
      fprint_d3explst (pf | out, d3es_arg);
      prstr ")"
    end
  | D3Eapp_sta d3e => begin
      prstr "D3Eapp_sta("; fprint_d3exp (pf | out, d3e); prstr ")"
    end
  | D3Earrinit (s2e_elt, od3e_asz, d3es_elt) => begin
      prstr "D3Earrinit(";
      fprint_s2exp (pf | out, s2e_elt);
      prstr "; ";
      begin case+ od3e_asz of
      | Some d3e => fprint_d3exp (pf | out, d3e) | None () => ()
      end;
      prstr "; ";
      fprint_d3explst (pf | out, d3es_elt);
      prstr ")"
    end // end of [D3Earrinit]
  | D3Earrsize (s2e, d3es) => begin
      prstr "D3Earrsize(";
      fprint_s2exp (pf | out, s2e);
      prstr "; ";
      fprint_d3explst (pf | out, d3es);
      prstr ")"
    end // end of [D3Earrsize]
  | D3Eassgn_ptr (d3e_ptr, d3ls, d3e_val) => begin
      prstr "D3Eassgn_ptr(";
      fprint_d3exp (pf | out, d3e_ptr);
      prstr "; ";
      fprint_d3lab1lst (pf | out, d3ls);
      prstr "; ";
      fprint_d3exp (pf | out, d3e_val);
      prstr ")"
    end // end of [D3Eassgn_ptr]
  | D3Eassgn_var (d2v_ptr, d3ls, d3e_val) => begin
      prstr "D3Eassgn_val(";
      fprint_d2var (pf | out, d2v_ptr);
      prstr "; ";
      fprint_d3lab1lst (pf | out, d3ls);
      prstr "; ";
      fprint_d3exp (pf | out, d3e_val);
      prstr ")"
    end // end of [D3Eassgn_var]
  | D3Ecaseof (knd, d3es, c3ls) => begin
      fprint1_string (pf | out, "D3Ecaseof(...)")
    end
  | D3Echar chr => begin
      prstr "D3Echar("; fprint1_char (pf | out, chr); prstr ")"
    end
  | D3Econ (d2c, npf, d3es) => begin
      prstr "D3Econ(";
      fprint_d2con (pf | out, d2c);
      prstr "; ";
      fprint1_int (pf | out, npf);
      prstr "; ";
      fprint_d3explst (pf | out, d3es);
      prstr ")"
    end // end of [D3Econ]
  | D3Ecst d2c => begin
      prstr "D3Ecst("; fprint_d2cst (pf | out, d2c); prstr ")"
    end // end of [D3Ecst]
  | D3Ecstsp _ => begin
      prstr "D3Ecstsp("; fprint1_string (pf | out, "..."); prstr ")"
    end // end of [D3Ecstsp]
  | D3Ecrypt (knd, d3e) => begin
      prstr "D3Ecrypt(";
      fprint1_int (pf | out, knd);
      prstr "; ";
      fprint_d3exp (pf | out, d3e);
      prstr ")"
    end // end of [D3Ecrypt]
  | D3Edynload fil => begin
      prstr "D3Edynload(";
      $Fil.fprint_filename (pf | out, fil);
      prstr ")"
    end
  | D3Eeffmask (effs, d3e) => begin
      prstr "D3Eeffmask(";
      $Eff.fprint_effectlst (pf | out, effs);
      prstr "; ";
      fprint_d3exp (pf | out, d3e);
      prstr ")"
    end
  | D3Eempty () => begin
      fprint1_string (pf | out, "D3Eempty()")
    end
  | D3Eextval (str) => begin
      fprintf1_exn (pf | out, "D3Eextval(\"%s\")", @(str))
    end
  | D3Efix (d2v, d3e) => begin
      prstr "D3Efix(";
      fprint_d2var (pf | out, d2v);
      prstr "; ";
      fprint_d3exp (pf | out, d3e);
      prstr ")"
    end // end of [D3Efix]
  | D3Efloat (str) => begin
      fprintf1_exn (pf | out, "D3Efloat(%s)", @(str))
    end
  | D3Efloatsp (str) => begin
      fprintf1_exn (pf | out, "D3Efloatsp(%s)", @(str))
    end
  | D3Efoldat d3e => begin
      prstr "D3Efoldat("; fprint_d3exp (pf | out, d3e); prstr ")"
    end
  | D3Efreeat d3e => begin
      prstr "D3Efreeat("; fprint_d3exp (pf | out, d3e); prstr ")"
    end
  | D3Eif (d3e_cond, d3e_then, d3e_else) => begin
      prstr "D3Eif(";
      fprint_d3exp (pf | out, d3e_cond);
      prstr "; ";
      fprint_d3exp (pf | out, d3e_then);
      prstr "; ";
      fprint_d3exp (pf | out, d3e_else);
      prstr ")"
    end // end of [D3Eif]
  | D3Eint (str, _(*intinf*)) => begin
      fprintf1_exn (pf | out, "D3Eint(%s)", @(str))
    end
  | D3Eintsp (str, _(*intinf*)) => begin
      fprintf1_exn (pf | out, "D3Eintsp(%s)", @(str))
    end
  | D3Elam_dyn (lin, npf, p3ts, d3e) => begin
      prstr "D3Elam_dyn(";
      fprint1_int (pf | out, lin);
      prstr "; ";
      fprint1_int (pf | out, npf);
      prstr "; ";
      fprint_p3atlst (pf | out, p3ts);
      prstr "; ";
      fprint_d3exp (pf | out, d3e);
      prstr ")"
    end // end of [D3Elam_dyn]
  | D3Elaminit_dyn (lin, npf, p3ts, d3e) => begin
      prstr "D3Elaminit_dyn(";
      fprint1_int (pf | out, lin);
      prstr "; ";
      fprint1_int (pf | out, npf);
      prstr "; ";
      fprint_p3atlst (pf | out, p3ts);
      prstr "; ";
      fprint_d3exp (pf | out, d3e);
      prstr ")"
    end // end of [D3Elaminit_dyn]
  | D3Elam_met (s2es, d3e) => begin
      prstr "D3Elam_met(";
      fprint_s2explst (pf | out, s2es);
      prstr "; ";
      fprint_d3exp (pf | out, d3e);
      prstr ")"
    end // end of [D3Elam_met]
  | D3Elam_sta (s2vs, s2ps, d3e) => begin
      prstr "D3Elam_sta(";
      fprint_s2varlst (pf | out, s2vs);
      prstr "; ";
      fprint_s2explst (pf | out, s2ps);
      prstr "; ";
      fprint_d3exp (pf | out, d3e);
      prstr ")"
    end // end of [D3Elam_sta]
  | D3Elazy_delay (d3e) => begin
      prstr "D3Elazy_delay("; fprint_d3exp (pf | out, d3e); prstr ")"
    end // end of [D3Elazy_delay]
  | D3Elazy_vt_delay (d3e_eval, d3e_free) => begin
      prstr "D3Elazy_delay(";
      fprint_d3exp (pf | out, d3e_eval);
      prstr "; ";
      fprint_d3exp (pf | out, d3e_free);
      prstr ")"
    end // end of [D3Elazy_vt_delay]
  | D3Elazy_force (lin, d3e) => begin
      prstr "D3Elazy_force(";
      fprint_int (pf | out, lin);
      prstr "; ";
      fprint_d3exp (pf | out, d3e);
      prstr ")"
    end // end of [D3Elazy_force]
  | D3Elet (d3cs, d3e) => begin
      prstr "D3Elet(";
      fprint1_string (pf | out, "...");
      prstr "; ";
      fprint_d3exp (pf | out, d3e);
      prstr ")"
    end // end of [D3Elet]
  | D3Eloop (od3e_init, d3e_test, od3e_post, d3e_body) => begin
      prstr "D3Eloop(";
      begin case+ od3e_init of
        | None () => () | Some d3e => fprint_d3exp (pf | out, d3e);
      end;
      prstr "; ";
      fprint_d3exp (pf | out, d3e_test);
      prstr "; ";
      begin case+ od3e_post of
        | None () => () | Some d3e => fprint_d3exp (pf | out, d3e)
      end;
      prstr "; ";
      fprint_d3exp (pf | out, d3e_body);
      prstr ")"
    end // end of [D3Eloop]
  | D3Eloopexn i => begin
      fprintf1_exn (pf | out, "D3Eloopexn(%i)", @(i))
    end
  | D3Elst (lin, s2e, d3es) => begin
      prstr "D3Elst(";
      fprint_s2exp (pf | out, s2e);
      prstr "; ";
      fprint_d3explst (pf | out, d3es);
      prstr ")"
    end // end of [D3Elst]
  | D3Emtd d2m => begin
      prstr "D3Emtd("; fprint_d2mtd (pf | out, d2m); prstr ")"
    end // end of [D3Emtd]
  | D3Eobj _ => begin
      prstr "D3Eobj("; fprint_string (pf | out, "..."); prstr ")"
    end // end of [D3Eobj]
  | D3Eptrof_ptr (d3e, d3ls) => begin
      prstr "D3Eptrof_ptr(";
      fprint_d3exp (pf | out, d3e);
      prstr "; ";
      fprint_d3lab1lst (pf | out, d3ls);
      prstr ")"
    end // end of [D3Eptrof_ptr]
  | D3Eptrof_var (d2v, d3ls) => begin
      prstr "D3Eptrof_var(";
      fprint_d2var (pf | out, d2v);
      prstr "; ";
      fprint_d3lab1lst (pf | out, d3ls);
      prstr ")"
    end // end of [D3Eptrof_var]
  | D3Eraise (d3e_exn) => begin
      prstr "D3Eraise("; fprint_d3exp (pf | out, d3e_exn); prstr ")"
    end // end of [D3Eraise]
  | D3Erec (knd, npf, ld3es) => begin
      prstr "D3Erec(";
      fprint1_int (pf | out, knd);
      prstr "; ";
      fprint1_int (pf | out, npf);
      prstr "; ";
      fprint_labd3explst (pf | out, ld3es);
      prstr ")"
    end // end of [D3Erec]
  | D3Erefarg (refval, freeknd, d3e) => begin
      prstr "D3Erefarg(";
      fprint1_int (pf | out, refval);
      prstr "; ";
      fprint1_int (pf | out, freeknd);
      prstr "; ";
      fprint_d3exp (pf | out, d3e);
      prstr ")"
    end // end of [D3Erefarg]
  | D3Escaseof (s2e, sc3ls) => begin
      fprint1_string (pf | out, "D3Escaseof(...)")
    end // end of [D3Escaseof]
  | D3Esel (d3e, d3ls) => begin
      prstr "D3Esel(";
      fprint_d3exp (pf | out, d3e);
      prstr "; ";
      fprint_d3lab1lst (pf | out, d3ls);
      prstr ")"
    end // end of [D3Esel]
  | D3Esel_ptr (d3e, d3ls) => begin
      prstr "D3Esel_ptr(";
      fprint_d3exp (pf | out, d3e);
      prstr "; ";
      fprint_d3lab1lst (pf | out, d3ls);
      prstr ")"
    end // end of [D3Esel_ptr]
  | D3Esel_var (d2v, d3ls) => begin
      prstr "D3Esel_var(";
      fprint_d2var (pf | out, d2v);
      prstr "; ";
      fprint_d3lab1lst (pf | out, d3ls);
      prstr ")"
    end // end of [D3Esel_var]
  | D3Eseq d3es => begin
      prstr "D3Eseq("; fprint_d3explst (pf | out, d3es); prstr ")"
    end
  | D3Esif (s2e_cond, d3e_then, d3e_else) => begin
      prstr "D3Esif(";
      fprint_s2exp (pf | out, s2e_cond);
      prstr "; ";
      fprint_d3exp (pf | out, d3e_then);
      prstr "; ";
      fprint_d3exp (pf | out, d3e_else);
      prstr ")"
    end // end of [sif]
  | D3Espawn (d3e) => begin
      prstr "D3Espawn("; fprint_d3exp (pf | out, d3e); prstr ")"
    end // end of [D3Espawn]
  | D3Estring (str, len) => begin
      fprint1_string (pf | out, "D3Estring(...)")
    end // end of [D3Estring]
  | D3Estruct (ld3es) => begin
      prstr "D3Estruct("; fprint_labd3explst (pf | out, ld3es); prstr ")"
    end // end of [D3Estruct]
  | D3Etmpcst (d2c, s2ess) => begin
      prstr "D3Etmpcst(";
      fprint_d2cst (pf | out, d2c);
      prstr "; ";
      fprint_s2explstlst (pf | out, s2ess);
      prstr ")"
    end // end of [D3Etmpcst]
  | D3Etmpvar (d2v, s2ess) => begin
      prstr "D3Etmpvar(";
      fprint_d2var (pf | out, d2v);
      prstr "; ";
      fprint_s2explstlst (pf | out, s2ess);
      prstr ")"
    end // end of [D3Etmpvar]
  | D3Etop () => begin
      fprint1_string (pf | out, "D3Etop()")
    end // end of [D3Etop]
  | D3Etrywith (d3e, c3ls) => begin
      fprint1_string (pf | out, "D3Etrywith(...)")
    end // end of [trywith]
  | D3Evar d2v => begin
      prstr "D3Evar("; fprint_d2var (pf | out, d2v); prstr ")"
    end // end of [D3Evar]
  | D3Eviewat_assgn_ptr (d3e_l, d3ls, d3e_r) => begin
      prstr "D3Eviewat_assgn_ptr(";
      fprint_d3exp (pf | out, d3e_l);
      prstr "; ";
      fprint_d3lab1lst (pf | out, d3ls);
      prstr "; ";
      fprint_d3exp (pf | out, d3e_r);
      prstr ")"
    end // end of [D3Eviewat_assgn_ptr]
  | D3Eviewat_assgn_var (d2v_l, d3ls, d3e_r) => begin
      prstr "D3Eviewat_assgn_var(";
      fprint_d2var (pf | out, d2v_l);
      prstr "; ";
      fprint_d3lab1lst (pf | out, d3ls);
      prstr "; ";
      fprint_d3exp (pf | out, d3e_r);
      prstr ")"
    end // end of [D3Eviewat_assgn_var]
  | D3Eviewat_ptr (d3e, _, _, _) => begin
      prstr "D3Eviewat_ptr("; fprint_d3exp (pf | out, d3e); prstr ")"
    end // end of [D3Eviewat_ptr]
  | D3Eviewat_var (d2v, _, _, _) => begin
      prstr "D3Eviewat_var("; fprint_d2var (pf | out, d2v); prstr ")"
    end // end of [D3Eviewat_var]
  | D3Ewhere (d3e, d3cs) => begin
      prstr "D3Ewhere(";
      fprint_d3exp (pf | out, d3e);
      prstr "; ";
      fprint1_string (pf | out, "...");
      prstr ")"
    end // end of [D3Ewhere]
(*
  | _ => begin
      fprint1_string (pf | out, "[...]")
    end
*)
end // end of [fprint_d3exp]

implement fprint_d3explst {m} (pf | out, d3es) = let
  fun aux (out: &FILE m, i: int, d3es: d3explst)
    : void = begin case+ d3es of
    | cons (d3e, d3es) => begin
        if (i > 0) then fprint1_string (pf | out, ", ");
        fprint_d3exp (pf | out, d3e); aux (out, i + 1, d3es)
      end
    | nil () => ()
  end // end of [aux]
in
  aux (out, 0, d3es)
end // end of [fprint_d3explst]

implement fprint_d3explstlst {m} (pf | out, d3ess) = let
  fun aux (out: &FILE m, i: int, d3ess: d3explstlst)
    : void = begin case+ d3ess of
    | cons (d3es, d3ess) => begin
        if (i > 0) then fprint1_string (pf | out, ", ");
        fprint_d3explst (pf | out, d3es); aux (out, i + 1, d3ess)
      end
    | nil () => ()
  end // end of [aux]
in
  aux (out, 0, d3ess)
end // end of [fprint_d3explstlst]

implement fprint_labd3explst {m} (pf | out, ld3es0) = let
  fun aux (out: &FILE m, i: int, ld3es: labd3explst): void = let
    macdef prstr (s) = fprint1_string (pf | out, ,(s))
  in
    case+ ld3es of
    | LABD3EXPLSTcons (l, d3e, ld3es) => begin
        if i > 0 then prstr ", ";
        fprint_label (pf | out, l); prstr "= ";
        fprint_d3exp (pf | out, d3e); aux (out, i+1, ld3es)
      end
    | LABD3EXPLSTnil () => ()
    end // end of [aux]
in
  aux (out, 0, ld3es0)
end // end of [fprint_labd3explst]

(* ****** ****** *)

implement fprint_d3lab1 (pf | out, d3l) = let
  macdef prstr (s) = fprint1_string (pf | out, ,(s))
in
  case+ d3l.d3lab1_node of
  | D3LAB1lab (l, s2e_rec) => begin
      fprint_label (pf | out, l);
      prstr "("; fprint_s2exp (pf | out, s2e_rec); prstr ")"
    end // end of [D3LAB1lab]
  | D3LAB1ind (d3ess_ind, s2e_elt) => begin
      prstr "[";
      fprint_d3explstlst (pf | out, d3ess_ind);
      prstr "]";
      prstr "(";
      fprint_s2exp (pf | out, s2e_elt);
      prstr ")"
    end // end of [D3LAB1ind]
end // end of [fprint_d3lab1]

implement fprint_d3lab1lst {m} (pf | out, d3ls) = let
  fun aux (out: &FILE m, i: int, d3ls: d3lab1lst)
    : void = begin case+ d3ls of
    | cons (d3l, d3ls) => begin
        if (i > 0) then fprint1_string (pf | out, ", ");
        fprint_d3lab1 (pf | out, d3l); aux (out, i + 1, d3ls)
      end
    | nil () => ()
  end // end of [aux]
in
  aux (out, 0, d3ls)
end // end of [fprint_d3lab1lst]

(* ****** ****** *)

implement print_d3exp (d3e) = print_mac (fprint_d3exp, d3e)
implement prerr_d3exp (d3e) = prerr_mac (fprint_d3exp, d3e)

implement print_d3explst (d3es) = print_mac (fprint_d3explst, d3es)
implement prerr_d3explst (d3es) = prerr_mac (fprint_d3explst, d3es)

implement print_d3explstlst (d3ess) = print_mac (fprint_d3explstlst, d3ess)
implement prerr_d3explstlst (d3ess) = prerr_mac (fprint_d3explstlst, d3ess)

implement print_labd3explst (ld3es) = print_mac (fprint_labd3explst, ld3es)
implement prerr_labd3explst (ld3es) = prerr_mac (fprint_labd3explst, ld3es)

(* ****** ****** *)

(* end of [ats_dynexp3_print.dats] *)
