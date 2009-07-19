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
// Time: July 2009

(* ****** ****** *)

staload Loc = "ats_location.sats"
typedef loc_t = $Loc.location_t

staload Sym = "ats_symbol.sats"
typedef sym_t = $Sym.symbol_t

(* ****** ****** *)

staload "ats_staexp2.sats"
staload "ats_dynexp2.sats"

(* ****** ****** *)

typedef d2mtd_struct = struct {
  d2mtd_loc= loc_t
, d2mtd_sym= sym_t
, d2mtd_knd= mtdkind
, d2mtd_typ= s2exp
, d2mtd_stamp= stamp_t // uniqueness stamp
} // end of [d2mtd_struct]

(* ****** ****** *)

local

assume d2mtd_t =
  [l:addr] (vbox (d2mtd_struct @ l) | ptr l)
// end of [assume]

in // in of [local]

implement d2mtd_loc_get (d2m) =
  let val (vbox pf | p) = d2m in p->d2mtd_loc end

implement d2mtd_sym_get (d2m) =
  let val (vbox pf | p) = d2m in p->d2mtd_sym end

implement d2mtd_typ_get (d2m) =
  let val (vbox pf | p) = d2m in p->d2mtd_typ end

implement d2mtd_stamp_get (d2m) =
  let val (vbox pf | p) = d2m in p->d2mtd_stamp end

end // end of [local]

(* ****** ****** *)

implement fprint_d2mtd (pf_out | out, d2m) = begin
  $Sym.fprint_symbol (pf_out | out, d2mtd_sym_get d2m)
end // end of [fprint_d2mtd]

implement print_d2mtd (d2m) = print_mac (fprint_d2mtd, d2m)
implement prerr_d2mtd (d2m) = prerr_mac (fprint_d2mtd, d2m)

(* ****** ****** *)

(* end of [ats_dynexp2_dmtd.dats] *)
