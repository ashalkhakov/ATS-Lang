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

// Time: May 2008
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

staload "ats_charlst.sats"

(* ****** ****** *)

implement charlst_free (cs) = begin case+ cs of
  | ~CHARLSTcons (c, cs) => charlst_free cs | ~CHARLSTnil () => ()
end // end of [charlst_free]

//

implement charlst_length (cs) = let
  fun aux {i,j:nat}
    (cs: !charlst_vt i, j: int j)
    : int (i+j) = begin case+ cs of
    | CHARLSTcons (_, !cs1) => begin
        let val n = aux (!cs1, j+1) in fold@ (cs); n end
      end
    | CHARLSTnil () => (fold@ (cs); j)
  end // end of [aux]
in
  aux (cs, 0)
end // end of [charlst_length]

//

implement string_make_rev_charlst (cs) = begin
   string_make_rev_charlst_int (cs, charlst_length cs)
end // end of [string_make_rev_charlst]

//

extern fun charlst_is_nil {n:nat} (cs: !charlst_vt n): bool (n == 0) =
  "ats_charlst_is_nil"

extern fun
charlst_uncons {n:pos} (cs: &charlst_vt n >> charlst_vt (n-1)): char =
  "ats_charlst_uncons"

implement charlst_is_nil (cs) = case+ cs of
  | CHARLSTcons _ => (fold@ cs; false) | CHARLSTnil _ => (fold@ cs; true)

implement charlst_uncons (cs) =
  let val+ ~CHARLSTcons (c, cs_r) = cs in cs := cs_r; c end

%{

ats_ptr_type
string_make_rev_charlst_int (ats_ptr_type cs, const ats_int_type n) {
  char *s0, *s;

  s0 = ATS_MALLOC (n+1) ;
  s = s0 + n ;
  *s = '\0' ; --s ;

  while (!ats_charlst_is_nil(cs)) { *s = ats_charlst_uncons(&cs) ; --s ; }

  return s0 ;
}

%}

(* ****** ****** *)

(* end of [ats_charlst.dats] *)
