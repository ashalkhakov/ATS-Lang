(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Power of Types!
**
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
*)

(* ****** ****** *)
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: July 2007
//
(* ****** ****** *)
// Unicode encoding/decoding functions
// defined in unicode.dats

exception Malformed

#define CODEPOINT_MAX 0x10ffff

dataprop utf8_width_p (int, int) =
  | {p:int | p >= 0; p <= 0x7f} utf8_one (p, 1)
  | {p:nat | p > 0x7f; p <= 0x7ff} utf8_two (p, 2)
  | {p:nat | p > 0x7ff; p <= 0xffff} utf8_three (p, 3)
  | {p:nat | p > 0xffff; p <= 0x10ffff} utf8_four (p, 4)

fun utf8_codepoint_width {i:int} (
  p: int i
) :<!exn> [m:int] (
  utf8_width_p (i, m) | int m
) = "utf8_codepoint_width"

fun utf8_codepoint_store {i,n:int} {l:addr} (
  pf_wid: utf8_width_p (i, n)
, pf_at: !b0ytes n @ l >> bytes n @ l
| p: ptr l
, n: size_t n
, c: int i
) : ptr (l+n) = "utf8_codepoint_store"

fun utf8_encode
  {m:file_mode} {l:addr} {p:nat} (
  pf_mod: file_mode_lte (m, w)
, pf_fil: !FILE m @ l
| p_fil: ptr l
, p: int p
) : int

fun utf8_decode
  {m:file_mode} {l:addr} (
  pf_mod: file_mode_lte (m, r)
, pf_fil: !FILE m @ l
| p_fil: ptr l
) : int

fun utf32_isdigit (p: int): bool
fun utf32_isspace (p: int): bool

// FIXME: bad name... also, should be abstract
// TODO: use 0-terminated int32 strings
viewtypedef string32_vt (n:int) = array (Nat, n)
viewtypedef string32_vt = [n:nat] string32_vt (n)
fun eq_string32_string32 {m,n:nat} (
  m: size_t m
, p: string32_vt m
, n: size_t n
, q: string32_vt n
) :<!ref> bool
fun fprint_string32 {m:file_mode} {l:addr} {n:nat} (
  pf_mod: file_mode_lte (m, w)
, pf_fil: !FILE m @ l
| p_fil: ptr l, x: string32_vt n
, asz: size_t n
): void
(*
fun print_string32 (x: string32_vt): void
overload print with print_string32
fun prerr_string32 (x: string32_vt): void
overload prerr with prerr_string32
*)

fun string_of_string32 {n:nat} (
  x: string32_vt n, n: size_t
) :<> string = "string_of_string32"

(* ****** ****** *)

abstype pos_t (* defined in position.dats *)
abstype charset_t (* defined in [charset.dats] *)
abstype intset_t (* defined in [intset.dats] *)
absviewtype states_t (* defined in [states.dats] *)

dataviewtype
token = // type for tokens
  | TOKchar of int (* literal character *)
  | TOKcode of string (* arbitrary ATS code *)
  | TOKint of int (* integer literal *)
  | {n:nat} TOKstring of @(size_t n, string32_vt n) (* string literal *)
  | {n:nat} TOKword of @(size_t n, string32_vt n) (* identifier *)
  | TOKlit of int (* literal *)
  | TOKmark of string (* %{ and %} *)
  | TOKeof
// end of [token]

datatype regex = // type for regular expressions
  | REGalt of (regex, regex)
  | REGchars of charset_t
  | REGid of string
  | REGnil
  | REGopt of regex
  | REGplus of regex
  | REGrep of (regex, int)
  | REGseq of (regex, regex)
  | REGstar of regex
  | {n:nat} REGstr of @(size_t n, string32_vt n)
// end if [regex]

// TODO? viewtypedef redef = List_vt @(string (*identifier*), regex)
dataviewtype redef =
  | redef_nil | redef_cons of (string (* identifier *), regex, redef)
// end of [redef]

// TODO? viewtypedef rules = List_vt @(regex, string (*code for action*))
dataviewtype rules =
  | rules_nil | rules_cons of (regex, string (* code for action *), rules)
// end of [rules]

// TODO? viewtypedef lexfns = List_vt @{name= string, arg= string, rules= rules}
dataviewtype lexfns =
  | lexfns_nil | lexfns_cons of (string (*name*), string (*arg*), rules, lexfns)
// end if [lexfns]

viewtypedef lexer = '{
  preamble= string, redef= redef, lexfns= lexfns, postamble= string
} // end of [lexer]

typedef lexer0 = '{
  preamble= string, redef= redef?, lexfns= lexfns?, postamble= string
} // end of [lexer0]

(* ****** ****** *)

fun the_atslex_input_fin (): void
fun the_atslex_input_set {l:addr} (pf: FILE r @ l | p: ptr l): void

fun atslex_getchar (): int = "atslex_getchar"

fun atslex_get_reentrant (): bool
fun atslex_set_reentrant (b: bool): void

(* ****** ****** *)
//
// HX: implemented in [token.dats]
//
fun pos_get_line (): int = "pos_get_line"
fun pos_get_char (): int = "pos_get_char"
fun pos_get_line_prev (): int = "pos_get_line_prev"
fun pos_get_char_prev (): int = "pos_get_char_prev"

fun token_get (): token
fun token_free (x: token):<> void = "token_free"

fun tokenize_line_comment (): void
fun tokenize_rest_text (): string
fun tokenize_logue (): string
fun tokenize_funarg (): string

fun print_token (tok: !token): void = "print_token"
fun prerr_token (tok: !token): void = "prerr_token"

fun token_initialization (): void

(* ****** ****** *)
//
// HX: implemented in [position.dats]
//
fun position_get (): pos_t = "position_get"
fun position_prev_get (): pos_t = "position_prev_get"

fun print_pos (p: pos_t): void = "print_pos"
fun prerr_pos (p: pos_t): void = "prerr_pos"

(* ****** ****** *)
//
// HX: implemented in [charset.sats]
//

val charset_all: charset_t // the full charset
val charset_nil: charset_t // the empty charset
val charset_eof: charset_t

fun charset_is_nil (cs: charset_t): bool

fun charset_interval (c1: int, c2: int): charset_t
fun charset_singleton (c: int): charset_t

fun charset_complement (cs: charset_t): charset_t
fun charset_difference (cs1: charset_t, cs2: charset_t): charset_t
fun charset_intersect (cs1: charset_t, cs2: charset_t): charset_t
fun charset_union (cs1: charset_t, cs2: charset_t): charset_t
fun charset_is_member (cs: charset_t, c: int): bool
fun charset_is_joint (cs1: charset_t, cs2: charset_t): bool

fun compare_charset_charset (cs1: charset_t, cs2: charset_t): Sgn

fun list_vt_of_charset (cs: charset_t, tag: int): [m:nat] (int m, list_vt (@(int, int, int), m))

fun fprint_charset {m:file_mode}
  (pf_mod: file_mode_lte (m, w) | fil: &FILE m, cs: charset_t): void =
  "fprint_charset"

fun print_charset (cs: charset_t): void
fun prerr_charset (cs: charset_t): void

val charset_base_char: charset_t
val charset_ideographic: charset_t
val charset_combining_char: charset_t
val charset_digit: charset_t
val charset_extender: charset_t
val charset_blank: charset_t
val charset_letter: charset_t
val charset_tr8876_ident_char: charset_t

(* ****** ****** *)
//
// HX: implemented in [parser.dats]
//
fun print_regex (reg: regex): void
fun prerr_regex (reg: regex): void

fun lexer_parse (): lexer
fun lexer_free (x: &lexer >> lexer0): void

(* ****** ****** *)
//
// HX: implemented in [intset.dats]
//
val intset_nil : intset_t
fun intset_is_nil (ns: intset_t): bool

fun intset_singleton (n: int): intset_t

fun eq_intset_intset (ns1: intset_t, ns2: intset_t): bool
overload = with eq_intset_intset

fun compare_intset_intset (ns1: intset_t, ns2: intset_t): Sgn
overload compare with compare_intset_intset

fun union_intset_intset (ns1: intset_t, ns2: intset_t): intset_t
overload + with union_intset_intset

fun fprint_intset {m:file_mode}
  (pf_mod: file_mode_lte (m, w) | fil: &FILE m, ns: intset_t): void =
  "fprint_intset"

fun print_intset (cs: intset_t): void
fun prerr_intset (cs: intset_t): void

fun foreach_intset {v:view}
  (pf: !v | f: &(!v | int) -<clo1> void, ns: intset_t): void
// end of [foreach_intset]

(* ****** ****** *)
//
// HX: implemented in [states.dats]
//
fun states_nil (): states_t

fun states_free (sts: states_t): void

fun states_find (sts: !states_t, ns0: intset_t): int(*tag or ~1*)
fun states_insert (sts: &states_t, tag0: int, ns0: intset_t): void

fun states_foreach_and_free {v:view}
  (pf: !v | f: &(!v | int, intset_t) -<clo1> void, sts: states_t): void
// end of [states_foreach_and_free]

(* ****** ****** *)
//
// HX: implemented in [lexgen.dats]
//
fun fprint_lexfns {m:file_mode}
  (pf_mod: file_mode_lte (m, w) | fil: &FILE m, rds: &redef, lfs: !lexfns): void
// end of [fprint_lexfns]

(* ****** ****** *)

(* end of top.sats *)
