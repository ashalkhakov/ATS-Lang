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
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
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
**
*)

(* ****** ****** *)
//
// Author: Artyom Shalkhakov (artyom DOT shalkhakov AT gmail DOT com)
// Time: February 2013
//
(* ****** ****** *)

staload "unicode.sats"

staload "libc/SATS/stdio.sats"

staload _ = "prelude/DATS/array.dats"

(* ****** ****** *)

#define ATS_DYNLOADFLAG 0

(* ****** ****** *)

implement
get_byte_order (c0, c1) =
  case+ (c0, c1) of
  | ('\xFE', '\xFF') => BObig
  | ('\xFF', '\xFE') => BOlittle
  | (_, _) => $raise Malformed
// end of [get_byte_order]

fun
number_of_char_pair (bo: byte_order, c1: char, c2: char): int = let
  val c1 = uint_of_char c1
  and c2 = uint_of_char c2
in
  case+ bo of
  | BOlittle () => int_of_uint ((c2 << 8) lor c1)
  | BObig () => int_of_uint ((c1 << 8) lor c2)
end // end of [number_of_char_pair]
(*
fun
char_pair_of_number (bo: byte_order, n: int): @(char, char) = let
  val n = uint_of_int n
in
  case+ bo of
  | BOlittle () => @(char_of_uint (n land 0xffu), char_of_uint ((n >> 8) land 0xffu))
  | BObig () => @(char_of_uint ((n >> 8) land 0xffu), char_of_uint (n land 0xffu))
end // end of [char_pair_of_number]


fun
next_in_string {n,i:nat | i <= n} (bo: byte_order, s: string n, pos: size_t i, bytes: size_t n): int = begin
  if pos+1 >= bytes then $raise Malformed;
  number_of_chair_pair (bo, s[pos], s[pos+1])
end // end of [next_in_string]

fun
next_code {n,i:nat | i <= n} (bo: byte_order, s: string n, pos: size_t i, bytes: size_t n): [i:nat | i <= n] @(int, size_t i) = let
  val w1 = next_in_string (bo, s, pos, bytes)
in
  if w1 = 0xfffe then $raise (InvalidCodepoint w1);
  if w1 < 0xd800 || 0xdfff < w1 then @(w1, succ (succ pos))
  else if w1 <= 0xdbff then let
    val w2 = next_in_string (bo, s, succ (succ pos), bytes)
    val () = if w2 < 0xdc00 || w2 > 0xdfff then $raise Malformed
    val upper10 = (w1 land 0x3ff) << 10
    and lower10 = w2 land 0x3ff
  in
    @(0x10000 + upper10 + lower10, succ (succ (succ (succ pos))))
  end else $raise Malformed
// end of [next_code]
*)

(* ****** ****** *)

implement utf8_codepoint_width (p) =
  if p < 0 then $raise Malformed
  else if p <= 0x7f then (utf8_one () | 1)
  else if p <= 0x7ff then (utf8_two () | 2)
  else if p <= 0xffff then (utf8_three () | 3)
  else if p <= 0x10ffff then (utf8_four () | 4)
  else $raise Malformed
// end of [utf8_char_width]

(* ****** ****** *)

implement
utf8_codepoint_store {i,n} {l} (
  pf_wid, pf_at | p, n, c
) = let
  #define i2u uint_of_int
  #define nil array_v_nil
  #define cons array_v_cons
  #define unnil array_v_unnil
  #define uncons array_v_uncons
  #define i2b byte_of_int
  #define u2b byte_of_uint
  viewdef V = bytes n @ l
  prval () = eqsize_byte_one ()
in
  if :(pf_at: V) => c <= 0x7f then let
    prval utf8_one () = pf_wid
    prval (pf1_at, pf1_res) = uncons {byte?} (pf_at)
    prval () = unnil {byte?} (pf1_res)
    val () = !p := i2b c
    val p = p+sizeof<byte>
    prval () = pf_at := cons {byte} (pf1_at, nil {byte} ())
  in
    p
  end else if :(pf_at: V) => c <= 0x7ff then let
    prval utf8_two () = pf_wid
    prval (pf1_at, pf1_res) = uncons {byte?} (pf_at)
    prval (pf2_at, pf2_res) = uncons {byte?} (pf1_res)
    prval () = unnil {byte?} (pf2_res)
    val () = !p := u2b (0xc0u lor (i2u c >> 6))
    val p = p+sizeof<byte>
    val () = !p := u2b (0x80u lor ((i2u c) land 0x3fu))
    prval () = pf_at := cons {byte} (pf1_at, cons {byte} (pf2_at, nil {byte} ()))
    val p = p+sizeof<byte>
  in
    p
  end else if :(pf_at: V) => c <= 0xffff then let
    val () = if c >= 0xd800 && c < 0xe000 then $raise Malformed
    prval utf8_three () = pf_wid
    prval (pf1_at, pf1_res) = uncons {byte?} (pf_at)
    prval (pf2_at, pf2_res) = uncons {byte?} (pf1_res)
    prval (pf3_at, pf3_res) = uncons {byte?} (pf2_res)
    prval () = unnil {byte?} (pf3_res)
    val () = !p := u2b (0xe0u lor (i2u c >> 6))
    val p = p+sizeof<byte>
    val () = !p := u2b (0x80u lor ((i2u c >> 6) land 0x3fu))
    val p = p+sizeof<byte>
    val () = !p := u2b (0x80u lor ((i2u c) land 0x3fu))
    prval () = pf_at := cons {byte} (
      pf1_at
    , cons {byte} (pf2_at, cons {byte} (pf3_at, nil {byte} ())))
    val p = p+sizeof<byte>
  in
    p
  end else if :(pf_at: V) => c <= 0x10ffff then let
    prval utf8_four () = pf_wid
    prval (pf1_at, pf1_res) = uncons {byte?} (pf_at)
    prval (pf2_at, pf2_res) = uncons {byte?} (pf1_res)
    prval (pf3_at, pf3_res) = uncons {byte?} (pf2_res)
    prval (pf4_at, pf4_res) = uncons {byte?} (pf3_res)
    prval () = unnil {byte?} (pf4_res)
    val () = !p := u2b (0xf0u lor (i2u c >> 18))
    val p = p+sizeof<byte>
    val () = !p := u2b (0x80u lor ((i2u c >> 12) land 0x3fu))
    val p = p+sizeof<byte>
    val () = !p := u2b (0x80u lor ((i2u c >> 6) land 0x3fu))
    val p = p+sizeof<byte>
    val () = !p := u2b (0x80u lor ((i2u c) land 0x3fu))
    prval () = pf_at := cons {byte} (
      pf1_at
    , cons {byte} (pf2_at, cons {byte} (pf3_at
    , cons {byte} (pf4_at, nil {byte} ()))))
    val p = p+sizeof<byte>
  in
    p
  end else let
    extern
    prfun __pf1 (_: !b0ytes n @ l >> bytes n @ l):<> void
    prval () = __pf1 (pf_at)
  in
    $raise Malformed
  end // end of [if]
end // end of [utf8_codepoint_store]

(* ****** ****** *)

implement utf8_encode {m} {l} (pf_mod, pf_fil | p_fil, p) = let
  #define i2u uint_of_int
  #define u2c char_of_uint
  macdef fpc (x) = fputc1_err (pf_mod | char_of_uint ,(x), !p_fil)
  macdef fpcge0 (x, y) = let
    val res = fputc1_err (pf_mod | char_of_uint ,(x), !p_fil)
  in
    if res < 0 then res
    else ,(y)
  end // end of [fpcge0]
in
  if p <= 0x7f then
    fpc (i2u p)
  else if p <= 0x7ff then begin
    fpcge0 (
      0xc0u lor (i2u p >> 6)
    , fpc (0x80u lor (i2u p land 0x3fu)))
  end else if p <= 0xffff then begin
    if p >= 0xd800 && p < 0xe000 then $raise Malformed
    else begin
      fpcge0 (
        0xe0u lor (i2u p >> 12)
      , fpcge0 (
          0x80u lor ((i2u p >> 6) land 0x3fu)
        , fpc (0x80u lor ((i2u p) land 0x3fu))))
    end // end of [if]
  end else if p <= 0x10ffff then begin
    fpcge0 (0xf0u lor (i2u p >> 18)
    , fpcge0 (0x80u lor ((i2u p >> 12) land 0x3fu)
      , fpcge0 (0x80u lor ((i2u p >> 6) land 0x3fu)
        , fpc (0x80u lor ((i2u p) land 0x3fu)))))
  end else $raise Malformed
end // end of [utf8_encode]

(* ****** ****** *)

implement utf8_decode {m} {l} (pf_mod, pf_fil | p_fil) = let
  #define i2u uint_of_int
  #define u2i int_of_uint
  macdef invalid (x) = ,(x) >> 6 <> 0x2
  macdef ift (x, y) =
    if ,(x) then $raise Malformed else u2i ,(y)
  // end of [ift]
  macdef ifge0 (x, y) =
    if ,(x) >= 0 then ,(y) else $raise Malformed
  // end of [ifge0]
  val c1 = fgetc1_err (pf_mod | !p_fil)
in
  if c1 >= 0 then begin
    case+ 0 of
    | _ when c1 >= 0 && c1 <= 127 => c1
    | _ when c1 >= 192 && c1 <= 223 => let
        val c2 = fgetc1_err (pf_mod | !p_fil)
      in
        ifge0 (c2, ift (invalid c2, (((i2u c1) land 0x1fu) << 6) lor ((i2u c2) land 0x3fu)))
      end // end of [let]
    | _ when c1 >= 224 && c1 <= 239 => let
        val c2 = fgetc1_err (pf_mod | !p_fil)
      in
        ifge0 (c2, let
          val c3 = fgetc1_err (pf_mod | !p_fil)
        in
          ifge0 (c3, ift (invalid c2 || invalid c3,
                  (((i2u c1) land 0x0fu) << 12)
              lor (((i2u c2) land 0x3fu) << 6)
              lor ((i2u c3) land 0x3fu)))
        end)
      end // end of [let]
    | _ when c1 >= 240 && c1 <= 247 => let
        val c2 = fgetc1_err (pf_mod | !p_fil)
      in
        ifge0 (c2, let
          val c3 = fgetc1_err (pf_mod | !p_fil)
        in
          ifge0 (c3, let
            val c4 = fgetc1_err (pf_mod | !p_fil)
          in
            ifge0 (c4,
              ift (invalid c2 || invalid c3 || invalid c4,
                  (((i2u c1) land 0x07u) << 18)
                lor (((i2u c2) land 0x3fu) << 12)
                lor (((i2u c3) land 0x3fu) << 6)
                lor ((i2u c4) land 0x3fu)))
          end)
        end)
      end // end of [let]
    | _ => $raise Malformed
  end else c1 // EOF
end // end of [utf8_decode]

(* ****** ****** *)

%{
// Copyright (c) 2008-2010 Bjoern Hoehrmann <bjoern@hoehrmann.de>
// See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.

#define UTF8_ACCEPT 0
#define UTF8_REJECT 12

static const uint8_t utf8d[] = {
  // The first part of the table maps bytes to character classes that
  // to reduce the size of the transition table and create bitmasks.
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,

  // The second part is a transition table that maps a combination
  // of a state of the automaton and a character class to a state.
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12,
} ;

ats_bool_type
utf8_state_is_accept(uint32_t s) {
  return s == UTF8_ACCEPT ? ats_true_bool : ats_false_bool ;
} // end of [utf8_state_is_accept]

ats_bool_type
utf8_state_is_reject(uint32_t s) {
  return s == UTF8_REJECT ? ats_true_bool : ats_false_bool ;
} // end of [utf8_state_is_reject]

uint32_t
utf8_decode_step(
  ats_ref_type pstate, ats_ref_type pcodep, ats_char_type b
) {
  uint32_t *state = (uint32_t*)pstate, *codep = (uint32_t*)pcodep ;
  uint32_t byte = (uint32_t)b ;
  uint32_t type = utf8d[byte] ;

  *codep = (*state != UTF8_ACCEPT) ?
    (byte & 0x3fu) | (*codep << 6) :
    (0xff >> type) & (byte) ;

  *state = utf8d[256 + *state + type] ;
  return *state ;
} // end of [utf8_decode_step]

%}

(* ****** ****** *)

implement utf32_isdigit (p) = p >= 48 (* '0' *) && p <= 57 (* '9' *)

implement utf32_isspace (p) = char_isspace (char_of_int p)

(* ****** ****** *)

implement
eq_string32_string32 {m,n} (
  m, p, n, q
) =
  if m <> n then false
  else let
    fun loop {i,n:nat | i <= n} .<n-i>. (
      p: string32_vt n, q: string32_vt n, i: size_t i, n: size_t n
    ) :<!ref> bool = if i < n then begin
        if p[i] <> q[i] then false
        else loop (p, q, i+1, n)
      end else true
    // end of [loop]
  in
    loop (p, q, size1_of_int1 0, m)
  end // end of [eq_string32_string32]

(* ****** ****** *)

implement
fprint_string32 {m} {l} {n} (
  pf_mod
, pf_fil
| p_fil, x, asz
) = let
(*
  fun loop {n,i:nat | i <= n} {l1,l2:addr} .<n-i>. (
    pf_mod: file_mode_lte (m, w)
  , pf_fil: !FILE m @ l2
  | p_fil: ptr l2, arr: string32_vt n, i: size_t i, n: size_t n
  ) : void =
    if i < n then let
      val res = utf8_encode (pf_mod, pf_fil | p_fil, p_arr->[i])
    in
      assert_errmsg (res >= 0, "[fprint_string32]: error while decoding to utf-8");
      loop (pf_mod, pf_fil, pf_arr | p_fil, p_arr, i+1, n)
    end
  // end of [loop]
  val (pf_gc, pf_arr | p_arr, n) = x
  val () = loop (pf_mod, pf_fil, pf_arr | p_fil, p_arr, size1_of_int1 0, n)
  prval () = x.0 := pf_gc
  prval () = x.1 := pf_arr *)
  val () = array_foreach_vclo<Nat> {V} (pf_fil | x, !p_f, asz) where {
    viewdef V = FILE m @ l
    var !p_f = @lam (pf_fil: !V | a: &Nat):void =<clo> $effmask_all (let
      val res = utf8_encode (pf_mod, pf_fil | p_fil, a)
    in
      assert_errmsg (res >= 0, "[fprint_string32]: error while decoding to utf-8")
    end) // end of [var]
  }
in
  (*empty*)
end // end of [fprint_string32]

(* ****** ****** *)
(*
implement
print_string32 (x) = lete
  val (pf_stderr | p_stderr) = stdout_get ()
in
  fprint_string32 (file_mode_lte_w_w, pf_stdout | p_stdout, s, n);
  stdout_view_set (pf_stderr | (*none*))
end // end of [print_string32]
*)
(* ****** ****** *)
(*
implement
prerr_string32 (x) = let
  val (pf_stderr | p_stderr) = stderr_get ()
in
  fprint_string32 (file_mode_lte_w_w, pf_stderr | p_stderr, s, n);
  stderr_view_set (pf_stderr | (*none*))
end // end of [prerr_string32]
*)
(* ****** ****** *)

%{$

ats_ptr_type
string_of_string32 (ats_ptr_type cs, const ats_size_type n) {
  ats_size_type sz = 0, i = 0 ;
  ats_int_type* pc = (ats_int_type*)cs ;
  char *s0, *s ;

  while (i < n) { sz += utf8_codepoint_width (pc[i]) ; ++i ; }
  s0 = ats_malloc_gc(sz+1) ; s = s0 ; i = 0 ;
  while (i < n) {
    ats_size_type csz = utf8_codepoint_width (pc[i]) ;
    s = utf8_codepoint_store (s, csz, pc[i]) ;
    ++i ;
  }
  *s = '\0' ;
  return s0 ;
}

%}

(* ****** ****** *)

(* end of [unicode.dats] *)
