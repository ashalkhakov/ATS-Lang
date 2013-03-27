// Unicode encoding/decoding functions
// Author: Artyom Shalkhakov
// Time: February 2013

exception Malformed
exception InvalidCodepoint of int

#define CODEPOINT_MAX 0x10ffff

datatype byte_order = BOlittle | BObig

fun get_byte_order (c0: char, c1: char): byte_order

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

abst@ype utf8_state = $extype"uint32_t"
macdef UTF8init = $extval (utf8_state, "0")
// true if accepting
fun utf8_state_is_accept (x: utf8_state):<> bool = "utf8_state_is_accept"
fun utf8_state_is_reject (x: utf8_state):<> bool = "utf8_state_is_reject"
fun utf8_decode_step (
  s: &utf8_state, codep: &int, b: char
) :<> utf8_state = "utf8_decode_step"

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
