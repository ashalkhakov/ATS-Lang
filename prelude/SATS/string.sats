(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Potential of Types!
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
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

(* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) *)

(* ****** ****** *)

// some string operations

(* ****** ****** *)

#include "prelude/params.hats"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then

#print "Loading [string.sats] starts!\n"

#endif

(* ****** ****** *)

stadef NUL = '\000'

(* ****** ****** *)

typedef bytes (n:int) = @[byte][n]
typedef b0ytes (n:int) = @[byte?][n]

typedef chars (n:int) = @[char][n]
typedef c0hars (n:int) = @[char?][n]
typedef c1har = [c:char | c <> NUL] char c
typedef c1hars (n:int) = @[c1har][n]

(* ****** ****** *)

viewdef strbuf_v
  (m: int, n: int, l:addr) = strbuf (m, n) @ l

viewdef strbuf_v (l:addr) = [m,n:nat] strbuf (m, n) @ l

prfun strbuf_vcontain_lemma0
  {m,n:nat} {l:addr} (): strbuf_v l <= strbuf_v (m, n, l)

(* ****** ****** *)

viewtypedef strbufptr_gc (m:int, n:int, l:addr) =
  @(free_gc_v (m, l), strbuf_v (m, n, l) | ptr l)

(* ****** ****** *)

praxi bytes_v_of_b0ytes_v {bsz:int}
  {l:addr} (pf: b0ytes (bsz) @ l):<prf> bytes (bsz) @ l

praxi char_v_of_b0yte_v {l:addr} (pf: byte? @ l): char @ l

praxi chars_v_of_b0ytes_v {bsz:int}
  {l:addr} (pf: b0ytes (bsz) @ l):<prf> chars (bsz) @ l

(* ****** ****** *)

praxi bytes_v_of_chars_v {bsz:int}
  {l:addr} (pf: chars (bsz) @ l):<prf> bytes (bsz) @ l

praxi bytes_v_of_strbuf_v {bsz:int}
  {l:addr} (pf: strbuf (bsz) @ l):<prf> bytes (bsz) @ l

(* ****** ****** *)

praxi strbuf_v_null {n:nat} {l:addr}
  (pf1: char NUL @ l, pf2: b0ytes (n) @ l + sizeof(char))
  : strbuf_v (n+1, 0, l)

//

praxi strbuf_v_cons
  {m,n:nat} {l:addr}
  (pf1: c1har @ l, pf2: strbuf_v (m, n, l + sizeof(char)))
  :<prf> strbuf_v (m+1, n+1, l)

//

dataview strbufopt_v (int, int, addr, char) =
  | {m:nat} {l:addr}
    strbufopt_v_none (m, ~1, l, NUL) of b0ytes m @ l
  | {m,n:nat} {l:addr} {c:char | c <> NUL}
    strbufopt_v_some (m, n, l, c) of strbuf_v (m, n, l)

praxi strbuf_v_uncons
  {m,n:nat} {l:addr} (pf: strbuf_v (m, n, l))
  :<prf> [c:char] @(
     char c @ l, strbufopt_v (m-1, n-1, l + sizeof(char), c)
   )

//

prfun strbuf_v_split
  {m,n:nat} {i:nat | i <= n} {l:addr} {ofs:int}
  (pf_mul: MUL (i, sizeof char, ofs), pf_str: strbuf_v (m, n, l))
  : (c1hars i @ l, strbuf_v (m-i, n-i, l+ofs))

prfun strbuf_v_unsplit
  {n1:nat} {m2,n2:nat} {l:addr} {ofs:int} (
    pf_mul: MUL (n1, sizeof char, ofs)
  , pf_buf: c1hars n1 @ l
  , pf_str: strbuf_v (m2, n2, l+ofs)
  ) : strbuf_v (n1+m2, n1+n2, l)

(* ****** ****** *)

fun bytes_strbuf_trans {m,n:nat | n < m} {l:addr}
  (pf: !b0ytes m @ l >> strbuf (m, n1) @ l | p: ptr l, n: size_t n)
  :<> #[n1: nat | n1 <= n] void
  = "atspre_bytes_strbuf_trans"

(* ****** ****** *)

// val string_empty : string 0 // this not really necessary

(* ****** ****** *)

// a casting function
castfn string1_of_string (str: string):<> [n:nat] string n
  = "atspre_string1_of_string"

(* ****** ****** *)

castfn string1_of_strbuf {m,n:nat} {l:addr}
  (pf_gc: free_gc_v (m, l), pf_buf: strbuf (m, n) @ l | p: ptr l) :<> string n
  = "atspre_string1_of_strbuf"

castfn strbuf_of_string1 {n:nat} (str: string n)
  :<> [m:int | n < m] [l:addr] (vbox (strbuf (m, n) @ l) | ptr l)
  = "atspre_strbuf_of_string1"

(* ****** ****** *)

fun strbuf_ptr_free {m,n:nat} {l:addr}
  (pf_gc: free_gc_v (m, l), pf_buf: strbuf (m, n) @ l | base: ptr l):<> void
  = "atspre_strbuf_ptr_free"

(* ****** ****** *)

fun lt_string_string (s1: string, s2: string):<> bool
  = "atspre_lt_string_string"
overload < with lt_string_string

fun lt_string_string__main {v:view} {l1,l2:addr} (
    pf: !v, pf1: strbuf_v l1 <= v, pf2: strbuf_v l2 <= v
  | p1: ptr l1, p2: ptr l2
  ) :<> bool
  = "atspre_lt_string_string"

//

fun lte_string_string (s1: string, s2: string):<> bool
  = "atspre_lte_string_string"
overload <= with lte_string_string

fun lte_string_string__main {v:view} {l1,l2:addr} (
    pf: !v, pf1: strbuf_v l1 <= v, pf2: strbuf_v l2 <= v
  | p1: ptr l1, p2: ptr l2
  ) :<> bool
  = "atspre_lte_string_string"

//

fun gt_string_string (s1: string, s2: string):<> bool
  = "atspre_gt_string_string"
overload > with gt_string_string

fun gt_string_string__main {v:view} {l1,l2:addr} (
    pf: !v, pf1: strbuf_v l1 <= v, pf2: strbuf_v l2 <= v
  | p1: ptr l1, p2: ptr l2
  ) :<> bool
  = "atspre_gt_string_string"

//

fun gte_string_string (s1: string, s2: string):<> bool
  = "atspre_gte_string_string"
overload >= with gte_string_string

fun gte_string_string__main {v:view} {l1,l2:addr} (
    pf: !v, pf1: strbuf_v l1 <= v, pf2: strbuf_v l2 <= v
  | p1: ptr l1, p2: ptr l2
  ) :<> bool
  = "atspre_gte_string_string"

//

fun eq_string_string (s1: string, s2: string):<> bool
  = "atspre_eq_string_string"
overload = with eq_string_string

fun eq_string_string__main {v:view} {l1,l2:addr} (
    pf: !v, pf1: strbuf_v l1 <= v, pf2: strbuf_v l2 <= v
  | p1: ptr l1, p2: ptr l2
  ) :<> bool
  = "atspre_eq_string_string"

//

fun neq_string_string (s1: string, s2: string):<> bool
  = "atspre_neq_string_string"
overload <> with neq_string_string

fun neq_string_string__main {v:view} {l1,l2:addr} (
    pf: !v, pf1: strbuf_v l1 <= v, pf2: strbuf_v l2 <= v
  | p1: ptr l1, p2: ptr l2
  ) :<> bool
  = "atspre_neq_string_string"

(* ****** ****** *)

fun compare_string_string (s1: string, s2: string):<> Sgn
  = "atspre_compare_string_string"
overload compare with compare_string_string

fun compare_string_string__main {v:view} {l1,l2:addr} (
    pf: !v, pf1: strbuf_v l1 <= v, pf2: strbuf_v l2 <= v
  | p1: ptr l1, p2: ptr l2
  ) :<> Sgn
  = "atspre_compare_string_string"

(* ****** ****** *)

symintr fprint_string

fun fprint0_string (out: FILEref, x: string):<!exnref> void
  = "atspre_fprint_string"

fun fprint1_string {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: string):<!exnref> void
  = "atspre_fprint_string"

overload fprint_string with fprint0_string
overload fprint_string with fprint1_string
overload fprint with fprint_string

fun fprint1_string__main {v:view} {l:addr}
  (pf: !v, fpf: strbuf_v l <= v | out: FILEref, p: ptr l):<!exnref> void
  = "atspre_fprint_string"

(* ****** ****** *)

fun print_string (b: string):<!ref> void = "atspre_print_string"
and prerr_string (b: string):<!ref> void = "atspre_prerr_string"

overload print with print_string
overload prerr with prerr_string

(* ****** ****** *)

fun strbuf_get_char_at {m,n:nat} {i:nat | i < n}
  (sbf: &strbuf (m, n), i: size_t i):<> [c:char | c <> NUL] char c
  = "atspre_string_get_char_at"
overload [] with strbuf_get_char_at

fun string_get_char_at {n:nat} {i:nat | i < n}
  (str: string n, i: size_t i):<> [c:char | c <> NUL] char c // no effect
  = "atspre_string_get_char_at"
overload [] with string_get_char_at

//
// these functions are present mostly for convenience as a programmer
// ofter uses values of the type int as array indices:
//

fun strbuf_get_char_at__intsz {m,n:nat} {i:nat | i < n}
  (sbf: &strbuf (m, n), i: int i):<> [c:char | c <> NUL] char c
  = "atspre_string_get_char_at__intsz"

fun string_get_char_at__intsz {n:nat} {i:nat | i < n}
  (str: string n, i: int i):<> [c:char | c <> NUL] char c // no effect
  = "atspre_string_get_char_at__intsz"

overload [] with strbuf_get_char_at__intsz
overload [] with string_get_char_at__intsz

(* ****** ****** *)

fun strbuf_set_char_at {m,n:nat}
  {i:nat | i < n} {c: char | c <> NUL}
  (sbf: &strbuf (m, n), i: size_t i, c: char c):<> void
  = "atspre_strbuf_set_char_at"

fun string_set_char_at {n:nat}
  {i:nat | i < n} {c: char | c <> NUL}
  (str: string n, i: size_t i, c: char c):<!ref> void
  = "atspre_strbuf_set_char_at"

overload [] with strbuf_set_char_at
overload [] with string_set_char_at

//
// these functions are present mostly for convenience as a programmer
// ofter uses values of the type int as array indices:
//

fun strbuf_set_char_at__intsz {m,n:nat}
  {i:nat | i < n} {c: char | c <> NUL}
  (sbf: &strbuf (m, n), i: int i, c: char c):<> void
  = "atspre_strbuf_set_char_at__intsz"

fun string_set_char_at__intsz {n:nat}
  {i:nat | i < n} {c: char | c <> NUL}
  (str: string n, i: int i, c: char c):<!ref> void
  = "atspre_strbuf_set_char_at__intsz"

overload [] with strbuf_set_char_at__intsz
overload [] with string_set_char_at__intsz

(* ****** ****** *)

fun strbuf_test_char_at {m,n:nat}
  {i:nat | i <= n} (sbf: &strbuf (m, n), i: size_t i)
  :<> [c:char | (c <> NUL && i < n) || (c == NUL && i >= n)] char c
  = "atspre_string_test_char_at"

fun string_test_char_at {n:nat}
  {i:nat | i <= n} (str: string n, i: size_t i)
  :<> [c:char | (c <> NUL && i < n) || (c == NUL && i >= n)] char c
  = "atspre_string_test_char_at"

//
// these functions are present mostly for convenience as a programmer
// ofter uses values of the type int as array indices:
//

fun strbuf_test_char_at__intsz {m,n:nat}
  {i:nat | i <= n} (sbf: &strbuf (m, n), i: size_t i)
  :<> [c:char | (c <> NUL && i < n) || (c == NUL && i >= n)] char c
  = "atspre_string_test_char_at__intsz"

fun string_test_char_at__intsz {n:nat}
  {i:nat | i <= n} (str: string n, i: size_t i)
  :<> [c:char | (c <> NUL && i < n) || (c == NUL && i >= n)] char c
  = "atspre_string_test_char_at__intsz"

(* ****** ****** *)

fun strbuf_initialize_substring {bsz:int}
  {n:int} {st,ln:nat | st+ln <= n; ln < bsz} {l:addr} (
    pf: !b0ytes bsz @ l >> strbuf (bsz, ln) @ l
  | p: ptr l, str: string n, st: size_t st, ln: size_t ln
  ) : void
  = "atspre_strbuf_initialize_substring"

(* ****** ****** *)

fun string_make_char {n:nat}
  (n: size_t n, c: char):<> string n
  = "atspre_string_make_char"

fun string_make_char__bufptr
  {n:nat} (n: size_t n, c: char)
  :<> [m:nat] [l:addr] strbufptr_gc (m, n, l)
  = "atspre_string_make_char"

(* ****** ****** *)

fun string_make_list_int {n:nat}
  (cs: list (char, n), n: int n):<> string n
  = "atspre_string_make_list_int"

fun string_make_list_int__bufptr
  {n:nat} (cs: list (char, n), n: int n)
  :<> [m:nat] [l:addr] strbufptr_gc (m, n, l)
  = "atspre_string_make_list_int"

(* ****** ****** *)

fun string_make_list_rev_int {n:nat}
  (cs: list (char, n), n: int n):<> string n
  = "atspre_string_make_list_rev_int"

fun string_make_list_rev_int__bufptr
  {n:nat} (cs: list (char, n), n: int n)
  :<> [m:nat] [l:addr] strbufptr_gc (m, n, l)
  = "atspre_string_make_list_rev_int"

(* ****** ****** *)

fun string_make_substring
  {n:int} {st,ln:nat | st + ln <= n}
  (str: string n, st: size_t st, ln: size_t ln)
  :<> string ln
  = "atspre_string_make_substring"

fun string_make_substring__bufptr
  {n:int} {st,ln:nat | st + ln <= n} (
    str: string n, st: size_t st, ln: size_t ln
  ) :<> [m:nat] [l:addr] strbufptr_gc (m, ln, l)
  = "atspre_string_make_substring"

fun string_make_substring__main {v:view}
  {m,n:int} {st,ln:nat | st+ln <= n} {l:addr} (
    pf: !v
  , pf_con: strbuf_v (m, n, l) <= v
  | p: ptr l, st: size_t st, ln: size_t ln
  ) :<> [m:nat] [l:addr] strbufptr_gc (m, ln, l)
  = "atspre_string_make_substring"

(* ****** ****** *)

fun string0_append
  (s1: string, s2: string):<> string
  = "atspre_string_append" 
overload + with string0_append

fun string1_append {i,j:nat}
  (s1: string i, s2: string j):<> string (i+j)
  = "atspre_string_append"
overload + with string1_append

fun string1_append__bufptr {i,j:nat}
  (s1: string i, s2: string j)
  :<> [m:nat] [l:addr] strbufptr_gc (m, i+j, l)
  = "atspre_string_append"

fun string1_append__main
  {v:view} {m1,i:nat} {m2,j:nat} {l1,l2:addr} (
    pf: !v
  , pf1: strbuf (m1,i) <= v
  , pf2: strbuf (m2,j) <= v
  | p1: ptr l1, p2: ptr l2
  ) :<> [m:nat] [l:addr] strbufptr_gc (m, i+j, l)
  = "atspre_string_append"

(* ****** ****** *)

fun string_compare (s1: string, s2: string):<> Sgn
  = "atspre_compare_string_string"

(* ****** ****** *)

fun stringlst_concat (xs: List string):<> string
  = "atspre_stringlst_concat"

fun stringlst_concat__bufptr
  (xs: List string):<> [m,n:nat] [l:addr] strbufptr_gc (m, n, l)
  = "atspre_stringlst_concat"

(* ****** ****** *)

fun string_contains (str: string, c: char):<> bool
  = "atspre_string_contains"

(* ****** ****** *)

fun string_equal (s1: string, s2: string):<> bool
  = "atspre_eq_string_string"

(* ****** ****** *)

fun strbuf_length {m,n:nat} (sbf: &strbuf (m, n)):<> size_t n
  = "atspre_string_length"

fun string0_length (str: string):<> size_t
  = "atspre_string_length"

fun string1_length {n:nat} (str: string n):<> size_t n
  = "atspre_string_length"

symintr string_length
overload string_length with string0_length
overload string_length with string1_length

(* ****** ****** *)

symintr string_is_empty

fun strbuf_is_empty {m,n:nat}
  (sbf: &strbuf (m, n)):<> bool (n==0)
  = "atspre_string_is_empty"

fun string0_is_empty (str: string):<> bool
  = "atspre_string_is_empty"

fun string1_is_empty {n:nat} (str: string n):<> bool (n==0)
  = "atspre_string_is_empty"

overload string_is_empty with string0_is_empty
overload string_is_empty with string1_is_empty

(* ****** ****** *)

symintr string_isnot_empty

fun strbuf_isnot_empty {m,n:nat}
  (sbf: &strbuf (m, n)):<> bool (n > 0)
  = "atspre_string_isnot_empty"

fun string0_isnot_empty (str: string):<> bool
  = "atspre_string_isnot_empty"

fun string1_isnot_empty {n:nat} (str: string n):<> bool (n > 0)
  = "atspre_string_isnot_empty"

overload string_isnot_empty with string0_isnot_empty
overload string_isnot_empty with string1_isnot_empty

(* ****** ****** *)

fun strbuf_is_at_end {m,n,i:nat | i <= n}
  (sbf: &strbuf (m, n), i: size_t i):<> bool (i == n)
  = "atspre_string_is_at_end"

fun string_is_at_end {n,i:nat | i <= n}
  (str: string n, i: size_t i):<> bool (i == n)
  = "atspre_string_is_at_end"

(* ****** ****** *)

fun strbuf_isnot_at_end {m,n,i:nat | i <= n}
  (sbf: &strbuf (m, n), i: size_t i):<> bool (i <> n)
  = "atspre_string_isnot_at_end"

fun string_isnot_at_end {n,i:nat | i <= n}
  (str: string n, i: size_t i):<> bool (i <> n)
  = "atspre_string_isnot_at_end"

(* ****** ****** *)

fun string_explode {n:nat}
  (str: string n):<> list_vt (char, n) = "atspre_string_explode"

(* ****** ****** *)

// alias of [string_make_list]
fun string_implode {n:nat} (cs: list (char, n)):<> string n
  = "atspre_string_implode"

(* ****** ****** *)

// This function is based on [strchr] in [string.h]
// the NULL character at the end of a string is considered in the string

fun string_index_of_char_from_left // locate a character from left
  {n:nat} (str: string n, c: c1har):<> ssizeBtw (~1, n)
  = "atspre_string_index_of_char_from_left"

// This function is based on [strrchr] in [string.h]
// the NULL character at the end of a string is considered in the string

fun string_index_of_char_from_right // locate a character from right
  {n:nat} (str: string n, c: c1har):<> ssizeBtw (~1, n)
  = "atspre_string_index_of_char_from_right"

(* ****** ****** *)

// This function is based on [strstr] in [string.h]
// Note that the NULL character is not compared
fun string_index_of_string // locate a substring from left
  {n1,n2:nat} (s1: string n1, s2: string n2):<> ssizeBtw (~1, n1)
  = "atspre_string_index_of_string"

(* ****** ****** *)

// implemented in [prelude/CATS/string.cats]
fun string_singleton (c: char):<> string 1 = "atspre_string_singleton"

fun string_singleton__bufptr (c: char)
  :<> [m:nat] [l:addr] strbufptr_gc (m, 1, l) = "atspre_string_singleton"

(* ****** ****** *)

// implemented in [prelude/DATS/string.dats]
fun string_foreach__main {v:view} {vt:viewtype} {n:nat} {f:eff}
  (pf: !v | str: string n, f: (!v | c1har, !vt) -<f> void, env: !vt) :<f> void

(* ****** ****** *)

// implemented in [prelude/DATS/string.dats]
fun strbuf_tolower {m,n:nat} (buf: &strbuf (m, n)): void
  = "atspre_strbuf_tolower"

// implemented in [prelude/DATS/string.dats] // note that
fun string_tolower {n:nat} (str: string n):<> string n // a new string is created
  = "atspre_string_tolower"

fun string_tolower__bufptr {v:view} {l:addr}
  (pf: !v, fpf: strbuf_v l <= v | p: ptr l):<> [m,n:nat] [l:addr] strbufptr_gc (m, n, l)
  = "atspre_string_tolower"

(* ****** ****** *)

// implemented in [prelude/DATS/string.dats]
fun strbuf_toupper {m,n:nat} (buf: &strbuf (m, n)): void
  = "atspre_strbuf_toupper"

// implemented in [prelude/DATS/string.dats] // note that
fun string_toupper {n:nat} (str: string n):<> string n // a new string is created
  = "atspre_string_toupper"

fun string_toupper__bufptr {v:view} {l:addr}
  (pf: !v, fpf: strbuf_v l <= v | p: ptr l):<> [m,n:nat] [l:addr] strbufptr_gc (m, n, l)
  = "atspre_string_toupper"

(* ****** ****** *)

// h = (h << 5) + h + c
fun string_hash_33 (str: string):<> uInt = "atspre_string_hash_33"

(* ****** ****** *)

// functions for optional strings

// extval (ats_ptr_type, "0")
val stropt_none : stropt (~1) = "atspre_stropt_none"

castfn stropt_some {n:nat} (str: string n):<> stropt n
  = "atspre_stropt_some"
castfn stropt_unsome {n:nat} (stropt: stropt n):<> string n
  = "atspre_stropt_unsome"

fun stropt_is_none {i:int} (stropt: stropt i):<> bool (i < 0)
  = "atspre_stropt_is_none"
fun stropt_is_some {i:int} (stropt: stropt i):<> bool (i >= 0)
  = "atspre_stropt_is_some"

(* ****** ****** *)

absviewtype stropt_gc (m:int, n:int)

// extval (ats_ptr_type, "0")
val stropt_gc_none : stropt_gc (0,0) = "atspre_stropt_none"

castfn stropt_gc_unnone
  {n:int} (x: stropt_gc (0, n)):<> ptr (null)

castfn stropt_gc_some
  {m,n:nat} {l:addr} (x: strbufptr_gc (m,n,l))
  :<> stropt_gc (m, n)
  = "atspre_stropt_some"

castfn stropt_gc_unsome {m,n:nat | m > 0} // [m > n]
  (x: stropt_gc (m, n)):<> [l:addr] strbufptr_gc (m, n, l)
  = "atspre_stropt_unsome"

fun stropt_gc_is_none
  {m,n:nat} (s: !stropt_gc (m,n)):<> bool (m == 0)
  = "atspre_stropt_is_none"

fun stropt_gc_is_some
  {m,n:nat} (s: !stropt_gc (m,n)):<> bool (m >= 1)
  = "atspre_stropt_is_some"

viewtypedef Stropt_gc = [m,n:nat] stropt_gc (m, n)

(* ****** ****** *)

//
// [tostringf] and [sprintf] are declared in [printf.sats]
//

fun tostringf__bufptr {ts:types}
  (fmt: printf_c ts, arg: ts):<> [m,n:nat][l:addr] strbufptr_gc (m, n, l)
  = "atspre_tostringf"

fun sprintf__bufptr {ts:types}
  (fmt: printf_c ts, arg: ts):<> [m,n:nat][l:addr] strbufptr_gc (m, n, l)
  = "atspre_tostringf"

(* ****** ****** *)

#if VERBOSE_PRELUDE #then

#print "Loading [string.sats] finishes!\n"

#endif

(* end of [string.sats] *)
