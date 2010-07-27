(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
**
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the  terms of the  GNU General Public License as published by the Free
** Software Foundation; either version 2.1, or (at your option) any later
** version.
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

%{#
#include "libc/CATS/gmp.cats"
%} // end of [%{#]

(* ****** ****** *)

// integral numbers

absviewt@ype mpz_viewt0ype = $extype "ats_mpz_viewt0ype"
stadef mpz_vt = mpz_viewt0ype

// rational numbers

absviewt@ype mpq_viewt0ype = $extype "ats_mpq_viewt0ype"
stadef mpq_vt = mpq_viewt0ype

// floating point numbers

absviewt@ype mpf_viewt0ype = $extype "ats_mpf_viewt0ype"
stadef mpf_vt = mpf_viewt0ype

(* ****** ****** *)

// [x] is initialized with 0
fun mpz_init
  (x: &mpz_vt? >> mpz_vt):<> void = "#atslib_mpz_init" // macro!
// end of [mpz_init]

// [x] is initialized with 0 while given [n]-bit space
fun mpz_init2
  (x: &mpz_vt? >> mpz_vt, n: ulint) :<> void = "#atslib_mpz_init2"
// end of [mpz_init2]

// [x] is cleared
fun mpz_clear
  (x: &mpz_vt >> mpz_vt?):<> void = "#atslib_mpz_clear" // macro!
// end of [mpz_clear]

// [x] is reallocated; the original value of [x] is carried over
// if there is enough space or 0 is assigned to [x]
fun mpz_realloc2
  (x: &mpz_vt >> mpz_vt, n: ulint):<> void = "#atslib_mpz_realloc2"
// end of [mpz_realloc2]

(* ****** ****** *)

symintr mpz_get

fun mpz_get_int (x: &mpz_vt):<> int = "#atslib_mpz_get_int"
overload mpz_get with mpz_get_int

fun mpz_get_uint (x: &mpz_vt):<> uint = "#atslib_mpz_get_uint"
overload mpz_get with mpz_get_uint

fun mpz_get_lint (x: &mpz_vt):<> lint = "#atslib_mpz_get_lint"
overload mpz_get with mpz_get_lint

fun mpz_get_ulint (x: &mpz_vt):<> ulint = "#atslib_mpz_get_ulint"
overload mpz_get with mpz_get_ulint

fun mpz_get_double (x: &mpz_vt):<> double = "#atslib_mpz_get_double"
overload mpz_get with mpz_get_double

fun mpz_get_str
  {i:int | 2 <= i; i <= 36} (base: int i, x: &mpz_vt): Strbufptr_gc
  = "atslib_mpz_get_str" // function!
// end of [mpz_get_str]

(* ****** ****** *)

symintr mpz_set

// [x] := [y]
fun mpz_set_mpz
  (x: &mpz_vt, y: &mpz_vt):<> void = "#atslib_mpz_set_mpz"
overload mpz_set with mpz_set_mpz

// [x] := [y]
fun mpz_set_int (x: &mpz_vt, y: int):<> void = "#atslib_mpz_set_int"
overload mpz_set with mpz_set_int

// [x] := [y]
fun mpz_set_uint (x: &mpz_vt, y: uint):<> void = "#atslib_mpz_set_uint"
overload mpz_set with mpz_set_uint

// [x] := [y]
fun mpz_set_lint (x: &mpz_vt, y: lint):<> void = "#atslib_mpz_set_lint"
overload mpz_set with mpz_set_lint

// [x] := [y]
fun mpz_set_ulint (x: &mpz_vt, y: ulint):<> void = "#atslib_mpz_set_ulint"
overload mpz_set with mpz_set_ulint

// [x] := [y]
fun mpz_set_double
  (x: &mpz_vt, y: double):<> void = "#atslib_mpz_set_double"
overload mpz_set with mpz_set_double

// [x] := [y]
fun mpz_set_mpq (x: &mpz_vt, y: &mpq_vt):<> void = "#atslib_mpz_set_mpq"
overload mpz_set with mpz_set_mpq

// [x] := [y]
fun mpz_set_mpf (x: &mpz_vt, y: &mpf_vt):<> void = "#atslib_mpz_set_mpf"
overload mpz_set with mpz_set_mpf

// the function returns 0 if the string is valid, or -1 otherwise.
fun mpz_set_str_err
  {i:int | 2 <= i; i <= 62} (x: &mpz_vt, s: string, base: int i):<> int
  = "#atslib_mpz_set_str_err" // macro
// end of [mpz_set_str_err]

fun mpz_set_str_exn
  {i:int | 2 <= i; i <= 62} (x: &mpz_vt, s: string, base: int i):<> void
  = "atslib_mpz_set_str_exn" // function!
// end of [mpz_set_str_exn]

(* ****** ****** *)

symintr mpz_init_set

// [x] := [y]
fun mpz_init_set_mpz (x: &mpz_vt? >> mpz_vt, y: &mpz_vt):<> void
  = "#atslib_mpz_init_set_mpz"
overload mpz_init_set with mpz_init_set_mpz

// [x] := [y]
fun mpz_init_set_int (x: &mpz_vt? >> mpz_vt, y: int):<> void
  = "#atslib_mpz_init_set_int"
overload mpz_init_set with mpz_init_set_int

// [x] := [y]
fun mpz_init_set_uint (x: &mpz_vt? >> mpz_vt, y: uint):<> void
  = "#atslib_mpz_init_set_uint"
overload mpz_init_set with mpz_init_set_uint

// [x] := [y]
fun mpz_init_set_lint (x: &mpz_vt? >> mpz_vt, y: lint):<> void
  = "#atslib_mpz_init_set_lint"
overload mpz_init_set with mpz_init_set_lint

// [x] := [y]
fun mpz_init_set_ulint (x: &mpz_vt? >> mpz_vt, y: ulint):<> void
  = "#atslib_mpz_init_set_ulint"
overload mpz_init_set with mpz_init_set_ulint

// [x] := [y]
fun mpz_init_set_double (x: &mpz_vt? >> mpz_vt, y: double):<> void
  = "#atslib_mpz_init_set_double"
overload mpz_init_set with mpz_init_set_double

// [x] := [y]
fun mpz_init_set_mpq (x: &mpz_vt? >> mpz_vt, y: &mpq_vt):<> void
  = "atslib_mpz_init_set_mpq" // function!
overload mpz_init_set with mpz_init_set_mpq

// [x] := [y]
fun mpz_init_set_mpf (x: &mpz_vt? >> mpz_vt, y: &mpf_vt):<> void
  = "atslib_mpz_init_set_mpf" // function!
overload mpz_init_set with mpz_init_set_mpf

// the function returns 0 if the string is valid, or -1 otherwise.
fun mpz_init_set_str_err
  {i:int | 2 <= i; i <= 62} (x: &mpz_vt? >> mpz_vt, s: string, base: int i):<> int
  = "atslib_mpz_init_set_str_err" // function!

// the function exits the string is invalid.
fun mpz_init_set_str_exn
  {i:int | 2 <= i; i <= 62} (x: &mpz_vt? >> mpz_vt, s: string, base: int i):<> void
  = "atslib_mpz_init_set_str_exn" // function!

(* ****** ****** *)

#define sixtythree 63
fun mpz_out_str_err {m:file_mode} (
    pf_mode: file_mode_lte (m, w)
  | file: &FILE m, base: intBtw (2, sixtythree), x: &mpz_vt
  ) : int = "#atslib_mpz_out_str_err"
// end of [mpz_out_str_err]

fun mpz_out_str_exn {m:file_mode} (
    pf_mode: file_mode_lte (m, w)
  | file: &FILE m, base: intBtw (2, sixtythree), x: &mpz_vt
  ) : void = "atslib_mpz_out_str_exn"
// end of [mpz_out_str_exn]

(* ****** ****** *)

// negation

symintr mpz_neg

// [x] := -[y]
fun mpz_neg_2 (x: &mpz_vt, y: &mpz_vt):<> void = "#atslib_mpz_neg_2"
overload mpz_neg with mpz_neg_2

// [x] := -[x]
fun mpz_neg_1 (x: &mpz_vt):<> void = "atslib_mpz_neg_1" // function!
overload mpz_neg with mpz_neg_1

// absolute value

symintr mpz_abs

// [x] := | [y] |
fun mpz_abs_2 (x: &mpz_vt, y: &mpz_vt):<> void = "#atslib_mpz_abs_2"
overload mpz_abs with mpz_abs_2

// [x] := | [x] |
fun mpz_abs_1 (x: &mpz_vt):<> void = "atslib_mpz_abs_1" // function!
overload mpz_abs with mpz_abs_1

// addition

symintr mpz_add

// [x] := [y] + [z]
fun mpz_add_mpz_3
  (x: &mpz_vt, y: &mpz_vt, z: &mpz_vt):<> void = "#atslib_mpz_add_mpz_3"
overload mpz_add with mpz_add_mpz_3

fun mpz_add_int_3
  (x: &mpz_vt, y: &mpz_vt, z: int):<> void = "atslib_mpz_add_int_3" // fun!
overload mpz_add with mpz_add_int_3

fun mpz_add_uint_3
  (x: &mpz_vt, y: &mpz_vt, z: uint):<> void = "#atslib_mpz_add_uint_3"
overload mpz_add with mpz_add_uint_3

fun mpz_add_lint_3
  (x: &mpz_vt, y: &mpz_vt, z: lint):<> void = "atslib_mpz_add_lint_3" // fun!
overload mpz_add with mpz_add_lint_3

fun mpz_add_ulint_3
  (x: &mpz_vt, y: &mpz_vt, z: ulint):<> void = "#atslib_mpz_add_ulint_3"
overload mpz_add with mpz_add_ulint_3

// [x] := [x] + [y]
fun mpz_add_mpz_2
  (x: &mpz_vt, y: &mpz_vt):<> void = "atslib_mpz_add_mpz_2"
overload mpz_add with mpz_add_mpz_2

fun mpz_add_int_2
  (x: &mpz_vt, y: int):<> void = "atslib_mpz_add_int_2"
overload mpz_add with mpz_add_int_2

fun mpz_add_uint_2
  (x: &mpz_vt, y: uint):<> void = "atslib_mpz_add_uint_2"
overload mpz_add with mpz_add_uint_2  

fun mpz_add_lint_2
  (x: &mpz_vt, y: lint):<> void = "atslib_mpz_add_lint_2"
overload mpz_add with mpz_add_lint_2

fun mpz_add_ulint_2
  (x: &mpz_vt, y: ulint):<> void = "atslib_mpz_add_ulint_2"
overload mpz_add with mpz_add_ulint_2

// subtraction

symintr mpz_sub

// [x] := [y] - [z]
fun mpz_sub_mpz_3
  (x: &mpz_vt, y: &mpz_vt, z: &mpz_vt):<> void = "#atslib_mpz_sub_mpz_3"
overload mpz_sub with mpz_sub_mpz_3  

fun mpz_sub_int_3
  (x: &mpz_vt, y: &mpz_vt, z: int):<> void = "atslib_mpz_sub_int_3" // fun!
overload mpz_sub with mpz_sub_int_3

fun mpz_sub_uint_3
  (x: &mpz_vt, y: &mpz_vt, z: uint):<> void = "#atslib_mpz_sub_uint_3"
overload mpz_sub with mpz_sub_uint_3

fun mpz_sub_lint_3
  (x: &mpz_vt, y: &mpz_vt, z: lint):<> void = "atslib_mpz_sub_lint_3" // fun!
overload mpz_sub with mpz_sub_lint_3  

fun mpz_sub_ulint_3
  (x: &mpz_vt, y: &mpz_vt, z: ulint):<> void = "#atslib_mpz_sub_ulint_3"
overload mpz_sub with mpz_sub_ulint_3  

//

// [x] := [x] - [y]
fun mpz_sub_mpz_2
  (x: &mpz_vt, y: &mpz_vt):<> void = "atslib_mpz_sub_mpz_2"
overload mpz_sub with mpz_sub_mpz_2

fun mpz_sub_int_2
  (x: &mpz_vt, y: int):<> void = "atslib_mpz_sub_int_2"
overload mpz_sub with mpz_sub_int_2

fun mpz_sub_uint_2
  (x: &mpz_vt, y: uint):<> void = "atslib_mpz_sub_uint_2"
overload mpz_sub with mpz_sub_uint_2

fun mpz_sub_lint_2
  (x: &mpz_vt, y: lint):<> void = "atslib_mpz_sub_lint_2"
overload mpz_sub with mpz_sub_lint_2

fun mpz_sub_ulint_2
  (x: &mpz_vt, y: ulint):<> void = "atslib_mpz_sub_ulint_2"
overload mpz_sub with mpz_sub_ulint_2

(* ****** ****** *)

// multiplication

symintr mpz_mul

//

// [x] := [y] * [z]
fun mpz_mul_mpz_3
  (x: &mpz_vt, y: &mpz_vt, z: &mpz_vt):<> void = "#atslib_mpz_mul_mpz_3"
overload mpz_mul with mpz_mul_mpz_3

fun mpz_mul_int_3
  (x: &mpz_vt, y: &mpz_vt, z: int):<> void = "#atslib_mpz_mul_int_3"
overload mpz_mul with mpz_mul_int_3

fun mpz_mul_uint_3
  (x: &mpz_vt, y: &mpz_vt, z: uint):<> void = "#atslib_mpz_mul_uint_3"
overload mpz_mul with mpz_mul_uint_3

fun mpz_mul_lint_3
  (x: &mpz_vt, y: &mpz_vt, z: lint):<> void = "#atslib_mpz_mul_lint_3"
overload mpz_mul with mpz_mul_lint_3

fun mpz_mul_ulint_3
  (x: &mpz_vt, y: &mpz_vt, z: ulint):<> void = "#atslib_mpz_mul_ulint_3"
overload mpz_mul with mpz_mul_ulint_3

//

// [x] := [x] * [y]
fun mpz_mul_mpz_2
  (x: &mpz_vt, y: &mpz_vt):<> void = "atslib_mpz_mul_mpz_2"
overload mpz_mul with mpz_mul_mpz_2

fun mpz_mul_int_2 (x: &mpz_vt, y: int):<> void = "atslib_mpz_mul_int_2"
overload mpz_mul with mpz_mul_int_2

fun mpz_mul_uint_2 (x: &mpz_vt, y: uint):<> void = "atslib_mpz_mul_uint_2"
overload mpz_mul with mpz_mul_uint_2

fun mpz_mul_lint_2 (x: &mpz_vt, y: lint):<> void = "atslib_mpz_mul_lint_2"
overload mpz_mul with mpz_mul_lint_2

fun mpz_mul_ulint_2 (x: &mpz_vt, y: ulint):<> void = "atslib_mpz_mul_ulint_2"
overload mpz_mul with mpz_mul_ulint_2

// [x] := [x] * [x]
fun mpz_mul_mpz_1 (x: &mpz_vt):<> void = "atslib_mpz_mul_mpz_1"
overload mpz_mul with mpz_mul_mpz_1

(* ****** ****** *)

(*
** Function: mpz_mul_2exp
** Input: arg1, arg2
** Output: res
** Return: void
** Description: Set res so that res = arg1 * (2 ^ arg2)
** Remarks: The same object can be passed for both res and arg1.
** Others:
**   It's up to an application to call functions like mpz_mul_2exp when appropriate.
**   General purpose functions like mpz_mul make no attempt to identify powers of two
**   or other special forms.
*)
fun mpz_mul_2exp
  (res: &mpz_vt, arg1: &mpz_vt, arg2: int):<> void = "atslib_mpz_mul_2exp"
// end of [mpz_mul_2exp]

(* ****** ****** *)
//
// truncate division
//
symintr mpz_tdiv_qr

// (q, r) = n / d
fun mpz_tdiv_qr_mpz_4
  (q: &mpz_vt, r: &mpz_vt, n: &mpz_vt, d: &mpz_vt):<> void = "#atslib_mpz_tdiv_qr_mpz_4"
// end of [mpz_tdiv_qr_mpz_4]
overload mpz_tdiv_qr with mpz_tdiv_qr_mpz_4

// (q, r) = n / d
fun mpz_tdiv_qr_ulint_4
  (q: &mpz_vt, r: &mpz_vt, n: &mpz_vt, d: ulint):<> void = "#atslib_mpz_tdiv_qr_ulint_4"
// end of [mpz_tdiv_qr_ulint_4]
overload mpz_tdiv_qr with mpz_tdiv_qr_ulint_4

//

symintr mpz_tdiv_q

// [q] := [n] / [d]
fun mpz_tdiv_q_mpz_3
  (q: &mpz_vt, n: &mpz_vt, d: &mpz_vt):<> void = "#atslib_mpz_tdiv_q_mpz_3"
overload mpz_tdiv_q with mpz_tdiv_q_mpz_3

// [q] := [n] / [d]
fun mpz_tdiv_q_ulint_3
  (q: &mpz_vt, n: &mpz_vt, d: ulint):<> void = "#atslib_mpz_tdiv_q_ulint_3"
overload mpz_tdiv_q with mpz_tdiv_q_ulint_3

// [q] := [q] / [d]
fun mpz_tdiv_q_mpz_2
  (q: &mpz_vt, d: &mpz_vt):<> void = "atslib_mpz_tdiv_q_mpz_2"
overload mpz_tdiv_q with mpz_tdiv_q_mpz_2

// [q] := [q] / [d]
fun mpz_tdiv_q_ulint_2
  (q: &mpz_vt, d: ulint):<> void = "atslib_mpz_tdiv_q_ulint_2"
overload mpz_tdiv_q with mpz_tdiv_q_ulint_2

(* ****** ****** *)
//
// floor division
//
(*
** Function: mpz_fdiv_qr
** Input: dividend, divisor
** Output: quot, rem
** Return: void
** Description:
**   Set quot and rem so that dividend = quot * divisor + rem
**   Rounds quot down towards negative infinity, and rem will
**   have the same sign as divisor, and 0 <= |rem| < |divisor|.
**   'f' stands for "floor". e.g. 5 = (-2) * (-3) + (-1); -5 = 1 * (-3) + (-2)
** Remarks:
**   The same object cannot be passed for both quot and rem, or the result will be
**   unpredictable. No other constraints on the pass of other arguments, e.g. the same
**   object can be passed to both quot and dividend.
*)

//

symintr mpz_fdiv_qr

fun mpz_fdiv_qr_mpz_4
  (quot: &mpz_vt, rem: &mpz_vt, dividend: &mpz_vt, divisor: &mpz_vt):<> void
  = "#atslib_mpz_fdiv_qr_mpz_4"
overload mpz_fdiv_qr with mpz_fdiv_qr_mpz_4

fun mpz_fdiv_qr_ulint_4
  (quot: &mpz_vt, rem: &mpz_vt, dividend: &mpz_vt, divisor: ulint):<> void
  = "#atslib_mpz_fdiv_qr_ulint_4"
overload mpz_fdiv_qr with mpz_fdiv_qr_ulint_4

//

symintr mpz_fdiv_q

// [q] := [n] / [d]
fun mpz_fdiv_q_mpz_3
  (q: &mpz_vt, n: &mpz_vt, d: &mpz_vt):<> void = "#atslib_mpz_fdiv_q_mpz_3"
overload mpz_fdiv_q with mpz_fdiv_q_mpz_3

// [q] := [n] / [d]
fun mpz_fdiv_q_ulint_3
  (q: &mpz_vt, n: &mpz_vt, d: ulint):<> void = "#atslib_mpz_fdiv_q_ulint_3"
overload mpz_fdiv_q with mpz_fdiv_q_ulint_3

// [q] := [q] / [d]
fun mpz_fdiv_q_mpz_2
  (q: &mpz_vt, d: &mpz_vt):<> void = "atslib_mpz_fdiv_q_mpz_2"
overload mpz_fdiv_q with mpz_fdiv_q_mpz_2

// [q] := [q] / [d]
fun mpz_fdiv_q_ulint_2
  (q: &mpz_vt, d: ulint):<> void = "atslib_mpz_fdiv_q_ulint_2"
overload mpz_fdiv_q with mpz_fdiv_q_ulint_2

(* ****** ****** *)

symintr mpz_mod

fun mpz_mod_mpz_3
  (r: &mpz_vt, n: &mpz_vt, d: &mpz_vt):<> void = "#atslib_mpz_mod_mpz_3"
overload mpz_mod with mpz_mod_mpz_3

fun mpz_mod_ulint_3
  (r: &mpz_vt, n: &mpz_vt, d: ulint):<> void = "#atslib_mpz_mod_ulint_3"
overload mpz_mod with mpz_mod_ulint_3

(* ****** ****** *)

// add/mul combination

symintr mpz_addmul

fun mpz_addmul_mpz_3
  (x: &mpz_vt, y: &mpz_vt, z: &mpz_vt):<> void = "#atslib_mpz_addmul_mpz_3"
overload mpz_addmul with mpz_addmul_mpz_3

fun mpz_addmul_uint_3
  (x: &mpz_vt, y: &mpz_vt, z: uint):<> void = "#atslib_mpz_addmul_uint_3"
overload mpz_addmul with mpz_addmul_uint_3

// sub/mul combination

symintr mpz_submul

fun mpz_submul_mpz_3
  (x: &mpz_vt, y: &mpz_vt, z: &mpz_vt):<> void = "#atslib_mpz_submul_mpz_3"
overload mpz_submul with mpz_submul_mpz_3

fun mpz_submul_uint_3
  (x: &mpz_vt, y: &mpz_vt, z: uint):<> void = "#atslib_mpz_submul_uint_3"
overload mpz_submul with mpz_submul_uint_3

(* ****** ****** *)

symintr mpz_cmp

fun mpz_cmp_mpz (x: &mpz_vt, y: &mpz_vt):<> Sgn = "#atslib_mpz_cmp_mpz"
overload mpz_cmp with mpz_cmp_mpz

fun mpz_cmp_int (x: &mpz_vt, y: int):<> Sgn = "#atslib_mpz_cmp_int"
overload mpz_cmp with mpz_cmp_int

fun mpz_cmp_uint (x: &mpz_vt, y: uint):<> Sgn = "#atslib_mpz_cmp_uint"
overload mpz_cmp with mpz_cmp_uint

fun mpz_cmp_lint (x: &mpz_vt, y: lint):<> Sgn = "#atslib_mpz_cmp_lint"
overload mpz_cmp with mpz_cmp_lint

fun mpz_cmp_ulint (x: &mpz_vt, y: ulint):<> Sgn = "#atslib_mpz_cmp_ulint"
overload mpz_cmp with mpz_cmp_ulint

fun mpz_sgn (x: &mpz_vt):<> Sgn = "atslib_mpz_sgn" // function!

(* ****** ****** *)

fun fprint0_mpz
  (out: FILEref, x: &mpz_vt):<!exnref> void = "atslib_fprint_mpz"
overload fprint with fprint0_mpz

fun fprint1_mpz {m:file_mode}
  (pf: file_mode_lte (m, w) | out: &FILE m, x: &mpz_vt):<!exnref> void
  = "atslib_fprint_mpz"
overload fprint with fprint1_mpz

fun print_mpz (x: &mpz_vt) :<!ref> void = "atslib_print_mpz"
overload print with print_mpz

fun prerr_mpz (x: &mpz_vt) :<!ref> void = "atslib_prerr_mpz"
overload prerr with prerr_mpz

fun tostring_mpz (x: &mpz_vt):<> String = "atslib_tostring_mpz"
overload tostring with tostring_mpz

(* ****** ****** *)

(* end of [gmp.sats] *)
