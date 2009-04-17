(*
**
** a Tiger compiler written in ATS
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Time: Spring, 2009
**
*)

(* ****** ****** *)

staload TL = "templab.sats"

(* ****** ****** *)

abstype tempset_t

(* ****** ****** *)

fun tempset_nil (): tempset_t

fun tempset_make_templst (ts: $TL.templst): tempset_t

(* ****** ****** *)

fun fprint_tempset (out: FILEref, ts: tempset_t): void

(* ****** ****** *)

fun tempset_ismem (ts: tempset_t, t: $TL.temp_t): bool

(* ****** ****** *)

fun tempset_add
  (ts: tempset_t, t: $TL.temp_t): tempset_t

fun tempset_add_flag
  (ts: tempset_t, t: $TL.temp_t, flag: &int): tempset_t

(* ****** ****** *)

fun tempset_union
  (ts1: tempset_t, ts2: tempset_t): tempset_t

fun tempset_union_flag
  (ts1: tempset_t, ts2: tempset_t, flag: &int): tempset_t

(* ****** ****** *)

fun tempset_diff
  (ts1: tempset_t, ts2: tempset_t): tempset_t

(* ****** ****** *)

(* end of [temp.sats] *)
