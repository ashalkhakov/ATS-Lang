(*
** RMS: rate-monotonic scheduler
*)

(* ****** ****** *)
//
// HX: no need for staloading
#define ATS_DYNLOADFLAG 0 // at run-time
//
(* ****** ****** *)

staload "rms.sats"

(* ****** ****** *)

assume span (i:int) = uint (i)
assume tick (i:int) = uint (i)

(* ****** ****** *)

implement
sub_span_span (m, n) = m usub n

implement sub_tick_tick (m, n) = m usub n
implement add_tick_span (m, n) = m uadd n

(* ****** ****** *)

implement lt_span_span (m, n) = m ult n
implement lt_tick_tick (m, n) = m ult n

(* ****** ****** *)

(* end of [rms_misc.dats] *)
