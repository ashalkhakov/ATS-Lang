(*
** RMS: rate-monotonic scheduler
*)

(* ****** ****** *)

(*
** It is primarily based on an implementation by Matthew Danish
*)

(* ****** ****** *)
//
// HX: no need for staloading
#define ATS_DYNLOADFLAG 0 // at run-time
//
(* ****** ****** *)

staload "rms.sats"

(* ****** ****** *)

extern fun update_prev_sched (): span

(* ****** ****** *)

extern fun get_prev_timer_val (): span
extern fun set_prev_timer_val (s: span): void

(* ****** ****** *)

implement
process_recharge_if (p, t0) = let
  val R = process_get_replenish_time (p)
in
//
if R <= t0 then let
  val () = process_reset_budget (p)
  val () = process_reset_replenish_time (p)
in
  // nothing
end else () // end of [if]
//
end // end of [process_recharge_if]

(* ****** ****** *)

absviewtype
pqueue (n:int) = ptr // boxed linear type
viewtypedef pqueue = [n:nat] pqueue (n)

(* ****** ****** *)

extern
praxi lemma_pqueue_param {n:int} (pq: !pqueue n): [n>=0] void

(* ****** ****** *)

extern
fun pqueue_is_empty
  {n:int}
  (pq: !pqueue n): bool (n==0) = "mac#pqueue_is_empty"
// end of [pqueue_is_empty]

extern
fun pqueue_isnot_empty
  {n:int}
  (pq: !pqueue n): bool (n > 0) = "mac#pqueue_isnot_empty"
// end of [pqueue_isnot_empty]

(* ****** ****** *)

extern
praxi process_return (p: process): void

(* ****** ****** *)

extern
fun get_current (): process = "mac#get_current"

extern
fun get_process (id: int): process1 = "mac#get_process"
extern
fun sched_wakeup (proc: process1): void = "ext#sched_wakeup"
extern
fun waitqueue_append
  {n:int} (pq: !pqueue n >> pqueue (n+1), p: process1): void = "mac#waitqueue_append"
// end of [waitqueue_append]

(* ****** ****** *)

absviewt@ype
prociter (n:int) = int // unboxed linear type
stadef prociter = [n:nat] prociter (n)

extern
fun make_prociter (): prociter = "mac#make_process_iter"
extern
fun prociter_get_next
  {n:pos} (pi: &prociter (n) >> prociter (n-1)): process1
// end of [prociter_get_next]

extern
fun prociter_has_next {n:int} (pi: !prociter (n)): bool (n > 0)

extern
fun prociter_done {n:int} (pi: prociter (n)): void

(* ****** ****** *)

absview must_set_timer_v

extern
fun pvttimer_set
  (pf: must_set_timer_v | s: span): void
// end of [pvttimer_set]

(* ****** ****** *)

extern
fun schedule (pf: must_set_timer_v | (*void*)): void

local

(*
//
// This is too messy!
//
fun loop_fin_0
  {t0:int} (
  pf_mstv: must_set_timer_v | t_now: tick (t0), next_r: tickGt (t0)
) : void = let
  val span = next_r - t_now
in
  if span >= MIN_BUDGET then {
    val () = set_prev_timer_val (span)
    val () = pvttimer_set (pf_mstv | span)
    val () = do_idle ()
  } else {
    val () = set_prev_timer_val (span)
    val () = timer_32k_delay (span)
    val () = schedule (pf_mstv | (*void*))
  }
end // end of [loop_fin_0]

fun loop_fin_1
  {t0:int} (
  pf_mstv: must_set_timer_v | t_now: tick (t0), next_i: int
) : void = let
  val p_next = get_process (next_i)
  val b_next = process_get_budget (p_next)
  val t_next = process_get_period (p_next)
in
end // end of [loop_fin_1]
*)

in

implement
schedule
  (pf_mstv | (*void*)) = let

val prev_span = update_prev_sched ()
val p_current = get_current ()
val b_current = process_get_budget (p_current)
val b_current = (
  if (b_current <= prev_span) then ZERO_SPAN else (b_current - prev_span)
) : span // end of [val]
val b_current = (
  if (b_current <= MIN_BUDGET) then ZERO_SPAN else b_current
) : span // end of [val]
val () = process_set_budget (p_current, b_current)
prval () = process_return (p_current)

val [t0:int] t_now = timer_32k_value ()

in

end // end of [schedule]

end // end of [local]

(* ****** ****** *)

(* end of [rms.dats] *)
