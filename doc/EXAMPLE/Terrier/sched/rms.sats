(*
** RMS: rate-monotonic scheduler
*)

(* ****** ****** *)
//
// HX: no need for staloading
#define ATS_STALOADFLAG 0 // at run-time
//
(* ****** ****** *)

abst@ype span (i:int) = uint
typedef span = [n:nat] span (n)
typedef span1 = [n:int | n > 0] span (n)

val MAX_SPAN : span1 = "mac#MAX_SPAN"
val ZERO_SPAN : span (0) = "mac#ZERO_SPAN"
val MIN_BUDGET : span1 = "mac#MIN_BUDGET"

fun sub_span_span
  {m,n:int | m >= n} (s1: span m, s2: span n): span (m-n)
overload - with sub_span_span

fun lt_span_span
  {m,n:int} (s1: span m, s2: span n): bool (m < n)
overload < with lt_span_span
fun lte_span_span
  {m,n:int} (s1: span m, s2: span n): bool (m <= n)
overload <= with lte_span_span

fun gt_span_span
  {m,n:int} (s1: span m, s2: span n): bool (m > n)
overload > with gt_span_span
fun gte_span_span
  {m,n:int} (s1: span m, s2: span n): bool (m >= n)
overload >= with gte_span_span

(* ****** ****** *)

abst@ype tick (i:int) = uint
typedef tick = [n:nat] tick (n)
typedef tickGt (n:int) = [n2:int | n2 > n] tick (n2)

fun timer_32k_value (): tick = "mac#timer_32k_value"
fun timer_32k_delay (s: span1): void = "mac#timer_32k_delay"

fun add_tick_span
  {m,n:int} (t1: tick m, s2: span n): span (m+n)
overload + with add_tick_span

fun sub_tick_tick
  {m,n:int | m >= n} (t1: tick m, t2: tick n): span (m-n)
overload - with sub_tick_tick

fun lt_tick_tick
  {m,n:int} (t1: tick m, t2: tick n): bool (m < n)
overload < with lt_tick_tick
macdef is_earlier_than (t1, t2) = lt_tick_tick (,(t1), ,(t2))

(* ****** ****** *)

abst@ype procid (i:int) = int // process id

absviewtype
process (int) = ptr // boxed linear type
viewtypedef process = [i:nat] process (i)
viewtypedef process1 = [i:int | i > 0] process (i)

fun process_get_id {i:int} (p: !process i): procid (i)

fun process_get_period {i:int} (p: !process i): span1
fun process_get_capacity {i:int} (p: !process i): span1

fun process_get_replenish_time {i:int} (p: !process i): tick
fun process_set_replenish_time {i:int} (p: !process i, t: tick): void

fun process_get_budget {i:int} (p: !process i): span
fun process_set_budget {i:int} (p: !process i, x: span): void

fun process_reset_budget {i:int} (p: !process i): void
fun process_reset_replenish_time {i:int} (p: !process i): void

fun process_is_active {i:int} (p: !process i): bool
fun process_recharge_if {i:int} (p: !process i, t0: tick): void

(* ****** ****** *)

(* end of [rms.sats] *)
