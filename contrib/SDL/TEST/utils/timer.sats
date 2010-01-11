//
// LazyFoo-lesson20 _translated_ into ATS
// See http://lazyfoo.net/SDL_tutorials/lesson20
//

(* ****** ****** *)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: January, 2010
//

(* ****** ****** *)

%{#
typedef struct {
  struct {
    ats_int_type startTicks ;
    ats_bool_type started ; 
    ats_int_type pausedTicks ;
    ats_bool_type paused ;
  } private ;
} Timer ;
%} // end of [%{#}

(* ****** ****** *)

staload "contrib/SDL/SATS/SDL.sats"

(* ****** ****** *)

abst@ype Timer_private
typedef Timer =
  $extype_rec "Timer" of { private= Timer_private }
// end of [Timer]

fun Timer_init (obj: &Timer? >> Timer):<> void
fun Timer_start (obj: &Timer): void
fun Timer_stop (obj: &Timer):<> void
fun Timer_pause (obj: &Timer): void
fun Timer_unpause (obj: &Timer): void
fun Timer_getTicks (obj: &Timer): int
fun Timer_is_started (obj: &Timer):<> bool
fun Timer_is_paused (obj: &Timer):<> bool

(* ****** ****** *)

(* end of [LazyFoo-timer.sats] *)
