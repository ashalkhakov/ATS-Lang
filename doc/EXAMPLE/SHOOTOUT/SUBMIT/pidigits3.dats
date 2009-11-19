(*
** The Great Computer Language Shootout
** http://shootout.alioth.debian.org/
**
** contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
**
** This code is a direct translation from a C submission by
** Sean Bartell, which is based on the Scheme PLT #4 version
**
** compilation command:
**   atscc -O3 -fomit-frame-pointer pidigits3.dats -o pidigits3 -lgmp
*)

(* ****** ****** *)

staload "libc/SATS/gmp.sats"

(* ****** ****** *)

var numer: mpz_vt
viewdef v_numer = mpz_vt @ numer
val () = mpz_init_set_uint (numer, 1U)

var denom: mpz_vt
viewdef v_denom = mpz_vt @ denom
val () = mpz_init_set_uint (denom, 1U)

var accum: mpz_vt
viewdef v_accum = mpz_vt @ accum
val () = mpz_init_set_uint (accum, 0U)

var tmp1: mpz_vt
viewdef v_tmp1 = mpz_vt @ tmp1
val () = mpz_init (tmp1)

var tmp2: mpz_vt
viewdef v_tmp2 = mpz_vt @ tmp2
val () = mpz_init (tmp2)

(* ****** ****** *)

viewdef v_all = @(v_numer, v_denom, v_accum, v_tmp1, v_tmp2)
prval pf_all = @(
  view@ numer, view@ denom, view@ accum, view@ tmp1, view@ tmp2
)
prval pfbox_all =
  vbox_make {v_all} (pf_all) where {
  extern prfun vbox_make {v:view} (pf: v): vbox (v)
} // end of [val]

(* ****** ****** *)

extern
fun mpz_mul_2exp (_: &mpz_vt, _: &mpz_vt, _: int): void
  = "atslib_mpz_mul_2exp"

extern
fun mpz_fdiv_qr (_: &mpz_vt, _: &mpz_vt, _: &mpz_vt, _: &mpz_vt): void
  = "atslib_mpz_fdiv_qr"

%{^

static inline
ats_void_type
atslib_mpz_mul_2exp (
  ats_mpz_ptr_type x
, ats_mpz_ptr_type y
, ats_int_type n
) {
  mpz_mul_2exp((mpz_ptr)x, (mpz_ptr)y, n) ; return ;
} // end of [atslib_mpz_mul_2exp]

static inline
ats_void_type
atslib_mpz_fdiv_qr (
  ats_mpz_ptr_type x
, ats_mpz_ptr_type y
, ats_mpz_ptr_type u
, ats_mpz_ptr_type v
) {
  mpz_fdiv_qr((mpz_ptr)x, (mpz_ptr)y, (mpz_ptr)u, (mpz_ptr)v) ; return ;
} // end of [atslib_mpz_fdiv_qr]

%} // end of [%{^]

(* ****** ****** *)

fn extract_digit (
    pf_numer: !v_numer
  , pf_denom: !v_denom
  , pf_accum: !v_accum
  , pf_tmp1: !v_tmp1
  , pf_tmp2: !v_tmp2
  | (*none*)
  ) : int = let
  val sgn = mpz_cmp (numer, accum)
in
  case+ 0 of
  | _ when sgn > 0 => ~1
  | _ => let
      val () = mpz_mul_2exp (tmp1, numer, 1)
      val () = mpz_add (tmp1, numer)
      val () = mpz_add (tmp1, accum)
      val [l:addr] (pf_tmp11 | p_tmp11) = __cast (tmp1) where {
        extern castfn __cast (_: &mpz_vt):<> [l:addr] (mpz_vt @ l | ptr l)
      }
      val () = mpz_fdiv_qr (tmp1, tmp2, !p_tmp11, denom)
      prval _ = __absorb (pf_tmp11) where {
        extern prfun __absorb (pf: mpz_vt @ l): void
      }
      val () = mpz_add (tmp2, numer)
    in
      if mpz_cmp (tmp2, denom) >= 0 then ~1 else mpz_get_int (tmp1)
    end // end of [_]
end // end of [extract]

(* ****** ****** *)

fn next_term (
    pf_numer: !v_numer
  , pf_denom: !v_denom
  , pf_accum: !v_accum
  , pf_tmp1: !v_tmp1
  , pf_tmp2: !v_tmp2
  | k: uint
  ) : void = let
(*
  val () = (print "next_term: k = "; print k; print_newline ())
*)
  val y2 = 2U * k + 1U
  val () = mpz_mul_2exp (tmp1, numer, 1)
  val () = mpz_add (accum, tmp1)
  val () = mpz_mul (accum, y2)
  val () = mpz_mul (numer, k)
  val () = mpz_mul (denom, y2)
in
  // nothing
end // end of [next_term] 

(* ****** ****** *)

fn eliminate_digit (
    pf_numer: !v_numer
  , pf_denom: !v_denom
  , pf_accum: !v_accum
  | d: uint
  ) : void = () where {
  val () = begin
    mpz_submul (accum, denom, d); mpz_mul (accum, 10); mpz_mul (numer, 10)
  end // end of [val]
} // end of [eliminate_digit]

(* ****** ****** *)

staload "libc/SATS/stdio.sats"

fn pidigits (
    pf_numer: !v_numer
  , pf_denom: !v_denom
  , pf_accum: !v_accum
  , pf_tmp1: !v_tmp1
  , pf_tmp2: !v_tmp2
  | n: int
  ) : void = () where {
  var d: int?
  var i: int = 0 and k: uint = 0U and m: int?
  val () = while (true) let
    val () = d := ~1
    val () = while* (d: int) => (d = ~1) begin
      k := k+1U;
      next_term (pf_numer, pf_denom, pf_accum, pf_tmp1, pf_tmp2 | k);
      d := extract_digit (pf_numer, pf_denom, pf_accum, pf_tmp1, pf_tmp2 | (*none*));
    end // end of [while]
    val _ = fputc0_err (char_of_int (int_of '0' + d), stdout_ref)
    val () = i := i+1;
    val () = m := i mod 10;
    val () = if (m = 0) then fprintf (stdout_ref, "\t:%d\n", @(i));
    val () = if (i >= n) then break;
(*
    val () = (print "numer = "; print numer; print_newline ())
*)
    val () = eliminate_digit (pf_numer, pf_denom, pf_accum | uint_of_int d);
  in
    // nothing
  end // end of [while]
} // end of [pidigits]

(* ****** ****** *)

implement main (argc, argv) = let
  val n = (if argc > 1 then int_of_string (argv.[1]) else 27): int
  prval vbox pf_all = pfbox_all
in
  $effmask_ref (pidigits (pf_all.0, pf_all.1, pf_all.2, pf_all.3, pf_all.4 | n))
end // end of [main]

(* ****** ****** *)

(* end of [pidigits3.dats] *)
