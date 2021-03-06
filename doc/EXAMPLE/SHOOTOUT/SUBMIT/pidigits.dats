(*
** The Great Computer Language Shootout
** http://shootout.alioth.debian.org/
**
** contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
**
** compilation command:
**   atscc -O3 -fomit-frame-pointer pidigits.dats -o pidigits -lgmp
*)

staload "libc/SATS/gmp.sats"

fn print_digit (i: int, d: int): void = begin
  print (char_of_int (d + int_of '0')) ;
  if i mod 10 = 0 then printf ("\t:%i\n", @(i))
end // end of [print_digit]

fun g (
    q: &mpz_vt, r: &mpz_vt, t: &mpz_vt
  , k: int, n: &mpz_vt, l: int, i: int, N: int
  ) : void = let
  var x1: mpz_vt? and x2: mpz_vt?
  val () = mpz_init x1 and () = mpz_init x2
  val () = mpz_mul (x1, q, 4) // x1 = 4*q
  val () = mpz_add (x1, r) // x1 = 4*q + r
  val () = mpz_add (x2, n, 1) // x2 = n+1
  val () = mpz_mul (x2, t) // x2 = t * (n+1)
  val cmp = mpz_cmp (x1, x2)
in
  case+ 0 of
  | _ when cmp >= 0 => begin
      mpz_mul (x1, q, 7 * k + 2);
      mpz_mul (x2, r, l);
      mpz_add (x1, x2);
      mpz_mul (t, l);
      mpz_tdiv_q (n, x1, t);
      mpz_mul (x2, q, 2);
      mpz_add (x2, r);
      mpz_mul (r, x2, l);
      mpz_mul (q, k);
      mpz_clear x1; mpz_clear x2;
      g (q, r, t, k+1, n, l+2, i, N)
    end // end of [_ when ...]
  | _ => begin
      print_digit (i, mpz_get_int n);
      mpz_mul (x1, t, n);
      mpz_sub (x2, r, x1);
      mpz_mul (x2, 10);
      mpz_mul (x1, q, 3);
      mpz_add (x1, r);
      mpz_mul (x1, 10);
      mpz_tdiv_q (x1, t);
      mpz_set (r, x2);
      mpz_mul (x2, n, 10);
      mpz_sub (n, x1, x2);
      mpz_mul (q, 10);
      mpz_clear x1; mpz_clear x2;
      if i < N then g (q, r, t, k, n, l, i+1, N)
    end // end of [_]
end // end of [g]

implement main (argc, argv) = let
  var q: mpz_vt and r: mpz_vt and t: mpz_vt and n: mpz_vt
  val () = assert (argc = 2)
  val N = int1_of argv.[1]
  val () = assert_errmsg_bool1
    (N >= 2, "The input integer needs to be a natural number.\n")
in
  mpz_init_set (q, 1);
  mpz_init_set (r, 0);
  mpz_init_set (t, 1);
  mpz_init_set (n, 3);
  g (q, r, t, 1, n, 3, 1, N);
  mpz_clear q; mpz_clear r; mpz_clear t; mpz_clear n;
end // end of [main]

(* end of [pidigits.dats] *)
