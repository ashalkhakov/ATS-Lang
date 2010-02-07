//
// ProjectEuler: Problem 3
//

(* ****** ****** *)

//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: February, 2010
//

(* ****** ****** *)

staload "libats/SATS/intinf.sats"

(* ****** ****** *)

absprop DIV (n:int, p:int, q:int) // n div p = q
absprop MOD (n:int, p:int, r:int) // n mod p = r

extern prfun divmod_ft {n:nat}
  {p:pos} {q,r:int} (pf1: DIV (n, p, q), pf2: MOD (n, p, r)): MUL (p, q, n-r)
// end of [divmod_ft]

(* ****** ****** *)

absprop PRIME (n:int) // n is a prime

dataprop por (A: prop, B: prop) =
  | inl (A, B) of A
  | inr (A, B) of B

extern prfun prime_ft1 {n:nat} {p:nat} {n1,n2:nat}
  (pf1: PRIME p, pf2: MOD (n, p, 0), pf3: MUL (n1, n2, n))
  : MOD (p, n1, 0) \por MOD (p, n2, 0)

(* ****** ****** *)

absprop P3aux1 (n:int, p:int)

extern prfun P3aux1_ft1 {n:nat} (pf: P3aux1 (n, 1)): PRIME n
extern prfun P3aux1_ft2 {n:nat}
  {p:int | p >= 2} (pf: P3aux1 (n, p)): (PRIME p, MOD (n, p, 0))
// end of [P3aux1_ft2]
extern prfun P3aux1_ft3 {n:nat}
  {p:int | p >= 2} (pf: P3aux1 (n, p)): [p2:int | p2 <= n] MUL (p, p, p2)
// end of [P3aux1_ft3]

extern fun P3aux1_fun
  {n:nat | n >=2} (n: &intinf n): [p:pos] (P3aux1 (n, p) | int p)
  = "P3aux1_fun"
// end of [P3aux1_fun]

(* ****** ****** *)

extern fun P3aux1_fun_dummy {n:nat} (n: &intinf n): int = "P3aux1_fun"

implement P3aux1_fun_dummy {n} (n) = let
  fun loop {p:pos} (n: &intinf n, p: int p): int =
    if n >= p * p then let
      val (_ | r) = n mod p
    in
      if r = 0 then p else loop (n, p+1)
    end else 1
in
  loop (n, 2)
end // end of [P3aux1_fun_dummy]

(* ****** ****** *)

propdef P3 (n:int, p:int) = (
  () -> [n>=2] void
, PRIME p
, MOD (n, p, 0)
, {p1:nat} (PRIME p1, MOD (n, p1, 0)) -> [p1 <= p] void
) // end of [P3]

extern prfun P3_ft1 {p:int} (pf: PRIME p): P3 (p, p)

extern prfun P3_ft2 {n:nat}
  {n1,n2:int | n1 >= 2; n2 >= 2} {p1,p2:int}
  (pf_mul: MUL (n1, n2, n), pf1: P3 (n1, p1), pf2: P3 (n2, p2))
  : P3 (n, max(p1, p2))
// end of [P3_ft1]

(* ****** ****** *)

extern fun div {n:nat} {p:pos}
  (n: &intinf n, p: int p): [q:int] (DIV (n, p, q) | intinfptr_gc q)
  = "atslib_fdiv_intinf_int"
// end of [div]

fun P3main {n:int | n >= 2} // .<n>.
  (n: &intinf n): [p:int] (P3 (n, p) | int p) = let
  val [p1:int] (pf_P3aux1 | p1) = P3aux1_fun (n)
in
  if p1 >= 2 then let
    val (pf_div | n2obj) = div (n, p1)
    val (pf_n2_gc, pf_n2 | p_n2) = n2obj
    prval (pf_prime, pf_mod) = P3aux1_ft2 (pf_P3aux1)
    prval pf_mul = divmod_ft (pf_div, pf_mod) // n = p1 * n2
    prval pf1_P3 = P3_ft1 (pf_prime)
    prval () = let // proving [n2 >= 2]
      prfun lemma {n:pos} .<n>.
        {x,y:int} {nx,ny:int | nx <= ny}
        (pf1: MUL (n, x, nx), pf2: MUL (n, y, ny)): [x <= y] void = let
        prval pf2 = mul_commute pf2
        prval pf2 = mul_negate pf2
        prval pf2 = mul_commute pf2
        prval pf12 = mul_distribute (pf1, pf2) // MUL (n,x-y) <= 0
      in
        sif (x > y) then let
          prval MULind pf12 = pf12; prval () = mul_nat_nat_nat (pf12)
        in
          // a contradiction is reached here
        end else () // end of [sif]
      end // end of [lemma]
      prval pf1_mul = P3aux1_ft3 (pf_P3aux1) // n >= p1 * p1
    in
      lemma (pf1_mul, pf_mul)
    end // end of [val]
    val (pf2_P3 | p2) = P3main (!p_n2)
    val () = intinf_free (pf_n2_gc, pf_n2 | p_n2)
    prval pf_P3 = P3_ft2 (pf_mul, pf1_P3, pf2_P3)
  in
    (pf_P3 | max (p1, p2))
  end else let
    prval pf_prime = P3aux1_ft1 (pf_P3aux1)
    prval pf_P3 = P3_ft1 (pf_prime)
    val p = intinf_get_int (n)
  in
    (pf_P3 | p)
  end // end of [if]
end // end of [P3main]

(* ****** ****** *)

dynload "libats/DATS/intinf.dats"

(* ****** ****** *)

implement main () = () where {
  val N1 = 13195
  val (pf_N1_gc, pf_N1 | p_N1) = intinf_make (N1)
  val (pf_P3 | p) = P3main (!p_N1)
  val () = intinf_free (pf_N1_gc, pf_N1 | p_N1)
  val () = printf ("The largest prime factor of [%i] is [%i].\n", @(N1,p))
//
  val [n2:int] N2 = lint1_of_lint (600851475143L)
  prval () = __assert () where { extern prfun __assert (): [n2 >= 2] void }
  val (pf_N2_gc, pf_N2 | p_N2) = intinf_make_lint (N2)
  val (pf_P3 | p) = P3main (!p_N2)
  val () = begin
    print ("The largest prime factor of ["); print N2; print "] is ["; print p; print "]."; print_newline ()
  end // end of [val]
//
// (*
  val [_:int] (pf_mul | N3obj) = square (!p_N2)
// *)
  val () = intinf_free (pf_N2_gc, pf_N2 | p_N2)
//
// (*
  // HX-2010-02-06: I added this one
  val (pf_N3_gc, pf_N3 | p_N3) = N3obj
  val N4obj = !p_N3 - 1
  val () = intinf_free (pf_N3_gc, pf_N3 | p_N3)
  val (pf_N4_gc, pf_N4 | p_N4) = N4obj
  val () = assert (!p_N4 >= 2)
  val (pf_P3 | p) = P3main (!p_N4)
  val () = begin
    print ("The largest prime factor of ["); print !p_N4; print "] is ["; print p; print "]."; print_newline ()
  end // end of [val]
  val () = intinf_free (pf_N4_gc, pf_N4 | p_N4)
// *)
} // end of [main]

(* ****** ****** *)

(* end of [problem3.dats] *)
