(*
** The Great Computer Language Shootout
** http://shootout.alioth.debian.org/
**
** contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
**
** compilation command:
**   atscc -fomit-frame-pointer -O3 fannkuch.dats -o fannkuch
*)

staload _(*anonymous*) = "prelude/DATS/array.dats"

macdef iarr (n) = array_make_elt (,(n)+1, 0)
typedef iarr (n:int) = array (natLte n, n+1)

fn iarr_copy {n:nat}
  (A: iarr n, B: iarr n, n: int n): void = let
  var i: intGte 1 = 1 in while (i <= n) (B[i] := A[i]; i := i+1)
end // end of [iarr_copy]

fn print_iarr {n:nat} (A: iarr n, n: int n): void = let
  var i: intGte 1 = 1
in
  while (i <= n) (print A[i]; i := i+1); print_newline ()
end

fun perm_rotate
  {n,i:int | 1 <= i; i <= n} (P: iarr n, i: int i): void = let
  var k: intGte 1 = 1; var k1: int; val P1 = P[1]
  val () = while (k < i) (k1 := k+1; P[k] := P[k1]; k := k1)
in
  P[i] := P1
end

fun perm_next {n,i:int | 1 <= i; i <= n}
  (C: iarr n, P: iarr n, n: int n, i: int i): natLte (n+1) = let
  val x = C[i]; val x1 = x-1; val () = perm_rotate {n,i} (P, i)
in
  case+ 0 of
  | _ when x1 > 0 => (C[i] := x1; i) | _ (* x1 = 0 *) => let
      val () = C[i] := i; val i1 = i + 1
    in
      if i1 <= n then perm_next (C, P, n, i1) else i1
    end
end

fun fannkuch {n:int | n >= 2}
  (C: iarr n, P: iarr n, S: iarr n, n: int n, max: int): int = let
  fun rev {l,u:int | 1 <= l; l <= u+1; u <= n}
    (S: iarr n, l: int l, u: int u): void = if (l < u) then let
      val tmp = S[l] in S[l] := S[u]; S[u] := tmp; rev (S, l+1, u-1)
    end
  var max: int = max; val () =
    if P[1] = 1 then () else
    if P[n] = n then () else let
      var cnt: int = 0; val () = iarr_copy (P, S, n)
      var x: natLte n = S[1]; val () = while (x > 1) begin
        cnt := cnt + 1; rev (S, 1, x); x := S[1]
      end
    in
      if max < cnt then max := cnt
    end
in
  if perm_next (C, P, n, 2) <= n then fannkuch (C, P, S, n, max) else max
end

fun iarr_init {n:nat} (A: iarr n, n: int n): void =
  let var i: intGte 1 = 1 in while (i <= n) (A[i] := i; i := i+1) end

#define NPRINT 30

implement main (argc, argv) = let
  val () = assert (argc >= 2)
  val [n:int] n = int1_of argv.[1]
  val () = assert (n >= 2)
  val C = iarr n; val () = iarr_init (C, n)
  val P = iarr n; val () = iarr_init (P, n)
  val () = if NPRINT > 0 then print_iarr (P, n) else ()
  var times: int = 1; val () = while (times < NPRINT) begin
     perm_next (C, P, n, 2); print_iarr (P, n); times := times + 1
  end
  val () = iarr_init (C, n)
  val () = iarr_init (P, n)
  val S = iarr n; val ans = fannkuch (C, P, S, n, 0)
in
  printf ("Pfannkuchen(%i) = %i\n", @(n, ans))
end

(* end of [fannkuch1.dats] *)
