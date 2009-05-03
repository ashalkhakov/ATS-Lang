(*
** This example is taken from Appel's book:
** Modern Compiler Design and Implementation in ML
*)

(* A program to solve the 8-queens problem *)

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/array.dats"
staload _(*anonymous*) = "prelude/DATS/reference.dats"

(* ****** ****** *)

#define N 8
#define N1 (N - 1)

(* ****** ****** *)

val NSOL = ref_make_elt<Nat> (0)

(* ****** ****** *)

val row = array_make_elt<int> (N, 0)
val col = array_make_elt<int> (N, 0)
val diag1 = array_make_elt<int> (size1_of_int1 N+N1, 0)
val diag2 = array_make_elt<int> (size1_of_int1 N+N1, 0)

(* ****** ****** *)

fn printboard (): void = let
  var i: natLte N and j: natLte N
  val () = for* (j: int?) =>
    (i := 0; i < N; i := i + 1) let
    val () = for* (i: natLt N) =>
      (j := 0; j < N; j := j + 1) begin
      print_string (if :(j: natLt N) => (col[i] = j) then " Q" else " .")
    end // end of [val]
  in
    print_newline ()
  end // end of [for]
  val () = print_newline ()
in
  // empty
end (* end of [printboard] *)

(* ****** ****** *)

fun _try (c: natLte N): void =
  if (c = N) then begin
    !NSOL := !NSOL + 1; printboard ()
  end else let
    var r: natLte N // unitialized
    val () = for (r := 0; r < N; r := r+1) (
      if :(r: natLt N) => (row[r] = 0) then begin
        if :(r: natLt N) => (diag1[r+c] = 0) then begin
          if :(r: natLt N) => (diag2[r+N1-c] = 0) then begin
            row[r] := 1; diag1[r+c] := 1; diag2[r+N1-c] := 1;
            col[c] := r; _try (c+1);
            row[r] := 0; diag1[r+c] := 0; diag2[r+N1-c] := 0;
          end (* end of [if] *)
        end (* end of [if] *)
      end (* end of [if] *)
    ) // end of [val]
  in
    // empty
  end // end of [if]
// end of [_try]

(* ****** ****** *)
	
implement main () = let
  val () = _try (0) in printf
    ("The total number of solutions is [%i].\n",@(!NSOL))
end // end of [main]

(* ****** ****** *)

(* end of [queens3.dats] *)
