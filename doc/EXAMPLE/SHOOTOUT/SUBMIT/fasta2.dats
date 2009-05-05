(*
** The Computer Language Shootout
** http://shootout.alioth.debian.org/
** 
** contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
**
** compilation command:
**   atscc -O3 fasta.dats -msse2 -mfpmath=sse -o fasta
*)

(* ****** ****** *)

staload "libc/SATS/stdio.sats"
staload "libc/SATS/stdlib.sats" // for [atoi]

(* ****** ****** *)

staload _(*anonymous*) = "prelude/DATS/array.dats"

(* ****** ****** *)

typedef float = float; #define i2f float_of_int

// this is about 10% slower:
// typedef float = double; #define i2f double_of_int

(* ****** ****** *)

local

#define IM 139968
#define IA 3877
#define IC 29573

var state: int = 42
val (pfbox | ()) = vbox_make_view_ptr {int} (view@ state | &state)

in // in of [local]

fn random_gen
  (max: float): float = let
  prval vbox pf = pfbox
  val () = state := (state * IA + IC) mod IM in max * (i2f state / IM)
end // end of [random_gen]

end // end of [local]

(* ****** ****** *)

typedef amino = @{ c= char, p= float }

fn make_cumulative {n:nat}
  (table: &(@[amino][n]), n: size_t n): void = let
  fun loop {i:nat | i <= n} .<n-i>. (
      table: &(@[amino][n]), n: size_t n, i: int i, prob: float
    ) : void =
    if i < n then let
      val prob = prob + table.[i].p in
      table.[i].p := prob; loop (table, n, i+1, prob)
    end // end of [if]
  // end of [loop]
in
  loop (table, n, 0, 0.0: float)
end // end of [make_cumulative]

(* ****** ****** *)

extern fun fwrite_substring {m,p,n:nat | p + n <= m}
  (str: string m, beg: size_t p, n: size_t n, out: FILEref): void
  = "fasta_fwrite_substring"

extern fun fputc (c: char, out: FILEref): void = "fasta_fputc"

(* ****** ****** *)

#define WIDTH 60

fn repeat_fasta {len:nat} {n:nat}
  (out: FILEref, str: string len, n: size_t n): void = let
  macdef WIDTH_sz = size1_of_int1 (WIDTH)
  val len = string1_length str; val () = assert (len >= WIDTH_sz)
  fun loop {n,pos:nat | pos <= len}
    (out: FILEref, n: size_t n, pos: size_t pos):<cloref1> void =
    if n > WIDTH_sz then let
      val left = len - pos in
      if left >= WIDTH_sz then begin
        fwrite_substring (str, pos, WIDTH_sz, out); fputc ('\n', out);
        loop (out, n - WIDTH_sz, pos + WIDTH_sz)
      end else begin
        fwrite_substring (str, pos, left, out);
	fwrite_substring (str, 0, WIDTH_sz - left, out); fputc ('\n', out);
	loop (out, n - WIDTH_sz, WIDTH_sz - left)
      end // end of [if]
    end else let
      val left = len - pos in
      if left >= n then begin
        fwrite_substring (str, pos, n, out); fputc ('\n', out)
      end else begin
        fwrite_substring (str, pos, left, out);
	fwrite_substring (str, 0, n-left, out); fputc ('\n', out)
      end // end of [if]
    end (* end of [if] *)
  // end of [loop]
in
  loop (out, n, 0)
end // end of [repeat_fasta]

fun random_char {sz,i:nat | i <= sz}
  (tbl: &(@[amino][sz]), sz: size_t sz, prob: float, i: size_t i): char =
  if i < sz then
    if prob >= tbl.[i].p then random_char (tbl, sz, prob, i+1) else tbl.[i].c
  else begin
    exit_errmsg {char} (1, "Exit: [random_char] failed.\n")
  end (* end of [if] *)
// end of [random_char]

fun random_buf
  {sz:nat} {i,len,bsz:nat | i <= len; len <= bsz}
  (tbl: &(@[amino][sz]), buf: &bytes(bsz), sz: size_t sz, len: size_t len, i: size_t i)
  : void =
  if i < len then let
    val c = random_char (tbl, sz, random_gen (1.0: float), 0)
    val () = buf[i] := byte_of_char c
  in
    random_buf (tbl, buf, sz, len, i+1)
  end
// end of [random_buf]

extern fun fwrite_byte {bsz,n:nat | n <= bsz}
  (buf: &bytes (bsz), n: size_t n, out: FILEref):<> sizeLte n
  = "atslib_fwrite_byte"

fn random_fasta {sz,n:nat} (
    out: FILEref, tbl: &(@[amino][sz]), sz: size_t sz, n: size_t n
  ) : void = let
  macdef WIDTH_sz = size1_of_int1 (WIDTH)
  fun loop {n:nat} .<n>. (
      out: FILEref
    , tbl: &(@[amino][sz]), buf: &bytes(WIDTH+1)
    , sz: size_t sz, n: size_t n
    ) : void =
    if (n > WIDTH_sz) then let
      val () = random_buf (tbl, buf, sz, WIDTH_sz, 0)
      val _(*int*) = fwrite_byte (buf, WIDTH_sz+1, out)
    in
      loop (out, tbl, buf, sz, n-WIDTH_sz)
    end else let
      val () = random_buf (tbl, buf, sz, n, 0)
      val _(*int*) = fwrite_byte (buf, n, out)
      val () = fputc ('\n', out)
    in
      // empty
    end // end of [loop]
  val () = make_cumulative (tbl, sz)
  var !p_buf with pf_buf = @[byte][WIDTH+1]()
  prval () = pf_buf := bytes_v_of_b0ytes_v (pf_buf)
  val () = p_buf->[WIDTH_sz] := byte_of_char '\n'
  val () = loop (out, tbl, !p_buf, sz, n)
in
  // empty
end // end of [random_fasta]

val alu ="\
GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

//

implement main (argc, argv) = () where {

val () = assert (argc = 2)
val n = int1_of_string (argv.[1])
val () = assert (n >= 0)
val @(pf_gc, pf_iub | p_iub, iub_sz) = $arrsz{amino}(
  @{c='a', p=0.27}
, @{c='c', p=0.12}
, @{c='g', p=0.12}
, @{c='t', p=0.27}
, @{c='B', p=0.02}
, @{c='D', p=0.02}
, @{c='H', p=0.02}
, @{c='K', p=0.02}
, @{c='M', p=0.02}
, @{c='N', p=0.02}
, @{c='R', p=0.02}
, @{c='S', p=0.02}
, @{c='V', p=0.02}
, @{c='W', p=0.02}
, @{c='Y', p=0.02}
) // end of [val]

val @(pf_homo_gc, pf_homo | p_homo, homo_sz) = $arrsz{amino}(
  @{c='a', p=0.3029549426680}
, @{c='c', p=0.1979883004921}
, @{c='g', p=0.1975473066391}
, @{c='t', p=0.3015094502008}
) // end of [val]

#define i2sz size1_of_int1
val () = fprint (stdout_ref, ">ONE Homo sapiens alu\n")
val () = repeat_fasta (stdout_ref, alu, i2sz (2 * n))
val () = fprint (stdout_ref, ">TWO IUB ambiguity codes\n")
val () = random_fasta (stdout_ref, !p_iub, iub_sz, i2sz (3 * n))
val () = array_ptr_free {amino} (pf_gc, pf_iub | p_iub)
val () = fprint (stdout_ref, ">THREE Homo sapiens frequency\n")
val () = random_fasta (stdout_ref, !p_homo, homo_sz, i2sz (n * 5))
val () = array_ptr_free {amino} (pf_homo_gc, pf_homo | p_homo)

} // end of [main]

(* ****** ****** *)

%{$

ats_void_type
fasta_fwrite_substring (
  ats_ptr_type str, ats_size_type beg
, ats_size_type len, ats_ptr_type out
) {
  // locked/unlocked: no observable difference
  fwrite_unlocked(((char*)str)+beg, 1, len, (FILE*)out) ; return ;
}

ats_void_type
fasta_fputc (ats_char_type c, ats_ptr_type out) {
  // locked/unlocked: no observable difference
  fputc_unlocked ((char)c, (FILE*)out) ; return ;
}

%}

(* ****** ****** *)

(* end of [fasta2.dats] *)
