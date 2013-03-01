(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS - Unleashing the Power of Types!
**
** Copyright (C) 2002-2008 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see  the  file  COPYING.  If not, write to the Free
** Software Foundation, 51  Franklin  Street,  Fifth  Floor,  Boston,  MA
** 02110-1301, USA.
**
*)

(* ****** ****** *)
//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: July 2007
//
(* ****** ****** *)

staload "top.sats"

staload _ = "prelude/DATS/reference.dats"

(* ****** ****** *)

extern fun errmsg {a:viewt@ype} (msg: string): a
implement errmsg (msg) = (prerr msg; prerr_newline (); exit 1)

(* ****** ****** *)

#define BASE 8
#define c2i int_of_char
#define i2c char_of_int1

(* ****** ****** *)

typedef intch = intBtw (~1, CODEPOINT_MAX+1)

extern fun char_get (): intch = "char_get"
extern fun char_update (): void = "char_update"
extern fun char_get_update (): intch = "char_get_update"
extern fun char_update_get (): intch = "char_update_get"

extern fun pos_prev_reset (): void = "pos_prev_reset"

%{^

static int the_line_cnt = 1 ;
static int the_line_cnt_prev = 1 ;
static int the_char_cnt = 0 ;
static int the_char_cnt_prev = 0 ;

ats_int_type
pos_get_line () { return the_line_cnt ; }
ats_int_type
pos_get_char () { return the_char_cnt ; }

ats_int_type
pos_get_line_prev () { return the_line_cnt_prev ; }
ats_int_type
pos_get_char_prev () { return the_char_cnt_prev ; }

ATSinline()
ats_void_type
pos_prev_reset (
// there is no argument for this fun
) {
  the_line_cnt_prev = the_line_cnt ;
  the_char_cnt_prev = the_char_cnt ;
  return ;  
} // end of [pos_prev_reset]

/* ****** ****** */

ATSinline()
ats_void_type
pos_advance (ats_int_type c) {
  switch (c) {
    case '\n':
      ++the_line_cnt ; the_char_cnt = 0 ; break ;
    default: ++the_char_cnt ; break ;
  } /* end of [switch] */
  return ;
}

static ats_int_type the_char ;

ATSinline()
ats_int_type char_get() { return the_char ; }

ATSinline()
ats_void_type
char_update() { 
  the_char = atslex_getchar () ;
  pos_advance (the_char) ;
  return ;
} // end of [char_update]

ATSinline()
ats_int_type
char_get_update() {
  int c = the_char ;
  the_char = atslex_getchar () ;
  pos_advance (the_char) ;
  return c ;
} // end of [char_get_update]

ATSinline()
ats_int_type
char_update_get() {
  the_char = atslex_getchar () ;
  pos_advance (the_char) ;
  return the_char ;
} // end of [char_update_get]

%} // end of [%{^]

(* ****** ****** *)

dataviewtype chars (int) =
  | chars_nil (0)
  | {n:nat} chars_cons (n+1) of (int, chars n)
// end of [chars]

#define nil chars_nil
#define :: chars_cons

extern
fun chars_is_nil
  {n:nat} (cs: !chars n): bool (n == 0) = "chars_is_nil"
// end of [chars_is_nil]

implement
chars_is_nil (cs) = case+ cs of
  | nil () => (fold@ cs; true) | _ :: _ => (fold@ cs; false)
// end of [chars_is_nil]

extern
fun chars_uncons {n:pos}
  (cs: &chars n >> chars (n-1)): int = "chars_uncons"
// end of [chars_uncons]

implement
chars_uncons (cs) =
  let val+ ~(c :: cs1) = cs in cs := cs1; c end
// end of [chars_uncons]

fun chars_free
  {n:nat} (cs: chars n): void =
  case+ cs of ~(c :: cs) => chars_free cs | ~nil () => ()
// end of [chars_free]

(* ****** ****** *)

fun list_vt_of_chars {n:nat} .<n>. (
  cs: chars n
, res: &List_vt Nat? >> list_vt (Nat, n)
) :<!exn> void = case+ cs of
  | ~nil () => res := list_vt_nil ()
  | ~(c :: cs) => let
      val [c:int] c = int1_of_int c
      val () = assert (c >= 0)
      val () = res := list_vt_cons {Nat} {0} (c, ?)
      val+ list_vt_cons (_, !p_res) = res
      val () = list_vt_of_chars (cs, !p_res)
      val () = fold@ (res)
    in
      (*empty*)
    end // end of [let]
// end of [list_vt_of_chars]

// TODO: move to unicode.dats?
fun string32_make_charlst_rev_int {n:nat} (
  cs: chars n, n: int n
) : string32_vt n = let
  val nsz = size1_of_int1 n
  val tsz = sizeof<Nat>
  val (pf_gc, pf_arr | p_arr) = array_ptr_alloc_tsz {Nat} (nsz, tsz)
  prval () = free_gc_elim {Nat?} (pf_gc) // return the certificate to GC
  val (pfmul | ofs) = mul2_size1_size1 (nsz, tsz)
  fun loop {m,n:nat} {ofs:int} {l:addr} .<m>. (
    pfmul: MUL (m, sizeof Nat, ofs)
  , pf1: array_v (Nat?, m, l)
  , pf2: array_v (Nat, n, l+ofs)
  | p: ptr (l+ofs), cs: chars m
  ) : (array_v (Nat, m+n, l) | void) = let
    prval () = mul_nat_nat_nat (pfmul)
  in
    case+ cs of
    | ~nil () => let
        prval MULbas () = pfmul
        prval () = array_v_unnil {Nat?} (pf1)
      in
        (pf2 | ())
      end // end of [let]
    | ~(c :: cs) => let
        prval (pf11, pf1at) = array_v_unextend {Nat?} (pfmul, pf1)
        val [c:int] c = int1_of_int c
        val () = assert (c >= 0)
        val p = p-sizeof<Nat>
        val () = !p := c
        prval () = mul_nat_nat_nat (pfmul)
        prval MULind pf1mul = pfmul
        prval pf22 = array_v_cons {Nat} (pf1at, pf2)
      in
        loop (pf1mul, pf11, pf22 | p, cs)
      end // end of [let]
  end // end of [loop]
  val (pf1_arr | ()) = loop (pfmul, pf_arr, array_v_nil {Nat} () | p_arr+ofs, cs)
  prval () = pf_arr := pf1_arr
in
  array_make_view_ptr {Nat} (pf_arr | p_arr)
end // end of [string32_make_charlst_rev_int]

extern // [cs] must not contain nulls
fun string_make_charlst_rev_int
  {n:nat} (
  cs: chars n, n: int n
) : [m:nat] string m =
  "string_make_charlst_rev_int"
// end of [fun]

%{$

ats_ptr_type
string_make_charlst_rev_int
  (ats_ptr_type cs, const ats_int_type n) {
  ats_ptr_type cs0 = cs ;
  int i0 ;
  ats_size_type m = 0 ;
  char *s0, *s ;

  while (!chars_is_nil(cs)) { i0 = chars_uncons(&cs) ; m += utf8_codepoint_width(i0) ; }
  cs = cs0 ;
  s0 = ats_malloc_gc(m+1) ; s = s0 + m ; *s = '\0' ; --s ;
  while (!chars_is_nil(cs)) {
    int wdt ; /* uninitialized */
    i0 = chars_uncons(&cs) ;
    wdt = utf8_codepoint_width(i0) ;
    s -= wdt ;
    utf8_codepoint_store(s, wdt, i0) ;
  }
  return s0 ;
} /* string_make_charlst_rev_int */

%} // end of [%{$]

(* ****** ****** *)

implement
tokenize_line_comment () = loop () where {
  fun loop (): void = let
    val c = char_get () in
    if c >= 0 then begin
      char_update ();
      if c <> 0x0a (* \n *) then loop ()
    end
  end // end of [let]
} // end of [tokenize_line_comment]

(* ****** ****** *)

implement
tokenize_rest_text () = loop (nil (), 0) where {
  fun loop {n:nat} (cs: chars n, n: int n): string = let
    val c = char_get () in
    if c >= 0 then begin
      char_update (); loop (c :: cs, n+1)
    end else begin // c = EOF
      string_make_charlst_rev_int (cs, n) // the end of file is reached
    end // end of [if]
  end // end of [let]
} // end of [tokenize_rest_text]

(* ****** ****** *)

fn errmsg_unclosed_logue
  (): string = let
  val pos = position_prev_get ()
in
  prerr_string ("The logue starting at [");
  prerr_pos pos;
  prerr_string ("] is not closed.");
  prerr_newline ();
  exit {string} (1)
end // end of [errmsg_unclosed_logue]

implement
tokenize_logue () =
  loop (nil (), 0, 0) where {
  fun loop {n,level:nat}
    (cs: chars n, n: int n, level: int level): string = let
    val c = char_get ()
  in
    if c >= 0 then begin
      if c = 37 (* '%' *) then let
        val c1 = char_update_get () in
        if c1 >= 0 then begin
          if c1 = 123 (* '}' *) then begin
            if level > 0 then begin
              char_update (); loop (c1 :: c :: cs, n+2, level-1)
            end else begin
              char_update (); string_make_charlst_rev_int (cs, n)
            end // end of ['}']
          end else if c1 = 123 (* '\{' *) then begin
            char_update (); loop (c1 :: c :: cs, n+2, level+1)
          end // end of ['\{']
          else loop (c :: cs, n+1, level)
        end else begin // c1 = EOF
          chars_free cs; errmsg_unclosed_logue ()
        end // end of [if]
      end (* end of ['%'] *)
      else (* c <> '%' *) begin
        char_update (); loop (c :: cs, n+1, level)
      end // end of [_]
    end else begin // c = EOF
      chars_free cs; errmsg_unclosed_logue ()
    end // end of [if]
  end // end of [loop]
} // end of [tokenize_logue]

(* ****** ****** *)

fn errmsg_unclosed_funarg
  (): string = let
  val pos = position_prev_get ()
in
  prerr_string ("The function argument starting at [");
  prerr_pos pos;
  prerr_string ("] is not closed.");
  prerr_newline ();
  exit {string} (1)
end // end of [errmsg_unclosed_funarg]

implement
tokenize_funarg () =
  loop (nil (), 0) where {
  fun loop {n:nat} (cs: chars n, n: int n): string = let
    val c = char_get ()
  in
    if c >= 0 then begin
      if c = 41 (* ')' *) then begin
        char_update (); string_make_charlst_rev_int (cs, n)
      end // end of [')']
      else (char_update (); loop (c :: cs, n+1))
    end else begin
      chars_free cs; errmsg_unclosed_funarg ()
    end // end of [if]
  end // end of [loop]
} // end of [tokenize_funarg]

(* ****** ****** *)

fn errmsg_char_esc
  (): char = let
  val pos = position_prev_get ()
in
  prerr_string ("The escaped char at [");
  prerr_pos pos;
  prerr_string ("] is not supported.");
  prerr_newline ();
  exit {char} (1)
end // end of [errmsg_char_esc]

fun tokenize_char_esc_code (ci: int, i: int): int =
  if i < 2 then let
    val c = char_get () in
    if c >= 0 then begin
      if utf32_isdigit c then begin
          char_update (); tokenize_char_esc_code (BASE * ci + (c - 48 (* '0' *)), i+1)
      end // end of [_ when ...]
      else ci // end of [_]
    end else ci
  end else begin
    ci // function returns: an error is to be reported later
  end // end of [if]
// end of [tokenize_char_esc_code]

fun tokenize_char_esc (): int = let
  val c = char_get_update () in case+ 0 of
  | _ when c >= 0 => begin
      if utf32_isdigit c then
        tokenize_char_esc_code (c - 48 (* '0' *), 0)
      else begin case+ c of
        | 97 (*'a'*) => 7 (* alert *)
        | 98 (*'b'*) => 10 (* backspace *)
        | 102 (*'f'*) => 14 (* line feed *)
        | 110 (*'n'*) => 12 (* newline *)
        | 114 (*'r'*) => 15 (* carriage return *)
        | 116 (*'t'*) => 11 (* horizonal tab *)
        | 118 (*'v'*) => 13 (* vertical tab *)
        | _ => c (* no effect on other chars *)
      end // end of [if]
    end // end of [_ when ...]
  | _ (* c = EOF *) => 0 // an error is to be reported later
end // end of [tokenize_char_esc]

fn errmsg_unclosed_char (): int = let
  val pos = position_prev_get ()
in
  prerr_string ("The char starting at [");
  prerr_pos pos;
  prerr_string ("] is not closed.");
  prerr_newline ();
  exit {int} (1)
end // end of [errmsg_unclose_char]

fun tokenize_char (): int = let
  val c0 = char_get (); val c = case+ 0 of
  | _ when c0 >= 0 => begin
      case+ c0 of
      | 92 (*'\\'*) => (char_update (); tokenize_char_esc ())
      | 39 (*'\''*) => 0 (* '' stands for '\0' *)
      | _ => (char_update (); c0)
    end // end of [_ when ...]
  | _ (* c0 < 0 *) => 0
  val c1 = char_get_update ()
in
  case+ 0 of
  | _ when c1 >= 0 => begin
      if c1 <> 39 (* '\'' *) then errmsg_unclosed_char () else c
    end // end of [_ when ...]
  | _ (* c1 < 0 *) => errmsg_unclosed_char ()
end // end of [tokenize_char]

(* ****** ****** *)

fn errmsg_unclosed_code
  (): string = let
  val pos = position_prev_get ()
in
  prerr_string ("The code starting at [");
  prerr_pos pos;
  prerr_string ("] is not closed.");
  prerr_newline ();
  exit {string} (1)
end // end of [errmsg_unclosed_code]

fun tokenize_code
  {n,level:nat} (
  cs: chars n, n: int n, level: int level
) : string = let
  val c = char_get ()
in
  if c >= 0 then begin
    case+ c of
    | 123 (*'\{'*) => begin
        char_update (); tokenize_code (c :: cs, n+1, level+1)
      end // end of ['\{']
    | 125 (*'}'*) when level > 0 => begin
        char_update (); tokenize_code (c :: cs, n+1, level-1)
      end // end of ['}' when ...]
    | 125 (*'}'*) (* level = 0 *) => begin
        char_update (); string_make_charlst_rev_int (cs, n)
      end // end of ['}']
    | _ => begin
        char_update (); tokenize_code (c :: cs, n+1, level)
      end // end of [_]
  end else begin
    chars_free cs; errmsg_unclosed_code ()
  end // end of [if]
end // end of [tokenize_code]

(* ****** ****** *)

fn errmsg_unclosed_comment
  (): void = let
  val pos = position_prev_get ()
in
  prerr_string ("The comment starting at [");
  prerr_pos pos;
  prerr_string ("] is not closed.");
  prerr_newline ();
  exit {void} (1)
end // end of [errmsg_unclosed_comment]

fun tokenize_comment
  {level:nat} (level: int level): void = let
  val c = char_get ()
in
  if c >= 0 then begin
    case+ c of
    | 40 (*'\('*) => let
        val c1 = char_update_get () in case+ 0 of
        | _ when c1 >= 0 => begin
            case+ c1 of
            | 42 (* '*' *) => tokenize_comment (level+1) | _ => tokenize_comment level
          end // end of [_ when ...]
        | _ (* c1 < 0 *) => tokenize_comment level
      end // end of ['\(']
    | 42 (* '*' *) => let
        val c1 = char_update_get () in case+ 0 of
        | _ when c1 >= 0 => begin
            case+ c1 of
            | 41 (*')'*) => begin
                if level > 0 then tokenize_comment (level-1) else char_update ()
              end // end of [')']
            | _ => tokenize_comment level
          end // end of [_ when ...]
        | _ (* c1 < 0 *) => tokenize_comment level
      end // end of ['*']
    | _ => (char_update (); tokenize_comment level)
  end else begin
    errmsg_unclosed_comment ()
  end // end of [if]
end // end of [tokenize_comment]

(* ****** ****** *)

fun tokenize_int
  (i: int): int = let
  val c = char_get () in
  case+ 0 of
  | _ when c >= 0 => begin
      if utf32_isdigit c then
        (char_update (); tokenize_int (BASE * i + (c - 48 (*'0'*))))
      else i
    end // end of [_ when ...]
  | _ (* c = EOF *) => i
end // end of [tokenize_int]

(* ****** ****** *)

fun tokenize_string_char (): int = let
  val c = char_get_update () in case+ 0 of
  | _ when c >= 0 => begin
      if c = 92 (* '\\' *) then tokenize_char_esc () else c
    end // end of [_ when ...]
  | _ (* c = EOF *) => 0 // an error is to be reported later
end // end of [tokenize_string_char]

fn errmsg_unclosed_string {n:nat} (): @(size_t n, string32_vt n) = let
  val pos = position_prev_get ()
in
  prerr_string ("The string starting at [");
  prerr_pos pos;
  prerr_string ("] is not closed.");
  prerr_newline ();
  exit {@(size_t n, string32_vt n)} (1)
end // end of [errmsg_unclosed_string]

fun tokenize_string {n:nat}
  (cs: chars n, n: int n): [m:nat] @(size_t m, string32_vt m) = let
  val c0 = char_get ()
in
  case+ 0 of
  | _ when (c0 >= 0) => begin
      if c0 = 34 (*'"'*) then begin
        char_update (); @(size1_of_int1 n, string32_make_charlst_rev_int (cs, n))
      end else let
        val c = tokenize_string_char ()
      in
        tokenize_string (c :: cs, n+1)
      end // end of [if]
    end // end of [begin]
  | _ (* c0 < 0 *) => begin
      chars_free cs; errmsg_unclosed_string {n} ()
    end // end of [_]
end // end of [tokenize_string]

(* ****** ****** *)

fn char_iseof (c: int): bool =
  if c >= 0 then false else true

fn char_issymbl (c: int): bool =
  case+ c of
  | 33 (* ! *) => true
  | 37 (* % *) => true
  | 38 (* & *) => true
  | 35 (* # *) => true
  | 43 (* + *) => true
  | 45 (* - *) => true
  | 47 (* / *) => true
  | 58 (* : *) => true
  | 60 (* < *) => true
  | 61 (* = *) => true
  | 62 (* > *) => true
  | 64 (* @ *) => true
  | 92 (* \\ *) => true
  | 126 (* ~ *) => true
  | 96 (* ` *) => true
  | 124 (* | *) => true
  | 42 (* * *) => true
  | _ => false
// end of [char_issymbl]

fun tokenize_word_sym {n:nat}
  (cs: chars n, n: int n): [m:nat] @(size_t m, string32_vt m) = let
  val c = char_get () in case+ 0 of
  | _ when c >= 0 => begin
      if char_issymbl c then
        (char_update (); tokenize_word_sym (c :: cs, n+1))
      else @(size1_of_int1 n, string32_make_charlst_rev_int (cs, n))
    end // end of [_ when ...]
  | _ (* c = EOF *) => @(size1_of_int1 n, string32_make_charlst_rev_int (cs, n))
end // end of [tokenize_word_sym]

fun tokenize_word_ide
  {n:nat} (
  cs: chars n, n: int n
) : [m:nat] @(size_t m, string32_vt m) = let
  fn char_islttr (c: int): bool =
    if c >= 97 (* 'a' *) || c <= 122 (* 'z' *) then true
    else if c >= 65 (* 'A' *) || c <= 90 (* 'Z' *) then true
    else if utf32_isdigit c then true
    else c = 95 (* '_' *)
  // end of [char_islttr]
  val c = char_get () in case+ 0 of
  | _ when c >= 0 => begin
      if char_islttr c then
        (char_update (); tokenize_word_ide (c :: cs, n+1))
      else @(size1_of_int1 n, string32_make_charlst_rev_int (cs, n))
    end // end of [_ when ...]
  | _ (* c = EOF *) => @(size1_of_int1 n, string32_make_charlst_rev_int (cs, n))
end // end of [tokenize_word_ide]

(* ****** ****** *)

extern fun
fprint_token {m:file_mode}
  (pf_mod: file_mode_lte (m, w) | fil: &FILE m, tok: !token): void =
  "fprint_token"

implement
fprint_token (
  pf_mod | fil, tok
) = begin case+ tok of
  | TOKchar c => (fprintf (pf_mod | fil, "char(%x)", @(uint_of_int c)); fold@ tok)
  | TOKcode s => (fprintf (pf_mod | fil, "code(%s)", @(s)); fold@ tok)
  | TOKint i => (fprintf (pf_mod | fil, "int(%i)", @(i)); fold@ tok)
  | TOKstring @(n, s) => begin
      fprint (pf_mod | fil, "string(");
      fprint_string32 (pf_mod, view@ fil | &fil, s, n);
      fprint (pf_mod | fil, ")");
      fold@ tok
    end // end of [begin]
  | TOKword @(n, s) => begin
      fprint (pf_mod | fil, "word(");
      fprint_string32 (pf_mod, view@ fil | &fil, s, n);
      fprint (pf_mod | fil, ")");
      fold@ tok
    end // end of [begin]
  | TOKlit c => (fprintf (pf_mod | fil, "lit(%x)", @(uint_of_int c)); fold@ tok)
  | TOKmark s => (fprintf (pf_mod | fil, "mark(%s)", @(s)); fold@ tok)
  | TOKeof () => (fprint_string (pf_mod | fil, "EOF"); fold@ tok)
end // end of [fprint_token]

implement
print_token (tok) = () where {
  val (pf_stdout | ptr_stdout) = stdout_get ()
  val () = fprint_token (file_mode_lte_w_w | !ptr_stdout, tok)
  val () = stdout_view_set (pf_stdout | (*none*))
} // end of [print_token]

implement
prerr_token (tok) = () where {
  val (pf_stderr | ptr_stderr) = stderr_get ()
  val () = fprint_token (file_mode_lte_w_w | !ptr_stderr, tok)
  val () = stderr_view_set (pf_stderr | (*none*))
} // end of [prerr_token]

(* ****** ****** *)

fun pos_prev_reset_and_char_update
  (): void = (pos_prev_reset (); char_update ())
// end of [fun]

extern fun tokenize (): token = "tokenize"

implement
tokenize () = let
//
fun tokenize_main (c: int): token = case+ c of
  | 39 (* '\'' *) => let
      val () = pos_prev_reset_and_char_update ()
    in
      TOKchar (tokenize_char ())
    end // end of ['\'']
  | 40 (* '\(' *) => let
      val () = pos_prev_reset_and_char_update ()
      val c1 = char_get (); val isstar = (
        if c1 >= 0 then begin
          case+ c1 of 42 (* '*' *) => true | _ => false
        end else false
      ) : bool // end of [isstar]
    in
      if isstar then (tokenize_comment (0); tokenize ())
      else TOKlit 40 (* '\(' *)
    end // end of ['\(']
  | 47 (* '/' *) => let
      val () = pos_prev_reset_and_char_update ()
      val c1 = char_get (); val isslash = (
        if c1 >= 0 then begin
          case+ c1 of 47 (* '/' *) => true | _ => false
        end else false
      ) : bool // end of [isslash]
    in
      if isslash then begin
        tokenize_line_comment (); tokenize ()
      end else begin
        TOKword (tokenize_word_sym (47 (* '/' *) :: nil (), 1))
      end // end of [if]
    end // end of ['/']
  | 123 (* '\{' *) => let
      val () = pos_prev_reset_and_char_update ()
    in
      TOKcode (tokenize_code (nil (), 0, 0))
    end // end of ['\{']
  | 34 (* '"' *) => let
      val () = pos_prev_reset_and_char_update ()
    in
      TOKstring (tokenize_string (nil (), 0))
    end // end of ['"']
  | 95 (* '_' *) => let
      val () = pos_prev_reset_and_char_update ()
    in
      TOKword (tokenize_word_ide (c :: nil (), 1))
    end // end of ['_']
  | 37 (* '%' *) => let
      val () = pos_prev_reset_and_char_update ()
      val c1 = char_get (); val islbrace = (
        if c1 >= 0 then begin
          case+ c1 of 123 (* '\{' *) => true | _ => false
        end else false
      ) : bool // end of [islbrace]
    in
      if islbrace then begin
        char_update (); TOKmark "%{"
      end else begin
        TOKword (tokenize_word_sym (c :: nil (), 1))
      end // end of [if]
    end // end of ['%']
  | _ when utf32_isdigit c => let
      val () = pos_prev_reset_and_char_update ()
    in
      TOKint (tokenize_int (c - 48 (* '0' *)))
    end // end of [digit]
  | _ when c >= 97 && c <= 122 || c >= 65 && c <= 90 (* char_isalpha c *) => let
      val () = pos_prev_reset_and_char_update ()
    in
      TOKword (tokenize_word_ide (c :: nil (), 1))
    end // end of [alpha]
  | _ when char_issymbl c => let
      val () = pos_prev_reset_and_char_update ()
    in
      TOKword (tokenize_word_sym (c :: nil (), 1))
    end // end of [symbl]
  | _ when utf32_isspace c (* char_isspace c *) => let
      val () = char_update () in tokenize ()
    end // end of [space]
  | _ => let
      val () = pos_prev_reset_and_char_update ()
    in
      TOKlit c
    end // end of [_]
(* end of [tokenize_main] *)
//
  val c = char_get () in case+ 0 of
  | _ when c >= 0 => tokenize_main c
  | _ (* c = EOF *) => TOKeof ()
end // end of [tokenize]

(* ****** ****** *)

implement
token_get () = tokenize ()
(*
local

val the_token = ref<Option_vt token> (None_vt ())

in // in of [local]

implement
token_get () = let
  val (vbox pf | p) = ref_get_view_ptr (the_token)
in
  case+ !p of
  | ~Some_vt tok => (!p := None_vt (); tok)
  | None_vt () => (fold@ !p; $effmask_ref (exit_errmsg (1, "[token_get]: internal error")))
end // end of [token_get]

implement
token_update () = let
  val (vbox pf | p) = ref_get_view_ptr (the_token)
in
  case+ !p of
  | ~Some_vt tok => (token_free tok; !p := Some_vt ($effmask_ref (tokenize ())))
  | ~None_vt () => !p := Some_vt ($effmask_ref (tokenize ()))
end // end of [token_update]

implement
token_get_update () = let
  val (vbox pf | p) = ref_get_view_ptr (the_token)
in
  case+ !p of
  | ~Some_vt tok => (token_free tok; !p := None_vt (); $effmask_ref (tokenize ()))
  | None_vt () => (fold@ !p; $effmask_ref (tokenize ()))
end // end of [token_get_update]

implement
token_putback (x) = let
  val (vbox pf | p) = ref_get_view_ptr (the_token)
in
  case+ !p of
  | Some_vt _ => (fold@ !p; token_free x; $effmask_ref (exit_errmsg (1, "[token_putback]: internal error")))
  | ~None_vt () => (!p := Some_vt x)
end // end of [token_putback]

end // end of [local]
*)

(* ****** ****** *)

implement
token_free (
  tok
) = begin case+ tok of
  | ~TOKchar c => ()
  | ~TOKcode s => ()
  | ~TOKint _ => ()
  | ~TOKstring @(n, arr) => ()
  | ~TOKword @(n, arr) => ()
  | ~TOKlit c => ()
  | ~TOKmark s => ()
  | ~TOKeof () => ()
end // end of [token_free]

(* ****** ****** *)
//
// HX: initialization
//
implement
token_initialization () = char_update () // flush out a junk value

(* ****** ****** *)

(* end of [token.dats] *)
