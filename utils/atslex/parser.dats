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
*)

(* ****** ****** *)

// July 2007
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

staload "top.sats"

staload "libc/SATS/stdio.sats"

staload _ = "prelude/DATS/array.dats"

staload "libats/lex/unicode.sats"

(* ****** ****** *)

fun prerr_range (): void = let
  val pos_prev = position_prev_get ()
  val pos = position_get ()
in
  prerr_pos pos_prev; prerr "-"; prerr_pos pos
end // end of [prerr_range]

fun errmsg_illegal
  {a:viewt@ype} (msg: string): a = begin
  prerr msg;
  prerr (": The token at [");
  prerr_range ();
  prerr ("] is illegal.");
  prerr_newline ();
  exit {a} (1)
end // end of [errmsg_illegal]

(* ****** ****** *)

fun errmsg_literal (c: int): void = begin
  prerr ("The token at [");
  prerr_range ();
  prerr ("] is not [");
  let
    val (pf_stderr | p_stderr) = stderr_get ()
    val c = int1_of_int c
    val () = assert_errmsg (c >= 0, "[errmsg_literal]: invalid codepoint")
    val res = utf8_encode (file_mode_lte_w_w, pf_stderr | p_stderr, c)
    val () = assert_errmsg (res >= 0, "[errmsg_literal]: error while trying to decode UTF-32")
  in
    fclose1_exn (pf_stderr | p_stderr)
  end;
  prerr ("].");
  prerr_newline ();
  exit {void} (1)
end // end of [errmsg_literal]

fun literal (tok: &token, c0: int): void =
  case+ tok of
  | TOKlit c =>
     if c0 = c then (free@ tok; tok := token_get ())
     else (fold@ tok; errmsg_literal (c0))
  | _ => errmsg_literal (c0)
// end of [literal]

(* ****** ****** *)

fun
errmsg_litword {n:nat} (
  s: string32_vt n, n: size_t n
): void = begin
  prerr ("The token at [");
  prerr_range ();
  prerr ("] is not [");
  let
    val (pf_stderr | p_stderr) = stderr_get ()
  in
    fprint_string32 (file_mode_lte_w_w, pf_stderr | p_stderr, s, n);
    stderr_view_set (pf_stderr | (*none*))
  end;
  prerr ("].");
  prerr_newline ();
  exit {void} (1)
end // end of [errmsg_litword]

fun litword {n:nat} (tok: &token, s0: string32_vt n, n0: size_t n): void =
  case+ tok of
  | TOKword @(n, s) => begin
      if n0 = n andalso eq_string32_string32 (n, s, n0, s0) then begin
        free@ {0} tok; tok := token_get ()
      end else (fold@ tok; errmsg_litword (s0, n0))
    end // end of [begin]
  | _ => errmsg_litword (s0, n0)
// end of [litword]

(* ****** ****** *)

fun errmsg_char (): int = begin
  prerr ("The token at [");
  prerr_range ();
  prerr ("] is not a char.");
  prerr_newline ();
  exit {int} (1)
end // end of [errmsg_char]

fun char (tok: &token): int =
  case+ tok of
  | ~TOKchar c => (tok := token_get (); c)
  | _ => errmsg_char ()
// end of [char ()]

(* ****** ****** *)
// AS-20130318: allow specification of codepoints

fun errmsg_codepoint (): int = begin
  prerr ("The token at [");
  prerr_range ();
  prerr ("] is not a codepoint.");
  prerr_newline ();
  exit {int} (1)
end // end of [errmsg_codepoint]

fun codepoint (tok: &token): int =
  case+ tok of
  | ~TOKint i => (tok := token_get (); i) // TODO: more checking
  | _ => errmsg_codepoint ()

(* ****** ****** *)
(*
fun errmsg_string (): string = begin
  prerr ("The token at [");
  prerr_range ();
  prerr ("] is not a string.");
  prerr_newline ();
  exit {string} (1)
end // end of [errmsg_string]

fun string (tok: &token): [n:nat] @(size_t n, string32_vt n) =
  case+ tok of
  | ~TOKstring s => (tok := token_get (); s)
  | _ => errmsg_string ()
// end of [string ()]
*)
(* ****** ****** *)

fun errmsg_ident (): string = begin
  prerr ("The token at [");
  prerr_range ();
  prerr ("] is not an identifier.");
  prerr_newline ();
  exit {string} (1)
end // end of [errmsg_ident]

fun ident (tok: &token): string =
  case+ tok of
  | ~TOKword @(n, s) => (tok := token_get (); string_of_string32 (s, n))
  | _ => errmsg_ident ()
// end of [ident]

(* ****** ****** *)

fun errmsg_code (): string = begin
  prerr ("The token at [");
  prerr_range ();
  prerr ("] is not code.");
  prerr_newline ();
  exit {string} (1)
end // end of [errmsg_code]

fun code (tok: &token): string =
  case+ tok of
  | ~TOKcode s => (tok := token_get (); s)
  | _ => errmsg_code ()
// end of [code]

(* ****** ****** *)

fun charset_atm_r
  (tok: &token, c0: int, ft: (&token) -<fun1> int): charset_t =
  case+ tok of
  | TOKword @(n, s) => begin
      if n = 1 andalso bool1_of_bool (s[0] = 45) (* '-' *) then let
        val () = free@ {0} tok
        val () = tok := token_get (); val c1 = ft (tok)
      in
        charset_interval (c0, c1)
      end else (fold@ tok; charset_singleton c0)
    end // end of [TOKword]
  | _ => charset_singleton c0
// end of [charset_atm_r]

fun charset_seq_r
  (tok: &token, cs0: charset_t): charset_t =
  case+ tok of
  | ~TOKchar c => let
      val () = tok := token_get (); val cs1 = charset_atm_r (tok, c, char)
    in
      charset_seq_r (tok, charset_union (cs0, cs1))
    end //end of [TOKchar]
  | _ => cs0
// end of [charset_seq_r]

fun charset_codepoint_seq_r
  (tok: &token, cs0: charset_t): charset_t =
  case+ tok of
  | ~TOKint c => let
      val () = tok := token_get (); val cs1 = charset_atm_r (tok, c, codepoint)
    in
      charset_codepoint_seq_r (tok, charset_union (cs0, cs1))
    end //end of [TOKint]
  | _ => cs0
// end of [charset_codepoint_seq_r]

(* ****** ****** *)

fun charset_r (tok: &token): charset_t =
  case+ tok of
  | TOKlit c => begin
      case+ 0 of
      | _ when c = 93 (* ']' *) => let
      	   val () = free@ tok
          val () = tok := token_get () in charset_nil
        end // end of [TOKlit ']']
      | _ when c = 94 (* '^' *) => let
          val () = free@ tok
          val () = tok := token_get (); val c = charset_r (tok)
        in
          charset_complement (c)
        end // end of [TOKlit '^']
      | _ => (fold@ tok; errmsg_illegal {charset_t} ("charset_r"))
    end // end of [TOKlit _]
  | ~TOKint i => let
      val () = tok := token_get ()
      val cs1 = charset_atm_r (tok, i, codepoint)
      val cs = charset_codepoint_seq_r (tok, cs1)
      val () = literal (tok, 93) (* ']' *)
    in
      cs
    end // end of [TOKint _]
  | ~TOKchar c => let
      val () = tok := token_get ()
      val cs1 = charset_atm_r (tok, c, char)
      val cs = charset_seq_r (tok, cs1)
      val () = literal (tok, 93) (* ']' *)
    in
      cs
    end // end of [TOKchar _]
  | _ => begin
      errmsg_illegal {charset_t} ("charset_r")
    end // end of [_]
// end of [charset_seq_r]

(* ****** ****** *)

extern fun fprint_regex {m:file_mode}
  (pf_mod: file_mode_lte (m, w) | fil: &FILE m, reg: regex): void =
  "fprint_regex"

implement fprint_regex (pf_mod | fil, reg): void = begin
  case+ reg of
  | REGalt (reg1, reg2) => begin
      fprint_string (pf_mod | fil, "REGalt(");
      fprint_regex (pf_mod | fil, reg1);
      fprint_string (pf_mod | fil, ", ");
      fprint_regex (pf_mod | fil, reg2);
      fprint_string (pf_mod | fil, ")");
    end // end of [REGalt]
  | REGchars cs => begin
      fprint_string (pf_mod | fil, "REGchars(");
      fprint_charset (pf_mod | fil, cs);
      fprint_string (pf_mod | fil, ")");
    end // end [REGchars]
  | REGid id => begin
      fprint_string (pf_mod | fil, "REGid(");
      fprint_string (pf_mod | fil, id);
      fprint_string (pf_mod | fil, ")");
    end // end of [id]
  | REGnil () => begin
      fprint_string (pf_mod | fil, "REGnil()");
    end // end of [REGnil]
  | REGopt reg => begin
      fprint_string (pf_mod | fil, "REGopt(");
      fprint_regex (pf_mod | fil, reg);
      fprint_string (pf_mod | fil, ")");
    end // end of [REGopt]
  | REGplus reg => begin
      fprint_string (pf_mod | fil, "REGplus(");
      fprint_regex (pf_mod | fil, reg);
      fprint_string (pf_mod | fil, ")");
    end // end of [REGplus]
  | REGrep (reg, i) => begin
      fprint_string (pf_mod | fil, "REGrep(");
      fprint_regex (pf_mod | fil, reg);
      fprint_string (pf_mod | fil, ", ");
      fprint_int (pf_mod | fil, i);
      fprint_string (pf_mod | fil, ")");
    end // end of [REGrep]
  | REGseq (reg1, reg2) => begin
      fprint_string (pf_mod | fil, "REGseq(");
      fprint_regex (pf_mod | fil, reg1);
      fprint_string (pf_mod | fil, ", ");
      fprint_regex (pf_mod | fil, reg2);
      fprint_string (pf_mod | fil, ")");
    end // end of [REGseq]
  | REGstar reg => begin
      fprint_string (pf_mod | fil, "REGstar(");
      fprint_regex (pf_mod | fil, reg);
      fprint_string (pf_mod | fil, ")");
    end // end of [REGstar]
  | REGstr @(n, s) => begin
      fprint_string (pf_mod | fil, "REGstr(\"");
      fprint_string32 (pf_mod, view@ fil | &fil, s, n);
      fprint_string (pf_mod | fil, "\")");
    end // end of [REGstr]
end // end of [fprint_regex]

implement print_regex (reg) = let
   val (pf_stdout | ptr_stdout) = stdout_get ()
in
   fprint_regex (file_mode_lte_w_w | !ptr_stdout, reg);
   stdout_view_set (pf_stdout | (*none*))
end // end of [print_regex]

implement prerr_regex (reg) = let
  val (pf_stderr | ptr_stderr) = stderr_get ()
in
  fprint_regex (file_mode_lte_w_w | !ptr_stderr, reg);
  stderr_view_set (pf_stderr | (*none*))
end // end of [prerr_regex]

(* ****** ****** *)

fun is_regex_0 (tok: &token): bool = begin
  case+ tok of
  | TOKword @(n, s) => (fold@ tok; n = 1 andalso bool1_of_bool (s[0] = 95 (*'_'*)))
  | TOKchar c => (fold@ tok; true)
  | TOKlit 36 (* '$' *) => (fold@ tok; true)
  | TOKstring s => (fold@ tok; true)
  | TOKlit 91 (* '\[' *) => (fold@ tok; true)
  | TOKlit 40 (* '\(' *) => (fold@ tok; true)
  | _ => false
end // end of [is_regex_0]

fun regex_0 (tok: &token): regex =
  case+ tok of
  | ~TOKchar c => begin
      tok := token_get (); REGchars (charset_singleton c)
    end // end of [TOKchar]
  | ~TOKstring s => (tok := token_get (); REGstr s)
  | TOKlit c => begin
      case+ 0 of
      | _ when c = 36 (* '$' *) => begin
          free@ tok;
          tok := token_get ();
	  REGid (ident (tok))
        end // end of [begin]
      | _ when c = 91 (* '\[' *) => begin
      	  free@ tok;
          tok := token_get ();
          REGchars (charset_r (tok))
        end // end of [begin]
      | _ when c = 40 (* '\(' *) => let
          val () = free@ tok;
          val () = tok := token_get ()
	  val re = regex_3 (tok)
	  val () = literal (tok, 41 (* ')' *))
        in
          re
        end // end of [let]
      | _ => (fold@ tok; errmsg_illegal {regex} ("regex_0"))
    end // end of [TOKlit]
  | TOKword @(n, s) => begin
      if n = 1 andalso bool1_of_bool (s[0] = 95 (* "_" *)) then
      (free@ {0} tok; tok := token_get (); REGchars (charset_all))
      else (fold@ tok; errmsg_illegal {regex} ("regex_0"))
    end // end of [TOKword]
  | _ => errmsg_illegal {regex} ("regex_0")
// end of [regex_0]

and regex_1 (tok: &token): regex = begin
  let val reg = regex_0 (tok) in regex_1_r (tok, reg) end
end // end of [regex_1]

and regex_1_r (tok: &token, reg0: regex): regex = let
  #define b2b bool1_of_bool
in
  case+ tok of
  | TOKword @(n, s) => begin
      if n = 1 then begin
        if s[0] = 42 (* '*' *) then
          (free@ {0} tok; tok := token_get (); regex_1_r (tok, REGstar reg0))
        else if s[0] = 43 (* '+' *) then
	  (free@ {0} tok; tok := token_get (); regex_1_r (tok, REGplus reg0))
        else (fold@ tok; reg0)
      end else (fold@ tok; reg0)
    end // end of [begin]
  (*
   * AS-20130227: these consecutive patterns cause atsopt to hang during compilation...
   * rewritten into the above code
  | ~TOKword @(n, s) when n = 1 andalso b2b (s[0] = 42 (* '*' *)) =>
      let val () = tok := token_get () in regex_1_r (tok, REGstar reg0) end
  | ~TOKword @(n, s) when n = 1 andalso b2b (s[0] = 43 (* "+" *)) =>
      let val () = tok := token_get () in regex_1_r (tok, REGplus reg0) end
  *)
  | ~TOKlit 63 (* '?' *) =>
      let val () = tok := token_get () in regex_1_r (tok, REGopt reg0) end
  | _ => reg0
end // end of [regex_1_r]

and regex_2 (tok: &token): regex = let
  val reg = regex_1 (tok)
in
  regex_2_r (tok, reg)
end // end of [regex_2]

and regex_2_r (tok: &token, reg0: regex): regex = begin
  if is_regex_0 (tok) then let
    val reg1 = regex_1 (tok) in regex_2_r (tok, REGseq (reg0, reg1))
  end else reg0
end // end of [regex_2_r]

and regex_3 (tok: &token): regex = let
  val reg = regex_2 (tok)
  val res = regex_3_r (tok, reg)
in
  res
end // end of [regex_3]

and regex_3_r (tok: &token, reg0: regex): regex =
  case+ tok of
  | TOKword @(n, s) =>
    if n = 1 andalso bool1_of_bool (s[0] = 124 (* "|" *)) then let
      val () = free@ {0} tok
      val () = tok := token_get (); val reg1 = regex_2 (tok)
    in
      regex_3_r (tok, REGalt (reg0, reg1))
    end else (fold@ tok; reg0)
    // end of [TOKword]
  | _ => reg0
  // end of [case]
// end of [regex_3_r]

val regex = regex_3

(* ****** ****** *)

fun redef_reverse (rds: redef): redef = let
  fun loop (rds1: redef, rds2: redef): redef =
    case+ rds1 of
    | ~redef_cons (id, reg, rds1) =>
        loop (rds1, redef_cons (id, reg, rds2))
    | ~redef_nil () => rds2
  // end of [loop]
in
  loop (rds, redef_nil ())
end // end of [redef_reverse]

fun ismrk {n:nat} (n: size_t n, s: string32_vt n): bool =
  if n = 2 then s[0] = 37 && s[1] = 37
  else false
// end of [ismrk]

fun redef (tok: &token, rds: redef): redef =
  case+ tok of
  | TOKword @(n, id) => begin
      if ~ismrk (n, id) (* id <> "%%" *) then let
        val () = free@ {0} (tok)
        val id = string_of_string32 (id, n)
(*
        val () = (prerr "redef: id = "; prerr id; prerr_newline ())
*)
        val () = tok := token_get ()
        val () = litword (tok, array_make_lst_vt (asz, 61(* '=' *) :: nil), asz) where {
          #define :: list_vt_cons
          #define nil list_vt_nil
          val asz = size1_of_int1 1
        } // end of [val]
        val reg = regex (tok)
(*
        val () = (prerr "redef: reg = "; prerr_regex reg; prerr_newline ())
*)
      in
        redef (tok, redef_cons (id, reg, rds))
      end else (fold@ tok; redef_reverse rds)
    end // end of [begin]
  | _ => redef_reverse rds
// end of [redef]

(* ****** ****** *)

fun rules_reverse (rls: rules): rules = let
  fun loop (rls1: rules, rls2: rules): rules =
    case+ rls1 of
    | ~rules_cons (r, s, rls1) =>
        loop (rls1, rules_cons (r, s, rls2))
    | ~rules_nil () => rls2
  // end of [loop]
in
  loop (rls, rules_nil ())
end // end of [rules_reverse]

fun barrules (tok: &token, rls: rules): rules =
  case+ tok of
  | ~TOKword @(n, s) when n = 1 andalso bool1_of_bool (s[0] = 124 (* "|" *)) => let
      val () = tok := token_get ()
      val reg = regex (tok)
      val cstr = code (tok)
(*
      val () = (prerr "rules: reg = "; prerr_regex reg; prerr_newline ())
      val () = (prerr "rules: cstr = "; prerr cstr; prerr_newline ())
*)
    in
      barrules (tok, rules_cons (reg, cstr, rls))
    end // end of [TOKword "|"]
  | _ => rules_reverse rls
// end of [barrules]

fun rules (tok: &token): rules =
  case+ tok of
  | TOKword @(n, s) when n = 1 andalso bool1_of_bool (s[0] = 124 (* "|" *)) => (fold@ tok; barrules (tok, rules_nil ()))
  | _ => let
      val reg = regex (tok)
(*
      val () = (prerr "rules: reg = "; prerr_regex reg; prerr_newline ())
*)
      val cstr = code (tok)
(*
      val () = (prerr "rules: cstr = "; prerr cstr; prerr_newline ())
*)
    in
      barrules (tok, rules_cons (reg, cstr, rules_nil ()))
    end // end of [_]
  // end of [case]
// end of [rules]

(* ****** ****** *)

fun lexfn_funarg (tok: &token): string = case+ tok of
  | ~TOKlit 40 (* '\(' *) => begin
      let val arg = tokenize_funarg () in tok := token_get (); arg end
    end
  | _ => ""
// end of [lexfn_funarg]

fun lexfns (tok: &token): lexfns = case+ tok of
  | ~TOKword @(n, id) when ~ismrk (n, id) (* id <> "%%" *) => let
(*
      val () = (prerr "lexfns: id = "; prerr id; prerr_newline ())
*)
      val id = string_of_string32 (id, n)
      val () = tok := token_get ()
      val arg = lexfn_funarg (tok)
      val () = litword (tok, array_make_lst_vt (asz, 61(* '=' *) :: nil), asz) where {
        #define :: list_vt_cons
        #define nil list_vt_nil
        val asz = size1_of_int1 1
      } // end of [val]
      val rls = rules (tok)
    in
      lexfns_cons (id, arg, rls, lexfns (tok))
    end // end of [TOKword id when ...]
  | _ => lexfns_nil ()
// end of [lexfns]

(* ****** ****** *)

fun preamble (tok: &token): string = let
(*
  val () = begin
    prerr "preamble: enter"; prerr_newline ()
  end
*)
  val result = case+ tok of
    | ~TOKmark "%{" => let
        val s = tokenize_logue ()
      in
        tok := token_get (); s
      end // end of [let]
    | _ => ""
  // end of [result]
(*
  val () = begin
    prerr "preamble: leave"; prerr_newline ()
  end // end of [val]
*)
in
  result
end // end of [preamble]

fun postamble (tok: token): string =
  case+ tok of
  | ~TOKword @(n, s) when ismrk (n, s) (* "%%" *) => let
      val s = tokenize_rest_text () in s
    end // end of [TOKword "%%"]
  | _ => (token_free tok; "")
// end of [postamble]

(* ****** ****** *)

fun done (tok: token): void =
  case+ tok of
  | ~TOKeof () => ()
  | _ => (token_free tok; errmsg_illegal {void} "done")
// end of [done()]

(* ****** ****** *)

implement lexer_parse () = let
  var tok = token_get ()
  val str1 = preamble (tok)
(*
  val () = (prerr "preamble =\n"; prerr str1; prerr_newline ())
*)
  val rds = redef (tok, redef_nil ())
  val () = litword (tok, array_make_lst_vt (asz, 37(* '%' *) :: 37(* '%' *) :: nil ()), asz) where {
    #define :: list_vt_cons
    #define nil list_vt_nil
    val asz = size1_of_int1 2
  } // end of [val]
  val lfs = lexfns (tok)
  val str2 = postamble (tok)
(*
  val () = (prerr "postamble =\n"; prerr str2; prerr_newline ())
*)
  // val () = done () // no need for this because of [postamble]
in '{
  preamble= str1, redef= rds, lexfns= lfs, postamble= str2
} end // end of [lexer_parse]

implement lexer_free (lex) = let
  fun redef_free (x: redef): void = case+ x of
    | ~redef_nil () => ()
    | ~redef_cons (_, _, x) => redef_free x
  // end of [redef_free]
  fun rules_free (x: rules): void = case+ x of
    | ~rules_nil () => ()
    | ~rules_cons (_, _, x) => rules_free x
  // end of [rules_free]
  fun lexfns_free (x: lexfns): void = case+ x of
    | ~lexfns_nil () => ()
    | ~lexfns_cons (_, _, rls, x) => (rules_free rls; lexfns_free x)
  // end of [lexfns_free]
  val () = redef_free lex.redef
  val () = lexfns_free lex.lexfns
in
  // empty
end // end of [lexer_free]

(* ****** ****** *)

(* end of [parser.dats] *)
