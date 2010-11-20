(*
**
** For documenting the grammar of ATS
**
** Contributed by Hongwei Xi (hwxi AT cs DOT bu DOT edu)
** Contributed by Sylvain Nahas (sylvain.nahas AT googlemail DOT com)
**
** Time: November, 2010
**
*)

(* ****** ****** *)

staload _(*anon*) = "prelude/DATS/list.dats"
staload _(*anon*) = "prelude/DATS/list_vt.dats"

(* ****** ****** *)

staload "atsgrammar.sats"

(* ****** ****** *)

val theExternHeader = "\
#include <stdio.h> // for [fprintf]\n\
#include \"ats_memory.h\" // HX: loading [ats_types.h] as well\n\
#define malloc ats_malloc_ngc\n\
#define realloc ats_realloc_ngc\n\
\n\
/* ****** ****** */\n\
\n\
typedef ats_ptr_type c0har_t ;\n\
typedef ats_ptr_type e0xtcode_t ;\n\
typedef ats_ptr_type f0loat_t ;\n\
typedef ats_ptr_type f0loatsp_t ;\n\
typedef ats_ptr_type i0nt_t ;\n\
typedef ats_ptr_type i0ntsp_t ;\n\
typedef ats_ptr_type s0tring_t ;\n\
typedef ats_ptr_type t0kn_t ;\n\
\n\
typedef ats_ptr_type abskind_t ;\n\
typedef ats_ptr_type dcstkind_t ;\n\
typedef ats_ptr_type datakind_t ;\n\
typedef ats_ptr_type stadefkind_t ;\n\
typedef ats_ptr_type valkind_t ;\n\
typedef ats_ptr_type funkind_t ;\n\
typedef ats_ptr_type lamkind_t ;\n\
typedef lamkind_t fixkind_t ;\n\
typedef ats_ptr_type srpifkindtok_t ;\n\
\n\
typedef ats_ptr_type i0de_t ;\n\
typedef ats_ptr_type i0delst_t ;\n\
typedef ats_ptr_type i0delstlst_t ;\n\
typedef ats_ptr_type i0dext_t ;\n\
typedef ats_ptr_type l0ab_t ;\n\
\n\
typedef ats_ptr_type p0rec_t ;\n\
\n\
typedef ats_ptr_type e0xp_t ;\n\
typedef ats_ptr_type e0xplst_t ;\n\
typedef ats_ptr_type e0xpopt_t ;\n\
\n\
typedef ats_ptr_type e0fftag_t ;\n\
typedef ats_ptr_type e0fftaglst_t ;\n\
typedef ats_ptr_type e0fftaglstopt_t ;\n\
\n\
typedef ats_ptr_type s0rtq_t ;\n\
typedef ats_ptr_type s0rt_t ;\n\
typedef ats_ptr_type s0rtlst_t ;\n\
typedef ats_ptr_type s0rtopt_t ;\n\
typedef ats_ptr_type s0rtpol_t ;\n\
\n\
typedef ats_ptr_type d0atsrtcon_t ;\n\
typedef ats_ptr_type d0atsrtconlst_t ;\n\
typedef ats_ptr_type d0atsrtdec_t ;\n\
typedef ats_ptr_type d0atsrtdeclst_t ;\n\
\n\
typedef ats_ptr_type s0taq_t ;\n\
typedef ats_ptr_type d0ynq_t ;\n\
typedef ats_ptr_type sqi0de_t ;\n\
typedef ats_ptr_type dqi0de_t ;\n\
typedef ats_ptr_type arrqi0de_t ;\n\
typedef ats_ptr_type tmpqi0de_t ;\n\
typedef ats_ptr_type s0arg_t ;\n\
typedef ats_ptr_type s0arglst_t ;\n\
typedef ats_ptr_type s0arglstlst_t ;\n\
typedef ats_ptr_type s0exp_t ;\n\
typedef ats_ptr_type s0explst_t ;\n\
typedef ats_ptr_type s0expopt_t ;\n\
typedef ats_ptr_type s0explstlst_t ;\n\
typedef ats_ptr_type s0explstopt_t ;\n\
typedef ats_ptr_type labs0explst_t ;\n\
typedef ats_ptr_type s0arrind_t ;\n\
typedef ats_ptr_type t1mps0explstlst_t ; // with location information\n\
typedef ats_ptr_type s0rtext_t ;\n\
typedef ats_ptr_type s0qua_t ;\n\
typedef ats_ptr_type s0qualst_t ;\n\
typedef ats_ptr_type s0qualstlst_t ;\n\
typedef ats_ptr_type s0qualstopt_t ;\n\
typedef ats_ptr_type impqi0de_t ;\n\
\n\
typedef ats_ptr_type sp0at_t ;\n\
\n\
typedef ats_ptr_type d0atarg_t ;\n\
typedef ats_ptr_type d0atarglst_t ;\n\
typedef ats_ptr_type s0rtdef_t ;\n\
typedef ats_ptr_type s0rtdeflst_t ;\n\
typedef ats_ptr_type s0tacon_t ;\n\
typedef ats_ptr_type s0taconlst_t ;\n\
typedef ats_ptr_type s0tacst_t ;\n\
typedef ats_ptr_type s0tacstlst_t ;\n\
typedef ats_ptr_type s0tavar_t ;\n\
typedef ats_ptr_type s0tavarlst_t ;\n\
typedef ats_ptr_type s0expdef_t ;\n\
typedef ats_ptr_type s0expdeflst_t ;\n\
typedef ats_ptr_type s0aspdec_t ;\n\
typedef ats_ptr_type d0atcon_t ;\n\
typedef ats_ptr_type d0atconlst_t ;\n\
typedef ats_ptr_type d0atdec_t ;\n\
typedef ats_ptr_type d0atdeclst_t ;\n\
typedef ats_ptr_type e0xndec_t ;\n\
typedef ats_ptr_type e0xndeclst_t ;\n\
\n\
typedef ats_ptr_type p0arg_t ;\n\
typedef ats_ptr_type p0arglst_t ;\n\
typedef ats_ptr_type d0arg_t ;\n\
typedef ats_ptr_type d0arglst_t ;\n\
typedef ats_ptr_type m0acarg_t ;\n\
typedef ats_ptr_type m0acarglst_t ;\n\
typedef ats_ptr_type extnamopt_t ;\n\
typedef ats_ptr_type d0cstdec_t ;\n\
typedef ats_ptr_type d0cstdeclst_t ;\n\
typedef ats_ptr_type p0at_t ;\n\
typedef ats_ptr_type p0atlst_t ;\n\
typedef ats_ptr_type labp0atlst_t ;\n\
typedef ats_ptr_type s0vararg_t ;\n\
typedef ats_ptr_type s0exparg_t ;\n\
typedef ats_ptr_type f0arg_t ;\n\
typedef ats_ptr_type f0arglst_t ;\n\
typedef ats_ptr_type s0elop_t ;\n\
typedef ats_ptr_type witht0ype_t ;\n\
typedef ats_ptr_type d0exp_t ;\n\
typedef ats_ptr_type d0explst_t ;\n\
typedef ats_ptr_type d0expopt_t ;\n\
typedef ats_ptr_type labd0explst_t ;\n\
typedef ats_ptr_type d0arrind_t ;\n\
typedef ats_ptr_type ifhead_t ;\n\
typedef ats_ptr_type casehead_t ;\n\
typedef ats_ptr_type loophead_t ;\n\
typedef ats_ptr_type tryhead_t ;\n\
typedef ats_ptr_type m0atch_t ;\n\
typedef ats_ptr_type m0atchlst_t ;\n\
typedef ats_ptr_type guap0at_t ;\n\
typedef ats_ptr_type c0lau_t ;\n\
typedef ats_ptr_type c0laulst_t ;\n\
typedef ats_ptr_type sc0lau_t ;\n\
typedef ats_ptr_type sc0laulst_t ;\n\
typedef ats_ptr_type i0nvarg_t ;\n\
typedef ats_ptr_type i0nvarglst_t ;\n\
typedef ats_ptr_type i0nvresstate_t ;\n\
typedef ats_ptr_type loopi0nv_t ;\n\
typedef ats_ptr_type initestpost_t ;\n\
typedef ats_ptr_type v0aldec_t ;\n\
typedef ats_ptr_type v0aldeclst_t ;\n\
typedef ats_ptr_type f0undec_t ;\n\
typedef ats_ptr_type f0undeclst_t ;\n\
typedef ats_ptr_type v0arwth_t ;\n\
typedef ats_ptr_type v0ardec_t ;\n\
typedef ats_ptr_type v0ardeclst_t ;\n\
typedef ats_ptr_type m0acdef_t ;\n\
typedef ats_ptr_type m0acdeflst_t ;\n\
typedef ats_ptr_type i0mpdec_t ;\n\
typedef ats_ptr_type d0ec_t ;\n\
typedef ats_ptr_type d0eclst_t ;\n\
typedef ats_ptr_type d0ecllst_t ;\n\
typedef ats_ptr_type guad0ec_t ;\n\
\n\
/* ****** ****** */\n\
\n\
extern abskind_t abskind_prop (void) ;\n\
extern abskind_t abskind_type (void) ;\n\
extern abskind_t abskind_t0ype (void) ;\n\
extern abskind_t abskind_view (void) ;\n\
extern abskind_t abskind_viewtype (void) ;\n\
extern abskind_t abskind_viewt0ype (void) ;\n\
\n\
extern dcstkind_t dcstkind_fun (void) ;\n\
extern dcstkind_t dcstkind_val (void) ;\n\
extern dcstkind_t dcstkind_castfn (void) ;\n\
extern dcstkind_t dcstkind_praxi (void) ;\n\
extern dcstkind_t dcstkind_prfun (void) ;\n\
extern dcstkind_t dcstkind_prval (void) ;\n\
\n\
extern datakind_t datakind_prop (void) ;\n\
extern datakind_t datakind_type (void) ;\n\
extern datakind_t datakind_view (void) ;\n\
extern datakind_t datakind_viewtype (void) ;\n\
\n\
extern stadefkind_t stadefkind_generic (void) ;\n\
extern stadefkind_t stadefkind_prop (t0kn_t) ;\n\
extern stadefkind_t stadefkind_type (t0kn_t) ;\n\
extern stadefkind_t stadefkind_view (t0kn_t) ;\n\
extern stadefkind_t stadefkind_viewtype (t0kn_t) ;\n\
\n\
extern valkind_t valkind_val (void) ;\n\
extern valkind_t valkind_valminus (void) ;\n\
extern valkind_t valkind_valplus (void) ;\n\
extern valkind_t valkind_prval (void) ;\n\
\n\
extern funkind_t funkind_fn (void) ;\n\
extern funkind_t funkind_fnstar (void) ;\n\
extern funkind_t funkind_fun (void) ;\n\
extern funkind_t funkind_castfn (void) ;\n\
extern funkind_t funkind_prfn (void) ;\n\
extern funkind_t funkind_prfun (void) ;\n\
\n\
extern lamkind_t lamkind_lam (t0kn_t) ;\n\
extern lamkind_t lamkind_atlam (t0kn_t) ;\n\
extern lamkind_t lamkind_llam (t0kn_t) ;\n\
extern lamkind_t lamkind_atllam (t0kn_t) ;\n\
extern fixkind_t fixkind_fix (t0kn_t) ;\n\
extern fixkind_t fixkind_atfix (t0kn_t) ;\n\
\n\
extern srpifkindtok_t srpifkindtok_if (t0kn_t) ;\n\
extern srpifkindtok_t srpifkindtok_ifdef (t0kn_t) ;\n\
extern srpifkindtok_t srpifkindtok_ifndef (t0kn_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern i0de_t i0de_make_ampersand (t0kn_t) ;\n\
extern i0de_t i0de_make_backslash (t0kn_t) ;\n\
extern i0de_t i0de_make_bang (t0kn_t) ;\n\
extern i0de_t i0de_make_eq (t0kn_t) ;\n\
extern i0de_t i0de_make_gt (t0kn_t) ;\n\
extern i0de_t i0de_make_lrbrackets (t0kn_t, t0kn_t) ;\n\
extern i0de_t i0de_make_lt (t0kn_t) ;\n\
extern i0de_t i0de_make_minusgt (t0kn_t) ;\n\
extern i0de_t i0de_make_minuslt (t0kn_t) ;\n\
extern i0de_t i0de_make_minusltgt (t0kn_t) ;\n\
extern i0de_t i0de_make_module (t0kn_t) ;\n\
extern i0de_t i0de_make_r0ead (t0kn_t) ;\n\
extern i0de_t i0de_make_tilda (t0kn_t) ;\n\
extern i0de_t i0de_make_t0ype (t0kn_t) ;\n\
extern i0de_t i0de_make_viewt0ype (t0kn_t) ;\n\
\n\
extern i0de_t i0de_make_DO (t0kn_t) ;\n\
extern i0de_t i0de_make_IN (t0kn_t) ;\n\
extern i0de_t i0de_make_WHILE (t0kn_t) ;\n\
\n\
extern i0delst_t i0delst_nil (void) ;\n\
extern i0delst_t i0delst_sing (i0de_t) ;\n\
extern i0delst_t i0delst_cons (i0de_t, i0delst_t) ;\n\
\n\
extern i0delstlst_t i0delstlst_nil (void) ;\n\
extern i0delstlst_t i0delstlst_cons (i0delst_t, i0delstlst_t) ;\n\
\n\
extern l0ab_t l0ab_ide (i0de_t) ;\n\
extern l0ab_t l0ab_int (i0nt_t) ;\n\
\n\
extern i0de_t stai0de_make (i0de_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern p0rec_t p0rec_emp (void) ;\n\
extern p0rec_t p0rec_ide (i0de_t) ;\n\
extern p0rec_t p0rec_int (i0nt_t) ;\n\
extern p0rec_t p0rec_opr (i0de_t, i0de_t/*opr*/, i0nt_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern e0xp_t e0xp_app (e0xp_t, e0xp_t) ;\n\
extern e0xp_t e0xp_char (c0har_t) ;\n\
extern e0xp_t e0xp_eval (t0kn_t, e0xp_t, t0kn_t) ;\n\
extern e0xp_t e0xp_float (f0loat_t) ;\n\
extern e0xp_t e0xp_ide (i0de_t) ;\n\
extern e0xp_t e0xp_int (i0nt_t) ;\n\
extern e0xp_t e0xp_list (t0kn_t, e0xplst_t, t0kn_t) ;\n\
extern e0xp_t e0xp_string (s0tring_t) ;\n\
extern e0xplst_t e0xplst_nil (void) ;\n\
extern e0xplst_t e0xplst_cons (e0xp_t, e0xplst_t) ;\n\
extern e0xpopt_t e0xpopt_none (void) ;\n\
extern e0xpopt_t e0xpopt_some (e0xp_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern e0fftag_t e0fftag_cst (int, i0de_t) ;\n\
extern e0fftag_t e0fftag_var (i0de_t) ;\n\
extern e0fftag_t e0fftag_var_fun (t0kn_t) ;\n\
extern e0fftag_t e0fftag_int (i0nt_t) ;\n\
extern e0fftaglst_t e0fftaglst_nil (void) ;\n\
extern e0fftaglst_t e0fftaglst_cons (e0fftag_t, e0fftaglst_t) ;\n\
extern e0fftaglstopt_t e0fftaglstopt_none (void) ;\n\
extern e0fftaglstopt_t e0fftaglstopt_some (e0fftaglst_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern s0rtq_t s0rtq_str (s0tring_t) ;\n\
extern s0rtq_t s0rtq_sym (i0de_t) ;\n\
\n\
extern s0rt_t s0rt_prop (t0kn_t) ;\n\
extern s0rt_t s0rt_type (t0kn_t) ;\n\
extern s0rt_t s0rt_t0ype (t0kn_t) ;\n\
extern s0rt_t s0rt_view (t0kn_t) ;\n\
extern s0rt_t s0rt_viewtype (t0kn_t) ;\n\
extern s0rt_t s0rt_viewt0ype (t0kn_t) ;\n\
\n\
extern s0rt_t s0rt_app (s0rt_t, s0rt_t) ;\n\
extern s0rt_t s0rt_ide (i0de_t) ;\n\
extern s0rt_t s0rt_qid (s0rtq_t, i0de_t) ;\n\
extern s0rt_t s0rt_list (t0kn_t, s0rtlst_t, t0kn_t) ;\n\
extern s0rt_t s0rt_tup (t0kn_t, s0rtlst_t, t0kn_t) ;\n\
\n\
extern s0rtlst_t s0rtlst_nil (void) ;\n\
extern s0rtlst_t s0rtlst_cons (s0rt_t, s0rtlst_t) ;\n\
\n\
extern s0rtopt_t s0rtopt_none (void) ;\n\
extern s0rtopt_t s0rtopt_some (s0rt_t) ;\n\
\n\
extern s0rtpol_t s0rtpol_make (s0rt_t, int) ;\n\
\n\
/* ****** ****** */\n\
\n\
/*\n\
** datasort declaration\n\
*/\n\
extern d0atsrtcon_t d0atsrtcon_make_none (i0de_t) ;\n\
extern d0atsrtcon_t d0atsrtcon_make_some (i0de_t, s0rt_t) ;\n\
extern d0atsrtconlst_t d0atsrtconlst_nil (void) ;\n\
extern d0atsrtconlst_t d0atsrtconlst_cons (d0atsrtcon_t, d0atsrtconlst_t) ;\n\
extern d0atsrtdec_t d0atsrtdec_make (i0de_t, d0atsrtconlst_t) ;\n\
extern d0atsrtdeclst_t d0atsrtdeclst_nil (void) ;\n\
extern d0atsrtdeclst_t d0atsrtdeclst_cons (d0atsrtdec_t, d0atsrtdeclst_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
/*\n\
** static qualifiers\n\
*/\n\
extern s0taq_t s0taq_symdot (i0de_t) ;\n\
extern s0taq_t s0taq_symcolon (i0de_t) ;\n\
extern s0taq_t s0taq_fildot (s0tring_t) ;\n\
\n\
/*\n\
** dynamic qualifiers\n\
*/\n\
extern d0ynq_t d0ynq_symcolon(i0de_t) ;\n\
extern d0ynq_t d0ynq_symdot(i0de_t) ;\n\
extern d0ynq_t d0ynq_symdot_symcolon(i0de_t, i0de_t) ;\n\
extern d0ynq_t d0ynq_fildot(s0tring_t) ;\n\
extern d0ynq_t d0ynq_fildot_symcolon(s0tring_t, i0de_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
/*\n\
** (qualified) static identifiers\n\
*/\n\
extern sqi0de_t sqi0de_make_none (i0de_t) ;\n\
extern sqi0de_t sqi0de_make_some (s0taq_t, i0de_t) ;\n\
\n\
/*\n\
** (qualified) dynamic identifiers\n\
*/\n\
extern dqi0de_t dqi0de_make_none (i0de_t) ;\n\
extern dqi0de_t dqi0de_make_some (d0ynq_t, i0de_t) ;\n\
\n\
/*\n\
** (qualified) array identifiers\n\
*/\n\
extern arrqi0de_t arrqi0de_make_none (i0de_t) ;\n\
extern arrqi0de_t arrqi0de_make_some (d0ynq_t, i0de_t) ;\n\
\n\
/*\n\
** (qualified) template identifiers\n\
*/\n\
extern tmpqi0de_t tmpqi0de_make_none (i0de_t) ;\n\
extern tmpqi0de_t tmpqi0de_make_some (d0ynq_t, i0de_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
/*\n\
** static arguments\n\
*/\n\
extern s0arg_t s0arg_make (i0de_t, s0rtopt_t) ;\n\
extern s0arg_t s0arg_make_none (i0de_t) ;\n\
extern s0arglst_t s0arglst_nil (void) ;\n\
extern s0arglst_t s0arglst_cons (s0arg_t, s0arglst_t) ;\n\
extern s0arglstlst_t s0arglstlst_nil (void) ;\n\
extern s0arglstlst_t s0arglstlst_cons (s0arglst_t, s0arglstlst_t) ;\n\
extern s0arglstlst_t s0arglstlst_cons_ide (i0de_t, s0arglstlst_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern impqi0de_t impqi0de_make_none (dqi0de_t) ;\n\
extern impqi0de_t impqi0de_make_some\n\
  (tmpqi0de_t, s0explst_t, t1mps0explstlst_t, t0kn_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern sp0at_t sp0at_con (sqi0de_t, s0arglst_t, t0kn_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
/*\n\
** static expressions\n\
*/\n\
extern s0exp_t s0exp_ann (s0exp_t, s0rt_t) ;\n\
extern s0exp_t s0exp_app (s0exp_t, s0exp_t) ;\n\
extern s0exp_t s0exp_char (c0har_t) ;\n\
extern s0exp_t s0exp_exi (t0kn_t, int/*funres*/, s0qualst_t, t0kn_t) ;\n\
extern s0exp_t s0exp_extern (t0kn_t, s0tring_t) ;\n\
extern s0exp_t s0exp_ide (i0de_t) ;\n\
extern s0exp_t s0exp_imp (t0kn_t, e0fftaglst_t, t0kn_t) ;\n\
extern s0exp_t s0exp_imp_emp (t0kn_t) ;\n\
extern s0exp_t s0exp_int (i0nt_t) ;\n\
extern s0exp_t s0exp_intsp_err (i0nt_t) ; /* error handling */\n\
extern s0exp_t s0exp_lams (t0kn_t, s0arglstlst_t, s0rtopt_t, s0exp_t) ;\n\
extern s0exp_t s0exp_list (t0kn_t, s0explst_t, t0kn_t) ;\n\
extern s0exp_t s0exp_list2 (t0kn_t, s0explst_t, s0explst_t, t0kn_t) ;\n\
extern s0exp_t s0exp_named (i0de_t, s0exp_t) ;\n\
extern s0exp_t s0exp_opide (t0kn_t, i0de_t) ;\n\
extern s0exp_t s0exp_qid (s0taq_t, i0de_t) ;\n\
extern s0exp_t s0exp_struct (t0kn_t, labs0explst_t, t0kn_t) ;\n\
extern s0exp_t s0exp_tyarr (t0kn_t, s0exp_t, s0arrind_t) ;\n\
extern s0exp_t s0exp_tyrec (int, t0kn_t, labs0explst_t, t0kn_t) ;\n\
extern s0exp_t s0exp_tyrec_ext (t0kn_t, s0tring_t, labs0explst_t, t0kn_t) ;\n\
extern s0exp_t s0exp_tytup (int, t0kn_t, s0explst_t, t0kn_t) ;\n\
extern s0exp_t s0exp_tytup2 (int, t0kn_t, s0explst_t, s0explst_t, t0kn_t) ;\n\
extern s0exp_t s0exp_uni (t0kn_t, s0qualst_t, t0kn_t) ;\n\
extern s0exp_t s0exp_union (t0kn_t, s0exp_t, labs0explst_t, t0kn_t) ;\n\
\n\
extern s0explst_t s0explst_nil (void) ;\n\
extern s0explst_t s0explst_cons (s0exp_t, s0explst_t) ;\n\
\n\
extern s0expopt_t s0expopt_none (void) ;\n\
extern s0expopt_t s0expopt_some (s0exp_t) ;\n\
\n\
extern s0explstlst_t s0explstlst_nil (void) ;\n\
extern s0explstlst_t s0explstlst_cons (s0explst_t, s0explstlst_t) ;\n\
\n\
extern s0explstopt_t s0explstopt_none (void) ;\n\
extern s0explstopt_t s0explstopt_some (s0explst_t) ;\n\
\n\
extern labs0explst_t labs0explst_nil (void) ;\n\
extern labs0explst_t labs0explst_cons (l0ab_t, s0exp_t, labs0explst_t) ;\n\
\n\
extern s0arrind_t s0arrind_make_sing (s0explst_t, t0kn_t) ;\n\
extern s0arrind_t s0arrind_make_cons (s0explst_t, s0arrind_t) ;\n\
\n\
extern t1mps0explstlst_t gtlt_t1mps0expseqseq_nil (void) ;\n\
extern t1mps0explstlst_t\n\
gtlt_t1mps0expseqseq_cons_tok (t0kn_t, s0explst_t, t1mps0explstlst_t) ;\n\
// end of [extern]\n\
\n\
/* ****** ****** */\n\
\n\
extern s0rtext_t s0rtext_srt (s0rt_t) ;\n\
extern s0rtext_t s0rtext_sub\n\
  (t0kn_t, i0de_t, s0rtext_t, s0exp_t, s0explst_t, t0kn_t) ;\n\
\n\
extern s0qua_t s0qua_prop(s0exp_t) ;\n\
extern s0qua_t s0qua_vars(i0de_t, i0delst_t, s0rtext_t) ;\n\
extern s0qualst_t s0qualst_nil (void) ;\n\
extern s0qualst_t s0qualst_cons (s0qua_t, s0qualst_t) ;\n\
extern s0qualstlst_t s0qualstlst_nil (void) ;\n\
extern s0qualstlst_t s0qualstlst_cons (s0qualst_t, s0qualstlst_t) ;\n\
extern s0qualstopt_t s0qualstopt_none (void) ;\n\
extern s0qualstopt_t s0qualstopt_some (s0qualst_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern d0atarg_t d0atarg_srt (s0rtpol_t) ;\n\
extern d0atarg_t d0atarg_id_srt (i0de_t, s0rtpol_t) ;\n\
extern d0atarglst_t d0atarglst_nil (void) ;\n\
extern d0atarglst_t d0atarglst_cons (d0atarg_t, d0atarglst_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern s0rtdef_t s0rtdef_make (i0de_t, s0rtext_t) ;\n\
extern s0rtdeflst_t s0rtdeflst_nil (void) ;\n\
extern s0rtdeflst_t s0rtdeflst_cons (s0rtdef_t, s0rtdeflst_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern s0tacon_t s0tacon_make_none_none (i0de_t) ;\n\
extern s0tacon_t s0tacon_make_some_none (i0de_t, d0atarglst_t, t0kn_t) ;\n\
extern s0tacon_t s0tacon_make_none_some (i0de_t, s0exp_t) ;\n\
extern s0tacon_t s0tacon_make_some_some (i0de_t, d0atarglst_t, s0exp_t) ;\n\
extern s0taconlst_t s0taconlst_nil (void) ;\n\
extern s0taconlst_t s0taconlst_cons (s0tacon_t, s0taconlst_t) ;\n\
\n\
extern s0tacst_t s0tacst_make_none (i0de_t, s0rt_t) ;\n\
extern s0tacst_t s0tacst_make_some (i0de_t, d0atarglst_t, s0rt_t) ;\n\
extern s0tacstlst_t s0tacstlst_nil (void) ;\n\
extern s0tacstlst_t s0tacstlst_cons (s0tacst_t, s0tacstlst_t) ;\n\
\n\
extern s0tavar_t s0tavar_make (i0de_t, s0rt_t) ;\n\
extern s0tavarlst_t s0tavarlst_nil (void) ;\n\
extern s0tavarlst_t s0tavarlst_cons (s0tavar_t, s0tavarlst_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern s0expdef_t s0expdef_make (i0de_t, s0arglstlst_t, s0rtopt_t, s0exp_t) ;\n\
extern s0expdeflst_t s0expdeflst_nil (void) ;\n\
extern s0expdeflst_t s0expdeflst_cons (s0expdef_t, s0expdeflst_t) ;\n\
//\n\
extern s0aspdec_t s0aspdec_make (i0de_t, s0arglstlst_t, s0rtopt_t, s0exp_t) ;\n\
//\n\
extern d0atcon_t\n\
d0atcon_make (s0qualstlst_t, i0de_t, s0explstopt_t, s0expopt_t) ;\n\
extern d0atconlst_t d0atconlst_nil (void) ;\n\
extern d0atconlst_t d0atconlst_cons (d0atcon_t, d0atconlst_t) ;\n\
//\n\
extern d0atdec_t d0atdec_make_none (i0de_t, d0atconlst_t) ;\n\
extern d0atdec_t\n\
d0atdec_make_some (i0de_t, d0atarglst_t, t0kn_t, d0atconlst_t) ;\n\
extern d0atdeclst_t d0atdeclst_nil (void) ;\n\
extern d0atdeclst_t d0atdeclst_cons (d0atdec_t, d0atdeclst_t) ;\n\
//\n\
extern e0xndec_t e0xndec_make (s0qualstlst_t, i0de_t, s0expopt_t) ;\n\
extern e0xndeclst_t e0xndeclst_nil (void) ;\n\
extern e0xndeclst_t e0xndeclst_cons (e0xndec_t, e0xndeclst_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern p0arg_t p0arg_make_none (i0de_t) ;\n\
extern p0arg_t p0arg_make_some (i0de_t, s0exp_t) ;\n\
extern p0arglst_t p0arglst_nil (void) ;\n\
extern p0arglst_t p0arglst_cons (p0arg_t, p0arglst_t) ;\n\
//\n\
extern d0arg_t d0arg_var (i0de_t) ;\n\
extern d0arg_t d0arg_dyn (t0kn_t, p0arglst_t, t0kn_t) ;\n\
extern d0arg_t d0arg_dyn2 (t0kn_t, p0arglst_t, p0arglst_t, t0kn_t) ;\n\
extern d0arg_t d0arg_sta (t0kn_t, s0qualst_t, t0kn_t) ;\n\
extern d0arglst_t d0arglst_nil (void) ;\n\
extern d0arglst_t d0arglst_cons (d0arg_t, d0arglst_t) ;\n\
//\n\
extern m0acarg_t m0acarg_one (i0de_t) ;\n\
extern m0acarg_t m0acarg_lst (t0kn_t, i0delst_t, t0kn_t) ;\n\
extern m0acarglst_t m0acarglst_nil () ;\n\
extern m0acarglst_t m0acarglst_cons (m0acarg_t, m0acarglst_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern extnamopt_t extnamopt_none (void) ;\n\
extern extnamopt_t extnamopt_some (s0tring_t) ;\n\
\n\
extern d0cstdec_t\n\
d0cstdec_make (i0de_t, d0arglst_t, e0fftaglstopt_t, s0exp_t, extnamopt_t) ;\n\
extern d0cstdeclst_t d0cstdeclst_nil (void) ;\n\
extern d0cstdeclst_t d0cstdeclst_cons (d0cstdec_t, d0cstdeclst_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern p0at_t p0at_ann (p0at_t, s0exp_t) ;\n\
extern p0at_t p0at_apps (p0at_t, p0atlst_t) ;\n\
extern p0at_t p0at_as (i0de_t, p0at_t) ;\n\
extern p0at_t p0at_char (c0har_t) ;\n\
extern p0at_t p0at_exist (t0kn_t, s0arglst_t, t0kn_t) ;\n\
extern p0at_t p0at_float (f0loat_t); \n\
extern p0at_t p0at_free (t0kn_t, p0at_t); \n\
extern p0at_t p0at_ide (i0de_t) ;\n\
extern p0at_t p0at_int (i0nt_t) ;\n\
extern p0at_t p0at_list (t0kn_t, p0atlst_t, t0kn_t) ;\n\
extern p0at_t p0at_list2 (t0kn_t, p0atlst_t, p0atlst_t, t0kn_t) ;\n\
extern p0at_t p0at_lst (t0kn_t, p0atlst_t, t0kn_t) ;\n\
extern p0at_t p0at_qid (d0ynq_t, i0de_t) ;\n\
extern p0at_t p0at_opide (t0kn_t, i0de_t) ;\n\
extern p0at_t p0at_rec (int, t0kn_t, labp0atlst_t, t0kn_t) ;\n\
extern p0at_t p0at_ref (t0kn_t, i0de_t); \n\
extern p0at_t p0at_refas (t0kn_t, i0de_t, p0at_t); \n\
extern p0at_t p0at_svararg (t0kn_t, s0vararg_t, t0kn_t) ;\n\
extern p0at_t p0at_string (s0tring_t) ;\n\
extern p0at_t p0at_tup (int, t0kn_t, p0atlst_t, t0kn_t) ;\n\
extern p0at_t p0at_tup2 (int, t0kn_t, p0atlst_t, p0atlst_t, t0kn_t) ;\n\
//\n\
extern p0atlst_t p0atlst_nil (void) ;\n\
extern p0atlst_t p0atlst_cons (p0at_t, p0atlst_t) ;\n\
//\n\
extern labp0atlst_t labp0atlst_nil (void) ;\n\
extern labp0atlst_t labp0atlst_dot (void) ;\n\
extern labp0atlst_t labp0atlst_cons (l0ab_t, p0at_t, labp0atlst_t) ;\n\
//\n\
extern s0vararg_t s0vararg_one (void) ;\n\
extern s0vararg_t s0vararg_all (void) ;\n\
extern s0vararg_t s0vararg_seq (s0arglst_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern s0exparg_t s0exparg_one (void) ;\n\
extern s0exparg_t s0exparg_all (void) ;\n\
extern s0exparg_t s0exparg_seq (s0explst_t) ;\n\
\n\
extern f0arg_t f0arg_sta1 (t0kn_t, s0qualst_t, t0kn_t) ;\n\
extern f0arg_t f0arg_sta2 (t0kn_t, s0arglst_t, t0kn_t) ;\n\
extern f0arg_t f0arg_dyn (p0at_t) ;\n\
extern f0arg_t f0arg_met_none (t0kn_t) ;\n\
extern f0arg_t f0arg_met_some (t0kn_t, s0explst_t, t0kn_t) ;\n\
extern f0arglst_t f0arglst_nil (void) ;\n\
extern f0arglst_t f0arglst_cons (f0arg_t, f0arglst_t) ;\n\
\n\
extern s0elop_t s0elop_make (int, t0kn_t) ;\n\
\n\
extern witht0ype_t witht0ype_none (void) ;\n\
extern witht0ype_t witht0ype_prop (s0exp_t) ;\n\
extern witht0ype_t witht0ype_type (s0exp_t) ;\n\
extern witht0ype_t witht0ype_view (s0exp_t) ;\n\
extern witht0ype_t witht0ype_viewtype (s0exp_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
/*\n\
** dynamic expressions\n\
*/\n\
\n\
extern d0exp_t d0exp_ann (d0exp_t, s0exp_t) ;\n\
//\n\
extern d0exp_t d0exp_apps (d0exp_t, d0explst_t) ;\n\
//\n\
extern d0exp_t d0exp_arrinit_none\n\
  (t0kn_t, s0exp_t, d0explst_t /*elt*/, t0kn_t) ;\n\
extern d0exp_t d0exp_arrinit_some\n\
  (t0kn_t, s0exp_t, d0exp_t /*asz*/, d0explst_t /*elt*/, t0kn_t) ;\n\
//\n\
extern d0exp_t d0exp_arrsize\n\
  (t0kn_t, s0exp_t, t0kn_t/*lparen*/, d0explst_t, t0kn_t/*rparen*/) ;\n\
//\n\
extern d0exp_t d0exp_arrsub (arrqi0de_t, d0arrind_t) ;\n\
//\n\
extern d0exp_t d0exp_char (t0kn_t) ;\n\
//\n\
extern d0exp_t d0exp_caseof (casehead_t, d0exp_t, t0kn_t, c0laulst_t) ;\n\
//\n\
extern d0exp_t d0exp_crypt (int, t0kn_t) ;\n\
//\n\
extern d0exp_t d0exp_decseq (t0kn_t, d0eclst_t, t0kn_t) ;\n\
//\n\
extern d0exp_t d0exp_delay (int/*lin*/, t0kn_t) ;\n\
//\n\
extern d0exp_t d0exp_dynload (t0kn_t) ;\n\
//\n\
// HX: [d0exp_effmask_*] are implemented in [ats_effect.dats]\n\
//\n\
extern d0exp_t d0exp_effmask_all (t0kn_t) ;\n\
extern d0exp_t d0exp_effmask_exn (t0kn_t) ;\n\
extern d0exp_t d0exp_effmask_ntm (t0kn_t) ;\n\
extern d0exp_t d0exp_effmask_ref (t0kn_t) ;\n\
//\n\
extern d0exp_t d0exp_empty (void) ;\n\
//\n\
extern d0exp_t d0exp_exist (t0kn_t, s0exparg_t, t0kn_t, d0exp_t, t0kn_t) ;\n\
//\n\
extern d0exp_t d0exp_extval (t0kn_t, s0exp_t, s0tring_t, t0kn_t) ;\n\
//\n\
extern d0exp_t d0exp_fix\n\
  (fixkind_t, i0de_t, f0arglst_t, s0expopt_t, e0fftaglstopt_t, d0exp_t) ;\n\
//\n\
extern d0exp_t d0exp_float (f0loat_t) ;\n\
extern d0exp_t d0exp_floatsp (f0loatsp_t) ;\n\
//\n\
extern d0exp_t d0exp_foldat (t0kn_t, d0explst_t) ;\n\
//\n\
extern d0exp_t d0exp_for_itp (loophead_t, initestpost_t, d0exp_t) ;\n\
//\n\
extern d0exp_t d0exp_freeat (t0kn_t, d0explst_t) ;\n\
//\n\
extern d0exp_t d0exp_ide (i0de_t) ;\n\
extern d0exp_t d0exp_idext (i0de_t) ;\n\
//\n\
extern d0exp_t d0exp_if_none (ifhead_t, d0exp_t, d0exp_t) ;\n\
extern d0exp_t d0exp_if_some (ifhead_t, d0exp_t, d0exp_t, d0exp_t) ;\n\
//\n\
extern d0exp_t d0exp_int (i0nt_t) ;\n\
extern d0exp_t d0exp_intsp (i0ntsp_t) ;\n\
//\n\
extern\n\
d0exp_t d0exp_lam\n\
  (lamkind_t, f0arglst_t, s0expopt_t, e0fftaglstopt_t, d0exp_t) ;\n\
//\n\
extern\n\
d0exp_t d0exp_let_seq (t0kn_t, d0eclst_t, t0kn_t, d0explst_t, t0kn_t) ;\n\
//\n\
extern\n\
d0exp_t d0exp_list (t0kn_t, d0explst_t, t0kn_t) ;\n\
extern\n\
d0exp_t d0exp_list2 (t0kn_t, d0explst_t, d0explst_t, t0kn_t) ;\n\
//\n\
extern\n\
d0exp_t d0exp_lst (\n\
  int, t0kn_t, s0expopt_t, t0kn_t/*lparen*/, d0explst_t, t0kn_t/*rparen*/\n\
) ; // end of [d0exp_lst]\n\
extern d0exp_t d0exp_lst_quote (t0kn_t, d0explst_t, t0kn_t) ;\n\
//\n\
extern d0exp_t d0exp_loopexn (int, t0kn_t) ;\n\
//\n\
extern d0exp_t d0exp_macsyn_cross (t0kn_t, d0explst_t, t0kn_t) ;\n\
extern d0exp_t d0exp_macsyn_decode (t0kn_t, d0explst_t, t0kn_t) ;\n\
extern d0exp_t d0exp_macsyn_encode_seq (t0kn_t, d0explst_t, t0kn_t) ;\n\
//\n\
extern d0exp_t d0exp_opide (t0kn_t, i0de_t) ;\n\
extern d0exp_t d0exp_ptrof (t0kn_t) ;\n\
extern d0exp_t d0exp_qid (d0ynq_t, i0de_t) ;\n\
extern d0exp_t d0exp_raise (t0kn_t, d0exp_t) ;\n\
extern d0exp_t d0exp_rec (int, t0kn_t, labd0explst_t, t0kn_t) ;\n\
extern d0exp_t d0exp_scaseof (casehead_t, s0exp_t, t0kn_t, sc0laulst_t) ;\n\
extern d0exp_t d0exp_sel_lab (t0kn_t, l0ab_t) ;\n\
extern d0exp_t d0exp_sel_ind (t0kn_t, d0arrind_t) ;\n\
extern d0exp_t d0exp_seq (t0kn_t, d0explst_t, t0kn_t) ;\n\
extern d0exp_t d0exp_sexparg (t0kn_t, s0exparg_t, t0kn_t) ;\n\
extern d0exp_t d0exp_sif (ifhead_t, s0exp_t, d0exp_t, d0exp_t) ;\n\
extern d0exp_t d0exp_string (s0tring_t) ;\n\
extern d0exp_t d0exp_tmpid (tmpqi0de_t, s0explst_t, t1mps0explstlst_t, t0kn_t) ;\n\
extern d0exp_t d0exp_trywith_seq (tryhead_t, d0explst_t, t0kn_t, c0laulst_t) ;\n\
extern d0exp_t d0exp_tup (int, t0kn_t, d0explst_t, t0kn_t) ;\n\
extern d0exp_t d0exp_tup2 (int, t0kn_t, d0explst_t, d0explst_t, t0kn_t) ;\n\
extern d0exp_t d0exp_viewat (t0kn_t) ;\n\
extern d0exp_t d0exp_where (d0exp_t, d0eclst_t, t0kn_t) ;\n\
extern d0exp_t d0exp_while (loophead_t, d0exp_t, d0exp_t) ;\n\
//\n\
extern d0exp_t d0exp_FILENAME (t0kn_t) ; // a special string constant\n\
extern d0exp_t d0exp_LOCATION (t0kn_t) ; // a special string constant\n\
//\n\
extern d0explst_t d0explst_nil (void) ;\n\
extern d0explst_t d0explst_cons (d0exp_t, d0explst_t) ;\n\
extern d0explst_t d0explst_sing (d0exp_t) ;\n\
//\n\
extern d0expopt_t d0expopt_none (void) ;\n\
extern d0expopt_t d0expopt_some (d0exp_t) ;\n\
//\n\
extern labd0explst_t labd0explst_nil (void) ;\n\
extern labd0explst_t labd0explst_cons (l0ab_t, d0exp_t, labd0explst_t) ;\n\
//\n\
extern d0arrind_t d0arrind_make_sing (d0explst_t, t0kn_t) ;\n\
extern d0arrind_t d0arrind_make_cons (d0explst_t, d0arrind_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern ifhead_t ifhead_make (t0kn_t, i0nvresstate_t) ;\n\
extern casehead_t casehead_make (int, t0kn_t, i0nvresstate_t) ;\n\
extern loophead_t loophead_make_none (t0kn_t) ;\n\
extern loophead_t loophead_make_some (t0kn_t, loopi0nv_t, t0kn_t) ;\n\
extern tryhead_t tryhead_make (t0kn_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
/*\n\
** pattern matching\n\
*/\n\
extern m0atch_t m0atch_make_none (d0exp_t) ;\n\
extern m0atch_t m0atch_make_some (d0exp_t, p0at_t) ;\n\
extern m0atchlst_t m0atchlst_nil (void) ;\n\
extern m0atchlst_t m0atchlst_cons (m0atch_t, m0atchlst_t) ;\n\
\n\
extern guap0at_t guap0at_make_none (p0at_t) ;\n\
extern guap0at_t guap0at_make_some (p0at_t, d0exp_t) ;\n\
\n\
extern c0lau_t c0lau_make (guap0at_t, int, int, d0exp_t) ;\n\
extern c0laulst_t c0laulst_nil (void) ;\n\
extern c0laulst_t c0laulst_cons (c0lau_t, c0laulst_t) ;\n\
\n\
extern sc0lau_t sc0lau_make (sp0at_t, d0exp_t) ;\n\
extern sc0laulst_t sc0laulst_nil (void) ;\n\
extern sc0laulst_t sc0laulst_cons (sc0lau_t, sc0laulst_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern i0nvarg_t i0nvarg_make_none (i0de_t) ;\n\
extern i0nvarg_t i0nvarg_make_some (i0de_t, s0exp_t) ;\n\
\n\
extern i0nvarglst_t i0nvarglst_nil (void) ;\n\
extern i0nvarglst_t i0nvarglst_cons (i0nvarg_t, i0nvarglst_t) ;\n\
\n\
extern i0nvresstate_t i0nvresstate_none (void) ;\n\
extern i0nvresstate_t i0nvresstate_some (s0qualstopt_t, i0nvarglst_t) ;\n\
\n\
extern loopi0nv_t loopi0nv_make\n\
  (s0qualstopt_t, s0explstopt_t, i0nvarglst_t, i0nvresstate_t) ;\n\
\n\
extern initestpost_t initestpost_make\n\
  (t0kn_t, d0explst_t, t0kn_t, d0explst_t, t0kn_t, d0explst_t, t0kn_t) ;\n\
// end of [extern]\n\
\n\
/* ****** ****** */\n\
\n\
extern v0aldec_t v0aldec_make (p0at_t, d0exp_t, witht0ype_t) ;\n\
extern v0aldeclst_t v0aldeclst_nil (void) ;\n\
extern v0aldeclst_t v0aldeclst_cons (v0aldec_t, v0aldeclst_t) ;\n\
\n\
extern f0undec_t f0undec_make_none\n\
  (i0de_t, f0arglst_t, d0exp_t, witht0ype_t) ;\n\
extern f0undec_t f0undec_make_some\n\
  (i0de_t, f0arglst_t, e0fftaglstopt_t, s0exp_t, d0exp_t, witht0ype_t) ;\n\
extern f0undeclst_t f0undeclst_nil (void) ;\n\
extern f0undeclst_t f0undeclst_cons (f0undec_t, f0undeclst_t) ;\n\
\n\
extern v0arwth_t v0arwth_none () ;\n\
extern v0arwth_t v0arwth_some (i0de_t) ;\n\
\n\
extern v0ardec_t v0ardec_make_some_none\n\
  (int /*stadyn*/, i0de_t, v0arwth_t, s0exp_t) ;\n\
extern v0ardec_t v0ardec_make_none_some\n\
  (int /*stadyn*/, i0de_t, v0arwth_t, d0exp_t) ;\n\
extern v0ardec_t v0ardec_make_some_some\n\
  (int /*stadyn*/, i0de_t, s0exp_t, v0arwth_t, d0exp_t) ;\n\
extern v0ardeclst_t v0ardeclst_nil (void) ;\n\
extern v0ardeclst_t v0ardeclst_cons (v0ardec_t, v0ardeclst_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern m0acdef_t m0acdef_make (i0de_t, m0acarglst_t, d0exp_t) ;\n\
extern m0acdeflst_t m0acdeflst_nil (void) ;\n\
extern m0acdeflst_t m0acdeflst_cons (m0acdef_t, m0acdeflst_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
extern\n\
i0mpdec_t i0mpdec_make (impqi0de_t, f0arglst_t, s0expopt_t, d0exp_t) ;\n\
// end of [i0mpdec_make]\n\
 \n\
/* ****** ****** */\n\
\n\
extern d0ec_t d0ec_infix (t0kn_t, p0rec_t, int, i0delst_t) ;\n\
extern d0ec_t d0ec_prefix (t0kn_t, p0rec_t, i0delst_t) ;\n\
extern d0ec_t d0ec_postfix (t0kn_t, p0rec_t, i0delst_t) ;\n\
extern d0ec_t d0ec_nonfix (t0kn_t, i0delst_t) ;\n\
extern d0ec_t d0ec_symintr (t0kn_t, i0delst_t) ;\n\
extern d0ec_t d0ec_include (int/*0:sta/1:dyn*/, s0tring_t) ;\n\
extern d0ec_t d0ec_e0xpundef (i0de_t) ;\n\
extern d0ec_t d0ec_e0xpdef (i0de_t, e0xpopt_t) ;\n\
extern d0ec_t d0ec_e0xpact_assert (e0xp_t) ;\n\
extern d0ec_t d0ec_e0xpact_error (e0xp_t) ;\n\
extern d0ec_t d0ec_e0xpact_print (e0xp_t) ;\n\
extern d0ec_t d0ec_srtdefs (s0rtdef_t, s0rtdeflst_t) ;\n\
extern d0ec_t d0ec_datsrts (int/*para*/, d0atsrtdec_t, d0atsrtdeclst_t) ;\n\
extern d0ec_t d0ec_stacons (abskind_t, s0tacon_t, s0taconlst_t) ;\n\
extern d0ec_t d0ec_stacsts (s0tacst_t, s0tacstlst_t) ;\n\
extern d0ec_t d0ec_stavars (s0tavar_t, s0tavarlst_t) ;\n\
extern d0ec_t d0ec_sexpdefs (stadefkind_t, s0expdef_t, s0expdeflst_t) ;\n\
extern d0ec_t d0ec_propdefs (t0kn_t, s0expdef_t, s0expdeflst_t) ;\n\
extern d0ec_t d0ec_typedefs (t0kn_t, s0expdef_t, s0expdeflst_t) ;\n\
extern d0ec_t d0ec_viewdefs (t0kn_t, s0expdef_t, s0expdeflst_t) ;\n\
extern d0ec_t d0ec_viewtypedefs (t0kn_t, s0expdef_t, s0expdeflst_t) ;\n\
extern d0ec_t d0ec_saspdec (s0aspdec_t) ;\n\
extern d0ec_t d0ec_dcstdecs\n\
  (dcstkind_t, s0qualstlst_t, d0cstdec_t, d0cstdeclst_t) ;\n\
extern d0ec_t d0ec_datdecs\n\
  (datakind_t, d0atdec_t, d0atdeclst_t, s0explstopt_t) ;\n\
extern d0ec_t d0ec_exndecs (t0kn_t, e0xndec_t, e0xndeclst_t) ;\n\
//\n\
extern d0ec_t d0ec_classdec_none (t0kn_t, i0de_t) ;\n\
extern d0ec_t d0ec_classdec_some (t0kn_t, i0de_t, s0exp_t) ;\n\
//\n\
extern d0ec_t d0ec_overload (t0kn_t, i0de_t, dqi0de_t) ;\n\
extern d0ec_t d0ec_overload_lrbrackets (t0kn_t, t0kn_t, t0kn_t, dqi0de_t) ;\n\
//\n\
extern d0ec_t d0ec_dynload (s0tring_t) ;\n\
extern d0ec_t d0ec_staload_none (s0tring_t) ;\n\
extern d0ec_t d0ec_staload_some (i0de_t, s0tring_t) ;\n\
\n\
extern d0ec_t d0ec_extype (s0tring_t, s0exp_t) ;\n\
extern d0ec_t d0ec_extval (s0tring_t, d0exp_t) ;\n\
extern d0ec_t d0ec_extcode_dyn (e0xtcode_t) ;\n\
extern d0ec_t d0ec_extcode_sta (e0xtcode_t) ;\n\
extern d0ec_t d0ec_valdecs (valkind_t, v0aldec_t, v0aldeclst_t) ;\n\
extern d0ec_t d0ec_valdecs_par (v0aldec_t, v0aldeclst_t) ;\n\
extern d0ec_t d0ec_valdecs_rec (v0aldec_t, v0aldeclst_t) ;\n\
extern d0ec_t d0ec_fundecs (funkind_t, s0qualstlst_t, f0undec_t, f0undeclst_t) ;\n\
extern d0ec_t d0ec_vardecs (v0ardec_t, v0ardeclst_t) ;\n\
extern d0ec_t d0ec_macdefs (int, m0acdef_t, m0acdeflst_t) ;\n\
extern d0ec_t d0ec_impdec (t0kn_t, s0arglstlst_t, i0mpdec_t) ;\n\
\n\
extern d0ec_t d0ec_local (t0kn_t, d0eclst_t, d0eclst_t, t0kn_t) ;\n\
extern d0ec_t d0ec_guadec (srpifkindtok_t, guad0ec_t) ;\n\
\n\
extern guad0ec_t guad0ec_one (e0xp_t, d0eclst_t, t0kn_t) ;\n\
extern guad0ec_t guad0ec_two (e0xp_t, d0eclst_t, d0eclst_t, t0kn_t) ;\n\
extern guad0ec_t guad0ec_cons (e0xp_t, d0eclst_t, srpifkindtok_t, guad0ec_t) ;\n\
\n\
extern d0eclst_t d0eclst_nil (void) ;\n\
extern d0eclst_t d0eclst_cons (d0ec_t, d0eclst_t) ;\n\
extern d0ecllst_t d0ecllst_nil (void) ;\n\
extern d0ecllst_t d0ecllst_cons (d0ecllst_t, d0ec_t) ;\n\
extern d0eclst_t d0ecllst_reverse (d0ecllst_t) ;\n\
\n\
/* ****** ****** */\n\
\n\
" // end of [theExternHeader]

(* ****** ****** *)

val thePostamble = "\
int\n\
yylex_tok0 = -1 ;\n\
\n\
int\n\
yylex() {\n\
//\n\
  int tok ;\n\
//\n\
  if (yylex_tok0 >= 0) {\n\
    tok = yylex_tok0 ; yylex_tok0 = -1 ;\n\
  } else {\n\
    tok = atsopt_lexer_token_get () ;\n\
  } // end of [if]\n\
/*\n\
** fprintf (stdout, \"tok = %i\n\", tok) ;\n\
*/\n\
  return tok ;\n\
//\n\
} /* end of [yylex_tok0] */\n\
\n\
//\n\
// HX: needed in [ats_lexer.lats]\n\
//\n\
ats_void_type\n\
yylval_char_set(c0har_t val)\n\
  { yylval.c0har = val ; return ; }\n\
\n\
ats_void_type\n\
yylval_extcode_set(e0xtcode_t val)\n\
  { yylval.e0xtcode = val ; return ; }\n\
\n\
ats_void_type\n\
yylval_float_set(f0loat_t val)\n\
  { yylval.f0loat = val ; return ; }\n\
\n\
ats_void_type\n\
yylval_floatsp_set(f0loatsp_t val)\n\
  { yylval.f0loatsp = val ; return ; }\n\
\n\
ats_void_type\n\
yylval_ide_set(i0de_t val)\n\
  { yylval.i0de = val ; return ; }\n\
\n\
ats_void_type\n\
yylval_int_set(i0nt_t val)\n\
  { yylval.i0nt = val ; return ; }\n\
\n\
ats_void_type\n\
yylval_intsp_set(i0ntsp_t val)\n\
  { yylval.i0ntsp = val ; return ; }\n\
\n\
ats_void_type\n\
yylval_string_set(s0tring_t val)\n\
  { yylval.s0tring = val ; return ; }\n\
\n\
ats_void_type\n\
yylval_token_set(t0kn_t val)\n\
  { yylval.t0kn = val ; return ; }\n\
\n\
// HX: implemented in [ats_filename.dats]\n\
extern ats_void_type atsopt_filename_prerr () ;\n\
//\n\
extern ats_ptr_type lexing_fstpos_get () ;\n\
extern ats_void_type lexing_prerr_position (ats_ptr_type) ;\n\
//\n\
void\n\
yyerror(char *s) {\n\
  fprintf (stderr, \"%s: \", s) ;\n\
  atsopt_filename_prerr () ;\n\
  fprintf (stderr, \": [\") ;\n\
  lexing_prerr_position (lexing_fstpos_get ()) ;\n\
  fprintf (stderr, \"]\n\") ;\n\
  exit (1) ; // HX: no error recovery yet; maybe in future\n\
  return ;\n\
} /* end of [yyerror] */\n\
\n\
ats_ptr_type\n\
yyparse_main (\n\
  ats_int_type tok0\n\
) {\n\
/*\n\
** HX: must take care of garbage collection\n\
*/\n\
  // fprintf (stderr, \"yyparse_main: &yyss = %p\n\", &yyss) ;\n\
  // ATS_GC_MARKROOT (&yyss, sizeof(short*)) ; // [ats_malloc_ngc] is used\n\
  // fprintf (stderr, \"yyparse_main: &yyvs = %p\n\", &yyvs) ;\n\
  // ATS_GC_MARKROOT (&yyvs, sizeof(YYSTYPE*)) ;  // [ats_malloc_ngc] is used\n\
/*\n\
** HX-2010-02-25:\n\
** if BISON is used then [yyval] is a stack variable and\n\
** thus there is no need to treat it as a GC root explicitly\n\
*/\n\
  extern YYSTYPE yyval;\n\
  // fprintf (stderr, \"yyparse_main: &yyval = %p\n\", &yyval) ;\n\
  ATS_GC_MARKROOT (&yyval, sizeof(YYSTYPE)) ;\n\
//\n\
  extern YYSTYPE yylval;\n\
  // fprintf (stderr, \"yyparse_main: &yylval = %p\n\", &yylval) ;\n\
  ATS_GC_MARKROOT (&yylval, sizeof(YYSTYPE)) ;\n\
//\n\
  yylex_tok0 = tok0 ;\n\
//\n\
  yyparse () ;\n\
//\n\
  return yyval.d0eclst ;\n\
} /* end of [yyparse_main] */

/* ****** ****** */

" // end of [thePostamble]


(* ****** ****** *)

fun fprint_stropt
  (out: FILEref, x: Stropt): void =
  if stropt_is_some (x) then
    fprint_string (out, stropt_unsome (x))
  else ()
// end of [fprint_stropt]

(* ****** ****** *)

fun emit_YYSTYPE_union
  (out: FILEref, xs: !tynamelst_vt): void = let
  val () = fprint_string (out, "typedef union {\n")
  val () = loop (out, xs) where {
    fun loop (out: FILEref, xs: !tynamelst_vt): void =
      case+ xs of
      | list_vt_cons
          (x, !p_xs1) => let
//
          val def = tyname_get_def (x)
          val () = fprint_stropt (out, def)
          val () = fprint_string (out, " ")
          val nam = tyname_get_nam (x)
          val () = fprint_stropt (out, nam)
          val () = fprint_string (out, " ;\n")
//
          val () = loop (out, !p_xs1)
          val () = fold@ (xs)
        in
          // nothing
        end // end of [list_vt_cons]
      | list_vt_nil () => (fold@ xs)
    // end of [loop]
  } // end of [val]
  val () = fprint_string (out, "} YYSTYPE_union ;\n")
  val () = fprint_string (out, "#define YYSTYPE YYSTYPE_union\n")
in
  // nothing
end // end of [emit_YYSTYPE_union]

(* ****** ****** *)

fun emit_sym_term
  (out: FILEref, x: symbol): void = let
  val name = symbol_get_name (x)
  val tname = symbol_get_tyname (x)
  val () = fprint_string (out, "%token ")
  val () = if
    tyname_is_some (tname) then let
    val () = fprint_string (out, "<")
    val () = fprint_tyname (out, tname)
    val () = fprint_string (out, "> ")
  in
    // nothing
  end // end of [val]
  val () = fprint_string (out, name)
  val () = fprint_newline (out)
in
  // nothing
end // end of [emit_sym_term]

fun emit_symall_term (
  out: FILEref, xs: !symlst_vt
) : void = let
  fun loop (out: FILEref, xs: !symlst_vt): void =
    case+ xs of
    | list_vt_cons (x, !p_xs1) => let
        val isnt = symbol_get_nonterm (x)
        val () = if isnt then () else emit_sym_term (out, x)
        val () = loop (out, !p_xs1)
      in
        fold@ (xs)
      end // end of [list_vt_cons]
    | list_vt_nil () => (fold@ xs) // end of [list_vt_nil]
  // end of [loop]
in
  loop (out, xs)
end // end of [emit_symall_term]

(* ****** ****** *)

fun emit_sym_nonterm
  (out: FILEref, x: symbol): void = let
  val name = symbol_get_name (x)
  val tname = symbol_get_tyname (x)
  val () = fprint_string (out, "%type ")
  val () = if
    tyname_is_some (tname) then let
    val () = fprint_string (out, "<")
    val () = fprint_tyname (out, tname)
    val () = fprint_string (out, "> ")
  in
    // nothing
  end // end of [val]
  val () = fprint_string (out, name)
  val () = fprint_newline (out)
in
  // nothing
end // end of [emit_sym_nonterm]

fun emit_symall_nonterm (
  out: FILEref, xs: !symlst_vt
) : void = let
  fun loop (out: FILEref, xs: !symlst_vt): void =
    case+ xs of
    | list_vt_cons (x, !p_xs1) => let
        val isnt = symbol_get_nonterm (x)
        val () = if isnt then emit_sym_nonterm (out, x) else ()
        val () = loop (out, !p_xs1)
      in
        fold@ (xs)
      end // end of [list_vt_cons]
    | list_vt_nil () => (fold@ xs) // end of [list_vt_nil]
  // end of [loop]
in
  loop (out, xs)
end // end of [emit_symall_nonterm]

(* ****** ****** *)

fun emit_symreg
  (out: FILEref, r: symreg): void = case+ r of
  | SYMREGlit (x) => fprint_string (out, symbol_get_name (x))
  | _ => fprint_string (out, "(ERROR)")
// end of [emit_symreg]

fun emit_grmrule (
  out: FILEref, gr: grmrule
) : void = let
  fun loop (
    out: FILEref, xs: symreglst, i: int
  ) : void =
    case+ xs of
    | list_cons (x, xs) => let
        val () = if (i > 0) then fprint_string (out, " ")
        val () = emit_symreg (out, x)
      in
        loop (out, xs, i+1)
      end // end of [list_cons]
    | list_nil () => ()
  // end of [loop]
  val xs = grmrule_get_symreglst (gr)
  val () = (case+ xs of
    | list_cons _ => loop (out, xs, 0)
    | list_nil () => fprint_string (out, "/*(empty)*/")
  ) : void // end of [val]
  val action = grmrule_get_action (gr)
  val () = if
    stropt_is_some (action) then let
    val action = stropt_unsome (action)
  in
    fprintf (out, "  %s", @(action))
  end // end of [val]
in
  // nothing
end // end of [emit_grmrule]

(* ****** ****** *)

fun emit_sym_defn (
  out: FILEref, x: symbol
) : void = let
  fun loop (
    out: FILEref, grs: grmrulelst, i: &int
  ) : void =
    case+ grs of
    | list_cons (gr, grs) => let
        val knd = grmrule_get_kind (gr)
        val () = if knd = 0 then let
          val c = (
            if i = 0 then ':' else '|'
          ) : char // end of [val]
          val () = i := i+1
          val () = fprintf (out, "  %c ", @(c))
          val () = emit_grmrule (out, gr)
          val () = fprint_newline (out)
        in
          // nothing
        end // end of [val]
      in
        loop (out, grs, i)
      end // end of [list_cons]
    | list_nil () => ()
  // end of [loop]
//
  val name = symbol_get_name (x)
  val () = fprintf (out, "%s\n", @(name))
  var i: int = 0
  val () = loop (out, symbol_get_grmrulelst (x), i)
  val () = fprintf (out, "; /* %s */\n\n", @(name))
//
in
  // nothing  
end // end of [emit_sym_defn]

(* ****** ****** *)

fun emit_symall_defn (
  out: FILEref, xs: !symlst_vt
) : void = let
  fun loop (out: FILEref, xs: !symlst_vt): void =
    case+ xs of
    | list_vt_cons (x, !p_xs1) => let
        val isnt = symbol_get_nonterm (x)
        val () = if isnt then emit_sym_defn (out, x)
        val () = loop (out, !p_xs1)
      in
        fold@ (xs)
      end // end of [list_vt_cons]
    | list_vt_nil () => fold@ (xs) // end of [list_vt_nil]
  // end of [loop]
in
  loop (out, xs)
end // end of [emit_symall_defn]

(* ****** ****** *)

implement
emit_yats (out) = let
//
  val () = fprint_string (out, "%{\n\n")
//
  val () = fprint_string (out, theExternHeader)
//
  val () = () where {
    val xs = theTynamelst_get ()
    val xs = list_reverse (xs)
    val () = emit_YYSTYPE_union (out, xs)
    val () = list_vt_free (xs)
  } // end of [val]
//
  val () = fprint_string (out, "\n%}\n\n")
  val () = fprint_string (out, "/* ****** ****** */\n\n")
//
  val () = () where {
    val xs = theSymlst_get ()
    val xs = list_reverse (xs)
    val () = emit_symall_term (out, xs)
    val () = fprint_string (out, "\n/* ****** ****** */\n\n")
    val () = emit_symall_nonterm (out, xs)
    val () = fprint_string (out, "\n/* ****** ****** */\n\n")
    val () = fprint_string (out, "%start theStartEntry\n")
    val () = fprint_string (out, "\n/* ****** ****** */\n\n")
    val () = fprint_string (out, "%%\n")
    val () = fprint_string (out, "\n/* ****** ****** */\n\n")
    val () = emit_symall_defn (out, xs)
    val () = fprint_string (out, "\n/* ****** ****** */\n\n")
    val () = fprint_string (out, "%%\n")
    val () = fprint_string (out, "\n/* ****** ****** */\n\n")
    val () = list_vt_free (xs)
  } // end of [val]
//
  val () = fprint_string (out, thePostamble)
//
  val () = fprint_string (out, "// end of [atsgrammar.yats]")
  val () = fprint_newline (out)  
//
in
  // nothing
end // end of [emit_yats]

(* ****** ****** *)

(* end of [atsgrammar_emit_yats.dats] *)
