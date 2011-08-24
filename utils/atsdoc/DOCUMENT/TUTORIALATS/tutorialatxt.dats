(*
**
** Author: Hongwei Xi (gmhwxi AT gmail DOT com)
** Time: August, 2011
**
*)
//
// For write the TUTORIALATS book
//
staload UN = "prelude/SATS/unsafe.sats"
//
staload
STDIO = "libc/SATS/stdio.sats"
staload TIME = "libc/SATS/time.sats"
//
dynload "libatsdoc/dynloadall.dats"
//
staload "libatsdoc/SATS/atsdoc_text.sats"
//
val LT = "<"
val LTSLASH = "</"
val GT = ">"

val COMMENTopn = TEXTstrcst"<!--"
and COMMENTcls = TEXTstrcst("-->")

fun xmltagging (
  tag: string, x: string
) : text = let
  val _opn = TEXTappstr3 (LT, tag, GT)
  val _clo = TEXTappstr3 (LTSLASH, tag, GT)
in
  TEXTapptxt3 (_opn, TEXTstrsub(x), _clo)
end // end of [xmltagging]
//
fun id(x) = TEXTstrcst(x)
//
macdef title (x) = xmltagging ("title", ,(x))
//
macdef emph (x) = xmltagging ("emphasis", ,(x))
macdef para (x) = xmltagging ("para", ,(x))
//
macdef code (x) = xmltagging ("code", ,(x))
//
macdef command (x) = xmltagging ("command", ,(x))
//
local
val ATSCODEopn = "<informalexample><programlisting><![CDATA["
val ATSCODEcls = "]]></programlisting></informalexample>"
in
fun atscode
  (x: string): text = TEXTappstr3 (ATSCODEopn, x, ATSCODEcls)
(*
fun atscode2xmls (x: string): text = atscode2xml_strcode (0, x)
fun atscode2xmld (x: string): text = atscode2xml_strcode (1, x)
*)
end // end of [local]
//
(* ****** ****** *)

fun timestamp
  (): text = let
  var time = $TIME.time_get ()
  val (fpf | x) = $TIME.ctime (time)
  val x1 = sprintf ("%s", @($UN.castvwtp1(x)))
  prval () = fpf (x)
  val x1 = string_of_strptr (x1)
in
  TEXTstrcst (x1)
end // end of [val]

(* ****** ****** *)

fun ignore (x: string): text = TEXTnil ()

fun comment (x: string): text =
  TEXTapptxt3 (COMMENTopn, TEXTstrsub(x), COMMENTcls)
// end of [comment]

(* ****** ****** *)

(*
#define MYCODEROOT "http://www.ats-lang.org/DOCUMENT"
*)
#define MYCODEROOT "http://www.cs.bu.edu/~hwxi/ATS/DOCUMENT"

fun mycodelink (
  codepath: string, linkname: string
) : text = let
  val res = sprintf (
    "<ulink url=\"%s/TUTORIALATS/CODE/%s\">%s</ulink>", @(MYCODEROOT, codepath, linkname)
  ) // end of [val]
  val res = string_of_strptr (res)
in
  TEXTstrcst (res)
end // end of [mycodelink]

fun myatscodelink (
  codepath: string, linkname: string
) : text = let
  val res = sprintf (
    "<ulink url=\"%s/ANAIRIATS/%s\">%s</ulink>", @(MYCODEROOT, codepath, linkname)
  ) // end of [val]
  val res = string_of_strptr (res)
in
  TEXTstrcst (res)
end // end of [myatscodelink]

(* ****** ****** *)

(* end of [tutorial.dats] *)
