(*
**
** Author: Hongwei Xi (gmhwxi AT gmail DOT com)
** Time: August, 2011
**
*)
//
staload _(*anon*) = "prelude/DATS/list.dats"
staload _(*anon*) = "prelude/DATS/list_vt.dats"
staload _(*anon*) = "prelude/DATS/reference.dats"
//
(* ****** ****** *)
//
#include "utils/atsdoc/HATS/xhtmlatxt.hats"
//
macdef para (x) = xmltagging ("p", ,(x))
macdef emph (x) = xmltagging ("em", ,(x))
//
staload _(*anon*) = "libatsdoc/DATS/atsdoc_text.dats"
//
(* ****** ****** *)

#include "atslangweb_params.hats"

(* ****** ****** *)

#define s2s string_of_strptr

(* ****** ****** *)
//
macdef strcst (x) = TEXTstrcst ,(x)
macdef strsub (x) = TEXTstrsub ,(x)
//
fun strcst_of_strptr (x: strptr1): text = TEXTstrcst ((s2s)x)
fun strsub_of_strptr (x: strptr1): text = TEXTstrsub ((s2s)x)
//
(* ****** ****** *)

macdef command(x) = xmltagging ("pre", ,(x)) // <pre> ... </pre>

fn filename (x: string): text = let
  val opn = strcst"<span style=\"text-decoration: underline;\">"
  val cls = strcst"</span>"
in
  TEXTapptxt3 (opn, strsub(x), cls)
end

(* ****** ****** *)

fn menu
  (itmlst: string): text = xmltagging ("ul", itmlst)
// end of [menu]

fn lisub
  (x: string): text = xmltagging ("li", x)
// end of [lisub]

fn litxt (x: text): text =
  TEXTapptxt3 (strcst"<li>", x, strcst"</li>")
// end of [litxt]

(* ****** ****** *)

fn HR (sz: int) =
  strcst_of_strptr (sprintf ("<hr style=\"background-color: #E0E0E0; height: %ipx;\"></hr>", @(sz)))
// end of [HR]

(* ****** ****** *)

fun uid (id: string, name: string): text =
  strcst_of_strptr (sprintf ("<a id=\"%s\">%s</a>", @(id, name)))
// end of [uid]

fun ulink (link: string, name: string): text =
  strcst_of_strptr (sprintf ("<a href=\"%s\">%s</a>", @(link, name)))
// end of [ulink]

fun ulink1 (link: string, name: string): text =
  strsub_of_strptr (sprintf ("<a href=\"%s\">%s</a>", @(link, name)))
// end of [ulink1]

(* ****** ****** *)

#define ATSLANGSVNROOT
"http://ats-lang.svn.sourceforge.net/svnroot/ats-lang/trunk"
// end of [#define]

fun ATSLANGSVNROOTget (): text = TEXTstrcst (ATSLANGSVNROOT)

(* ****** ****** *)

fun ATSLANGWEBROOTget (): text = TEXTstrcst (ATSLANGWEBROOT)

val ATSLANGWEBHOME: text = strcst ((s2s)res) where {
  val res = sprintf ("<a href=\"%s/\">Home</a>", @(ATSLANGWEBROOT))
}
val ATSLANGWEBDOWNLOAD: text = strcst ((s2s)res) where {
  val res = sprintf ("<a href=\"%s/DOWNLOAD/\">Download</a>", @(ATSLANGWEBROOT))
}
val ATSLANGWEBDOCUMENT: text = strcst ((s2s)res) where {
  val res = sprintf ("<a href=\"%s/DOCUMENT/\">Documentation</a>", @(ATSLANGWEBROOT))
}
val ATSLANGWEBLIBRARY: text = strcst ((s2s)res) where {
  val res = sprintf ("<a href=\"%s/LIBRARY/\">Libraries</a>", @(ATSLANGWEBROOT))
}
val ATSLANGWEBRESOURCE: text = strcst ((s2s)res) where {
  val res = sprintf ("<a href=\"%s/RESOURCE/\">Resources</a>", @(ATSLANGWEBROOT))
}
val ATSLANGWEBCOMMUNITY: text = strcst ((s2s)res) where {
  val res = sprintf ("<a href=\"%s/COMMUNITY/\">Community</a>", @(ATSLANGWEBROOT))
}
val ATSLANGWEBEXAMPLE: text = strcst ((s2s)res) where {
  val res = sprintf ("<a href=\"%s/EXAMPLE/\">Examples</a>", @(ATSLANGWEBROOT))
}
val ATSLANGWEBIMPLEMENT: text = strcst ((s2s)res) where {
  val res = sprintf ("<a href=\"%s/IMPLEMENT/\">Implementations</a>", @(ATSLANGWEBROOT))
}
val ATSLANGWEBPAPER: text = strcst ((s2s)res) where {
  val res = sprintf ("<a href=\"%s/PAPER/\">Papers</a>", @(ATSLANGWEBROOT))
}

#ifndef ISTEMP
#define ISTEMP 0
#endif
#if(ISTEMP==0)
val () = theTextMap_insert_str ("ATSLANGWEBHOME", ATSLANGWEBHOME)
val () = theTextMap_insert_str ("ATSLANGWEBDOWNLOAD", ATSLANGWEBDOWNLOAD)
val () = theTextMap_insert_str ("ATSLANGWEBDOCUMENT", ATSLANGWEBDOCUMENT)
val () = theTextMap_insert_str ("ATSLANGWEBLIBRARY", ATSLANGWEBLIBRARY)
val () = theTextMap_insert_str ("ATSLANGWEBRESOURCE", ATSLANGWEBRESOURCE)
val () = theTextMap_insert_str ("ATSLANGWEBCOMMUNITY", ATSLANGWEBCOMMUNITY)
val () = theTextMap_insert_str ("ATSLANGWEBEXAMPLE", ATSLANGWEBEXAMPLE)
val () = theTextMap_insert_str ("ATSLANGWEBIMPLEMENT", ATSLANGWEBIMPLEMENT)
val () = theTextMap_insert_str ("ATSLANGWEBPAPER", ATSLANGWEBPAPER)
#endif

(* ****** ****** *)

local

fn make_ahref (
  link: string, name: string
) : string = let
  val res = sprintf ("<a href=\"%s\">%s</a>", @(link, name))
in
  (s2s)res
end // end of [make_ahref]

fn subpage_ahref
  (flag: int, link: string, name: string): string = let
in
  if flag > 0 then let
    val name = sprintf ("<strong>%s</strong>", @(name))
  in
    (s2s)name // name only
  end else 
    make_ahref (link, name)
   // end of [if]
end // end of [subpage_ahref]

val root = ATSLANGWEBROOT

in // in of [local]

fn HOME_ahref
  (flag: int): string = let
  val link = sprintf ("%s/", @(root))
  val link = (s2s)link
in
  subpage_ahref (flag, link, "Home")
end // end of [HOME_ahref]

fn DOWNLOAD_ahref
  (flag: int): string = let
  val link = sprintf ("%s/%s", @(root, "DOWNLOAD"))
  val link = (s2s)link
in
  subpage_ahref (flag, link, "Download")
end // end of [DOWNLOAD_ahref]

fn DOCUMENT_ahref
  (flag: int): string = let
  val link = sprintf ("%s/%s", @(root, "DOCUMENT"))
  val link = (s2s)link
in
  subpage_ahref (flag, link, "Documentation")
end // end of [DOCUMENT_ahref]

fn LIBRARY_ahref
  (flag: int): string = let
  val link = sprintf ("%s/%s", @(root, "LIBRARY"))
  val link = (s2s)link
in
  subpage_ahref (flag, link, "Libraries")
end // end of [LIBRARY_ahref]

fn RESOURCE_ahref
  (flag: int): string = let
  val link = sprintf ("%s/%s", @(root, "RESOURCE"))
  val link = (s2s)link
in
  subpage_ahref (flag, link, "Resources")
end // end of [RESOURCE_ahref]

fn COMMUNITY_ahref
  (flag: int): string = let
  val link = sprintf ("%s/%s", @(root, "COMMUNITY"))
  val link = (s2s)link
in
  subpage_ahref (flag, link, "Community")
end // end of [COMMUNITY_ahref]

end // end of [local]

(* ****** ****** *)

fn mysitelinks (current: string) = let
//
  val flag = (if (current = "HOME") then 1 else 0): int
  val HOME = strcst (HOME_ahref (flag))
//
  val flag = (if (current = "DOWNLOAD") then 1 else 0): int
  val DOWNLOAD = strcst (DOWNLOAD_ahref (flag))
//
  val flag = (if (current = "DOCUMENT") then 1 else 0): int
  val DOCUMENT = strcst (DOCUMENT_ahref (flag))
//
  val flag = (if (current = "LIBRARY") then 1 else 0): int
  val LIBRARY = strcst (LIBRARY_ahref (flag))
//
  val flag = (if (current = "RESOURCE") then 1 else 0): int
  val RESOURCE = strcst (RESOURCE_ahref (flag))
//
  val flag = (if (current = "COMMUNITY") then 1 else 0): int
  val COMMUNITY = strcst (COMMUNITY_ahref (flag))
//
  val xs = $lst {text} (
    HOME, DOWNLOAD, DOCUMENT, LIBRARY, RESOURCE, COMMUNITY
  ) // end of [val]
//
  val sep = strcst ("<span class=\"separator\"> | </span>")
//
in
  TEXTcontxtsep (xs, sep)
end // end of [mysitelinks]

(* ****** ****** *)

fun staexp (x: string) = let
  val opn = "<span class=atsyntax_staexp>"
  val cls = "</span>"
in
  TEXTappstr3 (opn, x, cls)
end // end of [staexp]

fun dynexp (x: string) = let
  val opn = "<span class=atsyntax_dynexp>"
  val cls = "</span>"
in
  TEXTappstr3 (opn, x, cls)
end // end of [dynexp]

(* ****** ****** *)
//
local
(*
** nothing
*)
in
//
fun atscode2xmls (x: string): text = atscode2xml_strcode (0, x)
fun atscode2xmld (x: string): text = atscode2xml_strcode (1, x)
//
end // end of [local]
//
(* ****** ****** *)

(* end of [atslangwebatxt.dats] *)
