//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: February, 2011
//
(* ****** ****** *)

#define ATS_STALOADFLAG 0

(* ****** ****** *)

%{#
#include "ATS/scull.cats"
%} // end of [%{#]

(* ****** ****** *)

staload UN = "prelude/SATS/unsafe.sats"
stadef viewout = $UN.viewout

(* ****** ****** *)

staload "libats/ngc/SATS/slist.sats"

(* ****** ****** *)

staload "contrib/kernel/basics.sats"

(* ****** ****** *)

propdef ftakeout_p
  (v1: view, v2: view) = v1 -<prf> (v2, v2 -<lin,prf> v1)
// end of [ftakeout_p]

(* ****** ****** *)
//
// HX:
// n: quantum size; l: location
//
absviewtype
qtmptr (n: int, l:addr) = ptr
viewtypedef qtmptr (n: int) = [l:addr] qtmptr (n, l)

castfn ptr_of_qtmptr
  {n:nat} {l:addr} (x: !qtmptr (n, l)): ptr l
overload  ptr_of with ptr_of_qtmptr

fun qtmptr_make
  {n:nat}
  (n: int n):<> qtmptr (n) = "scull_qtmptr_make"
fun qtmptr_make_null
  {n:nat} ():<> qtmptr (n, null) = "mac#scull_ptr_make_null"

fun qtmptr_free {n:nat}
  (p: qtmptr (n)):<> void = "scull_qtmptr_free"

(* ****** ****** *)
//
// HX:
// m: qset data size; n: quantum size; l: location
//
absviewtype
qdatptr (m: int, n: int, l:addr) = ptr
viewtypedef
qdatptr (m: int, n: int) = [l:addr] qdatptr (m, n, l)

castfn ptr_of_qdatptr
  {m,n:nat} {l:addr} (x: !qdatptr (m, n, l)): ptr l
overload  ptr_of with ptr_of_qdatptr

fun qdatptr_make
  {m,n:nat}
  (m: int m):<> qdatptr (m, n) = "scull_qdatptr_make"
fun qdatptr_make_null
  {m,n:nat} ():<> qdatptr (m, n, null) = "mac#scull_ptr_make_null"

fun qdatptr_free {m,n:nat}
  (p: qdatptr (m, n), m: int m):<> void // implemented in ATS
// end of [qdatptr_free]

(* ****** ****** *)

viewtypedef
qset (m:int, n:int) =
$extype_struct "scull_qset_struct" of {
  data= qdatptr (m, n) // array(m) of arrays(n)
(*
, _rest= undefined_t
*)
} // end of [qset]
viewtypedef qsetlst (m: int, n: int, ln: int) = slist (qset (m, n), ln)

(* ****** ****** *)
//
// HX: m: qset data size; n: quantum size; ln: qsetlst length
//
(*
struct scull_dev {
  int m_qset;               // the current array size
  int n_quantum;            // the current quantum size
//
  struct scull_qset *data;  // pointer to first quantum set
  int ln_qlst;              // the current qsetlst length
//
  unsigned long size;       // amount of data stored here
//
  unsigned int access_key;  // used by sculluid and scullpriv
//
  struct semaphore sem;     // mutual exclusion semaphore
//
  struct cdev cdev;	    // char device structure
//
} ; // end of [scull_dev]
*)
viewtypedef
scull_dev (
  m: int
, n: int
, ln: int
, sz: int
) =
$extype_struct "scull_dev_struct" of {
  empty=empty
, m_qset= int (m)
, n_quantum= int (n)
, data= qsetlst (m, n, ln)
, ln_qlst= int (ln)
, size= ulint (sz)
, _rest= undefined_vt
} // end of [scull_dev]

(* ****** ****** *)

fun scull_trim_main
  {m0,n0:nat}
  {ln:nat}
  {sz:nat}
  {m,n:pos} (
  dev: &scull_dev (m0, n0, ln, sz) >> scull_dev (m, n, 0, 0)
, m: int m
, n: int n
) : void = "scull_trim_main"

(* ****** ****** *)

(*
fun{a:vt0p}
slist_split_at
  {n:int} {i:nat | i < n} {la:addr}
  (pflst: slist_v (a, n, la) | p: ptr la, i: size_t i)
  : [lm:addr] (slseg_v (a, i, la, lm), slist_v (a, n-i, lm) | ptr lm)
// end of [slist_split_at]
*)
fun scull_follow_main
  {m,n:nat} {ln0:nat} {ln:nat} (
  xs: &slist (qset(m, n), ln0) >> slist (qset(m, n), ln0)
, ln0: &int(ln0) >> int (ln0)
, ln: int (ln)
) : #[ln0:nat;lm:addr] (
  option_v (viewout (qset(m, n) @ lm), lm > null) | ptr (lm)
) = "scull_follow_main"
// end of [fun]

(* ****** ****** *)

(* end of [scull.sats] *)
