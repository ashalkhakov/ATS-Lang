//
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: February, 2011
//
(* ****** ****** *)

#define ATS_DYNLOADFLAG 0

(* ****** ****** *)

staload "myheader.sats"

(* ****** ****** *)

(*
// loaded by atsopt:
staload "prelude/SATS/array.sats"
*)
staload _(*anon*) = "prelude/DATS/array.dats"

(* ****** ****** *)

staload "contrib/linux/basics.sats"
staload "contrib/linux/SATS/utils.sats"

(* ****** ****** *)

staload
UACC = "contrib/linux/asm/SATS/uaccess.sats"
macdef copy_to_user = $UACC.copy_to_user
macdef copy_from_user = $UACC.copy_from_user

(* ****** ****** *)

staload "scull.sats"

#define i2sz size1_of_int1
macdef viewout_decode = $UN.viewout_decode

(* ****** ****** *)

absview qtm_v (n: int, l:addr)

extern
prfun qtm_v_nil {n:nat} (): qtm_v (n, null)
extern
prfun qtm_v_takeout
  {n:nat} {l:agz} () : ftakeout_p (qtm_v (n, l), bytes (n) @ l)
// end of [qtm_v]

(* ****** ****** *)

absview qdat_v (m: int, n: int, l:addr)

extern
prfun qdatptr_fold {m,n:nat} {l:addr}
  (pf: qdat_v (m, n, l) | x: !ptr l >> qdatptr (m, n, l)): void
extern
prfun qdatptr_unfold {m,n:nat} {l:addr}
  (x: !qdatptr (m, n, l) >> ptr l): (qdat_v (m, n, l) | void)

extern
prfun qdat_v_nil
  {m,n:nat} (): qdat_v (m, n, null)
extern
prfun qdat_v_unnil
  {m,n:int} (pf: qdat_v (m, n, null)): void

extern
prfun qdat_v_takeout : {m,n:nat} {l:agz}
  ftakeout_p (qdat_v (m, n, l), array_v (qtmptr (n), m, l))
// end of [qdat_v_takeout]

extern
prfun qdat_v_takeout_all {m,n:nat} {l:agz}
  (pf : qdat_v (m, n, l)): (kfree_v l, array_v (qtmptr (n), m, l))
// end of [qdat_v_takeout_all]

(* ****** ****** *)

(*
fun qdatptr_free
  {m,n:nat} (p: qdatptr (m, n), m: int m): void
// end of [qdatptr_free]
*)

implement
qdatptr_free
  {m,n} (p, m) = let
//
viewtypedef qtmptr = qtmptr (n)
//
extern fun array_ptr_kfree
  {a:viewt@ype}
  {n:int}
  {l:addr} (
  pf_gc: kfree_v l
, pf_arr: array_v (a?, n, l)
| p_arr: ptr l
) :<> void = "mac#atsctrb_linux_array_ptr_kfree"
//
fun free_agz {l:agz} .<>. (
  pf: qdat_v (m, n, l) | p: ptr l, m: int m
) :<> void = let
  prval (pfgc, pfarr) = qdat_v_takeout_all (pf)
  val m = size1_of_int1 (m)
  val () = array_ptr_clear_fun<qtmptr> (!p, m, lam x =<fun> qtmptr_free (x))
in
  array_ptr_kfree {ptr} (pfgc, pfarr | p)
end // end of [free_agz]
//
prval (pf | ()) =
  qdatptr_unfold (p)
val p = p
prval () = ptr_is_gtez (p)
//
in
  if p > null then
    free_agz (pf | p, m)
  else let
    prval () = qdat_v_unnil (pf)
  in
    // nothing
  end (* end of [if] *)
end // end of [qdatptr_free]

(* ****** ****** *)

macdef
ENOMEM = $extval (Pos, "ENOMEM")
macdef
EFAULT = $extval (Pos, "EFAULT")

extern
fun add_loff_int {i,j:int}
  (x: loff_t i, y: int j): loff_t (i+j) = "mac#add_loff_int"
// end of [fun]

(* ****** ****** *)

extern
prfun qtmptr_vtakeout_bytes_read
  {n:nat} {l:addr}
  (p: !qtmptr (n, l)): (
  option_v (viewout (bytes(n) @ l), l > null)
) // end of [qtmptr_vtakeout_bytes_read]

extern
fun qdatptr_vtakeout_bytes_read
  {m,n:nat} {l0:addr} (
  p: !qdatptr (m, n, l0), i: natLt m
) : [l:addr] (
  option_v (viewout (bytes(n) @ l), l > null) | ptr l
) = "scull_qdatptr_vtakeout_bytes_read"

implement
qdatptr_vtakeout_bytes_read
  {m,n}
  (p, i) = let
  val p1 = ptr_of (p)
in
  if p1 > null then let
    prval (pfdat | ()) = qdatptr_unfold (p)
    prval (pfarr, fpfdat) = qdat_v_takeout (pfdat)
    val i = i2sz (i)
    val (pfat, fpfarr | p_i) = array_ptr_takeout<qtmptr(n)> (pfarr | p1, i)
    val pqtm = ptr_of (!p_i)
    prval pfopt = qtmptr_vtakeout_bytes_read (!p_i)
    prval () = pfarr := fpfarr (pfat)
    prval () = pfdat := fpfdat (pfarr)
    prval () = qdatptr_fold (pfdat | p)
  in
    #[.. | (pfopt | pqtm)]
  end else (
    #[.. | (None_v () | null)]
  ) (* end of [if] *)
end // end of [qdatptr_vtakeout_bytes_read]

(* ****** ****** *)

(*
fun scull_read_main
  {m,n:nat}
  {ln0:nat}
  {lbf:addr}
  {cnt:nat}
  {tot:nat} (
  pfbuf: !bytes(cnt) @ lbf
| m: int m, n: int n
, xs: !slist (qset(m, n), ln0)
, ln: natLt (ln0), i: natLt (m), j: natLt (n)
, pbf: uptr (lbf)
, cnt: int (cnt)
, fpos: &loff_t(tot) >> loff_t(tot+max(0, cnt1))
) : #[cnt1:int | cnt1 <= cnt] intLte (cnt1) = "scull_read_main"
// end of [fun]
*)
implement
scull_read_main
  {m,n}
  {ln0}
  {lb}
  {cnt}
  {tot} (
  pfbuf
| m, n, xs, ln, i, j, pbf, cnt, fpos
) = let
  stadef qset = qset (m, n)
  val [lm:addr] (pfout | pm) = scull_follow_lessthan {m,n} (xs, ln)
  prval (pfqs, fpfqs) = viewout_decode {qset@lm} (pfout)
  val (pfopt | pqtm) = qdatptr_vtakeout_bytes_read (pm->data, i)
  prval () = fpfqs (pfqs)
in
  if pqtm > null then let
    prval Some_v pfout = pfopt
    prval (pf, fpf) = viewout_decode (pfout)
    stavar j: int
    val j = j : int j
    prval (pf1, pf2) = bytes_v_split {n} {j} (pf)
    val [cnt:int] cnt = imin (cnt, n-j)
(*
    prval () = verify_constraint {n-j > 0} ()
*)
    val cnt_ul = $UN.cast {ulint(cnt)} (cnt)
    val nleft = copy_to_user (pfbuf | pbf, !(pqtm+j), cnt_ul)
    prval () = fpf (bytes_v_unsplit (pf1, pf2))
  in
    if nleft = 0UL then let
      val () = fpos := add_loff_int (fpos, cnt) in #[cnt | cnt]
    end else let
      val [x:int] x = EFAULT in #[~x | ~x]
    end // end of [if]
  end else let
    prval None_v () = pfopt
  in
    #[0 | 0] (* unavailable *)
  end // end of [if]
end // end of [scull_read_main]
  
(* ****** ****** *)

extern
fun qtmptr_vtakeout_bytes_write
  {n:nat} {l:addr} (
  p: &qtmptr (n, l) >> qtmptr (n, l), n: int n, ntry: int
) : #[l:addr] (
  option_v (viewout (bytes(n) @ l), l > null) | ptr l
) // end of [qtmptr_vtakeout_bytes_write]

implement
qtmptr_vtakeout_bytes_write
  {n} {l} (
  r, n, ntry
) = let
  val p = ptr_of (r)
in
  if p > null then
    (qtmptr_vtakeout_bytes_read (r) | p)
  else if ntry > 0 then
    (qtmptr_vtakeout_bytes_read (r) | p)
  else let
    prval () = ptr_is_gtez (p)
    val () = qtmptr_free_null (r)
    val () = r := qtmptr_make (n)
  in
    qtmptr_vtakeout_bytes_write (r, n, ntry+1)
  end (* end of [if] *)
end // end of [qtmptr_vtakeout_bytes_write]

extern
fun qdatptr_vtakeout_bytes_write
  {m,n:nat} {l0:addr} (
  p: &qdatptr (m, n, l0) >> qdatptr (m, n, l0)
, m: int m, n: int n
, i: natLt m
, ntry: int
) : #[l0,l:addr] (
  option_v (viewout (bytes(n) @ l), l > null) | ptr l
) = "scull_qdatptr_vtakeout_bytes_write"

implement
qdatptr_vtakeout_bytes_write
  {m,n}
  (r, m, n, i, ntry) = let
  val p = ptr_of (r)
in
  if p > null then let
    prval (pfdat | ()) = qdatptr_unfold (r)
    prval (pfarr, fpfdat) = qdat_v_takeout (pfdat)
    val i = i2sz (i)
    val (pfat, fpfarr | p_i) = array_ptr_takeout<qtmptr(n)> (pfarr | p, i)
    val (pfopt | pqtm) = qtmptr_vtakeout_bytes_write (!p_i, n, 0)
    prval () = pfarr := fpfarr (pfat)
    prval () = pfdat := fpfdat (pfarr)
    prval () = qdatptr_fold (pfdat | r)
  in
    #[ .. | (pfopt | pqtm) ]
  end else if ntry > 0 then
    #[.. | (None_v () | null)]
  else let
    prval () = ptr_is_gtez (p)
    val () = qdatptr_free_null (r)
    val () = r := qdatptr_make {m,n} (m)
  in
    qdatptr_vtakeout_bytes_write (r, m, n, i, ntry+1)
  end // end of [if]
end // end of [qdatptr_vtakeout_bytes_read]

(* ****** ****** *)

(*
fun scull_write_main
  {m,n:nat}
  {ln0.ln:nat}
  {lbf:addr}
  {cnt:nat}
  {tot:nat} (
  pfbuf: !bytes(cnt) @ lbf
| m: int m, n: int n
, xs: &slist (qset(m, n), ln0) >> slist (qset(m, n), ln1)
, ln0: int (ln0), ln: int (ln)
, i: natLt (m), j: natLt (n)
, pbf: uptr (lbf)
, cnt: int (cnt)
, fpos: &loff_t(tot) >> loff_t(tot+max(0, cnt1))
) : #[
  ln1,cnt1:int
| ln0 <= ln1
; cnt1 <= cnt
] intLte (cnt1) = "scull_write_main"
// end of [fun]
*)
implement
scull_write_main
  {m,n}
  {ln0,ln}
  {lbf}
  {cnt}
  {tot} (
  pfbuf
| m, n, xs, ln0, ln, i, j, pbf, cnt, fpos
) = let
  val (pfopt | pm) = scull_follow_main (xs, ln0, ln)
  stavar ln1: int
  val ln1 = ln0: int (ln1)
in
//
if pm > null then let
  prval Some_v pfout = pfopt
  prval (pf, fpf) = viewout_decode (pfout)
  val (pfopt | pqtm) = qdatptr_vtakeout_bytes_write (pm->data, m, n, i, 0)
  prval () = fpf (pf)
in
  if pqtm > null then let  
    prval Some_v pfout = pfopt
    prval (pf, fpf) = viewout_decode (pfout)
    stavar j: int
    val j = j : int j
    prval (pf1, pf2) = bytes_v_split {n} {j} (pf)
    val [cnt:int] cnt = imin (cnt, n-j)
(*
    prval () = verify_constraint {n-j > 0} ()
*)
    val cnt_ul = $UN.cast {ulint(cnt)} (cnt)
    val nleft = copy_from_user (pfbuf | !(pqtm+j), pbf, cnt_ul)
    prval () = fpf (bytes_v_unsplit (pf1, pf2))
  in
    if nleft = 0UL then let
      val () = fpos := add_loff_int (fpos, cnt) in #[ln1, cnt | cnt]
    end else let
      val [x:int] x = EFAULT in #[ln1, ~x | ~x]
    end // end of [if]
  end else let
    prval None_v () = pfopt
    val [x:int] x = ENOMEM in #[ln1, ~x | ~x] // out-of_memory
  end (* end of [if] *)
end else let
  prval None_v () = pfopt
  val [x:int] x = ENOMEM in #[ln1, ~x | ~x] // out-of_memory
end (* end of [if] *)
//
end // end of [scull_write_main]
  
(* ****** ****** *)

(* end of [scull_data.dats] *)
