(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Anairiats - Unleashing the Power of Types!
**
** Copyright (C) 2002-2009 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
** Free Software Foundation; either version 3, or (at  your  option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)

// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Time: October, 2009

(* ****** ****** *)

#include "gcats2_ats.hats"

(* ****** ****** *)

staload "gcats2.sats"

(* ****** ****** *)

implement the_chunkpagelst_remove
  (pf | (*none*)) = let
  #define NBATCH 1024 // it is arbitrarily chosen!
  val (pfopt | p) = the_chunkpagelst_remove_opt (pf | (*none*))
in
  if (p <> null) then let
    prval Some_v pf = pfopt in (pf | p)
  end else let
    prval None_v () = pfopt
    val n = the_chunkpagelst_replenish (pf | NBATCH)
  in
    case+ 0 of
    | _ when n >= 0 => the_chunkpagelst_remove (pf | (*none*))
    | _ => $effmask_all (let
        val () = prerr "exit(ATS/GCATS2)"
        val () = prerr ": the_chunkpagelst_remove: mmap failed"
        val () = prerr_newline ()
      in
        exit (1)
      end) // end of [_]
  end (* end of [if] *)
end // end of [...]

(* ****** ****** *)

implement the_topsegtbl_clear_mrkbits
  (pf_tbl | (*none*)) = let
  prval pf = unit_v ()
  val f = lam {l:anz} (
      pf: !unit_v | p_chunk: !chunkptr_vt l, env: !ptr
    ) : void =<fun> let
    val (pf_chunk | p) = chunkptr_unfold (p_chunk)
    val () = chunk_mrkbits_clear (!p)
    val _(*ptr*) = chunkptr_fold (pf_chunk | p_chunk)
  in
    // nothing
  end
  val () = the_topsegtbl_foreach_chunkptr {unit_v} {ptr} (pf_tbl, pf | f, null)
  prval unit_v () = pf
in
  // nothing
end // end of [the_topsegtbl_clear_mrkbits]

(* ****** ****** *)

%{^

static freepagelst_vt the_chunkpagelst = (freepagelst_vt)0 ;

/*
fun the_chunkpagelst_insert {l:addr} // inserting one page
  (pf: !the_chunkpagelst_v, pf_page: freepage @ l | p: ptr l):<> void
// end of [...]
*/

ats_void_type
gcats2_the_chunkpagelst_insert
  (ats_ptr_type p_freepage) {
  *(freepagelst_vt*)p_freepage = the_chunkpagelst ;
  the_chunkpagelst = (freepagelst_vt)p_freepage ; return ;
} /* end of [gcats2_the_chunkpagelst_insert] */

/*
fun the_chunkpagelst_remove_opt // taking out one page
  (pf: !the_chunkpagelst_v | (*none*)):<> [l:addr] (ptropt_v (freepage, l) | ptr l)
// end of [...]
*/

ats_ptr_type
gcats2_the_chunkpagelst_remove_opt () {
  freepagelst_vt p_freepage = the_chunkpagelst ;
  if (p_freepage) the_chunkpagelst = *(freepagelst_vt*)p_freepage ;
  return p_freepage ;
} /* end of [gcats2_the_chunkpagelst_remove_opt] */

/*
fun the_chunkpagelst_replenish
  (pf: !the_chunkpagelst_v | (*none*)):<> void // adding N pages for some N >= 1
// end of [...]
*/
ats_int_type
gcats2_the_chunkpagelst_replenish
  (ats_int_type n0) {
  int n ; char *p0 ;
  p0 = (char*)mmap(
    (void*)0 // start
  , n0 * CHUNK_BYTESIZE
  , (PROT_READ | PROT_WRITE)
  , (MAP_PRIVATE | MAP_ANONYMOUS)
  , 0 // fd is ignored
  , 0 // offset is ignored
  ) ; // end of [mmap]
  if (p0 == MAP_FAILED) { perror ("mmap") ; return -1 ; }
// /*
  fprintf(stderr, "gcats2_the_chunkpagelst_replenish: mmap: p0 = %p\n", p0) ;
// */  
  gcats2_the_chunkpagelst_insert(p0) ; n = n0 - 1; // n0 > 0
  while (n > 0) {
    p0 += CHUNK_BYTESIZE ; gcats2_the_chunkpagelst_insert(p0) ; n -= 1 ;
  } // end of [while]
  return n0 ;
} /* gcats2_the_chunkpagelst_replenish */

%} // end of [%{^]

(* ****** ****** *)

%{

ats_ptr_type
gcats2_chunk_make_norm (
  ats_int_type itmwsz
, ats_int_type itmwsz_log
) {
  int itmtot ;
  freepageptr_vt p_freepage ;
  chunkptr_vt p_chunk ;
#if (GCATS2_DEBUG > 0)
  if (itmwsz != (1 << itmwsz_log)) {
    fprintf(stderr, "gcats2_chunk_make_norm: itmwsz = %i\n", itmwsz) ;
    fprintf(stderr, "gcats2_chunk_make_norm: itmwsz_log = %i\n", itmwsz_log) ;
    exit(1) ;
  } // end of [if]
#endif
  itmtot = (CHUNK_WORDSIZE >> itmwsz_log) ;
  p_chunk = (chunkptr_vt)gcats2_malloc_ext(sizeof(chunk_vt)) ;
  p_freepage = (freepageptr_vt)gcats2_the_chunkpagelst_remove() ;
//
  p_chunk->itmwsz = itmwsz ;
  p_chunk->itmwsz_log = itmwsz_log ;
  p_chunk->itmtot = itmtot ;
//
  p_chunk->mrkcnt = 0 ; // for fast threading!
  memset(p_chunk->mrkbits, 0, NMARKBIT_PER_CHUNK) ;
//
  p_chunk->chunk_data = p_freepage ;
/*
  fprintf (stderr, "chunklst_create: p_chunk = %p(%i)\n", p_chunk, p_chunk) ;
*/
  return p_chunk ;
} /* end of [gcats2_chunk_make_norm] */

/*
fun chunk_free_norm {l:anz}
  (pf: !the_chunkpagelst_v | p_chunk: chunkptr_vt l):<> void
*/
ats_void_type
gcats2_chunk_free_norm
  (ats_ptr_type p_chunk) { // p_chunk <> null)
#if (GCATS2_DEBUG > 0)
  if (((chunk_vt*)p_chunk)->itmwsz_log < 0) {
    fprintf(stderr, "gcats2_chunk_free_norm: the chunk to be freed is large.\n") ;
    exit(1) ;
  } // end of [if]
#endif
  gcats2_the_chunkpagelst_insert(((chunk_vt*)p_chunk)->chunk_data) ;
  gcats2_free_ext(p_chunk) ;
  return ;
} /* end of [gcats2_chunk_free_norm] */

%}

(* ****** ****** *)

%{^

static  // the number of botsegtbls that
long int the_nbotsegtbl_alloc = 0 ; // is allocated

ats_lint_type
gcats2_the_nbotsegtbl_alloc_get () { return the_nbotsegtbl_alloc ; }
// end of ...

ats_int_type
gcats2_the_topsegtbl_insert_chunkptr
  (ats_ptr_type p_chunk) {
  freeitmptr_vt p_chunk_data ;
  topseg_t ofs_topseg ; int ofs_botseg ; // ofs_chkseg == 0
  botsegtblptr_vt p_botsegtbl, *r_p_botsegtbl ;
  chunkptr_vt *r_p_chunk ;

  p_chunk_data = ((chunk_vt*)p_chunk)->chunk_data ;
  ofs_topseg = PTR_TOPSEG_GET(p_chunk_data) ;
  r_p_botsegtbl =
    (botsegtblptr_vt*)(gcats2_the_topsegtbl_takeout(ofs_topseg)) ;
  p_botsegtbl = *r_p_botsegtbl ;
  if (!p_botsegtbl) {
    p_botsegtbl =
      gcats2_malloc_ext(sizeof(botsegtbl_vt)) ;
    the_nbotsegtbl_alloc += 1 ;
    memset(p_botsegtbl, 0, sizeof(botsegtbl_vt)) ;
#if (__WORDSIZE == 64)
     p_botsegtbl->key =(uintptr_t)ofs_topseg ; p_botsegtbl->hashnxt = (botsegtblptr_vt)0 ;
#endif // end of [#if (__WORDSIZE == 64)]
/*
    fprintf (stderr, "gcats2_the_topsegtbl_insert_chunkptr: the_nbotsegtbl_alloc = %i\n", the_nbotsegtbl_alloc) ;
*/
    *r_p_botsegtbl = p_botsegtbl ;
  } // end of [if]
  ofs_botseg = PTR_BOTSEG_GET(p_chunk_data) ;
  r_p_chunk = gcats2_botsegtblptr1_takeout(p_botsegtbl, ofs_topseg, ofs_botseg) ;
//
#if (__WORDSIZE == 64)
  if (!r_p_chunk) {
    p_botsegtbl =
      gcats2_malloc_ext(sizeof(botsegtbl_vt)) ;
    the_nbotsegtbl_alloc += 1 ;
    memset(p_botsegtbl, 0, sizeof(botsegtbl_vt)) ;
    p_botsegtbl->key = (uintptr_t)ofs_topseg ; p_botsegtbl->hashnxt = *r_p_botsegtbl ;
/*
    fprintf(stderr, "gcats2_the_topsegtbl_insert_chunkptr: the_nbotsegtbl_alloc = %i\n", the_nbotsegtbl_alloc) ;
*/
    *r_p_botsegtbl = p_botsegtbl ;
    (p_botsegtbl->headers)[ofs_botseg] = p_chunk ;
    return 0 ;
  } // end of [if]
#endif // end of [#if (__WORDSIZE == 64)]
//
#if (GCATS2_DEBUG > 0)
  if (*r_p_chunk != (chunkptr_vt)0) {
/*
    fprintf(stderr, "exit(ATS/GC): gcats2_the_topsegtbl_insert_chunkptr: p_chunk = %p\n", p_chunk) ;
    fprintf(stderr, "exit(ATS/GC): gcats2_the_topsegtbl_insert_chunkptr: *r_p_chunk = %p\n", *r_p_chunk) ;
    exit(1) ;
*/
    return 1 ;
  } // end of [if]
#endif // end of [#if (GCATS2_DEBUG > 0)]
//
  *r_p_chunk = p_chunk ; return 0 ;
} /* end of [gcats2_the_topsegtbl_insert_chunkptr] */

/* ****** ****** */

ats_ptr_type // chunkptr
gcats2_the_topsegtbl_remove_chunkptr
  (ats_ptr_type ptr) {
  // ptr = p_chunk->chunk_data if p_chunk is removed
  topseg_t ofs_topseg ; int ofs_botseg ; // ofs_chkseg == 0
  botsegtblptr_vt p_botsegtbl;
  chunkptr_vt p_chunk, *r_p_chunk ;

  ofs_topseg = PTR_TOPSEG_GET(ptr) ;
  p_botsegtbl = // pf_botsegtbl, fpf_topsegtbl
    *(botsegtblptr_vt*)(gcats2_the_topsegtbl_takeout(ofs_topseg)) ;
  if (!p_botsegtbl) return (chunkptr_vt)0 ; // error return
  ofs_botseg = PTR_BOTSEG_GET(ptr) ;
  r_p_chunk = gcats2_botsegtblptr1_takeout(p_botsegtbl, ofs_topseg, ofs_botseg) ;
  if (!r_p_chunk) { return (chunkptr_vt)0 ; } // error return
  p_chunk = *r_p_chunk ; *r_p_chunk = (chunkptr_vt)0 ;
#if (GCATS2_DEBUG > 0)
  if (ptr != p_chunk->chunk_data) {
    fprintf(stderr, "exit(ATS/GC): gcats2_the_topsegtbl_remove_chunkptr: ptr = %p\n", ptr) ;
    fprintf(stderr, "exit(ATS/GC): gcats2_the_topsegtbl_remove_chunkptr: p_chunk = %p\n", p_chunk) ;
    exit(1) ;
  } // end of [if]
#endif // end of [#if (GCATS2_DEBUG > 0)]
  return p_chunk ;
} /* end of ... */

%} // end of [ %{^ ]

(* ****** ****** *)

%{^

ats_void_type
gcats2_the_topsegtbl_foreach_chunkptr
  (ats_ptr_type f, ats_ptr_type env) {
  int i, j ;
  botsegtblptr_vt p_botsegtbl ;
  chunkptr_vt p_chunk ;

#if (__WORDSIZE == 32)
  for (i = 0; i < TOPSEG_TABLESIZE; i += 1) {
    p_botsegtbl = the_topsegtbl[i] ;
    if (p_botsegtbl) {
      for (j = 0; j < BOTSEG_TABLESIZE; j += 1) {
        p_chunk = p_botsegtbl->headers[j] ;
        if (p_chunk) ((ats_void_type (*)(ats_ptr_type, ats_ptr_type))f)(p_chunk, env) ;
      } // end of [for]
    } // end of [if]
  } // end of [for]
#endif // end of ...

#if (__WORDSIZE == 64)
  for (i = 0; i < TOPSEG_HASHTABLESIZE; i += 1) {
    p_botsegtbl = the_topsegtbl[i] ;
    while (p_botsegtbl) {
      for (j = 0; j < BOTSEG_TABLESIZE; j += 1) {
        p_chunk = p_botsegtbl->headers[j] ;
        if (p_chunk) ((ats_void_type (*)(ats_ptr_type, ats_ptr_type))f)(p_chunk, env) ;
      } // end of [for]
      p_botsegtbl = p_botsegtbl->hashnxt ;
    } // end of [while]
  } // end of [for]
#endif // end of ...

  return ;
} /* end of [gcats2_the_topsegtbl_foreach_chunkptr] */

%} // end of [ %{^ ]

(* ****** ****** *)

(* end of [gcats2_chunk.dats] *)
