(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(*                              Hongwei Xi                             *)
(*                                                                     *)
(***********************************************************************)

(*
 * ATS/Anairiats - Unleashing the Power of Types!
 *
 * Copyright (C) 2002-2008 Hongwei Xi, Boston University
 *
 * All rights reserved
 *
 * ATS is free software;  you can  redistribute it and/or modify it under
 * the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
 * Free Software Foundation; either version 3, or (at  your  option)  any
 * later version.
 * 
 * ATS is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
 * for more details.
 * 
 * You  should  have  received  a  copy of the GNU General Public License
 * along  with  ATS;  see the  file COPYING.  If not, please write to the
 * Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
 *
 *)

(* ****** ****** *)

// Time: June 2008
// Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu)

(* ****** ****** *)

%{^

#include "config.h" // automatically generated by [configure]

// [posix_memalign] is in [stdlib.h]
extern int posix_memalign(void **memptr, size_t alignment, size_t size);

// [memalign] is in [stdlib.h] or [malloc.h]
extern void *memalign(size_t boundary, size_t size);

static inline
int gcats_memalign (
  void **memptr
, size_t alignment
, size_t size
) {
  int err ;
#if HAVE_POSIX_MEMALIGN // for [posix_memalign]
  err = posix_memalign (memptr, alignment, size) ;
#elif HAVE_MEMALIGN // if [memalign] is avaiable
  void *ptr ;
  ptr = memalign(alignment, size) ; *memptr = ptr ;
  err = (ptr ? 0 : -1) ;
#else // neither [posix_memalign] nor [memalign] is available
  void *ptr ;
  ptr = malloc (size + alignment - 1) ; *memptr = ptr ;
  err = (ptr ? 0 : -1) ;
#endif
  return err ;
} /* end of [gcats_memalign] */

#include "gc.cats"

%}

#include "gc.hats"

(* ****** ****** *)

staload "gc.sats"

(* ****** ****** *)

#define ATS_FUNCTION_NAME_PREFIX "gc_chunk_"

(* ****** ****** *)

implement chunklst_sweep_length (chks) = let
  fun aux {i,j:nat}
    (chks: chunklst i, j: int j): int (i+j) = begin
    if chunklst_is_cons (chks) then begin
      aux (chunklst_sweep_next_get (chks), j+1)
    end else begin
      j // return value
    end // end of [if]
  end // end of [aux]
in
  aux (chks, 0)
end // end of [chunklst_sweep_length]

(* ****** ****** *)

#if (__WORDSIZE == 32)

extern fun the_topsegtbl_get_32 {i:nat}
  (pf: PTR_TOPSEG_GET_p i | ofs: uintptr i): botsegtbllst0
  = "the_topsegtbl_get_32"

extern fun the_topsegtbl_set_32 {i:nat}
  (pf: PTR_TOPSEG_GET_p i | ofs: uintptr i, tbls: botsegtbllst1): void
  = "the_topsegtbl_set_32"

implement the_topsegtbl_get (pf | ofs) = the_topsegtbl_get_32 (pf | ofs)

implement the_topsegtbl_get_some (pf | ofs) = let
  val tbls = the_topsegtbl_get_32 (pf | ofs)
in
  if botsegtbllst_is_nil tbls then let
    val tbls = botsegtbl_make_32 ()
    val () = the_topsegtbl_set_32 (pf | ofs, tbls)
  in
    tbls // [tbls] is not null
  end else begin
    tbls // [tbls] is not null
  end // end of [if]
end // end of [the_topsegtbl_get_some]

#endif // end of [__WORDSIZE == 32]

(* ****** ****** *)

#if (__WORDSIZE == 64)

extern fun the_topsegtbl_get_64 {i:nat}
  (pf: PTR_TOPSEG_GET_p i | ofs: uintptr i): botsegtbllst0
  = "the_topsegtbl_get_64"

extern fun the_topsegtbl_set_64 {i:nat}
  (pf: PTR_TOPSEG_GET_p i | ofs: uintptr i, tbls: botsegtbllst1): void
  = "the_topsegtbl_set_64"

implement the_topsegtbl_get (pf | ofs) = the_topsegtbl_get_64 (pf | ofs)

implement the_topsegtbl_get_some (pf | ofs) = let
  val tbls = the_topsegtbl_get_64 (pf | ofs)
in
  if botsegtbllst_is_nil tbls then let
    val tbls = botsegtbl_make_64 (pf | ofs, tbls)
    val () = the_topsegtbl_set_64 (pf | ofs, tbls)
  in
    tbls // [tbls] is not null
  end else begin
    tbls // [tbls] is not null
  end // end of [if]
end // end of [the_topsegtbl_get_some]

#endif // end of [__WORDSIZE == 64]

(* ****** ****** *)

extern fun chunklst_insert_into_table (chks: chunklst1): void
  = "chunklst_insert_into_table"

extern fun chunklst_remove_from_table (chks: chunklst1): void
  = "chunklst_remove_from_table"

implement chunklst_insert_into_table (chks) = let
  val _data = chunklst_data_get (chks)
  val _data = freeitmlst2ptr (_data)
  val (pf | ofs_topseg) = PTR_TOPSEG_GET (_data)
  val tbls = the_topsegtbl_get_some (pf | ofs_topseg)
  val ofs_botseg = PTR_BOTSEG_GET (_data)
in
  botsegtbllst_set (tbls, ofs_botseg, chks)
end // end of [chunklst_insert_into_table]

implement chunklst_remove_from_table (chks) = let
  val _data = chunklst_data_get (chks)
  val _data = freeitmlst2ptr (_data)
  val (pf | ofs_topseg) = PTR_TOPSEG_GET (_data)
  val tbls = the_topsegtbl_get (pf | ofs_topseg)
in
  if botsegtbllst_is_cons tbls then let
    val ofs_botseg = PTR_BOTSEG_GET (_data)
  in
     botsegtbllst_clear (tbls, ofs_botseg)
  end else begin
    prerr "GC: Fatal Error: [chunklst_remove_from_table]";
    prerr ": the chunk to be removed is not in the table!";
    prerr_newline ();
    exit {void} (1)
  end // end of [if]
end // end of [chunklst_remove_from_table]

(* ****** ****** *)

implement chunklst_create_release
  (pf_main_lock | itemwsz_log, itemwsz) = let
  val chks = chunklst_create (pf_main_lock | itemwsz_log, itemwsz)
  val () = gc_main_lock_release (pf_main_lock | (*none*))
in
  chks
end // end of [chunklst_create_release]

(* ****** ****** *)

extern fun freeitmlst_chunk_data_free
  (pf: !gc_main_lock_v | p: freeitmlst1): void
  = "freeitmlst_chunk_data_free"

extern fun freeitmlst_chunk_data_recycle
  (pf: !gc_main_lock_v | p: freeitmlst1): void
  = "freeitmlst_chunk_data_recycle"

implement chunklst_destroy (pf_main_lock | chks) = let
(*
  val ptr = chunklst2ptr (chks)
  val () = begin
    prerr "chunklst_destroy: chks = "; prerr ptr; prerr_newline ()
  end
*)
  val () = chunklst_remove_from_table (chks)
  val _data_nonalign = chunklst_data_nonalign_get (chks)
  val itemwsz = chunklst_itemwsz_get (chks)
  val () = chunk_header_free (chks) where {
    extern fun chunk_header_free (chks: chunklst1): void = "chunk_header_free"
  } // end of [where]
  val nchunk = (itemwsz + CHUNK_WORDSIZE_MASK) >> CHUNK_WORDSIZE_LOG
  val () = the_chunk_count_dec_by (pf_main_lock | nchunk)
in
  case+ 0 of
  | _ when itemwsz > MAX_CHUNK_BLOCK_WORDSIZE => let
(*
      val () = begin
        prerr "chunklst_destroy: itemwsz = "; prerr itemwsz; prerr_newline ()
      end
*)
    in
      freeitmlst_chunk_data_free (pf_main_lock | _data_nonalign)
    end // end of [_ when ...]
  | _ => begin
      freeitmlst_chunk_data_recycle (pf_main_lock | _data_nonalign)
    end // end of [_]
end  // end of [chunklst_destroy]

(* ****** ****** *)

extern fun gc_markbits_clear_chunk (chks: chunklst1): void
  = "gc_markbits_clear_chunk"

extern fun gc_markbits_clear_botsegtbl (tbls: botsegtbllst1): void
  = "gc_markbits_clear_botsegtbl"

implement gc_markbits_clear_botsegtbl (tbls) = let
  stadef N = BOTSEG_TABLESIZE
  fun loop {i:nat | i <= N} .<N-i>.
    (tbls: botsegtbllst1, i: int i): void =
    if i < BOTSEG_TABLESIZE then let
      val chks = botsegtbllst_get (tbls, i); val () = begin
        if chunklst_is_cons (chks) then gc_markbits_clear_chunk (chks)
      end
    in
      loop (tbls, i+1)
    end // end of [if]
in
  loop (tbls, 0)
end // end of [gc_markbits_clear_botsegtbl]

(* ****** ****** *)

%{$

#if (__WORDSIZE == 32)

ats_ptr_type botsegtbl_make_32 () {
  botsegtbllst tbls ;
/*
  fprintf (
    stderr, "botsegtbl_make_32: sizeof(botsegtbl) = %i\n", sizeof(botsegtbl)
  ) ; // end of [fprintf]
*/
  tbls = (botsegtbllst)malloc(sizeof(botsegtbl)) ;
  if (!tbls) {
    fprintf (stderr, "Exit: [botsegtbl_make_32: malloc] failed\n") ;
    exit (1) ;
  } // end of [if]

  memset(tbls, 0, sizeof(botsegtbl)) ;
/*
  fprintf (stderr, "botsegtbl_make_32: return: tbls = %p\n", tbls) ;
*/
  return tbls ;
} /* end of [botsegtbl_make_32] */

#endif // end of [__WORDSIZE == 32]

/* ****** ****** */

#if (__WORDSIZE == 64)

ats_ptr_type botsegtbl_make_64
  (ats_uintptr_type ofs, ats_ptr_type tbls0) {
  botsegtbllst tbls ;
/*
  fprintf (
    stderr, "botsegtbl_make_64: sizeof(botsegtbl) = %i\n", sizeof(botsegtbl)
  ) ; // end of [fprintf]
*/
  tbls = (botsegtbllst)malloc(sizeof(botsegtbl)) ;
  if (!tbls) {
    fprintf (stderr, "Exit: [botsegtbl_make_64: malloc] failed\n") ;
    exit (1) ;
  } // end of [if]

  memset(tbls, 0, sizeof(botsegtbl)) ;

  tbls->key = ofs ; tbls->hash_next = (botsegtbllst)tbls0 ;
/*
  fprintf (stderr, "botsegtbl_make_64: tbls = %p\n", tbls) ;
*/
  return tbls ;
} /* end of [botsegtbl_make_64] */

#endif // end of [__WORDSIZE == 64]

%}

(* ****** ****** *)

%{$

ats_ptr_type gc_ptr_is_valid
  (ats_ptr_type ptr, ats_ref_type ofs_r) {
  uintptr_t ofs_topseg ;
  int ofs_botseg, ofs_chkseg, itemwsz ;
  botsegtbllst tbls = (botsegtbllst)0 ;
  chunklst chks = (chunklst)0 ;
/*
  fprintf (stderr, "gc_ptr_is_valid: 1: ptr = %p\n", ptr) ;
*/
  if (!ptr) return (chunklst)0 ;

/*
  fprintf (stderr, "gc_ptr_is_valid: 2: ptr = %p\n", ptr) ;
*/
  if ((uintptr_t)ptr & NBYTE_PER_WORD_MASK) return (chunklst)0 ;

  ofs_topseg = PTR_TOPSEG_GET (ptr) ;

/*
  fprintf (stderr, "gc_ptr_is_valid: 3: ofs_topseg = %li\n", ofs_topseg) ;
*/

#if (__WORDSIZE == 32)
  tbls = the_topsegtbl_get_32 (ofs_topseg) ;
#endif

#if (__WORDSIZE == 64)
  tbls = the_topsegtbl_get_64 (ofs_topseg) ;
#endif

  if (!tbls) return (chunklst)0 ;

  ofs_botseg = PTR_BOTSEG_GET (ptr) ;
  chks = botsegtbllst_get (tbls, ofs_botseg) ;
/*
  fprintf (stderr, "gc_ptr_is_valid: 4: ofs_botseg = %li\n", ofs_botseg) ;
*/
  if (!chks) return (chunklst)0 ;

  ofs_chkseg = PTR_CHKSEG_GET (ptr) ;
/*
  fprintf (stderr, "gc_ptr_is_valid: 5: ofs_chkseg = %li\n", ofs_chkseg) ;
*/
  itemwsz = chks->itemwsz ;
/*
  fprintf (stderr, "gc_ptr_is_valid: 6: itemwsz = %i\n", itemwsz) ;
*/

  if (ofs_chkseg % itemwsz) return (chunklst)0 ;

  *((int*)ofs_r) = (ofs_chkseg / itemwsz) ;

  return chks ;
} /* end of [gc_ptr_is_valid] */

%}

(* ****** ****** *)

%{$

/*

// itemwsz_log = log (itemwsz) if itemwsz_log >= 0
fun chunklst_create (itemwsz_log: int, itemwsz: int): chunklst1

*/

ats_ptr_type chunklst_create
  (ats_int_type itemwsz_log, ats_int_type itemwsz) {
  int i, err ;
  int chunk_bsz, data_ofs ;
  int itemtot, nmarkbit ;
  freeitmlst data, data_next ;
  chunklst chks ;
/*
  fprintf (stderr, "chunklst_create: itemwsz_log = %i\n", itemwsz_log) ;
  fprintf (stderr, "chunklst_create: itemwsz = %i\n", itemwsz) ;
*/
  if (itemwsz_log >= 0) {
    itemtot = (CHUNK_WORDSIZE >> itemwsz_log) ;
    nmarkbit = (itemtot + NBIT_PER_BYTE_MASK) >> NBIT_PER_BYTE_LOG ;
  } else {
    itemtot = 1 ; nmarkbit = 1 ;
  }

  chks = (chunklst)malloc(sizeof(chunk) + nmarkbit) ;
  if (!chks) {
    fprintf (
      stderr, "GC Fatal Error: [chunklst_create]: [malloc] failed.\n"
    ) ; // end of [fprintf]
    exit (1) ;
  } // end of [if]

  data = (freeitmlst)0 ;
  if (itemwsz <= MAX_CHUNK_BLOCK_WORDSIZE) {
    if (the_freeitmlst_chunk_data) {
      data = the_freeitmlst_chunk_data;
      the_freeitmlst_chunk_data = *(freeitmlst*)data ;
    } // end of [if]
  } // end of [if]
/*
  fprintf (stderr, "chunklst_create: data = %p(%i)\n", data, data) ;
*/
  if (!data) {
    if (itemwsz > CHUNK_WORDSIZE)
      chunk_bsz = itemwsz << NBYTE_PER_WORD_LOG ;
    else chunk_bsz = CHUNK_BYTESIZE ;

//  fprintf (stderr, "gcats_memalign: size = %i\n", chunk_bsz) ;

    err = gcats_memalign
      (&data, CHUNK_BYTESIZE/*alignment*/, chunk_bsz/*size*/) ;

    if (err) {
      fprintf (stderr, "GC: Fatal Error: [gcats_memalign] failed\n") ;
      exit (1) ;
    } // end of [if]
  } // end of [if]
/*
  fprintf (stderr, "chunklst_create: data = %p(%i)\n", data, data) ;
*/
  chks->itemwsz = itemwsz ;
  chks->itemwsz_log = itemwsz_log ;
  chks->itemtot = itemtot ;
  chks->markcnt = 0 ; // for fast threading!
#ifdef _ATS_MULTITHREAD
  chks->freecnt = 0 ; // not in use yet
#endif
  chks->sweep_next = (chunklst)0 ;
  data_ofs = (uintptr_t)data & (CHUNK_BYTESIZE - 1) ;
  if (data_ofs) {
/*
    fprintf (stderr, "chunklst_create: data_ofs = %i\n", data_ofs) ;
*/
    chks->data = (byte*)data - data_ofs + CHUNK_BYTESIZE ;
  } else {
    chks->data = data ;
  }
  chks->data_nonalign = data ; // [data] is *not* zeroed
  memset (chks->markbits, 0, nmarkbit) ;

  the_chunk_count_inc_by ((itemwsz + CHUNK_WORDSIZE_MASK) >> CHUNK_WORDSIZE_LOG) ;

  chunklst_insert_into_table (chks) ;
/*
  fprintf (stderr, "chunklst_create: chks = %p(%i)\n", chks, chks) ;
*/
  return chks ;

} /* end of [chunklst_create] */

%}

(* ****** ****** *)

%{$

#if (__WORDSIZE == 32)

ats_void_type gc_markbits_clear_the_topsegtbl () {
  int i ; botsegtbllst tbls ;
  for (i = 0; i < TOPSEG_TABLESIZE; i += 1) {
    tbls = the_topsegtbl[i] ;
    if (tbls) gc_markbits_clear_botsegtbl (tbls) ;
  } // end of [for]
  return ;
} /* end of [gc_markbits_clear_the_topsegtbl] */

#endif // end of [__WORDSIZE == 32]

#if (__WORDSIZE == 64)

ats_void_type gc_markbits_clear_the_topsegtbl () {
  int i ; botsegtbllst tbls ;
  for (i = 0; i < TOPSEG_HASHTABLESIZE; i += 1) {
    tbls = the_topsegtbl[i] ;
    while (tbls) { // terminating: obvious
      gc_markbits_clear_botsegtbl (tbls) ; tbls = tbls->hash_next ;
    } // end of [while]
  } // end of [for]
  return ;
} /* end of [gc_markbits_clear_the_topsegtbl] */

#endif // end of [__WORDSIZE == 64]

%}

(* ****** ****** *)

(* end of [gc_chunk.dats] *)
