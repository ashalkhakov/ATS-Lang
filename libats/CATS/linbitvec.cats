/************************************************************************/
/*                                                                      */
/*                         Applied Type System                          */
/*                                                                      */
/*                              Hongwei Xi                              */
/*                                                                      */
/************************************************************************/

/*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2008 Hongwei Xi.
**
** ATS is  free software;  you can redistribute it and/or modify it under
** the  terms of the  GNU General Public License as published by the Free
** Software Foundation; either version 2.1, or (at your option) any later
** version.
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
*/

/* ****** ****** */

/*
**
** Author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) 
**
*/

/* ****** ****** */

#ifndef ATS_LIBATS_LINBITVEC_CATS
#define ATS_LIBATS_LINBITVEC_CATS

/* ****** ****** */

#define NBIT_PER_BYTE 8
#define NBIT_PER_BYTE_LOG 3

/* ****** ****** */

#ifndef __WORDSIZE
#error "[__WORDSIZE] is undefined.\n"
#endif

#if (__WORDSIZE == 32)

#define NBYTE_PER_WORD 4
#define NBYTE_PER_WORD_LOG 2
#if (NBYTE_PER_WORD != (1 << NBYTE_PER_WORD_LOG))
#error "NBYTE_PER_WORD != (1 << NBYTE_PER_WORD_LOG)\n"
#endif

#elif (__WORDSIZE == 64)

#define NBYTE_PER_WORD 8
#define NBYTE_PER_WORD_LOG 3
#if (NBYTE_PER_WORD != (1 << NBYTE_PER_WORD_LOG))
#error "NBYTE_PER_WORD != (1 << NBYTE_PER_WORD_LOG)\n"
#endif

#else
#error "[__WORDSIZE] is not supported.\n"
#endif

/* ****** ****** */

#define NBIT_PER_WORD (NBIT_PER_BYTE * NBYTE_PER_WORD)
#define NBIT_PER_WORD_LOG (NBIT_PER_BYTE_LOG + NBYTE_PER_WORD_LOG)
#if (NBIT_PER_WORD != (1 << NBIT_PER_WORD_LOG))
#error "NBIT_PER_WORD != (1 << NBIT_PER_WORD_LOG)\n"
#endif

/* ****** ****** */
//
// HX: these functions need to be inlined for efficiency!
//
/* ****** ****** */

static inline
ats_int_type
atslib_linbitvec_bitvec_get_at (
  ats_ptr_type p_vec, ats_size_type i
) {
  size_t q, r ;
  q = i >> NBIT_PER_WORD_LOG ; r = i & (NBIT_PER_WORD - 1) ;
  return (((uintptr_t*)p_vec)[q] >> r) & 0x1 ;
} // end of [atslib_linbitvec_bitvec_get_at]

static inline
ats_void_type
atslib_linbitvec_bitvec_set_at (
  ats_ptr_type p_vec, ats_size_type i, ats_int_type b
) {
  size_t q, r ;
  q = i >> NBIT_PER_WORD_LOG ; r = i & (NBIT_PER_WORD - 1) ;
  if (b) {
    ((uintptr_t*)p_vec)[q] |= ((uintptr_t)1 << r) ;
  } else {
    ((uintptr_t*)p_vec)[q] &= ~((uintptr_t)1 << r) ;
  } ;
  return ;
} // end of [linbitvec_bitvec_set_at]

/* ****** ****** */

#endif // end of [#ifndef ATS_CONTRIB_LINBITVEC_CATS]

/* ****** ****** */

/* end of [linbitvec.cats] */
