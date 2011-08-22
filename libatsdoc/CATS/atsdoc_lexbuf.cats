/* ******************************************************************* */
/*                                                                     */
/*                         Applied Type System                         */
/*                                                                     */
/*                              Hongwei Xi                             */
/*                                                                     */
/* ******************************************************************* */

/*
** ATS/Postiats - Unleashing the Potential of Types!
** Copyright (C) 2011-20?? Hongwei Xi, ATS Trustworthy Software, Inc.
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
*/

/* ****** ****** */
//
// Author: Hongwei Xi (gmhwxi@gmail.com)
// Start Time: July, 2011
//
/* ****** ****** */

#ifndef ATSDOC_LEXBUF_CATS
#define ATSDOC_LEXBUF_CATS

/* ****** ****** */

#include "libats/CATS/linqueue_arr.cats"

/* ****** ****** */

#include "libatsdoc/CATS/atsdoc_reader.cats"

/* ****** ****** */

typedef struct {
//
  atslib_linqueue_arr_QUEUE cbuf ;
//
  ats_lint_type base ;
  ats_int_type base_nrow ; // line number
  ats_int_type base_ncol ; // line offset
//
  ats_int_type nspace ; // leading space
//
  atsdoc_reader_struct reader ; // for getchar
//
} atsdoc_lexbuf_struct ;

/* ****** ****** */

#endif // end of [ATSDOC_LEXBUF_CATS]

/* ****** ****** */

/* end of [atsdoc_lexbuf.cats] */
