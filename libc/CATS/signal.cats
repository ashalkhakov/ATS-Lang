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
** Copyright (C) 2002-2010 Hongwei Xi.
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
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*/

/* ****** ****** */

/* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) */

/* ****** ****** */

#ifndef ATS_LIBC_SIGNAL_CATS
#define ATS_LIBC_SIGNAL_CATS

/* ****** ****** */

#include <signal.h>

/* ****** ****** */

typedef ats_int_type signum_t ;
typedef void (*sighandler_t)(signum_t) ;

/* ****** ****** */

/*
//
// HX-2010-09-27:
// these two are now cast functions
//
ATSinline()
ats_ptr_type
atslib_fun_of_sighandler (ats_ptr_type f) { return f ; }

ATSinline()
ats_ptr_type
atslib_sighandler_of_fun (ats_ptr_type f) { return f ; }
*/

/* ****** ****** */

#define atslib_signal signal

/* ****** ****** */

#endif /* ATS_LIBC_SIGNAL_CATS */
