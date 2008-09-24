/************************************************************************/
/*                                                                      */
/*                         Applied Type System                          */
/*                                                                      */
/*                              Hongwei Xi                              */
/*                                                                      */
/************************************************************************/

/*
 * ATS - Unleashing the Power of Types!
 *
 * Copyright (C) 2002-2008 Hongwei Xi.
 *
 * ATS is free software;  you can  redistribute it and/or modify it under
 * the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
 * Free Software Foundation; either version 2.1, or (at your option)  any
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
 */

/* ****** ****** */

/* author: Hongwei Xi (hwxi AT cs DOT bu DOT edu) */

/* ****** ****** */

#ifndef __ATS_BASICS_H
#define __ATS_BASICS_H

/* ****** ****** */

/*
#define ATSstringize(id) # id
*/

/* ****** ****** */

#define ATSextern(ty, var) extern ty var
#define ATSstatic(ty, var) static ty var
#define ATSstatic_void(var)
#define ATSunused __attribute__ ((unused))
#define ATSlocal(ty, var) ty ATSunused var
#define ATSlocal_void(var)
#define ATSglobal(ty, var) ty var

/* ****** ****** */

#define ATSdeadcode() \
  do { \
    fprintf ( \
      stderr, \
      "Error in the file %s on line %d: the deadcode is executed!\n", \
      __FILE__, __LINE__ \
    ); \
    abort (); \
  } while (0);

/* boolean values */
#define ats_true_bool 1
#define ats_false_bool 0

/* ****** ****** */

/* closure function selection */
#define ats_closure_fun(f) ((ats_clo_ptr_type)f)->closure_fun

/* ****** ****** */

/* while loop */
#define ats_while_beg_mac(clab) while(ats_true_bool) { clab:
#define ats_while_end_mac(blab, clab) goto clab ; blab: break ; }

/* ****** ****** */

#define ats_instr_move_ref_mac(tmp, hit, val) \
  do { tmp = ATS_MALLOC (sizeof(hit)) ; *(hit*)tmp = val ; } while (0)

/* ****** ****** */

/* [main_prelude] is called in the function [mainats] */
extern void main_prelude () ;

/* ****** ****** */

#endif /* __ATS_BASICS_H */

/* end of [ats_basics.h] */
