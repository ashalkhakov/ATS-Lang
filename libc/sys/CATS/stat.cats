/************************************************************************/
/*                                                                      */
/*                         Applied Type System                          */
/*                                                                      */
/*                              Hongwei Xi                              */
/*                                                                      */
/************************************************************************/

/*
** ATS - Unleashing the Power of Types!
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

#ifndef ATS_LIBC_SYS_STAT_CATS
#define ATS_LIBC_SYS_STAT_CATS

/* ****** ****** */

#include <sys/stat.h>
typedef struct stat ats_stat_type ;

/* ****** ****** */

extern
void perror (const char *msg) ; // declared in [stdio.h]

//
// HX: implemented in [prelude/DATS/basics.dats]
//
extern
ats_void_type ats_exit_errmsg(ats_int_type n, ats_ptr_type msg) ;

/* ****** ****** */

ATSinline()
ats_bool_type
atslib_S_ISBLK (ats_mode_type m) { return S_ISBLK(m) ; }

ATSinline()
ats_bool_type
atslib_S_ISCHR (ats_mode_type m) { return S_ISCHR(m) ; }

ATSinline()
ats_bool_type
atslib_S_ISDIR (ats_mode_type m) { return S_ISDIR(m) ; }

ATSinline()
ats_bool_type
atslib_S_ISFIFO (ats_mode_type m) { return S_ISFIFO(m) ; }

ATSinline()
ats_bool_type
atslib_S_ISREG (ats_mode_type m) { return S_ISREG(m) ; }

ATSinline()
ats_bool_type
atslib_S_ISLNK (ats_mode_type m) { return S_ISLNK(m) ; }

ATSinline()
ats_bool_type
atslib_S_ISSOCK (ats_mode_type m) { return S_ISSOCK(m) ; }

/* ****** ****** */

#define atslib_chmod_err chmod

ATSinline()
ats_void_type
atslib_chmod_exn (
  ats_ptr_type path, ats_mode_type mode
) {
  int err = chmod ((char*)path, mode) ;
  if (err < 0) {
    perror ("chmod"); ats_exit_errmsg (1, "exit(ATS): [chmod] failed.\n") ;
  } // end of [if]
  return ;
} /* end of [atslib_chmod_exn] */

/* ****** ****** */

#define atslib_mkdir_err mkdir

ATSinline()
ats_void_type
atslib_mkdir_exn (
  ats_ptr_type path, ats_mode_type mode
) {
  int err = mkdir ((char*)path, mode) ;
  if (err < 0) {
    perror ("mkdir"); ats_exit_errmsg (1, "exit(ATS): [mkdir] failed.\n") ;
  } // end of [if]
  return ;
} /* end of [atslib_mkdir_exn] */

/* ****** ****** */

#define atslib_stat_err stat

ATSinline()
ats_void_type
atslib_stat_exn (
  ats_ptr_type name, ats_ptr_type buf
) {
  int err ;
  err = stat ((char*)name, (ats_stat_type*)buf) ;
  if (err < 0) {
    perror ("stat"); ats_exit_errmsg (1, "exit(ATS): [stat] failed.\n") ;
  } // end of [if]
  return ;
} /* end of [atslib_stat_exn] */

/* ****** ****** */

#define atslib_lstat_err lstat

ATSinline()
ats_void_type
atslib_lstat_exn (
  ats_ptr_type name, ats_ptr_type buf
) {
  int err ;
  err = lstat ((char*)name, (ats_stat_type*)buf) ;
  if (err < 0) {
    perror ("lstat"); ats_exit_errmsg (1, "exit(ATS): [lstat] failed.\n") ;
  } // end of [if]
  return ;
} /* end of [atslib_lstat_exn] */

/* ****** ****** */

ATSinline()
ats_mode_type
atslib_umask (
  ats_mode_type mask_new
) {
  return umask (mask_new) ; /* the original mask is returned */
} /* end of [atslib_umask] */

/* ****** ****** */

#endif /* end of [ATS_LIBC_SYS_STAT_CATS] */

/* end of [stat.cats] */
