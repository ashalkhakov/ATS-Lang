/************************************************************************/
/*                                                                      */
/*                         Applied Type System                          */
/*                                                                      */
/*                              Hongwei Xi                              */
/*                                                                      */
/************************************************************************/

/*
 * ATS - Unleashing the Potential of Types!
 *
 * Copyright (C) 2002-2008 Hongwei Xi.
 *
 * ATS is  free software;  you can redistribute it and/or modify it under
 * the  terms of the  GNU General Public License as published by the Free
 * Software Foundation; either version 2.1, or (at your option) any later
 * version.
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

#ifndef ATS_LIBC_STDIO_CATS
#define ATS_LIBC_STDIO_CATS

/* ****** ****** */

#include <errno.h>
#include <stdio.h>

/* ****** ****** */

#include "ats_types.h"

typedef fpos_t ats_fpos_type ;
typedef FILE ats_FILE_viewtype ;

/* --------------------------------------- */

static inline
ats_void_type
atslib_clearerr(ats_ptr_type fil) {
  clearerr ((FILE*)fil) ; return ;
}

/* --------------------------------------- */

static inline
ats_int_type
atslib_fclose_err(ats_ptr_type fil) {
  return fclose((FILE*)fil) ;
}

static inline
ats_void_type
atslib_fclose_exn(ats_ptr_type fil) {
  int err = fclose((FILE*)fil) ;
  if (err < 0) {
    perror ("fclose") ;
    ats_exit_errmsg (1, "exit(ATS): [fclose] failed\n") ;
  } // end of [if]
  return ;
}

static inline
ats_void_type
atslib_fclose_stdin() {
  atspre_stdin_view_get() ;
  atslib_fclose(stdin) ;
  return ;
}

static inline
ats_void_type
atslib_fclose_stdout() {
  atspre_stdout_view_get() ;
  atslib_fclose(stdout) ;
  return ;
}

static inline
ats_void_type
atslib_fclose_stderr() {
  atspre_stderr_view_get() ;
  atslib_fclose(stderr) ;
  return ;
}

/* --------------------------------------- */

static inline
ats_int_type
atslib_feof (ats_ptr_type fil) {
  return feof((FILE*)fil) ;
}

static inline
ats_int_type
atslib_ferror(ats_ptr_type fil) {
  return ferror((FILE*)fil) ;
}

/* --------------------------------------- */

static inline
ats_int_type
atslib_fflush_err(ats_ptr_type fil) {
  return fflush((FILE*)fil) ;
}

static inline
ats_void_type
atslib_fflush_exn (ats_ptr_type fil) {
  int err = fflush((FILE*)fil) ;
  if (err < 0) {
    perror ("fflush") ;
    ats_exit_errmsg (1, "exit(ATS): [fflush] failed\n") ;
  } // end of [if]
  return ;
} /* end of [atslib_fflush_exn] */

static inline
ats_void_type
atslib_fflush_stdout (void) {
  atspre_stdout_view_get ();
  atslib_fflush_exn (stdout);
  atspre_stdout_view_set () ;
  return ;
} /* end of [atslib_fflush_stdout] */

/* --------------------------------------- */

static inline
ats_int_type
atslib_fgetc_err (ats_ptr_type fil) { return fgetc((FILE*)fil) ; }

static inline
ats_int_type
atslib_getchar () {
  int i ;
  atspre_stdin_view_get (); i = getchar (); atspre_stdin_view_set () ;
  return i ;
} /* end of [atslib_getchar] */

/* --------------------------------------- */

static inline
ats_ptr_type
atslib_fgets_err
  (ats_ptr_type buf, ats_int_type n, ats_ptr_type fil) {
  return fgets((char*)buf, (int)n, (FILE*)fil) ;
}

static inline
ats_void_type
atslib_fgets_exn
  (ats_ptr_type buf, ats_int_type n, ats_ptr_type fil) {
  ats_ptr_type p ;
  p = fgets((char*)buf, (int)n, (FILE*)fil) ;
  if (!p) {
    perror ("fgets") ;
    ats_exit_errmsg(1, "exit(ATS): [fgets] failed\n") ;
  }
  return ;  
} /* end of [atslib_fgets_exn] */

/* --------------------------------------- */

static inline
ats_int_type
atslib_fileno(ats_ptr_type fil) { return fileno((FILE*)fil) ; }

/* --------------------------------------- */

static inline
ats_ptr_type
atslib_fopen_err
  (ats_ptr_type name, ats_ptr_type mode) {
  return fopen((char*)name, (char*)mode) ;
}

static inline
ats_ptr_type
atslib_fopen_exn
  (ats_ptr_type name, ats_ptr_type mode) {
  FILE *fil = fopen((char*)name, (char*)mode) ;
  if (!fil) {
    perror ("fopen") ; atspre_exit_prerrf (
      1, "exit(ATS): [fopen(\"%s\", \"%s\")] failed\n", name, mode
    ) ;
  }
  return fil ;
} /* atslib_fopen_exn */

/* --------------------------------------- */

static inline
ats_int_type
atslib_fputc_err
  (ats_char_type c, ats_ptr_type fil) {
  return fputc((unsigned char)c, (FILE*)fil) ;
}

static inline
ats_void_type
atslib_fputc_exn
  (ats_char_type c, ats_ptr_type fil) {
  int n = fputc((unsigned char)c, (FILE*)fil) ;
  if (n < 0) {
    perror ("fputc") ;
    atspre_exit_prerrf (1, "exit(ATS): [fputc(%c)] failed\n", c) ;
  }
  return ;
}

/* --------------------------------------- */

static inline
ats_int_type
atslib_fputs_err(ats_ptr_type s, ats_ptr_type fil) {
  return fputs ((char*)s, (FILE*)fil) ;
}

static inline
ats_void_type
atslib_fputs_exn(ats_ptr_type s, ats_ptr_type fil) {
  int n = fputs ((char*)s, (FILE*)fil) ;
  if (n < 0) {
    perror ("fputs") ;
    atspre_exit_prerrf (1, "exit(ATS): [fputs(%s)] failed\n", s) ;
  }
  return ;
}

/* --------------------------------------- */

static inline
ats_int_type
atslib_fread
  (ats_ptr_type buf, ats_size_type sz, ats_size_type n, ats_ptr_type fil)
{
  return fread ((void*)buf, sz, n, (FILE*)fil) ;
}

static inline
ats_int_type
atslib_fread_byte
  (ats_ptr_type buf, ats_size_type n, ats_ptr_type fil) {
  return fread ((void*)buf, 1, n, (FILE*)fil) ;
}

static inline
ats_int_type
atslib_fread_byte_exn
  (ats_ptr_type buf, ats_size_type ntotal, ats_ptr_type fil) {
  int nread ;
  nread = fread ((void*)buf, 1, ntotal, (FILE*)fil) ;
  if (nread < ntotal) {
    perror ("fread") ;
    ats_exit_errmsg (1, "exit(ATS): [fread] failed\n") ;
  }
  return ;
}

/* --------------------------------------- */

static inline
ats_ptr_type
atslib_freopen_err
  (ats_ptr_type name, ats_ptr_type mode, ats_ptr_type fil) {
  return freopen(name, mode, (FILE*)fil) ;
}

static inline
ats_void_type
atslib_freopen_exn
  (ats_ptr_type name, ats_ptr_type mode, ats_ptr_type fil) {
  FILE *fil_new = freopen(name, mode, (FILE*)fil) ;
  if (!fil_new) {
    perror ("freopen") ; atspre_exit_prerrf (
      1, "exit(ATS): [freopen(\"%s\", \"%s\")] failed\n", name, mode
    ) ;
  }
  return ;
}

/* --------------------------------------- */

static inline
ats_void_type
atslib_freopen_stdin
  (ats_ptr_type name) {
  FILE *fil_new ;
  atspre_stdin_view_get() ;
  fil_new = freopen(name, "r", stdin) ;
  if (!fil_new) {
    perror ("freopen") ; atspre_exit_prerrf (
      1, "exit(ATS): [freopen_stdin(\"%s\")] failed\n", name
    ) ;
  }
  atspre_stdin_view_set() ;
  return ;
}

static inline
ats_void_type
atslib_freopen_stdout
  (ats_ptr_type name) {
  FILE *fil_new ;
  atspre_stdout_view_get () ;
  fil_new = freopen(name, "w", stdout) ;
  if (!fil_new) {
    perror ("freopen") ; atspre_exit_prerrf (
      1, "exit(ATS): [freopen_stdout(\"%s\")] failed\n", name
    ) ;
  }
  atspre_stdout_view_set () ;
  return ;
}

static inline
ats_void_type
atslib_freopen_stderr
  (ats_ptr_type name) {
  FILE *fil_new ;
  atspre_stderr_view_get() ;
  fil_new = freopen(name, "w", stderr) ;
  if (!fil_new) {
    perror ("freopen") ; atspre_exit_prerrf (
      1, "exit(ATS): [freopen_stderr(\"%s\")] failed\n", name
    ) ;
  }
  atspre_stderr_view_set() ;
  return ;
}

/* --------------------------------------- */

static inline
ats_int_type atslib_fseek_err
  (ats_ptr_type fil, ats_lint_type offset, ats_int_type whence) {
  return fseek ((FILE*)fil, offset, whence) ;
} /* atslib_fseek_err */

static inline
ats_int_type atslib_fseek_exn
  (ats_ptr_type fil, ats_lint_type offset, ats_int_type whence) {
  int err ;
  err = fseek ((FILE*)fil, offset, whence) ;
  if (err < 0) {
    perror ("fseek") ;
    ats_exit_errmsg (1, "exit(ATS): [fseek] failed\n") ;
  }
  return ;
} /* atslib_fseek_exn */

/* --------------------------------------- */

static inline
ats_lint_type
atslib_ftell_err
  (ats_ptr_type fil) { return ftell((FILE*)fil) ; }

static inline
ats_lint_type
atslib_ftell_exn
  (ats_ptr_type fil) {
  long int ret = ftell((FILE*)fil) ;
  if (ret < 0) {
    perror ("ftell") ;
    ats_exit_errmsg (1, "exit(ATS): [ftell] failed\n") ;
  }
  return ret ;
}

/* --------------------------------------- */

static inline
ats_int_type atslib_fwrite (
  ats_ptr_type buf, ats_size_type sz, ats_size_type n, ats_ptr_type fil
) {
  return fwrite((void*)buf, sz, n, (FILE*)fil) ;
} /* atslib_fwrite */

static inline
ats_int_type
atslib_fwrite_byte
  (ats_ptr_type buf, ats_size_type n, ats_ptr_type fil) {
  return fwrite((void*)buf, 1, n, (FILE*)fil) ;
} /* atslib_fwrite_byte */

static inline
ats_void_type
atslib_fwrite_byte_exn
  (ats_ptr_type buf0, ats_size_type ntotal, ats_ptr_type fil) {
  char *buf = (char*) buf0 ; size_t nwritten ; size_t i ;
  nwritten = fwrite((void*)buf, 1, ntotal, (FILE*)fil) ;
  if (nwritten < ntotal) {
    perror ("fwrite") ;
    ats_exit_errmsg (1, "exit(ATS): [fwrite] failed\n") ; 
  }
  return ;
} /* end of [atslib_fwrite_all_byte] */

/* --------------------------------------- */

static inline
ats_void_type
atslib_perror(ats_ptr_type msg) {
  atspre_stderr_view_get () ;
  perror ((char*)msg) ;
  atspre_stderr_view_set () ;
  return ;
}

/* --------------------------------------- */

static inline
ats_int_type
atslib_putchar (ats_char_type c) {
  int i ;
  atspre_stdout_view_get () ;
  i = putchar((unsigned char)c) ;
  atspre_stdout_view_set () ;
  return i ;
} /* end of [atslib_putchar] */

/* --------------------------------------- */

static inline
ats_int_type
atslib_puts_err (ats_ptr_type str) {
  int err ;
  atspre_stdout_view_get () ;
  err = puts ((char*)str) ;
  atspre_stdout_view_set () ;
  return err ;
} /* end of [atslib_puts_err] */

static inline
ats_void_type
atslib_puts_exn (ats_ptr_type str) {
  int err ;
  atspre_stdout_view_get () ;
  err = puts ((char*)str) ;
  atspre_stdout_view_set () ;
  if (err < 0) {
    perror ("puts") ;
    ats_exit_errmsg (1, "exit(ATS): [remove] failed\n") ;
  } /* end of [if] */
  return ;
} /* end of [atslib_puts_exn] */

/* --------------------------------------- */

static inline
ats_int_type
atslib_remove_err (ats_ptr_type path) {
  return remove((char*)path) ;
}

static inline
ats_int_type
atslib_remove_exn (ats_ptr_type path) {
  int err = remove((char*)path) ;
  if (err < 0) {
    perror ("remove") ;
    ats_exit_errmsg (1, "exit(ATS): [remove] failed\n") ;
  }
  return ;
}

/* --------------------------------------- */

static inline
ats_int_type
atslib_rename_err
  (ats_ptr_type oldpath, ats_ptr_type newpath) {
  return rename((char*)oldpath, (char*)newpath) ;
}

static inline
ats_void_type
atslib_rename_exn
  (ats_ptr_type oldpath, ats_ptr_type newpath) {
  int err = rename((char*)oldpath, (char*)newpath) ;
  if (err < 0) {
    perror ("rename") ;
    ats_exit_errmsg (1, "exit: [rename] failed\n") ;   
  }
  return ;
}

/* --------------------------------------- */

static inline
ats_void_type
atslib_rewind (ats_ptr_type fil) {
  rewind((FILE*)fil) ; return ;
}

/* --------------------------------------- */

static inline
ats_ptr_type
atslib_tmpfile_err () { return tmpfile() ; }

static inline
ats_ptr_type
atslib_tmpfile_exn () {
  FILE* fil =  tmpfile() ;
  if (!fil) {
    perror ("tmpfile") ;
    ats_exit_errmsg (1, "exit(ATS): [tmpfile] failed\n") ;
  }
  return fil ;
} /* end of [atslib_tmpfile_exn] */

/* --------------------------------------- */

static inline
ats_int_type
atslib_ungetc_err
  (ats_char_type c, ats_ptr_type fil) {
  return ungetc((unsigned char)c, (FILE*)fil) ;
}

static inline
ats_void_type
atslib_ungetc_exn
  (ats_char_type c, ats_ptr_type fil) {
  int err = ungetc((unsigned char)c, (FILE*)fil) ;
  if (err < 0) {
    perror ("ungetc") ;
    ats_exit_errmsg (1, "exit(ATS): [ungetc] failed\n") ;
  }
  return ;
} /* end of [atslib_ungetc_exn] */

/* --------------------------------------- */

#endif /* ATS_LIBC_STDIO_CATS */

/* end of [stdio.cats] */
