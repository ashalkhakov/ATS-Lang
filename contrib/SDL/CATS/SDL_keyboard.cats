/***********************************************************************/
/*                                                                     */
/*                         Applied Type System                         */
/*                                                                     */
/*                              Hongwei Xi                             */
/*                                                                     */
/***********************************************************************/

/*
** ATS - Unleashing the Potential of Types!
**
** Copyright (C) 2002-2010 Hongwei Xi, Boston University
**
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of the GNU LESSER GENERAL PUBLIC LICENSE as published by the
** Free Software Foundation; either version 2.1, or (at your option)  any
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

// Author of the file: Hongwei Xi (hwxi AT cs DOT bu DOT edu)
// Starting time: January, 2010

/* ****** ****** */

#ifndef ATSCTRB_SDL_SDL_KEYBOARD_CATS
#define ATSCTRB_SDL_SDL_KEYBOARD_CATS

/* ****** ****** */

#include "contrib/SDL/CATS/SDL_keysym.cats"

/* ****** ****** */

#define atsctrb_SDL_keysym_scancode(ref) \
  ats_field_getval(SDL_keysym, ref, scancode)
#define atsctrb_SDL_keysym_sym(ref) \
  ats_field_getval(SDL_keysym, ref, sym)
#define atsctrb_SDL_keysym_mod(ref) \
  ats_field_getval(SDL_keysym, ref, mod)
#define atsctrb_SDL_keysym_unicode(ref) \
  ats_field_getval(SDL_keysym, ref, unicode)

/* ****** ****** */

#endif // end of [ATSCTRB_SDL_SDL_KEYBOARD_CATS]

/* end of [SDL_keyboard.cats] */
