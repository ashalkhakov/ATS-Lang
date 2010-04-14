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

#ifndef ATSCTRB_GOBJECT_CATS
#define ATSCTRB_GOBJECT_CATS

/* ****** ****** */

#include "glib-object.h"

/* ****** ****** */

#define atsctrb_g_object_is_floating g_object_is_floating

#define atsctrb_g_object_ref g_object_ref
#define atsctrb_g_object_unref g_object_unref

#define atsctrb_g_signal_connect g_signal_connect
#define atsctrb_g_signal_connect_after g_signal_connect_after
#define atsctrb_g_signal_connect_swapped g_signal_connect_swapped

/* ****** ****** */

#endif /* ATSCTRB_GOBJECT_CATS */
