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
** Copyright (C) 2002-2008 Hongwei Xi.
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
// Starting time: December, 2009

/* ****** ****** */

#ifndef ATSTRIB_GL_GLUT_CATS
#define ATSTRIB_GL_GLUT_CATS

/* ****** ****** */

#include <GL/glut.h>

/* ****** ****** */

/*
** Initialization functions, see fglut_init.c
*/
#define atsctrb_glutInit glutInit
#define atsctrb_glutInitWindowPosition glutInitWindowPosition
#define atsctrb_glutInitWindowSize glutInitWindowSize
#define atsctrb_glutInitDisplayMode glutInitDisplayMode
#define atsctrb_glutInitDisplayString glutInitDisplayString

/* ****** ****** */

/*
** Process loop function, see freeglut_main.c
*/
#define atsctrb_glutMainLoop glutMainLoop

/* ****** ****** */

/*
** Window management functions, see freeglut_window.c
*/
#define atsctrb_glutCreateWindow glutCreateWindow
#define atsctrb_glutCreateSubWindow glutCreateSubWindow
#define atsctrb_glutDestroyWindow atsctrb_glutDestroyWindow
#define atsctrb_glutGetWindow glutGetWindow
#define atsctrb_glutSetWindow glutSetWindow
#define atsctrb_glutSetWindowTitle glutSetWindowTitle
#define atsctrb_glutSetIconTitle glutSetIconTitle
#define atsctrb_glutReshapeWindow glutReshapeWindow
#define atsctrb_glutPositionWindow glutPositionWindow
#define atsctrb_glutShowWindow glutShowWindow
#define atsctrb_glutHideWindow glutHideWindow
#define atsctrb_glutIconifyWindow glutIconifyWindow
#define atsctrb_glutPushWindow glutPushWindow
#define atsctrb_glutPopWindow glutPopWindow
#define atsctrb_glutFullScreen glutFullScreen

/* ****** ****** */

/*
** Display-connected functions, see freeglut_display.c
*/
#define atsctrb_glutPostWindowRedisplay glutPostWindowRedisplay
#define atsctrb_glutPostRedisplay glutPostRedisplay
#define atsctrb_glutSwapBuffers glutSwapBuffers

/* ****** ****** */

// Global callback functions, see freeglut_callbacks.c

static inline
ats_void_type
atsctrb_glutTimerFunc (
  ats_uint_type time
, ats_ptr_type callback
, ats_int_type value
) {
  glutTimerFunc (time, (void (*)(int))callback, value); return ;
} // end of [atsctrb_glutTimerFunc]

/* ****** ****** */

static inline
ats_void_type
atsctrb_glutIdleFunc
  (ats_ptr_type callback) {
  glutIdleFunc ((void (*)(void))callback) ; return ;
} // end of [atsctrb_glutIdleFunc]

static inline
ats_void_type
atsctrb_glutIdleFunc_null () {
  glutIdleFunc ((void (*)(void))0) ; return ;
} // end of [atsctrb_glutIdleFunc_null]

/* ****** ****** */

/*
** Window-specific callback functions, see freeglut_callbacks.c
*/

static inline
ats_void_type
atsctrb_glutKeyboardFunc
  (ats_ptr_type callback) {
  glutKeyboardFunc ((void (*)(unsigned char, int, int))callback) ;
  return ;
} // end of [atsctrb_glutKeyboardFunc]

static inline
ats_void_type
atsctrb_glutMouseFunc
  (ats_ptr_type callback) {
  glutMouseFunc ((void (*)(int, int, int, int))callback) ;
  return ;
} // end of [atsctrb_glutMouseFunc]

static inline
ats_void_type
atsctrb_glutSpecialFunc
  (ats_ptr_type callback) {
  glutSpecialFunc ((void (*)(int, int, int))callback) ; return ;
} // end of [atsctrb_glutSpecialFunc]

static inline
ats_void_type
atsctrb_glutReshapeFunc
  (ats_ptr_type callback) {
  glutReshapeFunc ((void (*)(int, int))callback) ; return ;
} // end of [atsctrb_glutReshapeFunc]

static inline
ats_void_type
atsctrb_glutVisibilityFunc
  (ats_ptr_type callback) {
  glutVisibilityFunc ((void (*)(int))callback) ; return ;
} // end of [atsctrb_glutVisibilityFunc]

static inline
ats_void_type
atsctrb_glutDisplayFunc
  (ats_ptr_type callback) {
  glutDisplayFunc ((void (*)(void))callback) ; return ;
} // end of [atsctrb_glutDisplayFunc]

static inline
ats_void_type
atsctrb_glutMotionFunc
  (ats_ptr_type callback) {
  glutMotionFunc ((void (*)(int, int))callback) ; return ;
} // end of [atsctrb_glutMotionFunc]

static inline
ats_void_type
atsctrb_glutPassiveMotionFunc
  (ats_ptr_type callback) {
  glutPassiveMotionFunc ((void (*)(int, int))callback) ; return ;
} // end of [atsctrb_glutPassiveMotionFunc]

static inline
ats_void_type
atsctrb_glutEntryFunc
  (ats_ptr_type callback) {
  glutEntryFunc ((void (*)(int))callback) ; return ;
} // end of [atsctrb_glutEntryFunc]

/* ****** ****** */

#define atsctrb_glutGet glutGet
#define atsctrb_glutDeviceGet glutDeviceGet
#define atsctrb_glutGetModifiers glutGetModifiers
#define atsctrb_glutLayerGet glutLayerGet

/* ****** ****** */

static inline
ats_void_type
atsctrb_glutWireCube_type
  (ats_double_type size) {
  glutWireCube (size) ; return ;
} // end of [atsctrb_glutWireCube_type]

static inline
ats_void_type
atsctrb_glutWireCube_GLtype
  (ats_GLdouble_type size) {
  glutWireCube (size) ; return ;
} // end of [atsctrb_glutWireCube_GLtype]

//

static inline
ats_void_type
atsctrb_glutSolidCube_type
  (ats_double_type size) {
  glutSolidCube (size) ; return ;
} // end of [atsctrb_glutSolidCube_type]

static inline
ats_void_type
atsctrb_glutSolidCube_GLtype
  (ats_GLdouble_type size) {
  glutSolidCube (size) ; return ;
} // end of [atsctrb_glutSolidCube_GLtype]

/* ****** ****** */

static inline
ats_void_type
atsctrb_glutWireSphere_type (
  ats_double_type radius
, ats_int_type slices
, ats_int_type stacks
) {
  glutWireSphere (radius, slices, stacks) ; return ;
} // end of [atsctrb_glutWireSphere_type]

static inline
ats_void_type
atsctrb_glutWireSphere_GLtype (
  ats_GLdouble_type radius
, ats_GLint_type slices
, ats_GLint_type stacks
) {
  glutWireSphere (radius, slices, stacks) ; return ;
} // end of [atsctrb_glutWireSphere_GLtype]

//

static inline
ats_void_type
atsctrb_glutSolidSphere_type (
  ats_double_type radius
, ats_int_type slices
, ats_int_type stacks
) {
  glutSolidSphere (radius, slices, stacks) ; return ;
} // end of [atsctrb_glutSolidSphere_type]

static inline
ats_void_type
atsctrb_glutSolidSphere_GLtype (
  ats_GLdouble_type radius
, ats_GLint_type slices
, ats_GLint_type stacks
) {
  glutSolidSphere (radius, slices, stacks) ; return ;
} // end of [atsctrb_glutSolidSphere_GLtype]

/* ****** ****** */

static inline
ats_void_type
atsctrb_glutWireCone_type (
  ats_double_type base
, ats_double_type height
, ats_int_type slices
, ats_int_type stacks
) {
  glutWireCone (base, height, slices, stacks) ; return ;
} // end of [atsctrb_glutWireCone_type]

static inline
ats_void_type
atsctrb_glutWireCone_GLtype (
  ats_GLdouble_type base
, ats_GLdouble_type height
, ats_GLint_type slices
, ats_GLint_type stacks
) {
  glutWireCone (base, height, slices, stacks) ; return ;
} // end of [atsctrb_glutWireCone_GLtype]

//

static inline
ats_void_type
atsctrb_glutSolidCone_type (
  ats_double_type base
, ats_double_type height
, ats_int_type slices
, ats_int_type stacks
) {
  glutSolidCone (base, height, slices, stacks) ; return ;
} // end of [atsctrb_glutSolidCone_type]

static inline
ats_void_type
atsctrb_glutSolidCone_GLtype (
  ats_GLdouble_type base
, ats_GLdouble_type height
, ats_GLint_type slices
, ats_GLint_type stacks
) {
  glutSolidCone (base, height, slices, stacks) ; return ;
} // end of [atsctrb_glutSolidCone_GLtype]

/* ****** ****** */

static inline
ats_void_type
atsctrb_glutWireTorus_type (
  ats_double_type innerRadius
, ats_double_type outerRadius
, ats_int_type sides
, ats_int_type rings
) {
  glutWireTorus (innerRadius, outerRadius, sides, rings) ; return ;
} // end of [atsctrb_glutWireTorus_type]

static inline
ats_void_type
atsctrb_glutWireTorus_GLtype (
  ats_GLdouble_type innerRadius
, ats_GLdouble_type outerRadius
, ats_GLint_type sides
, ats_GLint_type rings
) {
  glutWireTorus (innerRadius, outerRadius, sides, rings) ; return ;
} // end of [atsctrb_glutWireTorus_GLtype]

//

static inline
ats_void_type
atsctrb_glutSolidTorus_type (
  ats_double_type innerRadius
, ats_double_type outerRadius
, ats_int_type sides
, ats_int_type rings
) {
  glutSolidTorus (innerRadius, outerRadius, sides, rings) ; return ;
} // end of [atsctrb_glutSolidTorus_type]

static inline
ats_void_type
atsctrb_glutSolidTorus_GLtype (
  ats_GLdouble_type innerRadius
, ats_GLdouble_type outerRadius
, ats_GLint_type sides
, ats_GLint_type rings
) {
  glutSolidTorus (innerRadius, outerRadius, sides, rings) ; return ;
} // end of [atsctrb_glutSolidTorus_GLtype]

/* ****** ****** */

static inline
ats_void_type
atsctrb_glutWireTeapot_type
  (ats_double_type size) {
  glutWireTeapot (size) ; return ;
}

static inline
ats_void_type
atsctrb_glutWireTeapot_GLtype
  (ats_GLdouble_type size) {
  glutWireTeapot (size) ; return ;
}

//

static inline
ats_void_type
atsctrb_glutSolidTeapot_type
  (ats_double_type size) {
  glutSolidTeapot (size) ; return ;
}

static inline
ats_void_type
atsctrb_glutSolidTeapot_GLtype
  (ats_GLdouble_type size) {
  glutSolidTeapot (size) ; return ;
}

/* ****** ****** */

static inline
ats_void_type
atsctrb_glutWireDodecahedron () {
  glutWireDodecahedron () ; return ;
}

static inline
ats_void_type
atsctrb_glutSolidDodecahedron () {
  glutSolidDodecahedron () ; return ;
}

static inline
ats_void_type
atsctrb_glutWireOctahedron () {
  glutWireOctahedron () ; return ;
}

static inline
ats_void_type
atsctrb_glutSolidOctahedron () {
  glutSolidOctahedron () ; return ;
}

static inline
ats_void_type
atsctrb_glutWireTetrahedron () {
  glutWireTetrahedron () ; return ;
}

static inline
ats_void_type
atsctrb_glutSolidTetrahedron () {
  glutSolidTetrahedron () ; return ;
}

static inline
ats_void_type
atsctrb_glutWireIcosahedron () {
  glutWireIcosahedron () ; return ;
}

static inline
ats_void_type
atsctrb_glutSolidIcosahedron () {
  glutSolidIcosahedron () ; return ;
}

/* ****** ****** */

#endif /* ATSTRIB_GL_GLUT_CATS */

/* end of [glut.cats] */
