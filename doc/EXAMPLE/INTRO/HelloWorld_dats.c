/*
 *
 * The following C code is generated by ATS/Anairiats
 * The compilation time is: 2008-6-6:  3h:42m
 *
 */

/* include some .h files */
#include "ats_basics.h"
#include "ats_exception.h"
#include "ats_memory.h"
#include "ats_types.h"

/* include some .cats files */
#include "prelude/CATS/array.cats"
#include "prelude/CATS/basics.cats"
#include "prelude/CATS/bool.cats"
#include "prelude/CATS/byte.cats"
#include "prelude/CATS/char.cats"
#include "prelude/CATS/float.cats"
#include "prelude/CATS/integer.cats"
#include "prelude/CATS/pointer.cats"
#include "prelude/CATS/printf.cats"
#include "prelude/CATS/reference.cats"
#include "prelude/CATS/string.cats"

/* external codes at top */
/* type definitions */
/* external dynamic constructor declarations */
/* external dynamic constant declarations */
extern ats_void_type atspre_print_newline () ;
extern ats_void_type atspre_print_string (ats_ptr_type) ;

/* sum constructor declarations */
/* exn constructor declarations */
/* global dynamic (non-functional) constant declarations */
/* internal function declarations */

/* static temporary variable declarations */
/* external value variable declarations */

/* function implementations */

ats_void_type
mainats (ats_int_type arg0, ats_ref_type arg1) {
/* local vardec */
ATSlocal_void (tmp0) ;
ATSlocal_void (tmp1) ;

__ats_lab_mainats:
/* tmp1 = */ atspre_print_string ("Hello, world!") ;
/* tmp0 = */ atspre_print_newline () ;
return ;
} /* end of [mainats] */

/* static load function */

static int _2fhome_2ffac2_2fhwxi_2fresearch_2fATS_2fIMPLEMENT_2fGeizella_2fAnairiats_2fsvn_2fats_2dlang_2fdoc_2fEXAMPLE_2fINTRO_2fHelloWorld_2edats__staload_flag = 0 ;

ats_void_type _2fhome_2ffac2_2fhwxi_2fresearch_2fATS_2fIMPLEMENT_2fGeizella_2fAnairiats_2fsvn_2fats_2dlang_2fdoc_2fEXAMPLE_2fINTRO_2fHelloWorld_2edats__staload () {
if (_2fhome_2ffac2_2fhwxi_2fresearch_2fATS_2fIMPLEMENT_2fGeizella_2fAnairiats_2fsvn_2fats_2dlang_2fdoc_2fEXAMPLE_2fINTRO_2fHelloWorld_2edats__staload_flag) return ;
_2fhome_2ffac2_2fhwxi_2fresearch_2fATS_2fIMPLEMENT_2fGeizella_2fAnairiats_2fsvn_2fats_2dlang_2fdoc_2fEXAMPLE_2fINTRO_2fHelloWorld_2edats__staload_flag = 1 ;
return ;
} /* staload function */

/* dynamic load function */

// extern int _2fhome_2ffac2_2fhwxi_2fresearch_2fATS_2fIMPLEMENT_2fGeizella_2fAnairiats_2fsvn_2fats_2dlang_2fdoc_2fEXAMPLE_2fINTRO_2fHelloWorld_2edats__dynload_flag ;

ats_void_type _2fhome_2ffac2_2fhwxi_2fresearch_2fATS_2fIMPLEMENT_2fGeizella_2fAnairiats_2fsvn_2fats_2dlang_2fdoc_2fEXAMPLE_2fINTRO_2fHelloWorld_2edats__dynload () {
// _2fhome_2ffac2_2fhwxi_2fresearch_2fATS_2fIMPLEMENT_2fGeizella_2fAnairiats_2fsvn_2fats_2dlang_2fdoc_2fEXAMPLE_2fINTRO_2fHelloWorld_2edats__dynload_flag = 1 ;
_2fhome_2ffac2_2fhwxi_2fresearch_2fATS_2fIMPLEMENT_2fGeizella_2fAnairiats_2fsvn_2fats_2dlang_2fdoc_2fEXAMPLE_2fINTRO_2fHelloWorld_2edats__staload () ;

/* marking static variables for GC */

/* marking external values for GC */

/* code for dynamic loading */
return ;
} /* dynload function */

int main (int argc, char *argv[]) {
ats_gc_init () ;
mainats_prelude () ;
_2fhome_2ffac2_2fhwxi_2fresearch_2fATS_2fIMPLEMENT_2fGeizella_2fAnairiats_2fsvn_2fats_2dlang_2fdoc_2fEXAMPLE_2fINTRO_2fHelloWorld_2edats__dynload () ;
mainats ((ats_int_type)argc, (ats_ptr_type)argv) ;
return (0) ;
} /* end of main */

/* external typedefs */
/* external codes at mid */
/* external codes at bot */

/* ****** ****** */

/* end of [HelloWorld_dats.c] */
