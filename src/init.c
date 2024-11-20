#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>
#include "emd.h"

/*
 *   The following symbols/expressions for .NAME have been omitted
 *
 *       fun
 *
 *         Most likely possible values need to be added below.
 *         */

/* FIXME: 
 *    Check these declarations against the C/Fortran source code.
 *    */


static R_NativePrimitiveArgType emdR_t[] = {
	INTSXP,
	INTSXP,
	REALSXP,
	REALSXP,
	REALSXP,
	REALSXP,
	REALSXP,
	REALSXP,
	REALSXP,
	REALSXP,
	INTSXP
};
static R_NativePrimitiveArgType emdR_gd_t[] = {
	INTSXP,
	INTSXP,
	REALSXP,
	REALSXP,
	REALSXP,
	REALSXP,
	REALSXP,
	REALSXP,
	REALSXP,
	REALSXP,
	INTSXP
};

static const R_CMethodDef cMethods[] = {
	   {"emdR", (DL_FUNC) &emdR, 11, emdR_t},
	   {"emdR_gd", (DL_FUNC) &emdR_gd, 11, emdR_gd_t},
	      {NULL, NULL, 0, NULL}
};
/* .Call calls */



extern SEXP bgb(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP dbbmm2(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP llBGBvar(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
	{"bgb",      (DL_FUNC) &bgb,      10},
	{"dbbmm2",   (DL_FUNC) &dbbmm2,   10},
	{"llBGBvar", (DL_FUNC) &llBGBvar,  2},
	{NULL, NULL, 0}
};

void R_init_move(DllInfo *dll)
{
	R_registerRoutines(dll, cMethods, CallEntries, NULL, NULL);
	R_useDynamicSymbols(dll, FALSE);
}

