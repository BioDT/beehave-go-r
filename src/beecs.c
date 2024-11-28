#define R_NO_REMAP
#define STRICT_R_HEADERS
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "libbeecs.h"

SEXP gobeecs(SEXP x){
  return Rf_ScalarInteger( runbeecs( (char*)CHAR(STRING_ELT(x,0)) ) );
}

// Standard R package stuff
static const R_CallMethodDef CallEntries[] = {
  {"gobeecs", (DL_FUNC) &gobeecs, 1},
  {NULL, NULL, 0}
};

attribute_visible void init_gobeecs(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
