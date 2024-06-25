#include <R.h>
#include <Rinternals.h>
#include "libbeecs.h"

SEXP gobeecs(SEXP x){
  return Rf_ScalarInteger( runbeecs( (char*)CHAR(STRING_ELT(x,0)) ) );
}
