#define R_NO_REMAP
#define STRICT_R_HEADERS
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "libbeecs.h"

// Run simulation and return worker cohorts data as JSON
SEXP run_beecs(SEXP params) {
  const char* params_json = CHAR(STRING_ELT(params, 0));
  
  // Call Go function
  char* json_data = runBeecs((char*)params_json);
  if (json_data == NULL || json_data[0] == '\0') {
    if (json_data != NULL) {
      free(json_data);
    }
    return R_NilValue;
  }

  // Create R string containing JSON
  SEXP result = PROTECT(Rf_mkString(json_data));
  
  // Free the C string allocated by Go
  free(json_data);
  
  UNPROTECT(1);
  return result;
}

// Standard R package stuff
static const R_CallMethodDef CallEntries[] = {
  {"run_beecs", (DL_FUNC) &run_beecs, 1},
  {NULL, NULL, 0}
};

attribute_visible void init_gobeecs(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
