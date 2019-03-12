#include <mkl.h>

void dgemm_shim_(void) { cblas_dgemm(); }
