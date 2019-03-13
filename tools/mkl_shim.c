#include <assert.h>
#include <mkl.h>

#include "mkl_shim.h"

CBLAS_TRANSPOSE fortran_to_c_trans(char *const trans) {
  char t = *trans;
  if (t == 'n' || t == 'N') {
    return CblasNoTrans;
  } else if (t == 't' || t == 'T') {
    return CblasTrans;
  }

  assert(0 && "Invalid transpose char");
}

void dgemm_shim_(char *const transA, char *const transB, INT *m, INT *n, INT *k,
                 REAL *alpha, REAL *a, INT *lda, REAL *b, INT *ldb, REAL *beta,
                 REAL *c, INT *ldc) {
  CBLAS_LAYOUT layout = CblasRowMajor;
  cblas_dgemm(layout, fortran_to_c_trans(transA), fortran_to_c_trans(transB),
              *m, *n, *k, *alpha, a, *lda, b, *ldb, *beta, c, *ldc);
}
