#ifndef MKL_SHIM_H
#define MKL_SHIM_H

#define REAL double
#define INT long int

void dgemm_shim_(char *const transA, char *const transB, INT *m, INT *n, INT *k,
                 REAL *alpha, REAL *a, INT *lda, REAL *b, INT *ldb, REAL *beta,
                 REAL *c, INT *ldc);

#endif
