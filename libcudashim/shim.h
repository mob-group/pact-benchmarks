#ifndef SHIM_H
#define SHIM_H

#include <cuda_runtime.h>
#include <cublas_v2.h>

void dgemm_(
  char *transA, char *transB,
  int *m, int *n, int *k,
  double const* alpha, double const* A, int *lda,
  double const* B, int *ldb,
  double const* beta, double* C, int *ldc);

void zgemm_(
  char *transA, char *transB,
  int *m, int *n, int *k,
  cuDoubleComplex const* alpha, cuDoubleComplex const* A, int *lda,
  cuDoubleComplex const* B, int *ldb,
  cuDoubleComplex const* beta, cuDoubleComplex* C, int *ldc);

#endif
