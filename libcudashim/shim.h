#ifndef SHIM_H
#define SHIM_H

#include <cuda_runtime.h>
#include <cublas_v2.h>

#ifdef __cplusplus
extern "C" {
#endif

void dgemm_(
  char *transA, char *transB,
  int *m, int *n, int *k,
  double * alpha, double * A, int *lda,
  double * B, int *ldb,
  double * beta, double* C, int *ldc);

void sgemm_(
  char *transA, char *transB,
  int *m, int *n, int *k,
  float * alpha, float * A, int *lda,
  float * B, int *ldb,
  float * beta, float* C, int *ldc);

void zgemm_(
  char *transA, char *transB,
  int *m, int *n, int *k,
  cuDoubleComplex * alpha, cuDoubleComplex * A, int *lda,
  cuDoubleComplex * B, int *ldb,
  cuDoubleComplex * beta, cuDoubleComplex* C, int *ldc);

#ifdef __cplusplus
}
#endif

#endif
