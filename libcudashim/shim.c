#include <cuda_runtime.h>
#include <cublas_v2.h>

#include <stdio.h>
#include <stdlib.h>

typedef struct {
  double *real;
  double *imag;
} d_complex;

void dgemm_(
  char *transA, char *transB,
  int *m, int *n, int *k,
  double const* alpha, double const* A, int *lda,
  double const* B, int *ldb,
  double const* beta, double const* C, int *ldc)
{
}

void zgemm_(
  char *transA, char *transB,
  int *m, int *n, int *k,
  d_complex const* alpha, d_complex const* A, int *lda,
  d_complex const* B, int *ldb,
  d_complex const* beta, d_complex const* C, int *ldc)
{
}

/*

#define ERR(m) \
  do { \
    printf(m "\n"); \
    exit(EXIT_FAILURE); \
  } while(0)

int main(void)
{
  cudaError_t cudaStat;
  cublasStatus_t stat;
  cublasHandle_t handle;

  stat = cublasCreate(&handle);
  if(stat != CUBLAS_STATUS_SUCCESS) { ERR("cuBLAS init"); }

  int m = 2;
  int n = 1;
  int k = 3;
  float alpha = 1.0;
  float beta = 0;
  int lda = m;
  int ldb = k;
  int ldc = m;

  // Allocate and copy

  float *dev_a = 0;
  float *a = malloc(sizeof(*a) * lda * k);
  if(!a) { ERR("Alloc: A"); }
  for(int i = 0; i < lda * k; ++i) {
    a[i] = i + 1;
  }
  cudaStat = cudaMalloc((void **)&dev_a, lda * k * sizeof(*a));
  if(cudaStat != cudaSuccess) { ERR("Dev alloc: A");  }

  float *dev_b = 0;
  float *b = malloc(sizeof(*b) * ldb * n);
  if(!b) { ERR("Alloc: B"); }
  for(int i = 0; i < ldb * n; ++i) {
    b[i] = i + 1;
  }
  cudaStat = cudaMalloc((void **)&dev_b, ldb * n * sizeof(*b));
  if(cudaStat != cudaSuccess) { ERR("Dev alloc: B"); }

  float *dev_c = 0;
  float *c = malloc(sizeof(*c) * ldc * n);
  if(!c) { return EXIT_FAILURE; }
  for(int i = 0; i < ldc * n; ++i) {
    c[i] = 0;
  }
  cudaStat = cudaMalloc((void **)&dev_c, ldc * n * sizeof(*c));
  if(cudaStat != cudaSuccess) { ERR("Dev alloc: C"); }

  stat = cublasSetMatrix(m, k, sizeof(*a), a, m, dev_a, m);
  if(stat != CUBLAS_STATUS_SUCCESS) { ERR("Copy: A"); }

  stat = cublasSetMatrix(k, n, sizeof(*b), b, k, dev_b, k);
  if(stat != CUBLAS_STATUS_SUCCESS) { ERR("Copy: B"); }

  stat = cublasSetMatrix(m, n, sizeof(*c), c, m, dev_c, m);
  if(stat != CUBLAS_STATUS_SUCCESS) { ERR("Copy: C"); }

  // Do the work
  stat = cublasSgemm(
    handle, CUBLAS_OP_N, CUBLAS_OP_N, 
    m, n, k, 
    &alpha, dev_a, lda, 
    dev_b, ldb, &beta, 
    dev_c, ldc);
  if(stat != CUBLAS_STATUS_SUCCESS) { ERR("GEMM"); }
  
  // Copy back
  stat = cublasGetMatrix(m, n, sizeof(*c), dev_c, m, c, m);
  if(stat != CUBLAS_STATUS_SUCCESS) { printf("%d\n", stat); ERR("Copy back: C"); }

  for(int i = 0; i < ldc * n; ++i) {
    printf("%f\n", c[i]);
  }

  return 0;
}

*/
