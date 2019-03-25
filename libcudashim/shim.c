#include <pager/pager.h>

#include <cuda_runtime.h>
#include <cublas_v2.h>

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

static void cuda_deleter(void *ptr)
{
  (void)cudaFree(ptr);
}

static void cuda_copier(void *d, void *h, size_t s)
{
  (void)cudaMemcpy(d, h, s, cudaMemcpyHostToDevice);
}

static page_functions_st cuda_fns = {
  .deleter = cuda_deleter,
  .copier = cuda_copier
};

#define ERR(m) \
  do { \
    printf(m "\n"); \
    exit(EXIT_FAILURE); \
  } while(0)

#define ERRC(m, c) \
  do { \
    printf(m " [%d]\n", c); \
    exit(EXIT_FAILURE); \
  } while(0)

static cublasHandle_t get_handle()
{
  static bool setup = false;
  static cublasHandle_t handle;
  if(!setup) {
    cublasStatus_t stat = cublasCreate(&handle);
    if(stat != CUBLAS_STATUS_SUCCESS) {
      ERRC("Handle creation", stat);
    }
    setup = true;
  }
  return handle;
}

static cublasOperation_t str_to_op(char const* str)
{
  if(*str == 'N' || *str == 'n') {
    return CUBLAS_OP_N;
  } else if(*str == 'T' || *str == 't') {
    return CUBLAS_OP_T;
  } else if(*str == 'C' || *str == 'c') {
    return CUBLAS_OP_C;
  }
}

#define ALLOC_AND_COPY(T, rows, cols, host)                         \
  T *dev##host = 0;                                                 \
  do {                                                              \
    int rows_ = (rows);                                             \
    int cols_= (cols);                                              \
    T const* host_ = (host);                                        \
                                                                    \
    void *retr##host = get_table_entry((void*)host_);               \
    if(!retr##host) {                                               \
      cudaError_t stat##host = cudaMalloc(                          \
          (void **)&dev##host, rows_ * cols_ * sizeof(T));          \
                                                                    \
      if(stat##host != cudaSuccess) {                               \
        ERRC("Alloc " #host, stat##host);                           \
      }                                                             \
                                                                    \
      cublasStatus_t blas_stat##host = cublasSetMatrix(             \
        rows_, cols_, sizeof(T), host_, rows_, dev##host, rows_);   \
                                                                    \
      if(blas_stat##host != CUBLAS_STATUS_SUCCESS) {                \
        ERR("Copy " #host);                                         \
      }                                                             \
      new_table_entry((void *)host_, (void *)dev##host,             \
                      rows * cols * sizeof(T), cuda_fns);           \
    } else {                                                        \
      dev##host = retr##host;                                       \
    }                                                               \
  } while(0);                                                       \
  if(!dev##host) { ERR("Badly wrong"); }

void dgemm_(
  char *transA, char *transB,
  int *m, int *n, int *k,
  double const* alpha, double const* A, int *lda,
  double const* B, int *ldb,
  double const* beta, double* C, int *ldc)
{
  ALLOC_AND_COPY(double, *m, *k, A);
  ALLOC_AND_COPY(double, *k, *n, B);
  ALLOC_AND_COPY(double, *m, *n, C);

  cublasStatus_t stat = cublasDgemm(
    get_handle(), str_to_op(transA), str_to_op(transB), 
    *m, *n, *k,
    alpha, devA, *lda, 
    devB, *ldb, beta, 
    devC, *ldc);
  if(stat != CUBLAS_STATUS_SUCCESS) { 
    ERRC("DGEMM", stat);
  }

  stat = cublasGetMatrix(*m, *n, sizeof(*C), devC, *m, C, *m);
  if(stat != CUBLAS_STATUS_SUCCESS) { 
    ERRC("DGEMM copy back", stat);
  }
}

void zgemm_(
  char *transA, char *transB,
  int *m, int *n, int *k,
  cuDoubleComplex const* alpha, cuDoubleComplex const* A, int *lda,
  cuDoubleComplex const* B, int *ldb,
  cuDoubleComplex const* beta, cuDoubleComplex* C, int *ldc)
{
  ALLOC_AND_COPY(cuDoubleComplex, *m, *k, A);
  ALLOC_AND_COPY(cuDoubleComplex, *k, *n, B);
  ALLOC_AND_COPY(cuDoubleComplex, *m, *n, C);

  cublasStatus_t stat = cublasZgemm(
    get_handle(), str_to_op(transA), str_to_op(transB), 
    *m, *n, *k,
    alpha, devA, *lda, 
    devB, *ldb, beta, 
    devC, *ldc);
  if(stat != CUBLAS_STATUS_SUCCESS) { 
    ERRC("ZGEMM", stat);
  }

  cudaError_t err = cudaMemcpy(C, devC, *m * *n * sizeof(*C), cudaMemcpyDeviceToHost);
  if(err != cudaSuccess) {
    ERRC("ZGEMM copy back", err);
  }

  stat = cublasGetMatrix(*m, *n, sizeof(*C), devC, *m, C, *m);
  if(stat != CUBLAS_STATUS_SUCCESS) { 
    ERRC("ZGEMM copy back", stat);
  }
}
