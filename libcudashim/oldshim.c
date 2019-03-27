#include <cuda_runtime.h>
#include <cublas_v2.h>
#include <cublasXt.h>

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

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

static void * pointer_map[8] = { 0 };

void store_pointer(void* host, void* dev)
{
  static int next_index = 0;

  for(int i = 0; i < 4; ++i) {
    void * ex_host = pointer_map[i*2];
    if(ex_host == host) {
      cudaFree(pointer_map[i*2+1]);

      pointer_map[i*2] = host;
      pointer_map[i*2 + 1] = dev;
      return;
    } else if(!ex_host) {
      pointer_map[i*2] = host;
      pointer_map[i*2 + 1] = dev;
      return;
    }
  }

  cudaFree(pointer_map[next_index * 2 + 1]);

  pointer_map[next_index * 2] = host;
  pointer_map[next_index * 2 + 1] = dev;

  next_index = ++next_index % 4;
}

void* retrieve_pointer(void* host)
{
  for(int i = 0; i < 4; ++i) {
    void * ex_host = pointer_map[i*2];
    if(ex_host == host) {
      return pointer_map[i*2 + 1];
    }
  }

  return 0;
}

void delete_pointer(void *host)
{
  for(int i = 0; i < 4; ++i) {
    void * ex_host = pointer_map[i*2];
    if(ex_host == host) {
      cudaFree(pointer_map[i*2 + 1]);

      pointer_map[i*2] = NULL;
      pointer_map[i*2 + 1] = NULL;
    }
  }
}

#define ALLOC_AND_COPY(T, rows, cols, host)                         \
  do {                                                              \
    int rows_ = (rows);                                             \
    int cols_= (cols);                                              \
    T const* host_ = (host);                                        \
                                                                    \
    void *retr##host = retrieve_pointer((void *)host_);             \
    if(!retr##host) {                                               \
      cudaError_t stat##host = cudaMalloc(                          \
          (void **)&dev##host, rows_ * cols_ * sizeof(T));          \
      printf("%p %d\n", dev##host, rows_ * cols_ * sizeof(T)); \
                                                                    \
      if(stat##host != cudaSuccess) {                               \
        ERRC("Alloc " #host, stat##host);                           \
      }                                                             \
                                                                    \
      stat##host = cudaMemcpy(dev##host, host_, rows_ * cols_ * sizeof(T), cudaMemcpyHostToDevice); \
                                                                    \
      if(stat##host != cudaSuccess) {                \
        ERRC("Copy " #host, stat##host);                       \
      }                                                             \
      store_pointer((void *)host_, (void *)dev##host);              \
    } else {                                                        \
      dev##host = retr##host;                                       \
    }                                                               \
  } while(0);                                                       \
  if(!dev##host) { ERR("Badly wrong"); }

void sgemm_(
  char *transA, char *transB,
  int *m, int *n, int *k,
  float const* alpha, float const* A, int *lda,
  float const* B, int *ldb,
  float const* beta, float* C, int *ldc)
{
  static float* dev_buff = NULL;
  static int alloced = 0;

  int total_size = ((*m * *k) + (*k * *n) + (*m * *n)) * sizeof(float);

  cudaError_t stat;

  if(alloced < total_size) {
    stat = cudaMalloc((void **)&dev_buff, total_size);
    if(stat != cudaSuccess) { ERRC("Alloc", stat); }

    alloced = total_size;
  }

  float *devA = dev_buff;
  stat = cudaMemcpy(devA, A, *m * *k * sizeof(float), cudaMemcpyHostToDevice);
  if(stat != cudaSuccess) { ERRC("Copy A", stat); }

  float *devB = dev_buff + (*m * *k);
  stat = cudaMemcpy(devB, B, *k * *n * sizeof(float), cudaMemcpyHostToDevice);
  if(stat != cudaSuccess) { ERRC("Copy B", stat); }

  float *devC = dev_buff + (*m * *k) + (*k * *n);
  stat = cudaMemcpy(devC, C, *m * *n * sizeof(float), cudaMemcpyHostToDevice);
  if(stat != cudaSuccess) { ERRC("Copy C", stat); }

  cublasStatus_t blas_stat = cublasSgemm(
    get_handle(), CUBLAS_OP_N, CUBLAS_OP_N,
    *n, *m, *k,
    alpha, devB, *ldb,
    devA, *lda, beta, 
    devC, *ldc);
  if(blas_stat != CUBLAS_STATUS_SUCCESS) { 
    ERRC("SGEMM", blas_stat);
  }

  stat = cudaMemcpy(C, devC, *m * *n * sizeof(float), cudaMemcpyDeviceToHost);
  if(stat != cudaSuccess) { ERRC("Copy C back", stat); }
}

void dgemm_(
  char *transA, char *transB,
  int *m, int *n, int *k,
  double const* alpha, double const* A, int *lda,
  double const* B, int *ldb,
  double const* beta, double* C, int *ldc)
{
  double *devA, *devB, *devC = NULL;
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
  cuDoubleComplex *devA, *devB, *devC = NULL;

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
