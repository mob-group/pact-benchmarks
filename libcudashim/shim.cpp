#include "harness.hpp"
#include "shim.h"

#include <cuda_runtime.h>
#include <cublas_v2.h>

#include <type_traits>

namespace {
template<typename type_in, typename type_out>
void CudaRead_update(type_in* in, int size, type_out& out) {
    cudaMemcpy(out, in, sizeof(type_in)*size,
               cudaMemcpyHostToDevice);
}

template<typename type_in, typename type_out>
void CudaRead_construct(int size, type_out& out) {
    cudaMalloc(&out, sizeof(type_in)*size);
}

template<typename type_in, typename type_out>
void CudaRead_destruct(int size, type_out& out) {
    cudaFree(out);
}

template<typename type_in, typename type_out>
using CudaRead = ReadObject<type_in, type_out,
    CudaRead_update<type_in,type_out>,
    CudaRead_construct<type_in,type_out>,
    CudaRead_destruct<type_in,type_out>>;

template<typename type_in, typename type_out>
void CudaWrite_update(type_in* in, int size, type_out& out) {
    cudaMemcpy(in, out, sizeof(type_in)*size,
               cudaMemcpyDeviceToHost);
}

template<typename type_in, typename type_out>
void CudaWrite_construct(int size, type_out& out) {
    cudaMalloc(&out, sizeof(type_in)*size);
}

template<typename type_in, typename type_out>
void CudaWrite_destruct(int size, type_out& out) {
    cudaFree(out);
}

template<typename type_in, typename type_out>
using CudaWrite = WriteObject<type_in, type_out,
    CudaWrite_update<type_in,type_out>,
    CudaWrite_construct<type_in,type_out>,
    CudaWrite_destruct<type_in,type_out>>;

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

template <typename T>
struct Functor
{
    Functor() {
        cublasCreate(&handle);
    }

    ~Functor() {
        cublasDestroy(handle);
    }

    cublasHandle_t handle;
    CudaRead<T,T*> shadow_d_A;
    CudaRead<T,T*> shadow_d_B;
    CudaWrite<T,T*> shadow_d_C;

    void operator()(char const* transA, char const* transB, 
										int m, int n, int k, 
										T* alpha, T* A, int lda, 
                    T* B, int ldb, 
										T* beta, T* C, int ldc);
};

template <>
void Functor<double>::operator()(
  char const* transA, char const* transB, 
  int m, int n, int k, 
  double* alpha, double* A, int lda, 
  double* B, int ldb, 
  double* beta, double* C, int ldc)
{
  auto d_A = shadow_d_A(A, m*k);
  auto d_B = shadow_d_B(B, k*n);
  auto d_C = shadow_d_C(C, m*n);

  cublasDgemm_v2(handle, str_to_op(transA), str_to_op(transB),
                 m, n, k, alpha, d_A, lda, d_B, ldb, beta, d_C, ldc);
}

template <>
void Functor<cuDoubleComplex>::operator()(
  char const* transA, char const* transB, 
  int m, int n, int k, 
  cuDoubleComplex* alpha, cuDoubleComplex* A, int lda, 
  cuDoubleComplex* B, int ldb, 
  cuDoubleComplex* beta, cuDoubleComplex* C, int ldc)
{
  auto d_A = shadow_d_A(A, m*k);
  auto d_B = shadow_d_B(B, k*n);
  auto d_C = shadow_d_C(C, m*n);

  cublasZgemm_v2(handle, str_to_op(transA), str_to_op(transB),
                 m, n, k, alpha, d_A, lda, d_B, ldb, beta, d_C, ldc);
}

}

extern "C" {

void dgemm_(
  char *transA, char *transB,
  int *m, int *n, int *k,
  double * alpha, double * A, int *lda,
  double * B, int *ldb,
  double * beta, double* C, int *ldc)
{
  static Functor<double> functor;
  return functor(
    transA, transB, *m, *n, *k, 
    alpha, A, *lda,
    B, *ldb,
    beta, C, *ldc);
}

void zgemm_(
  char *transA, char *transB,
  int *m, int *n, int *k,
  cuDoubleComplex * alpha, cuDoubleComplex * A, int *lda,
  cuDoubleComplex * B, int *ldb,
  cuDoubleComplex * beta, cuDoubleComplex* C, int *ldc)
{
  static Functor<cuDoubleComplex> functor;
  return functor(
    transA, transB, *m, *n, *k, 
    alpha, A, *lda,
    B, *ldb,
    beta, C, *ldc);
}

}
