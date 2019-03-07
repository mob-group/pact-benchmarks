#include <float.h>
#include <math.h>
#include "darknet.h"

typedef void *cublasHandle_t;
cublasHandle_t blas_handle();
typedef int cudaError_t;
typedef cudaError_t cublasStatus_t;
typedef int cublasOperation_t;
void check_error(cublasStatus_t);
cublasOperation_t CUBLAS_OP_N = 'N';
cublasOperation_t CUBLAS_OP_T = 'T';

cublasStatus_t cublasSgemm(cublasHandle_t handle, cublasOperation_t transa,
                           cublasOperation_t transb, int m, int n, int k,
                           const float *alpha, const float *A, int lda,
                           const float *B, int ldb, const float *beta, float *C,
                           int ldc);

cublasStatus_t cublasSdot(cublasHandle_t handle, int n, const float *x,
                          int incx, const float *y, int incy, float *result);

typedef int cudnnStatus_t;
typedef void *cudnnHandle_t;
typedef int cudnnSoftmaxAlgorithm_t;
typedef int cudnnSoftmaxMode_t;
typedef void *cudnnTensorDescriptor_t;

cudnnStatus_t cudnnSoftmaxForward(cudnnHandle_t handle,
                                  cudnnSoftmaxAlgorithm_t algorithm,
                                  cudnnSoftmaxMode_t mode, const void *alpha,
                                  const cudnnTensorDescriptor_t xDesc,
                                  const void *x, const void *beta,
                                  const cudnnTensorDescriptor_t yDesc, void *y);

void ll_gemm(int TA, int TB, int M, int N, int K, float ALPHA, float *A_gpu,
             int lda, float *B_gpu, int ldb, float BETA, float *C_gpu,
             int ldc) {
  cublasHandle_t handle = blas_handle();
  cudaError_t status =
      cublasSgemm(handle, (TB ? CUBLAS_OP_T : CUBLAS_OP_N),
                  (TA ? CUBLAS_OP_T : CUBLAS_OP_N), N, M, K, &ALPHA, B_gpu, ldb,
                  A_gpu, lda, &BETA, C_gpu, ldc);
  check_error(status);
}

void ll_forward_convolutional_layer(convolutional_layer l, network net) {
  int i, j;

  fill_cpu(l.outputs * l.batch, 0, l.output, 1);

  if (l.xnor) {
    binarize_weights(l.weights, l.n, l.c / l.groups * l.size * l.size,
                     l.binary_weights);
    swap_binary(&l);
    binarize_cpu(net.input, l.c * l.h * l.w * l.batch, l.binary_input);
    net.input = l.binary_input;
  }

  int m = l.n / l.groups;
  int k = l.size * l.size * l.c / l.groups;
  int n = l.out_w * l.out_h;
  for (i = 0; i < l.batch; ++i) {
    for (j = 0; j < l.groups; ++j) {
      float *a = l.weights + j * l.nweights / l.groups;
      float *b = net.workspace;
      float *c = l.output + (i * l.groups + j) * n * m;
      float *im = net.input + (i * l.groups + j) * l.c / l.groups * l.h * l.w;

      if (l.size == 1) {
        b = im;
      } else {
        im2col_cpu(im, l.c / l.groups, l.h, l.w, l.size, l.stride, l.pad, b);
      }
      ll_gemm(0, 0, m, n, k, 1, a, k, b, n, 1, c, n);
    }
  }

  if (l.batch_normalize) {
    forward_batchnorm_layer(l, net);
  } else {
    add_bias(l.output, l.biases, l.batch, l.n, l.out_h * l.out_w);
  }

  activate_array(l.output, l.outputs * l.batch, l.activation);
  if (l.binary || l.xnor) swap_binary(&l);
}
