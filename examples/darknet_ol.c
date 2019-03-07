#include <float.h>
#include <math.h>
#include "darknet.h"

void gemm_nn(int M, int N, int K, float ALPHA, float *A, int lda, float *B,
             int ldb, float *C, int ldc) {
  int i, j, k;
  for (i = 0; i < M; ++i) {
    for (k = 0; k < K; ++k) {
      register float A_PART = ALPHA * A[i * lda + k];
      for (j = 0; j < N; ++j) {
        C[i * ldc + j] += A_PART * B[k * ldb + j];
      }
    }
  }
}

void ol_gemm(int TA, int TB, int M, int N, int K, float ALPHA, float *A,
             int lda, float *B, int ldb, float BETA, float *C, int ldc) {
  int i, j;
  for (i = 0; i < M; ++i) {
    for (j = 0; j < N; ++j) {
      C[i * ldc + j] *= BETA;
    }
  }
  gemm_nn(M, N, K, ALPHA, A, lda, B, ldb, C, ldc);
}

void ol_l2_cpu(int n, float *pred, float *truth, float *delta, float *error) {
  int i;
  for (i = 0; i < n; ++i) {
    float diff = truth[i] - pred[i];
    error[i] = diff * diff;
    delta[i] = diff;
  }
}

float ol_dot_cpu(int N, float *X, int INCX, float *Y, int INCY) {
  int i;
  float dot = 0;
  for (i = 0; i < N; ++i) dot += X[i * INCX] * Y[i * INCY];
  return dot;
}

void ol_softmax(float *input, int n, float temp, int stride, float *output) {
  int i;
  float sum = 0;
  float largest = -FLT_MAX;
  for (i = 0; i < n; ++i) {
    if (input[i * stride] > largest) largest = input[i * stride];
  }
  for (i = 0; i < n; ++i) {
    float e = exp(input[i * stride] / temp - largest / temp);
    sum += e;
    output[i * stride] = e;
  }
  for (i = 0; i < n; ++i) {
    output[i * stride] /= sum;
  }
}

void forward_convolutional_layer(convolutional_layer l, network net) {
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
      ol_gemm(0, 0, m, n, k, 1, a, k, b, n, 1, c, n);
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
