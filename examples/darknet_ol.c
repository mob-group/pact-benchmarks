#include <math.h>
#include "darknet.h"

typedef layer convolutional_layer;

void binarize_weights(float *weights, int n, int size, float *binary) {
  int i, f;
  for (f = 0; f < n; ++f) {
    float mean = 0;
    for (i = 0; i < size; ++i) {
      mean += fabs(weights[f * size + i]);
    }
    mean = mean / size;
    for (i = 0; i < size; ++i) {
      binary[f * size + i] = (weights[f * size + i] > 0) ? mean : -mean;
    }
  }
}

void swap_binary(convolutional_layer *l) {
  float *swap = l->weights;
  l->weights = l->binary_weights;
  l->binary_weights = swap;
}

void binarize_cpu(float *input, int n, float *binary) {
  int i;
  for (i = 0; i < n; ++i) {
    binary[i] = (input[i] > 0) ? 1 : -1;
  }
}

float im2col_get_pixel(float *im, int height, int width, int channels, int row,
                       int col, int channel, int pad) {
  row -= pad;
  col -= pad;

  if (row < 0 || col < 0 || row >= height || col >= width) return 0;
  return im[col + width * (row + height * channel)];
}

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

void gemm(int TA, int TB, int M, int N, int K, float ALPHA, float *A, int lda,
          float *B, int ldb, float BETA, float *C, int ldc) {
  int i, j;
  for (i = 0; i < M; ++i) {
    for (j = 0; j < N; ++j) {
      C[i * ldc + j] *= BETA;
    }
  }
  gemm_nn(M, N, K, ALPHA, A, lda, B, ldb, C, ldc);
}

// From Berkeley Vision's Caffe!
// https://github.com/BVLC/caffe/blob/master/LICENSE
void im2col_cpu(float *data_im, int channels, int height, int width, int ksize,
                int stride, int pad, float *data_col) {
  int c, h, w;
  int height_col = (height + 2 * pad - ksize) / stride + 1;
  int width_col = (width + 2 * pad - ksize) / stride + 1;

  int channels_col = channels * ksize * ksize;
  for (c = 0; c < channels_col; ++c) {
    int w_offset = c % ksize;
    int h_offset = (c / ksize) % ksize;
    int c_im = c / ksize / ksize;
    for (h = 0; h < height_col; ++h) {
      for (w = 0; w < width_col; ++w) {
        int im_row = h_offset + h * stride;
        int im_col = w_offset + w * stride;
        int col_index = (c * height_col + h) * width_col + w;
        data_col[col_index] = im2col_get_pixel(data_im, height, width, channels,
                                               im_row, im_col, c_im, pad);
      }
    }
  }
}

void forward_batchnorm_layer(layer l, network net);
void add_bias(float *output, float *biases, int batch, int n, int size);
void activate_array(float *x, const int n, const ACTIVATION a);

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
      gemm(0, 0, m, n, k, 1, a, k, b, n, 1, c, n);
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
