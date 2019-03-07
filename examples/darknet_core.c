#include <math.h>
#include "darknet.h"

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

