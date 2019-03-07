#define NTAPS 256
#define NPOINTS 64

#define A_ROW 4
#define A_COL 4
#define B_ROW 4
#define B_COL 4

void fir(float input[], float output[], float coefficient[]) {
  int i;
  float sum;
  sum = 0.0;

  for (i = 0; i < NTAPS; ++i) {
    sum += input[i] * coefficient[i];
  }
  output[0] = sum;
}

void fir_2(float input[], float output[], float coefficient[]) {
  int i;
  int j;

  float sum;
  for (i = 0; i < 1; i++) {
    sum = 0.0;
    for (j = 0; j < NTAPS; j++) {
      sum += input[i - j] * coefficient[j];
    }
    output[i] = sum;
  }
}

void fir_3(float *input, float *output, float *coefficient) {
  int i;
  float sum;
  float term1;
  float term2;

  sum = 0.0;
  for (i = 0; i < NTAPS; i++) {
    term1 = *input++;
    term2 = *coefficient++;
    sum += term1 * term2;
  }
  *output++ = sum;
}

void fir_4(float *input, float *output, float *coefficient) {
  int i;
  int j;
  float *data;
  float *coef;
  float sum;
  float term1;
  float term2;

  for (i = 0; i < NPOINTS; i++) {
    data = input++;
    coef = coefficient;
    sum = 0.0;

    for (j = 0; j < NTAPS; j++) {
      term1 = *data--;
      term2 = *coef++;
      sum += term1 * term2;
    }
    *output++ = sum;
  }
}

void mult_1(float a_matrix[A_ROW][A_COL], float b_matrix[B_ROW][B_COL],
            float c_matrix[A_ROW][B_COL]) {
  int i, j, k;

  float sum;

  for (i = 0; i < A_ROW; i++) {
    for (j = 0; j < B_COL; j++) {
      sum = 0.0;
      for (k = 0; k < B_ROW; ++k) {
        sum += a_matrix[i][k] * b_matrix[k][j];
      }
      c_matrix[i][j] = sum;
    }
  }
}

void mult_2(float *a_matrix, float *b_matrix, float *c_matrix) {
  int i, j, k;
  float *A, *B;
  float *a, *b;
  float sum;
  float term_a;
  float term_b;
  A = a_matrix;
  B = b_matrix;
  for (i = 0; i < A_ROW; i++) {
    for (j = 0; j < B_COL; j++) {
      a = A;
      b = B;
      sum = 0.0;
      for (k = 0; k < B_ROW; k++) {
        term_a = *a;
        term_b = *b;
        a++;
        b += B_COL;
        sum += term_a * term_b;
      }
      *c_matrix++ = sum;
      B++;
    }
    A += A_COL;
    B = b_matrix;
  }
}
