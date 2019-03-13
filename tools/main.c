#include "mkl_shim.h"

#include <stdio.h>
#include <stdlib.h>

int main() {
  INT m = 2;
  INT n = 1;
  INT k = 3;

  REAL alpha = 1;
  REAL beta = 0;

  INT lda = m;
  INT ldb = k;
  INT ldc = m;

  REAL *a = malloc(sizeof(*a) * lda * k);
  for (int i = 0; i < lda * k; ++i) {
    a[i] = i + 1;
  }

  REAL *b = malloc(sizeof(*b) * ldb * n);
  for (int i = 0; i < ldb * n; ++i) {
    b[i] = i + 1;
  }

  REAL *c = malloc(sizeof(*c) * ldc * n);
  for (int i = 0; i < ldc * n; ++i) {
    c[i] = 0;
  }

  char trans = 'N';
  dgemm_shim_(&trans, &trans, &m, &n, &k, &alpha, a, &lda, b, &ldb, &beta, c,
              &ldc);

  for (int i = 0; i < ldc * n; ++i) {
    printf("%f\n", c[i]);
  }

  free(a);
  free(b);
  free(c);
}
