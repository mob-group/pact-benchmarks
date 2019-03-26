#include "shim.h"

#include <stdio.h>
#include <stdlib.h>

int main() {
  int m = 2;
  int n = 1;
  int k = 3;

  double alpha = 1;
  double beta = 0;

  int lda = m;
  int ldb = k;
  int ldc = m;

  auto a = new double[lda * k];
  for (int i = 0; i < lda * k; ++i) {
    a[i] = i + 1;
  }

  auto b = new double[ldb * n];
  for (int i = 0; i < ldb * n; ++i) {
    b[i] = i + 1;
  }

  auto c = new double[ldc * n];
  for (int i = 0; i < ldc * n; ++i) {
    c[i] = 0;
  }

  char trans = 'N';
  dgemm_(&trans, &trans, &m, &n, &k, &alpha, a, &lda, b, &ldb, &beta, c, &ldc);

  for (int i = 0; i < ldc * n; ++i) {
    printf("%f\n", c[i]);
  }

  delete[] a;
  delete[] b;
  delete[] c;
}
