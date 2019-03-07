#define STORAGE_CLASS
#define TYPE int
#define LENGTH 16

#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
TYPE fir_5(TYPE *px, TYPE *px2, TYPE *ph, TYPE x0) {
  TYPE y = 0;
  STORAGE_CLASS TYPE i;
  for (i = 0; i < LENGTH - 1; i++) {
    y += *ph-- * *px;
    *px-- = *px2--;
  }

  y += *ph * *px;
  *px = x0;

  return (y);
}
#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
void dot_product(TYPE *p_z, TYPE *p_a, TYPE *p_b) {
  TYPE f;
  for (f = 0; f < 2; f++) *p_z += *p_a++ * *p_b++;
}
#ifdef __cplusplus
}
#endif

#define X 10 /* first dimension of array A */
#define Y 10 /* second dimension of array A, first dimension of array B */
#define Z 10 /* second dimension of array B */

#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
void mult_3(TYPE *p_a, TYPE *p_b, TYPE *p_c) {
  STORAGE_CLASS TYPE f, i, k;
  TYPE *A, *B;
  A = p_a;
  B = p_b;

  for (k = 0; k < Z; k++) {
    p_a = A; /* point to the beginning of array A */

    for (i = 0; i < X; i++) {
      p_b = B + (k * Y); /* take next column */

      *p_c = 0;

      for (f = 0; f < Y; f++) /* do multiply */
        *p_c += *p_a++ * *p_b++;

      p_c++;
    }
  }
}
#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
TYPE convolution(TYPE *px, TYPE *ph) {
  TYPE y = 0;
  TYPE i;

  for (i = 0; i < LENGTH; ++i) y += *px++ * *ph--;

  return y;
}
#ifdef __cplusplus
}
#endif
