extern "C" {

void blast_sgemm_(
  char const* transA, char const* transB,
  int m, int n, int k, float alpha,
  float *A, int lda,
  float *B, int ldb,
  float *C, int ldc
);

}
