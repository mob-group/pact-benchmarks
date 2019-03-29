#ifdef __cplusplus
extern "C" {
#endif

void blast_sgemm_(
  int m, int n, int k, float alpha,
  float *A, int lda,
  float *B, int ldb,
  float *C, int ldc
);

#ifdef __cplusplus
}
#endif
