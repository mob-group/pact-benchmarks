/***************************************************************************
 *cr
 *cr            (C) Copyright 2010 The Board of Trustees of the
 *cr                        University of Illinois
 *cr                         All Rights Reserved
 *cr
 ***************************************************************************/

/*
 * Main entry of dense matrix-matrix multiplication kernel
 */

#include <malloc.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <iostream>
#include <vector>
#include "parboil.h"

void basicSgemm(char transa, char transb, int m, int n, int k, float alpha,
                const float *A, int lda, const float *B, int ldb, float beta,
                float *C, int ldc) {
  if ((transa != 'N') && (transa != 'n')) {
    std::cerr << "unsupported value of 'transa' in regtileSgemm()" << std::endl;
    return;
  }

  if ((transb != 'T') && (transb != 't')) {
    std::cerr << "unsupported value of 'transb' in regtileSgemm()" << std::endl;
    return;
  }

  /* mm_harness(C, A, B, n, m, k); */
  for (int mm = 0; mm < m; ++mm) {
    for (int nn = 0; nn < n; ++nn) {
      float c = 0.0f;
      for (int i = 0; i < k; ++i) {
        float a = A[mm + i * lda];
        float b = B[nn + i * ldb];
        c += a * b;
      }
      C[mm + nn * ldc] = C[mm + nn * ldc] * beta + alpha * c;
    }
  }
}

// I/O routines
extern bool readColMajorMatrixFile(const char *fn, int &nr_row, int &nr_col,
                                   std::vector<float> &v);
extern bool writeColMajorMatrixFile(const char *fn, int, int,
                                    std::vector<float> &);

int parboil_sgemm(int argc, char *argv[]) {
  struct pb_Parameters *params;
  struct pb_TimerSet timers;

  int matArow, matAcol;
  int matBrow, matBcol;
  std::vector<float> matA, matBT;

  pb_InitializeTimerSet(&timers);

  /* Read command line. Expect 3 inputs: A, B and B^T
     in column-major layout*/
  params = pb_ReadParameters(&argc, argv);
  if ((params->inpFiles[0] == NULL) || (params->inpFiles[1] == NULL) ||
      (params->inpFiles[2] == NULL) || (params->inpFiles[3] != NULL)) {
    fprintf(stderr, "Expecting three input filenames\n");
    exit(-1);
  }

  /* Read in data */
  pb_SwitchToTimer(&timers, pb_TimerID_IO);

  // load A
  readColMajorMatrixFile(params->inpFiles[0], matArow, matAcol, matA);

  // load B^T
  readColMajorMatrixFile(params->inpFiles[2], matBcol, matBrow, matBT);

  pb_SwitchToTimer(&timers, pb_TimerID_COMPUTE);

  // allocate space for C
  std::vector<float> matC(matArow * matBcol);

  float *matA_data = (float *)malloc(sizeof(float) * matA.size());
  memcpy(matA_data, matA.data(), sizeof(float) * matA.size());

  float *matBT_data = (float *)malloc(sizeof(float) * matBT.size());
  memcpy(matBT_data, matBT.data(), sizeof(float) * matBT.size());

  float *matC_data = (float *)malloc(sizeof(float) * matC.size());
  memcpy(matC_data, matC.data(), sizeof(float) * matC.size());

  // Use standard sgemm interface
  basicSgemm('N', 'T', matArow, matBcol, matAcol, 1.0f, matA_data, matArow,
             matBT_data, matBcol, 0.0f, matC_data, matArow);

  memcpy(matC.data(), matC_data, sizeof(float) * matC.size());

  if (params->outFile) {
    /* Write C to file */
    pb_SwitchToTimer(&timers, pb_TimerID_IO);
    writeColMajorMatrixFile(params->outFile, matArow, matBcol, matC);
  }

  pb_SwitchToTimer(&timers, pb_TimerID_NONE);

  double CPUtime = pb_GetElapsedTime(&(timers.timers[pb_TimerID_COMPUTE]));
  std::cout << "GFLOPs = " << 2. * matArow * matBcol * matAcol / CPUtime / 1e9
            << std::endl;
  pb_PrintTimerSet(&timers);
  pb_FreeParameters(params);
  return 0;
}
