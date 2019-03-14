*> \brief \b AB_DCHKEE
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       PROGRAM AB_DCHKEE
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_DCHKEE tests the DOUBLE PRECISION LAPACK subroutines for the matrix
*> eigenvalue problem.  The test paths in this version are
*>
*> NEP (Nonsymmetric Eigenvalue Problem):
*>     Test AB_DGEHRD, AB_DORGHR, AB_DHSEQR, AB_DTREVC, AB_DHSEIN, and AB_DORMHR
*>
*> SEP (Symmetric Eigenvalue Problem):
*>     Test AB_DSYTRD, AB_DORGTR, AB_DSTEQR, AB_DSTERF, AB_DSTEIN, AB_DSTEDC,
*>     and drivers AB_DSYEV(X), AB_DSBEV(X), AB_DSPEV(X), AB_DSTEV(X),
*>                 AB_AB_DSYEVD,   AB_AB_DSBEVD,   AB_AB_DSPEVD,   AB_AB_DSTEVD
*>
*> SVD (Singular Value Decomposition):
*>     Test AB_DGEBRD, AB_DORGBR, AB_DBDSQR, AB_DBDSDC
*>     and the drivers AB_AB_DGESVD, AB_DGESDD
*>
*> DEV (Nonsymmetric Eigenvalue/eigenvector Driver):
*>     Test AB_DGEEV
*>
*> DES (Nonsymmetric Schur form Driver):
*>     Test AB_DGEES
*>
*> DVX (Nonsymmetric Eigenvalue/eigenvector Expert Driver):
*>     Test AB_AB_DGEEVX
*>
*> DSX (Nonsymmetric Schur form Expert Driver):
*>     Test AB_AB_DGEESX
*>
*> DGG (Generalized Nonsymmetric Eigenvalue Problem):
*>     Test AB_DGGHD3, AB_DGGBAL, AB_DGGBAK, AB_DHGEQZ, and AB_DTGEVC
*>
*> DGS (Generalized Nonsymmetric Schur form Driver):
*>     Test AB_DGGES
*>
*> DGV (Generalized Nonsymmetric Eigenvalue/eigenvector Driver):
*>     Test AB_DGGEV
*>
*> DGX (Generalized Nonsymmetric Schur form Expert Driver):
*>     Test AB_AB_DGGESX
*>
*> DXV (Generalized Nonsymmetric Eigenvalue/eigenvector Expert Driver):
*>     Test AB_AB_DGGEVX
*>
*> DSG (Symmetric Generalized Eigenvalue Problem):
*>     Test AB_DSYGST, AB_DSYGV, AB_AB_DSYGVD, AB_AB_DSYGVX, AB_DSPGST, AB_DSPGV, AB_AB_DSPGVD,
*>     AB_AB_DSPGVX, AB_DSBGST, AB_DSBGV, AB_AB_DSBGVD, and AB_AB_DSBGVX
*>
*> DSB (Symmetric Band Eigenvalue Problem):
*>     Test AB_DSBTRD
*>
*> DBB (Band Singular Value Decomposition):
*>     Test AB_DGBBRD
*>
*> DEC (Eigencondition estimation):
*>     Test AB_DLALN2, AB_DLASY2, DLAEQU, AB_DLAEXC, AB_DTRSYL, AB_DTREXC, AB_DTRSNA,
*>     AB_DTRSEN, and AB_DLAQTR
*>
*> DBL (Balancing a general matrix)
*>     Test AB_DGEBAL
*>
*> DBK (Back transformation on a balanced matrix)
*>     Test AB_DGEBAK
*>
*> DGL (Balancing a matrix pair)
*>     Test AB_DGGBAL
*>
*> DGK (Back transformation on a matrix pair)
*>     Test AB_DGGBAK
*>
*> GLM (Generalized Linear Regression Model):
*>     Tests AB_DGGGLM
*>
*> GQR (Generalized QR and RQ factorizations):
*>     Tests AB_DGGQRF and AB_DGGRQF
*>
*> GSV (Generalized Singular Value Decomposition):
*>     Tests AB_DGGSVD, AB_DGGSVP, AB_DTGSJA, AB_DLAGS2, AB_DLAPLL, and AB_DLAPMT
*>
*> CSD (CS decomposition):
*>     Tests DORCSD
*>
*> AB_LSE (Constrained Linear Least Squares):
*>     Tests AB_DGGAB_LSE
*>
*> Each test path has a different set of inputs, but the data sets for
*> the driver routines xEV, xES, xVX, and xSX can be concatenated in a
*> single input file.  The first line of input should contain one of the
*> 3-character path names in columns 1-3.  The number of remaining lines
*> depends on what is found on the first line.
*>
*> The number of matrix types used in testing is often controllable from
*> the input file.  The number of matrix types for each path, and the
*> test routine that describes them, is as follows:
*>
*> Path name(s)  Types    Test routine
*>
*> DHS or NEP      21     AB_DCHKHS
*> DST or SEP      21     AB_DCHKST (routines)
*>                 18     AB_DDRVST (drivers)
*> DBD or SVD      16     AB_DCHKBD (routines)
*>                  5     AB_DDRVBD (drivers)
*> DEV             21     AB_DDRVEV
*> DES             21     AB_DDRVES
*> DVX             21     AB_DDRVVX
*> DSX             21     AB_DDRVSX
*> DGG             26     AB_DCHKGG (routines)
*> DGS             26     AB_DDRGES
*> DGX              5     AB_DDRGSX
*> DGV             26     AB_DDRGEV
*> DXV              2     AB_DDRGVX
*> DSG             21     AB_DDRVSG
*> DSB             15     AB_DCHKSB
*> DBB             15     AB_DCHKBB
*> DEC              -     AB_AB_DCHKEC
*> DBL              -     AB_DCHKBL
*> DBK              -     AB_DCHKBK
*> DGL              -     AB_DCHKGL
*> DGK              -     AB_DCHKGK
*> GLM              8     AB_DCKGLM
*> GQR              8     AB_DCKGQR
*> GSV              8     AB_DCKGSV
*> CSD              3     AB_DCKCSD
*> AB_LSE              8     AB_DCKAB_LSE
*>
*>-----------------------------------------------------------------------
*>
*> NEP input file:
*>
*> line 2:  NN, INTEGER
*>          Number of values of N.
*>
*> line 3:  NVAL, INTEGER array, dimension (NN)
*>          The values for the matrix dimension N.
*>
*> line 4:  NPARMS, INTEGER
*>          Number of values of the parameters NB, NBMIN, NX, NS, and
*>          MAXB.
*>
*> line 5:  NBVAL, INTEGER array, dimension (NPARMS)
*>          The values for the blocksize NB.
*>
*> line 6:  NBMIN, INTEGER array, dimension (NPARMS)
*>          The values for the minimum blocksize NBMIN.
*>
*> line 7:  NXVAL, INTEGER array, dimension (NPARMS)
*>          The values for the crossover point NX.
*>
*> line 8:  INMIN, INTEGER array, dimension (NPARMS)
*>          LAHQR vs TTQRE crossover point, >= 11
*>
*> line 9:  INWIN, INTEGER array, dimension (NPARMS)
*>          recommended deflation window size
*>
*> line 10: INIBL, INTEGER array, dimension (NPARMS)
*>          nibble crossover point
*>
*> line 11: ISHFTS, INTEGER array, dimension (NPARMS)
*>          number of simultaneous shifts)
*>
*> line 12: IACC22, INTEGER array, dimension (NPARMS)
*>          select structured matrix multiply: 0, 1 or 2)
*>
*> line 13: THRESH
*>          Threshold value for the test ratios.  Information will be
*>          printed about each test for which the test ratio is greater
*>          than or equal to the threshold.  To have all of the test
*>          ratios printed, use THRESH = 0.0 .
*>
*> line 14: NEWSD, INTEGER
*>          A code indicating how to set the random number seed.
*>          = 0:  Set the seed to a default value before each run
*>          = 1:  Initialize the seed to a default value only before the
*>                first run
*>          = 2:  Like 1, but use the seed values on the next line
*>
*> If line 14 was 2:
*>
*> line 15: INTEGER array, dimension (4)
*>          Four integer values for the random number seed.
*>
*> lines 15-EOF:  The remaining lines occur in sets of 1 or 2 and allow
*>          the user to specify the matrix types.  Each line contains
*>          a 3-character path name in columns 1-3, and the number
*>          of matrix types must be the first nonblank item in columns
*>          4-80.  If the number of matrix types is at least 1 but is
*>          less than the maximum number of possible types, a AB_SECOND
*>          line will be read to get the numbers of the matrix types to
*>          be used.  For example,
*> NEP 21
*>          requests all of the matrix types for the nonsymmetric
*>          eigenvalue problem, while
*> NEP  4
*> 9 10 11 12
*>          requests only matrices of type 9, 10, 11, and 12.
*>
*>          The valid 3-character path names are 'NEP' or 'SHS' for the
*>          nonsymmetric eigenvalue routines.
*>
*>-----------------------------------------------------------------------
*>
*> SEP or DSG input file:
*>
*> line 2:  NN, INTEGER
*>          Number of values of N.
*>
*> line 3:  NVAL, INTEGER array, dimension (NN)
*>          The values for the matrix dimension N.
*>
*> line 4:  NPARMS, INTEGER
*>          Number of values of the parameters NB, NBMIN, and NX.
*>
*> line 5:  NBVAL, INTEGER array, dimension (NPARMS)
*>          The values for the blocksize NB.
*>
*> line 6:  NBMIN, INTEGER array, dimension (NPARMS)
*>          The values for the minimum blocksize NBMIN.
*>
*> line 7:  NXVAL, INTEGER array, dimension (NPARMS)
*>          The values for the crossover point NX.
*>
*> line 8:  THRESH
*>          Threshold value for the test ratios.  Information will be
*>          printed about each test for which the test ratio is greater
*>          than or equal to the threshold.
*>
*> line 9:  TSTCHK, LOGICAL
*>          Flag indicating whether or not to test the LAPACK routines.
*>
*> line 10: TSTDRV, LOGICAL
*>          Flag indicating whether or not to test the driver routines.
*>
*> line 11: TSTERR, LOGICAL
*>          Flag indicating whether or not to test the error exits for
*>          the LAPACK routines and driver routines.
*>
*> line 12: NEWSD, INTEGER
*>          A code indicating how to set the random number seed.
*>          = 0:  Set the seed to a default value before each run
*>          = 1:  Initialize the seed to a default value only before the
*>                first run
*>          = 2:  Like 1, but use the seed values on the next line
*>
*> If line 12 was 2:
*>
*> line 13: INTEGER array, dimension (4)
*>          Four integer values for the random number seed.
*>
*> lines 13-EOF:  Lines specifying matrix types, as for NEP.
*>          The 3-character path names are 'SEP' or 'SST' for the
*>          symmetric eigenvalue routines and driver routines, and
*>          'DSG' for the routines for the symmetric generalized
*>          eigenvalue problem.
*>
*>-----------------------------------------------------------------------
*>
*> SVD input file:
*>
*> line 2:  NN, INTEGER
*>          Number of values of M and N.
*>
*> line 3:  MVAL, INTEGER array, dimension (NN)
*>          The values for the matrix row dimension M.
*>
*> line 4:  NVAL, INTEGER array, dimension (NN)
*>          The values for the matrix column dimension N.
*>
*> line 5:  NPARMS, INTEGER
*>          Number of values of the parameter NB, NBMIN, NX, and NRHS.
*>
*> line 6:  NBVAL, INTEGER array, dimension (NPARMS)
*>          The values for the blocksize NB.
*>
*> line 7:  NBMIN, INTEGER array, dimension (NPARMS)
*>          The values for the minimum blocksize NBMIN.
*>
*> line 8:  NXVAL, INTEGER array, dimension (NPARMS)
*>          The values for the crossover point NX.
*>
*> line 9:  NSVAL, INTEGER array, dimension (NPARMS)
*>          The values for the number of right hand sides NRHS.
*>
*> line 10: THRESH
*>          Threshold value for the test ratios.  Information will be
*>          printed about each test for which the test ratio is greater
*>          than or equal to the threshold.
*>
*> line 11: TSTCHK, LOGICAL
*>          Flag indicating whether or not to test the LAPACK routines.
*>
*> line 12: TSTDRV, LOGICAL
*>          Flag indicating whether or not to test the driver routines.
*>
*> line 13: TSTERR, LOGICAL
*>          Flag indicating whether or not to test the error exits for
*>          the LAPACK routines and driver routines.
*>
*> line 14: NEWSD, INTEGER
*>          A code indicating how to set the random number seed.
*>          = 0:  Set the seed to a default value before each run
*>          = 1:  Initialize the seed to a default value only before the
*>                first run
*>          = 2:  Like 1, but use the seed values on the next line
*>
*> If line 14 was 2:
*>
*> line 15: INTEGER array, dimension (4)
*>          Four integer values for the random number seed.
*>
*> lines 15-EOF:  Lines specifying matrix types, as for NEP.
*>          The 3-character path names are 'SVD' or 'SBD' for both the
*>          SVD routines and the SVD driver routines.
*>
*>-----------------------------------------------------------------------
*>
*> DEV and DES data files:
*>
*> line 1:  'DEV' or 'DES' in columns 1 to 3.
*>
*> line 2:  NSIZES, INTEGER
*>          Number of sizes of matrices to use. Should be at least 0
*>          and at most 20. If NSIZES = 0, no testing is done
*>          (although the remaining  3 lines are still read).
*>
*> line 3:  NN, INTEGER array, dimension(NSIZES)
*>          Dimensions of matrices to be tested.
*>
*> line 4:  NB, NBMIN, NX, NS, NBCOL, INTEGERs
*>          These integer parameters determine how blocking is done
*>          (see AB_ILAENV for details)
*>          NB     : block size
*>          NBMIN  : minimum block size
*>          NX     : minimum dimension for blocking
*>          NS     : number of shifts in xHSEQR
*>          NBCOL  : minimum column dimension for blocking
*>
*> line 5:  THRESH, REAL
*>          The test threshold against which computed residuals are
*>          compared. Should generally be in the range from 10. to 20.
*>          If it is 0., all test case data will be printed.
*>
*> line 6:  TSTERR, LOGICAL
*>          Flag indicating whether or not to test the error exits.
*>
*> line 7:  NEWSD, INTEGER
*>          A code indicating how to set the random number seed.
*>          = 0:  Set the seed to a default value before each run
*>          = 1:  Initialize the seed to a default value only before the
*>                first run
*>          = 2:  Like 1, but use the seed values on the next line
*>
*> If line 7 was 2:
*>
*> line 8:  INTEGER array, dimension (4)
*>          Four integer values for the random number seed.
*>
*> lines 9 and following:  Lines specifying matrix types, as for NEP.
*>          The 3-character path name is 'DEV' to test AB_SGEEV, or
*>          'DES' to test AB_SGEES.
*>
*>-----------------------------------------------------------------------
*>
*> The DVX data has two parts. The first part is identical to DEV,
*> and the AB_SECOND part consists of test matrices with precomputed
*> solutions.
*>
*> line 1:  'DVX' in columns 1-3.
*>
*> line 2:  NSIZES, INTEGER
*>          If NSIZES = 0, no testing of randomly generated examples
*>          is done, but any precomputed examples are tested.
*>
*> line 3:  NN, INTEGER array, dimension(NSIZES)
*>
*> line 4:  NB, NBMIN, NX, NS, NBCOL, INTEGERs
*>
*> line 5:  THRESH, REAL
*>
*> line 6:  TSTERR, LOGICAL
*>
*> line 7:  NEWSD, INTEGER
*>
*> If line 7 was 2:
*>
*> line 8:  INTEGER array, dimension (4)
*>
*> lines 9 and following: The first line contains 'DVX' in columns 1-3
*>          followed by the number of matrix types, possibly with
*>          a AB_SECOND line to specify certain matrix types.
*>          If the number of matrix types = 0, no testing of randomly
*>          generated examples is done, but any precomputed examples
*>          are tested.
*>
*> remaining lines : Each matrix is stored on 1+2*N lines, where N is
*>          its dimension. The first line contains the dimension (a
*>          single integer). The next N lines contain the matrix, one
*>          row per line. The last N lines correspond to each
*>          eigenvalue. Each of these last N lines contains 4 real
*>          values: the real part of the eigenvalue, the imaginary
*>          part of the eigenvalue, the reciprocal condition number of
*>          the eigenvalues, and the reciprocal condition number of the
*>          eigenvector.  The end of data is indicated by dimension N=0.
*>          Even if no data is to be tested, there must be at least one
*>          line containing N=0.
*>
*>-----------------------------------------------------------------------
*>
*> The DSX data is like DVX. The first part is identical to DEV, and the
*> AB_SECOND part consists of test matrices with precomputed solutions.
*>
*> line 1:  'DSX' in columns 1-3.
*>
*> line 2:  NSIZES, INTEGER
*>          If NSIZES = 0, no testing of randomly generated examples
*>          is done, but any precomputed examples are tested.
*>
*> line 3:  NN, INTEGER array, dimension(NSIZES)
*>
*> line 4:  NB, NBMIN, NX, NS, NBCOL, INTEGERs
*>
*> line 5:  THRESH, REAL
*>
*> line 6:  TSTERR, LOGICAL
*>
*> line 7:  NEWSD, INTEGER
*>
*> If line 7 was 2:
*>
*> line 8:  INTEGER array, dimension (4)
*>
*> lines 9 and following: The first line contains 'DSX' in columns 1-3
*>          followed by the number of matrix types, possibly with
*>          a AB_SECOND line to specify certain matrix types.
*>          If the number of matrix types = 0, no testing of randomly
*>          generated examples is done, but any precomputed examples
*>          are tested.
*>
*> remaining lines : Each matrix is stored on 3+N lines, where N is its
*>          dimension. The first line contains the dimension N and the
*>          dimension M of an invariant subspace. The AB_SECOND line
*>          contains M integers, identifying the eigenvalues in the
*>          invariant subspace (by their position in a list of
*>          eigenvalues ordered by increasing real part). The next N
*>          lines contain the matrix. The last line contains the
*>          reciprocal condition number for the average of the selected
*>          eigenvalues, and the reciprocal condition number for the
*>          corresponding right invariant subspace. The end of data is
*>          indicated by a line containing N=0 and M=0. Even if no data
*>          is to be tested, there must be at least one line containing
*>          N=0 and M=0.
*>
*>-----------------------------------------------------------------------
*>
*> DGG input file:
*>
*> line 2:  NN, INTEGER
*>          Number of values of N.
*>
*> line 3:  NVAL, INTEGER array, dimension (NN)
*>          The values for the matrix dimension N.
*>
*> line 4:  NPARMS, INTEGER
*>          Number of values of the parameters NB, NBMIN, NS, MAXB, and
*>          NBCOL.
*>
*> line 5:  NBVAL, INTEGER array, dimension (NPARMS)
*>          The values for the blocksize NB.
*>
*> line 6:  NBMIN, INTEGER array, dimension (NPARMS)
*>          The values for NBMIN, the minimum row dimension for blocks.
*>
*> line 7:  NSVAL, INTEGER array, dimension (NPARMS)
*>          The values for the number of shifts.
*>
*> line 8:  MXBVAL, INTEGER array, dimension (NPARMS)
*>          The values for MAXB, used in determining minimum blocksize.
*>
*> line 9:  IACC22, INTEGER array, dimension (NPARMS)
*>          select structured matrix multiply: 1 or 2)
*>
*> line 10: NBCOL, INTEGER array, dimension (NPARMS)
*>          The values for NBCOL, the minimum column dimension for
*>          blocks.
*>
*> line 11: THRESH
*>          Threshold value for the test ratios.  Information will be
*>          printed about each test for which the test ratio is greater
*>          than or equal to the threshold.
*>
*> line 12: TSTCHK, LOGICAL
*>          Flag indicating whether or not to test the LAPACK routines.
*>
*> line 13: TSTDRV, LOGICAL
*>          Flag indicating whether or not to test the driver routines.
*>
*> line 14: TSTERR, LOGICAL
*>          Flag indicating whether or not to test the error exits for
*>          the LAPACK routines and driver routines.
*>
*> line 15: NEWSD, INTEGER
*>          A code indicating how to set the random number seed.
*>          = 0:  Set the seed to a default value before each run
*>          = 1:  Initialize the seed to a default value only before the
*>                first run
*>          = 2:  Like 1, but use the seed values on the next line
*>
*> If line 15 was 2:
*>
*> line 16: INTEGER array, dimension (4)
*>          Four integer values for the random number seed.
*>
*> lines 17-EOF:  Lines specifying matrix types, as for NEP.
*>          The 3-character path name is 'DGG' for the generalized
*>          eigenvalue problem routines and driver routines.
*>
*>-----------------------------------------------------------------------
*>
*> DGS and DGV input files:
*>
*> line 1:  'DGS' or 'DGV' in columns 1 to 3.
*>
*> line 2:  NN, INTEGER
*>          Number of values of N.
*>
*> line 3:  NVAL, INTEGER array, dimension(NN)
*>          Dimensions of matrices to be tested.
*>
*> line 4:  NB, NBMIN, NX, NS, NBCOL, INTEGERs
*>          These integer parameters determine how blocking is done
*>          (see AB_ILAENV for details)
*>          NB     : block size
*>          NBMIN  : minimum block size
*>          NX     : minimum dimension for blocking
*>          NS     : number of shifts in xHGEQR
*>          NBCOL  : minimum column dimension for blocking
*>
*> line 5:  THRESH, REAL
*>          The test threshold against which computed residuals are
*>          compared. Should generally be in the range from 10. to 20.
*>          If it is 0., all test case data will be printed.
*>
*> line 6:  TSTERR, LOGICAL
*>          Flag indicating whether or not to test the error exits.
*>
*> line 7:  NEWSD, INTEGER
*>          A code indicating how to set the random number seed.
*>          = 0:  Set the seed to a default value before each run
*>          = 1:  Initialize the seed to a default value only before the
*>                first run
*>          = 2:  Like 1, but use the seed values on the next line
*>
*> If line 17 was 2:
*>
*> line 7:  INTEGER array, dimension (4)
*>          Four integer values for the random number seed.
*>
*> lines 7-EOF:  Lines specifying matrix types, as for NEP.
*>          The 3-character path name is 'DGS' for the generalized
*>          eigenvalue problem routines and driver routines.
*>
*>-----------------------------------------------------------------------
*>
*> DXV input files:
*>
*> line 1:  'DXV' in columns 1 to 3.
*>
*> line 2:  N, INTEGER
*>          Value of N.
*>
*> line 3:  NB, NBMIN, NX, NS, NBCOL, INTEGERs
*>          These integer parameters determine how blocking is done
*>          (see AB_ILAENV for details)
*>          NB     : block size
*>          NBMIN  : minimum block size
*>          NX     : minimum dimension for blocking
*>          NS     : number of shifts in xHGEQR
*>          NBCOL  : minimum column dimension for blocking
*>
*> line 4:  THRESH, REAL
*>          The test threshold against which computed residuals are
*>          compared. Should generally be in the range from 10. to 20.
*>          Information will be printed about each test for which the
*>          test ratio is greater than or equal to the threshold.
*>
*> line 5:  TSTERR, LOGICAL
*>          Flag indicating whether or not to test the error exits for
*>          the LAPACK routines and driver routines.
*>
*> line 6:  NEWSD, INTEGER
*>          A code indicating how to set the random number seed.
*>          = 0:  Set the seed to a default value before each run
*>          = 1:  Initialize the seed to a default value only before the
*>                first run
*>          = 2:  Like 1, but use the seed values on the next line
*>
*> If line 6 was 2:
*>
*> line 7: INTEGER array, dimension (4)
*>          Four integer values for the random number seed.
*>
*> If line 2 was 0:
*>
*> line 7-EOF: Precomputed examples are tested.
*>
*> remaining lines : Each example is stored on 3+2*N lines, where N is
*>          its dimension. The first line contains the dimension (a
*>          single integer). The next N lines contain the matrix A, one
*>          row per line. The next N lines contain the matrix B.  The
*>          next line contains the reciprocals of the eigenvalue
*>          condition numbers.  The last line contains the reciprocals of
*>          the eigenvector condition numbers.  The end of data is
*>          indicated by dimension N=0.  Even if no data is to be tested,
*>          there must be at least one line containing N=0.
*>
*>-----------------------------------------------------------------------
*>
*> DGX input files:
*>
*> line 1:  'DGX' in columns 1 to 3.
*>
*> line 2:  N, INTEGER
*>          Value of N.
*>
*> line 3:  NB, NBMIN, NX, NS, NBCOL, INTEGERs
*>          These integer parameters determine how blocking is done
*>          (see AB_ILAENV for details)
*>          NB     : block size
*>          NBMIN  : minimum block size
*>          NX     : minimum dimension for blocking
*>          NS     : number of shifts in xHGEQR
*>          NBCOL  : minimum column dimension for blocking
*>
*> line 4:  THRESH, REAL
*>          The test threshold against which computed residuals are
*>          compared. Should generally be in the range from 10. to 20.
*>          Information will be printed about each test for which the
*>          test ratio is greater than or equal to the threshold.
*>
*> line 5:  TSTERR, LOGICAL
*>          Flag indicating whether or not to test the error exits for
*>          the LAPACK routines and driver routines.
*>
*> line 6:  NEWSD, INTEGER
*>          A code indicating how to set the random number seed.
*>          = 0:  Set the seed to a default value before each run
*>          = 1:  Initialize the seed to a default value only before the
*>                first run
*>          = 2:  Like 1, but use the seed values on the next line
*>
*> If line 6 was 2:
*>
*> line 7: INTEGER array, dimension (4)
*>          Four integer values for the random number seed.
*>
*> If line 2 was 0:
*>
*> line 7-EOF: Precomputed examples are tested.
*>
*> remaining lines : Each example is stored on 3+2*N lines, where N is
*>          its dimension. The first line contains the dimension (a
*>          single integer).  The next line contains an integer k such
*>          that only the last k eigenvalues will be selected and appear
*>          in the leading diagonal blocks of $A$ and $B$. The next N
*>          lines contain the matrix A, one row per line.  The next N
*>          lines contain the matrix B.  The last line contains the
*>          reciprocal of the eigenvalue cluster condition number and the
*>          reciprocal of the deflating subspace (associated with the
*>          selected eigencluster) condition number.  The end of data is
*>          indicated by dimension N=0.  Even if no data is to be tested,
*>          there must be at least one line containing N=0.
*>
*>-----------------------------------------------------------------------
*>
*> DSB input file:
*>
*> line 2:  NN, INTEGER
*>          Number of values of N.
*>
*> line 3:  NVAL, INTEGER array, dimension (NN)
*>          The values for the matrix dimension N.
*>
*> line 4:  NK, INTEGER
*>          Number of values of K.
*>
*> line 5:  KVAL, INTEGER array, dimension (NK)
*>          The values for the matrix dimension K.
*>
*> line 6:  THRESH
*>          Threshold value for the test ratios.  Information will be
*>          printed about each test for which the test ratio is greater
*>          than or equal to the threshold.
*>
*> line 7:  NEWSD, INTEGER
*>          A code indicating how to set the random number seed.
*>          = 0:  Set the seed to a default value before each run
*>          = 1:  Initialize the seed to a default value only before the
*>                first run
*>          = 2:  Like 1, but use the seed values on the next line
*>
*> If line 7 was 2:
*>
*> line 8:  INTEGER array, dimension (4)
*>          Four integer values for the random number seed.
*>
*> lines 8-EOF:  Lines specifying matrix types, as for NEP.
*>          The 3-character path name is 'DSB'.
*>
*>-----------------------------------------------------------------------
*>
*> DBB input file:
*>
*> line 2:  NN, INTEGER
*>          Number of values of M and N.
*>
*> line 3:  MVAL, INTEGER array, dimension (NN)
*>          The values for the matrix row dimension M.
*>
*> line 4:  NVAL, INTEGER array, dimension (NN)
*>          The values for the matrix column dimension N.
*>
*> line 4:  NK, INTEGER
*>          Number of values of K.
*>
*> line 5:  KVAL, INTEGER array, dimension (NK)
*>          The values for the matrix bandwidth K.
*>
*> line 6:  NPARMS, INTEGER
*>          Number of values of the parameter NRHS
*>
*> line 7:  NSVAL, INTEGER array, dimension (NPARMS)
*>          The values for the number of right hand sides NRHS.
*>
*> line 8:  THRESH
*>          Threshold value for the test ratios.  Information will be
*>          printed about each test for which the test ratio is greater
*>          than or equal to the threshold.
*>
*> line 9:  NEWSD, INTEGER
*>          A code indicating how to set the random number seed.
*>          = 0:  Set the seed to a default value before each run
*>          = 1:  Initialize the seed to a default value only before the
*>                first run
*>          = 2:  Like 1, but use the seed values on the next line
*>
*> If line 9 was 2:
*>
*> line 10: INTEGER array, dimension (4)
*>          Four integer values for the random number seed.
*>
*> lines 10-EOF:  Lines specifying matrix types, as for SVD.
*>          The 3-character path name is 'DBB'.
*>
*>-----------------------------------------------------------------------
*>
*> DEC input file:
*>
*> line  2: THRESH, REAL
*>          Threshold value for the test ratios.  Information will be
*>          printed about each test for which the test ratio is greater
*>          than or equal to the threshold.
*>
*> lines  3-EOF:
*>
*> Input for testing the eigencondition routines consists of a set of
*> specially constructed test cases and their solutions.  The data
*> format is not intended to be modified by the user.
*>
*>-----------------------------------------------------------------------
*>
*> DBL and DBK input files:
*>
*> line 1:  'DBL' in columns 1-3 to test AB_SGEBAL, or 'DBK' in
*>          columns 1-3 to test AB_SGEBAK.
*>
*> The remaining lines consist of specially constructed test cases.
*>
*>-----------------------------------------------------------------------
*>
*> DGL and DGK input files:
*>
*> line 1:  'DGL' in columns 1-3 to test AB_DGGBAL, or 'DGK' in
*>          columns 1-3 to test AB_DGGBAK.
*>
*> The remaining lines consist of specially constructed test cases.
*>
*>-----------------------------------------------------------------------
*>
*> GLM data file:
*>
*> line 1:  'GLM' in columns 1 to 3.
*>
*> line 2:  NN, INTEGER
*>          Number of values of M, P, and N.
*>
*> line 3:  MVAL, INTEGER array, dimension(NN)
*>          Values of M (row dimension).
*>
*> line 4:  PVAL, INTEGER array, dimension(NN)
*>          Values of P (row dimension).
*>
*> line 5:  NVAL, INTEGER array, dimension(NN)
*>          Values of N (column dimension), note M <= N <= M+P.
*>
*> line 6:  THRESH, REAL
*>          Threshold value for the test ratios.  Information will be
*>          printed about each test for which the test ratio is greater
*>          than or equal to the threshold.
*>
*> line 7:  TSTERR, LOGICAL
*>          Flag indicating whether or not to test the error exits for
*>          the LAPACK routines and driver routines.
*>
*> line 8:  NEWSD, INTEGER
*>          A code indicating how to set the random number seed.
*>          = 0:  Set the seed to a default value before each run
*>          = 1:  Initialize the seed to a default value only before the
*>                first run
*>          = 2:  Like 1, but use the seed values on the next line
*>
*> If line 8 was 2:
*>
*> line 9:  INTEGER array, dimension (4)
*>          Four integer values for the random number seed.
*>
*> lines 9-EOF:  Lines specifying matrix types, as for NEP.
*>          The 3-character path name is 'GLM' for the generalized
*>          linear regression model routines.
*>
*>-----------------------------------------------------------------------
*>
*> GQR data file:
*>
*> line 1:  'GQR' in columns 1 to 3.
*>
*> line 2:  NN, INTEGER
*>          Number of values of M, P, and N.
*>
*> line 3:  MVAL, INTEGER array, dimension(NN)
*>          Values of M.
*>
*> line 4:  PVAL, INTEGER array, dimension(NN)
*>          Values of P.
*>
*> line 5:  NVAL, INTEGER array, dimension(NN)
*>          Values of N.
*>
*> line 6:  THRESH, REAL
*>          Threshold value for the test ratios.  Information will be
*>          printed about each test for which the test ratio is greater
*>          than or equal to the threshold.
*>
*> line 7:  TSTERR, LOGICAL
*>          Flag indicating whether or not to test the error exits for
*>          the LAPACK routines and driver routines.
*>
*> line 8:  NEWSD, INTEGER
*>          A code indicating how to set the random number seed.
*>          = 0:  Set the seed to a default value before each run
*>          = 1:  Initialize the seed to a default value only before the
*>                first run
*>          = 2:  Like 1, but use the seed values on the next line
*>
*> If line 8 was 2:
*>
*> line 9:  INTEGER array, dimension (4)
*>          Four integer values for the random number seed.
*>
*> lines 9-EOF:  Lines specifying matrix types, as for NEP.
*>          The 3-character path name is 'GQR' for the generalized
*>          QR and RQ routines.
*>
*>-----------------------------------------------------------------------
*>
*> GSV data file:
*>
*> line 1:  'GSV' in columns 1 to 3.
*>
*> line 2:  NN, INTEGER
*>          Number of values of M, P, and N.
*>
*> line 3:  MVAL, INTEGER array, dimension(NN)
*>          Values of M (row dimension).
*>
*> line 4:  PVAL, INTEGER array, dimension(NN)
*>          Values of P (row dimension).
*>
*> line 5:  NVAL, INTEGER array, dimension(NN)
*>          Values of N (column dimension).
*>
*> line 6:  THRESH, REAL
*>          Threshold value for the test ratios.  Information will be
*>          printed about each test for which the test ratio is greater
*>          than or equal to the threshold.
*>
*> line 7:  TSTERR, LOGICAL
*>          Flag indicating whether or not to test the error exits for
*>          the LAPACK routines and driver routines.
*>
*> line 8:  NEWSD, INTEGER
*>          A code indicating how to set the random number seed.
*>          = 0:  Set the seed to a default value before each run
*>          = 1:  Initialize the seed to a default value only before the
*>                first run
*>          = 2:  Like 1, but use the seed values on the next line
*>
*> If line 8 was 2:
*>
*> line 9:  INTEGER array, dimension (4)
*>          Four integer values for the random number seed.
*>
*> lines 9-EOF:  Lines specifying matrix types, as for NEP.
*>          The 3-character path name is 'GSV' for the generalized
*>          SVD routines.
*>
*>-----------------------------------------------------------------------
*>
*> CSD data file:
*>
*> line 1:  'CSD' in columns 1 to 3.
*>
*> line 2:  NM, INTEGER
*>          Number of values of M, P, and N.
*>
*> line 3:  MVAL, INTEGER array, dimension(NM)
*>          Values of M (row and column dimension of orthogonal matrix).
*>
*> line 4:  PVAL, INTEGER array, dimension(NM)
*>          Values of P (row dimension of top-left block).
*>
*> line 5:  NVAL, INTEGER array, dimension(NM)
*>          Values of N (column dimension of top-left block).
*>
*> line 6:  THRESH, REAL
*>          Threshold value for the test ratios.  Information will be
*>          printed about each test for which the test ratio is greater
*>          than or equal to the threshold.
*>
*> line 7:  TSTERR, LOGICAL
*>          Flag indicating whether or not to test the error exits for
*>          the LAPACK routines and driver routines.
*>
*> line 8:  NEWSD, INTEGER
*>          A code indicating how to set the random number seed.
*>          = 0:  Set the seed to a default value before each run
*>          = 1:  Initialize the seed to a default value only before the
*>                first run
*>          = 2:  Like 1, but use the seed values on the next line
*>
*> If line 8 was 2:
*>
*> line 9:  INTEGER array, dimension (4)
*>          Four integer values for the random number seed.
*>
*> lines 9-EOF:  Lines specifying matrix types, as for NEP.
*>          The 3-character path name is 'CSD' for the CSD routine.
*>
*>-----------------------------------------------------------------------
*>
*> AB_LSE data file:
*>
*> line 1:  'AB_LSE' in columns 1 to 3.
*>
*> line 2:  NN, INTEGER
*>          Number of values of M, P, and N.
*>
*> line 3:  MVAL, INTEGER array, dimension(NN)
*>          Values of M.
*>
*> line 4:  PVAL, INTEGER array, dimension(NN)
*>          Values of P.
*>
*> line 5:  NVAL, INTEGER array, dimension(NN)
*>          Values of N, note P <= N <= P+M.
*>
*> line 6:  THRESH, REAL
*>          Threshold value for the test ratios.  Information will be
*>          printed about each test for which the test ratio is greater
*>          than or equal to the threshold.
*>
*> line 7:  TSTERR, LOGICAL
*>          Flag indicating whether or not to test the error exits for
*>          the LAPACK routines and driver routines.
*>
*> line 8:  NEWSD, INTEGER
*>          A code indicating how to set the random number seed.
*>          = 0:  Set the seed to a default value before each run
*>          = 1:  Initialize the seed to a default value only before the
*>                first run
*>          = 2:  Like 1, but use the seed values on the next line
*>
*> If line 8 was 2:
*>
*> line 9:  INTEGER array, dimension (4)
*>          Four integer values for the random number seed.
*>
*> lines 9-EOF:  Lines specifying matrix types, as for NEP.
*>          The 3-character path name is 'GSV' for the generalized
*>          SVD routines.
*>
*>-----------------------------------------------------------------------
*>
*> NMAX is currently set to 132 and must be at least 12 for some of the
*> precomputed examples, and LWORK = NMAX*(5*NMAX+5)+1 in the parameter
*> statements below.  For SVD, we assume NRHS may be as big as N.  The
*> parameter NEED is set to 14 to allow for 14 N-by-N matrices for DGG.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date June 2016
*
*> \ingroup double_eig
*
*  =====================================================================
      PROGRAM AB_DCHKEE
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     June 2016
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NMAX
      PARAMETER          ( NMAX = 132 )
      INTEGER            NCMAX
      PARAMETER          ( NCMAX = 20 )
      INTEGER            NEED
      PARAMETER          ( NEED = 14 )
      INTEGER            LWORK
      PARAMETER          ( LWORK = NMAX*( 5*NMAX+5 )+1 )
      INTEGER            LIWORK
      PARAMETER          ( LIWORK = NMAX*( 5*NMAX+20 ) )
      INTEGER            MAXIN
      PARAMETER          ( MAXIN = 20 )
      INTEGER            MAXT
      PARAMETER          ( MAXT = 30 )
      INTEGER            NIN, NOUT
      PARAMETER          ( NIN = 5, NOUT = 6 )
*     ..
*     .. Local Scalars ..
      LOGICAL            CSD, DBB, DGG, DSB, FATAL, GLM, GQR, GSV, AB_LS
     $E,
     $                   NEP, DBK, DBL, SEP, DES, DEV, DGK, DGL, DGS,
     $                   DGV, DGX, DSX, SVD, DVX, DXV, TSTCHK, TSTDIF,
     $                   TSTDRV, TSTERR
      CHARACTER          C1
      CHARACTER*3        C3, PATH
      CHARACTER*32       VNAME
      CHARACTER*10       INTSTR
      CHARACTER*80       LINE
      INTEGER            I, I1, IC, INFO, ITMP, K, LENP, MAXTYP, NEWSD,
     $                   NK, NN, NPARMS, NRHS, NTYPES,
     $                   VERS_MAJOR, VERS_MINOR, VERS_PATCH
      DOUBLE PRECISION   EPS, S1, S2, THRESH, THRSHN
*     ..
*     .. Local Arrays ..
      LOGICAL            DOTYPE( MAXT ), LOGWRK( NMAX )
      INTEGER            IOLDSD( 4 ), ISEED( 4 ), IWORK( LIWORK ),
     $                   KVAL( MAXIN ), MVAL( MAXIN ), MXBVAL( MAXIN ),
     $                   NBCOL( MAXIN ), NBMIN( MAXIN ), NBVAL( MAXIN ),
     $                   NSVAL( MAXIN ), NVAL( MAXIN ), NXVAL( MAXIN ),
     $                   PVAL( MAXIN )
      INTEGER            INMIN( MAXIN ), INWIN( MAXIN ), INIBL( MAXIN ),
     $                   ISHFTS( MAXIN ), IACC22( MAXIN )
      DOUBLE PRECISION   A( NMAX*NMAX, NEED ), B( NMAX*NMAX, 5 ),
     $                   C( NCMAX*NCMAX, NCMAX*NCMAX ), D( NMAX, 12 ),
     $                   RESULT( 500 ), TAUA( NMAX ), TAUB( NMAX ),
     $                   WORK( LWORK ), X( 5*NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            AB_AB_LSAMEN
      DOUBLE PRECISION   AB_DLAMCH, AB_DSECND
      EXTERNAL           AB_AB_LSAMEN, AB_DLAMCH, AB_DSECND
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_ALAREQ, AB_DCHKBB, AB_DCHKBD, AB_DCHKBK, AB_
     $DCHKBL, AB_AB_DCHKEC,
     $                   AB_DCHKGG, AB_DCHKGK, AB_DCHKGL, AB_DCHKHS, AB_
     $DCHKSB, AB_DCHKST,
     $                   AB_DCKCSD, AB_DCKGLM, AB_DCKGQR, AB_DCKGSV, AB_
     $DCKAB_LSE, AB_DDRGES,
     $                   AB_DDRGEV, AB_DDRGSX, AB_DDRGVX, AB_DDRVBD, AB_
     $DDRVES, AB_DDRVEV,
     $                   AB_DDRVSG, AB_DDRVST, AB_DDRVSX, AB_DDRVVX, AB_
     $DERRBD,
     $                   AB_DERRED, AB_DERRGG, AB_DERRHS, AB_DERRST, AB_
     $ILAVER, AB_XLAENV,
     $                   AB_AB_DDRGES3, AB_AB_DDRGEV3, 
     $                   AB_AB_DCHKST2STG, AB_AB_DDRVST2STG, AB_AB_DCHKS
     $B2STG, AB_AB_DDRVSG2STG
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          LEN, MIN
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*32       SRNAMT
      INTEGER            INFOT, MAXB, NPROC, NSHIFT, NUNIT, SELDIM,
     $                   SELOPT
*     ..
*     .. Arrays in Common ..
      LOGICAL            SELVAL( 20 )
      INTEGER            IPARMS( 100 )
      DOUBLE PRECISION   SELWI( 20 ), SELWR( 20 )
*     ..
*     .. Common blocks ..
      COMMON             / CENVIR / NPROC, NSHIFT, MAXB
      COMMON             / INFOC / INFOT, NUNIT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
      COMMON             / SSLCT / SELOPT, SELDIM, SELVAL, SELWR, SELWI
      COMMON             / CLAENV / IPARMS
*     ..
*     .. Data statements ..
      DATA               INTSTR / '0123456789' /
      DATA               IOLDSD / 0, 0, 0, 1 /
*     ..
*     .. Executable Statements ..
*
      A = 0.0
      B = 0.0
      C = 0.0
      D = 0.0
      S1 = AB_DSECND( )
      FATAL = .FALSE.
      NUNIT = NOUT
*
*     Return to here to read multiple sets of data
*
   10 CONTINUE
*
*     Read the first line and set the 3-character test path
*
      READ( NIN, FMT = '(A80)', END = 380 )LINE
      PATH = LINE( 1: 3 )
      NEP = AB_AB_LSAMEN( 3, PATH, 'NEP' ) .OR. AB_AB_LSAMEN( 3, PATH, '
     $DHS' )
      SEP = AB_AB_LSAMEN( 3, PATH, 'SEP' ) .OR. AB_AB_LSAMEN( 3, PATH, '
     $DST' ) .OR.
     $      AB_AB_LSAMEN( 3, PATH, 'DSG' ) .OR. AB_AB_LSAMEN( 3, PATH, '
     $SE2' )
      SVD = AB_AB_LSAMEN( 3, PATH, 'SVD' ) .OR. AB_AB_LSAMEN( 3, PATH, '
     $DBD' )
      DEV = AB_AB_LSAMEN( 3, PATH, 'DEV' )
      DES = AB_AB_LSAMEN( 3, PATH, 'DES' )
      DVX = AB_AB_LSAMEN( 3, PATH, 'DVX' )
      DSX = AB_AB_LSAMEN( 3, PATH, 'DSX' )
      DGG = AB_AB_LSAMEN( 3, PATH, 'DGG' )
      DGS = AB_AB_LSAMEN( 3, PATH, 'DGS' )
      DGX = AB_AB_LSAMEN( 3, PATH, 'DGX' )
      DGV = AB_AB_LSAMEN( 3, PATH, 'DGV' )
      DXV = AB_AB_LSAMEN( 3, PATH, 'DXV' )
      DSB = AB_AB_LSAMEN( 3, PATH, 'DSB' )
      DBB = AB_AB_LSAMEN( 3, PATH, 'DBB' )
      GLM = AB_AB_LSAMEN( 3, PATH, 'GLM' )
      GQR = AB_AB_LSAMEN( 3, PATH, 'GQR' ) .OR. AB_AB_LSAMEN( 3, PATH, '
     $GRQ' )
      GSV = AB_AB_LSAMEN( 3, PATH, 'GSV' )
      CSD = AB_AB_LSAMEN( 3, PATH, 'CSD' )
      AB_LSE = AB_AB_LSAMEN( 3, PATH, 'AB_LSE' )
      DBL = AB_AB_LSAMEN( 3, PATH, 'DBL' )
      DBK = AB_AB_LSAMEN( 3, PATH, 'DBK' )
      DGL = AB_AB_LSAMEN( 3, PATH, 'DGL' )
      DGK = AB_AB_LSAMEN( 3, PATH, 'DGK' )
*
*     Report values of parameters.
*
      IF( PATH.EQ.'   ' ) THEN
         GO TO 10
      ELSE IF( NEP ) THEN
         WRITE( NOUT, FMT = 9987 )
      ELSE IF( SEP ) THEN
         WRITE( NOUT, FMT = 9986 )
      ELSE IF( SVD ) THEN
         WRITE( NOUT, FMT = 9985 )
      ELSE IF( DEV ) THEN
         WRITE( NOUT, FMT = 9979 )
      ELSE IF( DES ) THEN
         WRITE( NOUT, FMT = 9978 )
      ELSE IF( DVX ) THEN
         WRITE( NOUT, FMT = 9977 )
      ELSE IF( DSX ) THEN
         WRITE( NOUT, FMT = 9976 )
      ELSE IF( DGG ) THEN
         WRITE( NOUT, FMT = 9975 )
      ELSE IF( DGS ) THEN
         WRITE( NOUT, FMT = 9964 )
      ELSE IF( DGX ) THEN
         WRITE( NOUT, FMT = 9965 )
      ELSE IF( DGV ) THEN
         WRITE( NOUT, FMT = 9963 )
      ELSE IF( DXV ) THEN
         WRITE( NOUT, FMT = 9962 )
      ELSE IF( DSB ) THEN
         WRITE( NOUT, FMT = 9974 )
      ELSE IF( DBB ) THEN
         WRITE( NOUT, FMT = 9967 )
      ELSE IF( GLM ) THEN
         WRITE( NOUT, FMT = 9971 )
      ELSE IF( GQR ) THEN
         WRITE( NOUT, FMT = 9970 )
      ELSE IF( GSV ) THEN
         WRITE( NOUT, FMT = 9969 )
      ELSE IF( CSD ) THEN
         WRITE( NOUT, FMT = 9960 )
      ELSE IF( AB_LSE ) THEN
         WRITE( NOUT, FMT = 9968 )
      ELSE IF( DBL ) THEN
*
*        AB_DGEBAL:  Balancing
*
         CALL AB_DCHKBL( NIN, NOUT )
         GO TO 10
      ELSE IF( DBK ) THEN
*
*        AB_DGEBAK:  Back transformation
*
         CALL AB_DCHKBK( NIN, NOUT )
         GO TO 10
      ELSE IF( DGL ) THEN
*
*        AB_DGGBAL:  Balancing
*
         CALL AB_DCHKGL( NIN, NOUT )
         GO TO 10
      ELSE IF( DGK ) THEN
*
*        AB_DGGBAK:  Back transformation
*
         CALL AB_DCHKGK( NIN, NOUT )
         GO TO 10
      ELSE IF( AB_AB_LSAMEN( 3, PATH, 'DEC' ) ) THEN
*
*        DEC:  Eigencondition estimation
*
         READ( NIN, FMT = * )THRESH
         CALL AB_XLAENV( 1, 1 )
         CALL AB_XLAENV( 12, 11 )
         CALL AB_XLAENV( 13, 2 )
         CALL AB_XLAENV( 14, 0 )
         CALL AB_XLAENV( 15, 2 )
         CALL AB_XLAENV( 16, 2 )
         TSTERR = .TRUE.
         CALL AB_AB_DCHKEC( THRESH, TSTERR, NIN, NOUT )
         GO TO 10
      ELSE
         WRITE( NOUT, FMT = 9992 )PATH
         GO TO 10
      END IF
      CALL AB_ILAVER( VERS_MAJOR, VERS_MINOR, VERS_PATCH )
      WRITE( NOUT, FMT = 9972 ) VERS_MAJOR, VERS_MINOR, VERS_PATCH
      WRITE( NOUT, FMT = 9984 )
*
*     Read the number of values of M, P, and N.
*
      READ( NIN, FMT = * )NN
      IF( NN.LT.0 ) THEN
         WRITE( NOUT, FMT = 9989 )'   NN ', NN, 1
         NN = 0
         FATAL = .TRUE.
      ELSE IF( NN.GT.MAXIN ) THEN
         WRITE( NOUT, FMT = 9988 )'   NN ', NN, MAXIN
         NN = 0
         FATAL = .TRUE.
      END IF
*
*     Read the values of M
*
      IF( .NOT.( DGX .OR. DXV ) ) THEN
         READ( NIN, FMT = * )( MVAL( I ), I = 1, NN )
         IF( SVD ) THEN
            VNAME = '    M '
         ELSE
            VNAME = '    N '
         END IF
         DO 20 I = 1, NN
            IF( MVAL( I ).LT.0 ) THEN
               WRITE( NOUT, FMT = 9989 )VNAME, MVAL( I ), 0
               FATAL = .TRUE.
            ELSE IF( MVAL( I ).GT.NMAX ) THEN
               WRITE( NOUT, FMT = 9988 )VNAME, MVAL( I ), NMAX
               FATAL = .TRUE.
            END IF
   20    CONTINUE
         WRITE( NOUT, FMT = 9983 )'M:    ', ( MVAL( I ), I = 1, NN )
      END IF
*
*     Read the values of P
*
      IF( GLM .OR. GQR .OR. GSV .OR. CSD .OR. AB_LSE ) THEN
         READ( NIN, FMT = * )( PVAL( I ), I = 1, NN )
         DO 30 I = 1, NN
            IF( PVAL( I ).LT.0 ) THEN
               WRITE( NOUT, FMT = 9989 )' P  ', PVAL( I ), 0
               FATAL = .TRUE.
            ELSE IF( PVAL( I ).GT.NMAX ) THEN
               WRITE( NOUT, FMT = 9988 )' P  ', PVAL( I ), NMAX
               FATAL = .TRUE.
            END IF
   30    CONTINUE
         WRITE( NOUT, FMT = 9983 )'P:    ', ( PVAL( I ), I = 1, NN )
      END IF
*
*     Read the values of N
*
      IF( SVD .OR. DBB .OR. GLM .OR. GQR .OR. GSV .OR. CSD .OR.
     $    AB_LSE ) THEN
         READ( NIN, FMT = * )( NVAL( I ), I = 1, NN )
         DO 40 I = 1, NN
            IF( NVAL( I ).LT.0 ) THEN
               WRITE( NOUT, FMT = 9989 )'    N ', NVAL( I ), 0
               FATAL = .TRUE.
            ELSE IF( NVAL( I ).GT.NMAX ) THEN
               WRITE( NOUT, FMT = 9988 )'    N ', NVAL( I ), NMAX
               FATAL = .TRUE.
            END IF
   40    CONTINUE
      ELSE
         DO 50 I = 1, NN
            NVAL( I ) = MVAL( I )
   50    CONTINUE
      END IF
      IF( .NOT.( DGX .OR. DXV ) ) THEN
         WRITE( NOUT, FMT = 9983 )'N:    ', ( NVAL( I ), I = 1, NN )
      ELSE
         WRITE( NOUT, FMT = 9983 )'N:    ', NN
      END IF
*
*     Read the number of values of K, followed by the values of K
*
      IF( DSB .OR. DBB ) THEN
         READ( NIN, FMT = * )NK
         READ( NIN, FMT = * )( KVAL( I ), I = 1, NK )
         DO 60 I = 1, NK
            IF( KVAL( I ).LT.0 ) THEN
               WRITE( NOUT, FMT = 9989 )'    K ', KVAL( I ), 0
               FATAL = .TRUE.
            ELSE IF( KVAL( I ).GT.NMAX ) THEN
               WRITE( NOUT, FMT = 9988 )'    K ', KVAL( I ), NMAX
               FATAL = .TRUE.
            END IF
   60    CONTINUE
         WRITE( NOUT, FMT = 9983 )'K:    ', ( KVAL( I ), I = 1, NK )
      END IF
*
      IF( DEV .OR. DES .OR. DVX .OR. DSX ) THEN
*
*        For the nonsymmetric QR driver routines, only one set of
*        parameters is allowed.
*
         READ( NIN, FMT = * )NBVAL( 1 ), NBMIN( 1 ), NXVAL( 1 ),
     $      INMIN( 1 ), INWIN( 1 ), INIBL(1), ISHFTS(1), IACC22(1)
         IF( NBVAL( 1 ).LT.1 ) THEN
            WRITE( NOUT, FMT = 9989 )'   NB ', NBVAL( 1 ), 1
            FATAL = .TRUE.
         ELSE IF( NBMIN( 1 ).LT.1 ) THEN
            WRITE( NOUT, FMT = 9989 )'NBMIN ', NBMIN( 1 ), 1
            FATAL = .TRUE.
         ELSE IF( NXVAL( 1 ).LT.1 ) THEN
            WRITE( NOUT, FMT = 9989 )'   NX ', NXVAL( 1 ), 1
            FATAL = .TRUE.
         ELSE IF( INMIN( 1 ).LT.1 ) THEN
            WRITE( NOUT, FMT = 9989 )'   INMIN ', INMIN( 1 ), 1
            FATAL = .TRUE.
         ELSE IF( INWIN( 1 ).LT.1 ) THEN
            WRITE( NOUT, FMT = 9989 )'   INWIN ', INWIN( 1 ), 1
            FATAL = .TRUE.
         ELSE IF( INIBL( 1 ).LT.1 ) THEN
            WRITE( NOUT, FMT = 9989 )'   INIBL ', INIBL( 1 ), 1
            FATAL = .TRUE.
         ELSE IF( ISHFTS( 1 ).LT.1 ) THEN
            WRITE( NOUT, FMT = 9989 )'   ISHFTS ', ISHFTS( 1 ), 1
            FATAL = .TRUE.
         ELSE IF( IACC22( 1 ).LT.0 ) THEN
            WRITE( NOUT, FMT = 9989 )'   IACC22 ', IACC22( 1 ), 0
            FATAL = .TRUE.
         END IF
         CALL AB_XLAENV( 1, NBVAL( 1 ) )
         CALL AB_XLAENV( 2, NBMIN( 1 ) )
         CALL AB_XLAENV( 3, NXVAL( 1 ) )
         CALL AB_XLAENV(12, MAX( 11, INMIN( 1 ) ) )
         CALL AB_XLAENV(13, INWIN( 1 ) )
         CALL AB_XLAENV(14, INIBL( 1 ) )
         CALL AB_XLAENV(15, ISHFTS( 1 ) )
         CALL AB_XLAENV(16, IACC22( 1 ) )
         WRITE( NOUT, FMT = 9983 )'NB:   ', NBVAL( 1 )
         WRITE( NOUT, FMT = 9983 )'NBMIN:', NBMIN( 1 )
         WRITE( NOUT, FMT = 9983 )'NX:   ', NXVAL( 1 )
         WRITE( NOUT, FMT = 9983 )'INMIN:   ', INMIN( 1 )
         WRITE( NOUT, FMT = 9983 )'INWIN: ', INWIN( 1 )
         WRITE( NOUT, FMT = 9983 )'INIBL: ', INIBL( 1 )
         WRITE( NOUT, FMT = 9983 )'ISHFTS: ', ISHFTS( 1 )
         WRITE( NOUT, FMT = 9983 )'IACC22: ', IACC22( 1 )
*
      ELSEIF( DGS .OR. DGX .OR. DGV .OR.  DXV ) THEN
*
*        For the nonsymmetric generalized driver routines, only one set
*        of parameters is allowed.
*
         READ( NIN, FMT = * )NBVAL( 1 ), NBMIN( 1 ), NXVAL( 1 ),
     $      NSVAL( 1 ), MXBVAL( 1 )
         IF( NBVAL( 1 ).LT.1 ) THEN
            WRITE( NOUT, FMT = 9989 )'   NB ', NBVAL( 1 ), 1
            FATAL = .TRUE.
         ELSE IF( NBMIN( 1 ).LT.1 ) THEN
            WRITE( NOUT, FMT = 9989 )'NBMIN ', NBMIN( 1 ), 1
            FATAL = .TRUE.
         ELSE IF( NXVAL( 1 ).LT.1 ) THEN
            WRITE( NOUT, FMT = 9989 )'   NX ', NXVAL( 1 ), 1
            FATAL = .TRUE.
         ELSE IF( NSVAL( 1 ).LT.2 ) THEN
            WRITE( NOUT, FMT = 9989 )'   NS ', NSVAL( 1 ), 2
            FATAL = .TRUE.
         ELSE IF( MXBVAL( 1 ).LT.1 ) THEN
            WRITE( NOUT, FMT = 9989 )' MAXB ', MXBVAL( 1 ), 1
            FATAL = .TRUE.
         END IF
         CALL AB_XLAENV( 1, NBVAL( 1 ) )
         CALL AB_XLAENV( 2, NBMIN( 1 ) )
         CALL AB_XLAENV( 3, NXVAL( 1 ) )
         CALL AB_XLAENV( 4, NSVAL( 1 ) )
         CALL AB_XLAENV( 8, MXBVAL( 1 ) )
         WRITE( NOUT, FMT = 9983 )'NB:   ', NBVAL( 1 )
         WRITE( NOUT, FMT = 9983 )'NBMIN:', NBMIN( 1 )
         WRITE( NOUT, FMT = 9983 )'NX:   ', NXVAL( 1 )
         WRITE( NOUT, FMT = 9983 )'NS:   ', NSVAL( 1 )
         WRITE( NOUT, FMT = 9983 )'MAXB: ', MXBVAL( 1 )
*
      ELSE IF( .NOT.DSB .AND. .NOT.GLM .AND. .NOT.GQR .AND. .NOT.
     $         GSV .AND. .NOT.CSD .AND. .NOT.AB_LSE ) THEN
*
*        For the other paths, the number of parameters can be varied
*        from the input file.  Read the number of parameter values.
*
         READ( NIN, FMT = * )NPARMS
         IF( NPARMS.LT.1 ) THEN
            WRITE( NOUT, FMT = 9989 )'NPARMS', NPARMS, 1
            NPARMS = 0
            FATAL = .TRUE.
         ELSE IF( NPARMS.GT.MAXIN ) THEN
            WRITE( NOUT, FMT = 9988 )'NPARMS', NPARMS, MAXIN
            NPARMS = 0
            FATAL = .TRUE.
         END IF
*
*        Read the values of NB
*
         IF( .NOT.DBB ) THEN
            READ( NIN, FMT = * )( NBVAL( I ), I = 1, NPARMS )
            DO 70 I = 1, NPARMS
               IF( NBVAL( I ).LT.0 ) THEN
                  WRITE( NOUT, FMT = 9989 )'   NB ', NBVAL( I ), 0
                  FATAL = .TRUE.
               ELSE IF( NBVAL( I ).GT.NMAX ) THEN
                  WRITE( NOUT, FMT = 9988 )'   NB ', NBVAL( I ), NMAX
                  FATAL = .TRUE.
               END IF
   70       CONTINUE
            WRITE( NOUT, FMT = 9983 )'NB:   ',
     $         ( NBVAL( I ), I = 1, NPARMS )
         END IF
*
*        Read the values of NBMIN
*
         IF( NEP .OR. SEP .OR. SVD .OR. DGG ) THEN
            READ( NIN, FMT = * )( NBMIN( I ), I = 1, NPARMS )
            DO 80 I = 1, NPARMS
               IF( NBMIN( I ).LT.0 ) THEN
                  WRITE( NOUT, FMT = 9989 )'NBMIN ', NBMIN( I ), 0
                  FATAL = .TRUE.
               ELSE IF( NBMIN( I ).GT.NMAX ) THEN
                  WRITE( NOUT, FMT = 9988 )'NBMIN ', NBMIN( I ), NMAX
                  FATAL = .TRUE.
               END IF
   80       CONTINUE
            WRITE( NOUT, FMT = 9983 )'NBMIN:',
     $         ( NBMIN( I ), I = 1, NPARMS )
         ELSE
            DO 90 I = 1, NPARMS
               NBMIN( I ) = 1
   90       CONTINUE
         END IF
*
*        Read the values of NX
*
         IF( NEP .OR. SEP .OR. SVD ) THEN
            READ( NIN, FMT = * )( NXVAL( I ), I = 1, NPARMS )
            DO 100 I = 1, NPARMS
               IF( NXVAL( I ).LT.0 ) THEN
                  WRITE( NOUT, FMT = 9989 )'   NX ', NXVAL( I ), 0
                  FATAL = .TRUE.
               ELSE IF( NXVAL( I ).GT.NMAX ) THEN
                  WRITE( NOUT, FMT = 9988 )'   NX ', NXVAL( I ), NMAX
                  FATAL = .TRUE.
               END IF
  100       CONTINUE
            WRITE( NOUT, FMT = 9983 )'NX:   ',
     $         ( NXVAL( I ), I = 1, NPARMS )
         ELSE
            DO 110 I = 1, NPARMS
               NXVAL( I ) = 1
  110       CONTINUE
         END IF
*
*        Read the values of NSHIFT (if DGG) or NRHS (if SVD
*        or DBB).
*
         IF( SVD .OR. DBB .OR. DGG ) THEN
            READ( NIN, FMT = * )( NSVAL( I ), I = 1, NPARMS )
            DO 120 I = 1, NPARMS
               IF( NSVAL( I ).LT.0 ) THEN
                  WRITE( NOUT, FMT = 9989 )'   NS ', NSVAL( I ), 0
                  FATAL = .TRUE.
               ELSE IF( NSVAL( I ).GT.NMAX ) THEN
                  WRITE( NOUT, FMT = 9988 )'   NS ', NSVAL( I ), NMAX
                  FATAL = .TRUE.
               END IF
  120       CONTINUE
            WRITE( NOUT, FMT = 9983 )'NS:   ',
     $         ( NSVAL( I ), I = 1, NPARMS )
         ELSE
            DO 130 I = 1, NPARMS
               NSVAL( I ) = 1
  130       CONTINUE
         END IF
*
*        Read the values for MAXB.
*
         IF( DGG ) THEN
            READ( NIN, FMT = * )( MXBVAL( I ), I = 1, NPARMS )
            DO 140 I = 1, NPARMS
               IF( MXBVAL( I ).LT.0 ) THEN
                  WRITE( NOUT, FMT = 9989 )' MAXB ', MXBVAL( I ), 0
                  FATAL = .TRUE.
               ELSE IF( MXBVAL( I ).GT.NMAX ) THEN
                  WRITE( NOUT, FMT = 9988 )' MAXB ', MXBVAL( I ), NMAX
                  FATAL = .TRUE.
               END IF
  140       CONTINUE
            WRITE( NOUT, FMT = 9983 )'MAXB: ',
     $         ( MXBVAL( I ), I = 1, NPARMS )
         ELSE
            DO 150 I = 1, NPARMS
               MXBVAL( I ) = 1
  150       CONTINUE
         END IF
*
*        Read the values for INMIN.
*
         IF( NEP ) THEN
            READ( NIN, FMT = * )( INMIN( I ), I = 1, NPARMS )
            DO 540 I = 1, NPARMS
               IF( INMIN( I ).LT.0 ) THEN
                  WRITE( NOUT, FMT = 9989 )' INMIN ', INMIN( I ), 0
                  FATAL = .TRUE.
               END IF
  540       CONTINUE
            WRITE( NOUT, FMT = 9983 )'INMIN: ',
     $         ( INMIN( I ), I = 1, NPARMS )
         ELSE
            DO 550 I = 1, NPARMS
               INMIN( I ) = 1
  550       CONTINUE
         END IF
*
*        Read the values for INWIN.
*
         IF( NEP ) THEN
            READ( NIN, FMT = * )( INWIN( I ), I = 1, NPARMS )
            DO 560 I = 1, NPARMS
               IF( INWIN( I ).LT.0 ) THEN
                  WRITE( NOUT, FMT = 9989 )' INWIN ', INWIN( I ), 0
                  FATAL = .TRUE.
               END IF
  560       CONTINUE
            WRITE( NOUT, FMT = 9983 )'INWIN: ',
     $         ( INWIN( I ), I = 1, NPARMS )
         ELSE
            DO 570 I = 1, NPARMS
               INWIN( I ) = 1
  570       CONTINUE
         END IF
*
*        Read the values for INIBL.
*
         IF( NEP ) THEN
            READ( NIN, FMT = * )( INIBL( I ), I = 1, NPARMS )
            DO 580 I = 1, NPARMS
               IF( INIBL( I ).LT.0 ) THEN
                  WRITE( NOUT, FMT = 9989 )' INIBL ', INIBL( I ), 0
                  FATAL = .TRUE.
               END IF
  580       CONTINUE
            WRITE( NOUT, FMT = 9983 )'INIBL: ',
     $         ( INIBL( I ), I = 1, NPARMS )
         ELSE
            DO 590 I = 1, NPARMS
               INIBL( I ) = 1
  590       CONTINUE
         END IF
*
*        Read the values for ISHFTS.
*
         IF( NEP ) THEN
            READ( NIN, FMT = * )( ISHFTS( I ), I = 1, NPARMS )
            DO 600 I = 1, NPARMS
               IF( ISHFTS( I ).LT.0 ) THEN
                  WRITE( NOUT, FMT = 9989 )' ISHFTS ', ISHFTS( I ), 0
                  FATAL = .TRUE.
               END IF
  600       CONTINUE
            WRITE( NOUT, FMT = 9983 )'ISHFTS: ',
     $         ( ISHFTS( I ), I = 1, NPARMS )
         ELSE
            DO 610 I = 1, NPARMS
               ISHFTS( I ) = 1
  610       CONTINUE
         END IF
*
*        Read the values for IACC22.
*
         IF( NEP .OR. DGG ) THEN
            READ( NIN, FMT = * )( IACC22( I ), I = 1, NPARMS )
            DO 620 I = 1, NPARMS
               IF( IACC22( I ).LT.0 ) THEN
                  WRITE( NOUT, FMT = 9989 )' IACC22 ', IACC22( I ), 0
                  FATAL = .TRUE.
               END IF
  620       CONTINUE
            WRITE( NOUT, FMT = 9983 )'IACC22: ',
     $         ( IACC22( I ), I = 1, NPARMS )
         ELSE
            DO 630 I = 1, NPARMS
               IACC22( I ) = 1
  630       CONTINUE
         END IF
*
*        Read the values for NBCOL.
*
         IF( DGG ) THEN
            READ( NIN, FMT = * )( NBCOL( I ), I = 1, NPARMS )
            DO 160 I = 1, NPARMS
               IF( NBCOL( I ).LT.0 ) THEN
                  WRITE( NOUT, FMT = 9989 )'NBCOL ', NBCOL( I ), 0
                  FATAL = .TRUE.
               ELSE IF( NBCOL( I ).GT.NMAX ) THEN
                  WRITE( NOUT, FMT = 9988 )'NBCOL ', NBCOL( I ), NMAX
                  FATAL = .TRUE.
               END IF
  160       CONTINUE
            WRITE( NOUT, FMT = 9983 )'NBCOL:',
     $         ( NBCOL( I ), I = 1, NPARMS )
         ELSE
            DO 170 I = 1, NPARMS
               NBCOL( I ) = 1
  170       CONTINUE
         END IF
      END IF
*
*     Calculate and print the machine dependent constants.
*
      WRITE( NOUT, FMT = * )
      EPS = AB_DLAMCH( 'Underflow threshold' )
      WRITE( NOUT, FMT = 9981 )'underflow', EPS
      EPS = AB_DLAMCH( 'Overflow threshold' )
      WRITE( NOUT, FMT = 9981 )'overflow ', EPS
      EPS = AB_DLAMCH( 'Epsilon' )
      WRITE( NOUT, FMT = 9981 )'precision', EPS
*
*     Read the threshold value for the test ratios.
*
      READ( NIN, FMT = * )THRESH
      WRITE( NOUT, FMT = 9982 )THRESH
      IF( SEP .OR. SVD .OR. DGG ) THEN
*
*        Read the flag that indicates whether to test LAPACK routines.
*
         READ( NIN, FMT = * )TSTCHK
*
*        Read the flag that indicates whether to test driver routines.
*
         READ( NIN, FMT = * )TSTDRV
      END IF
*
*     Read the flag that indicates whether to test the error exits.
*
      READ( NIN, FMT = * )TSTERR
*
*     Read the code describing how to set the random number seed.
*
      READ( NIN, FMT = * )NEWSD
*
*     If NEWSD = 2, read another line with 4 integers for the seed.
*
      IF( NEWSD.EQ.2 )
     $   READ( NIN, FMT = * )( IOLDSD( I ), I = 1, 4 )
*
      DO 180 I = 1, 4
         ISEED( I ) = IOLDSD( I )
  180 CONTINUE
*
      IF( FATAL ) THEN
         WRITE( NOUT, FMT = 9999 )
         STOP
      END IF
*
*     Read the input lines indicating the test path and its parameters.
*     The first three characters indicate the test path, and the number
*     of test matrix types must be the first nonblank item in columns
*     4-80.
*
  190 CONTINUE
*
      IF( .NOT.( DGX .OR. DXV ) ) THEN
*
  200    CONTINUE
         READ( NIN, FMT = '(A80)', END = 380 )LINE
         C3 = LINE( 1: 3 )
         LENP = LEN( LINE )
         I = 3
         ITMP = 0
         I1 = 0
  210    CONTINUE
         I = I + 1
         IF( I.GT.LENP ) THEN
            IF( I1.GT.0 ) THEN
               GO TO 240
            ELSE
               NTYPES = MAXT
               GO TO 240
            END IF
         END IF
         IF( LINE( I: I ).NE.' ' .AND. LINE( I: I ).NE.',' ) THEN
            I1 = I
            C1 = LINE( I1: I1 )
*
*        Check that a valid integer was read
*
            DO 220 K = 1, 10
               IF( C1.EQ.INTSTR( K: K ) ) THEN
                  IC = K - 1
                  GO TO 230
               END IF
  220       CONTINUE
            WRITE( NOUT, FMT = 9991 )I, LINE
            GO TO 200
  230       CONTINUE
            ITMP = 10*ITMP + IC
            GO TO 210
         ELSE IF( I1.GT.0 ) THEN
            GO TO 240
         ELSE
            GO TO 210
         END IF
  240    CONTINUE
         NTYPES = ITMP
*
*     Skip the tests if NTYPES is <= 0.
*
         IF( .NOT.( DEV .OR. DES .OR. DVX .OR. DSX .OR. DGV .OR.
     $       DGS ) .AND. NTYPES.LE.0 ) THEN
            WRITE( NOUT, FMT = 9990 )C3
            GO TO 200
         END IF
*
      ELSE
         IF( DXV )
     $      C3 = 'DXV'
         IF( DGX )
     $      C3 = 'DGX'
      END IF
*
*     Reset the random number seed.
*
      IF( NEWSD.EQ.0 ) THEN
         DO 250 K = 1, 4
            ISEED( K ) = IOLDSD( K )
  250    CONTINUE
      END IF
*
      IF( AB_AB_LSAMEN( 3, C3, 'DHS' ) .OR. AB_AB_LSAMEN( 3, C3, 'NEP' )
     $ ) THEN
*
*        -------------------------------------
*        NEP:  Nonsymmetric Eigenvalue Problem
*        -------------------------------------
*        Vary the parameters
*           NB    = block size
*           NBMIN = minimum block size
*           NX    = crossover point
*           NS    = number of shifts
*           MAXB  = minimum submatrix size
*
         MAXTYP = 21
         NTYPES = MIN( MAXTYP, NTYPES )
         CALL AB_ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
         CALL AB_XLAENV( 1, 1 )
         IF( TSTERR )
     $      CALL AB_DERRHS( 'AB_DHSEQR', NOUT )
         DO 270 I = 1, NPARMS
            CALL AB_XLAENV( 1, NBVAL( I ) )
            CALL AB_XLAENV( 2, NBMIN( I ) )
            CALL AB_XLAENV( 3, NXVAL( I ) )
            CALL AB_XLAENV(12, MAX( 11, INMIN( I ) ) )
            CALL AB_XLAENV(13, INWIN( I ) )
            CALL AB_XLAENV(14, INIBL( I ) )
            CALL AB_XLAENV(15, ISHFTS( I ) )
            CALL AB_XLAENV(16, IACC22( I ) )
*
            IF( NEWSD.EQ.0 ) THEN
               DO 260 K = 1, 4
                  ISEED( K ) = IOLDSD( K )
  260          CONTINUE
            END IF
            WRITE( NOUT, FMT = 9961 )C3, NBVAL( I ), NBMIN( I ),
     $         NXVAL( I ), MAX( 11, INMIN(I)),
     $         INWIN( I ), INIBL( I ), ISHFTS( I ), IACC22( I )
            CALL AB_DCHKHS( NN, NVAL, MAXTYP, DOTYPE, ISEED, THRESH, NOU
     $T,
     $                   A( 1, 1 ), NMAX, A( 1, 2 ), A( 1, 3 ),
     $                   A( 1, 4 ), A( 1, 5 ), NMAX, A( 1, 6 ),
     $                   A( 1, 7 ), D( 1, 1 ), D( 1, 2 ), D( 1, 3 ),
     $                   D( 1, 4 ), D( 1, 5 ), D( 1, 6 ), A( 1, 8 ),
     $                   A( 1, 9 ), A( 1, 10 ), A( 1, 11 ), A( 1, 12 ),
     $                   D( 1, 7 ), WORK, LWORK, IWORK, LOGWRK, RESULT,
     $                   INFO )
            IF( INFO.NE.0 )
     $         WRITE( NOUT, FMT = 9980 )'AB_DCHKHS', INFO
  270    CONTINUE
*
      ELSE IF( AB_AB_LSAMEN( 3, C3, 'DST' ) .OR. AB_AB_LSAMEN( 3, C3,
     $ 'SEP' ) 
     $                                .OR. AB_AB_LSAMEN( 3, C3, 'SE2' ) 
     $) THEN
*
*        ----------------------------------
*        SEP:  Symmetric Eigenvalue Problem
*        ----------------------------------
*        Vary the parameters
*           NB    = block size
*           NBMIN = minimum block size
*           NX    = crossover point
*
         MAXTYP = 21
         NTYPES = MIN( MAXTYP, NTYPES )
         CALL AB_ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
         CALL AB_XLAENV( 1, 1 )
         CALL AB_XLAENV( 9, 25 )
         IF( TSTERR )
     $      CALL AB_DERRST( 'DST', NOUT )
         DO 290 I = 1, NPARMS
            CALL AB_XLAENV( 1, NBVAL( I ) )
            CALL AB_XLAENV( 2, NBMIN( I ) )
            CALL AB_XLAENV( 3, NXVAL( I ) )
*
            IF( NEWSD.EQ.0 ) THEN
               DO 280 K = 1, 4
                  ISEED( K ) = IOLDSD( K )
  280          CONTINUE
            END IF
            WRITE( NOUT, FMT = 9997 )C3, NBVAL( I ), NBMIN( I ),
     $         NXVAL( I )
            IF( TSTCHK ) THEN
               IF( AB_AB_LSAMEN( 3, C3, 'SE2' ) ) THEN
               CALL AB_AB_DCHKST2STG( NN, NVAL, MAXTYP, DOTYPE, ISEED, T
     $HRESH,
     $                      NOUT, A( 1, 1 ), NMAX, A( 1, 2 ), D( 1, 1 ),
     $                      D( 1, 2 ), D( 1, 3 ), D( 1, 4 ), D( 1, 5 ),
     $                      D( 1, 6 ), D( 1, 7 ), D( 1, 8 ), D( 1, 9 ),
     $                      D( 1, 10 ), D( 1, 11 ), A( 1, 3 ), NMAX,
     $                      A( 1, 4 ), A( 1, 5 ), D( 1, 12 ), A( 1, 6 ),
     $                      WORK, LWORK, IWORK, LIWORK, RESULT, INFO )
               ELSE
               CALL AB_DCHKST( NN, NVAL, MAXTYP, DOTYPE, ISEED, THRESH,
     $                      NOUT, A( 1, 1 ), NMAX, A( 1, 2 ), D( 1, 1 ),
     $                      D( 1, 2 ), D( 1, 3 ), D( 1, 4 ), D( 1, 5 ),
     $                      D( 1, 6 ), D( 1, 7 ), D( 1, 8 ), D( 1, 9 ),
     $                      D( 1, 10 ), D( 1, 11 ), A( 1, 3 ), NMAX,
     $                      A( 1, 4 ), A( 1, 5 ), D( 1, 12 ), A( 1, 6 ),
     $                      WORK, LWORK, IWORK, LIWORK, RESULT, INFO )
               ENDIF
               IF( INFO.NE.0 )
     $            WRITE( NOUT, FMT = 9980 )'AB_DCHKST', INFO
            END IF
            IF( TSTDRV ) THEN
               IF( AB_AB_LSAMEN( 3, C3, 'SE2' ) ) THEN
               CALL AB_AB_DDRVST2STG( NN, NVAL, 18, DOTYPE, ISEED, THRES
     $H,
     $                      NOUT, A( 1, 1 ), NMAX, D( 1, 3 ), D( 1, 4 ),
     $                      D( 1, 5 ), D( 1, 6 ), D( 1, 8 ), D( 1, 9 ),
     $                      D( 1, 10 ), D( 1, 11 ), A( 1, 2 ), NMAX,
     $                      A( 1, 3 ), D( 1, 12 ), A( 1, 4 ), WORK,
     $                      LWORK, IWORK, LIWORK, RESULT, INFO )
               ELSE
               CALL AB_DDRVST( NN, NVAL, 18, DOTYPE, ISEED, THRESH, NOUT
     $,
     $                      A( 1, 1 ), NMAX, D( 1, 3 ), D( 1, 4 ),
     $                      D( 1, 5 ), D( 1, 6 ), D( 1, 8 ), D( 1, 9 ),
     $                      D( 1, 10 ), D( 1, 11 ), A( 1, 2 ), NMAX,
     $                      A( 1, 3 ), D( 1, 12 ), A( 1, 4 ), WORK,
     $                      LWORK, IWORK, LIWORK, RESULT, INFO )
               ENDIF
               IF( INFO.NE.0 )
     $            WRITE( NOUT, FMT = 9980 )'AB_DDRVST', INFO
            END IF
  290    CONTINUE
*
      ELSE IF( AB_AB_LSAMEN( 3, C3, 'DSG' ) ) THEN
*
*        ----------------------------------------------
*        DSG:  Symmetric Generalized Eigenvalue Problem
*        ----------------------------------------------
*        Vary the parameters
*           NB    = block size
*           NBMIN = minimum block size
*           NX    = crossover point
*
         MAXTYP = 21
         NTYPES = MIN( MAXTYP, NTYPES )
         CALL AB_ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
         CALL AB_XLAENV( 9, 25 )
         DO 310 I = 1, NPARMS
            CALL AB_XLAENV( 1, NBVAL( I ) )
            CALL AB_XLAENV( 2, NBMIN( I ) )
            CALL AB_XLAENV( 3, NXVAL( I ) )
*
            IF( NEWSD.EQ.0 ) THEN
               DO 300 K = 1, 4
                  ISEED( K ) = IOLDSD( K )
  300          CONTINUE
            END IF
            WRITE( NOUT, FMT = 9997 )C3, NBVAL( I ), NBMIN( I ),
     $         NXVAL( I )
            IF( TSTCHK ) THEN
*               CALL AB_DDRVSG( NN, NVAL, MAXTYP, DOTYPE, ISEED, THRESH,
*     $                      NOUT, A( 1, 1 ), NMAX, A( 1, 2 ), NMAX,
*     $                      D( 1, 3 ), A( 1, 3 ), NMAX, A( 1, 4 ),
*     $                      A( 1, 5 ), A( 1, 6 ), A( 1, 7 ), WORK,
*     $                      LWORK, IWORK, LIWORK, RESULT, INFO )
               CALL AB_AB_DDRVSG2STG( NN, NVAL, MAXTYP, DOTYPE, ISEED, T
     $HRESH,
     $                          NOUT, A( 1, 1 ), NMAX, A( 1, 2 ), NMAX,
     $                          D( 1, 3 ), D( 1, 3 ), A( 1, 3 ), NMAX,
     $                          A( 1, 4 ), A( 1, 5 ), A( 1, 6 ),
     $                          A( 1, 7 ), WORK, LWORK, IWORK, LIWORK,
     $                          RESULT, INFO )
               IF( INFO.NE.0 )
     $            WRITE( NOUT, FMT = 9980 )'AB_DDRVSG', INFO
            END IF
  310    CONTINUE
*
      ELSE IF( AB_AB_LSAMEN( 3, C3, 'DBD' ) .OR. AB_AB_LSAMEN( 3, C3,
     $ 'SVD' ) ) THEN
*
*        ----------------------------------
*        SVD:  Singular Value Decomposition
*        ----------------------------------
*        Vary the parameters
*           NB    = block size
*           NBMIN = minimum block size
*           NX    = crossover point
*           NRHS  = number of right hand sides
*
         MAXTYP = 16
         NTYPES = MIN( MAXTYP, NTYPES )
         CALL AB_ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
         CALL AB_XLAENV( 1, 1 )
         CALL AB_XLAENV( 9, 25 )
*
*        Test the error exits
*
         IF( TSTERR .AND. TSTCHK )
     $      CALL AB_DERRBD( 'DBD', NOUT )
         IF( TSTERR .AND. TSTDRV )
     $      CALL AB_DERRED( 'DBD', NOUT )
*
         DO 330 I = 1, NPARMS
            NRHS = NSVAL( I )
            CALL AB_XLAENV( 1, NBVAL( I ) )
            CALL AB_XLAENV( 2, NBMIN( I ) )
            CALL AB_XLAENV( 3, NXVAL( I ) )
            IF( NEWSD.EQ.0 ) THEN
               DO 320 K = 1, 4
                  ISEED( K ) = IOLDSD( K )
  320          CONTINUE
            END IF
            WRITE( NOUT, FMT = 9995 )C3, NBVAL( I ), NBMIN( I ),
     $         NXVAL( I ), NRHS
            IF( TSTCHK ) THEN
               CALL AB_DCHKBD( NN, MVAL, NVAL, MAXTYP, DOTYPE, NRHS, ISE
     $ED,
     $                      THRESH, A( 1, 1 ), NMAX, D( 1, 1 ),
     $                      D( 1, 2 ), D( 1, 3 ), D( 1, 4 ), A( 1, 2 ),
     $                      NMAX, A( 1, 3 ), A( 1, 4 ), A( 1, 5 ), NMAX,
     $                      A( 1, 6 ), NMAX, A( 1, 7 ), A( 1, 8 ), WORK,
     $                      LWORK, IWORK, NOUT, INFO )
               IF( INFO.NE.0 )
     $            WRITE( NOUT, FMT = 9980 )'AB_DCHKBD', INFO
            END IF
            IF( TSTDRV )
     $         CALL AB_DDRVBD( NN, MVAL, NVAL, MAXTYP, DOTYPE, ISEED,
     $                      THRESH, A( 1, 1 ), NMAX, A( 1, 2 ), NMAX,
     $                      A( 1, 3 ), NMAX, A( 1, 4 ), A( 1, 5 ),
     $                      A( 1, 6 ), D( 1, 1 ), D( 1, 2 ), D( 1, 3 ),
     $                      WORK, LWORK, IWORK, NOUT, INFO )
  330    CONTINUE
*
      ELSE IF( AB_AB_LSAMEN( 3, C3, 'DEV' ) ) THEN
*
*        --------------------------------------------
*        DEV:  Nonsymmetric Eigenvalue Problem Driver
*              AB_DGEEV (eigenvalues and eigenvectors)
*        --------------------------------------------
*
         MAXTYP = 21
         NTYPES = MIN( MAXTYP, NTYPES )
         IF( NTYPES.LE.0 ) THEN
            WRITE( NOUT, FMT = 9990 )C3
         ELSE
            IF( TSTERR )
     $         CALL AB_DERRED( C3, NOUT )
            CALL AB_ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
            CALL AB_DDRVEV( NN, NVAL, NTYPES, DOTYPE, ISEED, THRESH, NOU
     $T,
     $                   A( 1, 1 ), NMAX, A( 1, 2 ), D( 1, 1 ),
     $                   D( 1, 2 ), D( 1, 3 ), D( 1, 4 ), A( 1, 3 ),
     $                   NMAX, A( 1, 4 ), NMAX, A( 1, 5 ), NMAX, RESULT,
     $                   WORK, LWORK, IWORK, INFO )
            IF( INFO.NE.0 )
     $         WRITE( NOUT, FMT = 9980 )'AB_DGEEV', INFO
         END IF
         WRITE( NOUT, FMT = 9973 )
         GO TO 10
*
      ELSE IF( AB_AB_LSAMEN( 3, C3, 'DES' ) ) THEN
*
*        --------------------------------------------
*        DES:  Nonsymmetric Eigenvalue Problem Driver
*              AB_DGEES (Schur form)
*        --------------------------------------------
*
         MAXTYP = 21
         NTYPES = MIN( MAXTYP, NTYPES )
         IF( NTYPES.LE.0 ) THEN
            WRITE( NOUT, FMT = 9990 )C3
         ELSE
            IF( TSTERR )
     $         CALL AB_DERRED( C3, NOUT )
            CALL AB_ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
            CALL AB_DDRVES( NN, NVAL, NTYPES, DOTYPE, ISEED, THRESH, NOU
     $T,
     $                   A( 1, 1 ), NMAX, A( 1, 2 ), A( 1, 3 ),
     $                   D( 1, 1 ), D( 1, 2 ), D( 1, 3 ), D( 1, 4 ),
     $                   A( 1, 4 ), NMAX, RESULT, WORK, LWORK, IWORK,
     $                   LOGWRK, INFO )
            IF( INFO.NE.0 )
     $         WRITE( NOUT, FMT = 9980 )'AB_DGEES', INFO
         END IF
         WRITE( NOUT, FMT = 9973 )
         GO TO 10
*
      ELSE IF( AB_AB_LSAMEN( 3, C3, 'DVX' ) ) THEN
*
*        --------------------------------------------------------------
*        DVX:  Nonsymmetric Eigenvalue Problem Expert Driver
*              AB_AB_DGEEVX (eigenvalues, eigenvectors and condition numbers)
*        --------------------------------------------------------------
*
         MAXTYP = 21
         NTYPES = MIN( MAXTYP, NTYPES )
         IF( NTYPES.LT.0 ) THEN
            WRITE( NOUT, FMT = 9990 )C3
         ELSE
            IF( TSTERR )
     $         CALL AB_DERRED( C3, NOUT )
            CALL AB_ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
            CALL AB_DDRVVX( NN, NVAL, NTYPES, DOTYPE, ISEED, THRESH, NIN
     $,
     $                   NOUT, A( 1, 1 ), NMAX, A( 1, 2 ), D( 1, 1 ),
     $                   D( 1, 2 ), D( 1, 3 ), D( 1, 4 ), A( 1, 3 ),
     $                   NMAX, A( 1, 4 ), NMAX, A( 1, 5 ), NMAX,
     $                   D( 1, 5 ), D( 1, 6 ), D( 1, 7 ), D( 1, 8 ),
     $                   D( 1, 9 ), D( 1, 10 ), D( 1, 11 ), D( 1, 12 ),
     $                   RESULT, WORK, LWORK, IWORK, INFO )
            IF( INFO.NE.0 )
     $         WRITE( NOUT, FMT = 9980 )'AB_AB_DGEEVX', INFO
         END IF
         WRITE( NOUT, FMT = 9973 )
         GO TO 10
*
      ELSE IF( AB_AB_LSAMEN( 3, C3, 'DSX' ) ) THEN
*
*        ---------------------------------------------------
*        DSX:  Nonsymmetric Eigenvalue Problem Expert Driver
*              AB_AB_DGEESX (Schur form and condition numbers)
*        ---------------------------------------------------
*
         MAXTYP = 21
         NTYPES = MIN( MAXTYP, NTYPES )
         IF( NTYPES.LT.0 ) THEN
            WRITE( NOUT, FMT = 9990 )C3
         ELSE
            IF( TSTERR )
     $         CALL AB_DERRED( C3, NOUT )
            CALL AB_ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
            CALL AB_DDRVSX( NN, NVAL, NTYPES, DOTYPE, ISEED, THRESH, NIN
     $,
     $                   NOUT, A( 1, 1 ), NMAX, A( 1, 2 ), A( 1, 3 ),
     $                   D( 1, 1 ), D( 1, 2 ), D( 1, 3 ), D( 1, 4 ),
     $                   D( 1, 5 ), D( 1, 6 ), A( 1, 4 ), NMAX,
     $                   A( 1, 5 ), RESULT, WORK, LWORK, IWORK, LOGWRK,
     $                   INFO )
            IF( INFO.NE.0 )
     $         WRITE( NOUT, FMT = 9980 )'AB_AB_DGEESX', INFO
         END IF
         WRITE( NOUT, FMT = 9973 )
         GO TO 10
*
      ELSE IF( AB_AB_LSAMEN( 3, C3, 'DGG' ) ) THEN
*
*        -------------------------------------------------
*        DGG:  Generalized Nonsymmetric Eigenvalue Problem
*        -------------------------------------------------
*        Vary the parameters
*           NB    = block size
*           NBMIN = minimum block size
*           NS    = number of shifts
*           MAXB  = minimum submatrix size
*           IACC22: structured matrix multiply
*           NBCOL = minimum column dimension for blocks
*
         MAXTYP = 26
         NTYPES = MIN( MAXTYP, NTYPES )
         CALL AB_ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
         CALL AB_XLAENV(1,1)
         IF( TSTCHK .AND. TSTERR )
     $      CALL AB_DERRGG( C3, NOUT )
         DO 350 I = 1, NPARMS
            CALL AB_XLAENV( 1, NBVAL( I ) )
            CALL AB_XLAENV( 2, NBMIN( I ) )
            CALL AB_XLAENV( 4, NSVAL( I ) )
            CALL AB_XLAENV( 8, MXBVAL( I ) )
            CALL AB_XLAENV( 16, IACC22( I ) )
            CALL AB_XLAENV( 5, NBCOL( I ) )
*
            IF( NEWSD.EQ.0 ) THEN
               DO 340 K = 1, 4
                  ISEED( K ) = IOLDSD( K )
  340          CONTINUE
            END IF
            WRITE( NOUT, FMT = 9996 )C3, NBVAL( I ), NBMIN( I ),
     $         NSVAL( I ), MXBVAL( I ), IACC22( I ), NBCOL( I )
            TSTDIF = .FALSE.
            THRSHN = 10.D0
            IF( TSTCHK ) THEN
               CALL AB_DCHKGG( NN, NVAL, MAXTYP, DOTYPE, ISEED, THRESH,
     $                      TSTDIF, THRSHN, NOUT, A( 1, 1 ), NMAX,
     $                      A( 1, 2 ), A( 1, 3 ), A( 1, 4 ), A( 1, 5 ),
     $                      A( 1, 6 ), A( 1, 7 ), A( 1, 8 ), A( 1, 9 ),
     $                      NMAX, A( 1, 10 ), A( 1, 11 ), A( 1, 12 ),
     $                      D( 1, 1 ), D( 1, 2 ), D( 1, 3 ), D( 1, 4 ),
     $                      D( 1, 5 ), D( 1, 6 ), A( 1, 13 ),
     $                      A( 1, 14 ), WORK, LWORK, LOGWRK, RESULT,
     $                      INFO )
               IF( INFO.NE.0 )
     $            WRITE( NOUT, FMT = 9980 )'AB_DCHKGG', INFO
            END IF
  350    CONTINUE
*
      ELSE IF( AB_AB_LSAMEN( 3, C3, 'DGS' ) ) THEN
*
*        -------------------------------------------------
*        DGS:  Generalized Nonsymmetric Eigenvalue Problem
*              AB_DGGES (Schur form)
*        -------------------------------------------------
*
         MAXTYP = 26
         NTYPES = MIN( MAXTYP, NTYPES )
         IF( NTYPES.LE.0 ) THEN
            WRITE( NOUT, FMT = 9990 )C3
         ELSE
            IF( TSTERR )
     $         CALL AB_DERRGG( C3, NOUT )
            CALL AB_ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
            CALL AB_DDRGES( NN, NVAL, MAXTYP, DOTYPE, ISEED, THRESH, NOU
     $T,
     $                   A( 1, 1 ), NMAX, A( 1, 2 ), A( 1, 3 ),
     $                   A( 1, 4 ), A( 1, 7 ), NMAX, A( 1, 8 ),
     $                   D( 1, 1 ), D( 1, 2 ), D( 1, 3 ), WORK, LWORK,
     $                   RESULT, LOGWRK, INFO )
            IF( INFO.NE.0 )
     $          WRITE( NOUT, FMT = 9980 )'AB_DDRGES', INFO
*
*     Blocked version
*
            CALL AB_XLAENV(16, 2)
            CALL AB_AB_DDRGES3( NN, NVAL, MAXTYP, DOTYPE, ISEED, THRESH,
     $ NOUT,
     $                    A( 1, 1 ), NMAX, A( 1, 2 ), A( 1, 3 ),
     $                    A( 1, 4 ), A( 1, 7 ), NMAX, A( 1, 8 ),
     $                    D( 1, 1 ), D( 1, 2 ), D( 1, 3 ), WORK, LWORK,
     $                    RESULT, LOGWRK, INFO )
            IF( INFO.NE.0 )
     $          WRITE( NOUT, FMT = 9980 )'AB_AB_DDRGES3', INFO
         END IF
         WRITE( NOUT, FMT = 9973 )
         GO TO 10
*
      ELSE IF( DGX ) THEN
*
*        -------------------------------------------------
*        DGX:  Generalized Nonsymmetric Eigenvalue Problem
*              AB_AB_DGGESX (Schur form and condition numbers)
*        -------------------------------------------------
*
         MAXTYP = 5
         NTYPES = MAXTYP
         IF( NN.LT.0 ) THEN
            WRITE( NOUT, FMT = 9990 )C3
         ELSE
            IF( TSTERR )
     $         CALL AB_DERRGG( C3, NOUT )
            CALL AB_ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
            CALL AB_XLAENV( 5, 2 )
            CALL AB_DDRGSX( NN, NCMAX, THRESH, NIN, NOUT, A( 1, 1 ), NMA
     $X,
     $                   A( 1, 2 ), A( 1, 3 ), A( 1, 4 ), A( 1, 5 ),
     $                   A( 1, 6 ), D( 1, 1 ), D( 1, 2 ), D( 1, 3 ),
     $                   C( 1, 1 ), NCMAX*NCMAX, A( 1, 12 ), WORK,
     $                   LWORK, IWORK, LIWORK, LOGWRK, INFO )
            IF( INFO.NE.0 )
     $         WRITE( NOUT, FMT = 9980 )'AB_DDRGSX', INFO
         END IF
         WRITE( NOUT, FMT = 9973 )
         GO TO 10
*
      ELSE IF( AB_AB_LSAMEN( 3, C3, 'DGV' ) ) THEN
*
*        -------------------------------------------------
*        DGV:  Generalized Nonsymmetric Eigenvalue Problem
*              AB_DGGEV (Eigenvalue/vector form)
*        -------------------------------------------------
*
         MAXTYP = 26
         NTYPES = MIN( MAXTYP, NTYPES )
         IF( NTYPES.LE.0 ) THEN
            WRITE( NOUT, FMT = 9990 )C3
         ELSE
            IF( TSTERR )
     $         CALL AB_DERRGG( C3, NOUT )
            CALL AB_ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
            CALL AB_DDRGEV( NN, NVAL, MAXTYP, DOTYPE, ISEED, THRESH, NOU
     $T,
     $                   A( 1, 1 ), NMAX, A( 1, 2 ), A( 1, 3 ),
     $                   A( 1, 4 ), A( 1, 7 ), NMAX, A( 1, 8 ),
     $                   A( 1, 9 ), NMAX, D( 1, 1 ), D( 1, 2 ),
     $                   D( 1, 3 ), D( 1, 4 ), D( 1, 5 ), D( 1, 6 ),
     $                   WORK, LWORK, RESULT, INFO )
            IF( INFO.NE.0 )
     $         WRITE( NOUT, FMT = 9980 )'AB_DDRGEV', INFO
*
*     Blocked version
*
            CALL AB_AB_DDRGEV3( NN, NVAL, MAXTYP, DOTYPE, ISEED, THRESH,
     $ NOUT,
     $                    A( 1, 1 ), NMAX, A( 1, 2 ), A( 1, 3 ),
     $                    A( 1, 4 ), A( 1, 7 ), NMAX, A( 1, 8 ),
     $                    A( 1, 9 ), NMAX, D( 1, 1 ), D( 1, 2 ),
     $                    D( 1, 3 ), D( 1, 4 ), D( 1, 5 ), D( 1, 6 ),
     $                    WORK, LWORK, RESULT, INFO )
            IF( INFO.NE.0 )
     $         WRITE( NOUT, FMT = 9980 )'AB_AB_DDRGEV3', INFO
         END IF
         WRITE( NOUT, FMT = 9973 )
         GO TO 10
*
      ELSE IF( DXV ) THEN
*
*        -------------------------------------------------
*        DXV:  Generalized Nonsymmetric Eigenvalue Problem
*              AB_AB_DGGEVX (eigenvalue/vector with condition numbers)
*        -------------------------------------------------
*
         MAXTYP = 2
         NTYPES = MAXTYP
         IF( NN.LT.0 ) THEN
            WRITE( NOUT, FMT = 9990 )C3
         ELSE
            IF( TSTERR )
     $         CALL AB_DERRGG( C3, NOUT )
            CALL AB_ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
            CALL AB_DDRGVX( NN, THRESH, NIN, NOUT, A( 1, 1 ), NMAX,
     $                   A( 1, 2 ), A( 1, 3 ), A( 1, 4 ), D( 1, 1 ),
     $                   D( 1, 2 ), D( 1, 3 ), A( 1, 5 ), A( 1, 6 ),
     $                   IWORK( 1 ), IWORK( 2 ), D( 1, 4 ), D( 1, 5 ),
     $                   D( 1, 6 ), D( 1, 7 ), D( 1, 8 ), D( 1, 9 ),
     $                   WORK, LWORK, IWORK( 3 ), LIWORK-2, RESULT,
     $                   LOGWRK, INFO )
*
            IF( INFO.NE.0 )
     $         WRITE( NOUT, FMT = 9980 )'AB_DDRGVX', INFO
         END IF
         WRITE( NOUT, FMT = 9973 )
         GO TO 10
*
      ELSE IF( AB_AB_LSAMEN( 3, C3, 'DSB' ) ) THEN
*
*        ------------------------------
*        DSB:  Symmetric Band Reduction
*        ------------------------------
*
         MAXTYP = 15
         NTYPES = MIN( MAXTYP, NTYPES )
         CALL AB_ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
         IF( TSTERR )
     $      CALL AB_DERRST( 'DSB', NOUT )
*         CALL AB_DCHKSB( NN, NVAL, NK, KVAL, MAXTYP, DOTYPE, ISEED, THRESH,
*     $                NOUT, A( 1, 1 ), NMAX, D( 1, 1 ), D( 1, 2 ),
*     $                A( 1, 2 ), NMAX, WORK, LWORK, RESULT, INFO )
         CALL AB_AB_DCHKSB2STG( NN, NVAL, NK, KVAL, MAXTYP, DOTYPE, ISEE
     $D,
     $                 THRESH, NOUT, A( 1, 1 ), NMAX, D( 1, 1 ), 
     $                 D( 1, 2 ), D( 1, 3 ), D( 1, 4 ), D( 1, 5 ),
     $                 A( 1, 2 ), NMAX, WORK, LWORK, RESULT, INFO )
         IF( INFO.NE.0 )
     $      WRITE( NOUT, FMT = 9980 )'AB_DCHKSB', INFO
*
      ELSE IF( AB_AB_LSAMEN( 3, C3, 'DBB' ) ) THEN
*
*        ------------------------------
*        DBB:  General Band Reduction
*        ------------------------------
*
         MAXTYP = 15
         NTYPES = MIN( MAXTYP, NTYPES )
         CALL AB_ALAREQ( C3, NTYPES, DOTYPE, MAXTYP, NIN, NOUT )
         DO 370 I = 1, NPARMS
            NRHS = NSVAL( I )
*
            IF( NEWSD.EQ.0 ) THEN
               DO 360 K = 1, 4
                  ISEED( K ) = IOLDSD( K )
  360          CONTINUE
            END IF
            WRITE( NOUT, FMT = 9966 )C3, NRHS
            CALL AB_DCHKBB( NN, MVAL, NVAL, NK, KVAL, MAXTYP, DOTYPE, NR
     $HS,
     $                   ISEED, THRESH, NOUT, A( 1, 1 ), NMAX,
     $                   A( 1, 2 ), 2*NMAX, D( 1, 1 ), D( 1, 2 ),
     $                   A( 1, 4 ), NMAX, A( 1, 5 ), NMAX, A( 1, 6 ),
     $                   NMAX, A( 1, 7 ), WORK, LWORK, RESULT, INFO )
            IF( INFO.NE.0 )
     $         WRITE( NOUT, FMT = 9980 )'AB_DCHKBB', INFO
  370    CONTINUE
*
      ELSE IF( AB_AB_LSAMEN( 3, C3, 'GLM' ) ) THEN
*
*        -----------------------------------------
*        GLM:  Generalized Linear Regression Model
*        -----------------------------------------
*
         CALL AB_XLAENV( 1, 1 )
         IF( TSTERR )
     $      CALL AB_DERRGG( 'GLM', NOUT )
         CALL AB_DCKGLM( NN, MVAL, PVAL, NVAL, NTYPES, ISEED, THRESH, NM
     $AX,
     $                A( 1, 1 ), A( 1, 2 ), B( 1, 1 ), B( 1, 2 ), X,
     $                WORK, D( 1, 1 ), NIN, NOUT, INFO )
         IF( INFO.NE.0 )
     $      WRITE( NOUT, FMT = 9980 )'AB_DCKGLM', INFO
*
      ELSE IF( AB_AB_LSAMEN( 3, C3, 'GQR' ) ) THEN
*
*        ------------------------------------------
*        GQR:  Generalized QR and RQ factorizations
*        ------------------------------------------
*
         CALL AB_XLAENV( 1, 1 )
         IF( TSTERR )
     $      CALL AB_DERRGG( 'GQR', NOUT )
         CALL AB_DCKGQR( NN, MVAL, NN, PVAL, NN, NVAL, NTYPES, ISEED,
     $                THRESH, NMAX, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                A( 1, 4 ), TAUA, B( 1, 1 ), B( 1, 2 ), B( 1, 3 ),
     $                B( 1, 4 ), B( 1, 5 ), TAUB, WORK, D( 1, 1 ), NIN,
     $                NOUT, INFO )
         IF( INFO.NE.0 )
     $      WRITE( NOUT, FMT = 9980 )'AB_DCKGQR', INFO
*
      ELSE IF( AB_AB_LSAMEN( 3, C3, 'GSV' ) ) THEN
*
*        ----------------------------------------------
*        GSV:  Generalized Singular Value Decomposition
*        ----------------------------------------------
*
         CALL AB_XLAENV(1,1)
         IF( TSTERR )
     $      CALL AB_DERRGG( 'GSV', NOUT )
         CALL AB_DCKGSV( NN, MVAL, PVAL, NVAL, NTYPES, ISEED, THRESH, NM
     $AX,
     $                A( 1, 1 ), A( 1, 2 ), B( 1, 1 ), B( 1, 2 ),
     $                A( 1, 3 ), B( 1, 3 ), A( 1, 4 ), TAUA, TAUB,
     $                B( 1, 4 ), IWORK, WORK, D( 1, 1 ), NIN, NOUT,
     $                INFO )
         IF( INFO.NE.0 )
     $      WRITE( NOUT, FMT = 9980 )'AB_DCKGSV', INFO
*
      ELSE IF( AB_AB_LSAMEN( 3, C3, 'CSD' ) ) THEN
*
*        ----------------------------------------------
*        CSD:  CS Decomposition
*        ----------------------------------------------
*
         CALL AB_XLAENV(1,1)
         IF( TSTERR )
     $      CALL AB_DERRGG( 'CSD', NOUT )
         CALL AB_DCKCSD( NN, MVAL, PVAL, NVAL, NTYPES, ISEED, THRESH, NM
     $AX,
     $                A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), A( 1, 4 ),
     $                A( 1, 5 ), A( 1, 6 ), A( 1, 7 ), IWORK, WORK,
     $                D( 1, 1 ), NIN, NOUT, INFO )
         IF( INFO.NE.0 )
     $      WRITE( NOUT, FMT = 9980 )'AB_DCKCSD', INFO
*
      ELSE IF( AB_AB_LSAMEN( 3, C3, 'AB_LSE' ) ) THEN
*
*        --------------------------------------
*        AB_LSE:  Constrained Linear Least Squares
*        --------------------------------------
*
         CALL AB_XLAENV( 1, 1 )
         IF( TSTERR )
     $      CALL AB_DERRGG( 'AB_LSE', NOUT )
         CALL AB_DCKAB_LSE( NN, MVAL, PVAL, NVAL, NTYPES, ISEED, THRESH,
     $ NMAX,
     $                A( 1, 1 ), A( 1, 2 ), B( 1, 1 ), B( 1, 2 ), X,
     $                WORK, D( 1, 1 ), NIN, NOUT, INFO )
         IF( INFO.NE.0 )
     $      WRITE( NOUT, FMT = 9980 )'AB_DCKAB_LSE', INFO
*
      ELSE
         WRITE( NOUT, FMT = * )
         WRITE( NOUT, FMT = * )
         WRITE( NOUT, FMT = 9992 )C3
      END IF
      IF( .NOT.( DGX .OR. DXV ) )
     $   GO TO 190
  380 CONTINUE
      WRITE( NOUT, FMT = 9994 )
      S2 = AB_DSECND( )
      WRITE( NOUT, FMT = 9993 )S2 - S1
*
 9999 FORMAT( / ' Execution not attempted due to input errors' )
 9997 FORMAT( / / 1X, A3, ':  NB =', I4, ', NBMIN =', I4, ', NX =', I4 )
 9996 FORMAT( / / 1X, A3, ':  NB =', I4, ', NBMIN =', I4, ', NS =', I4,
     $      ', MAXB =', I4, ', IACC22 =', I4, ', NBCOL =', I4 )
 9995 FORMAT( / / 1X, A3, ':  NB =', I4, ', NBMIN =', I4, ', NX =', I4,
     $      ', NRHS =', I4 )
 9994 FORMAT( / / ' End of tests' )
 9993 FORMAT( ' Total time used = ', F12.2, ' AB_SECONDs', / )
 9992 FORMAT( 1X, A3, ':  Unrecognized path name' )
 9991 FORMAT( / / ' *** Invalid integer value in column ', I2,
     $      ' of input', ' line:', / A79 )
 9990 FORMAT( / / 1X, A3, ' routines were not tested' )
 9989 FORMAT( ' Invalid input value: ', A, '=', I6, '; must be >=',
     $      I6 )
 9988 FORMAT( ' Invalid input value: ', A, '=', I6, '; must be <=',
     $      I6 )
 9987 FORMAT( ' Tests of the Nonsymmetric Eigenvalue Problem routines' )
 9986 FORMAT( ' Tests of the Symmetric Eigenvalue Problem routines' )
 9985 FORMAT( ' Tests of the Singular Value Decomposition routines' )
 9984 FORMAT( / ' The following parameter values will be used:' )
 9983 FORMAT( 4X, A, 10I6, / 10X, 10I6 )
 9982 FORMAT( / ' Routines pass computational tests if test ratio is ',
     $      'less than', F8.2, / )
 9981 FORMAT( ' Relative machine ', A, ' is taken to be', D16.6 )
 9980 FORMAT( ' *** Error code from ', A, ' = ', I4 )
 9979 FORMAT( / ' Tests of the Nonsymmetric Eigenvalue Problem Driver',
     $      / '    AB_DGEEV (eigenvalues and eigevectors)' )
 9978 FORMAT( / ' Tests of the Nonsymmetric Eigenvalue Problem Driver',
     $      / '    AB_DGEES (Schur form)' )
 9977 FORMAT( / ' Tests of the Nonsymmetric Eigenvalue Problem Expert',
     $      ' Driver', / '    AB_AB_DGEEVX (eigenvalues, eigenvectors an
     $d',
     $      ' condition numbers)' )
 9976 FORMAT( / ' Tests of the Nonsymmetric Eigenvalue Problem Expert',
     $      ' Driver', / '    AB_AB_DGEESX (Schur form and condition',
     $      ' numbers)' )
 9975 FORMAT( / ' Tests of the Generalized Nonsymmetric Eigenvalue ',
     $      'Problem routines' )
 9974 FORMAT( ' Tests of AB_DSBTRD', / ' (reduction of a symmetric band 
     $',
     $      'matrix to tridiagonal form)' )
 9973 FORMAT( / 1X, 71( '-' ) )
 9972 FORMAT( / ' LAPACK VERSION ', I1, '.', I1, '.', I1 )
 9971 FORMAT( / ' Tests of the Generalized Linear Regression Model ',
     $      'routines' )
 9970 FORMAT( / ' Tests of the Generalized QR and RQ routines' )
 9969 FORMAT( / ' Tests of the Generalized Singular Value',
     $      ' Decomposition routines' )
 9968 FORMAT( / ' Tests of the Linear Least Squares routines' )
 9967 FORMAT( ' Tests of AB_DGBBRD', / ' (reduction of a general band ',
     $      'matrix to real bidiagonal form)' )
 9966 FORMAT( / / 1X, A3, ':  NRHS =', I4 )
 9965 FORMAT( / ' Tests of the Generalized Nonsymmetric Eigenvalue ',
     $      'Problem Expert Driver AB_AB_DGGESX' )
 9964 FORMAT( / ' Tests of the Generalized Nonsymmetric Eigenvalue ',
     $      'Problem Driver AB_DGGES' )
 9963 FORMAT( / ' Tests of the Generalized Nonsymmetric Eigenvalue ',
     $      'Problem Driver AB_DGGEV' )
 9962 FORMAT( / ' Tests of the Generalized Nonsymmetric Eigenvalue ',
     $      'Problem Expert Driver AB_AB_DGGEVX' )
 9961 FORMAT( / / 1X, A3, ':  NB =', I4, ', NBMIN =', I4, ', NX =', I4,
     $      ', INMIN=', I4,
     $      ', INWIN =', I4, ', INIBL =', I4, ', ISHFTS =', I4,
     $      ', IACC22 =', I4)
 9960 FORMAT( / ' Tests of the CS Decomposition routines' )
*
*     End of AB_DCHKEE
*
      END
