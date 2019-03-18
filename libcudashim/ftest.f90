program hello
  INTEGER(8), PARAMETER :: M = 2, N = 1, K = 3
  INTEGER(8) :: LDA, LDB, LDC
  REAL(8) :: ALPHA, BETA
  REAL(8), DIMENSION(M*K) :: A
  REAL(8), DIMENSION(K*N) :: B
  REAL(8), DIMENSION(M*N) :: C

  INTEGER(8) :: I

  ALPHA = 1.0
  BETA = 0.0

  A = (/ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 /)
  B = (/ 1.0, 2.0, 3.0 /)
  C = (/ 0.0, 0.0 /)

  LDA = M
  LDB = K
  LDC = M

  call dgemm('N', 'N', M, N, K, ALPHA, A, LDA, B, LDB, BETA, C, LDC)

  DO I=1,2
    WRITE (*,*) C(I)
  ENDDO
end program hello
