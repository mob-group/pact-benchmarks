C   OPTIM: A program for optimizing geometries and calculating reaction pathways
C   Copyright (C) 1999-2006 David J. Wales
C   This file is part of OPTIM.
C
C   OPTIM is free software; you can redistribute it and/or modify
C   it under the terms of the GNU General Public License as published by
C   the Free Software Foundation; either version 2 of the License, or
C   (at your option) any later version.
C
C   OPTIM is distributed in the hope that it will be useful,
C   but WITHOUT ANY WARRANTY; without even the implied warranty of
C   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C   GNU General Public License for more details.
C
C   You should have received a copy of the GNU General Public License
C   along with this program; if not, write to the Free Software
C   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
C
C
C********************************************************************
C
      SUBROUTINE EIG(A,B,L,N,N1)
      IMPLICIT NONE
C
C DIAGONALIZATION BY THE JACOBI METHOD.
C A - MATRIX TO BE DIAGONALIZED (eigenvalues returned in diagonal
C        elements of A).  If you want to save A, you must do this before
C        calling EIG.  Set N to the same value as L.
C B - EIGENVECTORS
C L - DIMENSION OF A AND B
C N - SIZE OF SUBMATRIX USED
C N1 - A FLAG INDICATING WHETHER THE EIGENVECTORS AND
C      EIGENVALUES ARE TO BE REORDERED.
C
CSW      1
      INTEGER MU, MM, J, II, JI, L, N, N1, ONE, IOFF, I, IM1, JJ
      DOUBLE PRECISION A(L,L),B(L,L)
      DOUBLE PRECISION W2, W1, C, T, ALP, ALN, D, SUM, S, DIFF, R, Q, P, ZER, TOL, TOL2
      DATA ZER/0.D00/,ONE/1.D00/
CSW     2
      TOL=1.D-14
      TOL2=1.D-10
      JJ=0
      IOFF=0
      B(1,1)=ONE
      IF(N.EQ.1) RETURN
      DO 20 I=2,N
         IM1=I-1
         DO 10 J=1,IM1
            B(I,J)=ZER
            B(J,I)=ZER
10       CONTINUE
         B(I,I)=ONE
20    CONTINUE
C
C FIRST SEE IF MATRIX IS ALREADY DIAGONAL- IF SO THEN
C  TAKE APPROPRIATE ACTION
C
      DO II=1,L
         DO JI=II+1,L
           IF(DABS(A(II,JI)).GT.TOL2)IOFF=IOFF+1
           IF(DABS(A(JI,II)).GT.TOL2)IOFF=IOFF+1
         ENDDO
      ENDDO
      IF(IOFF.EQ.0)THEN
          B(1:L,1:L)=0.0D0
          DO 40 I=1,L
          B(I,I)=ONE
40        CONTINUE
      ELSE
50    P=ZER
      DO 70 I=2,N
      IM1=I-1
      DO 60 J=1,IM1
      Q=A(I,J)
      IF(P.GE. ABS(Q)) GO TO 60
      P= ABS(Q)
      II=I
      JJ=J
60    CONTINUE
70    CONTINUE
      IF(P.EQ.0.) GO TO 140
      P=A(II,II)
      Q=A(II,JJ)
      R=A(JJ,JJ)
      DIFF=0.5D0*(P-R)
      IF( ABS(DIFF).LT. ABS(Q)) GO TO 80
      IF( ABS(Q/DIFF).GT.TOL) GO TO 80
      A(II,JJ)=ZER
      A(JJ,II)=ZER
      GO TO 50
80    S=SQRT(0.250*(P-R)**2+Q**2)
      SUM=0.5D0*(P+R)
      D=R*P-Q**2
      IF(SUM.GT.ZER) GO TO 90
      ALN=SUM-S
      ALP=D/ALN
      GO TO 100
90    ALP=SUM+S
      ALN=D/ALP
100   IF(DIFF.GT.ZER) GO TO 110
      T=Q/(DIFF-S)
      A(II,II)=ALN
      A(JJ,JJ)=ALP
      GO TO 120
110   T=Q/(DIFF+S)
      A(II,II)=ALP
      A(JJ,JJ)=ALN
120   C=1.0/SQRT(1.0+T**2)
      S=T*C
      A(II,JJ)=ZER
      A(JJ,II)=ZER
      DO 130 I=1,N
      P=B(I,II)
      Q=B(I,JJ)
      B(I,II)=C*P+S*Q
      B(I,JJ)=C*Q-S*P
      IF(I.EQ.II.OR.I.EQ.JJ) GO TO 130
      P=A(I,II)
      Q=A(I,JJ)
      R=C*P+S*Q
       A(I,II)=R
      A(II,I)=R
      R=Q*C-P*S
      A(I,JJ)=R
      A(JJ,I)=R
130   CONTINUE
      GO TO 50
      ENDIF
140   IF(N1.EQ.1) RETURN
      MM=N-1
      DO I=1,MM
      II=I+1
      DO J=II,N
      IF(A(I,I)-A(J,J)) 170,150,150
150   W1=A(I,I)
      A(I,I)=A(J,J)
      A(J,J)=W1
      DO MU=1,N
         W2=B(MU,I)
         B(MU,I)=B(MU,J)
         B(MU,J)=W2
      ENDDO
170   CONTINUE
      ENDDO
      ENDDO
      RETURN
      END
