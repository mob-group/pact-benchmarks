*> \brief \b AB_SLADIV performs complex division in real arithmetic, avoiding unnecessary overflow.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_SLADIV + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_SLADIV.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_SLADIV.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_SLADIV.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SLADIV( A, B, C, D, P, Q )
*
*       .. Scalar Arguments ..
*       REAL               A, B, C, D, P, Q
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_SLADIV performs complex division in  real arithmetic
*>
*>                       a + i*b
*>            p + i*q = ---------
*>                       c + i*d
*>
*> The algorithm is due to Michael Baudin and Robert L. Smith
*> and can be found in the paper
*> "A Robust Complex Division in Scilab"
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] A
*> \verbatim
*>          A is REAL
*> \endverbatim
*>
*> \param[in] B
*> \verbatim
*>          B is REAL
*> \endverbatim
*>
*> \param[in] C
*> \verbatim
*>          C is REAL
*> \endverbatim
*>
*> \param[in] D
*> \verbatim
*>          D is REAL
*>          The scalars a, b, c, and d in the above expression.
*> \endverbatim
*>
*> \param[out] P
*> \verbatim
*>          P is REAL
*> \endverbatim
*>
*> \param[out] Q
*> \verbatim
*>          Q is REAL
*>          The scalars p and q in the above expression.
*> \endverbatim
*
*  Authors:
*  ========
*
*> \author Univ. of Tennessee
*> \author Univ. of California Berkeley
*> \author Univ. of Colorado Denver
*> \author NAG Ltd.
*
*> \date January 2013
*
*> \ingroup realOTHERauxiliary
*
*  =====================================================================
      SUBROUTINE AB_SLADIV( A, B, C, D, P, Q )
*
*  -- LAPACK auxiliary routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     January 2013
*
*     .. Scalar Arguments ..
      REAL               A, B, C, D, P, Q
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               BS
      PARAMETER          ( BS = 2.0E0 )
      REAL               HALF
      PARAMETER          ( HALF = 0.5E0 )
      REAL               TWO
      PARAMETER          ( TWO = 2.0E0 )
*
*     .. Local Scalars ..
      REAL               AA, BB, CC, DD, AB, CD, S, OV, UN, BE, EPS
*     ..
*     .. External Functions ..
      REAL               AB_SLAMCH
      EXTERNAL           AB_SLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_SLADIV1
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, MAX
*     ..
*     .. Executable Statements ..
*
      AA = A
      BB = B
      CC = C
      DD = D
      AB = MAX( ABS(A), ABS(B) )
      CD = MAX( ABS(C), ABS(D) )
      S = 1.0E0

      OV = AB_SLAMCH( 'Overflow threshold' )
      UN = AB_SLAMCH( 'Safe minimum' )
      EPS = AB_SLAMCH( 'Epsilon' )
      BE = BS / (EPS*EPS)

      IF( AB >= HALF*OV ) THEN
         AA = HALF * AA
         BB = HALF * BB
         S  = TWO * S
      END IF
      IF( CD >= HALF*OV ) THEN
         CC = HALF * CC
         DD = HALF * DD
         S  = HALF * S
      END IF
      IF( AB <= UN*BS/EPS ) THEN
         AA = AA * BE
         BB = BB * BE
         S  = S / BE
      END IF
      IF( CD <= UN*BS/EPS ) THEN
         CC = CC * BE
         DD = DD * BE
         S  = S * BE
      END IF
      IF( ABS( D ).LE.ABS( C ) ) THEN
         CALL AB_SLADIV1(AA, BB, CC, DD, P, Q)
      ELSE
         CALL AB_SLADIV1(BB, AA, DD, CC, P, Q)
         Q = -Q
      END IF
      P = P * S
      Q = Q * S
*
      RETURN
*
*     End of AB_SLADIV
*
      END

*> \ingroup realOTHERauxiliary


      SUBROUTINE AB_SLADIV1( A, B, C, D, P, Q )
*
*  -- LAPACK auxiliary routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     January 2013
*
*     .. Scalar Arguments ..
      REAL               A, B, C, D, P, Q
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE
      PARAMETER          ( ONE = 1.0E0 )
*
*     .. Local Scalars ..
      REAL               R, T
*     ..
*     .. External Functions ..
      REAL               AB_SLADIV2
      EXTERNAL           AB_SLADIV2
*     ..
*     .. Executable Statements ..
*
      R = D / C
      T = ONE / (C + D * R)
      P = AB_SLADIV2(A, B, C, D, R, T)
      A = -A
      Q = AB_SLADIV2(B, A, C, D, R, T)
*
      RETURN
*
*     End of AB_SLADIV1
*
      END

*> \ingroup realOTHERauxiliary

      REAL FUNCTION AB_SLADIV2( A, B, C, D, R, T )
*
*  -- LAPACK auxiliary routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     January 2013
*
*     .. Scalar Arguments ..
      REAL               A, B, C, D, R, T
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E0 )
*
*     .. Local Scalars ..
      REAL               BR
*     ..
*     .. Executable Statements ..
*
      IF( R.NE.ZERO ) THEN
         BR = B * R
         if( BR.NE.ZERO ) THEN
            AB_SLADIV2 = (A + BR) * T
         ELSE
            AB_SLADIV2 = A * T + (B * T) * R
         END IF
      ELSE
         AB_SLADIV2 = (A + D * (B / C)) * T
      END IF
*
      RETURN
*
*     End of AB_SLADIV
*
      END
