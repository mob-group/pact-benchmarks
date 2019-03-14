*> \brief \b AB_ILAPREC
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_ILAPREC + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_ILAPREC.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_ILAPREC.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_ILAPREC.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       INTEGER FUNCTION AB_ILAPREC( PREC )
*
*       .. Scalar Arguments ..
*       CHARACTER          PREC
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> This subroutine translated from a character string specifying an
*> intermediate precision to the relevant BLAST-specified integer
*> constant.
*>
*> AB_ILAPREC returns an INTEGER.  If AB_ILAPREC < 0, then the input is not a
*> character indicating a supported intermediate precision.  Otherwise
*> AB_ILAPREC returns the constant value corresponding to PREC.
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
*> \date December 2016
*
*> \ingroup auxOTHERcomputational
*
*  =====================================================================
      INTEGER FUNCTION AB_ILAPREC( PREC )
*
*  -- LAPACK computational routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          PREC
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER BLAS_PREC_SINGLE, BLAS_PREC_DOUBLE, BLAS_PREC_INDIGENOUS,
     $           BLAS_PREC_EXTRA
      PARAMETER ( BLAS_PREC_SINGLE = 211, BLAS_PREC_DOUBLE = 212,
     $     BLAS_PREC_INDIGENOUS = 213, BLAS_PREC_EXTRA = 214 )
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      EXTERNAL           AB_LSAME
*     ..
*     .. Executable Statements ..
      IF( AB_LSAME( PREC, 'S' ) ) THEN
         AB_ILAPREC = BLAS_PREC_SINGLE
      ELSE IF( AB_LSAME( PREC, 'D' ) ) THEN
         AB_ILAPREC = BLAS_PREC_DOUBLE
      ELSE IF( AB_LSAME( PREC, 'I' ) ) THEN
         AB_ILAPREC = BLAS_PREC_INDIGENOUS
      ELSE IF( AB_LSAME( PREC, 'X' ) .OR. AB_LSAME( PREC, 'E' ) ) THE
     $N
         AB_ILAPREC = BLAS_PREC_EXTRA
      ELSE
         AB_ILAPREC = -1
      END IF
      RETURN
*
*     End of AB_ILAPREC
*
      END
