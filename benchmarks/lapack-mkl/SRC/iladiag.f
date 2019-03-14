*> \brief \b AB_ILADIAG
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_ILADIAG + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_ILADIAG.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_ILADIAG.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_ILADIAG.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       INTEGER FUNCTION AB_ILADIAG( DIAG )
*
*       .. Scalar Arguments ..
*       CHARACTER          DIAG
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> This subroutine translated from a character string specifying if a
*> matrix has unit diagonal or not to the relevant BLAST-specified
*> integer constant.
*>
*> AB_ILADIAG returns an INTEGER.  If AB_ILADIAG < 0, then the input is not a
*> character indicating a unit or non-unit diagonal.  Otherwise AB_ILADIAG
*> returns the constant value corresponding to DIAG.
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
      INTEGER FUNCTION AB_ILADIAG( DIAG )
*
*  -- LAPACK computational routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          DIAG
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER BLAS_NON_UNIT_DIAG, BLAS_UNIT_DIAG
      PARAMETER ( BLAS_NON_UNIT_DIAG = 131, BLAS_UNIT_DIAG = 132 )
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      EXTERNAL           AB_LSAME
*     ..
*     .. Executable Statements ..
      IF( AB_LSAME( DIAG, 'N' ) ) THEN
         AB_ILADIAG = BLAS_NON_UNIT_DIAG
      ELSE IF( AB_LSAME( DIAG, 'U' ) ) THEN
         AB_ILADIAG = BLAS_UNIT_DIAG
      ELSE
         AB_ILADIAG = -1
      END IF
      RETURN
*
*     End of AB_ILADIAG
*
      END
