*> \brief \b AB_ILATRANS
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_ILATRANS + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_ILATRANS.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_ILATRANS.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_ILATRANS.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       INTEGER FUNCTION AB_ILATRANS( TRANS )
*
*       .. Scalar Arguments ..
*       CHARACTER          TRANS
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> This subroutine translates from a character string specifying a
*> transposition operation to the relevant BLAST-specified integer
*> constant.
*>
*> AB_ILATRANS returns an INTEGER.  If AB_ILATRANS < 0, then the input is not
*> a character indicating a transposition operator.  Otherwise AB_ILATRANS
*> returns the constant value corresponding to TRANS.
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
      INTEGER FUNCTION AB_ILATRANS( TRANS )
*
*  -- LAPACK computational routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      CHARACTER          TRANS
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER BLAS_NO_TRANS, BLAS_TRANS, BLAS_CONJ_TRANS
      PARAMETER ( BLAS_NO_TRANS = 111, BLAS_TRANS = 112,
     $     BLAS_CONJ_TRANS = 113 )
*     ..
*     .. External Functions ..
      LOGICAL            AB_LSAME
      EXTERNAL           AB_LSAME
*     ..
*     .. Executable Statements ..
      IF( AB_LSAME( TRANS, 'N' ) ) THEN
         AB_ILATRANS = BLAS_NO_TRANS
      ELSE IF( AB_LSAME( TRANS, 'T' ) ) THEN
         AB_ILATRANS = BLAS_TRANS
      ELSE IF( AB_LSAME( TRANS, 'C' ) ) THEN
         AB_ILATRANS = BLAS_CONJ_TRANS
      ELSE
         AB_ILATRANS = -1
      END IF
      RETURN
*
*     End of AB_ILATRANS
*
      END
