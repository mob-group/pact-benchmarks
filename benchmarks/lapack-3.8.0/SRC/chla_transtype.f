*> \brief \b AB_CHLA_TRANSTYPE
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_CHLA_TRANSTYPE + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_CHLA_TRANSTYPE.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_CHLA_TRANSTYPE.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_CHLA_TRANSTYPE.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       CHARACTER*1 FUNCTION AB_CHLA_TRANSTYPE( TRANS )
*
*       .. Scalar Arguments ..
*       INTEGER            TRANS
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> This subroutine translates from a BLAST-specified integer constant to
*> the character string specifying a transposition operation.
*>
*> AB_CHLA_TRANSTYPE returns an CHARACTER*1.  If AB_CHLA_TRANSTYPE is 'X',
*> then input is not an integer indicating a transposition operator.
*> Otherwise AB_CHLA_TRANSTYPE returns the constant value corresponding to
*> TRANS.
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
      CHARACTER*1 FUNCTION AB_CHLA_TRANSTYPE( TRANS )
*
*  -- LAPACK computational routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      INTEGER            TRANS
*     ..
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER BLAS_NO_TRANS, BLAS_TRANS, BLAS_CONJ_TRANS
      PARAMETER ( BLAS_NO_TRANS = 111, BLAS_TRANS = 112,
     $     BLAS_CONJ_TRANS = 113 )
*     ..
*     .. Executable Statements ..
      IF( TRANS.EQ.BLAS_NO_TRANS ) THEN
         AB_CHLA_TRANSTYPE = 'N'
      ELSE IF( TRANS.EQ.BLAS_TRANS ) THEN
         AB_CHLA_TRANSTYPE = 'T'
      ELSE IF( TRANS.EQ.BLAS_CONJ_TRANS ) THEN
         AB_CHLA_TRANSTYPE = 'C'
      ELSE
         AB_CHLA_TRANSTYPE = 'X'
      END IF
      RETURN
*
*     End of AB_CHLA_TRANSTYPE
*
      END
