*> \brief \b AB_SECOND Using the INTERNAL function ETIME.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*      REAL FUNCTION AB_SECOND( )
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>  AB_SECOND returns the user time for a process in AB_SECONDs.
*>  This version gets the time from the INTERNAL function ETIME.
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
*> \date December 2016
*
*> \ingroup auxOTHERauxiliary
*
*  =====================================================================
      REAL FUNCTION AB_SECOND( )
*
*  -- LAPACK auxiliary routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
* =====================================================================
*
*     .. Local Scalars ..
      REAL               T1
*     ..
*     .. Local Arrays ..
      REAL               TARRAY( 2 )
*     ..
*     .. Intrinsic Functions ..
      REAL               ETIME
      INTRINSIC          ETIME
*     ..
*     .. Executable Statements ..
*
      T1 = ETIME( TARRAY )
      AB_SECOND = TARRAY( 1 )
      RETURN
*
*     End of AB_SECOND
*
      END
