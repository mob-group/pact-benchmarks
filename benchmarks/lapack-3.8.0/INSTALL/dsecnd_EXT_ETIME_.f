*> \brief \b AB_DSECND  Using ETIME_
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*      DOUBLE PRECISION FUNCTION AB_DSECND( )
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>  AB_DSECND returns the user time for a process in AB_SECONDs.
*>  This version gets the time from the system function ETIME_.
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
      DOUBLE PRECISION FUNCTION AB_DSECND( )
*
*  -- LAPACK auxiliary routine (version 3.7.0) --
*     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
*     February 2007
* =====================================================================
*
*     .. Local Scalars ..
      REAL               T1
*     ..
*     .. Local Arrays ..
      REAL               TARRAY( 2 )
*     ..
*     .. External Functions ..
      REAL               ETIME_
      EXTERNAL           ETIME_
*     ..
*     .. Executable Statements ..
*
      T1 = ETIME_( TARRAY )
      AB_DSECND = TARRAY( 1 )
      RETURN
*
*     End of AB_DSECND
*
      END
