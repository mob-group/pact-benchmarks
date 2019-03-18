*> \brief \b AB_SECOND returns nothing
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
*>  AB_SECOND returns nothing instead of returning the user time for a process in AB_SECONDs.
*>  If you are using that routine, it means that neither EXTERNAL ETIME,
*>  EXTERNAL ETIME_, INTERNAL ETIME, INTERNAL CPU_TIME is available  on
*>  your machine.
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
      AB_SECOND = 0.0E+0
      RETURN
*
*     End of AB_SECOND
*
      END
