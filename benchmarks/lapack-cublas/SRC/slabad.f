*> \brief \b AB_SLABAD
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_SLABAD + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_SLABAD.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_SLABAD.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_SLABAD.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       SUBROUTINE AB_SLABAD( SMALL, LARGE )
*
*       .. Scalar Arguments ..
*       REAL               LARGE, SMALL
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_SLABAD takes as input the values computed by AB_SLAMCH for underflow and
*> overflow, and returns the square root of each of these values if the
*> log of LARGE is sufficiently large.  This subroutine is intended to
*> identify machines with a large exponent range, such as the Crays, and
*> redefine the underflow and overflow limits to be the square roots of
*> the values computed by AB_SLAMCH.  This subroutine is needed because
*> AB_SLAMCH does not compensate for poor arithmetic in the upper half of
*> the exponent range, as is found on a Cray.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in,out] SMALL
*> \verbatim
*>          SMALL is REAL
*>          On entry, the underflow threshold as computed by AB_SLAMCH.
*>          On exit, if LOG10(LARGE) is sufficiently large, the square
*>          root of SMALL, otherwise unchanged.
*> \endverbatim
*>
*> \param[in,out] LARGE
*> \verbatim
*>          LARGE is REAL
*>          On entry, the overflow threshold as computed by AB_SLAMCH.
*>          On exit, if LOG10(LARGE) is sufficiently large, the square
*>          root of LARGE, otherwise unchanged.
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
*> \ingroup OTHERauxiliary
*
*  =====================================================================
      SUBROUTINE AB_SLABAD( SMALL, LARGE )
*
*  -- LAPACK auxiliary routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      REAL               LARGE, SMALL
*     ..
*
*  =====================================================================
*
*     .. Intrinsic Functions ..
      INTRINSIC          LOG10, SQRT
*     ..
*     .. Executable Statements ..
*
*     If it looks like we're on a Cray, take the square root of
*     SMALL and LARGE to avoid overflow and underflow problems.
*
      IF( LOG10( LARGE ).GT.2000. ) THEN
         SMALL = SQRT( SMALL )
         LARGE = SQRT( LARGE )
      END IF
*
      RETURN
*
*     End of AB_SLABAD
*
      END
