*> \brief \b AB_CLADIV performs complex division in real arithmetic, avoiding unnecessary overflow.
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*> \htmlonly
*> Download AB_CLADIV + dependencies
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.tgz?format=tgz&filename=/lapack/lapack_routine/AB_CLADIV.f">
*> [TGZ]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.zip?format=zip&filename=/lapack/lapack_routine/AB_CLADIV.f">
*> [ZIP]</a>
*> <a href="http://www.netlib.org/cgi-bin/netlibfiles.txt?format=txt&filename=/lapack/lapack_routine/AB_CLADIV.f">
*> [TXT]</a>
*> \endhtmlonly
*
*  Definition:
*  ===========
*
*       COMPLEX FUNCTION AB_CLADIV( X, Y )
*
*       .. Scalar Arguments ..
*       COMPLEX            X, Y
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*> AB_CLADIV := X / Y, where X and Y are complex.  The computation of X / Y
*> will not overflow on an intermediary step unless the results
*> overflows.
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] X
*> \verbatim
*>          X is COMPLEX
*> \endverbatim
*>
*> \param[in] Y
*> \verbatim
*>          Y is COMPLEX
*>          The complex scalars X and Y.
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
*> \ingroup complexOTHERauxiliary
*
*  =====================================================================
      COMPLEX FUNCTION AB_CLADIV( X, Y )
*
*  -- LAPACK auxiliary routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
*     .. Scalar Arguments ..
      COMPLEX            X, Y
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      REAL               ZI, ZR
*     ..
*     .. External Subroutines ..
      EXTERNAL           AB_SLADIV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          AIMAG, CMPLX, REAL
*     ..
*     .. Executable Statements ..
*
      CALL AB_SLADIV( REAL( X ), AIMAG( X ), REAL( Y ), AIMAG( Y ), ZR,
     $             ZI )
      AB_CLADIV = CMPLX( ZR, ZI )
*
      RETURN
*
*     End of AB_CLADIV
*
      END
