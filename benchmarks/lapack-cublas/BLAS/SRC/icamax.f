*> \brief \b AB_ICAMAX
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
*
*  Definition:
*  ===========
*
*       INTEGER FUNCTION AB_ICAMAX(N,CX,INCX)
*
*       .. Scalar Arguments ..
*       INTEGER INCX,N
*       ..
*       .. Array Arguments ..
*       COMPLEX CX(*)
*       ..
*
*
*> \par Purpose:
*  =============
*>
*> \verbatim
*>
*>    AB_ICAMAX finds the index of the first element having maximum |Re(.)| + |Im(.)|
*> \endverbatim
*
*  Arguments:
*  ==========
*
*> \param[in] N
*> \verbatim
*>          N is INTEGER
*>         number of elements in input vector(s)
*> \endverbatim
*>
*> \param[in] CX
*> \verbatim
*>          CX is COMPLEX array, dimension ( 1 + ( N - 1 )*abs( INCX ) )
*> \endverbatim
*>
*> \param[in] INCX
*> \verbatim
*>          INCX is INTEGER
*>         storage spacing between elements of SX
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
*> \date November 2017
*
*> \ingroup aux_blas
*
*> \par Further Details:
*  =====================
*>
*> \verbatim
*>
*>     jack dongarra, linpack, 3/11/78.
*>     modified 3/93 to return if incx .le. 0.
*>     modified 12/3/93, array(1) declarations changed to array(*)
*> \endverbatim
*>
*  =====================================================================
      INTEGER FUNCTION AB_ICAMAX(N,CX,INCX)
*
*  -- Reference BLAS level1 routine (version 3.8.0) --
*  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     November 2017
*
*     .. Scalar Arguments ..
      INTEGER INCX,N
*     ..
*     .. Array Arguments ..
      COMPLEX CX(*)
*     ..
*
*  =====================================================================
*
*     .. Local Scalars ..
      REAL SMAX
      INTEGER I,IX
*     ..
*     .. External Functions ..
      REAL AB_SCABS1
      EXTERNAL AB_SCABS1
*     ..
      AB_ICAMAX = 0
      IF (N.LT.1 .OR. INCX.LE.0) RETURN
      AB_ICAMAX = 1
      IF (N.EQ.1) RETURN
      IF (INCX.EQ.1) THEN
*
*        code for increment equal to 1
*
         SMAX = AB_SCABS1(CX(1))
         DO I = 2,N
            IF (AB_SCABS1(CX(I)).GT.SMAX) THEN
               AB_ICAMAX = I
               SMAX = AB_SCABS1(CX(I))
            END IF
         END DO
      ELSE
*
*        code for increment not equal to 1
*
         IX = 1
         SMAX = AB_SCABS1(CX(1))
         IX = IX + INCX
         DO I = 2,N
            IF (AB_SCABS1(CX(IX)).GT.SMAX) THEN
               AB_ICAMAX = I
               SMAX = AB_SCABS1(CX(IX))
            END IF
            IX = IX + INCX
         END DO
      END IF
      RETURN
      END
