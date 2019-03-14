*> \brief \b AB_SLAMCHTST
*
*  =========== DOCUMENTATION ===========
*
* Online html documentation available at
*            http://www.netlib.org/lapack/explore-html/
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
*  =====================================================================      PROGRAM AB_SLAMCHTST
*
*  -- LAPACK test routine (version 3.7.0) --
*  -- LAPACK is a software package provided by Univ. of Tennessee,    --
*  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
*     December 2016
*
* =====================================================================
*
*     .. Local Scalars ..
      REAL               BASE, EMAX, EMIN, EPS, RMAX, RMIN, RND, SFMIN,
     $                   T, PREC
*     ..
*     .. External Functions ..
      REAL               AB_SLAMCH
      EXTERNAL           AB_SLAMCH
*     ..
*     .. Executable Statements ..
*
      EPS   = AB_SLAMCH( 'Epsilon' )
      SFMIN = AB_SLAMCH( 'Safe minimum' )
      BASE  = AB_SLAMCH( 'Base' )
      PREC  = AB_SLAMCH( 'Precision' )
      T     = AB_SLAMCH( 'Number of digits in mantissa' )
      RND   = AB_SLAMCH( 'Rounding mode' )
      EMIN  = AB_SLAMCH( 'Minimum exponent' )
      RMIN  = AB_SLAMCH( 'Underflow threshold' )
      EMAX  = AB_SLAMCH( 'Largest exponent' )
      RMAX  = AB_SLAMCH( 'Overflow threshold' )
*
      WRITE( 6, * )' Epsilon                      = ', EPS
      WRITE( 6, * )' Safe minimum                 = ', SFMIN
      WRITE( 6, * )' Base                         = ', BASE
      WRITE( 6, * )' Precision                    = ', PREC
      WRITE( 6, * )' Number of digits in mantissa = ', T
      WRITE( 6, * )' Rounding mode                = ', RND
      WRITE( 6, * )' Minimum exponent             = ', EMIN
      WRITE( 6, * )' Underflow threshold          = ', RMIN
      WRITE( 6, * )' Largest exponent             = ', EMAX
      WRITE( 6, * )' Overflow threshold           = ', RMAX
      WRITE( 6, * )' Reciprocal of safe minimum   = ', 1 / SFMIN
*
      END
