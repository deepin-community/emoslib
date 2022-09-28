C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION UNPKCF( PARRAY, KTRUNC, KSUBSET, KGRIB, KLENG,
     X                          KNSPT, KBITS)
C
C---->
C**** UNPKCF
C
C     Purpose.
C     --------
C
C     Unpacks scaled values from a GRIB product to an array of spherical
C     harmonic coefficients, the first KSUBSET triangle of coefficients
C
C**   Interface.
C     ----------
C
C     IRET = UNPKCF(PARRAY,KTRUNC,KSUBSET,KGRIB,KLENG,KNSPT,KBITS)
C
C
C     Input Parameters.
C     -----------------
C
C     KTRUNC     - Truncation of the spherical harmonic coefficients.
C     KSUBSET    - Number indicating truncation of subset.
C     KGRIB      - Array containing generated GRIB product.
C     KLENG      - Length of KGRIB
C     KNSPT      - Bit pointer for next free position in KGRIB.
C
C
C     Output Parameters.
C     ------------------
C
C     PARRAY     - Array of spherical harmonic coefficients.
C     KNSPT      - Bit pointer for next free position in KGRIB(updated).
C     KBITS      - Number of bits per computer word.
C
C     Returns 0 if OK, otherwise prints an error message and returns
C     an error code.
C
C
C     Method.
C     -------
C                           <KSUBSET>|
C
C     Given numbers:         cccccccc|cccccc
C                              cccccc|cccccc
C                                cccc|cccccc
C                                  cc|cccccc
C                                    |cccccc
C                                       cccc
C                                         cc
C
C      Converts and moves :  cccccccc
C                              cccccc
C                                cccc
C                                  cc
C
C     Externals.
C     ----------
C
C     DECFP2  - Convert to floating point value from storage format
#if (defined CRAY) || (defined __uxp__ ) || (defined IBM_POWER4)
C     GSBITE  - Extract bit pattern (Vectorized Fortran).
#endif
C     GBYTES  - Extract bit pattern
C
C
C
C     Reference.
C     ----------
C
C     None.
C
C
C     Comments.
C     ---------
C
C     KSUBSET must be less than or equal to KTRUNC.
C
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      13:04:94
C
C
C     Modifications.
C     --------------
C
C     None.
C
C
C----<
C     ----------------------------------------------------------------
C*    Section 0 . Definition of variables. Data statements.
C     ----------------------------------------------------------------
C
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Parameters
      INTEGER JPMAXT
      PARAMETER ( JPMAXT = 2047 + 1 )
C
C     Subroutine arguments
C
      INTEGER KTRUNC, KSUBSET, KNSPT, KGRIB, KLENG, KBITS
C
      REAL PARRAY
      DIMENSION PARRAY(*), KGRIB(KLENG)
C
C     Local variables.
      INTEGER JCOL, JROW, INDEX, ILAST, ITRND, IEXP, IMANT
      INTEGER INSPT, INEXT, IWORD, IOFF
      DIMENSION IEXP(JPMAXT*2), IMANT(JPMAXT*2)
C
#if (defined CRAY) || (defined __uxp__ ) || (defined IBM_POWER4)
C
C     Values in IMASK are set in the first call to routine GSBITE, and
C     are used in subsequent calls.
C
      INTEGER IMASK
      DIMENSION IMASK(65)
      SAVE IMASK
C
C     Force routine GSBITE to calculate bit-masks first time through.
      DATA IMASK(2) /0/
#endif
C
C     ----------------------------------------------------------------
C*    Section 1 . Check for valid truncation and start values.
C     ----------------------------------------------------------------
C
  100 CONTINUE
C
C     Check for valid truncation and start values
C
      UNPKCF = 0
      IF ( KSUBSET .GT. KTRUNC ) THEN
        WRITE(GRPRSM,*) ' UNPKCF - Invalid start/truncation given'
        WRITE(GRPRSM,*) ' UNPKCF - KSUBSET, KTRUNC =', KSUBSET, KTRUNC
        UNPKCF = 16500
        GOTO 900
      ENDIF
C
C     ----------------------------------------------------------------
C*    Section 2. Convert values from IBM format exponent and mantissa.
C                Handle values 2 at a time (real and imaginary parts)
C     ----------------------------------------------------------------
C
  200 CONTINUE
C
      ITRND = 1
      ILAST = KSUBSET+1
      INDEX = -1
C
C     Extract values in pairs and convert to floating point
C
C     Loop through rows in the subset
      DO 590 JROW = 1 , ILAST
        INEXT = 2 * (ILAST - JROW + 1)
C
C     ----------------------------------------------------------------
C*    Section 3. Get exponents from bit array.
C     ----------------------------------------------------------------
C
  300   CONTINUE
C
C*      Calculate word pointer and offset.
        INSPT = KNSPT
        IWORD = INSPT / KBITS
        IOFF  = INSPT - IWORD * KBITS
        IWORD = IWORD + 1
C
#if (defined CRAY) || (defined __uxp__ ) || (defined IBM_POWER4)
        IF (INEXT.GE.8) THEN
C
C         Vectorising routine GSBITE.
          CALL GSBITE( KGRIB(IWORD), IEXP, IOFF, 8, 24, INEXT, KBITS,
     X                 IMASK, 'D')
        ELSE
C
C         Scalar faster.
          CALL GBYTES( KGRIB(IWORD), IEXP, IOFF, 8, 24, INEXT)
        ENDIF
#elif (defined CYBER)
        CALL GBYTES6( KGRIB(IWORD), IEXP, IOFF, 8, 24, INEXT)
#else
        CALL GBYTES( KGRIB(IWORD), IEXP, IOFF, 8, 24, INEXT)
#endif
C
C
C     ----------------------------------------------------------------
C*    Section 4. Get mantissae from bit array.
C     ----------------------------------------------------------------
C
  400   CONTINUE
C
C*      Calculate word pointer and offset.
        INSPT = KNSPT + 8
        IWORD = INSPT / KBITS
        IOFF  = INSPT - IWORD * KBITS
        IWORD = IWORD + 1
C
#if (defined CRAY) || (defined __uxp__ ) || (defined IBM_POWER4)
        IF (INEXT.GE.8) THEN
C
C         Vectorising routine GSBITE.
          CALL GSBITE( KGRIB(IWORD), IMANT, IOFF, 24, 8, INEXT, KBITS,
     X                 IMASK, 'D')
        ELSE
C
C         Scalar faster.
          CALL GBYTES( KGRIB(IWORD), IMANT, IOFF, 24, 8, INEXT)
        ENDIF
#elif (defined CYBER)
        CALL GBYTES6( KGRIB(IWORD), IMANT, IOFF, 24, 8, INEXT)
#else
        CALL GBYTES( KGRIB(IWORD), IMANT, IOFF, 24, 8, INEXT)
#endif
C
C     ----------------------------------------------------------------
C*    Section 5. Convert values from IBM format exponent and mantissa.
C     ----------------------------------------------------------------
C
  500 CONTINUE
C
C       Convert columns in subset
        INEXT = 0
        DO 510 JCOL = JROW , KTRUNC+1
          INDEX = INDEX + 2
          IF ( ILAST .GE. JCOL ) THEN
C
C           Real part ..
            INEXT = INEXT + 1
            CALL DECFP2 (PARRAY(INDEX),IEXP(INEXT),IMANT(INEXT))
C
C           Imaginary part ..
            INEXT = INEXT + 1
            CALL DECFP2 (PARRAY(INDEX+1),IEXP(INEXT),IMANT(INEXT))
          ENDIF
  510   CONTINUE
C
C       Update pointer.
        KNSPT = KNSPT + INEXT*32
C
  590 CONTINUE
C
C     ----------------------------------------------------------------
C*    Section 9. Return to calling routine.
C     ----------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
C
      END
