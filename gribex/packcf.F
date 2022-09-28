C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION PACKCF( PARRAY, KTRUNC, KSUBSET, KGRIB, KLENG,
     X                          KNSPT, KBITS)
C
C---->
C**** PACKCF
C
C     Purpose.
C     --------
C
C     Moves the unscaled values from an array of spherical harmonic
C     coefficients, just the first "KSUBSET" triangle of coefficients.
C
C**   Interface.
C     ----------
C
C     IRET = PACKCF(PARRAY,KTRUNC,KSUBSET,KGRIB,KLENG,KNSPT,KBITS)
C
C
C     Input Parameters.
C     -----------------
C
C     PARRAY     - Array of spherical harmonic coefficients.
C     KTRUNC     - Truncation of the spherical harmonic coefficients.
C     KSUBSET    - Number indicating truncation of subset to move.
C     KNSPT      - Bit pointer for next free position in KGRIB.
C
C
C     Output Parameters.
C     ------------------
C
C     KGRIB      - Array containing generated GRIB product.
C     KLENG      - Length of KGRIB
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
C     CONFP3  - Convert floating point value to storage format
#if (defined CRAY) || (defined __uxp__ ) || (defined IBM_POWER4)
C     GSBITE  - Store bit pattern (Vectorized Fortran).
#endif
C     SBYTES  - Store bit pattern
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
      INTEGER JCOL, JROW, INDEX, ILAST, ITRND, IEXP, IMANT, INEXT
      INTEGER INSPT, INEED, IWORD, IOFF
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
C     Check for valid truncation and subset values
C
      PACKCF = 0
      IF ( KSUBSET .GT. KTRUNC ) THEN
        WRITE(GRPRSM,*) 'PACKCF: Invalid subset/truncation given'
        WRITE(GRPRSM,*) 'PACKCF: KSUBSET, KTRUNC =', KSUBSET, KTRUNC
        PACKCF = 16400
        GOTO 900
      ENDIF
C
C     Check that the GRIB area is big enough.
      INEED = ( KNSPT + (KSUBSET+1)*(KSUBSET+2)*32 ) / KBITS
      IF (INEED.GT.KLENG) THEN
        PACKCF = 16401
        WRITE(GRPRSM,*) 'PACKCF: GRIB area is not big enough.'
        GOTO 900
      ENDIF
C
C     ----------------------------------------------------------------
C*    Section 2. Convert values to IBM format exponent and mantissa.
C                Handle values 2 at a time (real and imaginary parts)
C     ----------------------------------------------------------------
C
  200 CONTINUE
C
      ITRND = 1
      ILAST = KSUBSET+1
      INDEX = -1
C
C     Loop through rows in the subset
      DO 490 JROW = 1 , ILAST
C
C       Pick up columns in subset
        INEXT = 0
        DO 210 JCOL = JROW , KTRUNC+1
          INDEX = INDEX + 2
          IF ( ILAST .GE. JCOL ) THEN
C
C           Convert floating point nos. to GRIB and insert fields.
C           Real part ..
            INEXT = INEXT + 1
            CALL CONFP3( PARRAY(INDEX), IEXP(INEXT), IMANT(INEXT),
     X                   KBITS, ITRND)
C
C           Imaginary part ..
            INEXT = INEXT + 1
            CALL CONFP3( PARRAY(INDEX+1), IEXP(INEXT), IMANT(INEXT),
     X                   KBITS, ITRND)
          ENDIF
  210   CONTINUE
C
C     ----------------------------------------------------------------
C*    Section 3. Move exponents into bit array.
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
     X                 IMASK, 'C')
        ELSE
C
C         Scalar faster.
          CALL SBYTES( KGRIB(IWORD), IEXP, IOFF, 8, 24, INEXT)
        ENDIF
#elif (defined CYBER)
        CALL SBYTES6( KGRIB(IWORD), IEXP, IOFF, 8, 24, INEXT)
#else
        CALL SBYTES( KGRIB(IWORD), IEXP, IOFF, 8, 24, INEXT)
#endif
C
C     ----------------------------------------------------------------
C*    Section 4. Move mantissae into bit array.
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
     X                 IMASK, 'C')
        ELSE
C
C         Scalar faster.
          CALL SBYTES( KGRIB(IWORD), IMANT, IOFF, 24, 8, INEXT)
        ENDIF
#elif (defined CYBER)
        CALL SBYTES6( KGRIB(IWORD), IMANT, IOFF, 24, 8, INEXT)
#else
        CALL SBYTES( KGRIB(IWORD), IMANT, IOFF, 24, 8, INEXT)
#endif
C
C       Update the bit pointer
        KNSPT = KNSPT +  32 * INEXT
C
  490 Continue
C     End of loop through rows of the subset
C
C
C     ----------------------------------------------------------------
C*    Section 5. Update pointer.
C     ----------------------------------------------------------------
C
  500 CONTINUE
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
