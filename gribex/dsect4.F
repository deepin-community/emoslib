C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION DSECT4( PDATA, KTRUNC, KSEC1, KSEC4,
     X                         KGRIB, KLENG, KNSPT, KBITS, KPACK)
C
C---->
C**** DSECT4
C
C     Purpose.
C     --------
C
C     Decode GRIB section 4 for complex packing of spherical harmonics.
C
C
C**   Interface.
C     ----------
C
C     KRET = DSECT4( PDATA, KTRUNC, KSEC1, KSEC4,
C    X               KGRIB, KLENG, KNSPT, KBITS, KPACK)
C
C
C     Input Parameters.
C     -----------------
C
C     PDATA      - Array of floating point values.
C     KTRUNC     - Truncation of the spherical harmonic coefficients.
C     KLENG      - Length of GRIB product array
C     KNSPT      - Bit pointer for next value in GRIB product
C     KBITS      - Number of bits per computer word.
C     KPACK      - Number of bits per packed value.
C
C
C     Output Parameters.
C     -----------------
C
C     KSEC1      - Array of GRIB section 1 values
C     KSEC4      - Array of GRIB section 4 values
C     KGRIB      - Array containing GRIB product.
C     KNSPT      - Bit pointer for next value in GRIB product(updated)
C
C
C     Method.
C     -------
C
C                            <subset>|
C
C     Given numbers:         cccccccc|cccccc
C                              cccccc|cccccc
C                                cccc|cccccc
C                                  cc|cccccc
C                                    |cccccc
C                                       cccc
C                                         cc
C
C
C     Externals.
C     ----------
C
C     GSCALE    - Apply power scaling to values to be packed.
C     UNPKCF    - Unpack unscaled coefficients from GRIB product.
C     UNCMPCK   - Re-scatter the values being unpacked into array.
C     INXBIT    - Extract bits from GRIB product.
C     DECFP2    - Decode from IBM floating point format.
C     JMALLOC   - Routine to acquire memory
C     JFREE     - Routine to release memory
C
C
C     Reference.
C     ----------
C
C     None.
C
C
C     Comments.
C     --------
C
C     On entry, KNSPT points to the first bit of section 4 in the GRIB
C               product.
C     On exit,  KNSPT points to the first bit of section 5 in the GRIB
C               product.
C
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      06:05:94
C
C
C     Modifications.
C     _____________
C
C     J.D.Chambers     ECMWF      13.09.95
C     Put in check for 32-bit packing.
C     (Removed May 1996)
C
C     J.Clochard, Meteo France, for ECMWF - January 1998.
C     FLOAT function replaced by REAL.
C     Fix comments.
C     Use of external JFREE instead of FREE.
C
C----<
C     -----------------------------------------------------------------
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Parameters
      INTEGER JP15BIT, JP87SET, JPBYTES
#ifdef REAL_8
      PARAMETER (JPBYTES = 8)
#else
      PARAMETER (JPBYTES = 4)
#endif
      PARAMETER ( JP15BIT= 2**15 )
C                            `---> 32768    =   8000(hex)
      PARAMETER ( JP87SET= 192 )
C                            `--->          =     C0(hex)
C
C     Subroutine arguments
C
      INTEGER KTRUNC, KSEC1, KSEC4, KGRIB, KLENG, KNSPT, KBITS, KPACK
      REAL PDATA
      DIMENSION PDATA(*), KGRIB(*), KSEC1(*), KSEC4(*)
C
C     Local variables.
C
      INTEGER J110, J310, IPOWER, ISUBSET
      INTEGER IVAL, IRET, IRETA, IRETB, IEXP, IMANT
      INTEGER INSPTA, ILEN, ISCALE
      INTEGER ILEN4, IFLAG, ISIZE
C
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IPIVAL
#endif
#endif
      POINTER (IPIVAL, IVAL)
      DIMENSION IVAL(1)
      SAVE IPIVAL, ISIZE
C
      REAL ZREF, ZSCALE
C
C     Externals
C
#ifdef POINTER_64
      INTEGER*8 JMALLOC
#else
      INTEGER JMALLOC
#endif
      INTEGER UNPKCF, GSCALE
      EXTERNAL UNPKCF, GSCALE, JMALLOC
C
      DATA ISIZE/0/
C
C     -----------------------------------------------------------------
C*    Section 1 . Decode octets 1 to 11.
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
      DSECT4 = 0
C
C     Allocate memory (first time only)
      IF( ISIZE.LT.((KTRUNC+1)*(KTRUNC+2)*JPBYTES) ) THEN
        IF( ISIZE.NE.0 ) CALL JFREE(IPIVAL)
        ISIZE = (KTRUNC+1)*(KTRUNC+2)*JPBYTES
        IPIVAL = JMALLOC(ISIZE)
#ifdef hpR64
        IPIVAL = IPIVAL/(1024*1024*1024*4)
#endif
        IF( IPIVAL.EQ.0 ) THEN
          WRITE(*,*) 'DSECT4: Memory allocation failed.'
          WRITE(*,*) 'DSECT4: Number of bytes required = ', ISIZE
          DSECT4 = 16123
          GOTO 900
        ENDIF
      ENDIF
C
C     Preserve bit position of start of section 4.
      INSPTA = KNSPT
C
C     Fill in the number of values unpacked
      KSEC4(1) = (KTRUNC+1)*(KTRUNC+2)
C
C     Octets 1 - 3: Length of section.
C     One 24-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,ILEN4,1,KBITS,24,'D',IRET)
      IF (IRET.NE.0) THEN
        DSECT4 = 16110
        WRITE(*,*) 'DSECT4: Problem unpacking length of section.'
        GO TO 900
      ENDIF
C
C     Octet 4:
C     One 4-bit field -> flag, spherical harmonics/complex/float point
C     One 4-bit field -> number of unused bits at end of section 4.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,IFLAG,1,KBITS,8,'D',IRET)
      IF (IRET.NE.0) THEN
        DSECT4 = 16111
        WRITE(*,*) 'DSECT4: Problem unpacking flag byte.'
        GO TO 900
      ENDIF
C
C     Check the flag
      IF ( (IFLAG/16) .NE. 12 ) THEN
        DSECT4 = 16112
        WRITE(*,*) 'DSECT4: Flag wrong = ', IFLAG
        GO TO 900
      ELSE
        KSEC4(3) = 128
        KSEC4(4) = 64
        DO 110 J110 = 5, 33
          KSEC4(J110) = 0
  110   CONTINUE
      ENDIF
C
C*    Octets 5 - 6 : Scale factor.
C     One 16 bit field.
C
C     Adjust bit pointer over first 4 octets and insert scale factor.
      CALL INXBIT(KGRIB,KLENG,KNSPT,ISCALE,1,KBITS, 16,'D',IRET)
      IF (IRET.NE.0) THEN
        DSECT4 = 16113
        WRITE(*,*) 'DSECT4: Problem unpacking scale factor.'
        GO TO 900
      ENDIF
C
C     Make negative if sign bit set
      IF ( ISCALE .GT. JP15BIT ) THEN
        ISCALE = ISCALE - JP15BIT
        ISCALE = -ISCALE
      ENDIF
      ZSCALE = 2.0**ISCALE
C
C*    Octets 7 - 10 : Reference value.
C     One 8 bit and one 24 bit field.
C
C     Extract reference value exponent and mantissa.
      CALL INXBIT(KGRIB,KLENG,KNSPT,IEXP,1,KBITS, 8,'D',IRETA)
      CALL INXBIT(KGRIB,KLENG,KNSPT,IMANT,1,KBITS, 24,'D',IRETB)
      IRET = IRETA + IRETB
      IF (IRET.NE.0) THEN
        DSECT4 = 16114
        WRITE(*,*) 'DSECT4: Problem unpacking reference value.'
        GO TO 900
      ENDIF
C
C     Decode the reference value
      CALL DECFP2 (ZREF,IEXP,IMANT)
C
C*    Octet 11 : Number of bits containing each packed value.
C     One 8 bit field.
C
C     Extract number of bits for each packed value.
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC4(2),1,KBITS, 8,'D',IRET)
      IF (IRET.NE.0) THEN
        DSECT4 = 16115
        WRITE(*,*) 'DSECT4: Problem unpacking number of bits/value.'
        GO TO 900
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 2 . Decode octets 12 to 18.
C     -----------------------------------------------------------------
C
  200 CONTINUE
C
C     Octets 12 - 13: Octet number of the start of packed data
C     One 16-bit field.
C
C     Extract octet number of the start of packed data.
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC4(16),1,KBITS,16,'D',IRET)
      IF (IRET.NE.0) THEN
        DSECT4 = 16116
        WRITE(*,*) 'DSECT4: Problem unpacking start of packed data.'
        GO TO 900
      ENDIF
C
C     Octets 14 - 15: Scaling factor; power of Laplacian operator
C                     applied to field before packing.
C     One 16-bit field.
C
C     Extract scaling power.
      CALL INXBIT(KGRIB,KLENG,KNSPT,IPOWER,1,KBITS,16,'D',IRET)
      IF (IRET.NE.0) THEN
        DSECT4 = 16117
        WRITE(*,*) 'DSECT4: Problem unpacking scaling power.'
        GO TO 900
      ENDIF
C
C     Set value negative if sign bit set.
      IF (IPOWER.GT.JP15BIT) THEN
        IPOWER = IPOWER - JP15BIT
        IPOWER = -IPOWER
      ENDIF
      KSEC4(17) = IPOWER
C
C     Octets 16 - 18: Pentagonal resolution parameters J,K,M
C                     specifying truncation of subset of data
C                     represented unpacked.
C     Three 8-bit fields.
C
C     Extract pentagonal resolution parameters.
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC4(18),3,KBITS, 8,'D',IRET)
      IF (IRET.NE.0) THEN
        DSECT4 = 16118
        WRITE(*,*)
     X    'DSECT4: Problem unpacking pentagonal resolution params.'
        GO TO 900
      ENDIF
      ISUBSET = KSEC4(18)
      IF ( ISUBSET .GT. KTRUNC ) THEN
        WRITE(*,*) 'DSECT4: Invalid subset truncation given'
        WRITE(*,*) 'DSECT4: ISUBSET, KTRUNC =', ISUBSET, KTRUNC
        DSECT4 = 16122
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 3.  Extract and scale binary values from GRIB product.
C     -----------------------------------------------------------------
C
  300 CONTINUE
C
C     Adjust bit pointer to end of stored subset of coefficients and
C     extract binary values from GRIB product -> updates bit pointer.
      KNSPT = 8*KSEC4(16)
      ILEN = (KTRUNC+1)*(KTRUNC+2) - (ISUBSET+1)*(ISUBSET+2)
      CALL INXBIT (KGRIB,KLENG,KNSPT,IVAL,ILEN, KBITS,KPACK, 'D',IRET)
      IF ( IRET .NE. 0 ) THEN
        WRITE(*,*) 'DSECT4: Problem unpacking bits'
        DSECT4 = 16119
        GOTO 900
      ENDIF
C
      DO 310 J310 = 1 , ILEN
         PDATA(J310)   = ( REAL(IVAL(J310)) *  ZSCALE ) + ZREF
  310 CONTINUE
C
C     Move the coefficients to their correct (scattered) in the array.
      CALL UNCMPCK( PDATA, KTRUNC, ISUBSET)
C
C
C     -----------------------------------------------------------------
C*    Section 4 . Unpack the subset of coefficients (stored IBM style).
C     -----------------------------------------------------------------
C
  400 CONTINUE
C
C     Adjust pointer to avoid first 18 octets.
      KNSPT = INSPTA + 18*8
C
C     Move back into original locations in the array and convert from
C     IBM format.
      IRET = UNPKCF( PDATA,KTRUNC,ISUBSET,KGRIB,KLENG,KNSPT,KBITS)
      IF ( IRET .NE. 0 ) THEN
        WRITE(*,*) 'DSECT4: Problem unpacking subset of coeffs.'
        DSECT4 = 16120
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 5 . Apply power scaling to field.
C     -----------------------------------------------------------------
C
  500 CONTINUE
C
      IRET = GSCALE( PDATA, KTRUNC, ISUBSET, IPOWER, 'O')
      IF ( IRET .NE. 0 ) THEN
        WRITE(*,*) 'DSECT4: Problem applying power scaling'
        DSECT4 = 16121
        GOTO 900
      ENDIF
C
C     Set bit pointer on 7777 group - section 5
      KNSPT = INSPTA + 8*ILEN4
C
C     -----------------------------------------------------------------
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END
