C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

#if (defined IBM_POWER4)
@PROCESS ALIAS(NOPTEOVRLP)
#endif
      INTEGER FUNCTION DSECT4A( PDATA, KTRUNC, KSEC0, KSEC1, KSEC4,
     X                          KGRIB, KLENG, KNSPT, KBITS, KPACK)
C
C---->
C**** DSECT4A
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
C     KRET = DSECT4A( PDATA, KTRUNC, KSEC0, KSEC1, KSEC4,
C    X                KGRIB, KLENG,  KNSPT, KBITS, KPACK)
C
C
C     Input Parameters.
C     -----------------
C
C     PDATA      - Array of floating point values.
C     KTRUNC     - Truncation of the spherical harmonic coefficients.
C     KSEC0      - Array of GRIB section 0 values
C     KLENG      - Length of GRIB product array
C     KNSPT      - Bit pointer for next value in GRIB product
C     KBITS      - Number of bits per computer word.
C
C
C     Output Parameters.
C     -----------------
C
C     KSEC1      - Array of GRIB section 1 values
C     KSEC4      - Array of GRIB section 4 values
C     KGRIB      - Array containing GRIB product.
C     KNSPT      - Bit pointer for next value in GRIB product(updated)
C     KPACK      - Number of bits per packed value.
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
C     J.Clochard, Meteo France, for ECMWF - September 1998.
C     Fix KNSPT position at end of routine.
C     Enable processing of GRIB edition 0 data.
C     Dummy-argument KSEC0 added.
C     KPACK changed as output argument.
C
C     P.J.Towers       ECMWF
C     Optimised for IBM Power4
C       -added @PROCESS ALIAS(NOPTEOVRLP) directive
C
C----<
C     -----------------------------------------------------------------
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "common/grprs.h"
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
      INTEGER KSEC0
      REAL PDATA
      DIMENSION PDATA(*), KGRIB(*), KSEC0(*), KSEC1(*), KSEC4(*)
C
C     Local variables.
C
      INTEGER J110, LOOP, IPOWER, ISUBSET
#if (defined USE_NO_POINTERS)
      INTEGER NREST, NLOOPS, ILOOP
#endif
      INTEGER IRET, IRETA, IRETB, IEXP, IMANT
      INTEGER INSPTA, ILEN, ISCALE
      INTEGER ILEN4, IFLAG, ISIZE, IAUXIL, IOFF, ITEMP, J, INIL
C
      CHARACTER *1 YFUNC
C
#ifndef USE_NO_POINTERS
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IPIVAL
#endif
#endif
      POINTER (IPIVAL, IVAL)
      INTEGER IVAL(1)
      SAVE IPIVAL, ISIZE
#else
      INTEGER IVAL(1000)
#endif
C
      REAL ZREF, ZSCALE
C
C     Externals
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
      DSECT4A = 0
      YFUNC='D'
C
      IF( KSEC0(2).GE.1 ) THEN
        IAUXIL=2
      ELSE
        IAUXIL=1
      ENDIF
C
      ITEMP = (KTRUNC+1)*(KTRUNC+IAUXIL)
#ifndef USE_NO_POINTERS
C
C     Allocate memory (first time only)
      IF( ISIZE.LT.(MAX(ITEMP,4*(KTRUNC+1))*JPBYTES) ) THEN
        IF( ISIZE.NE.0 ) CALL JFREE(IPIVAL)
        ISIZE = (KTRUNC+1)*(KTRUNC+IAUXIL)*JPBYTES
        IPIVAL = JMALLOC(ISIZE)
#ifdef hpR64
        IPIVAL = IPIVAL/(1024*1024*1024*4)
#endif
        IF( IPIVAL.EQ.0 ) THEN
          WRITE(GRPRSM,*) 'DSECT4A: Memory allocation failed.'
          WRITE(GRPRSM,*) 'DSECT4A: Number of bytes required = ', ISIZE
          DSECT4A = 16123
          GOTO 900
        ENDIF
      ENDIF
#endif
C
C     Preserve bit position of start of section 4.
      INSPTA = KNSPT
C
C     Fill in the number of values unpacked
      KSEC4(1) = ITEMP
C
C     Octets 1 - 3: Length of section.
C     One 24-bit field.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,ILEN4,1,KBITS,24,YFUNC,IRET)
      IF( IRET.NE.0 ) THEN
        DSECT4A = 16110
        WRITE(GRPRSM,*) 'DSECT4A: Problem unpacking length of section.'
        GO TO 900
      ENDIF
C
C     Octet 4:
C     One 4-bit field -> flag, spherical harmonics/complex/float point
C     One 4-bit field -> number of unused bits at end of section 4.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,IFLAG,1,KBITS,8,YFUNC,IRET)
      IF( IRET.NE.0 ) THEN
        DSECT4A = 16111
        WRITE(GRPRSM,*) 'DSECT4A: Problem unpacking flag byte.'
        GO TO 900
      ENDIF
C
C     Check the flag
      IF( ( KSEC0(2) .GE. 1 .AND. (IFLAG/16) .NE. 12 ) .OR.
     X    ( KSEC0(2) .LT. 1 .AND. (IFLAG/64) .NE.  3 )      ) THEN
        DSECT4A = 16112
        WRITE(GRPRSM,*) 'DSECT4A: Flag wrong = ', IFLAG
        GO TO 900
      ELSE
        KSEC4(3) = 128
        KSEC4(4) = 64
        INIL = MOD(IFLAG,16)
        DO 110 J110 = 5, 33
          KSEC4(J110) = 0
  110   CONTINUE
      ENDIF
C
C*    Octets 5 - 6 : Scale factor.
CIAU     One 16 bit field.
C
C     Adjust bit pointer over first 4 octets and insert scale factor.
      CALL INXBIT(KGRIB,KLENG,KNSPT,ISCALE,1,KBITS, 16,YFUNC,IRET)
      IF( IRET.NE.0) THEN
        DSECT4A = 16113
        WRITE(GRPRSM,*) 'DSECT4A: Problem unpacking scale factor.'
        GO TO 900
      ENDIF
C
C     Make negative if sign bit set
C
      IF( ISCALE .GT. JP15BIT ) THEN
        ISCALE = ISCALE - JP15BIT
        ISCALE = -ISCALE
      ENDIF
      ZSCALE = 2.0**ISCALE
C
C*    Octets 7 - 10 : Reference value.
C     One 8 bit and one 24 bit field.
C
C     Extract reference value exponent and mantissa.
      CALL INXBIT(KGRIB,KLENG,KNSPT,IEXP,1,KBITS, 8,YFUNC,IRETA)
      CALL INXBIT(KGRIB,KLENG,KNSPT,IMANT,1,KBITS, 24,YFUNC,IRETB)
      IRET = IRETA + IRETB
      IF( IRET.NE.0 ) THEN
        DSECT4A = 16114
        WRITE(GRPRSM,*) 'DSECT4A: Problem unpacking reference value.'
        GO TO 900
      ENDIF
C
C     Decode the reference value
C
      CALL DECFP2 (ZREF,IEXP,IMANT)
C
C*    Octet 11 : Number of bits containing each packed value.
C     One 8 bit field.
C
C     Extract number of bits for each packed value.
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC4(2),1,KBITS, 8,YFUNC,IRET)
      IF( IRET.NE.0 ) THEN
        DSECT4A = 16115
        WRITE(GRPRSM,*)
     X    'DSECT4A: Problem unpacking number of bits/value.'
        GO TO 900
      ENDIF
C
      KPACK=KSEC4(2)
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
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC4(16),1,KBITS,16,YFUNC,IRET)
      IF( IRET.NE.0 ) THEN
        DSECT4A = 16116
        WRITE(GRPRSM,*)
     X    'DSECT4A: Problem unpacking start of packed data.'
        GO TO 900
      ENDIF
C
C     Octets 14 - 15: Scaling factor; power of Laplacian operator
C                     applied to field before packing.
C     One 16-bit field.
C
C     Extract scaling power.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,IPOWER,1,KBITS,16,YFUNC,IRET)
      IF( IRET.NE.0 ) THEN
        DSECT4A = 16117
        WRITE(GRPRSM,*) 'DSECT4A: Problem unpacking scaling power.'
        GO TO 900
      ENDIF
C
C     Set value negative if sign bit set.
C
      IF( IPOWER.GT.JP15BIT ) THEN
        IPOWER = IPOWER - JP15BIT
        IPOWER = -IPOWER
      ENDIF
C
C     Octets 16 - 18: Pentagonal resolution parameters J,K,M
C                     specifying truncation of subset of data
C                     represented unpacked.
C     Three 8-bit fields.
C
C     Extract pentagonal resolution parameters.
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC4(18),3,KBITS, 8,YFUNC,IRET)
      IF( IRET.NE.0 ) THEN
        DSECT4A = 16118
        WRITE(GRPRSM,*)
     X    'DSECT4A: Problem unpacking pentagonal resolution params.'
        GO TO 900
      ENDIF
      ISUBSET = KSEC4(18)
      IF(  ISUBSET .GT. KTRUNC ) THEN
        WRITE(GRPRSM,*) 'DSECT4A: Invalid subset truncation given'
        WRITE(GRPRSM,*) 'DSECT4A: ISUBSET, KTRUNC =', ISUBSET, KTRUNC
        DSECT4A = 16122
        GOTO 900
      ENDIF
C
      IF( KSEC0(2).GE.1 ) THEN
        KSEC4(17) = IPOWER
        IOFF=0
      ELSE
C
C       In edition 0, power was an unscaled integer, byte "offset" used
C       to be a pointer relative to start of section 4, content/ordering
C       also used to be different.
C
        KSEC4(17) = 1000 * IPOWER
        KSEC4(16) = INSPTA/8 + KSEC4(16) - 1
        IOFF=(ISUBSET+1)**2
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
C
      KNSPT = 8*KSEC4(16)
      ILEN = KSEC4(1) - (ISUBSET+1)*(ISUBSET+IAUXIL)
C
#ifndef USE_NO_POINTERS
      CALL INXBIT(KGRIB,KLENG,KNSPT,IVAL,ILEN, KBITS,KPACK,YFUNC,IRET)
      IF( IRET.NE.0 ) THEN
        WRITE(GRPRSM,*) 'DSECT4A: Problem unpacking bits'
        DSECT4A = 16119
        GOTO 900
      ENDIF
C       Check that length in the file is consistent with the position of pointer
C       this might not happen for very large grib products (KLENG.ge.2097140)
C       Here we cannot multiply the length because the most significant bit(negative values)
C       is used to signify the lack of data - if not we update ILEN4

      IF (KSEC0(1).GT.8388607) THEN
        IF (((KNSPT-INSPTA+INIL)/8).ne.ILEN4) THEN
          ILEN4=((KNSPT-INSPTA+INIL)/8)
        ENDIF
      ENDIF
C
      DO LOOP = 1, ILEN
         PDATA(IOFF+LOOP) = ( REAL(IVAL(LOOP)) *  ZSCALE ) + ZREF
      ENDDO
#else
      NLOOPS = ILEN / 1000
      NREST  = ILEN - (NLOOPS)*1000
      KNSPT = 8*KSEC4(16)
C
      DO ILOOP = 1, NLOOPS
        CALL INXBIT(KGRIB,KLENG,KNSPT,IVAL,1000,KBITS,KPACK,YFUNC,IRET)
        IF( IRET.NE.0 ) THEN
          WRITE(GRPRSM,*) 'DSECT4A: Problem unpacking bits'
          DSECT4A = 16119
          GOTO 900
        ENDIF
C
        DO LOOP = 1, 1000
           PDATA(IOFF+(ILOOP-1)*1000+LOOP) =
     X       (REAL(IVAL(LOOP))* ZSCALE)+ZREF
        ENDDO
      ENDDO
C
      CALL INXBIT(KGRIB,KLENG,KNSPT,IVAL,NREST,KBITS,KPACK,YFUNC,IRET)
      IF( IRET.NE.0 ) THEN
        WRITE(GRPRSM,*) 'DSECT4A: Problem unpacking bits'
        DSECT4A = 16119
        GOTO 900
      ENDIF
C
      DO LOOP = 1, NREST
         PDATA(IOFF+NLOOPS*1000+LOOP) = (REAL(IVAL(LOOP))* ZSCALE)+ZREF
      ENDDO
#endif
C
      IF( KSEC0(2).GE.1 ) THEN
C
C     Move the coefficients to their correct (scattered) in the array.
C
        CALL UNCMPCK( PDATA, KTRUNC, ISUBSET)
      ENDIF
C
C
C     -----------------------------------------------------------------
C*    Section 4 . Unpack the subset of coefficients (stored IBM style).
C     -----------------------------------------------------------------
C
  400 CONTINUE
C
C     Adjust pointer to avoid first 18 octets.
C
      KNSPT = INSPTA + 18*8
C
      IF( KSEC0(2).GE.1 ) THEN
C
C     Move back into original locations in the array and convert from
C     IBM format.
C
        IRET = UNPKCF( PDATA,KTRUNC,ISUBSET,KGRIB,KLENG,KNSPT,KBITS)
        IF(  IRET .NE. 0 ) THEN
          WRITE(GRPRSM,*) 'DSECT4A: Problem unpacking subset of coeffs.'
          DSECT4A = 16120
          GOTO 900
        ENDIF
C
      ELSE
C
C     Direct conversion from IBM format.
C
C*      Calculate word pointer and offset, and get all exponents and
C       all mantissae (split in 3 parts).
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,IVAL,IOFF*4,KBITS, 8,YFUNC,IRET)
        IF(  IRET .NE. 0 ) THEN
          WRITE(GRPRSM,*)
     X      'DSECT4A: Problem extracting subset of coeffs.'
          DSECT4A = 16120
          GOTO 900
        ENDIF
C
        DO 401 J = 1, IOFF
C
          IEXP = IVAL(4*J-3)
          IMANT = IVAL(4*J) + 256 * ( IVAL(4*J-1) + 256 * IVAL(4*J-2) )
          CALL DECFP2(PDATA(J),IEXP,IMANT)
C
  401   CONTINUE
C
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 5 . Apply power scaling to field.
C     -----------------------------------------------------------------
C
  500 CONTINUE
C
      IF( KSEC0(2).GE.1 ) THEN
C
        IRET = GSCALE( PDATA, KTRUNC, ISUBSET, IPOWER, 'O')
        IF(  IRET .NE. 0 ) THEN
          WRITE(GRPRSM,*) 'DSECT4A: Problem applying power scaling'
          DSECT4A = 16121
          GOTO 900
        ENDIF
C
      ENDIF
C
C     Set bit pointer to last useful of section 4, before padding
C     if any (for consistency with GRIBEX).
C
      KNSPT = INSPTA + 8*ILEN4 - INIL
C
C     -----------------------------------------------------------------
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END
