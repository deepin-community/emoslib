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
      INTEGER FUNCTION CSECT4( PDATA, KTRUNC, KSEC1, KSEC4,
     X                         KGRIB, KLENG, KNSPT, KBITS, KPACK)
C
C---->
C**** CSECT4
C
C     Purpose.
C     --------
C
C     Code GRIB section 4 for complex packing of spherical harmonics.
C
C
C**   Interface.
C     ----------
C
C     KRET = CSECT4( PDATA, KTRUNC, KSEC1, KSEC4,
C    X               KGRIB, KLENG, KNSPT, KBITS, KPACK)
C
C
C     Input Parameters.
C     -----------------
C
C     PDATA      - Array of floating point values.
C     KTRUNC     - Truncation of the spherical harmonic coefficients.
C     KSEC1      - Array of GRIB section 1 values
C     KSEC4      - Array of GRIB section 4 values
C     KLENG      - Length of GRIB product array
C     KNSPT      - Bit pointer for next value in GRIB product
C     KBITS      - Number of bits per computer word.
C     KPACK      - Number of bits per packed value.
C
C
C     Output Parameters.
C     -----------------
C
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
C     GSCALE    - Apply power sclaing to values to be packed.
C     PACKCF    - Store unscaled coefficients in GRIB product.
C     CMPCK     - Make the values to be packed contiguous in memory.
C     MAXMIN    - Find max/min values of array.
C     INXBIT    - Insert bits into GRIB product.
C     REF2GRB   - Convert reference value to IBM floating point format.
C     CONFP3    - Convert to IBM floating point format.
C     DECFP     - Decode from IBM floating point format.
C     CALCOP    - Calculate the P scale factor for a field
C     JMALLOC   - Routine to acquire memory
C     JFREE     - Routine to release memory
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
C     J.Clochard, Meteo France, for ECMWF - January 1998.
C     Use of external JFREE instead of FREE.
C
C     P.J.Towers       ECMWF      18.07.02
C     Optimised for IBM Power4.
C         -Added @PROCESS ALIAS(NOPTEOVRLP) directive
C         -Replaced Nint(x) by Int(x+0.5)
C         -Replaced division by scale by multiplication by reciprocal
C
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
C
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
      INTEGER J110, LOOP, IPOWER, ISUBSET
#if (defined USE_NO_POINTERS)
      INTEGER NREST, NLOOPS, ILOOP
#endif
      INTEGER ILENF, ISTATUS
      INTEGER IRET, IRETA, IRETB, IEXP, IMANT, ITEMP
      INTEGER INSPT1, INSPTA, INSPTB, ILEN, ISCALE, ISIGN
      INTEGER ILEN4, INIL, IL, IFLAG, ISIZE
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
      REAL ZCHANGE, ZMAX, ZREF, ZMIN, ZSCALE,ZRECIP
#if defined(CRAY) || defined(IBM) || defined(CYBER) || defined(hpiaR32)
      REAL    ZS
#else
      DOUBLE PRECISION    ZS
#endif
C
#include "grbcom.h"
C
C     Externals
C
#ifdef POINTER_64
      INTEGER*8 JMALLOC
#else
      INTEGER JMALLOC
#endif
      INTEGER PACKCF, GSCALE, CALCOP, REF2GRB
      EXTERNAL PACKCF, GSCALE, CALCOP, REF2GRB, JMALLOC
C
      DATA ISIZE/0/
C
C     -----------------------------------------------------------------
C*    Section 1 . Change units of data values , if required.
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
      CSECT4 = 0
#ifndef USE_NO_POINTERS
C
C     Allocate memory (first time only)
C
      IF( ISIZE.LT.((KTRUNC+1)*(KTRUNC+2)*JPBYTES) ) THEN
        IF( ISIZE.NE.0 ) CALL JFREE(IPIVAL)
        ISIZE = (KTRUNC+1)*(KTRUNC+2)*JPBYTES
        IPIVAL = JMALLOC(ISIZE)
#ifdef hpR64
        IPIVAL = IPIVAL/(1024*1024*1024*4)
#endif
        IF( IPIVAL.EQ.0 ) THEN
          WRITE(GRPRSM,*) 'CSECT4: Memory allocation failed.'
          WRITE(GRPRSM,*) 'CSECT4: Number of bytes required = ', ISIZE
          CSECT4 = 16112
          GOTO 900
        ENDIF
      ENDIF
#endif
C
C     Preserve bit position of start of section 4.
      INSPT1 = KNSPT
C
C     Apply decimal scaling if required.
      IF (KSEC1(23).NE.0) THEN
        ZCHANGE = 10.0**KSEC1(23)
        ILENF = (KTRUNC+1) * (KTRUNC+2)
        DO 110 J110 = 1 , ILENF
          PDATA(J110) = PDATA(J110) * ZCHANGE
  110   CONTINUE
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 2.  Check given truncations.
C     -----------------------------------------------------------------
C
  200 CONTINUE
C
      ISUBSET = KSEC4(18)
      IF ( ISUBSET .GT. KTRUNC ) THEN
        WRITE(GRPRSM,*) 'CSECT4: Invalid subset truncation given'
        WRITE(GRPRSM,*) 'CSECT4: ISUBSET, KTRUNC =', ISUBSET, KTRUNC
        CSECT4 = 16000
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 3 . Apply power scaling to field.
C     -----------------------------------------------------------------

  300 CONTINUE
C
C     If user supplied the P factor, use it ..
      IF ( NONOFF .EQ. 0 ) THEN
        IPOWER  = KSEC4(17)
C
C     .. otherwise calculate the P factor for the field ..
      ELSE
        IPOWER  = CALCOP( PDATA, KTRUNC, ISUBSET)
        IF( IPOWER.LT.-10000 ) THEN
          CSECT4 = 16001
          GOTO 900
        ENDIF
        KSEC4(17) = IPOWER
      ENDIF
C     .. and scale the field.
      IRET = GSCALE( PDATA, KTRUNC, ISUBSET, IPOWER, 'I')
      IF ( IRET .NE. 0 ) THEN
        CSECT4 = 16002
        GOTO 900
      ENDIF
C
C
C     -----------------------------------------------------------------
C*    Section 4 . Store subset of coefficients (IBM style).
C     -----------------------------------------------------------------
C
  400 CONTINUE
C
C     Adjust pointer to avoid first 18 octets -> filled in later.
      INSPTA = KNSPT
      KNSPT = KNSPT + 18*8
      IRET = PACKCF( PDATA,KTRUNC,ISUBSET,KGRIB,KLENG,KNSPT,KBITS)
      IF ( IRET .NE. 0 ) THEN
        CSECT4 = 16003
        GOTO 900
      ENDIF
C
C     Reset bit pointer to start of section 4.
      INSPTB = KNSPT
      KNSPT = INSPTA
C
C     -----------------------------------------------------------------
C*    Section 5 . Find the max and min for the values to be scaled.
C     -----------------------------------------------------------------
C
  500 CONTINUE
C
C     Rearrange the values to be packed so they are contiguous.
      CALL CMPCK( PDATA, KTRUNC, ISUBSET)
C
C     Find the max and min for the contiguous values
      ILEN = (KTRUNC+1)*(KTRUNC+2) - (ISUBSET+1)*(ISUBSET+2)
      CALL MAXMIN( PDATA, ILEN, ZMAX, ZMIN)
C
C     Calculate the scale and reference value
      ZREF = ZMIN
      ZS = (ZMAX-ZREF) / (2**(KSEC4(2)+1)-1)
#ifdef CRAY
      IF (ZS.NE.0.0) ZS = ALOG(ZS) / ALOG(2.0) + 2.0
#else
      IF (ZS.NE.0.0) ZS = LOG(ZS) / LOG(2.0) + 2.0
#endif
#if defined hpR32 || defined (__alpha) || defined hpR64 || defined hpiaR64
      ISCALE = MIN (INT(ZS),INT(ZS+DSIGN(1.0D0,ZS)))
#elif defined (linux) || defined (darwin)
      ISCALE = MIN (INT(ZS),INT(ZS+SIGN(1.D0,ZS)))
#else
      ISCALE = MIN (INT(ZS),INT(ZS+SIGN(1.,ZS)))
#endif
      ZSCALE = 2.0**ISCALE
C
C     Set scale factor sign bit.
      IF (ISCALE.LT.0) THEN
        ISCALE = -ISCALE
        ISIGN  = JP15BIT
        ISCALE = ISCALE + ISIGN
      ENDIF
C
C*    Octets 5 - 6 : Scale factor.
C     One 16 bit field.
C
C     Adjust bit pointer over first 4 octets and insert scale factor.
      KNSPT = KNSPT + 4*8
      CALL INXBIT(KGRIB,KLENG,KNSPT,ISCALE,1,KBITS, 16,'C',IRET)
      IF (IRET.NE.0) THEN
        WRITE(GRPRSM,*) 'CSECT4: Problem packing scale factor.'
        CSECT4 = 16004
        GO TO 900
      ENDIF
C
C*    Octets 7 - 10 : Reference value.
C     One 8 bit and one 24 bit field.
C
C     Convert floating point to GRIB representation.
      ISTATUS = REF2GRB(ZREF, IEXP, IMANT, KBITS)
      IF( ISTATUS.NE.0 ) THEN
        WRITE(GRPRSM,*) 'CSECT4: REF2GRB reference value problem'
        WRITE(GRPRSM,*) 'CSECT4: for parameter ', KSEC1(6)
        WRITE(GRPRSM,*) 'CSECT4: level type ', KSEC1(7)
        WRITE(GRPRSM,*) 'CSECT4: level ', KSEC1(8), KSEC1(9)
      ENDIF
C
C     Insert reference value exponent and mantissa.
      CALL INXBIT(KGRIB,KLENG,KNSPT,IEXP,1,KBITS, 8,'C',IRETA)
      CALL INXBIT(KGRIB,KLENG,KNSPT,IMANT,1,KBITS, 24,'C',IRETB)
      IRET = IRETA + IRETB
      IF (IRET.NE.0) THEN
        WRITE(GRPRSM,*) 'CSECT4: Problem packing reference value.'
        CSECT4 = 16005
        GO TO 900
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 6 . Store octets 11 to 18.
C     -----------------------------------------------------------------
C
  600 CONTINUE
C
C*    Octet 11 : Number of bits containing each packed value.
C     One 8 bit field.
C
C     Insert number of bits for each packed value.
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC4(2),1,KBITS, 8,'C',IRET)
      IF (IRET.NE.0) THEN
        WRITE(GRPRSM,*) 'CSECT4: Problem packing number of bits/value.'
        CSECT4 = 16006
        GO TO 900
      ENDIF
C
C     Octets 12 - 13: Octet number of the start of packed data
C     One 16-bit field.
C
C     Insert octet number of the start of packed data.
      KSEC4(16) = KNSPT/8 + 7 + 4*(ISUBSET+1)*(ISUBSET+2)
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC4(16),1,KBITS,16,'C',IRET)
      IF (IRET.NE.0) THEN
        WRITE(GRPRSM,*) 'CSECT4: Problem packing start of packed data.'
        CSECT4 = 16007
        GO TO 900
      ENDIF
C
C     Octets 14 - 15: Scaling factor; power of Laplacian operator
C                     applied to field before packing.
C     One 16-bit field.
C
C     Set sign bit if value is negative.
      IF (KSEC4(17).LT.0) THEN
        ITEMP = - KSEC4(17)
        ITEMP = ITEMP + JP15BIT
      ELSE
        ITEMP = KSEC4(17)
      ENDIF
C
C     Insert scaling power.
      CALL INXBIT(KGRIB,KLENG,KNSPT,ITEMP,1,KBITS,16,'C',IRET)
      IF (IRET.NE.0) THEN
        WRITE(GRPRSM,*) 'CSECT4: Problem packing scaling power.'
        CSECT4 = 16008
        GO TO 900
      ENDIF
C
C     Octets 16 - 18: Pentagonal resolution parameters J,K,M
C                     specifying truncation of subset of data
C                     represented unpacked.
C     Three 8-bit fields.
C
C     Insert pentagonal resolution parameters.
      CALL INXBIT(KGRIB,KLENG,KNSPT,KSEC4(18),3,KBITS, 8,'C',IRET)
      IF (IRET.NE.0) THEN
        WRITE(GRPRSM,*)
     X    'CSECT4: Problem packing pentagonal resolution params.'
        CSECT4 = 16009
        GO TO 900
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 7 . Adjust, scale and pack the binary values.
C     -----------------------------------------------------------------
C
  700 CONTINUE
#ifndef USE_NO_POINTERS
      ZRECIP=1.0/ZSCALE
C
      DO LOOP = 1 , ILEN
#ifdef VAX
        IVAL(LOOP)   = JNINT( (PDATA(LOOP) - ZREF) / ZSCALE )
#else
        IVAL(LOOP)   =  INT( (PDATA(LOOP) - ZREF ) * ZRECIP + 0.5 )
#endif
      ENDDO
C
C     Adjust bit pointer to end of stored subset of coefficients and
C     store packed binary values in GRIB product -> updates bit pointer.
C
      KNSPT = INSPTB
      CALL INXBIT (KGRIB,KLENG,KNSPT,IVAL,ILEN, KBITS,KPACK, 'C',IRET)
      IF ( IRET .NE. 0 ) THEN
        WRITE(GRPRSM,*) 'CSECT4: Problem packing bits'
        CSECT4 = 16010
        GOTO 900
      ENDIF
#else
      NLOOPS = ILEN / 1000
      NREST  = ILEN - (NLOOPS)*1000
      KNSPT  = INSPTB
C
      DO ILOOP = 1, NLOOPS
        DO LOOP = 1 , 1000
#ifdef VAX
          IVAL(LOOP) = JNINT((PDATA((ILOOP-1)*1000+LOOP)-ZREF)/ZSCALE)
#else
          IVAL(LOOP) =  NINT((PDATA((ILOOP-1)*1000+LOOP)-ZREF)/ZSCALE)
#endif
        ENDDO
C
C       Adjust bit pointer to end of stored subset of coefficients and
C       store packed binary values in GRIB product -> updates bit pointer.
C
        CALL INXBIT(KGRIB,KLENG,KNSPT,IVAL,1000,KBITS,KPACK,'C',IRET)
        IF( IRET.NE.0 ) THEN
          WRITE(GRPRSM,*) 'CSECT4: Problem packing bits'
          CSECT4 = 16010
          GOTO 900
        ENDIF
      ENDDO
C
      DO LOOP = 1, NREST
#ifdef VAX
        IVAL(LOOP) = JNINT((PDATA((NLOOPS*1000)+LOOP)-ZREF)/ZSCALE)
#else
        IVAL(LOOP) =  NINT((PDATA((NLOOPS*1000)+LOOP)-ZREF)/ZSCALE)
#endif
      ENDDO
C
C       Adjust bit pointer to end of stored subset of coefficients and
C       store packed binary values in GRIB product -> updates bit pointer.
C
      CALL INXBIT (KGRIB,KLENG,KNSPT,IVAL,NREST,KBITS,KPACK,'C',IRET)
      IF( IRET.NE.0 ) THEN
        WRITE(GRPRSM,*) 'CSECT4: Problem packing bits'
        CSECT4 = 16010
        GOTO 900
      ENDIF
#endif
C
C     -----------------------------------------------------------------
C*    Section 8 . Fill in first 4 octets of section 4.
C     -----------------------------------------------------------------
C
  800 CONTINUE
C
C     Adjust length of section 4 (in bits) to a multiple of 16.
      ILEN4 = KNSPT - INSPT1
      IL    = ILEN4 / 16
      IL    = ILEN4 - ( IL * 16 )
      INIL  = 0
      IF (IL.NE.0) INIL = 16 - IL
      KNSPT = KNSPT + INIL
      ILEN4 = ILEN4 + INIL
C
C     Set length of section 4 to octets.
      ILEN4 = ILEN4 / 8
C
C     Octets 1 - 3: Length of section.
C     One 24-bit field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT1,ILEN4,1,KBITS,24,'C',IRET)
      IF (IRET.NE.0) THEN
        WRITE(GRPRSM,*) 'CSECT4: Problem packing length of section.'
        CSECT4 = 16010
        GO TO 900
      ENDIF
C
C     Octet 4:
C     One 4-bit field -> flag, spherical harmonics/complex/float point
C     One 4-bit field -> number of unused bits at end of section 4.
C
      IFLAG = JP87SET + INIL
      CALL INXBIT(KGRIB,KLENG,INSPT1,IFLAG,1,KBITS,8,'C',IRET)
      IF (IRET.NE.0) THEN
        WRITE(GRPRSM,*) 'CSECT4: Problem packing flag.'
        CSECT4 = 16011
        GO TO 900
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 9. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END
