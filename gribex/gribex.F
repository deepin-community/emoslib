C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C
      SUBROUTINE GRIBEX (KSEC0,KSEC1,KSEC2,PSEC2,KSEC3,PSEC3,KSEC4,
     C                   PSEC4,KLENP,KGRIB,KLENG,KWORD,HOPER,KRET)
C---->
#include "gribex.h"
C----<
C     -----------------------------------------------------------------|
C*    Section 0 . Definition of variables. Data statements.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Subroutine arguments
C
      INTEGER KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,KGRIB
      DIMENSION KGRIB(*), KSEC0(*), KSEC1(*)
      DIMENSION KSEC2(*), KSEC3(*), KSEC4(*)
      INTEGER KLENP,KLENG,KWORD, KRET
      REAL PSEC2, PSEC3, PSEC4
      DIMENSION PSEC2(*), PSEC3(*), PSEC4(*)
      CHARACTER*(*) HOPER
C
C     Local variables
C
      INTEGER N, NP, NINC
      REAL  JPEPSLN
      PARAMETER (JPEPSLN=1E-12)
C                         `-----> tolerance used to check equality
C                                 of floating point numbers - needed
C                                 on some platforms (eg vpp700, linux)
      CHARACTER*1   YFUNC, YTEMP
      INTEGER IJDC, JLOOP, JLOOPO, ISTATUS, ISECLEN
      INTEGER IDUMP
#ifdef REAL_BIGGER_THAN_INTEGER
      INTEGER IINDEX, ILOOPS, NREM, JPNSEC4, ILNGTH
      PARAMETER (JPNSEC4=12800)
      INTEGER*4 NSEC4
      DIMENSION NSEC4(JPNSEC4)
      REAL*4    XSEC4
      DIMENSION XSEC4(JPNSEC4)
      EQUIVALENCE (NSEC4(1), XSEC4(1))
      INTEGER*8 IJDCXX
      REAL*8    RJDCXX
#else
      INTEGER IJDCXX
      REAL    RJDCXX
#endif
      EQUIVALENCE (IJDCXX, RJDCXX)
      INTEGER I,IBLEN,IBITS,IBMAP,IBMAP2,IBYTEX,ICOUNT,NBYTE
      INTEGER IEXP, IFLAG, IFLAGX, IFPT, IL
      INTEGER ILEN, ILENF, ILEN1, ILEN2, ILEN3, ILEN4
      INTEGER IMANT, IMISNG, IMISS, IMODAY, INC
      INTEGER INIL, INITAL, INOLAT, INOLNG, INSPT, INUB
      INTEGER INUM, IOFF, IPL, IPLEN, IPSEUD
      INTEGER IPVPL, IRESOL, ISBMAP, NONMISS
      INTEGER ISCALE, ISINT, ISKALE
      INTEGER ISKIP, ITEMP, ITRND, IVALS, IP7777
      INTEGER IEAST, IWEST, ILOEXT, INROWS
C
      INTEGER JPEDNO, JPLEN1
      INTEGER KRETA, KRETB
      INTEGER ITRUNC, ISUBSET, IS3BYTE
      INTEGER NBPV
      REAL RANGE
C
C     Wave coordinate information held in KSEC4 are 32-bit REALs,
C     so REAL*4 ZREAL4 must be used for encoding/decoding.
C
      REAL*4  ZREAL4
      REAL    ZMAXV, ZMAX, ZMIN, ZMISNG, ZMSVAL, ZREAL, ZREF, ZVAL
#if defined(CRAY) || defined(CYBER) ||((defined IBM)&&(!defined rs6000))
      REAL    ZS, ZAUXIL
#else
      DOUBLE PRECISION    ZS, ZAUXIL
#endif
      REAL    ZSCALE
C
      LOGICAL LENCODE, LDECODE, LPDEBUG
      LOGICAL LGRDPT, LSPHERC, LCOMPLX
      LOGICAL LECMWF, LECLOC, LKWBCE, LSST11
      LOGICAL LLARGE
      LOGICAL LQUASI, LSECT2, LSECT3, LPERIO
      LOGICAL L_IORJ
      LOGICAL LALLPOS
      LOGICAL LVEGGY
      LOGICAL LSATIMG
      INTEGER NITABLE, NICENTRE, NIPARAM
C
C     Externals
C
      INTEGER EGGSEC2, DGGSEC2, ELLSEC2, DLLSEC2, ESHSEC2, DSHSEC2
      INTEGER EOCSEC2, DOCSEC2, ESVSEC2, DSVSEC2, EMESEC2, DMESEC2
      INTEGER CSECT4, DSECT4A, GBITMAP, D2ORDR, C2ORDR, REF2GRB
      INTEGER ONEBITS, D13FLAG
C#ifdef GRIB2
C     INTEGER G2ENCOD
C     EXTERNAL G2ENCOD
C#endif
      EXTERNAL CSECT4, DSECT4A, GBITMAP, D2ORDR, C2ORDR, REF2GRB
      EXTERNAL EGGSEC2, DGGSEC2, ELLSEC2, DLLSEC2, ESHSEC2, DSHSEC2
      EXTERNAL EOCSEC2, DOCSEC2, ESVSEC2, DSVSEC2, EMESEC2, DMESEC2
      EXTERNAL ONEBITS, D13FLAG
C
C     GRIB code version number used in coding data.
C
      PARAMETER (JPEDNO=1)
C
C     GRIBEX version number
C
      INTEGER IGRIBEX
      CHARACTER*6 YGRIBEX
C
C     Length (in octets) used for Section 1, when coding data.
C
      PARAMETER (JPLEN1=28)
C
      INTEGER IPARM, ILALO, IGRIB, I7777
      DIMENSION IPARM(4), ILALO(2), IGRIB(4), I7777(4)
C
      INTEGER JP24SET, JP23SET, JP16SET, JP8SET
      PARAMETER ( JP24SET = 2**24 - 1 )
C                            ^---> 16777215 = FFFFFF(hex)
      PARAMETER ( JP23SET = 2**23 - 1 )
C                            ^---> 8388607  = 7FFFFF(hex)
      PARAMETER ( JP16SET = 2**16 - 1 )
C                            ^---> 65535    =   FFFF(hex)
      PARAMETER ( JP8SET  = 2**8 - 1 )
C                            ^---> 255      =     FF(hex)
C
C     Predetermined bitmask variables
C
#ifndef USE_NO_POINTERS
      INTEGER IBTMAP, IBTVALS
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IBTPTR
#endif
#endif
      POINTER (IBTPTR,IBTMAP)
      DIMENSION IBTMAP(1)
#endif
C
#include "grbcom.h"
C
C     Missing data indicator for integer and real values in GRIB code
C     header fields.
C
      EQUIVALENCE (IMISNG,ZMISNG)
C
      EQUIVALENCE (ZREAL4,ISINT)
C
      SAVE INITAL, IBITS, IMISNG, NBYTE
C
C     Characters GRIB and 7777 in Ascii for use in Sections 0 and 5
C     of GRIB code.
C
      DATA IGRIB /71,82,73,66/
      DATA I7777 /55,55,55,55/
      DATA YGRIBEX/'13.040'/, IGRIBEX/13040/
C
C     Initialise local variables
C
      DATA YFUNC/' '/,YTEMP/' '/
      DATA IJDC/0/,JLOOP/0/,JLOOPO/0/
#ifdef REAL_BIGGER_THAN_INTEGER
      DATA IINDEX/0/,ILOOPS/0/,NREM/0/,ILNGTH/0/
#endif
      DATA IJDCXX/0/,I/0/,IBLEN/0/,IBITS/0/,IBMAP/0/,IBMAP2/0/
      DATA IBYTEX/0/,ICOUNT/0/,NBYTE/0/
      DATA IEXP/0/,IFLAG/0/,IFLAGX/0/,IFPT/0/,IL/0/
      DATA ILEN/0/,ILENF/0/,ILEN1/0/,ILEN2/0/,ILEN3/0/,ILEN4/0/
      DATA IMANT/0/,IMISNG/0/,IMISS/0/,IMODAY/0/,INC/0/
      DATA INIL/0/,INITAL/0/,INOLAT/0/,INOLNG/0/,INSPT/0/,INUB/0/
      DATA INUM/0/,IOFF/0/,IPL/0/,IPLEN/0/,IPSEUD/0/
      DATA IPVPL/0/,IRESOL/0/,ISBMAP/0/,NONMISS/0/
      DATA ISCALE/0/,ISINT/0/,ISKALE/0/
      DATA ISKIP/0/,ITEMP/0/,ITRND/0/,IVALS/0/,IP7777/0/
      DATA IEAST/0/,IWEST/0/,ILOEXT/0/,INROWS/0/
C
      DATA KRETA/0/,KRETB/0/,ITRUNC/0/,ISUBSET/0/
C
      DATA ZMAXV/0.0/,ZMAX/0.0/,ZMIN/0.0/,ZMSVAL/0.0/
      DATA ZREF/0.0/,ZVAL/0.0/,ZS/0.0/,ZAUXIL/0.0/,ZSCALE/0.0/
C
      DATA LENCODE/.FALSE./,LDECODE/.FALSE./,LPDEBUG/.FALSE./
      DATA LGRDPT/.FALSE./,LSPHERC/.FALSE./,LCOMPLX/.FALSE./
      DATA LECMWF/.FALSE./,LECLOC/.FALSE./,LKWBCE/.FALSE./
      DATA LSST11/.FALSE./,LLARGE/.FALSE./,LQUASI/.FALSE./
      DATA LSECT2/.FALSE./,LSECT3/.FALSE./,LPERIO/.FALSE./
      DATA L_IORJ/.FALSE./,LALLPOS/.FALSE./
C
C     -----------------------------------------------------------------|
C*    Section 1 . Set initial values.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
      ISECLEN = 0
C
CC    Avoid using GRIBEX at (almost) all costs
      WRITE(GRPRSM,*) 'GRIBEX: functionality superseded by GRIB_API.'
#ifdef GRIBEX_ABORT
      CALL ABORTX('GRIBEX')
#endif
C
C     Handle 'V' option - display version number
C
      IF( HOPER(1:1).EQ.'V' ) THEN
        WRITE(GRPRSM,*) 'GRIBEX: Version is ',YGRIBEX
        KRET = IGRIBEX
        RETURN
      ENDIF
C
C     Clear definition 13  flag
C
      N13FLAG = -1
C
C     Clear flag indicating GRIB product longer than 2**23-1 bytes long.
      LLARGE = .FALSE.
C
C     Initialise flag indicating 'quasi-regular grid'
      LQUASI = .FALSE.
C
C*    Set number of bits per computer word,missing data indicator and
C     the number of bytes per computer word first time through.
C     Try to set default values for parameters in common area,
C     if not already set by user via calls to the GRS--- routines.
C
      IF( INITAL.EQ.0) THEN
        CALL SETPAR (IBITS,IMISNG,NDBG)
        NBYTE = IBITS/8
        INITAL = 1
        CALL GRSDEF
      ENDIF
C
      IMISS = 0
C
C
C     Set encoding/decoding flags initially dependent on 'L' option,
C     since this option skips round most processing.
      LENCODE = ( HOPER(1:1) .NE. 'L' )
      LDECODE = ( HOPER(1:1) .EQ. 'L' )
C
      LPDEBUG = ( NDBG.GE.1 )
C
      IF( LPDEBUG ) THEN
        WRITE(GRPRSM,*) 'GRIBEX: Version is ',YGRIBEX
        WRITE(GRPRSM,*) 'GRIBEX: Section 1. Input values used -'
        WRITE(GRPRSM,*)
     X  'GRIBEX: Length of GRIB array (KLENG) = ',KLENG
        WRITE(GRPRSM,*)
     X  'GRIBEX: Length of data array (KLENP) = ',KLENP
        WRITE(GRPRSM,*) 'GRIBEX: Operation code = ', HOPER
Ce
Ce      Extra printout if encoding
Ce
        IF( HOPER.EQ.'C'.OR.HOPER.EQ.'M'.OR.HOPER.EQ.'K') THEN
          KSEC0(2) = JPEDNO
          CALL GRPRS1(KSEC0,KSEC1)
Ce
          LSECT2 = KSEC1(5).GE.128
          LSECT3 = MOD(KSEC1(5),128).GE.64
Ce
Ce        Print section 2 if present.
Ce
          IF( LSECT2 ) CALL GRPRS2(KSEC0,KSEC2,PSEC2)
Ce
Ce        Print section 3 if present.
Ce
          IF( LSECT3 ) CALL GRPRS3(KSEC0,KSEC3,PSEC3)
Ce
Ce        Print section 4.
Ce
          CALL GRPRS4(KSEC0,KSEC4,PSEC4)
Ce
Ce        Special print for 2D spectra wave field real values in
Ce        section 4
Ce
          IF( (KSEC1(1).EQ.140) .AND.
     X        (KSEC1(2).EQ. 98) .AND.
     X        (KSEC1(24).EQ. 1) .AND.
     X        ( (KSEC1(40).EQ.1045).OR.(KSEC1(40).EQ.1081) ) .AND.
     X        ( (KSEC1(6) .EQ. 250).OR.(KSEC1(6) .EQ. 251) ) )
     X      CALL GRPRS4W(KSEC4)
        ENDIF
      ENDIF
C
C     Reset return code to 0, retaining input value to decide
C     on abort / no abort, if error encountered later.
C
      NOABORT = KRET
      KRET    = 0
C
C     IPSEUD is used to indicate pseudo-GRIB data encountered,
C     when decoding.
C     ISBMAP is the bit-map section flag and indicates what decoding
C     has been done on bit-maps and data.
C     See informative return codes for KRET when decoding.
C
      IPSEUD = 0
      ISBMAP = 0
C
C     Reset bit-pointer to 0.
C
      INSPT = 0
C
C     -----------------------------------------------------------------|
C
C
C
C
C
C
C
C
C
C
C*    Section 2 . Check input parameters.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IF( LPDEBUG ) WRITE(GRPRSM,*)
     X 'GRIBEX: Section 2. Check input parameters.'
C
C*    Check that valid function has been requested.
C
      YFUNC = HOPER
#ifdef USE_NO_POINTERS
      IF( YFUNC.EQ.'R' ) THEN
        KRET = 201
        WRITE(GRPRSM,*) 'GRIBEX: Invalid function requested - ',HOPER
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
#endif
      L_IORJ = (HOPER.EQ.'I').OR.(HOPER.EQ.'J')
      IF( YFUNC.NE.'C'.AND.YFUNC.NE.'D'.AND.(.NOT.L_IORJ).AND.
     X    YFUNC.NE.'L'.AND.YFUNC.NE.'R'.AND.YFUNC.NE.'S'.AND.
     X    YFUNC.NE.'X'.AND.YFUNC.NE.'Z'.AND.YFUNC.NE.'M'.AND.
     X    YFUNC.NE.'G'.AND.YFUNC.NE.'A'.AND.YFUNC.NE.'B'.AND.
     X    YFUNC.NE.'K') THEN
        KRET = 201
        WRITE(GRPRSM,*) 'GRIBEX: Invalid function requested - ',HOPER
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
C     Function 'A' encodes 8-bit data.
      IF( HOPER.EQ.'A') YFUNC = 'C'
C
C     Function 'B' decodes 8-bit data.
      IF( HOPER.EQ.'B') YFUNC = 'D'
C
C*    Function 'L' returns the length of the GRIB message and
C     GRIB Edition number only, so no array initialisation is
C     necessary.
C
      IF( YFUNC.EQ.'L') GO TO 300
C
C*    Function 'M' is for coding data, and if a bit map is encountered
C     GRIB messages are made a fixed length. HOPER is passed to the
C     bit-map handling routine.
C
      IF( HOPER.EQ.'M') YFUNC = 'C'
C
C*    Function 'K' is the same as 'C', but for grid-point fields,
C     an "aggressive packing" is performed: all user-allowed strategies
C     for second-order packing are attempted, in order to make the
C     shortest GRIB message.
C
      IF( HOPER.EQ.'K') YFUNC = 'C'
C
C*    Function 'I' is for decoding of sections 0, 1 and 2
C     of GRIB code only. Value of HOPER is checked at the start of
C     decoding section 3.
C*    Function 'J' is for decoding of sections 0, 1, 2, 3 and 4.
C
      IF( L_IORJ) YFUNC = 'D'
C
C*    Function 'R' is the same as 'D', but if a quasi-regular
C     Gaussian is encountered, it is converted to a regular one.
C     Value of HOPER is checked near end of section 8.
C
      IF( HOPER.EQ.'R') YFUNC = 'D'
C
C*    Function 'S' is the same as 'D', but if analysis data in
C     GRIB Experimental Edition is encountered, the time range
C     indicator flag is set to indicate initialised analysis.
C     Value of HOPER is checked  when time range indicator has
C     been extracted.
C
      IF( HOPER.EQ.'S') YFUNC = 'D'
C
C*    Function 'X' is the same as 'D', but only the data
C     at the requested points is unpacked.
C     Value of HOPER is checked prior to unpacking data values.
C
      IF( HOPER.EQ.'X') YFUNC = 'D'
C
C*    Function 'G' is the same as 'D', but only the reference
C     value and scale factor are unpacked. A pointer to packed data
C     is returned.
C     Value of HOPER is checked prior to unpacking data values.
C
      IF( HOPER.EQ.'G') YFUNC = 'D'
C
C*    Function 'Z' is for decoding only the information which could
C     be handled by the the old decoding routine DECOGB (eg no bit
C     maps) and is used by the new DECOGB interface.
C
      IF( HOPER.EQ.'Z') YFUNC = 'D'
C
      LENCODE = ( YFUNC.EQ.'C' )
      LDECODE = ( YFUNC.EQ.'D' )
Cd
Cd    Set up the definition 13 flag variable
Cd
      IF( LDECODE ) N13FLAG = D13FLAG(KGRIB)
C
C*    Preset some arrays to 0.
C
      IF( LENCODE ) THEN
Ce
Ce      Check number of bits per data field fits size of computer word.
Ce
        IF( IBITS.LT.KSEC4(2)) THEN
          KRET = 202
          WRITE(GRPRSM,*)
     X  'GRIBEX: Number of bits per data value = ',KSEC4(2)
          WRITE(GRPRSM,*)
     X  'GRIBEX: This exceeds word length which = ',IBITS
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Ce
Ce      Check number of bits is not zero or negative
Ce
        IF( KSEC4(2) .LT. 0 ) THEN
          KRET = 204
          WRITE(GRPRSM,*)
     X  'GRIBEX: Number of bits per data value = ',KSEC4(2)
          WRITE(GRPRSM,*) 'GRIBEX: This must be positive.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GOTO 900
        ELSEIF( KSEC4(2) .EQ. 0 ) THEN
          WRITE(GRPRSM,*)
     X  'GRIBEX: Number of bits per data value = ',KSEC4(2)
          KSEC4(2) = 8
          WRITE(GRPRSM,*)
     X  'GRIBEX: This must be positive - enforced to ',
     X               KSEC4(2)
        ENDIF
Ce
Ce      Check if number of bits per packed value = number of bits per
Ce      computer word.
Ce
        LALLPOS = KSEC4(2) .EQ. IBITS
Ce
Ce      If entire field is missing, check that all data values are zero.
Ce
        IF( KSEC4(1).LT.0) THEN
          IMISS = 1
          ILENF = - KSEC4(1)
          DO 210 JLOOP = 1 , ILENF
            IF( PSEC4(JLOOP).NE.0.0) THEN
              KRET = 203
              WRITE(GRPRSM,*)
     X  'GRIBEX: Non-zero value in missing data field.'
              WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
              GO TO 900
            ENDIF
  210     CONTINUE
        ELSE
          IMISS = 0
          ILENF = KSEC4(1)
        ENDIF
Ce
Ce      If input data is integer, change it to real.
Ce
        IF( ( KSEC4(5).EQ.32 ).AND.( HOPER.NE.'A' ) )
     X    CALL RORINT(PSEC4,PSEC4,ILENF,'R')
Ce
        DO 215 JLOOP = 21,24
          KSEC4(JLOOP) = 0
  215   CONTINUE
Ce
      ELSE
Cd
Cd      Preset arrays to receive section header information to 0.
Cd      Routine GSBITE resets data array to 0.
Cd
        DO 220 JLOOP = 1,25
          KSEC1(JLOOP) = 0
  220   CONTINUE
Cd
        DO 230 JLOOP = 1,22
          KSEC2(JLOOP) = 0
  230   CONTINUE
Cd
        DO 240 JLOOP = 1,33
          KSEC4(JLOOP) = 0
  240   CONTINUE
Cd
      ENDIF
C
C     -----------------------------------------------------------------|
C
C
C
C
C
C
C
C
C
C
C*    Section 3 . Indicator Section (Section 0) of GRIB code.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
C#ifdef GRIB2
C      IF( LENCODE.AND.(KSEC0(2).EQ.2) ) THEN
C        IF( LPDEBUG ) WRITE(GRPRSM,*) 'GRIBEX: Section 3. Handle GRIB2'
C        KRET = G2ENCOD(KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
C     X                 PSEC2,PSEC3,PSEC4,PSEC3(2))
C        RETURN
C      ENDIF
C#endif
C
      IF( LPDEBUG ) WRITE(GRPRSM,*)
     X  'GRIBEX: Section 3. Handle GRIB Indicator Section (Section 0)'
C
C*    Octets 1 - 4 : The letters G R I B.
C     Four 8 bit fields.
C
      IF( LENCODE ) THEN
Ce
Ce      Insert the letters G R I B.
Ce
        CALL INXBIT(KGRIB,KLENG,INSPT,IGRIB(1),4,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 301
          WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting letters GRIB'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
      ELSE
Cd
Cd      When decoding data, check letters -> GRIB, BUDG or TIDE.
Cd
        CALL INXBIT(KGRIB,KLENG,INSPT,IPARM(1),4,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 301
          WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting letters GRIB'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Cd
Cd      Check that 'GRIB' is found where expected.
Cd
        IF( IPARM(1).EQ.71.AND.IPARM(2).EQ.82.AND.
     X      IPARM(3).EQ.73.AND.IPARM(4).EQ.66) GO TO 310
Cd
Cd      ECMWF pseudo-grib data uses 'BUDG' and 'TIDE'.
Cd
        IF( IPARM(1).EQ.66.AND.IPARM(2).EQ.85.AND.
     X      IPARM(3).EQ.68.AND.IPARM(4).EQ.71) GO TO 310
Cd
        IF( IPARM(1).EQ.84.AND.IPARM(2).EQ.73.AND.
     X      IPARM(3).EQ.68.AND.IPARM(4).EQ.69) GO TO 310
Cd
Cd      Data is not GRIB or pseudo-grib.
Cd
        KRET = 305
        WRITE(GRPRSM,*) 'GRIBEX: Input data is not GRIB or pseudo-grib.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
Cd
      ENDIF
C
  310 CONTINUE
C
C*    Octets 5 - 7 : Length of message.
C     One 24 bit field.
C
      IF( LENCODE ) THEN
Ce
Ce      When coding data, skip field. Length is inserted
Ce      later, when known.
Ce      Update bit-pointer.
Ce
        INSPT = INSPT + 24
      ELSE
Cd
Cd      Extract field.
Cd
        CALL INXBIT(KGRIB,KLENG,INSPT,ITEMP,1,IBITS, 24,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 302
          WRITE(GRPRSM,*)
     X  'GRIBEX: Error extracting length of GRIB message.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Cd
Cd      When decoding, use most-significant bit as a sign bit.
Cd
        IF( LDECODE ) CALL DSGNBT( KSEC0(1), ITEMP, 24, KRET)
Cd
Cd      If count is negative, have to rescale by factor of -120.
Cd      This is a fixup to get round the restriction on product lengths
Cd      due to the count being only 24 bits. It is only possible because
Cd      the (default) rounding for GRIB products is 120 bytes.
Cd
        IF( KSEC0(1).LT.0 ) THEN
          IF( LPDEBUG ) WRITE(GRPRSM,*)
     X      'GRIBEX: Special case, negative length multiplied by -120'
          LLARGE = .TRUE.
          KSEC0(1) = KSEC0(1) * (-120)
        ENDIF
      ENDIF
C
C*    Octet 8 : GRIB Edition Number.
C     One 8 bit field.
C
      IF( LENCODE ) THEN
Ce
Ce      Set value, if coding data.
Ce
        KSEC0(2) = JPEDNO
        CALL INXBIT(KGRIB,KLENG,INSPT,KSEC0(2),1,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 303
          WRITE(GRPRSM,*)
     X      'GRIBEX: Error inserting/extracting GRIB Edition Number.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Ce
      ELSE
Cd
Cd      Extract field.
Cd
        CALL INXBIT(KGRIB,KLENG,INSPT,KSEC0(2),1,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 303
          WRITE(GRPRSM,*)
     X      'GRIBEX: Error inserting/extracting GRIB Edition Number.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Cd
Cd      When decoding or calculating length, previous editions
Cd      of the GRIB code must be taken into account.
Cd
Cd      In the table below, covering sections 0 and 1 of the GRIB
Cd      code, octet numbering is from the beginning of the GRIB
Cd      message;
Cd      * indicates that the value is not available in the code edition;
Cd      R indicates reserved, should be set to 0;
Cd      Experimental edition is considered as edition -1.
Cd
Cd      GRIB code edition -1 has fixed length of 20 octets for
Cd      section 1, the length not included in the message.
Cd      GRIB code edition 0 has fixed length of 24 octets for
Cd      section 1, the length being included in the message.
Cd      GRIB code edition 1 can have different lengths for section
Cd      1, the minimum being 28 octets, length being included in
Cd      the message.
Cd
Cd                                              Octet numbers for code
Cd                                                       editions
Cd
Cd                     Contents.                   -1      0      1
Cd                     ---------                ----------------------
Cd           Letters GRIB                          1-4    1-4    1-4
Cd           Total length of GRIB message.          *      *     5-7
Cd           GRIB code edition number               *      *      8
Cd           Length of Section 1.                   *     5-7    9-11
Cd           Reserved octet (R).                    *      8(R)   *
Cd           Version no. of Code Table 2.           *      *     12
Cd           Identification of centre.              5      9     13
Cd           Generating process.                    6     10     14
Cd           Grid definition .                      7     11     15
Cd           Flag (Code Table 1).                   8     12     16
Cd           Indicator of parameter.                9     13     17
Cd           Indicator of type of level.           10     14     18
Cd           Height, pressure etc of levels.      11-12  15-16  19-20
Cd           Year of century.                      13     17     21
Cd           Month.                                14     18     22
Cd           Day.                                  15     19     23
Cd           Hour.                                 16     20     24
Cd           Minute.                               17     21     25
Cd           Indicator of unit of time.            18     22     26
Cd           P1 - Period of time.                  19     23     27
Cd           P2 - Period of time                  20(R)   24     28
Cd           or reserved octet (R).
Cd           Time range indicator.                21(R)   25     29
Cd           or reserved octet (R).
Cd           Number included in average.       22-23(R)  26-27  30-31
Cd           or reserved octet (R).
Cd           Number missing from average.         24(R)  28(R)   32
Cd           or reserved octet (R).
Cd           Century of data.                       *      *     33
Cd           Designates sub-centre if not 0.        *      *     34
Cd           Decimal scale factor.                  *      *    35-36
Cd           Reserved. Set to 0.                    *      *    37-48
Cd           (Need not be present)
Cd           For originating centre use only.       *      *    49-nn
Cd           (Need not be present)
Cd
Cd      Identify which GRIB code edition is being decoded.
Cd
Cd      In GRIB edition 1, the edition number is in octet 8.
Cd      In GRIB edition 0, octet 8 is reserved and set to 0.
Cd      In GRIB edition -1, octet 8 is a flag field and can have a
Cd      a valid value of 0, 1, 2 or 3.
Cd
Cd      However, GRIB edition number 0 has a fixed
Cd      length of 24, included in the message, for section 1, so
Cd      if the value extracted from octets 5-7 is 24 and that from
Cd      octet 8 is 0, it is safe to assume edition 0 of the code.
Cd
        IF( KSEC0(1).EQ.24.AND.KSEC0(2).EQ.0) THEN
Cd
Cd        Set bit-pointer back by 32 bits (4 octets).
          INSPT = INSPT - 32
Cd
Cd        Set length of GRIB message to missing data value.
          KSEC0(1) = IMISNG
Cd
          GO TO 400
        ENDIF
Cd
Cd      In GRIB Edition -1, octets 22 and 23 are reserved and set
Cd      to 0. These octets in Edition 1 are the month and the day,
Cd      and must be non-zero.
Cd
        ITEMP = 168
        CALL INXBIT(KGRIB,KLENG,ITEMP,IMODAY,1,IBITS, 16,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 304
          WRITE(GRPRSM,*)
     X  'GRIBEX: Error extracting octets 22 and 23 for'
          WRITE(GRPRSM,*) 'GRIBEX: Experimental Edition check.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Cd
        IF( IMODAY.EQ.0) THEN
Cd
Cd        Set bit-pointer back by 32 bits (4 octets).
          INSPT = INSPT - 32
Cd
          KSEC0(2) = -1
Cd
Cd        Set length of GRIB message to missing data value.
Cd
          KSEC0(1) = IMISNG
Cd
Cd        Set length of section 1 of GRIB code to 20 octets.
Cd
          ILEN1 = 20
Cd
Cd        Skip next 4 octets, as they do not exist in
Cd        the Experimental Edition of the code. ie
Cd        length of Section and Table 2 Version Number.
Cd
          GO TO 401
        ENDIF
Cd
      ENDIF
C
C*    If Grib Edition 1 and only length is required, go to section 9.
C
      IF( YFUNC.EQ.'L') GO TO 900
C
C     -----------------------------------------------------------------|
C
C
C
C
C
C
C
C
C
C
C*    Section 4 . Product Definition Section (Section 1) of GRIB code.
C    -----------------------------------------------------------------
C
  400 CONTINUE
C
      IF( LPDEBUG ) WRITE(GRPRSM,*)
     X  'GRIBEX: Section 4. Handle Product Definition (Section 1)'
C
C*    Check consistency of values given, with GRIB code, if required.
C
      IF( NVCK.EQ.1.AND. LENCODE ) THEN
        CALL GRCHK1(KSEC1,KRET)
        IF( KRET.NE.0) THEN
          KRET = 499
          WRITE(GRPRSM,*)
     X  'GRIBEX: Error found when checking values for'
          WRITE(GRPRSM,*) 'GRIBEX: Section 1 against valid GRIB values.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
      ENDIF
C
C*    Octets 1 - 3 : Length of Section.
C     One 24 bit field.
C
      IF( LENCODE ) THEN
Ce
Ce      Set value, if coding data.
        ILEN1 = JPLEN1
Ce
Ce      Extra octets if ECMWF local definition usage in section 1.
Ce
        LECMWF = (KSEC1(2) .EQ.98)
        LECLOC = (KSEC1(2) .EQ.98) .OR.
     X           (KSEC1(22).EQ.98)
        LSST11 = (KSEC1( 6).EQ.139) .AND.
     X           (KSEC1( 7).EQ.  1) .AND.
     X           (KSEC1(22).EQ. 98)
        LKWBCE = (KSEC1( 2).EQ.  7) .AND.
     X           (KSEC1(22).NE. 98)
Ce
        IF( LPDEBUG ) WRITE(GRPRSM,*)
     X  'GRIBEX: Local extension in use for...'
        IF( LPDEBUG.AND.LECLOC ) WRITE(GRPRSM,*)
     X  'GRIBEX: ECMWF data'
        IF( LPDEBUG.AND.LKWBCE ) WRITE(GRPRSM,*)
     X  'GRIBEX: KWBC ensemble data'
        IF( LPDEBUG.AND.LSST11 ) WRITE(GRPRSM,*) 'GRIBEX: SST data'
Ce
      ENDIF
C
C     Insert / extract field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,ILEN1,1,IBITS, 24,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 401
        WRITE(GRPRSM,*)
     X    'GRIBEX: Error inserting/extracting length of Section 1.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
C*    Octet 4  : Version Number of Table 2.
C     One 8 bit field.
C
C     Insert / extract field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC1(1),1,IBITS, 8,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 402
        WRITE(GRPRSM,*)
     X    'GRIBEX: Error inserting/extracting Param Table Version No.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
  401 CONTINUE
C
C*    Check Edition number.
C
      IF( LPDEBUG ) WRITE(GRPRSM,*)
     X  'GRIBEX: GRIB Edition Number = ',KSEC0(2)
      IF( KSEC0(2).GT.1) THEN
        KRET = 413
        WRITE(GRPRSM,*) 'GRIBEX: Grib Edition not catered for.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
C*    Print length of Section 1, if required.
C
      IF( LPDEBUG ) WRITE(GRPRSM,*)
     X  'GRIBEX: Length of Section 1 of GRIB = ',ILEN1,' octets.'
C
C*    Octet 5  : Identification of centre.
C     Octet 6  : Generating process identification.
C     Octet 7  : Grid definition.
C     Octet 8  : Flag.
C     Octet 9  : Indicator of parameter.
C     Octet 10 : Indicator of type of level.
C                (or satellite identifier)
C                Satellite useage as defined by INPE/CPTEC
C                and used by ECMWF, pending final definition
C                by WMO.
C     Six 8 bit fields.
C
C     Insert / extract fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC1(2),6,IBITS, 8,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 403
        WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting six fields from'
        WRITE(GRPRSM,*)
     X  'GRIBEX: Identification of Centre to Indicator of'
        WRITE(GRPRSM,*) 'GRIBEX: type of level.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
C     If decoding, fix-up for Experimental Edition and Edition 0
C     of GRIB code.
C
      IF( .NOT. LENCODE ) THEN
Cd
Cd      Set logical flag for ECMWF data
Cd
        LECMWF = (KSEC1(2).EQ.98)
Cd
Cd      In GRIB Experimental Edition and Edition 0
Cd      the International Table Version Number in use was 0.
Cd      ECMWF has always used its own local table. It is the same
Cd      for Experimental Edition, Edition 0 and Edition 1 and is
Cd      local table number 128.
Cd
        IF( KSEC0(2).LT.1) THEN
          IF( LECMWF ) THEN
Cd
Cd          ECMWF data. Local table number.
            KSEC1(1) = 128
          ELSE
Cd
Cd          International table number.
Cd          KSEC1(1) = 0 is already preset.
          ENDIF
        ENDIF
Cd
Cd      Fix-up for flag field, which was different in Experimental
Cd      Edition.
Cd
Cd                        Experimental          Editions 0 and 1
Cd                        Edition
Cd
Cd           Sections     Binary    Decimal     Binary    Decimal
Cd           included     value     value       value     value
Cd
Cd             none       00000000    0         00000000     0
Cd              2         00000001    1         10000000   128
Cd              3         00000010    2         01000000    64
Cd           2 and 3      00000011    3         11000000   192
Cd
        IF( KSEC0(2).EQ.-1) THEN
          IF( KSEC1(5).EQ.1) KSEC1(5) = 128
          IF( KSEC1(5).EQ.2) KSEC1(5) = 64
          IF( KSEC1(5).EQ.3) KSEC1(5) = 192
        ENDIF
      ENDIF
      LSECT2 = KSEC1(5).GE.128
      LSECT3 = MOD(KSEC1(5),128).GE.64
C
C*    Once the flag field has been extracted, no further fields
C     from section 1 of the GRIB code are required, when length
C     of GRIB or pseudo-Grib message only is required.
C
      IF( YFUNC.EQ.'L') THEN
C
C       Length of section 0 + section 1 is 28 octets (224 bits)
C       for GRIB Edition 0 and 24 octets (192 bits) for
C       Experimental edition.
C       Set bit-pointer and jump to extraction of length of section 3.
C
        INSPT = 224
        IF( KSEC0(2).EQ.-1) INSPT = 192
        GO TO 500
      ENDIF
C
C*    Octets 11 - 12 : Height, pressure etc of levels or
C                      satellite spectral band.
C                      Satellite useage as defined by INPE/CPTEC
C                      and used by ECMWF, pending final definition
C                      by WMO.
C     One 16 bit field or two 8 bit fields.
C
C     Satellite image if WMO table and parameter 127
C
      LSATIMG = ((KSEC1(1).LT.128).AND.(KSEC1(6).EQ.127))
C
C     For certain level types, no description is necessary, so when
C     decoding the fields are set to 0 regardless of bit string content.
C
      IF( (.NOT.LENCODE).AND.
     X    (.NOT.LSATIMG).AND.
     X    ( ((KSEC1(7).LT.100).AND.(KSEC1(7).NE.20)).OR.
     X      (KSEC1(7).EQ.102) .OR.
     X      (KSEC1(7).EQ.200) .OR.
     X      (KSEC1(7).EQ.201) ) ) THEN
Cd
Cd      Update bit-pointer and skip extraction.
        KSEC1(8) = 0
        KSEC1(9) = 0
        INSPT = INSPT + 16
        GO TO 402
      ENDIF
C
C     Certain level types require that the description occupies
C     both octets.
C     Satellite Spectral band occupies both octets.
C     Satellite useage as defined by INPE/CPTEC
C     and used by ECMWF, pending final definition by WMO.
C     Image data uses 2 octets.
C
      IF( (KSEC1(7).NE. 20).AND.
     X    (KSEC1(7).NE.100).AND.
     X    (KSEC1(7).NE.103).AND.
     X    (KSEC1(7).NE.105).AND.
     X    (KSEC1(7).NE.107).AND.
     X    (KSEC1(7).NE.109).AND.
     X    (KSEC1(7).NE.111).AND.
     X    (KSEC1(7).NE.113).AND.
     X    (KSEC1(7).NE.115).AND.
     X    (KSEC1(7).NE.117).AND.
     X    (KSEC1(7).NE.125).AND.
     X    (KSEC1(7).NE.127).AND.
     X    (KSEC1(7).NE.160).AND.
     X    (KSEC1(7).NE.210).AND.
     X    (.NOT.LSATIMG) ) THEN
C
C       Two 8 bit fields.
        INUM  = 2
        IBLEN = 8
      ELSE
C
C       One 16 bit field.
        INUM     = 1
        IBLEN    = 16
        KSEC1(9) = 0
      ENDIF
C
C     Insert / extract fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC1(8),INUM,IBITS,
     X             IBLEN,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 404
        WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting Height,'
        WRITE(GRPRSM,*) 'GRIBEX: pressure, etc of levels.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
C*    Fix-up for ECMWF upper-air data incorrectly coded in Experimental
C     Edition.
C
      IF( KSEC0(2).EQ.-1.AND. LECMWF ) THEN
        ITEMP = INSPT - 16
        INUM  = 2
        IBLEN = 8
        CALL INXBIT(KGRIB,KLENG,ITEMP,KSEC1(8),INUM,IBITS,
     X               IBLEN,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 404
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting Height,'
          WRITE(GRPRSM,*) 'GRIBEX: pressure, etc of levels.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
        KSEC1(8) = KSEC1(8) * 32 + KSEC1(9)
        KSEC1(9) = 0
      ENDIF
C
  402 CONTINUE
C
C*    Octet 13 : Year of century.
C     Octet 14 : Month.
C     Octet 15 : Day.
C     Octet 16 : Hour.
C     Octet 17 : Minute.
C     Octet 18 : Indicator of unit of time range..
C     Six 8 bit fields.
C
C     Insert / extract fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC1(10),6,IBITS, 8,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 405
        WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting six fields from'
        WRITE(GRPRSM,*)
     X  'GRIBEX: Year of century to Indicator of unit of'
        WRITE(GRPRSM,*) 'GRIBEX: time range.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
C*    Fix-up for unit of time, which was different in Experimental
C     Edition.
C
C
C                  Experimental          Editions 0 and 1
C                  Edition
C
C     Meaning      Decimal               Decimal
C                  value                 value
C
C     Minute       0 or 30                 0
C     Hour         1 or 40                 1
C     Day          2 or 50                 2
C     Month        3 or 60                 3
C     Year         4 or 70                 4
C     Decade       5 or 80                 5
C     Normal         6                     6
C     Century      7 or 90                 7
C     Second         -                   254
C
      IF( KSEC0(2).EQ.-1) THEN
        IF( KSEC1(15).EQ.90) KSEC1(15) = 7
        IF( KSEC1(15).GT.10) KSEC1(15) = (KSEC1(15) / 10) - 3
      ENDIF
C
C*    Octets 19 - 20 : Period of time.
C     One 16 bit field or two 8 bit fields.
C
      IF( LENCODE .AND.KSEC1(18).EQ.10) THEN
Ce
Ce      One 16 bit field.
        INUM  = 1
        IBLEN = 16
      ELSE
Ce
Ce      Two 8 bit fields.
        INUM  = 2
        IBLEN = 8
      ENDIF
C
C     Insert / extract fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC1(16),INUM,IBITS,
     X            IBLEN,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 406
        WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting Period of time.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
C*    Octet 21 : Time range indicator.
C     One 8 bit field.
C
C     Insert / extract field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC1(18),1,IBITS, 8,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 407
        WRITE(GRPRSM,*)
     X    'GRIBEX: Error inserting/extracting time range indicator.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
C*    When decoding, period of time field and time range
C     indicator may need modification.
C
      IF( .NOT. LENCODE ) THEN
Cd
Cd      When decoding, length of period of time field is known
Cd      only at this time. If a 16 bit field is indicated, put
Cd      the two extracted 8-bit fields together.
Cd
        IF( KSEC1(18).EQ.10) THEN
Cd
Cd        One 16 bit field.
          KSEC1(16) = KSEC1(16) * 256 + KSEC1(17)
          KSEC1(17) = 0
        ENDIF
Cd
Cd      If data is known to be initialised analysis and GRIB is
Cd      Experimental Edition, set time range indicator flag.
Cd
        IF( KSEC1(16).EQ.0.AND.HOPER.EQ.'S'.AND.KSEC0(2).EQ.-1)
     X    KSEC1(18) = 1
Cd
      ENDIF
C
C*    Octet 22 - 23 : Number averaged.
C     One 16 bit field.
C
C     Insert / extract field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC1(19),1,IBITS, 16,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 408
        WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting number averaged.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
C*    Octet 24 : Number missing from averages etc.
C     One 8 bit field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC1(20),1,IBITS, 8,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 409
        WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting number missing'
        WRITE(GRPRSM,*) 'GRIBEX: from averages etc.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
C*    This is the end of Section 1 if Edition 0 or -1 of GRIB code.
C     Set other fields to be compatible with Edition 1, where possible.
C
      IF( KSEC0(2).LT.1) THEN
C
C       Century of data.
C
C       All ECMWF data in Edition 0 or -1 is 20th century.
C       Otherwise set century to missing data value.
C
        IF( LECMWF ) THEN
          KSEC1(21) = 20
        ELSE
          KSEC1(21) = IMISNG
        ENDIF
C
C       Reserved field and decimal scale factor field (which
C       was always 0).
C
C       KSEC1(22) and KSEC1(23) set to 0.
C
        KSEC1(22) = 0
        KSEC1(23) = 0
        GO TO 499
      ENDIF
C
C*    Octet 25 : Century of data.
C     One 8 bit field.
C
C     Insert / extract field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC1(21),1,IBITS, 8,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 410
        WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting century of data'
        WRITE(GRPRSM,*) 'GRIBEX: or reserved field.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
C*    Octet 26 : Sub-centre.
C     ( Now used at ECMWF/Washington to designate a sub-centre
C       =   2 for NMC ensemble products(decoding only).
C       =  98 for EPS products from Washington with ECMWF extensions.
C       = 231 Meteo France Climate Centre (ECSN)
C       = 232 MPI Climate Centre (ECSN)
C       = 233 UKMO Climate Centre (ECSN)
C       = 240 for ECMWF seasonal forecast centre (PROVOST).
C       = 241 for Meteo France seasonal forecast centre (PROVOST).
C       = 242 for EDF seasonal forecast centre (PROVOST).
C       = 243 for UKMO seasonal forecast centre (PROVOST).
C       = 0   otherwise. )
C
C     One 8 bit field.
C
      IF( LENCODE ) THEN
Ce
Ce      When coding ECMWF data, sub-centre is set to 0 unless a
Ce      designated sub-centre or explicitly enabled by previous user
Ce      call to GRSUBC.
Ce
        IF( .NOT.LECMWF .OR. NSUBCE.EQ.1 ) THEN
          ITEMP = KSEC1(22)
Ce
        ELSE IF( (KSEC1(22).EQ. 98) .OR.
     X           (KSEC1(22).EQ.231) .OR.
     X           (KSEC1(22).EQ.232) .OR.
     X           (KSEC1(22).EQ.233) .OR.
     X           (KSEC1(22).EQ.240) .OR.
     X           (KSEC1(22).EQ.241) .OR.
     X           (KSEC1(22).EQ.242) .OR.
     X           (KSEC1(22).EQ.243) )THEN
          ITEMP = KSEC1(22)
Ce
        ELSE
          ITEMP = 0
        ENDIF
      ENDIF
C
      CALL INXBIT(KGRIB,KLENG,INSPT, ITEMP, 1,IBITS, 8,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 810
        WRITE(GRPRSM,*) 'GRIBEX: Error inserting dummy zero.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
Cd
      IF( LDECODE ) THEN
Cd
Cd      When decoding, KSEC1(22) is set to message content.
Cd
        KSEC1(22) = ITEMP
Cd
Cd      Set flags for known special local extensions to section 1:
Cd        ECMWF data
Cd        Washington SST data
Cd        Washington ensemble data
Cd
        LECLOC = (KSEC1(2).EQ.98) .OR.
     X           (KSEC1(22).EQ.98)
        LSST11 = (KSEC1( 6).EQ.139) .AND.
     X           (KSEC1( 7).EQ.  1) .AND.
     X           (KSEC1(22).EQ. 98)
        LKWBCE = (KSEC1( 2).EQ.  7) .AND.
     X           (KSEC1(22).NE. 98)
Cd
        IF( LPDEBUG ) WRITE(GRPRSM,*)
     X  'GRIBEX: Local extension in use for...'
        IF( LPDEBUG.AND.LECLOC ) WRITE(GRPRSM,*) 'GRIBEX: ECMWF data'
        IF( LPDEBUG.AND.LKWBCE ) WRITE(GRPRSM,*)
     X  'GRIBEX: KWBC ensemble data'
        IF( LPDEBUG.AND.LSST11 ) WRITE(GRPRSM,*) 'GRIBEX: SST data'
Cd
      ENDIF
C
C*    Octets 27 - 28 : Units decimal scale factor.
C     One 16 bit field.
C
C     When coding, set sign bit if value is negative.
C
      IF( LENCODE ) CALL CSGNBT( ITEMP, KSEC1(23), 16, KRET)
C
C     Insert / extract field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,ITEMP,1,IBITS, 16,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 411
        WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting units decimal'
        WRITE(GRPRSM,*) 'GRIBEX: scale factor.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
C     When decoding, set sign bit if value is negative.
C
      IF( LDECODE ) CALL DSGNBT( KSEC1(23), ITEMP, 16, KRET)
C
      IF( LENCODE ) THEN
Ce
Ce      When coding data, the reserved octets 29-40 need not
Ce      be present and are not included, unless ECMWF local
Ce      use of octets 41 onwards is indicated.
Ce
        IF( KSEC1(24).EQ.1.AND.((LECLOC.OR.LSST11).OR.LKWBCE)) THEN
Ce
Ce        Fill reserved octets with 0.
Ce
          DO 410 JLOOP = 29,40
            CALL INXBIT(KGRIB,KLENG,INSPT, 0, 1,IBITS, 8,YFUNC,KRET)
            IF( KRET.NE.0) THEN
              KRET = 810
              WRITE(GRPRSM,*) 'GRIBEX: Error inserting dummy zero.'
              WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
              GO TO 900
            ENDIF
  410     CONTINUE
Ce
Ce        Have to set sectionLength for KWBC ensemble products to
Ce        force proper encoding
Ce
          IF( LKWBCE ) THEN
            IF( KSEC1(42).NE.0) THEN
              IF( KSEC1(46).NE.0) THEN
#ifdef linux
                ISECLEN = (256 + 86)*256*256
              ELSE
                ISECLEN = (256 + 60)*256*256
              ENDIF
#else
                ISECLEN = 86*256 + 1
              ELSE
                ISECLEN = 60*256 + 1
              ENDIF
#endif
            ENDIF
            KGRIB(3) = ISECLEN
          ENDIF
Ce
Ce        Insert local ECMWF data.
          KRET = NOABORT
          CALL ECLOC1(YFUNC,KSEC1,KGRIB,INSPT,IBITS,KRET)
          IF( KRET.NE.0) THEN
            KRET = 412
            WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting ECMWF local'
            WRITE(GRPRSM,*) 'GRIBEX: data.'
            WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
            GO TO 900
          ENDIF
Ce
        ENDIF
Ce
      ELSE
Cd
Cd      When decoding data the reserved octets are skipped.
Cd      If local use is not ECMWF usage or Washington ensemble usage,
Cd      the local use octets are also skipped.
Cd      Presence is indicated by length of section > 28 octets.
Cd
        IF( ILEN1.GT.28.AND. (LECLOC.OR.LKWBCE.OR.LSST11) ) THEN
Cd
Cd        Set decoded values as zero and move pointer past
Cd        reserved octets.
Cd
          DO 420 JLOOP = 25,36
            KSEC1(JLOOP) = 0
  420     CONTINUE
          INSPT = INSPT + 96
Cd
Cd        Set flag to indicate local use.
          KSEC1(24) = 1
Cd
Cd        Extract local ECMWF data.
Cd        .. or Washington ensemble data.
Cd
          KRET = NOABORT
          IF( (LECLOC.OR.LSST11).OR.LKWBCE )
     X      CALL ECLOC1(YFUNC,KSEC1,KGRIB,INSPT,IBITS,KRET)
          IF( KRET.NE.0) THEN
            KRET = 412
            WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting ECMWF or'
            WRITE(GRPRSM,*) 'GRIBEX: Washington local data.'
            WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
            GO TO 900
          ENDIF
Cd
        ELSE
          IF( ILEN1.GT.28) INSPT = INSPT + (ILEN1-28) * 8
        ENDIF
Cd
      ENDIF
C
  499 CONTINUE
C
C*    Check for ECMWF pseudo-grib data. This saves calling GRIBEX
C     with function 'I' to check if the data is GRIB data, and another
C     call with function 'D' when GRIB data is found.
C
      IF( (.NOT.LENCODE).AND.(KSEC1(6).EQ.127.OR.KSEC1(6).EQ.128)) THEN
        IF( KSEC1(1).EQ.128.AND. LECMWF ) THEN
Cd
          IPSEUD = -6
Cd
Cd        Change function to 'L' so that section 0 is fully decoded.
          YFUNC = 'L'
          GO TO 500
        ENDIF
      ENDIF
C
C     -----------------------------------------------------------------|
C
C
C
C
C
C
C
C
C
C
C*    Section 5 . Grid Description Section (Section 2) of GRIB code.
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
      IF( LPDEBUG ) WRITE(GRPRSM,*)
     X  'GRIBEX: Section 5. Handle Grid Description Section (Section 2)'
C
      IF( .NOT.LSECT2 ) THEN
C
        IF( LDECODE ) THEN
Cd
Cd      If decoding data, set section 2 values to missing data indicator
Cd
          DO 505 JLOOP=1,22
            KSEC2(JLOOP) = IMISNG
  505     CONTINUE
        ENDIF
Cd
Cd    Go to section 6, if no grid description included.
Cd
        GO TO 600
      ENDIF
C
C*    Check consistency of values given, with GRIB code, if required.
C
      IF( NVCK.EQ.1.AND. LENCODE ) THEN
        CALL GRCHK2 (KSEC1,KSEC2,PSEC2,KRET)
        IF( KRET.NE.0) THEN
          KRET = 599
          WRITE(GRPRSM,*)
     X  'GRIBEX: Error found when checking values for'
          WRITE(GRPRSM,*)
     X  'GRIBEX: Section 2 against valid GRIB values.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
      ENDIF
C
C*    Octets 1 - 3 : Length of section.
C     One 24 bit field.
C
C     Calculate length of section, if coding data.
C
      IF( LENCODE ) THEN
Ce
Ce      Length is normally 32 + stretched and/or rotated
Ce      parameters + vertical coordinate parameters + list of
Ce      numbers of points.
Ce      (Lambert conformal and Mercator are 42 octets in length,
Ce      while Space view is 40 for ECMWF (44 in GRIB specification)
Ce
Ce      Ordinary Grid.
Ce
        INC = 0
Ce
Ce      Space view perspective.
Ce
        IF( KSEC2(1).EQ.90) INC = 12
Ce
Ce      Lambert or Mercator +10.
Ce
        IF( KSEC2(1).EQ.3.OR.KSEC2(1).EQ.1) INC = 10
Ce
Ce
Ce      Rotated grid.
Ce      (Gaussian, Latitude/longitude or Spherical Harmonics)
Ce
        IF( KSEC2(1).EQ.10.OR.KSEC2(1).EQ.14.OR.KSEC2(1).EQ.60) INC = 10
Ce
Ce      Stretched grid.
Ce      (Gaussian, Latitude/longitude or Spherical Harmonics)
Ce
        IF( KSEC2(1).EQ.20.OR.KSEC2(1).EQ.24.OR.KSEC2(1).EQ.70) INC = 10
Ce
Ce      Stretched and rotated grid.
Ce      (Gaussian, Latitude/longitude or Spherical Harmonics)
Ce
        IF( KSEC2(1).EQ.30.OR.KSEC2(1).EQ.34.OR.KSEC2(1).EQ.80) INC = 20
Ce
Ce      Calculate data offset allowing for quasi-regular grid
Ce      (Polar sterographic cannot be a quasi-regular grid)
Ce
        LQUASI = ( KSEC2(17).EQ.1 ) .AND. (KSEC2(1).NE.5)
        IF( LQUASI ) THEN
          ILEN2 = 32 + INC + (KSEC2(12)*4) + 2*KSEC2(3)
        ELSE
          ILEN2 = 32 + INC + (KSEC2(12)*4)
        ENDIF
Ce
      ENDIF
C
C     Insert / extract field.
      CALL INXBIT(KGRIB,KLENG,INSPT,ILEN2,1,IBITS, 24,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 501
        WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
        WRITE(GRPRSM,*) 'GRIBEX: length of Section 2.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
C*    Print length of Section 2, if required.
C
      IF( LPDEBUG ) WRITE(GRPRSM,*)
     X  'GRIBEX: Length of Section 2 of GRIB = ',ILEN2,' octets.'
C
C*    If only length is required, update bit-pointer and jump
C     to extraction of length of section 3.
C
      IF( YFUNC.EQ.'L') THEN
        INSPT = INSPT -24 + ILEN2 * 8
        GO TO 600
      ENDIF
C
C*    Octet 4 : NV - number of vertical coordinate parameters.
C     One 8 bit field.
C
C     Insert / extract field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(12),1,IBITS, 8,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 502
        WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
        WRITE(GRPRSM,*)
     X  'GRIBEX: number of Vertical coordinate parameters.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
C*    Fixup for Editions -1 and 0 of GRIB code, where number
C     of Vertical Coordinate Parameters must be calculated,
C     as this octet contained the number of unused bits at the
C     end of the section, which by definition of the section
C     always had to be 0.
C
      IF( KSEC0(2).LT.1) THEN
        KSEC2(12) = ( ILEN2 - 32 ) / 4
      ENDIF
C
C*    Octet 5 : PV - location of list of vertical coordinate parameters,
C                    if any,
C               or
C               PL - location of list of numbers of points, if no PV,
C               or
C               255 - no PV or PL.
C     One 8 bit field.
C
      IF( LENCODE ) THEN
Ce
Ce      Set value, if coding data.
Ce
Ce      Neither present is default.
        IPVPL = 255
Ce
Ce      Vertical coordinate parameters present.
        IF( KSEC2(12).NE.0) IPVPL = 32 + INC + 1
Ce
Ce      List of number of points present, if no vertical
Ce      coordinate parameters present and if quasi-regular grid.
Ce
        IF( LQUASI.AND.KSEC2(12).EQ.0) IPVPL = 32 + INC + 1
Ce
Ce      Insert field.
Ce
        CALL INXBIT(KGRIB,KLENG,INSPT,IPVPL,1,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 503
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*)
     X  'GRIBEX: location of List of vertical coordinate'
          WRITE(GRPRSM,*)
     X  'GRIBEX: parameters or List of numbers of points.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Ce
      ELSE
Cd
Cd      If decoding data.  Extract field.
Cd
        CALL INXBIT(KGRIB,KLENG,INSPT,IPVPL,1,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 503
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*)
     X  'GRIBEX: location of List of vertical coordinate'
          WRITE(GRPRSM,*)
     X  'GRIBEX: parameters or List of numbers of points.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Cd
Cd      Experimental space view perspective data  received
Cd      at ECMWF has all 0 bits.
Cd      0 is illegal for all data types, so change it.
Cd
        IF( IPVPL.EQ.0) IPVPL = 255
Cd
Cd      Neither present, so set regular grid indicator.
Cd
        IF( IPVPL.EQ.255) THEN
          KSEC2(17) = 0
          LQUASI = .FALSE.
        ENDIF
Cd
Cd      Vertical coordinate parameters present.
Cd      If the length of section is greater than the
Cd      end of the vertical coordinate parameters, then
Cd      there is a list of numbers of points following, so
Cd      set quasi-regular grid indicator.
Cd
        IF( KSEC2(12).NE.0) THEN
          IPL = 4 * KSEC2(12) + IPVPL - 1
          IF( IPL.LT.ILEN2) THEN
            KSEC2(17) = 1
            LQUASI = .TRUE.
          ENDIF
        ENDIF
Cd
Cd
Cd      List of number of points present, no vertical
Cd      coordinate parameters present, so set quasi-regular
Cd      grid indicator.
Cd
        IF( KSEC2(12).EQ.0.AND.IPVPL.NE.255) THEN
          KSEC2(17) = 1
          LQUASI = .TRUE.
        ENDIF
Cd
Cd      Fixup for Editions -1 and 0 of GRIB code, where
Cd      all grids were regular.
Cd
        IF( KSEC0(2).LT.1) THEN
          KSEC2(17) = 0
          LQUASI = .FALSE.
        ENDIF
Cd
      ENDIF
C
C*    Octet 6 : Data representation type.
C     One 8 bit field.
C
C     Insert / extract field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(1),1,IBITS, 8,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 504
        WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
        WRITE(GRPRSM,*) 'GRIBEX: data representation type.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
      LSPHERC = .FALSE.
      LGRDPT = .TRUE.
C
C****
C     Gaussian grid definition.
C****
C
      IF( KSEC2(1).EQ.4. .OR.
     X    KSEC2(1).EQ.14 .OR.
     X    KSEC2(1).EQ.24 .OR.
     X    KSEC2(1).EQ.34     ) THEN
C
        IF( LENCODE ) THEN
          KRET = EGGSEC2(KGRIB,KLENG,INSPT,KSEC0,KSEC2,IBITS,LQUASI)
          IF( KRET.NE.0 ) THEN
            WRITE(GRPRSM,*)
     X  'GRIBEX: Error encoding gaussian grid section 2'
            KRET = 540
            GOTO 900
          ENDIF
        ELSE
          KRET = DGGSEC2(KGRIB,KLENG,INSPT,KSEC0,KSEC2,IBITS,IMISNG)
          IF( KRET.NE.0 ) THEN
            WRITE(GRPRSM,*)
     X  'GRIBEX: Error decoding gaussian grid section 2'
            KRET = 541
            GOTO 900
          ENDIF
        ENDIF
C
        GO TO 520
      ENDIF
C
C****
C     Latitude/longitude grid definition,
C     Equidistant Cylindrical or Plate Carree.
C****
C
      IF( KSEC2(1).EQ.0  .OR.
     X    KSEC2(1).EQ.10 .OR.
     X    KSEC2(1).EQ.20 .OR.
     X    KSEC2(1).EQ.30     ) THEN
C
        IF( LENCODE ) THEN
          KRET = ELLSEC2(KGRIB,KLENG,INSPT,KSEC0,KSEC2,IBITS,LQUASI)
          IF( KRET.NE.0 ) THEN
            WRITE(GRPRSM,*)
     X  'GRIBEX: Error encoding lat/long grid section 2'
            KRET = 542
            GOTO 900
          ENDIF
        ELSE
          KRET = DLLSEC2(KGRIB,KLENG,INSPT,KSEC0,KSEC2,IBITS,IMISNG)
          IF( KRET.NE.0 ) THEN
            WRITE(GRPRSM,*)
     X  'GRIBEX: Error decoding lat/long grid section 2'
            KRET = 543
            GOTO 900
          ENDIF
        ENDIF
C
        GO TO 520
      ENDIF
C
C****
C     Spherical Harmonic format.
C****
C
      IF( KSEC2(1).EQ.50 .OR.
     X    KSEC2(1).EQ.60 .OR.
     X    KSEC2(1).EQ.70 .OR.
     X    KSEC2(1).EQ.80     ) THEN
C
        LSPHERC = .TRUE.
        LGRDPT = .FALSE.
C
        IF( LENCODE ) THEN
          KRET = ESHSEC2(KGRIB,KLENG,INSPT,KSEC0,KSEC2,IBITS)
          IF( KRET.NE.0 ) THEN
            WRITE(GRPRSM,*) 'GRIBEX: Error encoding spectral section 2'
            KRET = 544
            GOTO 900
          ENDIF
        ELSE
          KRET = DSHSEC2(KGRIB,KLENG,INSPT,KSEC2,IBITS)
          IF( KRET.NE.0 ) THEN
            WRITE(GRPRSM,*) 'GRIBEX: Error decoding spectral section 2'
            KRET = 545
            GOTO 900
          ENDIF
        ENDIF
C
        ITRUNC = KSEC2(2)
C
        GOTO 520
      ENDIF
C
C****
C*    Polar Stereographic.
C****
C
      IF( KSEC2(1).EQ.5) THEN
C
C*      Octets 7 - 8  : Ni - number of points along X-axis.
C       Octets 9 - 10 : Nj - number of points along Y-axis.
C       Two 16 bit fields.
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(2),2,IBITS, 16,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 523
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*)
     X  'GRIBEX: number of points along X or Y axis.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C*      Octets 11 - 13 : La1 - latitude of first grid point.
C       Octets 14 - 16 : Lo1 - longitude of first grid point.
C       Two 24 bit fields.
C
C       When coding data, set sign bit to 1, if value is negative.
C
        IF( LENCODE ) THEN
          CALL CSGNBT( ILALO(1), KSEC2(4), 24, KRET)
          CALL CSGNBT( ILALO(2), KSEC2(5), 24, KRET)
        ENDIF
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,ILALO(1),2,IBITS, 24,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 506
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*)
     X  'GRIBEX: latitude or longitude of first grid pt.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C       When decoding data, if sign bit is 1, value is negative.
C
        IF( LDECODE ) THEN
          CALL DSGNBT( KSEC2(4), ILALO(1), 24, KRET)
          CALL DSGNBT( KSEC2(5), ILALO(2), 24, KRET)
        ENDIF
C
C*      Octet 17 : Resolution and components flag.
C       One 8 bit field.
C
C       Resolution flag ( KSEC2(6) ) is not applicable.
C
        KSEC2(6) = 0
        IF( LENCODE ) IRESOL = KSEC2(18)+KSEC2(19)
C
C       Insert / extract field.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,IRESOL,1,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 507
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*) 'GRIBEX: components flag.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
        IF( LDECODE ) THEN
Cd
Cd        All flag fields are already set to 0, so
          IF( IRESOL.EQ.0) GO TO 513
Cd
Cd        Fix up for flag which was different in Experimental edition.
Cd
          IF( KSEC0(2).EQ.-1.AND.(IRESOL.EQ.1.OR.IRESOL.EQ.3))
     X      IRESOL = 128
Cd
Cd        Resolution flag is not applicable.
          IF( IRESOL.GE.128) IRESOL = IRESOL - 128
Cd
Cd        Set earth flag.
          IF( IRESOL.GE.64) THEN
            KSEC2(18) = 64
            IRESOL    = IRESOL - 64
          ENDIF
Cd
Cd        Set components flag.
          KSEC2(19) = IRESOL
Cd
        ENDIF
C
  513   CONTINUE
C
C       Insert / extract field.
C
C       CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(6),1,IBITS, 8,YFUNC,KRET)
C       IF( KRET.NE.0) THEN
C         KRET = 507
C         WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
C         WRITE(GRPRSM,*) 'GRIBEX: components flag.'
C         WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
C         GO TO 900
C       ENDIF
C
C*      Octets 18 - 20 : LoV - orientation of the grid.
C       One 24 bit field.
C
C       When coding data, set sign bit to 1, if value is negative.
C
        IF( LENCODE ) CALL CSGNBT( ILALO(1), KSEC2(7), 24, KRET)
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,ILALO(1),1,IBITS, 24,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 508
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*)
     X  'GRIBEX: latitude or longitude of last grid point.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C       When decoding data, if sign bit is 1, value is negative.
C
        IF( LDECODE ) CALL DSGNBT( KSEC2(7), ILALO(1), 24, KRET)
C
C*      Octets 21 - 23 : Dx - X direction grid length.
C*      Octets 24 - 26 : Dy - Y direction grid length.
C       Two 24 bit fields.
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(9),2,IBITS, 24,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 524
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*) 'GRIBEX: X or Y axis grid length.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C*      Octet 27 : Projection centre flag.
C       One 8-bit field.
C
C       Insert / extract field.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(13),1,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 525
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*) 'GRIBEX: Projection centre flag.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C*      Octet 28 : Scanning mode flags.
C       One 8 bit field.
C
C       Insert / extract field.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(11),1,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 511
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*) 'GRIBEX: scanning mode flags.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C       Fix-up for flag which was different in Experimental Edition.
        IF( KSEC0(2).EQ.-1.AND.KSEC2(11).EQ.1) KSEC2(11) = 0
C
C*      Octets 29 - 32 : Reserved.
C       Two 16 bit fields.
C
C       If insertion, set bits to 0.
        IF( LENCODE ) THEN
          CALL INXBIT(KGRIB,KLENG,INSPT, 0, 1,IBITS, 32,YFUNC,KRET)
          IF( KRET.NE.0) THEN
            KRET = 810
            WRITE(GRPRSM,*) 'GRIBEX: Error inserting dummy zero.'
            WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
            GO TO 900
          ENDIF
        ELSE
Cd
Cd        If extraction, only update bit pointer.
          INSPT = INSPT + 32
        ENDIF
C
        GO TO 520
      ENDIF
C
C****
C*    Space view perspective or orthographic.
C****
C
      IF( KSEC2(1).EQ.90) THEN
C
        IF( LENCODE ) THEN
          KRET = ESVSEC2(KGRIB,KLENG,INSPT,KSEC2,IBITS,ILEN2)
          IF( KRET.NE.0 ) THEN
            WRITE(GRPRSM,*) 'GRIBEX: Error encoding spectral section 2'
            KRET = 546
            GOTO 900
          ENDIF
        ELSE
          KRET = DSVSEC2(KGRIB,KLENG,INSPT,KSEC2,IBITS,ILEN2)
          IF( KRET.NE.0 ) THEN
            WRITE(GRPRSM,*) 'GRIBEX: Error decoding spectral section 2'
            KRET = 547
            GOTO 900
          ENDIF
        ENDIF
C
C
        GO TO 520
C
      ENDIF
C
C****
C*    Mercator
C****
C
      IF( KSEC2(1).EQ.1) THEN
C
        IF( LENCODE ) THEN
          KRET = EMESEC2(KGRIB,KLENG,INSPT,KSEC0,KSEC2,IBITS,LQUASI)
          IF( KRET.NE.0 ) THEN
            WRITE(GRPRSM,*) 'GRIBEX: Error encoding Mercator section 2'
            KRET = 546
            GOTO 900
          ENDIF
        ELSE
          KRET = DMESEC2(KGRIB,KLENG,INSPT,KSEC0,KSEC2,IBITS,IMISNG)
          IF( KRET.NE.0 ) THEN
            WRITE(GRPRSM,*) 'GRIBEX: Error decoding Mercator section 2'
            KRET = 547
            GOTO 900
          ENDIF
        ENDIF
C
C
        GO TO 520
C
      ENDIF
C
C****
C*    Lambert Conformal.
C****
C
      IF( KSEC2(1).EQ.3) THEN
C
C*      Octets 7 - 8  : Ni - number of points along X-axis.
C       Octets 9 - 10 : Nj - number of points along Y-axis.
C       Two 16 bit fields.
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(2),2,IBITS, 16,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 523
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*)
     X  'GRIBEX: number of points along X or Y axis.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C*      Octets 11 - 13 : La1 - latitude of first grid point.
C       Octets 14 - 16 : Lo1 - longitude of first grid point.
C       Two 24 bit fields.
C
Ce      When coding data, set sign bit to 1, if value is negative.
Ce
        IF( LENCODE ) THEN
          CALL CSGNBT( ILALO(1), KSEC2(4), 24, KRET)
          CALL CSGNBT( ILALO(2), KSEC2(5), 24, KRET)
        ENDIF
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,ILALO(1),2,IBITS, 24,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 506
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*)
     X  'GRIBEX: latitude or longitude of first grid pt.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
Cd      When decoding data, if sign bit is 1, value is negative.
Cd
        IF( LDECODE ) THEN
          CALL DSGNBT( KSEC2(4), ILALO(1), 24, KRET)
          CALL DSGNBT( KSEC2(5), ILALO(2), 24, KRET)
        ENDIF
C
C*      Octet 17 : Resolution and components flag.
C       One 8 bit field.
C
C       Resolution flag ( KSEC2(6) ) is not applicable.
C
        IF( LENCODE ) IRESOL = KSEC2(18)+KSEC2(19)+KSEC2(6)
C
C       Insert / extract field.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,IRESOL,1,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 507
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*) 'GRIBEX: components flag.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
        IF( LDECODE ) THEN
Cd
Cd        If all flag fields are already set to 0, bypass.
Cd
          IF( IRESOL.EQ.0) GO TO 515
Cd
Cd        Resolution flag is not applicable.
Cd
          IF( IRESOL.GE.128) THEN
            KSEC2(6) = 128
            IRESOL   = IRESOL - 128
          ENDIF
Cd
Cd        Set earth flag.
Cd
          IF( IRESOL.GE.64) THEN
            KSEC2(18) = 64
            IRESOL    = IRESOL - 64
          ENDIF
Cd
Cd        Set components flag.
Cd
          KSEC2(19) = IRESOL
Cd
        ENDIF
C
  515   CONTINUE
C
C       Insert / extract field.
C
C*      Octets 18 - 20 : LoV - orientation of the grid.
C       One 24 bit field.
C
Ce      When coding data, set sign bit to 1, if value is negative.
Ce
        IF( LENCODE ) CALL CSGNBT( ILALO(1), KSEC2(7), 24, KRET)
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,ILALO(1),1,IBITS, 24,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 508
          WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*)
     X  'GRIBEX: latitude or longitude of last grid point.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
Cd      When decoding data, if sign bit is 1, value is negative.
Cd
        IF( LDECODE ) CALL DSGNBT( KSEC2(7), ILALO(1), 24, KRET)
C
C*      Octets 21 - 23 : Dx - X direction grid length.
C*      Octets 24 - 26 : Dy - Y direction grid length.
C       Two 24 bit fields.
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(9),2,IBITS, 24,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 524
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*) 'GRIBEX: X or Y axis grid length.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C*      Octet 27 : Projection centre flag.
C       One 8-bit field.
C
C       Insert / extract field.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(13),1,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 525
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*) 'GRIBEX: Projection centre flag.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C*      Octet 28 : Scanning mode flags.
C       One 8 bit field.
C
C       Insert / extract field.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(11),1,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 511
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*) 'GRIBEX: scanning mode flags.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C       Fix-up for flag which was different in Experimental
C            Edition.
C
        IF( KSEC0(2).EQ.-1.AND.KSEC2(11).EQ.1) KSEC2(11) = 0
C
C*      Octets 29 - 31 : Latin 1.
C*      Octets 32 - 34 : Latin 2.
C       Two 24 bit fields.
C
Ce      When coding data, set sign bit to 1, if value is negative.
Ce
        IF( LENCODE ) THEN
          CALL CSGNBT( ILALO(1), KSEC2(14), 24, KRET)
          CALL CSGNBT( ILALO(2), KSEC2(15), 24, KRET)
        ENDIF
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,ILALO(1),2,IBITS, 24,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 531
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*)
     X  'GRIBEX: Latin1 or Latin2 of secants points.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
Cd      When decoding data, if sign bit is 1, value is negative.
C
        IF( LDECODE ) THEN
          CALL DSGNBT( KSEC2(14), ILALO(1), 24, KRET)
          CALL DSGNBT( KSEC2(15), ILALO(2), 24, KRET)
        ENDIF
C
C*      Octets 35 - 37 : Latitude of the southern pole.
C*      Octets 38 - 40 : Longitude of the southern pole.
C       Two 24 bit fields.
C
Ce      When coding data, set sign bit to 1, if value is negative.
Ce
        IF( LENCODE ) THEN
          CALL CSGNBT( ILALO(1), KSEC2(20), 24, KRET)
          CALL CSGNBT( ILALO(2), KSEC2(21), 24, KRET)
        ENDIF
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,ILALO(1),2,IBITS, 24,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 532
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*)
     X  'GRIBEX: latitude/longitude of southern pole.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
Cd      When decoding data, if sign bit is 1, value is negative.
Cd
        IF( LDECODE ) THEN
          CALL DSGNBT( KSEC2(20), ILALO(1), 24, KRET)
          CALL DSGNBT( KSEC2(21), ILALO(2), 24, KRET)
        ENDIF
C
C*      Octets 41 - 42 : Reserved.
C       One 16 bit fields.
C
        IF( LENCODE ) THEN
C
Ce        All bits already set to 0.
Ce        No insertion, only update bit pointer.
Ce
          INSPT = INSPT + 16
Ce
        ELSE
Cd
Cd        No extraction, only update bit pointer.
Cd
          INSPT = INSPT + 16
Cd
        ENDIF
C
        GO TO 520
      ENDIF
C
C
C****
C     Ocean grid definition.
C****
C
      IF( KSEC2(1).EQ.192 ) THEN
C
        IF( LENCODE ) THEN
          KRET = EOCSEC2(KGRIB,KLENG,INSPT,KSEC2,IBITS)
          IF( KRET.NE.0 ) THEN
            WRITE(GRPRSM,*)
     X  'GRIBEX: Error encoding ECMWF ocean section 2'
            KRET = 546
            GOTO 900
          ENDIF
        ELSE
          KRET = DOCSEC2(KGRIB,KLENG,INSPT,KSEC2,IBITS)
          IF( KRET.NE.0 ) THEN
            WRITE(GRPRSM,*)
     X  'GRIBEX: Error decoding ECMWF ocean section 2'
            KRET = 547
            GOTO 900
          ENDIF
        ENDIF
        GOTO 520
C
      ENDIF
C
C****
C*    Other representation types not yet catered for.
C****
C
      KRET = 598
      WRITE(GRPRSM,*)
     X  'GRIBEX: Representation type not catered for:',KSEC2(1)
      WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
      GO TO 900
C
C*    Rotation parameters for rotated or stretched and rotated grids.
C     (Gaussian, Latitude/longitude or Spherical Harmonics)
C
  520 CONTINUE
C
      IF( KSEC2(1).EQ.10.OR.KSEC2(1).EQ.30.OR.
     X    KSEC2(1).EQ.14.OR.KSEC2(1).EQ.34.OR.
     X    KSEC2(1).EQ.60.OR.KSEC2(1).EQ.80) THEN
C
C*      Octets 33 - 35 : Latitude of the southern pole.
C       Octets 36 - 38 : Longitude of the southern pole.
C       Two 24 bit fields.
C
C       When coding data, set sign bit to 1, if value is negative.
C
        IF( LENCODE ) THEN
          CALL CSGNBT( ILALO(1), KSEC2(13), 24, KRET)
          CALL CSGNBT( ILALO(2), KSEC2(14), 24, KRET)
        ENDIF
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,ILALO(1),2,IBITS, 24,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 517
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*)
     X  'GRIBEX: latitude or longitude of southern pole.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C       When decoding data, if sign bit is 1, value is negative.
C
        IF( LDECODE ) THEN
          CALL DSGNBT( KSEC2(13), ILALO(1), 24, KRET)
          CALL DSGNBT( KSEC2(14), ILALO(2), 24, KRET)
        ENDIF
C
C*      Octets 39 - 42 : Angle of rotation.
C       One 8 bit and one 24 bit field.
C
        IF( LENCODE ) THEN
Ce
Ce        Convert floating point to GRIB representation.
          ITRND = 1
          CALL CONFP3 (PSEC2(1),IEXP,IMANT,IBITS,ITRND)
        ENDIF
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,IEXP,1,IBITS, 8,YFUNC,KRETA)
        CALL INXBIT(KGRIB,KLENG,INSPT,IMANT,1,IBITS, 24,YFUNC,KRETB)
        KRET = KRET + KRETB
        IF( KRET.NE.0) THEN
          KRET = 518
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*) 'GRIBEX: angle of rotation.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
        IF( LDECODE ) THEN
Cd
Cd        Convert GRIB representation to floating point.
          CALL DECFP2 (PSEC2(1),IEXP,IMANT)
Cd
        ENDIF
      ENDIF
C
C*    Stretching parameters for stretched grids.
C     (Gaussian, Latitude/longitude or Spherical Harmonics)
C
      IF( KSEC2(1).EQ.20.OR.KSEC2(1).EQ.24.OR.KSEC2(1).EQ.70) THEN
C
C*      Octets 33 - 35 : Latitude of pole of stretching.
C       Octets 36 - 38 : Longitude of pole of stretching.
C       Two 24 bit fields.
C
C       When coding data, set sign bit to 1, if value is negative.
C
        IF( LENCODE ) THEN
          CALL CSGNBT( ILALO(1), KSEC2(15), 24, KRET)
          CALL CSGNBT( ILALO(2), KSEC2(16), 24, KRET)
        ENDIF
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,ILALO(1),2,IBITS, 24,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 519
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*)
     X  'GRIBEX: latitude/longitude of pole of stretching.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C       When decoding data, if sign bit is 1, value is negative.
C
        IF( LDECODE ) THEN
          CALL DSGNBT( KSEC2(15), ILALO(1), 24, KRET)
          CALL DSGNBT( KSEC2(16), ILALO(2), 24, KRET)
        ENDIF
C
C*      Octets 39 - 42 : Stretching factor.
C       One 8 bit and one 24 bit field.
C
C       Convert floating point to GRIB representation.
        ITRND = 1
        IF( LENCODE ) CALL CONFP3 (PSEC2(2),IEXP,IMANT,IBITS,ITRND)
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,IEXP,1,IBITS, 8,YFUNC,KRETA)
        CALL INXBIT(KGRIB,KLENG,INSPT,IMANT,1,IBITS, 24,YFUNC,KRETB)
        KRET = KRETA + KRETB
        IF( KRET.NE.0) THEN
          KRET = 520
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*) 'GRIBEX: stretching factor.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C       Convert GRIB representation to floating point.
        IF( LDECODE ) CALL DECFP2 (PSEC2(2),IEXP,IMANT)
C
      ENDIF
C
C*    Stretching parameters for stretched and rotated grids.
C     (Gaussian, Latitude/longitude or Spherical Harmonics)
C
      IF( KSEC2(1).EQ.30.OR.KSEC2(1).EQ.34.OR.KSEC2(1).EQ.80) THEN
C
C*      Octets 43 - 45 : Latitude of pole of stretching.
C       Octets 46 - 48 : Longitude of pole of stretching.
C       Two 24 bit fields.
C
C       When coding data, set sign bit to 1, if value is negative.
C
        IF( LENCODE ) THEN
          CALL CSGNBT( ILALO(1), KSEC2(15), 24, KRET)
          CALL CSGNBT( ILALO(2), KSEC2(16), 24, KRET)
        ENDIF
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,ILALO(1),2,IBITS, 24,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 519
          WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*)
     X  'GRIBEX: latitude/longitude of pole of stretching.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C       When decoding data, if sign bit is 1, value is negative.
C
        IF( LDECODE ) THEN
          CALL DSGNBT( KSEC2(15), ILALO(1), 24, KRET)
          CALL DSGNBT( KSEC2(16), ILALO(2), 24, KRET)
        ENDIF
C
C*      Octets 49 - 52 : Stretching factor.
C       One 8 bit and one 24 bit field.
C
C       Convert floating point to GRIB representation.
        ITRND = 1
        IF( LENCODE ) CALL CONFP3 (PSEC2(2),IEXP,IMANT,IBITS,ITRND)
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,IEXP,1,IBITS, 8,YFUNC,KRETA)
        CALL INXBIT(KGRIB,KLENG,INSPT,IMANT,1,IBITS, 24,YFUNC,KRETB)
        KRET = KRETA + KRETB
        IF( KRET.NE.0) THEN
          KRET = 520
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*) 'GRIBEX: stretching factor.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C       Convert GRIB representation to floating point.
C
        IF( LDECODE ) CALL DECFP2 (PSEC2(2),IEXP,IMANT)
C
      ENDIF
C
C*    Vertical coordinate parameters, if any.
C
      IF( KSEC2(12).NE.0) THEN
        ITRND = 1
        DO 530 JLOOP = 1 , KSEC2(12)
C
C         One 8 bit and one 24 bit field.
C
Ce        Convert floating point to GRIB representation.
          IF( LENCODE )
     X      CALL CONFP3 (PSEC2(JLOOP+10),IEXP,IMANT, IBITS,ITRND)
C
C         Insert / extract fields.
C
          CALL INXBIT(KGRIB,KLENG,INSPT,IEXP,1,IBITS, 8,YFUNC,KRETA)
          CALL INXBIT(KGRIB,KLENG,INSPT,IMANT,1,IBITS, 24,YFUNC,KRETB)
          KRET = KRETA + KRETB
          IF( KRET.NE.0) THEN
            KRET = 521
            WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
            WRITE(GRPRSM,*) 'GRIBEX: vertical coordinate parameters.'
            WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
            GO TO 900
          ENDIF
C
Cd        Convert GRIB representation to floating point.
          IF( LDECODE ) CALL DECFP2 (PSEC2(JLOOP+10),IEXP,IMANT)
C
  530   CONTINUE
      ENDIF
C
C*    List of number of points, if any.
C     Number of 16 bit fields.
C
      IF( LQUASI) THEN
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,KSEC2(23),KSEC2(3),IBITS,
     X               16,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 522
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting'
          WRITE(GRPRSM,*) 'GRIBEX: list of numbers of points.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
      ENDIF
C
C     -----------------------------------------------------------------|
C
C
C
C
C
C
C
C
C
C
C*    Section 6 . Bit Map Section (section 3) of GRIB code.
C     -----------------------------------------------------------------|
C
  600 CONTINUE
C
      IF( LPDEBUG ) WRITE(GRPRSM,*)
     X  'GRIBEX: Section 6. Handle Bit Map Section (Section 3)'
C
C*    Go to section 9, if decoding of identification sections only and
C     GRIB Code Edition is higher than 0. If Edition is lower the
C     length of the GRIB message needs to be calculated, so change
C     function to 'L' to complete decoding of section 0.
C     Number of data values decoded ( KSEC4(1) ) already set to 0.
C
      IF( HOPER(1:1).EQ.'I') THEN
        IF( KSEC0(2).GT.0) THEN
          GO TO 900
        ELSE
          YFUNC = 'L'
        ENDIF
      ENDIF
C
C*    Set integer or real missing data value.
C
      IF( KSEC4(5) .EQ. 0 )  THEN
        ZMSVAL = PSEC3(2)
      ELSE
        ZMSVAL = REAL(KSEC3(2))
      ENDIF
C
C*    Go to section 7, if no bit map required.
C
      IF( .NOT.LSECT3 ) GO TO 700
C
C*    Set bit-map flag and attempt no decoding of bit-map, if
C     routine has been called by the DECOGB interface routine,
C     which is provided for upward compatibility with old software.
C
      IF( HOPER.EQ.'Z') THEN
        ISBMAP = -5
        WRITE(GRPRSM,*) 'GRIBEX: Bit-map found. No data decoded.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', ISBMAP
        GO TO 900
      ENDIF
C
C*    Check consistency of values given, with GRIB code, if required.
C
      IF( NVCK.EQ.1.AND. LENCODE ) THEN
        CALL GRCHK3 (KSEC1,KSEC3,PSEC3,KRET)
        IF( KRET.NE.0) THEN
          KRET = 699
          WRITE(GRPRSM,*)
     X  'GRIBEX: Error found checking values for section 3'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
      ENDIF
C
C*    When coding data, calculate the length of section and number
C     of unused bits.
C
      IF( LENCODE ) THEN
Ce
        IF( KSEC3(1).NE.0) THEN
Ce
Ce        Predetermined bit-map table included.
Ce        Length of section is 6 octets, number of unused bits is 0.
          ILEN3 = 6
          INUB  = 0
Cjdc      KRET = GBITMAP(KSEC3(1), IBTVALS, NONMISS, IBTPTR, NBYTE)
Cjdc      IVALS = NONMISS
Ce
        ELSE
Ce
Ce        Bit-map included in section 3.
Ce        Length of section = 6 octets of header + length of
Ce        bit-map, rounded to a multiple of 2 octets.
Ce
Ce        Set IVALS to the number of bits in the bit-map.
          IF( KSEC4(8).EQ.0) THEN
Ce
Ce          Each bit in the bit-map represents a single value.
            IVALS = ILENF
          ELSE
Ce
Ce          Each bit in the bit-map represents a matrix of values.
            IVALS = ILENF / (KSEC4(50)*KSEC4(51))
          ENDIF
Ce
          ITEMP = 48 + IVALS
          ILEN3 = ( ITEMP + 15 ) / 16
          ILEN3 = ILEN3 * 2
Ce
Ce        Number of unused bits.
          INUB = ILEN3 * 8 - ITEMP
Ce
        ENDIF
Ce
      ENDIF
C
C*    Octets 1 - 3 : Length of section.
C     One 24 bit field.
C
C     Insert/extract field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,ILEN3,1,IBITS, 24,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 601
          WRITE(GRPRSM,*)
     X      'GRIBEX: Error inserting/extracting section 3 length'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C*    Print length of Section 3, if required.
C
      IF( LPDEBUG ) WRITE(GRPRSM,*)
     X  'GRIBEX: Length of Section 3 of GRIB = ',ILEN3,' octets.'
C
C     If only length is required, update bit-pointer and jump
C     to extraction of length of section 4.
C
      IF( YFUNC.EQ.'L') THEN
        INSPT = INSPT -24 + ILEN3 * 8
        GO TO 700
      ENDIF
C
C*    Octet 4 : Number of unused bits at end of section.
C     One 8 bit field.
C
C     Insert/extract field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,INUB,1,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 602
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting number'
          WRITE(GRPRSM,*)
     X  'GRIBEX: unused bits at the end of Section 3.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C*    Octets 5-6 : Bit-map table reference.
C     One 16 bit field.
C
C     Insert/extract field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,KSEC3(1),1,IBITS, 16,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 603
        WRITE(GRPRSM,*)
     X    'GRIBEX: Error inserting/extracting bit-map reference table'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
C*    Set integer or real missing data value.
C
      IF( KSEC4(5) .EQ. 0 )  THEN
        ZMSVAL = PSEC3(2)
      ELSE
        ZMSVAL = REAL(KSEC3(2))
      ENDIF
C
C*    Finished if a predetermined bit-map table is given.
C
      IF( KSEC3(1).NE.0) GO TO 700
C
C*    Bit-map definition included.
C
      IF( LENCODE ) THEN
Ce
Ce      Insert primary bit-map. Set function for fixed length
Ce      messages if required.
Ce
        YTEMP = YFUNC
        IF( HOPER.EQ.'M') YTEMP = 'M'
Ce
        IF( KSEC4(8).EQ.64) THEN
Ce
Ce        Matrix of values at a point.
          ITEMP = KSEC4(50) * KSEC4(51)
        ELSE
Ce
Ce        Single value at each point.
          ITEMP = 1
        ENDIF
Ce
Ce      Retain pointer to bit-map location.
Ce
        IBMAP = INSPT
Ce
        CALL INSMP1 (KGRIB,KLENG,INSPT,KSEC4(9),PSEC4,
     X               ILENF,IBITS,ZMSVAL,YTEMP,ITEMP,NDBG,KRET)
        IF( KRET.NE.0) THEN
          KRET = 604
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting primary bit map.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Ce
Ce      Number of data values remaining to be handled
Ce      is now in ILENF, which is used when finding maximum and
Ce      minimum values etc. When a matrix of values is present
Ce      PSEC4 may still contain missing data indicators for each
Ce      matrix.
Ce
Ce      Effective number of points will be returned in KSEC4(21).
Ce
        NONMISS = ILENF
Ce
Ce      Unused bits at end of section.
Ce      These bits are already set to 0, so update bit-pointer only.
Ce
        INSPT = INSPT + INUB
Ce
      ELSE
Cd
Cd      Retain pointer to bit-map location.
Cd
        IBMAP = INSPT
Cd
Cd      IVALS is the number of bits in the bit-map. It is the same
Cd      as the number of data values (including missing data
Cd      values to be decoded) when each point represents a single
Cd      data value. When each bit in the bit-map represents a
Cd      matrix of values, it is the number of matrices.
Cd
        IVALS = (ILEN3 - 6) * 8 - INUB
Cd
Cd      Update bit-pointer to start of section 4 of Grib message.
        INSPT = INSPT - 48 + ILEN3 * 8
Cd
      ENDIF
C
C     -----------------------------------------------------------------|
C
C
C
C
C
C
C
C
C
C
C*    Section 7 . Binary Data Section (section 4) of GRIB code.
C     -----------------------------------------------------------------|
C
  700 CONTINUE
C
      IF( LPDEBUG ) WRITE(GRPRSM,*)
     X  'GRIBEX: Section 7. Handle Binary Data Section (Section 4)'
C
C*    Check consistency of values given, with GRIB code, if required.
C
      IF( NVCK.EQ.1.AND. LENCODE ) THEN
        CALL GRCHK4 (KSEC1,KSEC4,PSEC4,KRET)
        IF( KRET.NE.0) THEN
          KRET = 799
          WRITE(GRPRSM,*)
     X      'GRIBEX: Error found when checking values for section 4'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
      ENDIF
C
C*    Retain value of the pointer to the first bit in Section 4.
C     When coding data, this is needed later to insert the length
C     of Section 4 and the number of unused bits.
C     When decoding data, this is used later to calculate the
C     number of packed data values which have to be decoded.
C
      IPLEN = INSPT
C
C*    Octets 1 - 3 : Length of section.
C     One 24 bit field.
C
      IF( LENCODE ) THEN
Ce
Ce      Increment pointer.
        INSPT = INSPT + 24
      ELSE
Cd
Cd      Extract field.
Cd
        CALL INXBIT(KGRIB,KLENG,INSPT,ILEN4,1,IBITS, 24,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 701
          WRITE(GRPRSM,*) 'GRIBEX: Error extracting section 4 length'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Cd
Cd      If a very large product, the section 4 length field holds
Cd      the number of bytes in the product after section 4 upto
Cd      the end of the padding bytes.
Cd      This is a fixup to get round the restriction on product lengths
Cd      due to the count being only 24 bits. It is only possible because
Cd      the (default) rounding for GRIB products is 120 bytes.
Cd

Cljbf   in case of big fields, revert the
Cljbf   workaround in decoding in case we realize it
Cljbf   is not paked using the *120 trick
Cljbf

        IF( LLARGE .AND. ILEN4.GT.120) THEN
          KSEC0(1) = KSEC0(1) / 120
          KSEC0(1) = KSEC0(1) * 2
          LLARGE = .FALSE.
        ENDIF

        IF( LLARGE ) ILEN4 = KSEC0(1) - (INSPT/8) - ILEN4 + 3
Cd
Cd      Print length of Section 4, if required.
Cd
        IF( LPDEBUG ) WRITE(GRPRSM,*)
     X    'GRIBEX: Length of Section 4 of GRIB = ',ILEN4,' octets.'
Cd
Cd      Set length if required, by updating bit-pointer and
Cd      adding length of section 5 (32 bits). Length in bytes.
Cd      Finished if length only required, so go to section 9.
Cd
        IF( YFUNC.EQ.'L'.OR.HOPER.EQ.'X') THEN
          KSEC0(1) = (INSPT - 24 + ILEN4 * 8 + 32) / 8
          IF( YFUNC.EQ.'L') GO TO 900
        ENDIF
Cd
      ENDIF
C
C*    Octet 4 : 4 bit flag field and 4 bit unused bit count field.
C     One 8 bit field for insertion/extraction purposes.
C
      IF( LENCODE ) THEN
Ce
Ce      Type of data (spherical harmonic coefficients or grid
Ce      point) is taken from KSEC4(3) only if no Section 2 is
Ce      included.(This allows for data coded without Section 2).
Ce
        IF( LSECT2 ) THEN
Ce
Ce        If section 2 is present, it says if data is spherical harmonic
Ce
          IF( LSPHERC ) THEN
            KSEC4(3) = 128
          ELSE
            KSEC4(3) = 0
          ENDIF
Ce
        ELSE
Ce
Ce        Section 4 says if it's spherical harmonic data..
Ce
          LSPHERC = ( KSEC4(3) .EQ. 128 )
          LGRDPT  = .NOT.LSPHERC
        ENDIF
Ce
Ce      When coding data, field is inserted later, when
Ce      number of unused bits is known and added to it.
Ce
Ce      Increment pointer.
        INSPT = INSPT + 8
Ce
      ELSE
Cd
Cd      Decoding -> extract field.
Cd
        CALL INXBIT(KGRIB,KLENG,INSPT,IFLAG,1,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 706
          WRITE(GRPRSM,*)
     X  'GRIBEX: Error extracting section 4 flag field.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Cd
Cd      All flags already preset to 0.
Cd
        IF( KSEC0(2).EQ.-1) THEN
Cd
Cd        In the Experimental Edition flag field was
Cd        0000 for grid point data.
Cd        0001 for spherical harmonic data.
Cd
          ITEMP = IFLAG / 16
          INIL  = IFLAG - ITEMP * 16
          IF( ITEMP.NE.0) KSEC4(3) = 128
          GO TO 710
        ENDIF
Cd
        IF( KSEC0(2).EQ.0) THEN
Cd
Cd        In Edition 0 flag field only 2 first bits had significance.
Cd
Cd        0- for grid point data.
Cd        1- for spherical harmonic data.
Cd        -0 for simple packing.
Cd        -1 for complex packing (only supported for spectral data).
Cd
          IF( IFLAG.GE.128) THEN
            KSEC4(3) = 128
            IFLAG    = IFLAG - 128
          ENDIF
Cd
          IF( IFLAG.GE.64) THEN
            KSEC4(4) = 64
            IFLAG    = IFLAG - 64
          ENDIF
Cd
          INIL  = IFLAG
        ELSEIF( KSEC0(2).EQ.1) THEN
Cd
Cd        In Edition 1 flag field all 4 bits have significance.
Cd
Cd        0--- for grid point data.
Cd        1--- for spherical harmonic data.
Cd        -0-- for simple packing.
Cd        -1-- for complex or second order packing.
Cd        --0- for floating point values.
Cd        --1- for integer values.
Cd        ---0 for no additional flags at Octet 14.
Cd        ---1 for additional flags at Octet 14.
Cd
          IF( IFLAG.GE.128) THEN
            KSEC4(3) = 128
            IFLAG    = IFLAG - 128
          ENDIF
Cd
          IF( IFLAG.GE.64) THEN
            KSEC4(4) = 64
            IFLAG    = IFLAG - 64
          ENDIF
Cd
          IF( IFLAG.GE.32) THEN
            KSEC4(5) = 32
            IFLAG    = IFLAG - 32
          ENDIF
Cd
          IF( IFLAG.GE.16) THEN
            KSEC4(6) = 16
            IFLAG    = IFLAG - 16
          ENDIF
Cd
          INIL  = IFLAG
        ENDIF
Cd
  710   CONTINUE
Cd
Cd      Type of data (spherical harmonic coefficients or grid
Cd      point) is taken from KSEC4(3) only if no Section 2 is
Cd      included.(This allows for data coded without Section 2).
Cd
        IF( .NOT.LSECT2 ) THEN
          LSPHERC = ( KSEC4(3) .EQ. 128 )
          LGRDPT  = .NOT.LSPHERC
        ELSEIF( LSPHERC ) THEN
          KSEC4(3) = 128
        ELSE
          KSEC4(3) = 0
        ENDIF
Cd
Cd      Print number of unused bits, if required.
Cd
        IF( LPDEBUG ) WRITE(GRPRSM,*)
     X    'GRIBEX: Number of unused bits is ',INIL,'.'
Cd
      ENDIF
C
C*    Complex packing supported for spherical harmonics.
      LCOMPLX = (LSPHERC .AND. ( KSEC4(4).EQ.64 ) ) .OR.
     X          (LSPHERC .AND. LSECT2 .AND. ( KSEC2(6).EQ.2 ) )
C
C     Check input specification is consistent
      IF( LCOMPLX .AND. LSECT2 ) THEN
        IF( ( KSEC4(4).NE.64 ) .AND. ( KSEC2(6).EQ.2 )  ) THEN
           WRITE(GRPRSM,*) ' COMPLEX mismatch. KSEC4(4) = ', KSEC4(4)
           WRITE(GRPRSM,*) ' COMPLEX mismatch. KSEC2(6) = ', KSEC2(6)
           KRET = 807
           GOTO 900
        ELSEIF( ( KSEC4(4).EQ.64 ) .AND. ( KSEC2(6).NE.2 )  ) THEN

           WRITE(GRPRSM,*) ' COMPLEX mismatch. KSEC4(4) = ', KSEC4(4)
           WRITE(GRPRSM,*) ' COMPLEX mismatch. KSEC2(6) = ', KSEC2(6)
           KRET = 807
           GOTO 900
        ENDIF
      ELSEIF( LCOMPLX ) THEN
C
C         Truncation of full spectrum, which is supposed triangular,
C         has to be diagnosed. Define also sub-set truncation.
C
        IF (LENCODE) THEN
Ce
          ISUBSET = KSEC4(18)
Ce
Ce        When encoding, use the total number of data.
Ce
          ITEMP = KSEC4(1)
          ITRUNC= ( NINT ( SQRT ( REAL ( 4 * ITEMP + 1 ) ) ) - 3 ) / 2
Ce
        ELSE
Cd
Cd        When decoding, use section 4 descriptors.
Cd
          INSPT=INSPT+6*8
          CALL INXBIT(KGRIB,KLENG,INSPT,KSEC4(2),1,IBITS, 8,YFUNC,KRET)
          IF( KRET.NE.0) THEN
          KRET = 709
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting number of'
          WRITE(GRPRSM,*) 'GRIBEX: bits per data value'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
            GO TO 900
          ENDIF
Cd
          CALL INXBIT(KGRIB,KLENG,INSPT,ITEMP,1,IBITS,16,YFUNC,KRET)
          IF( KRET.NE.0) THEN
            KRET = 796
            WRITE(GRPRSM,*)
     X 'GRIBEX: Error extracting pointer to packed data.'
            WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
            GO TO 900
          ENDIF
Cd
          INSPT=INSPT+16
          CALL INXBIT(KGRIB,KLENG,INSPT,ISUBSET,1,IBITS, 8,YFUNC,KRET)
          IF( KRET.NE.0) THEN
            KRET = 797
            WRITE(GRPRSM,*)
     X 'GRIBEX: Error extracting sub-set truncation.'
            WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
            GO TO 900
          ENDIF
Cd
          IF (KSEC0(2).GE.1) THEN
            ITEMP = ILEN4 * 8 - INIL + (ITEMP-IPLEN/8) * 8
            ITEMP = ( ITEMP - 32 * ( ISUBSET + 1 ) * (ISUBSET + 2 ) )
     X               / KSEC4(2)
            ITRUNC= ( NINT ( SQRT ( REAL ( 4 * ITEMP + 1 ) ) ) - 3 ) / 2
          ELSE
            ITEMP = ILEN4 * 8 - INIL + (ITEMP-1) * 8
            ITEMP = ( ITEMP - 32 * ( ISUBSET + 1 ) ** 2 ) / KSEC4(2)
            ITRUNC= NINT ( SQRT ( REAL ( ITEMP ) ) ) - 1
          ENDIF
Cd
        ENDIF
C
        IF( LPDEBUG) WRITE(GRPRSM,*)
     X    'GRIBEX: diagnosed truncation of full spectrum is ', ITRUNC
C
      ELSE
        ISUBSET = 0
      ENDIF
C
C *******************************************
C
C     Handle complex packing of section 4.
C     (Avoid decoding of section 4 if 'I' or 'J' option)
C
      IF( LCOMPLX.AND.(.NOT.L_IORJ) ) THEN
C
C       Adjust bit pointer to start of section
        INSPT = IPLEN
C
        IF( LENCODE) THEN
C
C         Encoding ...
          KRET = CSECT4( PSEC4, ITRUNC, KSEC1, KSEC4,
     X                   KGRIB, KLENG, INSPT, IBITS, KSEC4(2))
        ELSE
C
C         Decoding ...
          KRET = DSECT4A( PSEC4, ITRUNC, KSEC0, KSEC1, KSEC4,
     X                   KGRIB, KLENG, INSPT, IBITS, KSEC4(2))
C
C         Special case when no.bits per packed value = no.bits per word
          IF( KRET .EQ. 16123 ) KRET = 205
        ENDIF
C
C       Give up if error reported.
        IF( KRET .NE. 0 ) GOTO 900
C
C       Got to deal with section 5 if OK.
        GOTO 799
      ENDIF
C
C *******************************************
C
C     Complex packing ("Second order packing") for grid point.
C
      IF( LGRDPT .AND. (KSEC4(4).NE.0.OR.HOPER.EQ.'K') ) THEN
C
        IF( LENCODE .AND. KSEC4(6).EQ.0 ) THEN
          KRET = 703
          WRITE(GRPRSM,*)
     X      'GRIBEX: Second-order packing implies additional flags'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ELSEIF( HOPER.EQ.'G'.OR.HOPER.EQ.'A'.OR.HOPER.EQ.'B' ) THEN
          KRET = 704
          WRITE(GRPRSM,*) 'GRIBEX: Function ',HOPER,' invalid for'
          WRITE(GRPRSM,*)
     X  'GRIBEX: second-order packed field (grid-point).'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
C
        ELSEIF( LDECODE ) THEN
Cd
Cd        Decoding ...
Cd
          INSPT=IPLEN
          KRET = D2ORDR( PSEC4, KLENP, KSEC1, KSEC2, KSEC3, KSEC4,
     X                   KGRIB, KLENG, INSPT, IBITS, HOPER, ILEN4,
     X                   INIL,  ZREF, ZSCALE, IBMAP, IVALS, NDBG )
Cd
Cd        Give up if error reported.
          IF( KRET .NE. 0 ) GOTO 900
Cd
Cd        At this stage, PSEC4 contains normalized INTEGER values.
Cd
          IFPT=0
          ILEN=KSEC4(1)
Cd
          IF( L_IORJ ) THEN
            KSEC4(34)=INSPT
Cd
Cjdc        IF( HOPER.EQ.'J' ) GOTO 815
            GOTO 900
          ELSE
            GOTO 796
          ENDIF
C
        ENDIF
C
C       Encoding is performed later, once the normalization of
C       field values into normalized integer values is performed.
C
      ENDIF
C
C*    Set IFPT to the number of data values stored in floating point
C     rather than packed format.
C     For simple packing of data in spherical harmonic
C     format the first word contains the real (0,0) coefficient,
C     which is treated separately. IFPT is 1 for spherical
C     harmonics, 0 for other data.
C
      IF( LGRDPT ) THEN
        IFPT = 0
      ELSE
        IFPT = 1
      ENDIF
C
C*    Octets 5 - 6 : Scale factor.
C     One 16 bit field.
C
C     Calculate scale factor, if coding data.
C
      IF( LENCODE ) THEN
Ce
Ce      Change units of data values , if required.
Ce
        IF( KSEC1(23).NE.0) THEN
          DO 720 JLOOP = 1 , ILENF
            PSEC4(JLOOP) = PSEC4(JLOOP)*(10.0**KSEC1(23))
  720     CONTINUE
        ENDIF
Ce
Ce      Find maximum and minimum values in data array, ignoring
Ce      any missing-data values, if secondary bit-maps are
Ce      indicated. Values not being packed are skipped.
Ce
        ILEN = ILENF - IFPT
Ce
        IF( KSEC4(8).EQ.64.AND.KSEC4(9).EQ.32) THEN
Ce
Ce        Secondary bit maps present.
Ce
          CALL MAXMN2 (PSEC4(IFPT+1),ILEN,ZMSVAL,ZMAX,ZMIN)
Ce
        ELSE
Ce
Ce        No secondary bit maps present.
Ce
          IF( HOPER.NE.'A' ) CALL MAXMIN(PSEC4(IFPT+1),ILEN,ZMAX,ZMIN)
Ce
        ENDIF
Ce
        IF( LPDEBUG ) THEN
          WRITE(GRPRSM,*) 'GRIBEX: Maximum value calculated = ', ZMAX
          WRITE(GRPRSM,*) 'GRIBEX: Minimum value calculated = ', ZMIN
        ENDIF
Ce
Ce      Calculate and pack scale factor.
Ce
Ce      If user has supplied a reference value, use it.
        IF( NFREF.EQ.1) THEN
          ZREF = FREF
Ce
Ce        If integer data being packed, ensure that
Ce        reference value represents an integer.
          IF(  KSEC4(5) .EQ. 32 ) THEN
#ifdef VAX
            ITEMP = JNINT(ZREF)
#else
            ITEMP = NINT(ZREF)
#endif
            ZREF  = REAL(ITEMP)
          ENDIF
Ce
Ce        Use user-supplied value unless it exceeds the minimum value.
Ce        Otherwise use the minimum value.
Ce
          IF( ZREF.GT.ZMIN) THEN
            WRITE(GRPRSM,*)
     X  'GRIBEX: User supplied reference value ',ZREF
            WRITE(GRPRSM,*) 'GRIBEX: exceeds minimum value ',ZMIN
            WRITE(GRPRSM,*) 'GRIBEX: Minimum value used instead.'
            ZREF = ZMIN
          ENDIF
Ce
        ELSE
          ZREF = ZMIN
        ENDIF
Ce
Ce      If user has supplied a maximum value, use it.
Ce
        IF( NFMAX.EQ.1) THEN
          ZMAXV = FMAX
Ce
Ce        If integer data being packed, ensure that
Ce        maximum value represents an integer.
          IF(  KSEC4(5) .EQ. 32 ) THEN
#ifdef VAX
            ITEMP = JNINT(ZMAXV)
#else
            ITEMP = NINT(ZMAXV)
#endif
            ZMAXV  = REAL(ITEMP)
          ENDIF
Ce
Ce        Use user-supplied value unless it is less than the maximum
Ce        value. Otherwise use the maximum value.
Ce
          IF( ZMAXV.LT.ZMAX) THEN
            WRITE(GRPRSM,*)
     X  'GRIBEX: User supplied maximum value ',ZMAXV
            WRITE(GRPRSM,*) 'GRIBEX: is less than maximum value ',ZMAX
            WRITE(GRPRSM,*) 'GRIBEX: Maximum value used instead.'
            ZMAXV = ZMAX
          ENDIF
Ce
        ELSE
          ZMAXV = ZMAX
        ENDIF
Ce
        IF( LPDEBUG ) THEN
          WRITE(GRPRSM,*) 'GRIBEX: Maximum value used = ', ZMAXV
          WRITE(GRPRSM,*) 'GRIBEX: Minimum value used = ', ZREF
        ENDIF
Ce
        IF( (HOPER .EQ. 'A') .OR. (HOPER .EQ. 'B') ) THEN
          ZREF = 0.0
          ZS = 0.0
          ISCALE = 0
          ZSCALE = 1.0
        ELSE
Ce
Ce        Before using reference value, convert floating point to GRIB
Ce        representation and come back with (possibly) adjusted value.
Ce
          ISTATUS = REF2GRB(ZREF, IEXP, IMANT, IBITS)
          IF( ISTATUS.NE.0 ) THEN
            WRITE(GRPRSM,*) 'GRIBEX: REF2GRB reference value problem'
            WRITE(GRPRSM,*) 'GRIBEX: for parameter ', KSEC1(6)
            WRITE(GRPRSM,*) 'GRIBEX: level type ', KSEC1(7)
            WRITE(GRPRSM,*) 'GRIBEX: level ', KSEC1(8), KSEC1(9)
          ENDIF

Ce
Ce        If KSEC4(2) is less than (IBITS-1), then IS is taken as the
Ce        largest integer that enables the following formula :
Ce
Ce        0 <= (ZMAXV-ZREF) / (2**IS) < 2**KSEC4(2)-0.5
Ce
Ce        (NINT of the ratio above is a positive integer that fits in
Ce         KSEC4(2) bits) ; this is equivalent to
Ce
Ce        2**(IS-1) <= (ZMAXV-ZREF) / (2**KSEC4(2)-0.5) < 2**IS
Ce        or :
Ce        2**IS     <= (ZMAXV-ZREF) / (2**(KSEC4(2)+1)-1) < 2**(IS+1)
Ce
Ce        Otherwise, the 0.5 is replaced by 1. to avoid any problem
Ce        with the numerical representation of a signed integer .
Ce
Cjdc      IF( LALLPOS) THEN
Cjdc        ZAUXIL = 1.
Cjdc        ZS = (ZMAXV-ZREF) / (2.**REAL(KSEC4(2))-1.)
Cjdc      ELSEIF( KSEC4(2).EQ.(IBITS-1) ) THEN
Cjdc        ZAUXIL = 1.
Cjdc        ZS = (ZMAXV-ZREF) / REAL(2**KSEC4(2)-1)
Cjdc      ELSE
Cjdc        ZAUXIL = 2.
Cjdc        ZS = (ZMAXV-ZREF) / REAL(2**(KSEC4(2)+1)-1)
Cjdc      ENDIF
Ce
#ifdef CRAY
Cjdc      IF( ZS.NE.0.0) ZS = ALOG(ZS) / ALOG(2.) + ZAUXIL
#else
Cjdc      IF( ZS.NE.0.0) ZS = LOG(ZS) / LOG(2.) + ZAUXIL
#endif
Cjdc      ISCALE = MIN (INT(ZS),INT(ZS+SIGN(1.,ZS)))
Cjdc
C
C         Adjust number of bits per value if full integer length to
C         avoid hitting most significant bit (sign bit).
C
          NBPV  = KSEC4(2)
          IF( NBPV.EQ.IBITS ) NBPV = NBPV - 1

C
C        Adjust the minumum value to be the scale factor in case the
C        scale factor coded cannot represent the minimun value (loss of precision)
C        This should be done before computing the range of the scale factor, as the
C        unpacking will be done
C        Using the reference value coded


          IF(ZMIN.NE.ZREF) THEN
            IF( LPDEBUG ) THEN
                 WRITE(GRPRSM,*)
     X  'GRIBEX: Minimum value rectified from ', ZMIN, ' to ', ZREF,
     X  'to match reference value precision'
            ENDIF
          ENDIF

C
C         Calculate the binary scaling factor to spread the range of
C         values over the number of bits per value.
C         Limit scaling to 2**-126 to 2**127 (using IEEE 32-bit floats
C         as a guideline).
C
          ZS = 1.0
          RANGE = (ZMAX-ZREF)

          IF( ABS(ZMAX-ZMIN).LT.JPEPSLN ) THEN
            ISCALE = 0
          ELSE IF( ABS(RANGE-1.0).LT.JPEPSLN ) THEN
            ISCALE = 1 - NBPV
          ELSE IF( RANGE.GT.1.0 ) THEN
            DO JLOOP = 1, 127
              ZS = 2.0 * ZS
              IF( ZS.GT.(RANGE+JPEPSLN) ) THEN
                ISCALE = JLOOP - NBPV
                GOTO 727
              ENDIF
            ENDDO
            WRITE(GRPRSM,*)
C
     X       'GRIBEX: Problem calculating binary scale value for encode'
            KRET = 707
            GOTO 900
C
  727       CONTINUE
          ELSE
            DO JLOOP = 1, 126
              ZS = ZS / 2.0
              IF( ZS.LT.(RANGE-JPEPSLN) ) THEN
                ISCALE = 1 - JLOOP - NBPV
                GOTO 728
              ENDIF
            ENDDO
C
            WRITE(GRPRSM,*)
     X       'GRIBEX: Problem calculating binary scale value for encode'
            KRET = 707
            GOTO 900
  728       CONTINUE
          ENDIF
C
#if (defined VAX) || (defined rs6000) || defined (__alpha) || defined hpR64
Cjdc      ISCALE = MIN (INT(ZS),INT(ZS+DSIGN(1.0D0,ZS)))
#else
Cjdc      ISCALE = MIN (INT(ZS),INT(ZS+SIGN(1.,ZS)))
#endif
Ce
Ce        Limit scale factor to head off problems of 'rogue' fields
Ce        which are flat except for one pimple value
Ce        (Is this still needed?)
Ce
          IF( ISCALE.LT.-99 ) THEN
            WRITE(GRPRSM,*)
     X  ' GRIBEX: scaling factor changed from ', ISCALE
            WRITE(GRPRSM,*)
     X  ' GRIBEX:                          to ', -99
            ISCALE = -99
          ENDIF
          IF( ISCALE.GT.99 ) THEN
            WRITE(GRPRSM,*)
     X  ' GRIBEX: scaling factor changed from ', ISCALE
            WRITE(GRPRSM,*)
     X  ' GRIBEX:                          to ',  99
            ISCALE =  99
          ENDIF
Ce
          ZSCALE = 2.**ISCALE
          IF( LPDEBUG ) THEN
            WRITE(GRPRSM,*) 'GRIBEX: Encoding ZMAX = ', ZMAX
            WRITE(GRPRSM,*) 'GRIBEX: Encoding ZREF = ', ZREF
            WRITE(GRPRSM,*) 'GRIBEX: Encoding RANGE = ', RANGE
            WRITE(GRPRSM,*)
     X  'GRIBEX: Encoding scaling value = ', ZSCALE
            WRITE(GRPRSM,*)
     X  'GRIBEX: Encoding scaling value = 2 to power ',
     X        ISCALE
          ENDIF
Ce
Ce        Set sign bit.
Ce
          ITEMP = ISCALE
          CALL CSGNBT( ISCALE, ITEMP, 16, KRET)
        ENDIF
Ce
Ce      Scale factor has all bits set to 1 for missing fields.
Ce      (ECMWF convention only).
Ce
        IF( IMISS.EQ.1) ISCALE = JP16SET
Ce
      ENDIF
C
C     Insert / extract field.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,ISCALE,1,IBITS, 16,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 707
        WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting scale factor.'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
Cd
Cd    If decoding, set scale factor.
Cd
      IF( LDECODE ) THEN
        ISKALE = ISCALE
        CALL DSGNBT( ISCALE, ISKALE, 16, KRET)
Cd
Cd      Limit scale factor to head off problems of 'rogue' fields
Cd      which are flat except for one pimple value
        IF( ISCALE .LT. -99 ) THEN
          WRITE(GRPRSM,*)
     X  ' GRIBEX: scaling factor changed from ', ISCALE
          WRITE(GRPRSM,*)
     X  ' GRIBEX:                          to ', -99
          ISCALE = -99
        ENDIF
        IF( ISCALE .GT.  99 ) THEN
          WRITE(GRPRSM,*)
     X  ' GRIBEX: scaling factor changed from ', ISCALE
          WRITE(GRPRSM,*)
     X  ' GRIBEX:                          to ',  99
          ISCALE =  99
        ENDIF
Cd
        ZSCALE = 2.**ISCALE
C
        IF( LPDEBUG ) THEN
          WRITE(GRPRSM,*)
     X  'GRIBEX: Decoding scaling value = ', ZSCALE
          WRITE(GRPRSM,*)
     X  'GRIBEX: Decoding scaling value = 2 to power ',
     X      ISCALE
        ENDIF
      ENDIF
C
C*    Octets 7 - 10 : Reference value.
C     One 8 bit and one 24 bit field.
C
      IF( LENCODE ) THEN
Ce
        IF( IMISS.EQ.1) THEN
Ce
Ce        For missing, these data fields are set to all 1 bits.
          IEXP   = JP8SET
          IMANT  = JP24SET
        ENDIF
Ce
      ENDIF
C
C     Insert / extract fields.
C
      CALL INXBIT(KGRIB,KLENG,INSPT,IEXP,1,IBITS, 8,YFUNC,KRETA)
      CALL INXBIT(KGRIB,KLENG,INSPT,IMANT,1,IBITS, 24,YFUNC,KRETB)
      KRET = KRETA + KRETB
      IF( KRET.NE.0) THEN
        KRET = 708
        WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting reference value'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
C     Conversion from GRIB format, if decoding.
C
      IF( LDECODE ) THEN
Cd
Cd      Set IMISS to 1 if entire field is missing, i.e. scale
Cd      factor, exponent and mantissa with all bits set to 1.
Cd
        IMISS = 0
        IF( ISKALE.EQ.JP16SET.AND.IEXP.EQ.JP8SET.AND.IMANT.EQ.JP24SET)
     X    IMISS = 1
Cd
Cd      Convert GRIB representation to floating point.
Cd
        IF( IMISS.EQ.0) THEN
Cd
Cd        Field is present.
          CALL DECFP2 (ZREF,IEXP,IMANT)
        ELSE
Cd
Cd        Field is missing. Print warning message and
Cd        field identification sections of Grib code,
Cd        forcing field data values to 0.
Cd
          WRITE(GRPRSM,*)
     X  'GRIBEX: Following field is missing ***********'
          CALL GRPRS1 (KSEC0,KSEC1)
          ZREF   = 0
          ZSCALE = 0
        ENDIF
        IF( LPDEBUG ) WRITE(GRPRSM,*)
     X    'GRIBEX: Decoded reference value = ',ZREF
      ENDIF
Cd
C*    Octet 11 : Number of bits containing each packed value.
C     One 8 bit field.
C
C     Insert / extract field.
C
      IF( .NOT.LGRDPT.OR.KSEC4(4).EQ.0) THEN
        CALL INXBIT(KGRIB,KLENG,INSPT,KSEC4(2),1,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 709
          WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting number of'
          WRITE(GRPRSM,*) 'GRIBEX: bits per data value'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
      ELSE
C
C        In second-order packing case, this number may differ from
C        the value specified for normalization process (KSEC4(2)).
C        Field is inserted later, within function C2ORDR or related
C        C2* sub-functions.
C
        INSPT=INSPT+8
C
      ENDIF
C
      IF( LDECODE ) THEN
Cd
Cd      Check if number of bits per packed value = number of bits per
Cd      computer word.
Cd
        LALLPOS = KSEC4(2) .EQ. IBITS
Cd
      ENDIF
C
C*    Octets 12 et sequentia can contain further header
C     information, depending on data representation type.
C
C
C*    For grid point data, simple packing, single value at each
C     grid point there is no further header information and
C     packed data begins in octet 12.
C
C*    For grid point data, simple packing, with a matrix of
C     values at each grid point further information is added.
C
      IF( LGRDPT .AND.KSEC4(6).EQ.16.AND.KSEC4(4).EQ.0) THEN
C
C       Octets 12 - 13. N - octet number at which packed
C       data begins.
C       One 16 bit field.
C
C            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C            !                                                !
C            ! This is the WMO definition, but it is entirely !
C            ! inadequate when secondary bit maps are present !
C            ! eg 3x3 global grid with a matrix of values     !
C            ! 12x26 at each point. This gives a bit map with !
C            ! a length of 285480 octets which cannot be given!
C            ! in 16 bits.                                    !
C            !                                                !
C            ! ECMWF uses the following definition  for its   !
C            ! wave model data.                               !
C            ! N - Number of secondary bit maps               !
C            !     (ie the number of points which are 'not    !
C            !      missing').                                !
C            !     This definition will accommodate a 1x1     !
C            !     degree global grid.                        !
C            !                                                !
C            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
        IF( LENCODE ) THEN
Ce
Ce        Octet number = 25 + NC1 + NC2
Ce
          IBYTEX = 25 + KSEC4(53) + KSEC4(55)
Ce
Ce        Add in length of bit-maps, if present.
Ce        Length in bits is the number of values
Ce        remaining to be packed rounded
Ce        up to a number of octets.
Ce
          IF( KSEC4(9).EQ.32) IBYTEX = IBYTEX + (ILENF+7) / 8
Ce
Ce        ECMWF wave model usage.
Ce        ECMWF is centre number 98, and local code table
Ce        2 used for wave models is 140.
Ce
          IF( LECLOC .AND.KSEC1(1).EQ.140) THEN
            IBYTEX = ILENF / (KSEC4(50) * KSEC4(51))
          ENDIF
        ENDIF
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,IBYTEX,1,IBITS, 16,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 720
          WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting octet number'
          WRITE(GRPRSM,*) 'GRIBEX: at which packed data begins'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C       Special fix to handle overflow of this 16-bit field when
C       a large wave field is being decoded
C
        IF( LLARGE .AND. (IBYTEX.LT.2000) ) IBYTEX = IBYTEX + 65536
C
C       Octet 14. Extended flags.
C       One 8 bit field.
C
        IF( LENCODE ) IFLAGX = KSEC4(8) + KSEC4(9) + KSEC4(10)
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,IFLAGX,1,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 721
          WRITE(GRPRSM,*)
     X      'GRIBEX: Error inserting/extracting extended flag field'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
        IF( LDECODE ) THEN
Cd
Cd        In Edition 1 only 3 bits are used.
Cd
Cd        -0------ for single datum at each grid point.
Cd        -1------ for matrix of values at each point.
Cd        --0----- for no secondary bit-maps.
Cd        --1----- for secondary bit-maps present.
Cd        ---0---- for second order values constant width.
Cd        ---1---- for second order values different widths.
Cd
          IF( IFLAGX.GE.64) THEN
            KSEC4(8) = 64
            IFLAGX   = IFLAGX - 64
          ENDIF
Cd
          IF( IFLAGX.GE.32) THEN
            KSEC4(9) = 32
            IFLAGX   = IFLAGX - 32
          ENDIF
Cd
          IF( IFLAGX.GE.16) THEN
            KSEC4(10) = 16
            IFLAGX   = IFLAGX - 16
          ENDIF
        ENDIF
C
C       Octets 15 - 16. NR - first dimension (rows) of
C       each matrix.
C       Octets 17 - 18. NC - second dimension (columns) of
C       each matrix.
C       Two 16 bit fields.
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,KSEC4(50),2,IBITS, 16,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 722
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting/extracting first or'
          WRITE(GRPRSM,*) 'GRIBEX: second dimension of matrix'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C       Octet 19. First dimension coordinate values
C       definition.
C       Octet 20. NC1 - Number of coefficients or values
C       used to specify first dimension coordinate function.
C       Octet 21. Second dimension coordinate values
C       definition.
C       Octet 22. NC2 - Number of coefficients or values
C       used to specify second dimension coordinate function.
C       Octet 23. First dimension physical significance.
C       Octet 24. Second dimension physical significance.
C       Six 8 bit fields.
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,KSEC4(52),6,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 723
          WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting six fields'
          WRITE(GRPRSM,*)
     X  'GRIBEX: from 1st dimension coordinate value on'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
C       Octets 25 - (24+NC1).
C       Coefficients to define first dimension coordinate
C       values in functional form, or the explicit
C       coordinate values.
C       Octets (25+NC1) - (24+NC1+NC2).
C       Coefficients to define second dimension coordinate
C       values in functional form, or the explicit
C       coordinate values.
C       (NC1+NC2) 8 bit fields.
C
C            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C            !                                                !
C            ! This is the WMO definition, but it is very     !
C            ! limited and can only accommodate small integer !
C            ! fields values.                                 !
C            !                                                !
C            ! ECMWF needs to use floating point numbers and  !
C            ! for the wave models the definition is NC1+NC2  !
C            ! (8 bit and 24 bit) fields.
C            !                                                !
C            !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
        ITEMP = KSEC4(53) + KSEC4(55)
C
C       ECMWF wave model usage.
C       ECMWF is centre number 98, and local code table
C       2 used for wave models is 140.
C
        IF( LECLOC .AND.KSEC1(1).EQ.140) THEN
          ITRND = 1
          DO 730 JLOOP=1,ITEMP
C
C           One 8 bit and one 24 bit field.
C
            IF( LENCODE ) THEN
Ce
Ce            Convert floating point to GRIB representation.
              ISINT = KSEC4(JLOOP+59)
              ZREAL = ZREAL4
              CALL CONFP3 (ZREAL,IEXP,IMANT, IBITS,ITRND)
            ENDIF
C
C           Insert / extract fields.
C
            CALL INXBIT(KGRIB,KLENG,INSPT,IEXP,1,IBITS, 8,YFUNC,KRETA)
            CALL INXBIT(KGRIB,KLENG,INSPT,IMANT,1,IBITS,24,YFUNC,KRETB)
            KRET = KRETA + KRETB
            IF( KRET.NE.0) THEN
              KRET = 724
              WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting first or'
              WRITE(GRPRSM,*)
     X  'GRIBEX: second dimension coefficients'
              WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
              GO TO 900
            ENDIF
C
            IF( LDECODE ) THEN
Cd
Cd            Convert GRIB representation to floating point.
              CALL DECFP2 (ZREAL,IEXP,IMANT)
              ZREAL4 = ZREAL
              KSEC4(JLOOP+59) = ISINT
            ENDIF
C
  730     CONTINUE
        ELSE
C
C         Insert / extract fields.
C
          CALL INXBIT(KGRIB,KLENG,INSPT,KSEC4(57),ITEMP,
     X                 IBITS,8,YFUNC,KRET)
          IF( KRET.NE.0) THEN
            KRET = 724
            WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting first or'
            WRITE(GRPRSM,*) 'GRIBEX: second dimension coefficients'
            WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
            GO TO 900
          ENDIF
        ENDIF
C
C       Matrix bit-maps may follow.
C
        IF( KSEC4(9).EQ.32) THEN
          ITEMP = KSEC4(50) * KSEC4(51)
          IF( LENCODE ) THEN
            CALL INSMP2 (KGRIB,KLENG,INSPT,PSEC4,
     X                   ILENF,IBITS,ZMSVAL,YFUNC,
     X                   ITEMP,NDBG,KRET)
            IF( KRET.NE.0) THEN
              KRET = 725
              WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting secondary bit-map'
              WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
              GO TO 900
            ENDIF
            ILEN = ILENF
          ELSE
Cd
Cd          Retain pointer to bit-map location.
            IBMAP2 = INSPT
Cd
Cd          Set pointer to start of packed data.
Cd
            IF( LECLOC .AND.KSEC1(1).EQ.140) THEN
              ITEMP = KSEC4(50)*KSEC4(51)*IBYTEX
              ITEMP = (ITEMP+7) / 8
              INSPT = INSPT + ITEMP * 8
            ELSE
              INSPT = INSPT + (IBYTEX-25-KSEC4(53) -KSEC4(55)) * 8
            ENDIF
C
          ENDIF
        ENDIF
C
      ENDIF
C
C*    For spherical harmonic data (simple packing), real (0,0)
C     coefficient is in floating point representation in
C     octets 12-15.
C     One 8 bit and one 24 bit field.
C
      IF( LSPHERC ) THEN
C
C       Convert floating point to GRIB representation.
        ITRND = 1
        IF( LENCODE ) CALL CONFP3 (PSEC4(1),IEXP,IMANT,IBITS,ITRND)
C
C       Insert / extract fields.
C
        CALL INXBIT(KGRIB,KLENG,INSPT,IEXP,1,IBITS, 8,YFUNC,KRETA)
        CALL INXBIT(KGRIB,KLENG,INSPT,IMANT,1,IBITS, 24,YFUNC,KRETB)
        KRET = KRETA + KRETB
        IF( KRET.NE.0) THEN
          KRET = 711
          WRITE(GRPRSM,*)
     X      'GRIBEX: Error inserting/extracting real coefficient'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
        IF( LDECODE .AND. (.NOT. L_IORJ) ) THEN
Cd
Cd        Convert GRIB representation to floating point.
Cd
          IF( IMISS.EQ.1) THEN
            PSEC4(1) = 0.0
          ELSE
            CALL DECFP2 (PSEC4(1),IEXP,IMANT)
          ENDIF
        ENDIF
      ENDIF
C
C*    Octet N onwards - Packed data.
C
C     If decoding , calculate number of data values, unless
C     number has been given by user for 'X' function.
C
      IF( LDECODE ) THEN
        IF( HOPER.EQ.'X') THEN
          ILEN = KSEC4(34)
Cdx
Cdx     Otherwise, use the byte counts
        ELSEIF( KSEC4(2).NE.0) THEN
          ILEN = (IPLEN+(ILEN4*8)-INSPT-INIL)/KSEC4(2)
Cd
Cd      If explicitly constant field, use section 2 information
Cd
        ELSEIF( .NOT.LSECT2 ) THEN
          KRET = 726
          WRITE(GRPRSM,*)
     X  'GRIBEX: Constant (0-bit) field without section 2'
          WRITE(GRPRSM,*) 'GRIBEX: not supported'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ELSEIF( LSPHERC ) THEN
          KRET = 727
          WRITE(GRPRSM,*) 'GRIBEX: Constant (0-bit) spectral field'
          WRITE(GRPRSM,*) 'GRIBEX: not supported'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ELSEIF( HOPER.EQ.'G'.OR.HOPER.EQ.'B') THEN
          KRET = 728
          WRITE(GRPRSM,*)
     X      'GRIBEX: Constant (0-bit) field: function ',HOPER(:1)
          WRITE(GRPRSM,*) 'not supported'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ELSEIF( LQUASI ) THEN
Cd
          IF( MOD(KSEC2(11),64).LT.32) THEN
            INROWS=KSEC2(3)
          ELSE
            INROWS=KSEC2(2)
          ENDIF
Cd
          ILEN=0
Cd
          DO 740 JLOOP = 1 , INROWS
            ILEN=ILEN+KSEC2(22+JLOOP)
  740     CONTINUE
Cd
        ELSE
          ILEN=KSEC2(2)*KSEC2(3)
        ENDIF
Cd
Cd      Total number of values = packed + unpacked.
Cd
        IF( HOPER .EQ. 'B' ) THEN
          KSEC4(1) = (ILEN+NBYTE-1)/NBYTE
          KSEC4(21) = ILEN
Cd
        ELSE
          KSEC4(1) = ILEN + IFPT
        ENDIF
Cd
Cd      Check length of output array.
Cd
        IF( (KSEC4(1).GT.KLENP) .AND.
     X       (HOPER.NE.'G')      .AND.
     X       (HOPER.NE.'J')    ) THEN
          KRET = 710
          WRITE(GRPRSM,*)
     X  'GRIBEX: Output array too small. Length = ',KLENP
          WRITE(GRPRSM,*) 'GRIBEX: Number of values = ', KSEC4(1)
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Cd
        IF( KSEC4(2).EQ.0) THEN
C
          IF( LPDEBUG ) WRITE(GRPRSM,*)
     X      'GRIBEX: Explicitly constant field (0-bit).'
C
C          Explicitly constant field, filled directly.
C
          IF( HOPER.NE.'J') THEN
C
            IF( KSEC1(23).EQ.0) THEN
              ZVAL = ZREF
            ELSE
              ZVAL = ZREF / 10.**KSEC1(23)
            ENDIF
C
            DO 750 JLOOP = 1 , ILEN
              PSEC4(IFPT+JLOOP) = ZVAL
  750       CONTINUE
C
          ENDIF
C
Cx        Option 'X', only a few points extracted.
Cx
          IF( HOPER.EQ.'X') THEN
Cx
Cx          Convert to integer if original data was integer.
Cx
            IF( KSEC4(5).EQ.32 ) CALL RORINT(PSEC4,PSEC4,KSEC4(1),'I')
            GO TO 900
Cx
          ENDIF
C
          GOTO 800
C
        ENDIF
C
      ENDIF
C
C
C*    Scale and store, or extract and scale data values.
C
C     Only a few points to be unpacked.
C
      IF( HOPER.EQ.'X') THEN
Cx
Cx      Check that a section 2 is present.
Cx
        IF( .NOT.LSECT2 ) THEN
          KRET = 798
          WRITE(GRPRSM,*)
     X  'GRIBEX: Function is X but no section 2 included'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Cx
Cx      Check that no bit-map is included.
Cx
        IF( LSECT3 ) THEN
          KRET = 717
          WRITE(GRPRSM,*)
     X  'GRIBEX: Function is X and a bit-map is included'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Cx
Cx      Check that field is Gaussian or latitude/longitude grid.
Cx
        IF( KSEC2(1).NE.0.AND.KSEC2(1).NE.4.AND.
     X      KSEC2(1).NE.10.AND.KSEC2(1).NE.14.AND.
     X      KSEC2(1).NE.20.AND.KSEC2(1).NE.24.AND.
     X      KSEC2(1).NE.30.AND.KSEC2(1).NE.34) THEN
          KRET = 716
          WRITE(GRPRSM,*) 'GRIBEX: Function is X and field is not'
          WRITE(GRPRSM,*)
     X  'GRIBEX: Gaussian or Latitude/longitude field'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Cx
Cx      Check that scanning mode is West to East and North to South.
Cx
        IF( KSEC2(11).NE.0) THEN
          KRET = 715
          WRITE(GRPRSM,*)
     X  'GRIBEX: Function is X and scanning mode is not'
          WRITE(GRPRSM,*) 'GRIBEX: North to South and West to East'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Cx
Cx      Check that number of points required does not exceed
Cx      maximum or minimum allowed.
Cx
        IF( KSEC4(34).GT.4.OR.KSEC4(34).LT.1) THEN
          KRET = 714
          WRITE(GRPRSM,*)
     X      'GRIBEX: Invalid no. of values for function X = ',KSEC4(34)
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Cx
        ITEMP = 1
Cx
Cx      Take into account decimal scaling factor, if any.
Cx
        IF( KSEC1(23).NE.0) THEN
          ZREF = ZREF / 10.**KSEC1(23)
          ZSCALE = ZSCALE / 10.**KSEC1(23)
        ENDIF
Cx
Cx      Skip down latitude rows.
        DO 770 JLOOPO = 1,KSEC4(34)
Cx
Cx        Regular grid.
          IF( .NOT. LQUASI) THEN
            ISKIP = (KSEC4(34+ITEMP)-1) * KSEC2(2)
Cx
Cx        Quasi-regular grid.
          ELSE
            ISKIP = 0
            DO 760 JLOOP = 1,KSEC4(34+ITEMP) - 1
              ISKIP = ISKIP + KSEC2(22+JLOOP)
  760       CONTINUE
          ENDIF
Cx
Cx        Skip any points not required on this latitude row.
          ISKIP = ISKIP + KSEC4(34+ITEMP+1) - 1
Cx
Cx        Calculate number of bits in these values and add
Cx        to current value of bit-pointer.
          ISKIP = ISKIP * KSEC4(2) + INSPT
Cx
Cx        Extract value from 1 point.
          CALL INXBIT(KGRIB,KLENG,ISKIP,IJDC,1,
     X                IBITS,KSEC4(2),YFUNC,KRET)
          IF( KRET.NE.0) THEN
            KRET = 712
            WRITE(GRPRSM,*) 'GRIBEX: Error extracting data values'
            WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
            GO TO 900
          ENDIF
          IJDCXX = IJDC
Cx
          IF( KSEC4(5).EQ.32 ) THEN
Cx
Cx          Integer values
Cx
#if (defined REAL_BIGGER_THAN_INTEGER)
            PSEC4(JLOOPO) = ZREF + DBLE(IJDCXX)*ZSCALE
#else
            PSEC4(JLOOPO) = ZREF + IJDCXX*ZSCALE
#endif
            IJDCXX = PSEC4(JLOOPO)
            PSEC4(JLOOPO) = RJDCXX
Cx
          ELSE
Cx
Cx          Real values
Cx
#if (defined REAL_BIGGER_THAN_INTEGER)
            PSEC4(JLOOPO) = ZREF + DBLE(IJDCXX)*ZSCALE
#else
            PSEC4(JLOOPO) = ZREF + IJDCXX*ZSCALE
#endif
          ENDIF
          ITEMP = ITEMP + 2
  770   CONTINUE
Cx
        GOTO 900
      ENDIF
C
C*******************************************************************
C     All data to be unpacked or packed.
C*******************************************************************
C
C     Scale the fields using the reference value and scaling factor
C
      IF( LENCODE .AND. (HOPER .NE. 'A')  )
     X  CALL INSCAL(PSEC4(IFPT+1),PSEC4(IFPT+1),ILEN,ZREF,ZSCALE,NBPV)
C
C     Insert / extract fields, unless special 'G' operation or
C     if 'J' option in effect.
C
      IF( LENCODE.AND.LGRDPT.AND.(KSEC4(4).NE.0.OR.HOPER.EQ.'K')) THEN
Ce
Ce*******************************************
Ce
Ce      Complex packing ("second-order packing") for grid-point.
Ce
        INSPT=INSPT-11*8
        KRET = C2ORDR( PSEC4, KLENP, KSEC1, KSEC2, KSEC3, KSEC4,
     X                 KGRIB, KLENG, INSPT, IBITS, HOPER, IBMAP,
     X                 IVALS, ZREF,  ZMAX,  ILEN,  NDBG )
Ce
Ce      Give up if error reported.
        IF( KRET .NE. 0 ) GOTO 900
Ce
C
      ELSEIF( (HOPER.NE.'G') .AND. (.NOT.L_IORJ) ) THEN
Cd
#ifdef REAL_BIGGER_THAN_INTEGER
        IF( LDECODE .AND. (HOPER.NE.'B') ) THEN
Cd
Cd        Split the values into groups of length 'JPNSEC4' (maximum)
Cd
          ILOOPS = 1+(ILEN-1)/JPNSEC4
          IINDEX = IFPT
Cd
          DO 785 JLOOPO = 1, ILOOPS
            ILNGTH = MIN(JPNSEC4,ILEN-(JLOOPO-1)*JPNSEC4)
            CALL INXBIT(KGRIB,KLENG,INSPT,NSEC4,ILNGTH,
     X                   IBITS,KSEC4(2),YFUNC,KRET)
            IF( KRET.NE.0) THEN
              KRET = 712
              WRITE(GRPRSM,*) 'GRIBEX: Error extracting data values'
              WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
              GO TO 900
            ENDIF
            DO 780 JLOOP = 1, ILNGTH
              PSEC4(IINDEX + JLOOP) = ZREF + NSEC4(JLOOP)*ZSCALE
  780       CONTINUE
            IINDEX = IINDEX + ILNGTH
  785     CONTINUE
Cd
Cd        Change units of data values, if required.
Cd
          IF( KSEC1(23).NE.0) THEN
            DO 790 JLOOP = 1 , KSEC4(1)
              PSEC4(JLOOP) = PSEC4(JLOOP)/10.0**KSEC1(23)
  790       CONTINUE
          ENDIF
          GOTO 799
        ENDIF
Cb
Cb      Handle data for option 'B'
Cb
        IF( HOPER.EQ.'B' ) THEN
Cb
          ILOOPS = 1+(ILEN/NBYTE-1)/JPNSEC4
          IINDEX = IFPT
Cb
          DO 794 JLOOPO = 1, ILOOPS
            ILNGTH = MIN(JPNSEC4,ILEN-(JLOOPO-1)*JPNSEC4)
            CALL INXBIT(KGRIB,KLENG,INSPT,NSEC4,ILNGTH,
     X                  IBITS,KSEC4(2),YFUNC,KRET)
            IF( KRET.NE.0) THEN
              KRET = 712
              WRITE(GRPRSM,*) 'GRIBEX: Error extracting data values'
              WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
              GO TO 900
            ENDIF
Cb
            DO 792 JLOOP = 1, ILNGTH
              PSEC4(IINDEX + JLOOP) = XSEC4(JLOOP)
  792       CONTINUE
Cb
            IINDEX = IINDEX + ILNGTH
  794     CONTINUE
Cb
          GOTO 799
Cb
        ENDIF
#endif
C
        IF( (HOPER.NE.'A') .AND. (HOPER.NE.'B') ) THEN
          CALL INXBIT( KGRIB,KLENG,INSPT,PSEC4(IFPT+1),ILEN,
     X                 IBITS,KSEC4(2),YFUNC,KRET)
        ELSE
          CALL INXBIT( KGRIB,KLENG,INSPT,PSEC4(IFPT+1),ILEN/NBYTE,
     X                 IBITS,IBITS,YFUNC,KRET)
        ENDIF
        IF( KRET.NE.0) THEN
          KRET = 712
          WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting/extracting data values'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
C
       ELSE
C
Cgj     Option 'G' or 'J' in effect.
Cgj
        KSEC4(34) = INSPT
C
        IF( .NOT. L_IORJ ) THEN
Cg
Cg        Option 'G' ...
Cg
Cg        Return reference value, binary scale factor and
Cg        bit pointer to start of packed data.
C
          PSEC4(1) = ZREF
          PSEC4(2) = ZSCALE
        ENDIF
Cj
        IF( HOPER.EQ.'J' ) GOTO 815
Cj
        GO TO 900
Cd
      ENDIF
C
  796 CONTINUE
C
      IF( LDECODE .AND. (HOPER .NE. 'B') ) THEN
Cd
       CALL EXSCAL(PSEC4(IFPT+1),PSEC4(IFPT+1),ILEN,ZREF,ZSCALE,LALLPOS)
Cd
Cd      Change units of data values, if required.
Cd
        IF( KSEC1(23).NE.0) THEN
          DO 798 JLOOP = 1 , KSEC4(1)
            PSEC4(JLOOP) = PSEC4(JLOOP)/10.0**KSEC1(23)
  798     CONTINUE
        ENDIF
Cd
      ENDIF
C
 799  CONTINUE
C
C*    Enter length of binary data section, ensuring that the
C     length is an even number of octets, padding with binary
C     zeroes as required.
C     One 24 bit field.
C
      IF( YFUNC.NE.'C') GO TO 800
Ce
Ce    Length of section 4, in bits.
Ce
      ILEN4 = INSPT - IPLEN
      IL    = ILEN4 / 16
      IL    = ILEN4 - ( IL * 16 )
      INIL  = 0
      IF( IL.NE.0) THEN
        INIL = 16 - IL
Ce
Ce      Insert padding zeroes at end of section 4
Ce
        ITEMP = 0
        CALL INXBIT(KGRIB,KLENG,INSPT,ITEMP,1,IBITS,INIL,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 734
          WRITE(GRPRSM,*)
     X      'GRIBEX: Error inserting padding zeroes at end of section 4'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ',KRET
          GO TO 900
        ENDIF
        ILEN4 = ILEN4 + INIL
      ENDIF
Ce
      ILEN4 = ILEN4 / 8
Ce
Ce    Because of the restriction on product lengths to 3 octets,
Ce    if a very large product, the section 4 length field holds
Ce    the number of bytes in the product after section 4 upto
Ce    the end of the padding bytes.
Ce    This is only feasible because the (default) rounding for
Ce    GRIB products is 120 bytes.
Ce    In this case, delay encoding the value until section 5 has
Ce    been encoded.
Ce
      IF( ILEN4.GE.JP23SET ) THEN
        LLARGE = .TRUE.
        IPLEN = IPLEN + 24
      ELSE
Ce
Ce      Insert field.
        CALL INXBIT(KGRIB,KLENG,IPLEN,ILEN4,1,IBITS, 24,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 701
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting section 4 length'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
                 GO TO 900
        ENDIF
      ENDIF
Ce
Ce    Enter flag / unused bits field.
Ce    One 8 bit field.
Ce    Two 4 bit fields.
Ce
      IFLAG = KSEC4(3) + KSEC4(4) + KSEC4(5) + KSEC4(6)
      IFLAG = IFLAG + INIL
Ce
Ce    Print number of unused bits, if required.
      IF( LPDEBUG ) WRITE(GRPRSM,*)
     X  'GRIBEX: Number of unused bits is ',INIL,'.'
Ce
Ce    Insert field.
      CALL INXBIT(KGRIB,KLENG,IPLEN,IFLAG,1,IBITS, 8,YFUNC,KRET)
      IF( KRET.NE.0) THEN
        KRET = 713
        WRITE(GRPRSM,*)
     X    'GRIBEX: Error inserting/extracting flag and unused bit field'
        WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
        GO TO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C
C
C
C
C
C
C
C
C
C
C*    Section 8 . Code/decode End Section (Section 5) of GRIB code.
C     -----------------------------------------------------------------|
C
  800 CONTINUE
C
      IF( LPDEBUG ) WRITE(GRPRSM,*)
     X  'GRIBEX: Section 8. Handle End Section (Section 5)'
C
C*    Ascii 7 7 7 7 at end of coded data.
C     Four 8 bit fields.
C
      IF( LENCODE ) THEN
Ce
Ce      Insert field.
        IP7777 = INSPT
        CALL INXBIT(KGRIB,KLENG,INSPT,I7777(1),4,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 801
          WRITE(GRPRSM,*) 'GRIBEX: Error inserting 7777 group'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ',KRET
          GO TO 900
        ENDIF
Ce
Ce      Length of GRIB message.
        KSEC0(1) = INSPT / 8
        ITEMP = KSEC0(1)
Ce
Ce    Because of the restriction on product lengths to 3 octets, if
Ce    more than 24 bits is needed, rescale by a factor of -120 to give
Ce    a (smaller) negative count.  It is only possible because
Ce    the (default) rounding for GRIB products is 120 bytes.
Ce
        IF( ITEMP.GE.JP23SET ) THEN
          LLARGE = .TRUE.
        ELSE
Ce
Ce        When encoding, use most-significant bit as a sign bit.
Ce
          CALL CSGNBT( KSEC0(1), ITEMP, 24, KRET)
Ce
Ce        Insert field.
          ITEMP = 32
          CALL INXBIT(KGRIB,KLENG,ITEMP,KSEC0(1),1,IBITS, 24,YFUNC,KRET)
          IF( KRET.NE.0) THEN
            KRET = 802
            WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting length of GRIB message'
            WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
            GO TO 900
          ENDIF
        ENDIF
Ce
Cjdc  ELSE
      ENDIF
Cj
Cj      'J' option jumps here for possible bitmap adjustment
Cj      to number of values in field section 4.
Cj
  815 CONTINUE
C
      IF( LDECODE ) THEN
Cj
        IF( L_IORJ ) GOTO 816
Cj
Cd      Skip padding.
        INSPT = INSPT + INIL
Cd
        CALL INXBIT(KGRIB,KLENG,INSPT,IPARM(1),4,IBITS, 8,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 801
          WRITE(GRPRSM,*) 'GRIBEX: Error extracting 7777 group'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ',KRET
          GO TO 900
        ENDIF
Cd      Check that length is consistent with the number of the header file
Cd      this will not happen for very large grib products with the *120 nightmare fix
Cd      in this *120 case the KSEC0(1) is updated with the value of the pointer/8
        IF( LLARGE) THEN
        IF( KSEC0(1).NE.INSPT/8) THEN
           IF( LPDEBUG ) THEN
            WRITE(GRPRSM,*) 'GRIBEX: Large product found', KSEC0(1)
           ENDIF
           KSEC0(1) = INSPT/8
           IF( LPDEBUG ) THEN
            WRITE(GRPRSM,*) 'GRIBEX: KSEC0(1) updated to = ', KSEC0(1)
           ENDIF
          ENDIF
        ENDIF
Cd
Cd      Check that 7777 group is found where expected.
        ICOUNT = 0
        DO 810 JLOOP = 1 , 4
          IF( IPARM(JLOOP).NE.55) ICOUNT = ICOUNT + 1
  810   CONTINUE
        IF( ICOUNT.NE.0) THEN
          KRET = 805
          WRITE(GRPRSM,*)
     X  'GRIBEX: End of message 7777 group not found.'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Cd
Cd      Final handling when bit-maps included.
Cd
  816   CONTINUE
Cd
        IF( LSECT3 ) THEN
Cd
Cd        Bit-map included in GRIB message.
          IF( KSEC3(1).EQ.0) THEN
Cd
            IF( KSEC4(8).EQ.0.OR.KSEC4(9).EQ.0) THEN
Cd
Cd            Single value at each point.
Cd
              ITEMP = 1
Cd
Cd          Pointer IBMAP2 is set negative if there
Cd          are no matrix bit-maps.
Cd
              IBMAP2 = -1
              ITEMP = 1
            ELSE
Cd
Cd            Matrix of values at a point.
Cd
              ITEMP = KSEC4(50) * KSEC4(51)
            ENDIF
Cd
            IF( .NOT.L_IORJ ) THEN
Cd
Cd            Check user array is big enough before using bitmap to
Cd            fill missing data values (not relevant for 'J' option)
Cd
              IF( KLENP .LT. (IVALS*ITEMP) ) THEN
                WRITE(GRPRSM,*)
     X  'GRIBEX: Output array is not big enough to'
                WRITE(GRPRSM,*)
     X  'GRIBEX: allow expansion using bitmaps'
                KRET = 729
                GOTO 900
              ENDIF
Cd
Cd            Expand using bitmaps
Cd
              CALL EXTMAP(KGRIB,KLENG,IBMAP,IBMAP2,KSEC4(1),PSEC4,IVALS,
     X                    IBITS,ISBMAP,ZMSVAL,ITEMP,NDBG,KRET,NONMISS)
              IF( KRET.NE.0) THEN
                KRET = 806
                WRITE(GRPRSM,*)
     X            'GRIBEX: Error extracting primary/secondary bit map.'
                WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
                GO TO 900
              ENDIF
              KSEC4(1) = IVALS*ITEMP
Cj
            ELSE IF( HOPER.EQ.'J' ) THEN
Cj
Cj            Cannot handle secondary bitmaps for 'J' option counting
Cj
              IF( IBMAP2.NE.-1 ) THEN
                WRITE(GRPRSM,*)
     X            'GRIBEX: Cannot handle 2ndary bitmaps for J option'
                KRET = 811
                GOTO 900
Cj
              ELSE
Cj
Cj              Count bits in primary bitmap only ('J' option)
Cj
                IS3BYTE = (IBMAP/8 - 6)
                NONMISS = ONEBITS(KGRIB,IS3BYTE)
                KSEC4(21) = NONMISS
                KSEC4(1)  = IVALS
              ENDIF
            ENDIF
Cd
          ELSE
#ifdef USE_NO_POINTERS
            WRITE(GRPRSM,*)
     X        'GRIBEX: predetermined bit-map reference not handled'
              KRET = 735
              GOTO 900
#else
Cd
Cd          Predetermined bit-map reference only.
Cd          (Secondary bitmaps not handled in this case).
Cd
            IBMAP2 = -1
Cd
Cd          Single value at each point.
Cd
            ITEMP = 1
Cd
Cd          Get the bitmask
Cd
            KRET = GBITMAP(KSEC3(1), IBTVALS, NONMISS, IBTPTR, NBYTE)
            IF( KRET.NE.0 ) THEN
              WRITE(GRPRSM,*) 'GRIBEX: Problem getting bitmap'
              GOTO 900
            ENDIF
Cd
Cd          Expand using bitmap
Cd
            IBMAP = 0
Cd
Cd          Set integer or real missing data value.
Cd
            IF( KSEC4(5) .EQ. 0 )  THEN
              ZMSVAL = PSEC3(2)
            ELSE
              ZMSVAL = REAL(KSEC3(2))
            ENDIF
            CALL EXTMAP(IBTMAP, KLENG, IBMAP, IBMAP2, KSEC4(1), PSEC4,
     X                  IBTVALS,IBITS, ISBMAP,ZMSVAL, ITEMP, NDBG, KRET,
     X                  NONMISS)
            IF( KRET.NE.0) THEN
              KRET = 806
              WRITE(GRPRSM,*)
     X          'GRIBEX: Error extracting primary or secondary bit map.'
              WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
              GO TO 900
            ENDIF
            KSEC4(1) = IBTVALS*ITEMP
Cd
#endif
          ENDIF
        ENDIF
Cd
#ifndef USE_NO_POINTERS
Cd
Cd      If required, convert quasi-regular grid to regular.
Cd
        IF( HOPER.EQ.'R'.AND.LQUASI) THEN
Cr
Cr        Quasi-regular gaussian ..
Cr
          IF( (KSEC2(1).EQ. 4).OR.
     X        (KSEC2(1).EQ.14).OR.
     X        (KSEC2(1).EQ.24).OR.
     X        (KSEC2(1).EQ.34) ) THEN
Cr
Cr          Gaussian grid must be global
Cr
            INOLAT = KSEC2(10) * 2
Cr
            IF( KSEC2(3).NE.INOLAT ) THEN
              KRET = 808
              WRITE(GRPRSM,*)
     X          'GRIBEX: Error converting quasi-regular gaussian grid'
              WRITE(GRPRSM,*) 'GRIBEX: to regular. Grid must be global.'
              WRITE(GRPRSM,*) 'GRIBEX: Return code = ',KRET
              GO TO 900
            ENDIF
Cr
            INOLNG    = INOLAT * 2
            KSEC2(9)  = NINT(360000.0/REAL(INOLNG))
            LPERIO = .TRUE.
C
C           Some ECMWF 'vegetation' parameters have to be interpolated
C           using 'nearest neighbour' processing
C
            NITABLE  = KSEC1(1)
            NICENTRE = KSEC1(2)
            NIPARAM  = KSEC1(6)
            LVEGGY = (NITABLE.EQ.128).AND.
     X               (NICENTRE.EQ.98).AND.
     X               ((NIPARAM.EQ.27).OR.
     X                (NIPARAM.EQ.28).OR.
     X                (NIPARAM.EQ.29).OR.
     X                (NIPARAM.EQ.30))
            IF( LPDEBUG ) THEN
              IF( LVEGGY ) WRITE(GRPRSM,*)
     X          'GRIBEX: Nearest neighbour processing used for R option'
            ENDIF
            CALL QU2REG3(PSEC4,KSEC2(23),INOLAT,INOLNG,1,ZMSVAL,KRET,
     X                   LSECT3,LPERIO,LVEGGY)
            IF( KRET .NE. 0 ) THEN
              KRET = 808
              WRITE(GRPRSM,*)
     X          'GRIBEX: Error converting quasi-regular gaussian grid'
              WRITE(GRPRSM,*)
     X          'GRIBEX: to regular. Return code = ',KRET
              GO TO 900
            ENDIF
Cr
Cr        Quasi-regular latitude-longitude ..
Cr
          ELSE IF( (KSEC2(1).EQ. 0).OR.
     X             (KSEC2(1).EQ.10).OR.
     X             (KSEC2(1).EQ.20).OR.
     X             (KSEC2(1).EQ.30) ) THEN
            INOLAT    = IABS(KSEC2(4) - KSEC2(7))
            IEAST   = KSEC2(5)
Cr
Cr          Special case: symmetrical about equator, no equator
Cr          Have to calculate extreme longitude
Cr          (Fixup for reduced lat/long grids reporting last point
Cr          correctly)
Cr
            IF((MOD(KSEC2(3),2).EQ.0).AND.(KSEC2(4).EQ.(-KSEC2(7))))THEN
              N = KSEC2(3)/2
              NP = KSEC2(22+N)
              NINC = 360000/NP
              KSEC2(8) = IEAST + NINC*(NP-1)
              KSEC2(9) = NINC
            ENDIF
            IWEST   = KSEC2(8)
            IF( IWEST .LT. IEAST ) IWEST = IWEST + 360000
            INOLNG    = IABS(IWEST - IEAST)
Cr
Cr          Allow for missing Di or Dj increment.
Cr
            IF( KSEC2(9) .EQ. IMISNG ) THEN
              INOLAT    = 1 + INOLAT/ KSEC2(10)
              INOLNG    = 1 + INOLNG/ KSEC2(10)
              KSEC2(9)  = KSEC2(10)
            ELSE
              INOLAT    = 1 + INOLAT/ KSEC2(9)
              INOLNG    = 1 + INOLNG/ KSEC2(9)
              KSEC2(10) = KSEC2(9)
            ENDIF
Cr
Cr          Check if input domain is periodic.
Cr
            ILOEXT = KSEC2(8)+(1-2*(KSEC2(11)/128))*KSEC2(9)
            LPERIO = MOD( IABS(ILOEXT-KSEC2(5) ), 360000) .EQ. 0
            CALL QU2REG3(PSEC4,KSEC2(23),INOLAT,INOLNG,1,ZMSVAL,KRET,
     X                   LSECT3,LPERIO,LVEGGY)
            IF( KRET .NE. 0 ) THEN
              KRET = 808
              WRITE(GRPRSM,*)
     X          'GRIBEX: Error converting quasi-regular gaussian grid'
              WRITE(GRPRSM,*)
     X          'GRIBEX: to regular. Return code = ',KRET
              GO TO 900
            ENDIF
Cr
          ENDIF
Cr
          KSEC4(1)  = INOLAT * INOLNG
          KSEC2(2)  = INOLNG
          KSEC2(3)  = INOLAT
          KSEC2(6)  = 128
          KSEC2(17) = 0
          LQUASI = .FALSE.
        ENDIF
#endif
Cj
        IF( HOPER.EQ.'J' ) GOTO 900
Cj
Cd
Cd      Convert to integer if original data was integer.
Cd
        IF( ( KSEC4(5) .EQ. 32 ) .AND. (HOPER.NE.'B') )
     X    CALL RORINT(PSEC4,PSEC4,KSEC4(1),'I')
Cd
Cd      Set number of values decoded negative, if missing data.
Cd
        IF( IMISS.EQ.1) KSEC4(1) = - KSEC4(1)
Cd
Cd      If GRIB Edition number is -1 or 0, set GRIB message
Cd      length for return to user.
Cd
        IF( KSEC0(2).EQ.-1.OR.KSEC0(2).EQ.0) KSEC0(1) = INSPT / 8
Cd
        GO TO 900
Cd
      ENDIF
C
C*    Set unused part of last word to binary zeroes.
C
      KWORD    = INSPT / IBITS
      ITEMP    = KWORD * IBITS
      IOFF     = INSPT - ITEMP
      IF( IOFF.NE.0) THEN
        CALL INXBIT(KGRIB,KLENG,INSPT,0,1,IBITS,(IBITS-IOFF),YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 809
          WRITE(GRPRSM,*)
     X  'GRIBEX: Error padding unused part of GRIB to zero'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
        KWORD = KWORD + 1
      ENDIF
C
C*    Round length to a multiple of 120 octets, if required,
C     Set any additional words to 0.
C
      IF( (NRND.EQ.1).OR.LLARGE ) THEN
        I = INSPT / 960
        I = I * 960
        I = INSPT - I
        IF( I.NE.0) THEN
          I = (960 - I)
          ITEMP = MOD(I,8)
          CALL INXBIT(KGRIB,KLENG,INSPT,0,1,IBITS,ITEMP,YFUNC,KRET)
          IF( KRET.NE.0) THEN
            KRET = 810
            WRITE(GRPRSM,*)
     X  'GRIBEX: Error padding GRIB to multiple of 120'
            WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
            GO TO 900
          ENDIF
          ITEMP = I/8
          DO 820 JLOOP = 1, ITEMP
            CALL INXBIT(KGRIB,KLENG,INSPT,0,1,IBITS,8,YFUNC,KRET)
            IF( KRET.NE.0) THEN
              KRET = 810
              WRITE(GRPRSM,*)
     X  'GRIBEX: Error padding GRIB to multiple of 120'
              WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
              GO TO 900
            ENDIF
  820     CONTINUE
          I = I / IBITS
        ENDIF
        KWORD = KWORD + I
      ENDIF
Ce
      IF( LLARGE.AND.LENCODE ) THEN
Ce
Ce      Because of the restriction on product lengths to 3 octets, if
Ce      more than 24 bits is needed, rescale by a factor of -120 to give
Ce      a (smaller) negative count.  It is only possible because
Ce      the (default) rounding for GRIB products is 120 bytes.
Ce
        ITEMP = (KWORD*(IBITS/8)) / (-120)
Ce
Ce      When encoding, use most-significant bit as a sign bit.
Ce
        CALL CSGNBT( KSEC0(1), ITEMP, 24, KRET)
Ce
Ce      Insert product length field.
        ITEMP = 32
        CALL INXBIT(KGRIB,KLENG,ITEMP,KSEC0(1),1,IBITS, 24,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 802
          WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting length of GRIB message'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
Ce
Ce      If encoding a very large product, the section 4 length field
Ce      holds the number of bytes in the product after section 4 upto
Ce      the end of the padding bytes. In this case, the setting
Ce      of this field has been postponed upto this point.
Ce
        IPLEN = IPLEN - 32
        ILEN4 = KWORD*(IBITS/8) - (IP7777/8)
Ce
Ce      Insert section 4 length.
        CALL INXBIT(KGRIB,KLENG,IPLEN,ILEN4,1,IBITS, 24,YFUNC,KRET)
        IF( KRET.NE.0) THEN
          KRET = 802
          WRITE(GRPRSM,*)
     X  'GRIBEX: Error inserting length of GRIB section 4'
          WRITE(GRPRSM,*) 'GRIBEX: Return code = ', KRET
          GO TO 900
        ENDIF
      ENDIF
C
C     -----------------------------------------------------------------|
C
C
C
C
C
C
C
C
C
C
C*    Section 9 . Abort/return to calling routine.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      IF( LPDEBUG ) THEN
        WRITE(GRPRSM,*) 'GRIBEX: Section 9.'
        WRITE(GRPRSM,*) 'GRIBEX: Output values set -'
        IF( LDECODE ) THEN
          CALL GRPRS0(KSEC0)
          CALL GRPRS1(KSEC0,KSEC1)
Cd
Cd        Print section 2 if present.
Cd
          IF( LSECT2 ) CALL GRPRS2(KSEC0,KSEC2,PSEC2)
Cd
Cd        Print section 3 if present.
Cd
          IF( LSECT3 ) CALL GRPRS3(KSEC0,KSEC3,PSEC3)
Cd
          CALL GRPRS4(KSEC0,KSEC4,PSEC4)
Cd
Cd        Special print for 2D spectra wave field real values in
Cd        section 4
Cd
          IF( (KSEC1(1).EQ.140) .AND.
     X        (KSEC1(2).EQ. 98) .AND.
     X        (KSEC1(24).EQ. 1) .AND.
     X        ( (KSEC1(40).EQ.1045).OR.(KSEC1(40).EQ.1081) ) .AND.
     X        ( (KSEC1(6) .EQ. 250).OR.(KSEC1(6) .EQ. 251) ) )
     X      CALL GRPRS4W(KSEC4)
        ENDIF
      ENDIF
C
C     Effective number of points in masked field returned in KSEC4(21).
C     Bit-map pointer within masked field returned in KSEC4(22).
C
      IF( LSECT3 ) THEN
        KSEC4(21) = NONMISS
        KSEC4(22) = IBMAP
      ENDIF
C
C     Length of section 4 (octets) in KSEC4(23).
C     Unused bit count returned in KSEC4(24).
C
      KSEC4(23) = ILEN4
      KSEC4(24) = INIL
C
C*    If no error has been encountered, set return code to informative
C     value, if required.
C
C     Set pseudo-GRIB data encountered.
C
      IF( KRET.EQ.0.AND.IPSEUD.NE.0) KRET = IPSEUD
C
C     Set data with bit-map encountered (unless 'J' option in use).
C
      IF( (KRET.EQ.0.AND.ISBMAP.NE.0).AND.(HOPER.NE.'J')) KRET = ISBMAP
C
C Dumping data values on a file if an error occurs and the environment
C variable GRIBEX_DUMP_DATA_ON_ERROR is set
        IF (( HOPER.EQ.'C'.OR.HOPER.EQ.'M'.OR.HOPER.EQ.'K')
     x  .AND.DUMPDATA.NE.0) THEN
          IDUMP = 42  ! FIXME: not guaranteed to be universally unique
          OPEN(IDUMP,FILE=DUMPPATH,FORM='FORMATTED',STATUS='UNKNOWN')
          DO jloop=1,klenp
            WRITE(IDUMP,'(E20.10)') PSEC4(jloop)
          ENDDO
          CLOSE(IDUMP)
        ENDIF
C
C*    Abort if an error has been encountered and user has requested
C     an abort. Informative values are negative and do not cause an
C     abort.
C
      IF( (NOABORT.EQ.0) .AND. (KRET.GT.0) ) THEN
C
C       Try to print some useful information before aborting
C
        WRITE(GRPRSM,*) 'GRIBEX: ********************************'
        WRITE(GRPRSM,*) 'GRIBEX: Version is ',YGRIBEX
        WRITE(GRPRSM,*) 'GRIBEX: KLENP = ', KLENP
        WRITE(GRPRSM,*) 'GRIBEX: KLENG = ', KLENG
        WRITE(GRPRSM,*) 'GRIBEX: HOPER = ', HOPER
        WRITE(GRPRSM,*) 'GRIBEX: ********************************'
        DO 910 JLOOP = 1, 43
          WRITE(GRPRSM,*) 'GRIBEX: KSEC1(',JLOOP,') = ', KSEC1(JLOOP)
  910   CONTINUE
        WRITE(GRPRSM,*) 'GRIBEX: ********************************'
        DO 920 JLOOP = 1, 22
          WRITE(GRPRSM,*) 'GRIBEX: KSEC2(',JLOOP,') = ', KSEC2(JLOOP)
  920   CONTINUE
        WRITE(GRPRSM,*) 'GRIBEX: ********************************'
        DO 930 JLOOP = 1, 10
          WRITE(GRPRSM,*) 'GRIBEX: PSEC2(',JLOOP,') = ', PSEC2(JLOOP)
  930   CONTINUE
        WRITE(GRPRSM,*) 'GRIBEX: ********************************'
        DO 940 JLOOP = 1, 2
          WRITE(GRPRSM,*) 'GRIBEX: KSEC3(',JLOOP,') = ', KSEC3(JLOOP)
  940   CONTINUE
        WRITE(GRPRSM,*) 'GRIBEX: ********************************'
        DO 950 JLOOP = 1, 2
          WRITE(GRPRSM,*) 'GRIBEX: PSEC3(',JLOOP,') = ', PSEC3(JLOOP)
  950   CONTINUE
        WRITE(GRPRSM,*) 'GRIBEX: ********************************'
        DO 960 JLOOP = 1, 20
          WRITE(GRPRSM,*) 'GRIBEX: KSEC4(',JLOOP,') = ', KSEC4(JLOOP)
  960   CONTINUE
        WRITE(GRPRSM,*) 'GRIBEX: ********************************'
        DO 970 JLOOP = 1, 20
          WRITE(GRPRSM,*) 'GRIBEX: PSEC4(',JLOOP,') = ', PSEC4(JLOOP)
  970   CONTINUE
        WRITE(GRPRSM,*) 'GRIBEX: ********************************'
        CALL ABORTX ('GRIBEX')
      ELSE
        RETURN
      ENDIF
C
      END


