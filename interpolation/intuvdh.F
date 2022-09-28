C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INTUVDH(PDATA,KSIZE,KGRIB,KGRBSZ,HOPER,KPARAM)
C
C---->
C**** INTUVDH
C
C     Purpose
C     -------
C
C     Encodes/decodes data into/from GRIB code.
C
C
C     Interface
C     ---------
C
C     IRET = INTUVDH(PDATA,KSIZE,KGRIB,KGRBSZ,HOPER,KPARAM)
C
C     Input
C     -----
C
C     PDATA - Unpacked field values (if encoding)
C     KSIZE - Number of unpacked values (if encoding)
C     KGRIB  - Input field in GRIB format (if decoding)
C     KGRBSZ - Input field length (words) (if decoding)
C     HOPER  - Coding option ('I', 'D' or 'C' only)
C     KPARAM - Parameter number of the field (for encoding)
C
C
C     Output
C     ------
C
C     PDATA  - Unpacked field values (if decoding)
C     KSIZE  - Number of unpacked values (if decoding)
C     KGRIB  - Output field in GRIB format (if encoding)
C     KGRBSZ - Output field length (words) (if encoding)
C
C
C     Method
C     ------
C
C     Uses arrays in intf.h for unpacking GRIB headers.
C
C
C     Externals
C     ---------
C
C     GRSVCK  - Turns off GRIBEX checking.
C     GRIBEX  - Encode/decode GRIB product.
C     IGSETUP - Setup GRIB sections for the output product.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     February 2001
C
C
C----<
C
C     -----------------------------------------------------------------|
C*    Section 0.   Variables
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "parim.h"
#include "intf.h"
#include "nifld.common"
#include "nofld.common"
C
C     Function arguments
C
      REAL PDATA(*)
      INTEGER KSIZE,KGRIB(*),KGRBSZ, KPARAM
      CHARACTER*1 HOPER
C
C     Local variables
C
      INTEGER IERR, IWORD
      CHARACTER*1 HFUNC
C
C     Externals
C
      INTEGER IGSETUP
      EXTERNAL IGSETUP
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
      INTUVDH = 0
      IERR    = 0
C
C     -----------------------------------------------------------------|
C*    Section 2.   Decode data from GRIB code (no checking).
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IF( HOPER.NE.'C' ) THEN
        CALL GRSVCK(0)
C
        IERR = 1
        ISEC3(2) = NINT(RMISSGV)
        ZSEC3(2) = RMISSGV
        CALL GRIBEX(ISEC0, ISEC1, ISEC2, ZSEC2, ISEC3, ZSEC3, ISEC4,
     X              PDATA, KSIZE, KGRIB, KGRBSZ, IWORD, HOPER,IERR)
C
        INTUVDH = IERR
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3.   Encode data into GRIB code.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
C     Setup GRIB sections for the output product
C
      IERR = IGSETUP(ISEC1,ISEC2,ISEC3,ISEC4)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTUVDH: GRIB section setup fail',JPQUIET)
        INTUVDH = IERR
        GOTO 900
      ENDIF
C
C     Code data into GRIB
C
      ISEC1(6) = KPARAM
      IERR = 1
C
C     If grid-point output, setup for 2nd order packing if requested.
C
      IF( (NOREPR.NE.JPSPHERE) .AND. (NOREPR.NE.JPSPHROT) ) THEN
        HFUNC = 'C'
        IF( NOHFUNC.EQ.'K' ) THEN
          HFUNC = 'K'
          ISEC4(4)  = 64
          ISEC4(6)  = 16
          ISEC4(9)  = 32
          ISEC4(10) = 16
          ISEC4(12) = 8
          ISEC4(13) = 4
          ISEC4(14) = 0
          ISEC4(15) = -1
        ELSE
          ISEC4(4)  = 0
          ISEC4(6)  = 0
        ENDIF
        IF( LIMISSV ) THEN
          ISEC1(5) = 192
          ISEC3(2) = NINT(RMISSGV)
          ZSEC3(2) = RMISSGV
        ENDIF
      ELSE
        ISEC1(5) = 128
        HFUNC = 'C'
        IF( NOHFUNC.EQ.'C' ) THEN
          ISEC2(6) = 2
          ISEC4(4) = 64
        ELSE IF( NOHFUNC.EQ.'S' ) THEN
          ISEC2(6) = 1
          ISEC4(4) = 0
        ENDIF
      ENDIF
C
      CALL GRIBEX( ISEC0, ISEC1, ISEC2, ZSEC2, ISEC3, ZSEC3, ISEC4,
     X             PDATA,KSIZE,KGRIB,KGRBSZ,IWORD,HFUNC,IERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'INTUVDH: GRIBEX encoding failed.',IERR)
        INTUVDH = IERR
        GOTO 900
      ENDIF
      KGRBSZ = IWORD
      INTUVDH = 0
C
C     -----------------------------------------------------------------|
C*    Section 9.   Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
