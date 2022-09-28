C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION MKBITMP(KWE,KNS,ZSEC4,ZMISS)
C
C---->
C**** MKBITMP
C
C     Purpose
C     -------
C
C     Remove points using a bitmap.
C
C
C     Interface
C     ---------
C
C     CALL  MKBITMP(KWE,KNS,ZSEC4,ZMISS)
C
C     Input
C     -----
C
C     KWE    - Number of points west-east in input field.
C     KNS    - Number of points north-south in input field.
C     ZSEC4  - GRIBEX section 4 values in the field.
C     ZMISS  - Value to be used as the missing data value..
C
C
C     Output
C     ------
C
C     ZSEC4  - GRIBEX section 4 values with the points marked as
C              'missing' by the bitmap replaced by missing data values.
C
C     Function returns 0 if all OK.
C
C
C     Method
C     ------
C
C     The field and bitmap are rectangular.
C     Use NOBITMP, the name of a file describing the bitmap.
C
C
C     Externals
C     ---------
C
C     INTLOG  - Log error message.
C     MAKEMAP - Build a bitmap from definition in a file.
C     GMAPBIT - Get the bit value from a given position in a bitmap.
C     JFREE   - Frees dynamically allocated memory.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     April 2000.
C
C----<
C
      IMPLICIT NONE
C
C     Subroutine arguments
C
      INTEGER KWE,KNS
      REAL ZSEC4(*), ZMISS
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER ( JPROUTINE = 44000 )
C
C     Local variables
C
      INTEGER IRET,BITMAP,NROWS,NCOLS,NVALUE,NEXT,IROW,ICOL,NINDEX
      INTEGER OLDROWS, OLDCOLS, LOOP
      CHARACTER*256 OLDFILE
      DATA OLDFILE/' '/, BITMAP/-1/
      SAVE BITMAP,OLDROWS,OLDCOLS,OLDFILE
C
C     Externals
C
      INTEGER  MAKEMAP, GMAPBIT, JINDEX
      EXTERNAL MAKEMAP, GMAPBIT, JINDEX
C
#include "parim.h"
#include "jparams.h"
#include "nofld.common"
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      MKBITMP = 0
C
C     Only build bitmap if filename has changed since last time through
C
      NINDEX = JINDEX(NOBITMP)
      IF( NINDEX.LT.1 ) THEN
        CALL INTLOG(JP_WARN,'MKBITMP: No bitmap name given',JPQUIET)
        CALL INTLOG(JP_WARN,'MKBITMP: BITMAP NOT APPLIED',JPQUIET)
        MKBITMP = JPROUTINE + 1
        GOTO 900
      ENDIF
C
      IF( OLDFILE(1:NINDEX).NE.NOBITMP(1:NINDEX) ) THEN
        OLDFILE(1:NINDEX) = NOBITMP(1:NINDEX)
        IF( NDBG.NE.0 )
     X    CALL INTLOG(JP_DEBUG,
     X      'MKBITMP: New BITMAP: '//OLDFILE(1:NINDEX),JPQUIET)
C
        IF( BITMAP.NE.-1 ) CALL JFREE(BITMAP)
C
        IRET = MAKEMAP(NOBITMP,NROWS,NCOLS,BITMAP)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_WARN,'MKBITMP: Failed to make bitmap',IRET)
          CALL INTLOG(JP_WARN,'MKBITMP: BITMAP NOT APPLIED',JPQUIET)
          DO LOOP = 1, 256
            OLDFILE(LOOP:LOOP) = ' '
          ENDDO
          MKBITMP = JPROUTINE + 2
          GOTO 900
        ENDIF
        OLDROWS = NROWS
        OLDCOLS = NCOLS
      ELSE
        NROWS = OLDROWS
        NCOLS = OLDCOLS
      ENDIF
C
      IF( (NROWS.NE.KNS).OR.(NCOLS.NE.KWE) ) THEN
        CALL INTLOG(JP_WARN,'MKBITMP: Bitmap invalid for area',JPQUIET)
        CALL INTLOG(JP_WARN,'MKBITMP: No. of subarea rows    = ',KNS)
        CALL INTLOG(JP_WARN,'MKBITMP: No. of subarea columns = ',KWE)
        CALL INTLOG(JP_WARN,'MKBITMP: No. of bitmap rows    = ',NROWS)
        CALL INTLOG(JP_WARN,'MKBITMP: No. of bitmap columns = ',NCOLS)
        CALL INTLOG(JP_WARN,'MKBITMP: BITMAP NOT APPLIED',JPQUIET)
        MKBITMP = JPROUTINE + 3
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2.   Use bitmap to force missing values
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IF( NDBG.NE.0 )
     X  CALL INTLOG(JP_DEBUG,'MKBITMP: BITMAP applied',JPQUIET)
C
      NEXT = 0
      DO IROW = 1, KNS
        DO ICOL = 1, KWE
          NEXT = NEXT + 1
          NVALUE = GMAPBIT(BITMAP,NCOLS,IROW,ICOL)
          IF( NVALUE.EQ.0 ) ZSEC4(NEXT) = ZMISS
        ENDDO
      ENDDO
C
C     -----------------------------------------------------------------|
C*    Section 9.   Closedown.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
