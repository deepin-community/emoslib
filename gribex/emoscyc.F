C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION EMOSCYC(KONOFF,KCYCLES)
C
C---->
C**** EMOSCYC
C
C     Purpose
C     -------
C
C     Returns current EMOS_CYCLE version number.
C
C
C     Interface
C     ---------
C
C     INUM = EMOSCYC(KONOFF,KCYCLES)
C
C     Input
C     -----
C
C     KONOFF - switch for displayed message
C              = 0 if display is required on the first call
C              = non-zero if display is not required
C
C
C     Output
C     ------
C
C     Returns a 6-digit version number, aaabbc, where:
C       aaa = 3-digit major number
C       bb  = 2-digit minor number
C       c   = 1-digit spare number (normally 0)
C
C     KCYCLES(1) = INTP_CYCLE number
C     KCYCLES(2) = BUFR_CYCLE number
C     KCYCLES(3) = GRIB_CYCLE number
C     KCYCLES(4) = PBIO_CYCLE number
C     
C
C     Method
C     ------
C
C     Reads a 6-digit version number from the environment variable
C     EMOS_CYCLE. If this does not give a 6-digit number, an internal
C     hard-coded default value is used.
C
C     On the first call, the function (optionally) displays a message:
C
C       **************************************
C       * EMOS_CYCLE version number = nnnnnn *
C       * INTP_CYCLE = iiiiii
C       * BUFR_CYCLE = bbbbbb
C       * GRIB_CYCLE = gggggg
C       * PBIO_CYCLE = pppppp
C       **************************************
C     
C
C     Externals
C     ---------
C
C     None.
C     
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     May 1998
C
C
C----<
C ------------------------------------------------------------------
C*    Section 0.   Variables.
C ------------------------------------------------------------------
C
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Function arguments
C
      INTEGER KONOFF, KCYCLES(4)
C
C     Local variables
C
      INTEGER INUMBER, ICOUNT, IOFFSET
      SAVE INUMBER, ICOUNT
      CHARACTER*38 CMESS
      CHARACTER*20 YNUMBER
C
C     Externals
C
      INTEGER INTPNUM, BUFRNUM, GRIBNUM, PBIONUM
C
      DATA INUMBER/100000/, ICOUNT/0/
      DATA CMESS/'* EMOS_CYCLE version number = ****** *'/
C
C ------------------------------------------------------------------
C*    Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
C
      IF( ICOUNT.EQ.0 ) THEN
C
C       See if the environment variable has an override value
C
        CALL GETENV( 'EMOS_CYCLE', YNUMBER)
        IOFFSET = INDEX( YNUMBER, ' ')
        IF( IOFFSET.EQ.7 ) THEN
          READ(YNUMBER,'(I6.6)') INUMBER
        ENDIF
C
      KCYCLES(1) = INTPNUM(1)
      KCYCLES(2) = BUFRNUM(1)
      KCYCLES(3) = GRIBNUM(1)
      KCYCLES(4) = PBIONUM(1)
C
C       First time through, display the message if required
C
        IF( KONOFF.EQ.0 ) THEN
          WRITE(CMESS(31:36),'(I6.6)') INUMBER
          WRITE(GRPRSM,*) '**************************************'
          WRITE(GRPRSM,*) CMESS
          CMESS(3:6) = 'INTP'
          WRITE(CMESS(31:36),'(I6.6)') KCYCLES(1)
          WRITE(GRPRSM,*) CMESS
          CMESS(3:6) = 'BUFR'
          WRITE(CMESS(31:36),'(I6.6)') KCYCLES(2)
          WRITE(GRPRSM,*) CMESS
          CMESS(3:6) = 'GRIB'
          WRITE(CMESS(31:36),'(I6.6)') KCYCLES(3)
          WRITE(GRPRSM,*) CMESS
          CMESS(3:6) = 'PBIO'
          WRITE(CMESS(31:36),'(I6.6)') KCYCLES(4)
          WRITE(GRPRSM,*) CMESS
          WRITE(GRPRSM,*) '**************************************'
        ENDIF
        ICOUNT = 1
      ENDIF
C
      EMOSCYC = INUMBER
C
      RETURN
      END
