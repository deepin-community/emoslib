C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE PTQUASI (KSEC2)
C
C---->
C**** PTQUASI
C
C     Purpose.
C     --------
C
C     Print the qusai-regular information in the Grid Description
C     Section (Section 2) of decoded GRIB data.
C
C**   Interface.
C     ----------
C
C     CALL PTQUASI (KSEC2)
C
C
C     Input Parameters.
C     -----------------
C
C     KSEC2 - Array of decoded integers from Section 2.
C
C
C     Output Parameters.
C     ------------------
C
C     None.
C
C
C     Method.
C     -------
C
C     See below.
C
C
C     Externals.
C     ----------
C
C     PRTBIN
C
C
C     Reference.
C     ----------
C
C     WMO Manual on Codes re GRIB Code.
C
C
C     Comments.
C     ---------
C
C     Only data representation types catered for are Gaussian
C     grid, latitude/longitude grid, Spherical Harmonics,
C     Polar stereographic and Space view perspective.
C
C
C     Author.
C     -------
C
C     J.D.Chambers   ECMWF 21.02.95
C
C
C     Modifications.
C     --------------
C
C     None.
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
C     Subroutine arguments
      INTEGER KSEC2
      DIMENSION KSEC2(*)
C
C     Local variables
      CHARACTER*12 YOUT
C
      INTEGER NEXTLAT, NREPEAT, LATCNT
      INTEGER J120
C
      LOGICAL NTOS
C
C     -----------------------------------------------------------------
C*    Section 1. Print quasi-grid data.
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
C     See if scanning is north->south or south->north
C
      WRITE(GRPRSM,*) ' Number of points along a parallel varies.'
      NTOS = ( MOD(KSEC2(11),128) .LT. 64 )
      IF ( NTOS ) THEN
        WRITE(GRPRSM,*)
     X    ' Number of points.   Parallel. (North to South)'
      ELSE
        WRITE(GRPRSM,*)
     X    ' Number of points.   Parallel. (South to North)'
      ENDIF
C
C     Display number of points for each latitude
      LATCNT  = KSEC2(3)
      NEXTLAT = 0
      YOUT    = ' '
C
      DO 120 J120 = 1,LATCNT
        NEXTLAT = NEXTLAT + 1
        WRITE (YOUT(1:4),'(I4)') NEXTLAT
C
C       Finished?
        IF (NEXTLAT.GT.LATCNT) GO TO 900
        IF (NEXTLAT.EQ.LATCNT) THEN
          WRITE(GRPRSM,9005) KSEC2(NEXTLAT+22) , YOUT
          GO TO 900
        ENDIF
C
C       Look for neighbouring latitudes with same number of points
C
        NREPEAT = 0
  110   CONTINUE
C
C       If neighbouring latitudes have same number of points
C       increase the repeat count.
        IF (KSEC2(NEXTLAT+22+1).EQ.KSEC2(NEXTLAT+22)) THEN
          NREPEAT = NREPEAT + 1
          NEXTLAT = NEXTLAT + 1
          IF (NEXTLAT.LT.LATCNT) GO TO 110
        ENDIF
C
C       Display neighbouring latitudes with same number of points as
C       'nn to mm'.
        IF (NREPEAT.GE.1) THEN
          YOUT(5:) = ' to '
          WRITE (YOUT(9:12),'(I4)') NEXTLAT
        ENDIF
        WRITE(GRPRSM,9005) KSEC2(NEXTLAT+22) , YOUT
        YOUT = ' '
C
  120 CONTINUE
C
C     -----------------------------------------------------------------
C*    Section 9 . Format statements. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
C
 9005 FORMAT (1H , I5,16X,A12)
C
      END
