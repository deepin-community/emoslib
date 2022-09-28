C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GRPRS2 (KSEC0,KSEC2,PSEC2)
C
C---->
C**** GRPRS2 - Print information from Section 2 of GRIB code.
C
C     Purpose.
C     --------
C
C     Print the information in the Grid Description
C     Section (Section 2) of decoded GRIB data.
C
C**   Interface.
C     ----------
C
C     CALL GRPRS2 (KSEC0,KSEC2,PSEC2)
C
C
C     Input Parameters.
C     -----------------
C
C     KSEC0 - Array of decoded integers from Section 0.
C
C     KSEC2 - Array of decoded integers from Section 2.
C
C     PSEC2 - Array of decoded reals from Section 2.
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
C     Flag fields are printed in binary representation
C     Other fields as integers or reals, as appropriate.
C     Fields printed depend on GRIB Edition.
C
C
C     Externals.
C     ----------
C
C     PRTBIN   - Print binary representation of value.
C     PTQUASI  - Print quasi-grid data.
C
C     Common block GRPRSCM.
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
C     J. Hennessy    ECMWF 18.06.91
C
C
C     Modifications.
C     --------------
C
C     J. Hennessy    ECMWF        02.09.91
C     Polar stereographic print added.
C
C     J. Hennessy    ECMWF        25.09.91
C     Space view perspective print added.
C
C     J.D.Chambers   ECMWF        21.02.95
C     Quasi-regular lat/long grids added.
C
C     J.D.Chambers     ECMWF      03.04.95
C     Allow Lambert conformal.
C
C     J.D.Chambers   ECMWF    June 2002
C     Add option to set output stream number using environment variable
C     GRPRS_STREAM
C
C     J.D.Chambers     ECMWF      July 2004
C     Allow Mercator
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
C
      INTEGER KSEC0,KSEC2
      DIMENSION KSEC0(*)
      DIMENSION KSEC2(*)
      REAL PSEC2
      DIMENSION PSEC2(*)
C
C     Local variables
C
      INTEGER I, IBIT, IEDIT, IERR, IOUT, IRESOL
C
C
C     -----------------------------------------------------------------|
C*    Section 1 . Print GRIB Edition number.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      IEDIT = KSEC0(2)
      WRITE(GRPRSM,9000)
      WRITE(GRPRSM,9001)
      WRITE(GRPRSM,9002)
C
C     -----------------------------------------------------------------|
C*    Section 2 . Print spherical harmonic data.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IF (KSEC2(1).EQ.50 .OR. KSEC2(1).EQ.60 .OR.
     X    KSEC2(1).EQ.70 .OR. KSEC2(1).EQ.80) THEN
        WRITE(GRPRSM,9111) KSEC2(1)
        WRITE(GRPRSM,9102) KSEC2(2)
        WRITE(GRPRSM,9103) KSEC2(3)
        WRITE(GRPRSM,9104) KSEC2(4)
        WRITE(GRPRSM,9105) KSEC2(5)
        WRITE(GRPRSM,9106) KSEC2(6)
        DO I = 7, 11
          WRITE(GRPRSM,9107) KSEC2(I)
        ENDDO
        WRITE(GRPRSM,9212) KSEC2(12)
        GO TO 800
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3 . Print Gaussian grid data.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      IF (KSEC2(1).EQ.4 .OR. KSEC2(1).EQ.14 .OR.
     X    KSEC2(1).EQ.24 .OR. KSEC2(1).EQ.34) THEN
        WRITE(GRPRSM,9200)
        WRITE(GRPRSM,9112) KSEC2(1)
C
C       Quasi-regular grids introduced in Edition 1.
C
        IF (KSEC2(17).EQ.0.OR.IEDIT.LT.1) THEN
          WRITE(GRPRSM,9201) KSEC2(2)
        ELSE
          CALL PTQUASI(KSEC2)
        ENDIF
        WRITE(GRPRSM,9202) KSEC2(3)
        WRITE(GRPRSM,9203) KSEC2(4)
        WRITE(GRPRSM,9204) KSEC2(5)
        IBIT = 8
        IRESOL = KSEC2(6) + KSEC2(18) + KSEC2(19)
        CALL PRTBIN (IRESOL,IBIT,IOUT,IERR)
        WRITE(GRPRSM,9205) IOUT
        WRITE(GRPRSM,9206) KSEC2(7)
        WRITE(GRPRSM,9207) KSEC2(8)
C
C       Print increment if given.
C
        IF (KSEC2(6).EQ.128) THEN
          WRITE(GRPRSM,9208) KSEC2(9)
        ELSE
          WRITE(GRPRSM,9236)
        ENDIF
        WRITE(GRPRSM,9210) KSEC2(10)
        IBIT = 8
        CALL PRTBIN (KSEC2(11),IBIT,IOUT,IERR)
        WRITE(GRPRSM,9211) IOUT
        WRITE(GRPRSM,9212) KSEC2(12)
        GO TO 800
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 4 . Print Latitude / longitude grid data.
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
      IF (KSEC2(1).EQ.0 .OR. KSEC2(1).EQ.10 .OR.
     X    KSEC2(1).EQ.20 .OR. KSEC2(1).EQ.30) THEN
        WRITE(GRPRSM,9200)
        WRITE(GRPRSM,9113) KSEC2(1)
C
C       Quasi-regular lat/long grids also possible.
C
        IF (KSEC2(17).EQ.0) THEN
          WRITE(GRPRSM,9201) KSEC2(2)
        ELSE
          CALL PTQUASI(KSEC2)
        ENDIF
        WRITE(GRPRSM,9202) KSEC2(3)
        WRITE(GRPRSM,9203) KSEC2(4)
        WRITE(GRPRSM,9204) KSEC2(5)
        IBIT = 8
        IRESOL = KSEC2(6) + KSEC2(18) + KSEC2(19)
        CALL PRTBIN (IRESOL,IBIT,IOUT,IERR)
        WRITE(GRPRSM,9205) IOUT
        WRITE(GRPRSM,9206) KSEC2(7)
        WRITE(GRPRSM,9207) KSEC2(8)
C
C       Print increment if given.
C
        IF ( KSEC2(9) .LT. 0) THEN
          WRITE(GRPRSM,9236)
        ELSE
          WRITE(GRPRSM,9208) KSEC2(9)
        ENDIF
        IF ( KSEC2(10) .LT. 0) THEN
          WRITE(GRPRSM,9237)
        ELSE
          WRITE(GRPRSM,9209) KSEC2(10)
        ENDIF
        IBIT = 8
        CALL PRTBIN (KSEC2(11),IBIT,IOUT,IERR)
        WRITE(GRPRSM,9211) IOUT
        WRITE(GRPRSM,9212) KSEC2(12)
        GO TO 800
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 5 . Print polar stereographic data.
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
      IF (KSEC2(1).EQ.5) THEN
        WRITE(GRPRSM,9200)
        WRITE(GRPRSM,9114) KSEC2(1)
        WRITE(GRPRSM,9301) KSEC2(2)
        WRITE(GRPRSM,9302) KSEC2(3)
        WRITE(GRPRSM,9203) KSEC2(4)
        WRITE(GRPRSM,9204) KSEC2(5)
        IBIT = 8
        IRESOL = KSEC2(18) + KSEC2(19)
        CALL PRTBIN (IRESOL,IBIT,IOUT,IERR)
        WRITE(GRPRSM,9205) IOUT
        WRITE(GRPRSM,9303) KSEC2(7)
        WRITE(GRPRSM,9304) KSEC2(9)
        WRITE(GRPRSM,9305) KSEC2(10)
        IBIT = 8
        CALL PRTBIN (KSEC2(11),IBIT,IOUT,IERR)
        WRITE(GRPRSM,9211) IOUT
        WRITE(GRPRSM,9212) KSEC2(12)
        WRITE(GRPRSM,9306) KSEC2(13)
        GO TO 800
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 6 . Print Lambert conformal data.
C     -----------------------------------------------------------------|
C
  600 CONTINUE
C
      IF (KSEC2(1).EQ.3) THEN
C
        WRITE(GRPRSM,9200)
        WRITE(GRPRSM,9115) KSEC2(1)
        WRITE(GRPRSM,9301) KSEC2(2)
        WRITE(GRPRSM,9302) KSEC2(3)
        WRITE(GRPRSM,9203) KSEC2(4)
        WRITE(GRPRSM,9204) KSEC2(5)
        IBIT = 8
        IRESOL = KSEC2(18) + KSEC2(19) + KSEC2(6)
        CALL PRTBIN (IRESOL,IBIT,IOUT,IERR)
        WRITE(GRPRSM,9205) IOUT
        WRITE(GRPRSM,9303) KSEC2(7)
        WRITE(GRPRSM,9304) KSEC2(9)
        WRITE(GRPRSM,9305) KSEC2(10)
        IBIT = 8
        CALL PRTBIN (KSEC2(11),IBIT,IOUT,IERR)
        WRITE(GRPRSM,9211) IOUT
        WRITE(GRPRSM,9212) KSEC2(12)
        WRITE(GRPRSM,9306) KSEC2(13)
        WRITE(GRPRSM,9214) KSEC2(14)
        WRITE(GRPRSM,9215) KSEC2(15)
        WRITE(GRPRSM,9216) KSEC2(20)
        WRITE(GRPRSM,9217) KSEC2(21)
        GO TO 800
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 7 . Print space view perspective or orthographic data.
C     -----------------------------------------------------------------|
C
  700 CONTINUE
C
      IF (KSEC2(1).EQ.90) THEN
        WRITE(GRPRSM,9200)
        WRITE(GRPRSM,9116) KSEC2(1)
        WRITE(GRPRSM,9301) KSEC2(2)
        WRITE(GRPRSM,9302) KSEC2(3)
        WRITE(GRPRSM,9310) KSEC2(4)
        WRITE(GRPRSM,9311) KSEC2(5)
        IBIT = 8
        IRESOL = KSEC2(18) + KSEC2(19)
        WRITE(GRPRSM,9312) KSEC2(7)
        WRITE(GRPRSM,9313) KSEC2(8)
        WRITE(GRPRSM,9314) KSEC2(9)
        WRITE(GRPRSM,9315) KSEC2(10)
        IBIT = 8
        CALL PRTBIN (KSEC2(11),IBIT,IOUT,IERR)
        WRITE(GRPRSM,9211) IOUT
        WRITE(GRPRSM,9212) KSEC2(12)
        WRITE(GRPRSM,9303) KSEC2(13)
        WRITE(GRPRSM,9316) KSEC2(14)
        WRITE(GRPRSM,9317) KSEC2(15)
        WRITE(GRPRSM,9318) KSEC2(16)
        WRITE(GRPRSM,9319) KSEC2(18)
        WRITE(GRPRSM,9320) KSEC2(19)
        GO TO 800
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 7.5 . Print ocean data
C     -----------------------------------------------------------------|
C
  750 CONTINUE
C
      IF( KSEC2(1).EQ.192 ) THEN
        WRITE(GRPRSM,9117) KSEC2(1)
        IF( KSEC2(2).EQ. 32767 ) THEN
          WRITE(GRPRSM,9501) KSEC2(2)
        ELSE
          WRITE(GRPRSM,9502) KSEC2(2)
        ENDIF
C
        IF( KSEC2(3).EQ. 32767 ) THEN
          WRITE(GRPRSM,9503) KSEC2(3)
        ELSE
          WRITE(GRPRSM,9504) KSEC2(3)
        ENDIF
C
        IBIT = 8
        CALL PRTBIN (KSEC2(11),IBIT,IOUT,IERR)
        WRITE(GRPRSM,9211) IOUT
        GO TO 800
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 7.6 . Print mercator data
C     -----------------------------------------------------------------|
C
  760 CONTINUE
C
      IF (KSEC2(1).EQ.1) THEN
        WRITE(GRPRSM,9200)
        WRITE(GRPRSM,9118) KSEC2(1)
        WRITE(GRPRSM,9201) KSEC2(2)
        WRITE(GRPRSM,9202) KSEC2(3)
        WRITE(GRPRSM,9203) KSEC2(4)
        WRITE(GRPRSM,9204) KSEC2(5)
        IBIT = 8
        IRESOL = KSEC2(6) + KSEC2(18) + KSEC2(19)
        CALL PRTBIN (IRESOL,IBIT,IOUT,IERR)
        WRITE(GRPRSM,9205) IOUT
        WRITE(GRPRSM,9206) KSEC2(7)
        WRITE(GRPRSM,9207) KSEC2(8)
        WRITE(GRPRSM,9218) KSEC2(9)
        WRITE(GRPRSM,9208) KSEC2(13)
        WRITE(GRPRSM,9209) KSEC2(14)
        IBIT = 8
        CALL PRTBIN (KSEC2(11),IBIT,IOUT,IERR)
        WRITE(GRPRSM,9211) IOUT
        WRITE(GRPRSM,9212) KSEC2(12)
        GO TO 800
      ENDIF
C
C     -----------------------------------------------------------------|
C     Drop through to here => representation type not catered for.
C     -----------------------------------------------------------------|
C
      WRITE(GRPRSM,*)
     X  'GRPRS2 :Data representation type not catered for -', KSEC2(1)
C
      GO TO 900
C
C     -----------------------------------------------------------------|
C*    Section 8 . Print vertical coordinate parameters,
C                 rotated grid information,
C                 stretched grid information, if any.
C     -----------------------------------------------------------------|
C
  800 CONTINUE
C
C     Vertical coordinate parameters ...
C
      IF (KSEC2(12).NE.0) THEN
        WRITE(GRPRSM,9000)
        WRITE(GRPRSM,9400)
        WRITE(GRPRSM,9401)
        DO I = 11, (KSEC2(12)+10)
          WRITE(GRPRSM,9402) PSEC2(I)
        ENDDO
      ENDIF
C
C     Rotated and stretched grids introduced in Edition 1.
C
      IF (IEDIT.LT.1) GO TO 900
C
C*    Rotated grid information ...
C
      IF (KSEC2(1).EQ.10 .OR. KSEC2(1).EQ.30 .OR.
     X    KSEC2(1).EQ.14 .OR. KSEC2(1).EQ.34 .OR.
     X    KSEC2(1).EQ.60 .OR. KSEC2(1).EQ.80 .OR.
     X    KSEC2(1).EQ.30) THEN
        WRITE(GRPRSM,9000)
        WRITE(GRPRSM,9220) KSEC2(13)
        WRITE(GRPRSM,9221) KSEC2(14)
        WRITE(GRPRSM,9222) PSEC2(1)
      ENDIF
C
C*    Stretched grid information ...
C
      IF (KSEC2(1).EQ.20 .OR. KSEC2(1).EQ.30 .OR.
     X    KSEC2(1).EQ.24 .OR. KSEC2(1).EQ.34 .OR.
     X    KSEC2(1).EQ.70 .OR. KSEC2(1).EQ.80) THEN
        WRITE(GRPRSM,9000)
        WRITE(GRPRSM,9230) KSEC2(15)
        WRITE(GRPRSM,9231) KSEC2(16)
        WRITE(GRPRSM,9232) PSEC2(2)
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9 . Format statements. Return to calling routine.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
C
 9000 FORMAT (1H )
 9001 FORMAT (1H ,'Section 2 - Grid Description Section.')
 9002 FORMAT (1H ,'-------------------------------------')
 9102 FORMAT (1H ,'J - Pentagonal resolution parameter.         ',I9)
 9103 FORMAT (1H ,'K - Pentagonal resolution parameter.         ',I9)
 9104 FORMAT (1H ,'M - Pentagonal resolution parameter.         ',I9)
 9105 FORMAT (1H ,'Representation type (Table 9)                ',I9)
 9106 FORMAT (1H ,'Representation mode (Table 10).              ',I9)
 9107 FORMAT (1H ,'Not used.                                    ',I9)
 9111 FORMAT (1H ,'Data represent type = spectral     (Table 6) ',I9)
 9112 FORMAT (1H ,'Data represent type = gaussian     (Table 6) ',I9)
 9113 FORMAT (1H ,'Data represent type = lat/long     (Table 6) ',I9)
 9114 FORMAT (1H ,'Data represent type = polar stereo (Table 6) ',I9)
 9115 FORMAT (1H ,'Data represent type = Lambert      (Table 6) ',I9)
 9116 FORMAT (1H ,'Data represent type = space/ortho  (Table 6) ',I9)
 9117 FORMAT (1H ,'Data represent type = ECMWF ocean  (Table 6) ',I9)
 9118 FORMAT (1H ,'Data represent type = Mercator  (Table 6)    ',I9)
C
 9200 FORMAT (1H ,'(Southern latitudes and Western longitudes',
     C            ' are negative.)')
 9201 FORMAT (1H ,'Number of points along a parallel.           ',I9)
 9202 FORMAT (1H ,'Number of points along a meridian.           ',I9)
 9203 FORMAT (1H ,'Latitude of first grid point.                ',I9)
 9204 FORMAT (1H ,'Longitude of first grid point.               ',I9)
 9205 FORMAT (1H ,'Resolution and components flag.               ',I8.8)
 9206 FORMAT (1H ,'Latitude of last grid point.                 ',I9)
 9207 FORMAT (1H ,'Longitude of last grid point.                ',I9)
 9208 FORMAT (1H ,'i direction (East-West) increment.           ',I9)
 9209 FORMAT (1H ,'j direction (North-South) increment.         ',I9)
 9210 FORMAT (1H ,'Number of parallels between pole and equator.',I9)
 9211 FORMAT (1H ,'Scanning mode flags (Code Table 8)            ',I8.8)
 9212 FORMAT (1H ,'Number of vertical coordinate parameters.    ',I9)
C
 9214 FORMAT (1H ,'Latitude intersection 1 - Latin 1 -.         ',I9)
 9215 FORMAT (1H ,'Latitude intersection 2 - Latin 2 -.         ',I9)
 9216 FORMAT (1H ,'Latitude of Southern Pole.                   ',I9)
 9217 FORMAT (1H ,'Longitude of Southern Pole.                  ',I9)
 9218 FORMAT (1H ,'Latitude projection intersects Earth(Latin)  ',I9)
C
 9220 FORMAT (1H ,'Latitude of southern pole of rotation.       ',I9)
 9221 FORMAT (1H ,'Longitude of southern pole of rotation.      ',I9)
 9222 FORMAT (1H ,'Angle of rotation.                     ',F20.10)
C
 9230 FORMAT (1H ,'Latitude of pole of stretching.              ',I9)
 9231 FORMAT (1H ,'Longitude of pole of stretching.             ',I9)
 9232 FORMAT (1H ,'Stretching factor.                     ',F20.10)
 9233 FORMAT (1H ,'Number of points along a parallel varies.')
 9235 FORMAT (1H , I5,16X,A10)
 9236 FORMAT (1H ,'i direction (East-West) increment',
     X        '            Not given')
 9237 FORMAT (1H ,'j direction (North-South) increment',
     X        '          Not given')
C
 9301 FORMAT (1H ,'Number of points along X axis.               ',I9)
 9302 FORMAT (1H ,'Number of points along Y axis.               ',I9)
 9303 FORMAT (1H ,'Orientation of the grid.                     ',I9)
 9304 FORMAT (1H ,'X direction increment.                       ',I9)
 9305 FORMAT (1H ,'Y direction increment.                       ',I9)
 9306 FORMAT (1H ,'Projection centre flag.                      ',I9)
C
 9310 FORMAT (1H ,'Latitude of sub-satellite point.             ',I9)
 9311 FORMAT (1H ,'Longitude of sub-satellite point.            ',I9)
 9312 FORMAT (1H ,'Diameter of the earth in x direction.        ',I9)
 9313 FORMAT (1H ,'Diameter of the earth in y direction.        ',I9)
 9314 FORMAT (1H ,'X coordinate of sub-satellite point.         ',I9)
 9315 FORMAT (1H ,'Y coordinate of sub-satellite point.         ',I9)
 9316 FORMAT (1H ,'Altitude of the camera.                      ',I9)
 9317 FORMAT (1H ,'Y coordinate of origin of sector image.      ',I9)
 9318 FORMAT (1H ,'X coordinate of origin of sector image.      ',I9)
 9319 FORMAT (1H ,'Earth flag                                   ',I9)
 9320 FORMAT (1H ,'Components flag                              ',I9)
C
 9400 FORMAT (1H ,'Vertical Coordinate Parameters.')
 9401 FORMAT (1H ,'-------------------------------')
 9402 FORMAT (4X,F20.12,4X,F20.12)
C
 9501 FORMAT(' Number of points along the first axis.       Not used')
 9502 FORMAT (' Number of points along the first axis.       ',I9)
 9503 FORMAT(' Number of points along the second axis.      Not used')
 9504 FORMAT (' Number of points along the second axis.      ',I9)
C
      END
