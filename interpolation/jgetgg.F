C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JGETGG(KNUM,HTYPE,PLAT,KPTS,KRET)
C
C---->
C**** JGETGG
C
C     PURPOSE
C     _______
C
C     This routine reads the definition of a gaussian grid.
C
C
C     INTERFACE
C     _________
C
C     CALL JGETGG(KNUM,HTYPE,PLAT,KPTS,KRET)
C
C
C     Input parameters
C     ________________
C
C     KNUM     - Gaussian grid number
C     HTYPE    - Gaussian grid type
C                = 'R' for reduced ("quasi-regular"),
C                = 'O' for reduced/octahedral,
C                = 'F' for full,
C                = 'U' for a user-defined gaussian grid
C     KPTS     - Array giving users definition of the number of points
C                at each gaussian latitude if HTYPE  = 'U'
C
C
C     Output parameters
C     ________________
C
C     PLAT     - Array of gaussian latitudes (both hemispheres)
C     KPTS     - Array giving number of points at each gaussian
C                latitude, both hemispheres
C                (unchanged from input if HTYPE = 'U')
C     KRET     - Return status code
C                0 = OK
C
C
C     Common block usage
C     __________________
C
C     None.
C
C     Method
C     ______
C
C     If HTYPE = 'U', the users definition of the grid is used.
C     Latitudes values are calculated corresponding to KNUM.
C
C     An array of predefined values is used.
C
C
C     Externals
C     _________
C
C     INTLOG   - Output log message
C     IGGLAT   - Computes Gaussian latitude lines for given truncation
C     QGOCT    - Reduced Gaussian grid, octahedral progression series
C
C
C     Reference
C     _________
C
C     None
C
C
C     Comments
C     ________
C
C     None
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers      ECMWF      Jan 1994
C
C
C     MODIFICATIONS
C     _____________
C
C     J.D.Chambers      ECMWF      Apr 1995
C     Use predefined definition file for N80 and N160 to cope with
C     changes in the 'standard' definition.
C
C     J.D.Chambers      ECMWF      Apr 1996
C     Call JGGLAT if regular grid but not N80 or N160
C
C     J.D.Chambers      ECMWF      Apr 1996
C     Fixup standard pathnames for VPPs.
C     Read array of values from file if possible (once only).
C
C     S.Siemen         ECMWF       April 2015
C     Ensure user defined numbers of latitudes (PL) are not overwritten
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER ( JPROUTINE = 30100 )
C
C     Subroutine arguments
C
      INTEGER KNUM, KPTS(*), KRET
      CHARACTER*1 HTYPE
      REAL PLAT(*)
C
C     Local variables
C
#include "parim.h"
! nifld.common only for NGSPEC
#include "nifld.common"
#include "grfixed.h"
#include "rgauss_032.h"
#include "rgauss_048.h"
#include "rgauss_064.h"
#include "rgauss_080.h"
#include "rgauss_080_19940113.h"
#include "rgauss_096.h"
#include "rgauss_128.h"
#include "rgauss_160.h"
#include "rgauss_160_19931222.h"
#include "rgauss_200.h"
#include "rgauss_256.h"
#include "rgauss_320.h"
#include "rgauss_400.h"
#include "rgauss_512.h"
#include "rgauss_640.h"
#include "rgauss_1024.h"
#include "rgauss_1280.h"
#include "rgauss_2000.h"
C
      INTEGER IERR
      INTEGER IMAGE
      INTEGER INUM
      INTEGER JJERR
      INTEGER JJPR
      INTEGER LOOP
C
C     Externals
C
      LOGICAL ISGGVALID
      EXTERNAL ISGGVALID
      INTEGER IGGLAT, QGOCT
      EXTERNAL IGGLAT, QGOCT
C
C     -----------------------------------------------------------------|
C*    Section 1.    Set up standard latitudes
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      IF( KNUM.GT.JPMAXNG .OR. .NOT.ISGGVALID(HTYPE,KNUM) ) THEN
        CALL INTLOG(JP_ERROR,
     X    'JGETGG: Grid not valid: '//HTYPE,KNUM)
        CALL INTLOG(JP_ERROR,
     X    'JGETGG: maybe too big for internal array dimension ',JPMAXNG)
        GOTO 910
      ENDIF
C
      INUM = KNUM*2
C
      IF( (HTYPE.EQ.'U').OR.(HTYPE.EQ.'u') ) GOTO 500
      IF( (HTYPE.EQ.'R').OR.(HTYPE.EQ.'r').OR.
     X    (HTYPE.EQ.'O').OR.(HTYPE.EQ.'o') ) GOTO 200
C
C     If a regular gaussian grid, calculate the gaussian latitudes
C
      CALL INTLOG(JP_DEBUG,
     X  'JGETGG: Calculate the gaussian latitudes for N = ',KNUM)
C
      JJPR  = 1
      JJERR = 1
      IERR  = IGGLAT(INUM, PLAT, JJPR, JJERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR, 'JGETGG: IGGLAT call failed',IERR)
        KRET = IERR
        GOTO 990
      ENDIF
C
C     Fill in the number of points at each latitude
C
      CALL INTLOG(JP_DEBUG,'JGETGG: Regular gaussian grid N ',KNUM)
C
      KPTS(1:INUM) = KNUM*4
C
      GOTO 930
C
C     -----------------------------------------------------------------|
C*    Section 2.    Read REDUCED gaussian definitions from array
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Fill in the gaussian latitudes
C
      JJPR  = 1
      JJERR = 1
      IERR  = IGGLAT(INUM, PLAT, JJPR, JJERR)
      IF( IERR.NE.0 ) THEN
        CALL INTLOG(JP_ERROR, 'JGETGG: IGGLAT call failed',IERR)
        KRET = IERR
        GOTO 990
      ENDIF
C
C     Fill the array for each row in both hemispheres.
C
      IF ((HTYPE.EQ.'O').OR.(HTYPE.EQ.'o')) THEN
          DO LOOP = 1, KNUM
              IMAGE       = INUM - LOOP +1
              KPTS(LOOP)  = QGOCT(LOOP)
              KPTS(IMAGE) = KPTS(LOOP)
          ENDDO
          GOTO 930
      ELSEIF( KNUM.EQ. 32 ) THEN
        DO LOOP = 1, KNUM
          IMAGE       = INUM - LOOP +1
          KPTS(LOOP)  = QG032(LOOP)
          KPTS(IMAGE) = KPTS(LOOP)
        ENDDO
        GOTO 930
C
      ELSEIF( KNUM.EQ. 48 ) THEN
        DO LOOP = 1, KNUM
          IMAGE       = INUM - LOOP +1
          KPTS(LOOP)  = QG048(LOOP)
          KPTS(IMAGE) = KPTS(LOOP)
        ENDDO
        GOTO 930

      ELSEIF( KNUM.EQ. 64 ) THEN
        DO LOOP = 1, KNUM
          IMAGE       = INUM - LOOP +1
          KPTS(LOOP)  = QG064(LOOP)
          KPTS(IMAGE) = KPTS(LOOP)
        ENDDO
        GOTO 930
C
      ELSE IF( KNUM.EQ. 80 ) THEN
C
        IF( NGSPEC.EQ.12 ) THEN
          DO LOOP = 1, KNUM
            IMAGE       = INUM - LOOP +1
            KPTS(LOOP)  = QG08012(LOOP)
            KPTS(IMAGE) = KPTS(LOOP)
          ENDDO
          GOTO 930
        ELSE
          DO LOOP = 1, KNUM
            IMAGE       = INUM - LOOP +1
            KPTS(LOOP)  = QG08018(LOOP)
            KPTS(IMAGE) = KPTS(LOOP)
          ENDDO
          GOTO 930
        ENDIF
C
      ELSE IF( KNUM.EQ.96 ) THEN
        DO LOOP = 1, KNUM
          IMAGE       = INUM - LOOP +1
          KPTS(LOOP)  = QG096(LOOP)
          KPTS(IMAGE) = KPTS(LOOP)
        ENDDO
        GOTO 930
C
      ELSE IF( KNUM.EQ.128 ) THEN
        DO LOOP = 1, KNUM
          IMAGE       = INUM - LOOP +1
          KPTS(LOOP)  = QG128(LOOP)
          KPTS(IMAGE) = KPTS(LOOP)
        ENDDO
        GOTO 930
C
      ELSE IF( KNUM.EQ.160 ) THEN
        IF( NGSPEC.EQ.12 ) THEN
          DO LOOP = 1, KNUM
            IMAGE       = INUM - LOOP +1
            KPTS(LOOP)  = QG16012(LOOP)
            KPTS(IMAGE) = KPTS(LOOP)
          ENDDO
          GOTO 930
        ELSE
          DO LOOP = 1, KNUM
            IMAGE       = INUM - LOOP +1
            KPTS(LOOP)  = QG16018(LOOP)
            KPTS(IMAGE) = KPTS(LOOP)
          ENDDO
          GOTO 930
        ENDIF
C
      ELSE IF( KNUM.EQ.200 ) THEN
        DO LOOP = 1, KNUM
          IMAGE       = INUM - LOOP +1
          KPTS(LOOP)  = QG200(LOOP)
          KPTS(IMAGE) = KPTS(LOOP)
        ENDDO
        GOTO 930
C
      ELSE IF( KNUM.EQ.256 ) THEN
        DO LOOP = 1, KNUM
          IMAGE       = INUM - LOOP +1
          KPTS(LOOP)  = QG256(LOOP)
          KPTS(IMAGE) = KPTS(LOOP)
        ENDDO
        GOTO 930
C
      ELSE IF( KNUM.EQ.320 ) THEN
        DO LOOP = 1, KNUM
          IMAGE       = INUM - LOOP +1
          KPTS(LOOP)  = QG320(LOOP)
          KPTS(IMAGE) = KPTS(LOOP)
        ENDDO
        GOTO 930
C
      ELSE IF( KNUM.EQ.400 ) THEN
        DO LOOP = 1, KNUM
          IMAGE       = INUM - LOOP +1
          KPTS(LOOP)  = QG400(LOOP)
          KPTS(IMAGE) = KPTS(LOOP)
        ENDDO
        GOTO 930
C
      ELSE IF( KNUM.EQ.512 ) THEN
        DO LOOP = 1, KNUM
          IMAGE       = INUM - LOOP +1
          KPTS(LOOP)  = QG512(LOOP)
          KPTS(IMAGE) = KPTS(LOOP)
        ENDDO
        GOTO 930
C
      ELSE IF( KNUM.EQ.640 ) THEN
        DO LOOP = 1, KNUM
          IMAGE       = INUM - LOOP +1
          KPTS(LOOP)  = QG640(LOOP)
          KPTS(IMAGE) = KPTS(LOOP)
        ENDDO
        GOTO 930
C
      ELSE IF( KNUM.EQ.1024 ) THEN
        DO LOOP = 1, KNUM
          IMAGE       = INUM - LOOP +1
          KPTS(LOOP)  = QG1024(LOOP)
          KPTS(IMAGE) = KPTS(LOOP)
        ENDDO
        GOTO 930
C
      ELSE IF( KNUM.EQ.1280 ) THEN
        DO LOOP = 1, KNUM
          IMAGE       = INUM - LOOP +1
          KPTS(LOOP)  = QG1280(LOOP)
          KPTS(IMAGE) = KPTS(LOOP)
        ENDDO
        GOTO 930
C
      ELSE
C       This is the default grid to use (N=1024)
        DO LOOP = 1, KNUM
          IMAGE       = INUM - LOOP +1
          KPTS(LOOP)  = QG1024(LOOP)
          KPTS(IMAGE) = KPTS(LOOP)
        ENDDO
        GOTO 930
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 5.    Use User's gaussian definitions from array
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
      CALL INTLOG(JP_DEBUG,
     X  'JGETGG: Read definitions from User array', JPQUIET)
C
C     Fill the array for each row in both hemispheres.
C
      DO LOOP = 1, INUM
        PLAT(LOOP) = ROGAUSS(LOOP)
C        KPTS(LOOP) = NOLPTS(LOOP)
      ENDDO
C
      GOTO 930
C
C     -----------------------------------------------------------------|
C*    Section 9. Return to calling routine. Format statements
C     -----------------------------------------------------------------|
C
 910  CONTINUE
      KRET = JPROUTINE + 2
      CALL INTLOG(JP_ERROR,
     X  'JGETGG: No suitable gaussian grid definition found.',JPQUIET)
      GOTO 990
C
 930  CONTINUE
      KRET = 0
C
 990  CONTINUE
      RETURN
      END

