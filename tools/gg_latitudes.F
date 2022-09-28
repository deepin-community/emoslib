C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      PROGRAM gg_latitudes
C
C---->
C**** gg_latitudes
C
C     Purpose
C     -------
C
C     Command-line interface to JGETGG (interpolation).
C
C
C     Externals
C     ---------
C
C     JGETGG - Reads the definition of a gaussian grid.
C
C----<
C

C     Declarations
      IMPLICIT NONE
      INTEGER IARGC

      ! local variables
      INTEGER I, J
      CHARACTER*5 ARG, GRID

      ! global variables (all this to get to NGSPEC)
      INTEGER
     X  NILOCAL, NISTREM, NGSPEC ,
     X  NIFORM , NIREPR , NIGRID , NIAREA , NIGAUSS, NIRESO ,
     X  NIWE   , NINS   , NISCNM , NIPARAM, NITABLE, NIACC  ,
     X  NJDCDOT, NIPCNT , NILEVEL, NILEVT , NIDATE , NICOMP ,
     X  NISLSCP, NIPOGRS, NIPNGRS, NIMATR
      REAL RISTRET, RMISSGV
      DIMENSION NIGRID(2)
      DIMENSION NIAREA(4)
      COMMON /JDNIFLD/
     X  RISTRET, RMISSGV,
     X  NILOCAL, NISTREM, NGSPEC ,
     X  NIFORM , NIREPR , NIGAUSS, NIRESO ,
     X  NIWE   , NINS   , NISCNM , NIPARAM, NITABLE, NIACC  ,
     X  NJDCDOT, NIPCNT , NILEVEL, NILEVT , NIDATE , NICOMP ,
     X  NISLSCP, NIPOGRS, NIPNGRS, NIGRID , NIAREA , NIMATR
      SAVE /JDNIFLD/

      ! JGETGG arguments
      INTEGER KNUM, KRET
      CHARACTER*1 HTYPE
      INTEGER KPTS(4000) ! (in parim.h, JPLAT = 4000)
      REAL    PLAT(4000) ! ...


C     Check arguments count
      IF (IARGC() < 1) THEN
          PRINT *,
     X'Usage -- gg_latitudes [|-12] [[N|O|F]KNUM] ...'
          PRINT *, '  -12     -- force RGG specification (NGSPEC=12)'
          PRINT *, '  [N|O|F] -- Gaussian grid type(s)'
          PRINT *, '             N: "quasi-regular" (default)'
          PRINT *, '             O: octahedral'
          PRINT *, '             F: non-reduced'
          PRINT *, '  KNUM    -- Gaussian grid number(s), up to 2000'
          STOP
      END IF


C     Process arguments in order
      NGSPEC = 0
      DO J = 1, IARGC()
          CALL GETARG(J,ARG)
          IF (ARG=='-12') THEN

C             Force RGG specification
              NGSPEC = 12

          ELSE IF (ARG(1:1).NE.'N' .AND.
     X             ARG(1:1).NE.'O' .AND.
     X             ARG(1:1).NE.'F') THEN

C             Gaussian grid type has to be specified
              PRINT *,'Error: ARG="',ARG,'", ARG(1:1)!="[N|O|F]"'
              CALL EXIT(-1)

          ELSE

C             Set Gaussian grid type (HTYPE) & number (KNUM)
C             (read ([NOF])([0-9]{4}), HTYPE=$1, KNUM=$2)
              KNUM = 0
              READ (ARG,'(A1,I4)') HTYPE, KNUM
              IF (HTYPE=='N') HTYPE = 'R'
              IF (KNUM.GT.2000) THEN
                  PRINT *,'Error: ARG="',ARG,'" KNUM>2000'
                  CALL EXIT(-1)
              END IF

C             Call JGETGG
              KPTS = 0
              PLAT = 0.
              CALL JGETGG(KNUM,HTYPE,PLAT,KPTS,KRET)
              IF (KRET.NE.0) THEN
                  PRINT *,'Error: ARG="',ARG,'" JGETGG KRET=',KRET
                  CALL EXIT(KRET)
              END IF

C             Output
76            FORMAT(8X,A5,8X,A4,8X,A12)
77            FORMAT(8X,I5,8X,I4,8X,F12.5)
              IF (HTYPE.EQ.'R') WRITE(GRID,'(A,I0)') 'N',   KNUM
              IF (HTYPE.NE.'R') WRITE(GRID,'(A,I0)') HTYPE, KNUM
              WRITE(*,76) GRID,    'KPTS',  'PLAT        '
              WRITE(*,76) '-----', '----',  '------------'
              DO I = 1, KNUM*2
              WRITE(*,77) I,       KPTS(I), PLAT(I)
              END DO

          END IF
      END DO

      END PROGRAM

