C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IBASINI(KFORCE)
C
C---->
C**** IBASINI
C  
C     Purpose
C     -------
C
C     Ensures basic initialisation of common blocks is done
C  
C     Interface
C     ---------
C
C     IRET = IBASINI(KFORCE)
C
C
C     Input parameters
C     ----------------
C
C     KFORCE  = 1, to force initialisation of common blocks.
C             = 0, to check if initialisation of common blocks is done
C                  already (and do it if not already done).
C  
C  
C     Method
C     ------
C
C     NJDCDOT in nifld.common is checked/set.
C
C     Looks for environment variable INTERP_GEN_COEFFS which gives
C     the name of a file containing the cutoff spectral truncation
C     above which interpolation coefficients are generated 'on the
C     fly'. Variable NICOMP is set with this value in nifld.common.
C     The file contains entries for each computer architecture in
C     format:
C
C       col 1
C         |
C         v
C         FUJITSU 319
C         sgimips 213
C         hppa 213
C         DEFAULT 106
C
C     If no matching $ARCH value, the DEFAULT value is used.
C     If no matching $ARCH and no DEFAULT value, a hard-code value is used.
C
C     Looks for environment variable USE_HIRLAM_12POINT to determine
C     whether or not the Hiralm 12-point horizontal interpolation is
C     to be used for rotations.
C
C
C     Externals
C     ---------
C
C     CLEAR_C - Clear common block variables
C     GETENV  - Get value of an environment variable
C     JINDEX  - Returns length of character string
C  
C  
C     Author
C     ------
C
C     J.D.Chambers       ECMWF        August 1994.
C
C----<
C
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "intf.h"
C
C     Function arguments
C
      INTEGER KFORCE
C
C     Local variables
C
      CHARACTER*120 LINE
      CHARACTER*20 ARCH, USEHIR
      CHARACTER*256 CONFIG
      INTEGER IMAGIC, IRET, ICONFIG, IBLANK, LOOP
      DATA ICONFIG/69/
      DATA IMAGIC/1952999238/
C
C     Externals
C
      INTEGER JINDEX
C
C     -----------------------------------------------------------------|
C     Section 1. Force initialisation if requested.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      IF ( KFORCE .EQ. 1 ) NJDCDOT = 0
C
C     See if basic initialisation has already been done or not
C
      IF ( NJDCDOT .NE. IMAGIC ) THEN
C
C       Clear common block variables
C
        CALL CLEAR_C()
C
        IRET = 1
        CALL IAINIT(0,IRET)
C
C       Set interpolation handling default values
C       (Replaces old call to rddefs)
C
        NILOCAL = 0
        NISTREM = 0
        NIFORM = 1
        NOFORM = 1
        NITABLE = 128
        NOTABLE = 128
        NIPARAM = 0
        DO LOOP = 1,4
          NIAREA(LOOP) = 0
          NOAREA(LOOP) = 0
        ENDDO
        NISCNM = 0
        NOSCNM = 0
C
C       Set default value for truncation above which interpolation
C       coefficients are to be computed dynamically
C
        NICOMP = 319
C
C       Now see if this default value has been modified in a
C       configuration file
C
        CALL GETENV('INTERP_GEN_COEFFS', CONFIG)
        IBLANK = JINDEX(CONFIG)
        IF( IBLANK.GE.1 ) THEN
C
C         Open the configuration file
C
          OPEN( ICONFIG, FILE=CONFIG, STATUS='OLD', ERR=200)
          CALL GETENV('ARCH', ARCH)
          IBLANK = JINDEX(ARCH)
          IF( IBLANK.LT.1 ) ARCH = 'DEFAULT'
          IBLANK = JINDEX(ARCH)
C
C         Look for matching 'arch'
C
  110     CONTINUE
          READ( ICONFIG, '(A)', END= 200) LINE
          IF( ARCH(1:IBLANK).EQ.LINE(1:IBLANK) ) THEN
            READ(LINE(IBLANK+1:),'(1X,I3)') NICOMP
            GOTO 200
          ENDIF
C
C         Pickup default (will be used if no matching 'arch')
C
          IF( (LINE(1:7).EQ.'DEFAULT').OR.
     X        (LINE(1:7).EQ.'default') )
     X      READ(LINE(IBLANK+1:),'(1X,I3)') NICOMP
C
          GOTO 110
        ENDIF
C
C     -----------------------------------------------------------------|
C     Section 2. See if Hirlam 12-point horizontal interpolation to be
C                use for rotations (default = 'yes').
C     -----------------------------------------------------------------|
C
  200   CONTINUE
C
        LUSEHIR = .TRUE.
        CALL GETENV('USE_HIRLAM_12POINT', USEHIR)
        IF( (USEHIR(1:3).EQ.'OFF').OR.(USEHIR(1:2).EQ.'NO') )
     X    LUSEHIR = .FALSE.
C
C     -----------------------------------------------------------------|
C     Section 9. Return
C     -----------------------------------------------------------------|
C
  900   CONTINUE
C
C       Set 'magic number' to show basic initialisation has been done
C
        NJDCDOT = IMAGIC
C
      ENDIF
C
      IBASINI = 0
C
      RETURN
      END
