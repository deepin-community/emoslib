C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HSH2GG(NS,EW,KTRUNC,KNUM,HTYPE,KPTS,PLATS,KSIZE)
C
C---->
C**** HSH2GG
C
C     Purpose
C     -------
C
C     Finds a suitable Gaussian grid and/or spectral truncation for a
C     given spectral truncation and/or lat/long increments. There are
C     three modes (by order of precedence):
C     - providing NS,EW sets KTRUNC (using AURESOL) and KNUM/HTYPE
C     - providing KTRUNC sets KNUM/HTYPE
C     - providing KNUM sets KTRUNC
C     With KNUM/HTYPE set, the Gaussian grid definition is obtained with
C     JGETGG.
C     Parameters with value zero are assumed not provided.
C     Historically, this merges the functionality of:
C     - HSP2GG
C     - HSP2GG2
C     - HSP2GG3
C
C
C     Interface
C     ---------
C
C     IRET = HSH2GG(NS,EW,KTRUNC,KNUM,HTYPE,KPTS,PLATS,KSIZE)
C
C
C     Input parameters
C     ----------------
C     NS     - North-South increment
C     EW     - East-West increment
C
C
C     Input/output parameters
C     ----------------
C     KTRUNC - The spectral truncation
C     KNUM   - Gaussian grid number
C     HTYPE  - Gaussian grid type
C              = 'R' for reduced ("quasi-regular"),
C              = 'O' for reduced/octahedral,
C              = 'F' for full,
C              = 'U' for a user-defined Gaussian grid
C
C
C     Output parameters
C     -----------------
C     KPTS   - Array giving number of points along each line of latitude
C              in the reduced Gaussian grid (both hemispheres)
C     PLATS  - Array giving Gaussian latitudes (both hemispheres)
C     KSIZE  - The number of points in the reduced Gaussian grid
C
C     Returns 0 if all OK, otherwise there is an error.
C
C
C     Externals
C     ---------
C
C     JGETGG  - Reads the definition of a Gaussian grid
C     AURESOL - Returns the default truncation for a given
C               latitude--longitude grid resolution
C
C
C     Author
C     ------
C
C     J.D.Chambers      ECMWF      February 2001
C
C
C     Modifications
C     -------------
C
C     S.Curic     ECMWF      March 2005
C     Added checking for a automatic trancation T255, T399, T799, T2047
C     and corresponding Gaussian grid.
C
C     S.Curic     ECMWF      April 2008
C     Added checking for a automatic trancation T1279
C     and corresponding Gaussian grid.
C
C     S.Curic     ECMWF      Semptember 2009
C     Match T255 and T213 against N128 instead of N160
C     upon Alan Geer request
C
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables
C     -----------------------------------------------------------------|

      IMPLICIT NONE
#include "parim.h"
#include "jparams.h"

C     Function arguments
      INTEGER KTRUNC, KNUM, KPTS(*), KSIZE
      REAL NS, EW, PLATS(*)
      CHARACTER*1 HTYPE

C     Local variables
      INTEGER I, IRET
      REAL STEP

C     Externals
      INTEGER AURESOL
      EXTERNAL AURESOL

C     -----------------------------------------------------------------|
C     Section 1.  Initialise
C     -----------------------------------------------------------------|

      HSH2GG = 0

C     -----------------------------------------------------------------|
C     Section 2.  Set adequate truncation/grid number
C     -----------------------------------------------------------------|


C     if lat/long increments are provided, set automatic KTRUNC truncation
      IF( (KTRUNC.EQ.0) .AND. (KNUM.EQ.0) ) KTRUNC = AURESOL(NS,EW)


C     from a given truncation (KTRUNC) find a Gaussian number (KNUM)
      IF (KTRUNC.NE.0) THEN
        IF( KTRUNC.NE.  63 .AND. KTRUNC.NE.  64 .AND.
     .      KTRUNC.NE.  95 .AND. KTRUNC.NE.  96 .AND.
     .      KTRUNC.NE. 106 .AND. KTRUNC.NE. 107 .AND.
     .      KTRUNC.NE. 159 .AND. KTRUNC.NE. 160 .AND.
     .      KTRUNC.NE. 191 .AND. KTRUNC.NE. 192 .AND.
     .      KTRUNC.NE. 213 .AND. KTRUNC.NE. 214 .AND.
     .      KTRUNC.NE. 255 .AND. KTRUNC.NE. 256 .AND.
     .      KTRUNC.NE. 319 .AND. KTRUNC.NE. 320 .AND.
     .      KTRUNC.NE. 399 .AND. KTRUNC.NE. 400 .AND.
     .      KTRUNC.NE. 511 .AND. KTRUNC.NE. 512 .AND.
     .      KTRUNC.NE. 639 .AND. KTRUNC.NE. 640 .AND.
     .      KTRUNC.NE. 799 .AND. KTRUNC.NE. 800 .AND.
     .      KTRUNC.NE.1279 .AND. KTRUNC.NE.1280 .AND.
     .      KTRUNC.NE.2047 .AND. KTRUNC.NE.2048 ) THEN
          CALL INTLOG(JP_ERROR,'HSH2GG: truncation unsupported',KTRUNC)
          HSH2GG = 2
          GOTO 900
        ENDIF
        STEP = MIN(ABS(NS),ABS(EW))
        IF(     (KTRUNC.EQ.  63).OR.(KTRUNC.EQ.  64).OR.
     .          (KTRUNC.EQ.  95).OR.(KTRUNC.EQ.  96).OR.
     .          (STEP.GE.2.5) ) THEN                         ! 2.5  <= step        -> T63
          KNUM =   48
        ELSEIF( (KTRUNC.EQ. 106).OR.(KTRUNC.EQ. 107).OR.
     .          (KTRUNC.EQ. 159).OR.(KTRUNC.EQ. 160).OR.
     .          (STEP.GE.1.5) ) THEN                         ! 1.5  <= step < 2.5  -> T106
          KNUM =   80
        ELSEIF( (KTRUNC.EQ. 191).OR.(KTRUNC.EQ. 192).OR.
     .          (STEP.GE.0.4 .AND. .FALSE.) ) THEN           ! 1.5  <= step < 2.5  -> T191 (commented)
          KNUM =   96
        ELSEIF( (KTRUNC.EQ. 213).OR.(KTRUNC.EQ. 214).OR.
     .          (KTRUNC.EQ. 255).OR.(KTRUNC.EQ. 256).OR.     ! Enfo 2004
     .          (STEP.GE.0.6) ) THEN                         ! 0.6  <= step < 1.5  -> T213
          KNUM =  128
        ELSEIF( (KTRUNC.EQ. 319).OR.(KTRUNC.EQ. 320).OR.
     .          (STEP.GE.0.4) ) THEN                         ! 0.4  <= step < 0.6  -> T319
          KNUM =  160
        ELSEIF( (KTRUNC.EQ. 399).OR.(KTRUNC.EQ. 400) ) THEN  ! Enfo 2005
          KNUM =  200
        ELSEIF( (KTRUNC.EQ. 511).OR.(KTRUNC.EQ. 512).OR.     ! Oper 2004
     .          (STEP.GE.0.3) ) THEN                         ! 0.3  <= step < 0.4  -> T511
          KNUM =  256
        ELSEIF( (KTRUNC.EQ. 639).OR.(KTRUNC.EQ. 640) ) THEN
          KNUM =  320
        ELSEIF( (KTRUNC.EQ. 799).OR.(KTRUNC.EQ. 800).OR.     ! Oper 2005
     .          (STEP.GE.0.15) ) THEN                        ! 0.15 <= step < 0.3  -> T799
          KNUM =  400
        ELSEIF( (KTRUNC.EQ.1279).OR.(KTRUNC.EQ.1280).OR.     ! Jan Haseler
     .          (STEP.GE.0.09) ) THEN                        ! 0.09 <= step < 0.15 -> T1279
          KNUM =  640
        ELSEIF( (KTRUNC.EQ.2047).OR.(KTRUNC.EQ.2048).OR.     ! Mariano
     .          (STEP.LT.0.09) ) THEN                        ! 0.09 <= step < 0.15 -> T1279
          KNUM = 1024
        ENDIF
        HTYPE = 'R'


C     from a given Gaussian number (KNUM) find a truncation (KTRUNC)
      ELSEIF (KNUM.NE.0) THEN
        IF( KNUM.NE.  48 .AND. KNUM.NE.  80 .AND. KNUM.NE.  96 .AND.
     .      KNUM.NE. 128 .AND. KNUM.NE. 160 .AND. KNUM.NE. 200 .AND.
     .      KNUM.NE. 256 .AND. KNUM.NE. 320 .AND. KNUM.NE. 400 .AND.
     .      KNUM.NE. 640 .AND. KNUM.NE.1024 .AND. KNUM.NE.1280 ) THEN
        CALL INTLOG(JP_ERROR,'HSH2GG: Gaussian number unsupported',KNUM)
          HSH2GG = 2
          GOTO 900
        ENDIF
        IF(     KNUM.EQ.  48 ) THEN
C         KTRUNC =   63
          KTRUNC =   95
        ELSEIF( KNUM.EQ.  80 ) THEN
          KTRUNC =  159
        ELSEIF( KNUM.EQ.  96 ) THEN
          KTRUNC =  191              ! Elias 2014
        ELSEIF( KNUM.EQ. 128 ) THEN
C         KTRUNC =  213
          KTRUNC =  255              ! Enfo 2004
        ELSEIF( KNUM.EQ. 160 ) THEN
          KTRUNC =  319
        ELSEIF( KNUM.EQ. 200 ) THEN
          KTRUNC =  399              ! Enfo 2005
        ELSEIF( KNUM.EQ. 256 ) THEN
          KTRUNC =  511              ! Oper 2004
        ELSEIF( KNUM.EQ. 320 ) THEN
          KTRUNC =  639
        ELSEIF( KNUM.EQ. 400 ) THEN
          KTRUNC =  799              ! Oper 2005
        ELSEIF( KNUM.EQ. 640 ) THEN
          KTRUNC = 1279              ! Jan Haseler
        ELSEIF( KNUM.EQ.1024 ) THEN
          KTRUNC = 2047              ! Mariano
        ELSEIF( KNUM.EQ.1280 ) THEN
          KTRUNC = 1279              ! (cubic octahedral)
        ENDIF

      ENDIF

C     -----------------------------------------------------------------|
C     Section 3.  Get the reduced Gaussian grid and count points
C     -----------------------------------------------------------------|

      CALL INTLOG(JP_DEBUG,'HSH2GG: spectral truncation:',KTRUNC)
      CALL INTLOG(JP_DEBUG,'HSH2GG: Gaussian grid: '//HTYPE,KNUM)
      IF( HTYPE.NE.'R' .AND. HTYPE.NE.'O' .AND.
     X    HTYPE.NE.'F' .AND. HTYPE.NE.'U' ) THEN
        CALL INTLOG(JP_ERROR,
     X    'HSH2GG: Gaussian type unsupported: '//HTYPE,JPQUIET)
        HSH2GG = 3
        GOTO 900
      ENDIF
      CALL JGETGG(KNUM,HTYPE,PLATS,KPTS,IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR, 'HSH2GG: JGETGG failed',JPQUIET)
        HSH2GG = 3
      ELSE
        KSIZE = 0
        DO I = 1, KNUM*2
          KSIZE = KSIZE + KPTS(I)
        ENDDO
      ENDIF

C     -----------------------------------------------------------------|
C     Section 9.  Return
C     -----------------------------------------------------------------|

  900 CONTINUE
      RETURN

      END
