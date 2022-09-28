C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION KRG2RGD(OLDFLD, NEWFLD, NUMME, NUMGG)
C
C---->
C**** KRG2RG
C
C     Purpose
C     -------
C
C     Interpolate a field based on one reduced gaussian grid to a
C     field based on a different reduced gaussian grid, using
C     intermediate interpolation
C
C
C     Interface
C     ---------
C
C     IRET = KRG2RGD(OLDFLD, NEWFLD, NUMME, NUMGG)
C
C     Input
C     -----
C
C     OLDFLD - Old field in GRIB format
C     NUMME  - Number of the intermediate gaussian field
C     NUMGG  - Number of the new gaussian field
C
C
C     Output
C     ------
C
C     NEWFLD - New field in GRIB format
C
C     Function returns:
C       - the size in bytes of the new GRIB product if all is well
C       - -1, otherwise.
C
C
C     Method
C     ------
C
C     Unpack input GRIBS.
C     Intermediate interpolation
C     Create new field and pack it into GRIB format.
C
C
C     Externals
C     ---------
C
C     GRIBEX  - Decode and encode GRIB products.
C     KINTRG  - Interpolate reduced gaussian field to reduced gaussian
C     JMALLOC - Dynamically allocate memory
C     INTLOG  - Log messages
C
C
C     Author
C     ------
C
C     S.Curic     ECMWF     October 2006
C
C----<
C
      IMPLICIT NONE
C
C     Function arguments
C
      INTEGER NUMME, NUMGG, OLDFLD(*), NEWFLD(*)
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
C
C     Parameters
C
      INTEGER JPACK, JUNPACK, JPINTB, JPREALB
      PARAMETER (JPACK   = 4000000)
      PARAMETER (JUNPACK = 6000000)
#ifdef INTEGER_8
      PARAMETER (JPINTB = 8)
#else
      PARAMETER (JPINTB = 4)
#endif
#ifdef REAL_8
      PARAMETER (JPREALB = 8)
#else
      PARAMETER (JPREALB = 8)
#endif
C
C     Local variables
C
      INTEGER IFSEC0(JPGRIB_ISEC0),IFSEC1(JPGRIB_ISEC1),
     X IFSEC2(JPGRIB_ISEC2),IFSEC3(JPGRIB_ISEC3),IFSEC4(JPGRIB_ISEC4)
      INTEGER NFSEC2(JPGRIB_ISEC2),NFSEC3(JPGRIB_ISEC3)
      INTEGER NEWGEO(JPGRIB_ISEC2)
      INTEGER ISIZE
      LOGICAL LFIRST
      CHARACTER*1 HTYPE
#ifndef _CRAYFTN
#if (!defined sgi)
#ifdef POINTER_64
      INTEGER*8 IZFSEC4, IPFSEC4, IMEDFLD
#else
      INTEGER   IZFSEC4, IPFSEC4, IMEDFLD
#endif
#endif
#endif
C
      REAL PLAT(JPMAXNG)
      REAL ZFSEC2(5000), ZFSEC3(2), ZFSEC4(1)
      REAL PFSEC3(2), PFSEC4(1), MEDFLD(1)
C
      POINTER( IZFSEC4, ZFSEC4 )
      POINTER( IPFSEC4, PFSEC4 )
      POINTER( IMEDFLD, MEDFLD )
C
      INTEGER IRET, ILENB, IPUNP, IWORD
C
C     Externals
C
#ifdef POINTER_64
      INTEGER*8 JMALLOC
#else
      INTEGER JMALLOC
#endif
      INTEGER KINTRG
      EXTERNAL KINTRG, JMALLOC
C
      DATA LFIRST/.TRUE./
      SAVE LFIRST, IZFSEC4, IPFSEC4, IMEDFLD
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      KRG2RGD = 0
C
      IF( (NUMGG*2).GT.JPMAXNG ) THEN
        CALL INTLOG(JP_ERROR,'KRG2RGD: Gaussian N too big = ',NUMGG)
        CALL INTLOG(JP_ERROR,'KRG2RGD: Maximum handled = ',(JPMAXNG/2))
        KRG2RGD = -1
        GOTO 900
      ENDIF

      IF( (NUMME*2).GT.JPMAXNG ) THEN
       CALL INTLOG(JP_ERROR,'KRG2RGD: Intermediate Gaussian N too big= '
     X   ,NUMME)
       CALL INTLOG(JP_ERROR,'KRG2RGD: Maximum handled = ',(JPMAXNG/2))
       KRG2RGD = -1
       GOTO 900
      ENDIF
C
C     First time, dynamically allocate memory for the field values
C
      IF( LFIRST ) THEN
        LFIRST = .FALSE.
        ISIZE = JUNPACK * JPREALB
        CALL INTLOG(JP_DEBUG,'KRG2RGD: Allocate memory = ',ISIZE)
C
        IZFSEC4 = JMALLOC(ISIZE)
#ifdef hpR64
        IZFSEC4 = IZFSEC4/(1024*1024*1024*4)
#endif
        IF( IZFSEC4.EQ.0 ) THEN
          CALL INTLOG(JP_ERROR,'KRG2RGD: JMALLOC fail IZFSEC4',JPQUIET)
          KRG2RGD = -1
          GOTO 900
        ENDIF
C
        IPFSEC4 = JMALLOC(ISIZE)
#ifdef hpR64
        IPFSEC4 = IPFSEC4/(1024*1024*1024*4)
#endif
        IF( IPFSEC4.EQ.0 ) THEN
          CALL INTLOG(JP_ERROR,'KRG2RGD: JMALLOC fail IPFSEC4',JPQUIET)
          KRG2RGD = -1
          GOTO 900
        ENDIF
C
        IMEDFLD = JMALLOC(ISIZE)
#ifdef hpR64
        IMEDFLD = IMEDFLD/(1024*1024*1024*4)
#endif
        IF( IMEDFLD.EQ.0 ) THEN
          CALL INTLOG(JP_ERROR,'KRG2RGD: JMALLOC fail IMEDFLD',JPQUIET)
          KRG2RGD = -1
          GOTO 900
        ENDIF
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2.   Unpack the input GRIB
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Old field
C
      IRET = 1
      IPUNP = JUNPACK
      ILENB = JPACK
      IFSEC3(1) = 0
      IFSEC3(2) = INT(RMISSGV)
      ZFSEC3(1) = 0.0
      ZFSEC3(2) = RMISSGV
      CALL GRIBEX (IFSEC0,IFSEC1,IFSEC2,ZFSEC2,IFSEC3,ZFSEC3,IFSEC4,
     X             ZFSEC4,IPUNP,OLDFLD,ILENB,IWORD,'D',IRET)
      IF( IRET.GT.0 ) THEN
        WRITE(*,*) 'KRG2RGD: Old field gribex return code = ',IRET
        KRG2RGD = -1
        RETURN
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3.   Create intermediate field
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      NFSEC2(1:JPGRIB_ISEC2) = IFSEC2(1:JPGRIB_ISEC2)
      NEWGEO(1:JPGRIB_ISEC2) = 0
      HTYPE = 'R'
      IF( LOCTAHEDRAL ) HTYPE = 'O'
      CALL JGETGG( NUMME, HTYPE, PLAT, NEWGEO(23), IRET)
      IF( IRET.NE.0 ) THEN
        WRITE(*,*) 'KRG2RGD: Problem calling JGETGG, status = ',IRET
        KRG2RGD = -1
        RETURN
      ENDIF
      NEWGEO(3)  = NUMME * 2
      NEWGEO(4)  = NINT(PLAT(1)*1000.0)
      NEWGEO(5)  = 0
      NEWGEO(7)  = NINT(PLAT(NUMME*2)*1000.0)
c     EMOS-199: adjusted for reduced_gg/octahedral
c     NEWGEO(8)  = NINT((360.0-(360.0/REAL(NUMME*4)))*1000.0)
      NEWGEO(8)  = NINT((360.0-(360.0/REAL(NEWGEO(23+NUMME))))*1000.0)
      NEWGEO(10) = NUMME
C
      IRET = KINTRG(IFSEC2, NEWGEO, ZFSEC4, MEDFLD)
      IF( IRET.LE.0 ) THEN
        WRITE(*,*) 'KRG2RGD: New field creation failed'
        KRG2RGD = -1
        RETURN
      ELSE
        CALL INTLOG(JP_DEBUG,
     X   'KRG2RGD: Intermediate gaussian field created - N',NUMME)
      ENDIF

C
C     -----------------------------------------------------------------|
C*    Section 4. Create new field from intermediate and put in GRIB
C     -----------------------------------------------------------------|
C

      HTYPE = 'R'
      IF( LOCTAHEDRAL ) HTYPE = 'O'
      CALL JGETGG( NUMGG, HTYPE, PLAT, NFSEC2(23), IRET)
      IF( IRET.NE.0 ) THEN
        WRITE(*,*) 'KRG2RGD: Problem calling JGETGG, status = ',IRET
        KRG2RGD = -1
        RETURN
      ENDIF
      NFSEC2(3)  = NUMGG * 2
      NFSEC2(4)  = NINT(PLAT(1)*1000.0)
      NFSEC2(7)  = NINT(PLAT(NUMGG*2)*1000.0)
c     EMOS-199: adjusted for reduced_gg/octahedral
c     NFSEC2(8)  = NINT((360.0-(360.0/REAL(NUMGG*4)))*1000.0)
      NFSEC2(8)  = NINT((360.0-(360.0/REAL(NFSEC2(23+NUMGG))))*1000.0)
      NFSEC2(10) = NUMGG
C
      IRET = KINTRG(NEWGEO, NFSEC2, MEDFLD, PFSEC4)
      IF( IRET.LE.0 ) THEN
        WRITE(*,*) 'KRG2RGD: New field creation failed'
        KRG2RGD = -1
        RETURN
      ELSE
        CALL INTLOG(JP_DEBUG,
     X   'KRG2RGD: New gaussian field created - N',NUMGG)
      ENDIF
      IFSEC4(1) = IRET
C
      IRET = 1
      IPUNP = JUNPACK
      ILENB = JPACK
C
C     If output 'accuracy' specified, use it for the new field
C
      IF( LNOACC ) THEN
        IFSEC4(2) = NOACC
      ENDIF
C
C     There may be 'missing' values in the new field if the old field
C     has 'missing' values.
C
      IF( IFSEC1(5).EQ.192 ) THEN
        IFSEC1(5) = 192
        NFSEC3(1) = 0
        NFSEC3(2) = INT(RMISSGV)
        PFSEC3(1) = 0.0
        PFSEC3(2) = RMISSGV
      ENDIF
C
      CALL GRIBEX (IFSEC0,IFSEC1,NFSEC2,ZFSEC2,NFSEC3,PFSEC3,IFSEC4,
     X             PFSEC4,IPUNP,NEWFLD,ILENB,IWORD,'C',IRET)
      IF( IRET.NE.0 ) THEN
        WRITE(*,*) 'KRG2RGD: New field gribex return code = ',IRET
        KRG2RGD = -1
        RETURN
      ENDIF
C
      KRG2RGD = IWORD * JPINTB
C
C     -----------------------------------------------------------------|
C*    Section 5.   Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END

