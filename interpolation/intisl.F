C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INTISL(OLDLSM, OLDFLD, NEWLSM, NEWFLD)
C
C---->
C**** INTISL
C
C     Purpose
C     -------
C
C     Interpolate a field based on an old land-sea mask to a field
C     based on a different land-sea mask.
C
C
C     Interface
C     ---------
C
C     IRET = INTISL(OLDLSM, OLDFLD, NEWLSM, NEWFLD)
C
C     Input
C     -----
C
C     OLDLSM - Old land-sea mask in GRIB format
C     OLDFLD - Old field in GRIB format
C     NEWLSM - New land-sea mask in GRIB format
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
C     Create new field and pack it into GRIB format.
C
C
C     Externals
C     ---------
C
C     GRIBEX  - Decode and encode GRIB products.
C     NEWISL  - Interpolate old field to new using both land-sea masks.
C     JMALLOC - Dynamically allocate memory
C     INTLOG  - Log messages
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     August 2000
C
C----<
C
      IMPLICIT NONE
C
C     Function arguments
C
      INTEGER OLDLSM(*), OLDFLD(*), NEWLSM(*), NEWFLD(*)
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
C
C     Parameters
C
      INTEGER JPINTB, JPREALB
      PARAMETER (JPINTB = 4)
#ifdef REAL_8
      PARAMETER (JPREALB = 8)
#else
      PARAMETER (JPREALB = 8)
#endif
C
C     Local variables
C
      INTEGER IXSEC4(512)
      INTEGER ILSEC0(2),ILSEC1(1024),ILSEC2(1024),ILSEC3(2),ILSEC4(512)
      INTEGER IFSEC0(2),IFSEC1(1024),IFSEC2(1024),IFSEC3(2),IFSEC4(512)
      INTEGER IMSEC0(2),IMSEC1(1024),IMSEC2(1024),IMSEC3(2),IMSEC4(512)
      INTEGER                                     NFSEC3(2)
C     INTEGER NFSEC0(2),NFSEC1(1024),NFSEC2(1024),          NFSEC4(512)
      INTEGER KLSEC4, KFSEC4, KMSEC4, KPSEC4, NEWTYPE
#ifndef _CRAYFTN
#if (!defined sgi)
#ifdef POINTER_64
      INTEGER*8 IZLSEC4, IZFSEC4, IZMSEC4, IPFSEC4
#else
      INTEGER IZLSEC4, IZFSEC4, IZMSEC4, IPFSEC4
#endif
#endif
#endif
C
      REAL ZDUMMY
      REAL ZLSEC2(1500), ZLSEC3(2), ZLSEC4(1)
      REAL ZFSEC2(1500), ZFSEC3(2), ZFSEC4(1)
      REAL ZMSEC2(1500), ZMSEC3(2), ZMSEC4(1)
      REAL               PFSEC3(2), PFSEC4(1)  !PFSEC2(1500)
C
      POINTER( IZLSEC4, ZLSEC4 )
      POINTER( IZFSEC4, ZFSEC4 )
      POINTER( IZMSEC4, ZMSEC4 )
      POINTER( IPFSEC4, PFSEC4 )
C
      INTEGER IRET, ILENB, IPUNP, IWORD
C
C     Externals
C
      INTEGER NEWISL
#ifdef POINTER_64
      INTEGER*8 JMALLOC
#else
      INTEGER JMALLOC
#endif
      EXTERNAL NEWISL, JMALLOC
C
      DATA KLSEC4/0/, KFSEC4/0/, KMSEC4/0/, KPSEC4/0/
      SAVE KLSEC4, KFSEC4, KMSEC4, KPSEC4
      SAVE IZLSEC4, IZFSEC4, IZMSEC4, IPFSEC4
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      INTISL = 0
C
C     -----------------------------------------------------------------|
C*    Section 2.   Dynamically allocate memory for the old land-sea mask
C                  values and old field values.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Old land-sea mask
C
      IRET = 1
      IPUNP = 1
      ILENB = NIPOGRS
      CALL GRIBEX (ILSEC0,ILSEC1,ILSEC2,ZLSEC2,ILSEC3,ZLSEC3,IXSEC4,
     X             ZDUMMY,IPUNP,OLDLSM,ILENB,IWORD,'J',IRET)
      IF( IRET.NE.0 ) THEN
        WRITE(*,*) 'INTISL: Old lsm gribex return code = ',IRET
        INTISL = -1
        RETURN
      ENDIF
C
      IF( KLSEC4.LT.(IXSEC4(1)*JPREALB) ) THEN
        IF( KLSEC4.NE.0 ) THEN
          CALL JFREE(IZLSEC4)
          CALL JFREE(IZFSEC4)
        ENDIF
        KLSEC4 = IXSEC4(1)*JPREALB
        KFSEC4 = KLSEC4
        CALL INTLOG(JP_DEBUG,'INTISL: Allocate old memory = ',KLSEC4)
        IZLSEC4 = JMALLOC(KLSEC4)
        IZFSEC4 = JMALLOC(KFSEC4)
#ifdef hpR64
        IZLSEC4 = IZLSEC4/(1024*1024*1024*4)
        IZFSEC4 = IZFSEC4/(1024*1024*1024*4)
#endif
        IF( (IZLSEC4.EQ.0).OR.(IZFSEC4.EQ.0) ) THEN
          CALL INTLOG(JP_ERROR,'INTISL: JMALLOC fail oldies',JPQUIET)
          INTISL = -1
          GOTO 900
        ENDIF
      ENDIF
C
      IRET = 1
      IPUNP = IXSEC4(1)
      ILENB = NIPOGRS
      CALL GRIBEX (ILSEC0,ILSEC1,ILSEC2,ZLSEC2,ILSEC3,ZLSEC3,ILSEC4,
     X             ZLSEC4,IPUNP,OLDLSM,ILENB,IWORD,'D',IRET)
      IF( IRET.NE.0 ) THEN
        WRITE(*,*) 'INTISL: Old lsm gribex return code = ',IRET
        INTISL = -1
        RETURN
      ENDIF
C
C     Old field
C
      IRET = 1
      IPUNP = IXSEC4(1)
      ILENB = NIPOGRS
      IF( LIMISSV ) THEN
        IFSEC3(2) = NINT(RMISSGV)
        ZFSEC3(2) = RMISSGV
      ENDIF
      CALL GRIBEX (IFSEC0,IFSEC1,IFSEC2,ZFSEC2,IFSEC3,ZFSEC3,IFSEC4,
     X             ZFSEC4,IPUNP,OLDFLD,ILENB,IWORD,'D',IRET)
      IF( ((IRET.EQ.-2).OR.(IRET.EQ.-4)).AND.
     X    (LIMISSV.EQV.(.FALSE.)) ) THEN
        WRITE(*,*) 'INTISL: Old field has a bitmap'
        WRITE(*,*) 'INTISL: A missing value must be specified'
        INTISL = -1
        RETURN
      ELSE IF( IRET.GT.0 ) THEN
        WRITE(*,*) 'INTISL: Old field gribex return code = ',IRET
        INTISL = -1
        RETURN
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3.   Dynamically allocate memory for the master land-sea
C                  mask values and new field values.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
C     New land-sea mask
C
      IRET = 1
      IPUNP = 1
      ILENB = NIPNGRS
      CALL GRIBEX (IMSEC0,IMSEC1,IMSEC2,ZMSEC2,IMSEC3,ZMSEC3,IXSEC4,
     X             ZDUMMY,IPUNP,NEWLSM,ILENB,IWORD,'J',IRET)
      IF( IRET.NE.0 ) THEN
        WRITE(*,*) 'INTISL: Master lsm gribex return code = ',IRET
        INTISL = -1
        RETURN
      ENDIF
C
C     Dynamically allocate memory for the master land-sea mask values
C     and new field values
C
      IF( KMSEC4.LT.(IXSEC4(1)*JPREALB) ) THEN
        IF( KMSEC4.NE.0 ) THEN
          CALL JFREE(IZMSEC4)
          CALL JFREE(IPFSEC4)
        ENDIF
        KMSEC4 = IXSEC4(1)*JPREALB
        KPSEC4 = KMSEC4
        CALL INTLOG(JP_DEBUG,'INTISL: Allocate new memory = ',KMSEC4)
        IZMSEC4 = JMALLOC(KMSEC4)
        IPFSEC4 = JMALLOC(KPSEC4)
#ifdef hpR64
        IZMSEC4 = IZMSEC4/(1024*1024*1024*4)
        IPFSEC4 = IPFSEC4/(1024*1024*1024*4)
#endif
        IF( (IZMSEC4.EQ.0).OR.(IPFSEC4.EQ.0) ) THEN
          CALL INTLOG(JP_ERROR,'INTISL: JMALLOC fail newies',JPQUIET)
          INTISL = -1
          GOTO 900
        ENDIF
      ENDIF
C
      IRET = 1
      IPUNP = IXSEC4(1)
      ILENB = NIPNGRS
      CALL GRIBEX (IMSEC0,IMSEC1,IMSEC2,ZMSEC2,IMSEC3,ZMSEC3,IMSEC4,
     X             ZMSEC4,IPUNP,NEWLSM,ILENB,IWORD,'D',IRET)
      IF( IRET.NE.0 ) THEN
        WRITE(*,*) 'INTISL: Master lsm gribex return code = ',IRET
        INTISL = -1
        RETURN
      ENDIF
C
C     Requested field must match the new land-sea mask
C
      NEWTYPE = NOREPR
      IF( NOREPR.EQ.JPQUASI ) NEWTYPE = JPGAUSSIAN
      IF( (NEWTYPE.NE.IMSEC2(1)).OR.
     X    ((NOREPR.NE.JPQUASI).AND.(IMSEC2(17).NE.0)).OR.
     X    ((NOREPR.EQ.JPQUASI).AND.(IMSEC2(17).NE.1)) ) THEN
        CALL INTLOG(JP_ERROR,
     X    'INTISL: Land-sea mask not suitable',JPQUIET)
        INTISL = -1
        RETURN
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 4.   Create new field and put in GRIB format
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
      IRET = NEWISL(ILSEC2, IMSEC2, ZLSEC4, ZFSEC4, ZMSEC4, PFSEC4)
      IF( IRET.NE.0 ) THEN
        WRITE(*,*) 'INTISL: New field creation failed'
        INTISL = -1
        RETURN
      ENDIF
C
      IRET = 1
      IPUNP = IXSEC4(1)
      ILENB = NIPNGRS
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
      CALL GRIBEX (IFSEC0,IFSEC1,IMSEC2,ZMSEC2,NFSEC3,PFSEC3,IMSEC4,
     X             PFSEC4,IPUNP,NEWFLD,ILENB,IWORD,'C',IRET)
      IF( IRET.NE.0 ) THEN
        WRITE(*,*) 'INTISL: New field gribex return code = ',IRET
        INTISL = -1
        RETURN
      ENDIF
C
      INTISL = IWORD * JPINTB
C
C     -----------------------------------------------------------------|
C*    Section 9.   Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
