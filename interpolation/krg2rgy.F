C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION KRG2RGY(OLDFLD, NEWFLD, NUMGG)
C
C---->
C**** KRG2RGY
C
C     Purpose
C     -------
C
C     Interpolate a field based on one reduced gaussian grid to a
C     field based on a different reduced gaussian grid.
C
C
C     Interface
C     ---------
C
C     IRET = KRG2RGY(OLDFLD, NEWFLD, NUMGG)
C
C     Input
C     -----
C
C     OLDFLD - Old field in GRIB format
C     NUMGG  - Number of the new gaussian field
C
C
C     Output
C     ------
C
C     NEWFLD - New field
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
C     Create new field
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
C     S.Curic     ECMWF     November 2006
C
C----<
C
      IMPLICIT NONE
C
C     Function arguments
C
      INTEGER NUMGG, OLDFLD(*), NEWFLD(*)
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "grfixed.h"
C
C     Parameters
C
      INTEGER JPACK, JUNPACK, JPREALB
      PARAMETER (JPACK   = 4000000)
      PARAMETER (JUNPACK = 6000000)
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
      INTEGER NFSEC2(JPGRIB_ISEC2)
      INTEGER ISIZE, LOOP
      LOGICAL LFIRST
      CHARACTER*1 HTYPE
#ifndef _CRAYFTN
#if (!defined sgi)
#ifdef POINTER_64
      INTEGER*8 IZFSEC4
#else
      INTEGER   IZFSEC4
#endif
#endif
#endif
C
      REAL PLAT(JPMAXNG)
      REAL ZFSEC2(5000), ZFSEC3(2), ZFSEC4(1)
C
      POINTER( IZFSEC4, ZFSEC4 )
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
      SAVE LFIRST, IZFSEC4
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      KRG2RGY = 0
C
      IF( (NUMGG*2).GT.JPMAXNG ) THEN
        CALL INTLOG(JP_ERROR,'KRG2RGY: Gaussian N too big = ',NUMGG)
        CALL INTLOG(JP_ERROR,'KRG2RGY: Maximum handled = ',(JPMAXNG/2))
        KRG2RGY = -1
        GOTO 900
      ENDIF
C
C     First time, dynamically allocate memory for the field values
C
      IF( LFIRST ) THEN
        LFIRST = .FALSE.
        ISIZE = JUNPACK * JPREALB
        CALL INTLOG(JP_DEBUG,'KRG2RGY: Allocate memory = ',ISIZE)
C
        IZFSEC4 = JMALLOC(ISIZE)
#ifdef hpR64
        IZFSEC4 = IZFSEC4/(1024*1024*1024*4)
#endif
        IF( IZFSEC4.EQ.0 ) THEN
          CALL INTLOG(JP_ERROR,'KRG2RGY: JMALLOC fail IZFSEC4',JPQUIET)
          KRG2RGY = -1
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
        WRITE(*,*) 'KRG2RGY: Old field gribex return code = ',IRET
        KRG2RGY = -1
        RETURN
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3.   Create new field
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      DO LOOP = 1, JPMAXNG
        NFSEC2(LOOP) = IFSEC2(LOOP)
      ENDDO
      HTYPE = 'R'
      IF( LOCTAHEDRAL ) HTYPE = 'O'
      CALL JGETGG( NUMGG, HTYPE, PLAT, NFSEC2(23), IRET)
      IF( IRET.NE.0 ) THEN
        WRITE(*,*) 'KRG2RGY: Problem calling JGETGG, status = ',IRET
        KRG2RGY = -1
        RETURN
      ENDIF
      NFSEC2(3)  = NUMGG * 2
      NFSEC2(4)  = NINT(PLAT(1)*1000.0)
      NFSEC2(7)  = NINT(PLAT(NUMGG*2)*1000.0)
c     EMOS-199: NFSEC2(8) adjusted for reduced_gg/octahedral
c     NFSEC2(8)  = NINT((360.0-(360.0/REAL(NUMGG*4)))*1000.0)
      NFSEC2(8)  = NINT((360.0-(360.0/REAL(NFSEC2(23+NUMGG))))*1000.0)
      NFSEC2(10) = NUMGG
C
      IRET = KINTRG(IFSEC2, NFSEC2, ZFSEC4, NEWFLD)
      IF( IRET.LE.0 ) THEN
        WRITE(*,*) 'KRG2RGY: New field creation failed'
        KRG2RGY = -1
        RETURN
      ENDIF
      IF( LDOUBLE ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'KRG2RGY: Set NIAREA - double interpolation',JPQUIET)
c       NIAREA(1)  = NFSEC2(4)*100
c       NIAREA(2)  = NFSEC2(5)*100
c       NIAREA(3)  = NFSEC2(7)*100
c       NIAREA(4)  = NFSEC2(8)*100
        NIAREA(1)  = 0
        NIAREA(2)  = 0
        NIAREA(3)  = 0
        NIAREA(4)  = 0
        NIGAUSS    = NUMGG
        RIGAUSS(1:NINS) = 0
        MILLEN (1:NINS) = 0
        NINS       = NUMGG*2
      ENDIF

C
      KRG2RGY = IRET
C     -----------------------------------------------------------------|
C*    Section 9.   Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
