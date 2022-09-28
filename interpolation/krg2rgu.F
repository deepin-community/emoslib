C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION KRG2RGU(OLDFLD, NEWFLD, NUMGGO, NUMGGN)
C
C---->
C**** KRG2RGU
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
C     IRET = KRG2RGU(OLDFLD, NEWFLD, NUMGGO, NUMGGN)
C
C     Input
C     -----
C
C     OLDFLD - Old field as array of values
C     NUMGGO - Number of the old gaussian field
C     NUMGGN - Number of the new gaussian field
C
C
C     Output
C     ------
C
C     NEWFLD - New field as array of values
C
C     Function returns:
C       - the size in bytes of the new GRIB product if all is well
C       - -1, otherwise.
C
C
C     Method
C     ------
C
C     Create new field and leave it unpacked.
C
C
C     Externals
C     ---------
C
C     KINTRG  - Interpolate reduced gaussian field to reduced gaussian
C     INTLOG  - Log messages
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     February 2002
C
C     Modifications
C     -------------
C
C     Use grid definition from input GRIB header to
C     cater for octahedral grid
C     S.Siemen         ECMWF     April 2015
C
C----<
C
      IMPLICIT NONE
C
C     Function arguments
C
      INTEGER NUMGGO, NUMGGN
      REAL OLDFLD(*), NEWFLD(*)
C
#include "parim.h"
#include "nofld.common"
#include "grfixed.h"
C
C     Parameters
C
      INTEGER JPINTB
      INTEGER JSEC2
      PARAMETER (JSEC2 = 5000)
#ifdef INTEGER_8
      PARAMETER (JPINTB = 8)
#else
      PARAMETER (JPINTB = 4)
#endif
C
C     Local variables
C
      INTEGER OLDGEO(JSEC2), NEWGEO(JSEC2)
      INTEGER LOOP
      CHARACTER*1 HTYPE
      REAL PLAT(JPMAXNG)
C
      INTEGER IRET
C
C     Externals
C
      INTEGER KINTRG
      EXTERNAL KINTRG
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      KRG2RGU = 0
C
      IF( (NUMGGN*2).GT.JPMAXNG ) THEN
        CALL INTLOG(JP_ERROR,'KRG2RGU: Gaussian N too big = ',NUMGGN)
        CALL INTLOG(JP_ERROR,'KRG2RGU: Maximum handled = ',(JPMAXNG/2))
        KRG2RGU = -1
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2.   Create input and new field.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      OLDGEO(1:JPMAXNG) = 0
      DO LOOP = 1, 2*NUMGGO
        OLDGEO(22+LOOP) = MILLEN(LOOP)
      ENDDO
      HTYPE = 'U'
      CALL JGETGG(NUMGGO,HTYPE,PLAT,OLDGEO(23),IRET)
      IF( IRET.NE.0 ) THEN
        WRITE(*,*) 'KRG2RGU: Problem calling JGETGG, status = ',IRET
        KRG2RGU = -1
      RETURN
      ENDIF

C
      OLDGEO(3)  = NUMGGO * 2
      OLDGEO(4)  = NINT(PLAT(1)*1000.0)
      OLDGEO(5)  = 0
      OLDGEO(7)  = NINT(PLAT(NUMGGO*2)*1000.0)
c     EMOS-199: adjusted for reduced_gg/octahedral
c     OLDGEO(8)  = NINT((360.0-(360.0/REAL(NUMGGO*4)))*1000.0)
      OLDGEO(8)  = NINT((360.0-(360.0/REAL(OLDGEO(23+NUMGGO))))*1000.0)
      OLDGEO(10) = NUMGGO
C
      DO LOOP = 1, JPMAXNG
        NEWGEO(LOOP) = 0
      ENDDO
      HTYPE = 'R'
      IF( LOCTAHEDRAL ) HTYPE = 'O'
      CALL JGETGG(NUMGGN,HTYPE,PLAT,NEWGEO(23),IRET)
      IF( IRET.NE.0 ) THEN
        WRITE(*,*) 'KRG2RGU: Problem calling JGETGG, status = ',IRET
        KRG2RGU = -1
        RETURN
      ENDIF
      NEWGEO(3)  = NUMGGN * 2
      NEWGEO(4)  = NINT(PLAT(1)*1000.0)
      NEWGEO(5)  = 0
      NEWGEO(7)  = NINT(PLAT(NUMGGN*2)*1000.0)
c     EMOS-199: adjusted for reduced_gg/octahedral
c     NEWGEO(8)  = NINT((360.0-(360.0/REAL(NUMGGN*4)))*1000.0)
      NEWGEO(8)  = NINT((360.0-(360.0/REAL(NEWGEO(23+NUMGGN))))*1000.0)
      NEWGEO(10) = NUMGGN
C
      IRET = KINTRG(OLDGEO, NEWGEO, OLDFLD, NEWFLD)
C
      IF( IRET.LE.0 ) THEN
        WRITE(*,*) 'KRG2RGU: New field creation failed'
        KRG2RGU = -1
        RETURN
      ENDIF
c sinisa for merging with grib_api
      NONS = 2*NOGAUSS
      NOAAPI(1) = NOAREA(1)
      NOAAPI(2) = NOAREA(2)
      NOAAPI(3) = NOAREA(3)
      NOAAPI(4) = NOAREA(4)
C
cs      KRG2RGU = IRET * JPINTB
cs this is the outlen
      KRG2RGU = IRET
C
C     -----------------------------------------------------------------|
C*    Section 9.   Return
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
