C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION INTUVY( PVIN, PDIN, INLEN,
     X                         PUOUT, PVOUT, OUTLEN)
C
C---->
C**** INTUVY
C
C     Purpose
C     -------
C
C     Convert GRIB format input vorticity and divergence field to
C     unpacked U and V fields.
C
C
C     Interface
C     ---------
C
C     IRET = INTUVY( PVIN, PDIN, INLEN, PUOUT,PVOUT,OUTLEN)
C
C     Input
C     -----
C
C     KVIN - Input vorticity field  (spectral).
C     KDIN - Input divergence field (spectral).
C     INLEN  - Input field length (words).
C
C
C     Output
C     ------
C
C     KUOUT - Output U field
C     KVOUT - Output V field
C     OUTLEN  - Output field length (words).
C
C
C     Method
C     ------
C
C     Convert spectral vorticity/divergence to spectral U/V without
C     subsequent interpolation.
C
C     Note that a common block is used in intf.h to hold the U/V
C     fields before interpolation.
C
C     Externals
C     ---------
C
C     INTUVU - Now does all the work!
C     INTLOG - Log error message.
C
C
C     Author
C     ------
C
C     S.Curic     ECMWF     Dec 2006
C
C
C----<
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
C     Function arguments
C
      INTEGER  INLEN, OUTLEN
      REAL     PVIN(INLEN), PDIN(INLEN), PUOUT(*), PVOUT(*)
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "grfixed.h"
#include "intf.h"
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 26900 )
C
C     Local variables
C
      INTEGER IHOLD1, IHOLD2, IHOLD3
      LOGICAL LHOLD4
C
C     Externals
C
      INTEGER INTUVU
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
C     -----------------------------------------------------------------|
C*    Section 2.   Unpack the vorticity/divergence fields.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C     Ensure output representation says 'spectral, no rotation'
C
      IHOLD1    = NOREPR
      NOREPR    = JPSPHERE
      IHOLD2    = NOROTA(1)
      IHOLD3    = NOROTA(2)
      NOROTA(1) = -9000000
      NOROTA(2) = 0.0
      LHOLD4    = LNOROTA
      LNOROTA   = .FALSE.
C
      INTUVY =
     X  INTUVU(PVIN,PDIN,INLEN,PUOUT,PVOUT,OUTLEN)
C
C     Restore output representation
C
      NOREPR    = IHOLD1
      NOROTA(1) = IHOLD2
      NOROTA(2) = IHOLD3
      LNOROTA   = LHOLD4
C
C     -----------------------------------------------------------------|
C*    Section 9.   Closedown.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END
