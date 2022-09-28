C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JALLWN( PSHUP, KTRUNC, PNORTH, PSOUTH, PWEST, PEAST,
     X                    PLATINC, PLONINC, POUTF, KRET)
C
C---->
C**** JALLWN
C
C     PURPOSE
C     _______
C
C     This routine converts spectral input fields to standard
C     lat/long grid fields.
C
C     INTERFACE
C     _________
C
C     CALL JALLWN( PSHUP, KTRUNC, PNORTH, PSOUTH, PWEST, PEAST,
C    X                    PLATINC, PLONINC, POUTF, KRET)
C
C     Input parameters
C     ________________
C
C     PSHUP    - Spherical harmonics field, unpacked
C     KTRUNC   - Truncation number of spherical harmonics field
C     PNORTH   - Northernmost latitude for output field (degrees)
C     PSOUTH   - Southernmost latitude for output field (degrees)
C     PWEST    - Westernmost longitude for output field (degrees)
C     PEAST    - Easternmost longitude for output field (degrees)
C     PLATINC  - Grid interval between latitudes in degrees
C     PLONINC  - Grid interval between longitudes in degrees
C     INTLOG   - Output log message
C     INTLOGR  - Output log message (with real value)
C
C     Output parameters
C     ________________
C
C     POUTF    - Output grid point field
C     KRET     - Return status code
C                0 = OK
C
C     Common block usage
C     __________________
C
C     JDCNDBG
C
C     Method
C     ______
C
C     Generates the latitudes in bands, 32 at a time plus some
C     remainder.
C
C     Externals
C     _________
C
C     JALLGP  - Cnverts spectral fields to lat/long grid fields.
C     JSLLGP  - Converts stretched spectral fields to lat/long grids
C
C
C     Reference
C     _________
C
C     None.
C
C
C     Comments
C     ________
C
C     Calls JALLGP or JSLLGP to do all the work!
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers      *ECMWF*      Nov 1993
C
C     MODIFICATIONS
C     _____________
C
C     J.D.Chambers      *ECMWF*      Oct 1996
C     Replaced all code by a call to JALLGP.
C
C----<
C     _______________________________________________________
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
      IMPLICIT NONE
#include "jparams.h"
#include "parim.h"
#include "nifld.common"
C
C     Subroutine arguments
C
      COMPLEX   PSHUP
      DIMENSION PSHUP(*)
      INTEGER   KTRUNC
      REAL      PNORTH, PSOUTH, PWEST, PEAST, PLATINC, PLONINC
      REAL      POUTF
      DIMENSION POUTF(*)
      INTEGER   KRET
C
C     Parameters
      INTEGER JPROUTINE
      PARAMETER ( JPROUTINE = 30000 )
C
C     Local variables
C
C     _______________________________________________________
C
C*    Section 1.    Initialization.
C     _______________________________________________________
C
  100 CONTINUE
      CALL JDEBUG( )
C
      IF( NDBG.GT.0 )
     X  CALL INTLOG(JP_DEBUG,'JALLWN: Calling JALLGP', JPQUIET)
C
      IF( RISTRET.NE.0 ) THEN
        CALL JSLLGP( PSHUP, KTRUNC, PNORTH, PSOUTH, PWEST, PEAST,
     X               PLATINC, PLONINC, POUTF, KRET)
      ELSE
        CALL JALLGP( PSHUP, KTRUNC, PNORTH, PSOUTH, PWEST, PEAST,
     X               PLATINC, PLONINC, POUTF, KRET)
      ENDIF
C
      IF( NDBG.GT.0 )
     X  CALL INTLOG(JP_DEBUG,'JALLWN: Returned from JALLGP', JPQUIET)
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
 900  CONTINUE
C
      RETURN
      END
