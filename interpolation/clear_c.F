C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

#define INT_MISSING -2147483647

      SUBROUTINE CLEAR_C( )
C
C---->
C**** CLEAR_C
C
C     Purpose
C     -------
C
C     Sets start values for parameters in common block.
C
C
C     Interface
C     ---------
C
C     CALL CLEAR_C()
C
C
C     Method
C     ------
C
C     Hard coded defaults.
C
C
C     Externals
C     ---------
C
C     None.
C
C
C     Author
C     ------
C
C     J.D.Chambers       ECMWF        August 1994.
C
C----<
C
C     IMPLICIT NONE
#include "parim.h"
#include "nifld.common"
#include "nifld.extracommon"
#include "nofld.common"
#include "grfixed.h"
#include "current.h"
C
C     ------------------------------------------------------------------
C*    Section 1.   Initialise
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
C     Set default processing flag settings.
C
      LCHANGE     = .TRUE.
      LSMCHNG     = .TRUE.
      LSM         = .TRUE.
      LPREC       = .FALSE.
      LWIND       = .FALSE.
      LOCTAHEDRAL = .FALSE.
C
C     Set default area indexes.
C
      NINS  = 0
      NIWE  = 0
      NO1NS = 0
      NO1WE = 0
      NONS  = 0
      NOWE  = 0
C
C     Defaults for output GRIB record ..
C     .. ECMWF local code table 128
C     .. level 500 hPa
C
      NOPARAM = 0
      NOTABLE = 128
      NOLEVT  = 100
      NOLEVEL = 500
      NOHFUNC = 'A'
C
C     Turn off all flags indicating that user has given a value.
C
      LSMPARSET = .FALSE.
      LSMSET    = .FALSE.
      LPRECSET  = .FALSE.
      LWINDSET  = .FALSE.
      LNIGAUS   = .FALSE.
      LNGSPEC   = .FALSE.
      LIGIVEL   = .FALSE.
      LIGIVEP   = .FALSE.
      LOGIVEL   = .FALSE.
      LOGIVEP   = .FALSE.
      LNOAREA   = .FALSE.
      LNORESO   = .FALSE.
      LNOGAUS   = .FALSE.
      LNOACC    = .FALSE.
      LNOREPR   = .FALSE.
      LNOTABL   = .FALSE.
      LNOPARA   = .FALSE.
      LNOLEVL   = .FALSE.
      LNOLEVT   = .FALSE.
      LNOGRID   = .FALSE.
      LNOFRAME  = .FALSE.
      LNOSTYLE  = .FALSE.
      LARESOL   = .FALSE.
      LINTOUT   = .FALSE.
      LIMISSV   = .FALSE.
      LDOUBLE   = .FALSE.
C
      RMISSGV   = REAL(INT_MISSING)
C
C     Clear input and output gaussian grid definitions
C
      NIGAUSS = 0
      RIGAUSS(1:JPLAT) = 0.0
      MILLEN (1:JPLAT) = 0

      NOGAUSS = 0
      ROGAUSS(1:JPLAT) = 0.0
      NOLPTS (1:JPLAT) = 0
C
C     Clear information about gaussian field definitions
C
      NGSPEC  = 0
      NIGAUSO = 0
      NOGAUSO = 0
      HIGAUST = 'X'
      HOGAUST = 'X'
C
C     Clear rotation angle values.
C
      NOROTA(1) = 0
      NOROTA(2) = 0
      LNOROTA   = .FALSE.
      LO12PT = .TRUE.
      LMETHOD = .FALSE.
      LNNLSM        = .FALSE.
      LNNLSM_CY41R1 = .FALSE.
C
C     Clear style and frame values
C
      NOFRAME = 0
      NOSTYLE = JPSMARS
C
C     Clear flags showing how to deal with legendre coefficients
C
      LFILEIO = .FALSE.
      LMEMORY = .FALSE.
      LSHARED = .FALSE.
      LON_FLY = .FALSE.
      LMAPPED = .FALSE.
      LIFIRST = .TRUE.
C
C     Clear fields for ISLSCP processing
C
      IF( LNIISCL ) THEN
        CALL JFREE(NIPOGRB)
        CALL JFREE(NIPNGRB)
      ENDIF
      LNIISCL = .FALSE.
C
C     ------------------------------------------------------------------
C*    Section 9.   Closedown.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END
