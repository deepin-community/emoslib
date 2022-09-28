C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      CHARACTER*1 FUNCTION GGHTYPE(REPR,KNUM,KPTS)
C
C---->
C**** *GGHTYPE*
C
C     PURPOSE
C     _______
C
C     Return Gaussian grid type, from field representation, grid number
C     and array with the number of points at each latitude
C
C
C     Input parameters
C     ________________
C
C     KNUM - Gaussian grid number
C     REPR - Field representation, including:
C              Spherical harmonic
C              Regular Gaussian grid
C              Reduced Gaussian grid
C              Latitude/longitude grid
C     KPTS - Array giving number of points at each gaussian 
C            latitude, both hemispheres (pl array)
C
C
C     Output parameters
C     -----------------
C
C     Returns non-blank if format detected successfully, one of:
C       'R' for reduced ("quasi-regular"),
C       'O' for reduced/octahedral,
C       'F' for full,
C
C----<
      IMPLICIT NONE
#include "parim.h"


C     Arguments
      INTEGER REPR, KNUM, KPTS(*)


C     Local variables
      CHARACTER*1 F, R
      LOGICAL ISOCTAHEDRAL, ISGGVALID
      EXTERNAL ISOCTAHEDRAL, ISGGVALID
      F = 'F'
      R = 'R'


C     Check for validity of combinations (F|[RO]###)
      GGHTYPE = ' '
      IF( (REPR.EQ.JPGAUSSIAN.OR.REPR.EQ.JPFGGROT)
     .  .AND. ISGGVALID(F,KNUM) ) THEN
        GGHTYPE = F
      ELSEIF( (REPR.EQ.JPQUASI.OR.REPR.EQ.JPQGGROT.OR.REPR.EQ.JPSTRGG)
     .  .AND. ISGGVALID(R,KNUM) ) THEN
        GGHTYPE = R
        IF( ISOCTAHEDRAL(KNUM,KPTS) ) GGHTYPE='O'
      ENDIF

      ENDFUNCTION

