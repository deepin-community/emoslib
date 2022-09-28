C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      LOGICAL FUNCTION ISSAME()
C
C---->
C**** ISSAME
C
C     Purpose
C     -------
C
C     Check if output field will be the same as the input field.
C
C
C     Interface
C     ---------
C
C     LSAME = ISSAME()
C
C     Input
C     -----
C
C     Values in common block.
C
C
C     Output
C     ------
C
C     Returns .TRUE. if the fields will be the same.
C
C
C     Method
C     ------
C
C     Checks interpolation request flags in common blocks.
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
C     J.D.Chambers     ECMWF
C
C----<
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "grfixed.h"
C
C     Local variables
C
      INTEGER IDIFF
      LOGICAL LDEFIN, LDEFOUT, LSUBAREA
C
C     Externals
C
      LOGICAL ISSAMEIARRAY, ISSAMERARRAY
      EXTERNAL ISSAMEIARRAY, ISSAMERARRAY
C
C     -----------------------------------------------------------------|
C
      ISSAME = .TRUE.
C
C     Exit if no postprocessing specified.
C
      IF( .NOT.LINTOUT ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'ISSAME: No postprocessing specified.',JPQUIET)
        RETURN
      ENDIF

C
C     Set logicals true if default input/output areas specified.
C
      LDEFIN  = ( (NIAREA(1).EQ.0) .AND. (NIAREA(2).EQ.0) .AND.
     X            (NIAREA(3).EQ.0) .AND. (NIAREA(4).EQ.0) )
      LDEFOUT = ( (NOAREA(1).EQ.0) .AND. (NOAREA(2).EQ.0) .AND.
     X            (NOAREA(3).EQ.0) .AND. (NOAREA(4).EQ.0) )
C
C     Set flag if a subarea has been requested
C
      LSUBAREA = (.NOT.LDEFIN .AND. .NOT.LDEFOUT) .AND.
     X           ( (NOAREA(1).LT.NIAREA(1)).OR.
     X             (NOAREA(2).GT.NIAREA(2)).OR.
     X             (NOAREA(3).GT.NIAREA(3)).OR.
     X             (NOAREA(4).LT.NIAREA(4)) )
C
C
C     Check for rotation
C
      IF( (NOROTA(1).NE.0) .OR. (NOROTA(2).NE.0) ) ISSAME = .FALSE.
C
C     Check for same formats, representation and bit packing accuracy.
C
cs      IF( NIFORM.NE.NOFORM) ISSAME = .FALSE.
      IF( NIREPR.NE.NOREPR) ISSAME = .FALSE.

      IF( (NOREPR.EQ.JPQUASI).AND.(NIREPR.EQ.JPQUASI) ) THEN
        ISSAME = ISSAMEIARRAY(NIGAUSS,MILLEN, NOGAUSS,NOLPTS)
     X     .AND. ISSAMERARRAY(NIGAUSS,RIGAUSS,NOGAUSS,ROGAUSS)
        IF (ISSAME) THEN
          CALL INTLOG(JP_DEBUG,
     X      'ISSAME: Input and output are Reduced Gaussian',JPQUIET)
          CALL INTLOG(JP_DEBUG,
     X      'ISSAME: with the same PL and latitudes arrays;',JPQUIET)
          CALL INTLOG(JP_DEBUG,
     X      'ISSAME: no postprocessing required.',JPQUIET)
          RETURN
        ENDIF
      ENDIF

      IF( (NOREPR.EQ.JPGAUSSIAN) .AND. (NIREPR.EQ.JPGAUSSIAN) ) THEN
        IF( (NIGAUSS.EQ.NOGAUSS) .AND. (.NOT.LSUBAREA) ) THEN
          ISSAME = .TRUE.
          CALL INTLOG(JP_DEBUG,
     X      'ISSAME: Input and output are Regular Gaussian',JPQUIET)
          CALL INTLOG(JP_DEBUG,
     X      'ISSAME: with the same resolution and area;',JPQUIET)
          CALL INTLOG(JP_DEBUG,
     X      'ISSAME: no postprocessing required.',JPQUIET)
          RETURN
        ENDIF
      ENDIF
C
C     If input is quasi gaussian and output required to be the same (ie
C     'gaussian' specified via CALL INTOUT('gaussian', ...) ), change
C     output to regular gaussian unless same resolution requested.
C
      IF( (NOREPR.EQ.JPNOTYPE) .AND.
     X    (NIGAUSS.NE.NOGAUSS) ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'ISSAME: Input is quasi gaussian and output required',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'ISSAME: not the same resolution;',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'ISSAME: change output to regular gaussian.',JPQUIET)
        NOREPR = JPGAUSSIAN
      ENDIF
C
C     If input is quasi gaussian and output required to be the same (ie
C     'gaussian' specified via CALL INTOUT('gaussian', ...) ), change
C     output to regular gaussian if subarea requested.
C
      IF( (NOREPR.EQ.JPNOTYPE) .AND. LSUBAREA ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'ISSAME: Input is quasi gaussian and output required',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'ISSAME: has a subarea;',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'ISSAME: change output to regular gaussian.',JPQUIET)
        NOREPR = JPGAUSSIAN
      ENDIF
C
C     Exit if both input and output are regular gaussian with
C     the same resolution and area, since no
C     postprocessing is required.
C
      IF( (NIREPR.EQ.JPGAUSSIAN) .AND.
     X    (NOREPR.EQ.JPGAUSSIAN) .AND.
     X    (NIGAUSS.EQ.NOGAUSS)   .AND.
     X    (.NOT.LSUBAREA)       ) THEN
        CALL INTLOG(JP_DEBUG,
     X    'ISSAME: Input and output are regular gaussian with',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'ISSAME: the same resolution and area;',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X    'ISSAME: no postprocessing is required.',JPQUIET)
        RETURN
      ENDIF
C
C     If spherical, check for same truncation.
C
      IF( (NIREPR.EQ.JPSPHERE) .AND.
     X    (NOREPR.EQ.JPSPHERE) .AND.
     X    (NIRESO.NE.NORESO)   ) ISSAME = .FALSE.
C
C     If lat.long, check for same grid intervals and area.
C
      IF( (NIREPR.EQ.JPREGULAR) .AND.
     X    (NOREPR.EQ.JPREGULAR) .AND.
     X    (NOGRID(1).NE.0)      ) THEN
        IF( NIGRID(1).NE.NOGRID(1)) ISSAME = .FALSE.
        IF( NIGRID(2).NE.NOGRID(2)) ISSAME = .FALSE.
      ENDIF
      IF( LSUBAREA ) ISSAME = .FALSE.
C
cs      IF( (NIREPR.EQ.JPREDLL) .AND.
cs     X    (NOREPR.EQ.JPREDLL)
cs     X    (NIGRID(2).NE.NOGRID(2)) ) ISSAME = .FALSE.
C
C     Check if areas are same unless defaults given.
C
      IF( (.NOT.LDEFIN) .AND. (.NOT.LDEFOUT) ) THEN
        IF( (NOGRID(1).NE.0) .OR. (NOGRID(2).NE.0) ) THEN
C
          IF( NIAREA(1).NE.NOAREA(1)) ISSAME = .FALSE.
C
          IDIFF = NIAREA(2) - NOAREA(2)
          IF( (IDIFF.NE.0) .AND. (IDIFF.NE.JP360) ) ISSAME = .FALSE.
C
          IF( NIAREA(3).NE.NOAREA(3) ) ISSAME = .FALSE.
C
          IDIFF = NIAREA(4) - NOAREA(4)
          IF( (IDIFF.NE.0) .AND. (IDIFF.NE.JP360) ) ISSAME = .FALSE.
        ENDIF
      ENDIF
C
C     If gaussian, check for same grid specification.
C
      IF( (NIREPR.EQ.JPGAUSSIAN) .AND. (NOREPR.EQ.JPGAUSSIAN) ) THEN
        ISSAME = (NIGAUSS.EQ.NOGAUSS)
      ENDIF
      IF( (NIREPR.EQ.JPQUASI) .AND. (NOREPR.EQ.JPQUASI) ) THEN
        ISSAME = ISSAMEIARRAY(NIGAUSS,MILLEN, NOGAUSS,NOLPTS)
     X     .AND. ISSAMERARRAY(NIGAUSS,RIGAUSS,NOGAUSS,ROGAUSS)
      ENDIF
C
C     Check for same scanning mode.
C
C Sinisa commented scanning mode checking because of UK fields
c      IF( NISCNM.NE.NOSCNM) ISSAME = .FALSE.
C
C     Check for same table number, parameter number, level number and
C     level type.
C
cs      IF( NITABLE.NE.NOTABLE) ISSAME = .FALSE.
cs      IF( NIPARAM.NE.NOPARAM) ISSAME = .FALSE.
cs      IF( NILEVEL.NE.NOLEVEL) ISSAME = .FALSE.
cs      IF( NILEVT .NE.NOLEVT ) ISSAME = .FALSE.
C
      IF( ISSAME ) RETURN
C
C     -----------------------------------------------------------------|
C
      RETURN
      END
