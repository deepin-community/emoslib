C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE DDSTYLE(FIELD,IN_EW,IN_NS,OUT_EW,OUT_NS,KRET)
C
C---->
C**** DDSTYLE
C
C     Purpose
C     -------
C
C     Create a subgrid of an input regular latitude/longitude
C     grid by selecting points (dissemination style).
C
C
C     Interface
C     ---------
C
C     CALL DDSTYLE(FIELD,IN_EW,IN_NS,OUT_EW,OUT_NS,KRET)
C
C     Input
C     -----
C
C     FIELD   - Input regular latitude/longitude gridpoint field.
C     IN_EW   - East-west increment of input field (degrees * 10*5).
C     IN_NS   - North-south increment of input field (degrees * 10*5).
C     OUT_EW  - East-west increment of output field (degrees * 10*5).
C     OUT_NS  - North-south increment of output field (degrees * 10*5).
C
C
C     Output
C     ------
C
C     FIELD   - Output regular latitude/longitude gridpoint field.
C     KRET    - Return status code
C               0 = OK
C
C
C     Method
C     ------
C
C     Area boundaries for output field are adjusted (by shrinking)
C     if necessary to fit the grid.
C
C
C     Externals
C     ---------
C
C     INTLOG  - Logs output messages
C     JDEBUG  - Checks whether the debug flag is to be turned on.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     May 1999
C
C----<
C
      IMPLICIT NONE
C
#include "jparams.h"
#include "parim.h"
#include "nofld.common"
C
C     Subroutine arguments
C
      REAL FIELD(*)
      INTEGER IN_EW,IN_NS,OUT_EW,OUT_NS,KRET
C
C     Local variables
C
      INTEGER N, INC_EW,INC_NS,N_EW,N_NS,LOOPO,LOOPI,NEXT
      INTEGER NWEST,NEAST, NUM_EW
      REAL NORTH,WEST,SOUTH,EAST
      LOGICAL LDEBUG
C
C     Externals
C
C ------------------------------------------------------------------
C     Section 1. Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE

      KRET = 0
C
      CALL JDEBUG( )
      LDEBUG = ( NDBG.GT.0 )
C
      IF( LDEBUG ) THEN
        CALL INTLOG(JP_DEBUG,'DDSTYLE: input E-W increment = ', IN_EW)
        CALL INTLOG(JP_DEBUG,'DDSTYLE: input N-S increment = ', IN_NS)
        CALL INTLOG(JP_DEBUG,'DDSTYLE: output E-W increment = ', OUT_EW)
        CALL INTLOG(JP_DEBUG,'DDSTYLE: output N-S increment = ', OUT_NS)
      ENDIF
C
C ------------------------------------------------------------------
C     Section 2. Check that output grid is a subset of the input grid.
C ------------------------------------------------------------------
C
  200 CONTINUE
C
C     Fixup input/output field area definitions according to grid
C     specification.
C
      NORTH = REAL(NOAREA(1)) / PPMULT
      WEST  = REAL(NOAREA(2)) / PPMULT
      SOUTH = REAL(NOAREA(3)) / PPMULT
      EAST  = REAL(NOAREA(4)) / PPMULT
C
      NWEST = NOAREA(2)
      NEAST = NOAREA(4)
      IF( NWEST.LT.0 ) NWEST = NWEST + 36000000
      IF( NEAST.LT.0 ) NEAST = NEAST + 36000000
      IF( NEAST.LT.NWEST ) NEAST = NEAST + 36000000
C
C     Check increments
C
      INC_EW = OUT_EW / IN_EW
      IF( (INC_EW*IN_EW).NE.OUT_EW ) THEN
         KRET = 1
        IF( LDEBUG ) THEN
          CALL INTLOG(JP_ERROR,
     X      'DDSTYLE: East-west step not multiple of input',JPQUIET)
          CALL INTLOG(JP_ERROR,'DDSTYLE: Requested step  = ',OUT_EW)
          CALL INTLOG(JP_ERROR,'DDSTYLE: Input GRIB step = ',IN_EW)
        ENDIF
        GOTO 900
      ENDIF
C
      INC_NS = OUT_NS / IN_NS
      IF( (INC_NS*IN_NS).NE.OUT_NS ) THEN
         KRET = 1
        IF( LDEBUG ) THEN
          CALL INTLOG(JP_ERROR,
     X     'DDSTYLE: North-south step not multiple of input',JPQUIET)
          CALL INTLOG(JP_ERROR,'DDSTYLE: Requested step  = ',OUT_NS)
          CALL INTLOG(JP_ERROR,'DDSTYLE: Input GRIB step = ',IN_NS)
        ENDIF
        GOTO 900
      ENDIF
C
      N_EW = ( (NEAST - NWEST) / OUT_EW ) + 1
      N_NS = ( (NOAREA(1) - NOAREA(3)) / OUT_NS ) + 1
C
      IF( LDEBUG ) THEN
        CALL INTLOG(JP_DEBUG,'DDSTYLE: No. of E_W output pts = ', N_EW)
        CALL INTLOG(JP_DEBUG,'DDSTYLE: No. of N-S output pts = ', N_NS)
      ENDIF
C
C ------------------------------------------------------------------
C     Section 3. Generate new grid.
C ------------------------------------------------------------------
C
  300 CONTINUE
C
C     Select points for new grid.
C
      NUM_EW = NINT((EAST-WEST)*PPMULT)/IN_EW + 1
C
      NEXT = 1
      DO LOOPO = 1, N_NS*INC_NS, INC_NS
        N = (LOOPO-1)*NUM_EW
        DO LOOPI = 1, N_EW*INC_EW, INC_EW
          FIELD(NEXT) = FIELD(N+LOOPI)
          NEXT = NEXT + 1
        ENDDO
      ENDDO
C
C ------------------------------------------------------------------
C     Section 9. Return.
C ------------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END
