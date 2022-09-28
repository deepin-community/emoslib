C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IRSIZE (KGTRUNC, KAREA, KLLEN, KNWE, KNNS, KLN,
     1   KTOTAL, PGAUSS, OWEGLOBE, ONPOLE, OSPOLE, KPR, KERR)
C
C---->
C**** *IRSIZE*
C
C     PURPOSE
C     _______
C
C     Evaluate the array sizes for a quasi regular Gaussian field.
C
C     INTERFACE
C     _________
C
C     IERR = IRSIZE (KGTRUNC, KAREA, KLLEN, KNWE, KNNS, KLN,
C    1   KTOTAL, PGAUSS, OWEGLOBE, ONPOLE, OSPOLE, KPR, KERR)
C
C     Input parameters
C     ________________
C
C     KGTRUNC    - The Gaussian truncation.
C
C     KAREA      - The field area definition (N, W, S, E) provided by
C                  the calling routine.
C
C     KLLEN      - This array contains the quasi regular Gaussian
C                  line length definition.
C
C     KPR        - The debug print switch.
C                  0  , No debugging output.
C                  1  , Produce debugging output.
C
C     KERR       - The error control flag.
C                  -ve, No error message. Return error code.
C                  0  , Hard failure with error message.
C                  +ve, Print error message. Return error code.
C
C     Output parameters
C     ________________
C
C     KNWE       - The maximum number of points in the West-East
C                  direction for this quasi regular Gaussian grid.
C
C     KNNS       - The number of points in the North-South direction
C                  in the field.
C
C     KLN        - The Northern Gaussian latitude number for this
C                  area.
C
C     KTOTAL     - The total field length for this quasi regular
C                  Gaussian grid and area.
C
C     PGAUSS     - The full list of Gaussian latitudes for this
C                  truncation.
C
C     OWEGLOBE   - This variable will be true if the array of
C                  longitudes spans the globe.
C
C     ONPOLE     - This variable will be true if the North polar line
C                  of latitude is included in the grid.
C
C     OSPOLE     - This variable will be true if the South polar line
C                  of latitude is included in the grid.
C
C     Return value
C     ____________
C
C     The error indicator (INTEGER).
C
C     Error and Warning Return Values
C     _______________________________
C
C     23801 A non-global area was requested and this option is not
C           supported yet.
C
C     Common block usage
C     __________________
C
C     None
C
C     EXTERNALS
C     _________
C
C     IGGLAT     - Compute Gaussian lines of latitude for a given
C                  truncation.
C     INTLOG(R)  - Logs messages.
C
C     METHOD
C     ______
C
C
C     A global definition is generated for the quasi regular field
C     and all three flag variable are set TRUE.
C
C     A REAL array of the full set of Gaussian latitudes is generated
C     for the Gaussian truncation requested.
C
C     REFERENCE
C     _________
C
C     None
C
C     COMMENTS
C     ________
C
C     Program contains sections 0 to 4 and 9
C
C     AUTHOR
C     ______
C
C     K. Fielding      *ECMWF*      Nov 1993
C
C     MODIFICATIONS
C     _____________
C
C     None
C
C----<
C     _______________________________________________________
C
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
C*    Prefix conventions for variable names
C
C     Logical      L (but not LP), global or common.
C                  O, dummy argument
C                  G, local variable
C                  LP, parameter.
C     Character    C, global or common.
C                  H, dummy argument
C                  Y (but not YP), local variable
C                  YP, parameter.
C     Integer      M and N, global or common.
C                  K, dummy argument
C                  I, local variable
C                  J (but not JP), loop control
C                  JP, parameter.
C     REAL         A to F and Q to X, global or common.
C                  P (but not PP), dummy argument
C                  Z, local variable
C                  PP, parameter.
C
C     Implicit statement to force declarations
C
      IMPLICIT NONE
C
#include "parim.h"
C
C     Dummy arguments
      LOGICAL OWEGLOBE, ONPOLE, OSPOLE
      INTEGER KGTRUNC, KNWE, KNNS, KLN, KTOTAL, KPR, KERR
      INTEGER KAREA (4), KLLEN (*)
      REAL PGAUSS (*)
C
C     Local variables
      LOGICAL GZERO
      INTEGER IGAUSS, ILATNUM, ILS, INDIST, ISDIST, IERR
      INTEGER JLAT
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 23800)
C
C     Function externals
C
      INTEGER IGGLAT
C
C     _______________________________________________________
C
C*    Section 1. Initialisation
C     _______________________________________________________
C
  100 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IRSIZE: Section 1.',JPQUIET)
C
      IRSIZE = 0
C
      IF (KPR .GE. 1) THEN
        CALL INTLOG(JP_DEBUG,'IRSIZE: Input parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,
     X     'IRSIZE: Gaussian truncation = ',KGTRUNC)
        IF (KNNS .NE. 0) CALL INTLOG(JP_DEBUG,
     X    'IRSIZE: No.lat. pts = ',KNNS)
C
        CALL INTLOG(JP_DEBUG,'IRSIZE: Area North = ', KAREA(1))
        CALL INTLOG(JP_DEBUG,'IRSIZE: Area West  = ', KAREA(2))
        CALL INTLOG(JP_DEBUG,'IRSIZE: Area South = ', KAREA(3))
        CALL INTLOG(JP_DEBUG,'IRSIZE: Area East  = ', KAREA(4))
      ENDIF
C
      GZERO = .FALSE.
C
      ILATNUM = KGTRUNC * 2
C
C     _______________________________________________________
C
C*    Section 2. If all elements of KAREA are zero generate
C                a global field
C     _______________________________________________________
C
  200 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IRSIZE: Section 2.',JPQUIET)
C
Cxxx  IF (KAREA (JPEAST) .NE. 0 .OR. KAREA (JPWEST) .NE. 0) THEN
C
Cxxx     IRSIZE = JPROUTINE + 1
Cxxx     IF (KERR .GE. 0) WRITE (*, 9001) IRSIZE,
Cxxx 1      REAL (KAREA (JPNORTH) ) / PPMULT,
Cxxx 2      REAL (KAREA (JPWEST) ) / PPMULT,
Cxxx 3      REAL (KAREA (JPSOUTH) ) / PPMULT,
Cxxx 4      REAL (KAREA (JPEAST) ) / PPMULT
Cxxx     IF (KERR .EQ. 0) CALL ABORTX ('IRSIZE')
Cxxx     GO TO 900
Cxxx  ENDIF
C
      OWEGLOBE = .TRUE.
C
C     _______________________________________________________
C
C*    Section 3. Generate points along a line of meridian
C     _______________________________________________________
C
  300 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IRSIZE: Section 3.',JPQUIET)
C
C     Generate full Gaussian latitudes for this truncation
C
      IERR = IGGLAT (ILATNUM, PGAUSS, KPR, KERR)
C
      IF (IERR .GT. 0) THEN
         IRSIZE = IERR
         GO TO 900
      ENDIF
C
C     Find Gaussian lines corresponding to North and South points
C     provided
C
      KLN = 1
      ILS = ILATNUM
C
      IF (KNNS .EQ. 0) THEN
C
C       Number of lines not set (output field)
C
        KLN = 0
        ILS = ILATNUM
C
        DO 310 JLAT = 2, ILATNUM
C
          IGAUSS = NINT (PGAUSS (JLAT) * PPMULT)
C
          IF (KLN .EQ. 0 .AND. KAREA (JPNORTH) .GT. IGAUSS) THEN
            KLN = JLAT - 1
            KAREA (JPNORTH) = NINT (PGAUSS (KLN) * PPMULT)
          ENDIF
C
          IF (ILS.EQ.ILATNUM .AND. KAREA(JPSOUTH).GE.IGAUSS) THEN
            ILS = JLAT
            KAREA (JPSOUTH) = NINT (PGAUSS (ILS) * PPMULT)
          ENDIF
C
  310   CONTINUE
C
        KNNS = ILS - KLN + 1
C
      ELSE
C
C       Number of lines set (input field)
C
        JLAT = 1
        KLN = JLAT
        ILS = JLAT
C
        INDIST = ABS (NINT (PGAUSS (JLAT) * PPMULT) - KAREA (JPNORTH) )
        ISDIST = ABS (NINT (PGAUSS (JLAT) * PPMULT) - KAREA (JPSOUTH) )
C
        DO 320 JLAT = 2, ILATNUM
C
          IF (ABS (NINT (PGAUSS (JLAT) * PPMULT) - KAREA (JPNORTH) )
     1         .LT. INDIST) THEN
C
             KLN = JLAT
             INDIST = ABS(NINT(PGAUSS (JLAT) * PPMULT) - KAREA(JPNORTH))
C
          ENDIF
C
          IF (ABS (NINT (PGAUSS (JLAT) * PPMULT) - KAREA (JPSOUTH) )
     1         .LT. ISDIST) THEN
C
            ILS = JLAT
            ISDIST = ABS(NINT(PGAUSS(JLAT) * PPMULT) - KAREA(JPSOUTH))
C
          ENDIF
C
  320   CONTINUE
C
        IF (KNNS .NE. ILS - KLN + 1) THEN
          IRSIZE = JPROUTINE + 2
          IF (KERR .GE. 0) THEN
            CALL INTLOG(JP_ERROR,
     X        'IRSIZE: No. lines specified = ',KNNS)
            CALL INTLOG(JP_ERROR,
     X        'IRSIZE: Gaussian N limit = ',KLN)
            CALL INTLOG(JP_ERROR,
     X        'IRSIZE: Gaussian S limit = ',ILS)
          ENDIF
C
          IF (KERR .EQ. 0) CALL INTLOG(JP_FATAL,
     X      'IGLSIZE: Interpolation failed.',IRSIZE)
          GO TO 900
        ENDIF
C
      ENDIF
C
      ONPOLE = KLN .EQ. 1
      OSPOLE = ILS .EQ. ILATNUM
C
      KNNS = ILS - KLN + 1
C
      KAREA (JPNORTH) = NINT (PGAUSS (KLN) * PPMULT)
      KAREA (JPSOUTH) = NINT (PGAUSS (ILS) * PPMULT)
C
C     _______________________________________________________
C
C*    Section 4. Use line lengths to set maximum length and total
C     _______________________________________________________
C
  400 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IRSIZE: Section 4.',JPQUIET)
C
      KNWE = KLLEN (KGTRUNC)
C
      KTOTAL = 0
C
      DO 410 JLAT = KLN, ILS
C
        KTOTAL = KTOTAL + KLLEN (JLAT)
C
  410 CONTINUE
C
      IF (KPR .GE. 1) THEN
        CALL INTLOG(JP_DEBUG,'IRSIZE: Output parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IRSIZE: Max W-E line len = ',KNWE)
        CALL INTLOG(JP_DEBUG,'IRSIZE: No lat lines = ',KNNS)
        CALL INTLOG(JP_DEBUG,'IRSIZE: Total no.pts = ',KTOTAL)
        CALL INTLOG(JP_DEBUG,'IGLSIZE: Northern line no. =',KLN)
        CALL INTLOG(JP_DEBUG,'IGLSIZE: Southern line no. =',ILS)
C
        IF( OWEGLOBE ) THEN
          CALL INTLOG(JP_DEBUG,'IGLSIZE: W-E fld is global',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IGLSIZE: W-E fld is NOT global',JPQUIET)
        ENDIF
C
        IF( ONPOLE ) THEN
          CALL INTLOG(JP_DEBUG,'IGLSIZE: N pole is in fld.',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IGLSIZE: N pole is NOT in fld.',JPQUIET)
        ENDIF
C
        IF( OSPOLE ) THEN
          CALL INTLOG(JP_DEBUG,'IGLSIZE: S pole is in fld.',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,'IGLSIZE: S pole is NOT in fld.',JPQUIET)
        ENDIF
C
      ENDIF
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
  900 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IRSIZE: Section 9.',JPQUIET)
C
C9001 FORMAT (' IRSIZE ERROR', I6, ' - One of the area limits is ',
C    1   'non-zero.' / 22X, 'They are', 4F10.4)
C
      RETURN
      END
