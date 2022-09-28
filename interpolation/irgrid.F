C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IRGRID (KAREA, KLLEN, KLN, PGAUSS, KSTART,
     1   KLATG, KNNS, OWEGLOBE, KPR, KERR)
C
C---->
C**** *IRGRID*
C
C     PURPOSE
C     _______
C
C     Generate the arrays of latitude points and the starting points
C     of the lines of latitude for a quasi regular Gaussian grid.
C
C     INTERFACE
C     _________
C
C     IERR = IRGRID (KAREA, KLLEN, KLN, PGAUSS, KSTART,
C    1   KLATG, KNNS, OWEGLOBE, KPR, KERR)
C
C     Input parameters
C     ________________
C
C     KAREA      - The area definition (N, W, S, E) provided by the
C                  calling routine. (Currently unused).
C
C     KLLEN   - This quasi regular Gaussian line length definition
C                  is for the full grid and not just the North and
C                  South limits selected.
C
C     KLN        - The Northern Gaussian latitude number for this area.
C
C     PGAUSS     - The full list of Gaussian latitudes for this
C                  truncation.
C
C     KNNS       - The number of points in the North-South direction
C                  in this field.
C
C     OWEGLOBE   - This variable will be true if the array of
C                  longitudes spans the globe and it is an input grid.
C                  (Currently unused)
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
C     KSTART     - The array offset for each line of latitude within
C                  the quasi regular field.
C
C     KLATG      - The array of latitudes for this Gaussian
C                  truncation and area.
C
C     Return value
C     ____________
C
C     The error indicator (INTEGER).
C
C     Error and Warning Return Values
C     _______________________________
C
C     None
C
C     Common block usage
C     __________________
C
C     None
C
C     EXTERNALS
C     _________
C
C     INTLOG(R)    - Logs messages.
C
C     METHOD
C     ______
C
C     The array KLATG is generated using the information provided by
C     the other parameters. The array KSTART is generated using the
C     array KLLEN. Note that Gaussian latitudes will be stored as
C     NINT (PGAUSS * 1.0E4).
C
C     REFERENCE
C     _________
C
C     None
C
C     COMMENTS
C     ________
C
C     Program contains sections 0 to 3 and 9
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
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nifld.common"
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 23700)
C
C     Dummy arguments
C
      LOGICAL OWEGLOBE
      INTEGER KNNS, KLN, KPR, KERR
      INTEGER KAREA (4), KLLEN (KNNS)
      INTEGER KLATG (KNNS), KSTART (KNNS)
      REAL PGAUSS (*)
C
C     Local variables
C
      LOGICAL LPRINT
      INTEGER JLAT
      REAL PNLAT, ZNLAT, MFACTOR
C
C     Function externals
C
C     _______________________________________________________
C
C*    Section 1. Initialisation
C     _______________________________________________________
C
  100 CONTINUE
C
      LPRINT = KPR.NE.0
C
      IF( LPRINT ) CALL INTLOG(JP_DEBUG,'IRGRID: Section 1.',JPQUIET)
C
      IRGRID = 0
C
      IF( LPRINT ) THEN
        CALL INTLOG(JP_DEBUG,'IRGRID: Input parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IRGRID: N Gaussian line = ',KLN)
        CALL INTLOG(JP_DEBUG,'IRGRID: No. of lat lines = ',KNNS)
        CALL INTLOG(JP_DEBUG,'IRGRID: Area North = ', KAREA(1))
        CALL INTLOG(JP_DEBUG,'IRGRID: Area West  = ', KAREA(2))
        CALL INTLOG(JP_DEBUG,'IRGRID: Area South = ', KAREA(3))
        CALL INTLOG(JP_DEBUG,'IRGRID: Area East  = ', KAREA(4))
        IF( OWEGLOBE ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'IRGRID: Input W-E fld is global.',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,
     X      'IRGRID: Input W-E fld is NOT global.',JPQUIET)
        ENDIF
      ENDIF
C
C     _______________________________________________________
C
C*    Section 2. Generate start position of lines of latitude
C     _______________________________________________________
C
  200 CONTINUE
C
      IF( LPRINT ) CALL INTLOG(JP_DEBUG,'IRGRID: Section 2.',JPQUIET)
C
      KSTART(1) = 1
      DO JLAT = KLN, KLN + KNNS - 2
        KSTART(JLAT+1) = KSTART(JLAT) + KLLEN(JLAT)
      ENDDO
C
C     _______________________________________________________
C
C*    Section 3. Generate points along a line of meridian
C     _______________________________________________________
C
  300 CONTINUE
C
      IF( LPRINT ) CALL INTLOG(JP_DEBUG,'IRGRID: Section 3.',JPQUIET)
C
      DO JLAT = KLN, KLN + KNNS - 1
C
C       If it is a stretched field, calculate the true latitude and
C       the map factor.
C
        PNLAT = PGAUSS(JLAT)
        IF( RISTRET.NE.0 ) THEN
          CALL STRLAT(PNLAT, RISTRET, 0, ZNLAT, MFACTOR)
        ELSE
          ZNLAT = PNLAT
        ENDIF
        KLATG(JLAT-KLN+1) = NINT(ZNLAT * PPMULT)
      ENDDO
      KAREA(1)   =  KLATG(1)
      KAREA(KLN) =  KLATG(KLN)
      IF( LPRINT ) THEN
        CALL INTLOG(JP_DEBUG,'IRGRID: Modified North = ', KAREA(1))
        CALL INTLOG(JP_DEBUG,'IRGRID: Modified South = ', KAREA(3))
      ENDIF
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
  900 CONTINUE
C
      IF( LPRINT ) CALL INTLOG(JP_DEBUG,'IRGRID: Section 9.',JPQUIET)
C
      RETURN
      END
