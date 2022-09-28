C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGSIZE(KGTRUNC, KAREA, KNWE, KNNS, KLN,
     X   PGAUSS, OWEGLOBE, ONPOLE, OSPOLE, KPR, KERR)
C
C---->
C**** *IGSIZE*
C
C     Purpose
C     -------
C     Evaluate the array sizes for a Gaussian truncation and area.
C
C
C     Interface
C     ---------
C     IERR = IGSIZE(KGTRUNC, KAREA, KNWE, KNNS, KLN,
C    X              PGAUSS, OWEGLOBE, ONPOLE, OSPOLE, KPR, KERR)
C
C
C     Input parameters
C     ----------------
C     KGTRUNC - The Gaussian truncation.
C     KAREA   - The field area definition (N, W, S, E) provided by
C               the calling routine.
C     KNWE    - The number of points in the West-East direction in
C               the field. This will be zero for an output field.
C     KNNS    - The number of points in the North-South direction
C               in the field. This will be zero for an output field.
C     KPR     - The debug print switch.
C                0  , No debugging output.
C                1  , Produce debugging output.
C     KERR    - The error control flag.
C                -ve, No error message. Return error code.
C                0  , Hard failure with error message.
C                +ve, Print error message. Return error code.
C
C
C     Output parameters
C     -----------------
C     KAREA    - The North and South values will be modified to
C                contain the values of the Gaussian latitudes used.
C     KNWE     - The number of points in the West-East direction in
C                the field.
C     KNNS     - The number of points in the North-South direction
C                in the field.
C     KLN      - The Northern Gaussian latitude number for this area.
C     PGAUSS   - The full list of Gaussian latitudes for this
C                truncation.
C     OWEGLOBE - This variable will be true if the array of
C                longitudes spans the globe.
C     ONPOLE   - This variable will be true if the North polar line
C                of latitude is included in the grid.
C     OSPOLE   - This variable will be true if the South polar line
C                of latitude is included in the grid.
C
C
C     Return value
C     ------------
C     The error indicator (INTEGER).
C
C
C     Error and Warning Return Values
C     -------------------------------
C     -1 The Eastern limit of the range did not have the expected
C        value. The Western value will be assumed to be correct. This
C        could arise because of known deficiencies in the GRIB
C        representation and will be treated as a WARNING error.
C
C     22501 The specified number of lines of latitude did not match
C           the Gaussian lines nearest the North and South values
C           provided.
C
C
C     Common block usage
C     ------------------
C     None
C
C
C     Externals
C     ---------
C     IGGLAT   - Compute Gaussian lines of latitude for a given
C                truncation.
C     INTLOG(R) - Logs messages.
C
C
C     Method
C     ------
C     If all the elements of the array KAREA are zero then a standard
C     global grid is generated and all elements of the array KAREA are
C     rewritten with the appropriate values. Otherwise it is assumed
C     that the West and East limits in KAREA have been validated. If
C     the number of longitude points is provided a warning will be
C     given if the West to East range does not match the number of
C     longitude points times the stride.
C
C     If the number of lines of longitude and latitude are not set on
C     entry then the North and South limits in KAREA will be expanded
C     outward to the next Gaussian latitudes. If the number of lines
C     of longitude and latitude are set on entry then the nearest
C     points to the North and South limits provided are chosen. It is
C     an error if these points do not provide the correct number of
C     latitudes. The revised North and South limits are written back
C     into the array KAREA.
C
C
C     Reference
C     ---------
C     None
C
C
C     Comments
C     --------
C     None
C
C
C     Author
C     ------
C     K. Fielding      *ECMWF*      Oct 1993
C
C
C     Modifications
C     -------------
C     Allow for unusual NCEP gaussian grids
C     J.D.Chambers   ECMWF   October 2000
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "parim.h"
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER(JPROUTINE = 22500)
C
C     Function arguments
C
      LOGICAL OWEGLOBE, ONPOLE, OSPOLE
      INTEGER KGTRUNC, KNWE, KNNS, KLN, KPR, KERR, KAREA(4)
      REAL PGAUSS(*)
C
C     Local variables
C
      INTEGER ILNSTR, IEAST, IWEST, IREACH, ILATNUM, IGAUSS
      INTEGER INDIST, ISDIST, ILS, IERR, JLAT
      LOGICAL LPRINT
C
C     Function externals
C
      INTEGER IGGLAT
C
C     -----------------------------------------------------------------|
C*    Section 1. Initialisation
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      IGSIZE = 0
      LPRINT = KPR.GE.1
C
      IF( LPRINT ) THEN
        CALL INTLOG(JP_DEBUG,'IGSIZE: Section 1.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IGSIZE: Input parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IGSIZE: Gaussian truncation = ',KGTRUNC)
        CALL INTLOG(JP_DEBUG,'IGSIZE: No.long pts = ',KNWE)
        CALL INTLOG(JP_DEBUG,'IGSIZE: No.lat. pts = ',KNNS)
        CALL INTLOG(JP_DEBUG,'IGSIZE: Area North = ',KAREA(1))
        CALL INTLOG(JP_DEBUG,'IGSIZE: Area West  = ',KAREA(2))
        CALL INTLOG(JP_DEBUG,'IGSIZE: Area South = ',KAREA(3))
        CALL INTLOG(JP_DEBUG,'IGSIZE: Area East  = ',KAREA(4))
      ENDIF
C
      ILNSTR = JP90 / KGTRUNC
      ILATNUM = KGTRUNC * 2
C
C     -----------------------------------------------------------------|
C*    Section 2. If all elements of KAREA are missing generate
C                a global field
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IF( LPRINT ) CALL INTLOG(JP_DEBUG,'IGSIZE: Section 2.',JPQUIET)
C
      IF( (KAREA(JPEAST).EQ.0) .AND.
     X    (KAREA(JPWEST).EQ.0) .AND.
     X    (KAREA(JPNORTH).EQ.0).AND.
     X    (KAREA(JPSOUTH).EQ.0) ) THEN
C
        KAREA(JPNORTH) = JP90
        KAREA(JPWEST)  = 0
        KAREA(JPSOUTH) = - JP90
        KAREA(JPEAST)  = JP360 - ILNSTR
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3. Generate points around a line of latitude.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      IF( LPRINT ) CALL INTLOG(JP_DEBUG,'IGSIZE: Section 3.',JPQUIET)
C
C     Force both KAREA(JPWEST) and KAREA(JPEAST) into standard
C     range (0 to JP360)
C
      IF( KAREA(JPWEST).LT.0 ) KAREA(JPWEST) = KAREA(JPWEST) + JP360
      IF( KAREA(JPEAST).LT.0 ) KAREA(JPEAST) = KAREA(JPEAST) + JP360
C
C     Standardise range so that EAST > WEST
C
      IEAST = KAREA(JPEAST)
      IWEST = KAREA(JPWEST)
C
      IF( IEAST.LT.IWEST ) THEN
        IF( IEAST.LT.0 ) THEN
          IEAST = IEAST + JP360
          KAREA(JPEAST) = IEAST
        ELSE
          IWEST = IWEST - JP360
          KAREA(JPWEST) = IWEST
        ENDIF
      ENDIF
C
      IF( KNWE.EQ.0 ) THEN
C
C       Is it a global grid ?
C
        IREACH   = IEAST - IWEST + ILNSTR
        OWEGLOBE = IREACH.GE.JP360
C
C       Number of points along line is
C
        KNWE = NINT( FLOAT(IREACH) / FLOAT(ILNSTR) )
        IF( OWEGLOBE ) KNWE = 4*KGTRUNC
C
      ELSE
C
C       Number of points is set (input field)
C
        IF( (IWEST + (KNWE - 1)*ILNSTR).NE.IEAST ) THEN
C
C         Warn of possible mismatch
C
          IGSIZE = - 1
          CALL INTLOG(JP_WARN,
     X      'IGSIZE: West-East range does not match the',JPQUIET)
          CALL INTLOG(JP_WARN,
     X      'IGSIZE: number of points and the stride.',JPQUIET)
          CALL INTLOGR(JP_WARN,
     X      'IGSIZE: West = ',REAL(KAREA(JPWEST))/PPMULT)
          CALL INTLOGR(JP_WARN,
     X      'IGSIZE: East = ',REAL(KAREA(JPEAST))/PPMULT)
          CALL INTLOG(JP_WARN,'IGSIZE: No. of points = ',KNWE)
          CALL INTLOGR(JP_WARN,'IGSIZE: Stride = ',REAL(ILNSTR)/PPMULT)
        ENDIF
C
C       NCEP products for DEMETER are pseudo gaussian
C       (regular N48 in longitude direction, N47 in latitude direction)
C
        OWEGLOBE = KNWE.GE.(KGTRUNC*4)
C
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 4. Generate points along a line of meridian
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
      IF( LPRINT ) CALL INTLOG(JP_DEBUG,'IGSIZE: Section 4.',JPQUIET)
C
C     Generate full Gaussian latitudes for this truncation
C
      IERR = IGGLAT(ILATNUM, PGAUSS, KPR, KERR)
C
      IF( IERR.GT.0 ) THEN
        IGSIZE = IERR
        GOTO 900
      ENDIF
C
C     Find Gaussian lines corresponding to given North and South points
C
C     Number of lines not set (output field)
C
      IF( KNNS.EQ.0 ) THEN
        KLN = 0
        ILS = ILATNUM
C
        DO JLAT = 2, ILATNUM
          IGAUSS = NINT(PGAUSS(JLAT) * PPMULT)
C
          IF( (KLN.EQ.0).AND.((KAREA(JPNORTH)-IGAUSS).GT.1000) ) THEN
            KLN = JLAT - 1
            KAREA(JPNORTH) = NINT(PGAUSS(KLN) * PPMULT)
          ENDIF
C
          IF( (ILS.EQ.ILATNUM).AND.(KAREA(JPSOUTH).GE.IGAUSS) ) THEN
            ILS = JLAT
            KAREA(JPSOUTH) = NINT(PGAUSS(ILS) * PPMULT)
          ENDIF
        ENDDO
C
        KNNS = ILS - KLN + 1
C
      ELSE
C
C       Number of lines set (input field)
C
        KLN = 1
        ILS = 1
C
        INDIST = ABS(NINT(PGAUSS(1) * PPMULT) - KAREA(JPNORTH) )
        ISDIST = ABS(NINT(PGAUSS(1) * PPMULT) - KAREA(JPSOUTH) )
C
        DO JLAT = 2, ILATNUM
C
          IF( (ABS(NINT(PGAUSS(JLAT)*PPMULT) - KAREA(JPNORTH) ) )
     X         .LT.INDIST) THEN
            KLN    = JLAT
            INDIST = ABS(NINT(PGAUSS(JLAT) * PPMULT) - KAREA(JPNORTH) )
          ENDIF
C
          IF( (ABS(NINT(PGAUSS(JLAT)*PPMULT) - KAREA(JPSOUTH) ) )
     X         .LT.ISDIST) THEN
            ILS    = JLAT
            ISDIST = ABS(NINT(PGAUSS(JLAT) * PPMULT) - KAREA(JPSOUTH) )
          ENDIF
C
        ENDDO
C
        IF( KNNS.NE.(ILS - KLN + 1) ) THEN
          IGSIZE = JPROUTINE + 1
          IF( KERR.GE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X          'IGSIZE: No.of lines specified = ',KNNS)
            CALL INTLOG(JP_ERROR,
     X          'IGSIZE: does not match the',JPQUIET)
            CALL INTLOG(JP_ERROR,
     X          'IGSIZE: Gaussian line limits = ', KLN)
            CALL INTLOG(JP_ERROR,
     X          'IGSIZE: and ',ILS)
          ENDIF
          IF( KERR.EQ.0 ) CALL INTLOG(JP_FATAL,
     X        'Interpolation failed.', JPQUIET)
          GOTO 900
        ENDIF
C
      ENDIF
C
      ONPOLE = KLN.EQ.1
      OSPOLE = ILS.EQ.ILATNUM
C
      KAREA(JPNORTH) = NINT(PGAUSS(KLN) * PPMULT)
      KAREA(JPSOUTH) = NINT(PGAUSS(ILS) * PPMULT)
C
      IF( LPRINT ) THEN
        CALL INTLOG(JP_DEBUG,'IGSIZE: Output parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IGSIZE: No.long pts = ',KNWE)
        CALL INTLOG(JP_DEBUG,'IGSIZE: No.lat. pts = ',KNNS)
        CALL INTLOG(JP_DEBUG,'IGSIZE: Stride along lat = ',ILNSTR)
        CALL INTLOG(JP_DEBUG,'IGSIZE: North line number = ',KLN)
        CALL INTLOG(JP_DEBUG,'IGSIZE: South line number = ',ILS)
        IF( OWEGLOBE ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'IGSIZE: Field is global West-East.',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,
     X      'IGSIZE: Field is NOT global West-East.',JPQUIET)
        ENDIF
        IF( ONPOLE ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'IGSIZE: North Pole is in field.',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,
     X      'IGSIZE: North Pole NOT is in field.',JPQUIET)
        ENDIF
        IF( OSPOLE ) THEN
          CALL INTLOG(JP_DEBUG,
     X      'IGSIZE: South Pole is in field.',JPQUIET)
        ELSE
          CALL INTLOG(JP_DEBUG,
     X      'IGSIZE: South Pole NOT is in field.',JPQUIET)
        ENDIF
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9. Return to calling routine. Format statements
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      IF( LPRINT ) CALL INTLOG(JP_DEBUG,'IGSIZE: Section 9.',JPQUIET)
C
      RETURN
      END
