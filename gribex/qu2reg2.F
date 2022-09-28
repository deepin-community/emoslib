C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE QU2REG2(PFIELD,KPOINT,KLAT,KLON,KCODE,PMSVAL,KRET)
C
C---->
C**** QU2REG - Convert quasi-regular grid data to regular.
C
C     Purpose.
C     --------
C
C     Convert quasi-regular grid data to regular,
C     using either a linear or cubic interpolation.
C
C
C**   Interface.
C     ----------
C
C     CALL QU2REG2(PFIELD,KPOINT,KLAT,KLON,KCODE,PMSVAL)
C
C
C     Input Parameters.
C     -----------------
C
C     PFIELD     - Array containing quasi-regular grid
C                  data.
C
C     KPOINT     - Array containing list of the number of
C                  points on each latitude (or longitude) of
C                  the quasi-regular grid.
C
C     KLAT       - Number of latitude lines
C
C     KLON       - Number of longitude lines
C
C     KCODE      - Interpolation required.
C                  1 , linear - data quasi-regular on
C                               latitude lines.
C                  3 , cubic -  data quasi-regular on
C                               latitude lines.
C                  11, linear - data quasi-regular on
C                               longitude lines.
C                  13, cubic -  data quasi-regular on
C                               longitude lines.
C
C     PMSVAL     - Value used for missing data indicator.
C
C
C     Output Parameters.
C     ------------------
C
C     KRET       - return code
C                  0 = OK
C                  non-zero indicates fatal error
C
C
C     Output Parameters.
C     ------------------
C
C     PFIELD     - Array containing regular grid data.
C
C
C     Method.
C     -------
C
C     Data is interpolated and expanded into a temporary array,
C     which is then copied back into the user's array.
C     Returns an error code if an invalid interpolation is requested
C     or field size exceeds array dimensions.
C
C
C     Externals.
C     ----------
C
C     ROWINA2
C
C
C     Reference.
C     ----------
C
C     WMO Manual on Codes for GRIB code specifications of
C     quasi-regular grids.
C
C
C     Comments.
C     ---------
C
C     This routine is an adaptation of QU2REG to allow missing data
C     values, and hence bit mapped fields.
C
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      22.07.94
C
C
C     Modifications.
C     --------------
C
C     J.D.Chambers     ECMWF      13.09.94
C     Add return code KRET and remove calls to ABORT.
C
C     J.D.Chambers     ECMWF        Feb 1997
C     Allow for 64-bit pointers
C
C
C----<
C     -----------------------------------------------------------------
C*    Section 0. Definition of variables. Data statements.
C     -----------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER JPBYTES, JPMAX
#ifdef REAL_8
      PARAMETER (JPBYTES = 8)
#else
      PARAMETER (JPBYTES = 4)
#endif
      PARAMETER (JPMAX=721)
C     Maximum number of latitudes (or longitudes), for which arrays
C     are dimensioned.
C
C     Subroutine arguments
C
      REAL PFIELD, PMSVAL
      DIMENSION PFIELD(*)
      INTEGER KPOINT, KLAT, KLON, KCODE, KRET
      DIMENSION KPOINT(*)
C
C     Local variables.
C
      INTEGER ICODE, ILII, ILIO, IQUANO, IREGNO
      INTEGER J210, J220, J225, J230, J240
C
      REAL ZLINE, ZWORK
      DIMENSION ZLINE(JPMAX*2)
      DIMENSION ZWORK(0:JPMAX*2+2,3)
C
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IZTEMP
#endif
#endif
      REAL ZTEMP
      DIMENSION ZTEMP(1)
      POINTER ( IZTEMP, ZTEMP )
      INTEGER ISIZE
      SAVE ISIZE, IZTEMP
C
C     Externals
C
#ifdef POINTER_64
      INTEGER*8 JMALLOC
#else
      INTEGER JMALLOC
#endif
      EXTERNAL JMALLOC
C
      DATA ISIZE/0/
C
C     ------------------------------------------------------------------
C*    Section 1. Set initial values.
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
      KRET = 0
C
C     Check input parameters.
C
      IF (KCODE.NE.1.AND.KCODE.NE.3.AND.KCODE.NE.11.AND.KCODE.NE.13)
     X  THEN
        WRITE (*,9001) KCODE
        KRET = 1
        GOTO 900
      ENDIF
C
      IF (KLAT.GT.JPMAX) THEN
        WRITE (*,9002) KLAT , JPMAX
        KRET = 2
        GOTO 900
      ENDIF
C
      IF (KLON.GT.JPMAX*2) THEN
        WRITE (*,9003) KLAT , JPMAX*2
        KRET = 3
        GOTO 900
      ENDIF
C
C     Set array indices to 0.
C
      ILII  = 0
      ILIO  = 0
C
C     Establish values of loop parameters.
C
      IF (KCODE.GT.10) THEN
C
C       Quasi-regular along longitude lines.
C
        IQUANO = KLON
        IREGNO = KLAT
        ICODE  = KCODE - 10
      ELSE
C
C       Quasi-regular along latitude lines.
C
        IQUANO = KLAT
        IREGNO = KLON
        ICODE  = KCODE
      ENDIF
C
C     Allocate memory (first time only)
      IF( ISIZE.EQ.0 ) THEN
        ISIZE = (JPMAX*JPMAX*2)*JPBYTES
        IZTEMP = JMALLOC(ISIZE)
#ifdef hpR64
        IZTEMP = IZTEMP/(1024*1024*1024*4)
#endif
        IF( IZTEMP.EQ.0 ) THEN
          WRITE(*,*) 'QU2REG2: Memory allocation failed.'
          WRITE(*,*) 'QU2REG2: Number of bytes required = ', ISIZE
          KRET = 5
          GOTO 900
        ENDIF
      ENDIF
C
C     ------------------------------------------------------------------
C*    Section 2. Interpolate field from quasi to regular grid.
C     ------------------------------------------------------------------
C
  200 CONTINUE
C
      DO 230 J230=1,IQUANO
C
        IF (IREGNO.NE.KPOINT(J230)) THEN
C
C         Line contains less values than required,so
C         extract quasi-regular grid values for a line
C
          DO 210 J210=1,KPOINT(J230)
            ILII        = ILII+1
            ZLINE(J210) = PFIELD(ILII)
  210     CONTINUE
C
C         and interpolate this line.
C
          CALL ROWINA2( ZLINE, IREGNO, KPOINT(J230), ZWORK, ICODE,
     X                  PMSVAL, KRET)
          IF ( KRET .NE. 0 ) GOTO 900
C
C         Add regular grid values for this line to the temporary array.
C
          DO 220 J220=1,IREGNO
            ILIO        = ILIO+1
            ZTEMP(ILIO) = ZLINE(J220)
  220     CONTINUE
C
        ELSE
C
C         Line contains the required number of values, so add
C         this line to the temporary array.
C
          DO 225 J225=1,IREGNO
            ILIO        = ILIO+1
            ILII        = ILII+1
            ZTEMP(ILIO) = PFIELD(ILII)
  225     CONTINUE
C
        ENDIF
C
  230 CONTINUE
C
C     Copy temporary array to user array.
C
      DO 240 J240=1,KLON*KLAT
        PFIELD(J240) = ZTEMP(J240)
  240 CONTINUE
C
C     ------------------------------------------------------------------
C*    Section 9. Return to calling routine. Format statements.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
C
 9001 FORMAT (1H ,'QU2REG : Invalid interpolation type code = ',I3)
C
 9002 FORMAT (1H ,'QU2REG : Number of latitudes is ',I4,', maximum ',
     C                      'allowed is ',I3,'.')
C
 9003 FORMAT (1H ,'QU2REG : Number of longitudes is ',I4,', maximum ',
     C                      'allowed is ',I3,'.')
C
      END
