C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C


      SUBROUTINE QU2REG (PFIELD,KPOINT,KLAT,KLON,KCODE)
C
C---->
C**** QU2REG - Convert quasi-regular grid data to regular.
C
C     Purpose.
C     --------
C
C           Convert quasi-regular grid data to regular,
C           using either a linear or cubic interpolation.
C
C**   Interface.
C     ----------
C
C           CALL QU2REG (PFIELD,KPOINT,KLAT,KLON,KCODE)
C
C           Integer    K.
C           Real       P.
C           Logical    O.
C           Character  H.
C
C               Input Parameters.
C               -----------------
C
C               PFIELD     - Array containing quasi-regular grid
C                            data.
C
C               KPOINT     - Array containing list of the number of
C                            points on each latitude (or longitude) of
C                            the quasi-regular grid.
C
C               KLAT       - Number of latitude lines
C
C               KLON       - Number of longitude lines
C
C               KCODE      - Interpolation required.
C                            1 , linear - data quasi-regular on
C                                         latitude lines.
C                            3 , cubic - data quasi-regular on
C                                         latitude lines.
C                            11, linear - data quasi-regular on
C                                         longitude lines.
C                            13, cubic - data quasi-regular on
C                                         longitude lines.
C
C               Output Parameters.
C               ------------------
C
C               PFIELD     - Array containing regular grid data.
C
C     Method.
C     -------
C
C           Data is interpolated and expanded into a temporary array,
C           which is then copied back into the user's array.
C           Routine aborts if an invalid interpolation is requested or
C           field size exceeds array dimensions.
C
C     Externals.
C     ----------
C
C           ROWINA
C           ABORTX
C
C     Reference.
C     ----------
C
C           WMO Manual on Codes for GRIB code specifications of
C           quasi-regular grids.
C
C     Comments.
C     ---------
C
C           This routine is an adaptation of INTPGRR and runs
C           on the Cray only.
C
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      18.06.91
C
C     Modifications.
C     --------------
C
C           J. Hennessy      ECMWF      08.10.91
C           Call to ROWINT changed to call to ROWINA.
C           ROWINA called if only number of values required is not
C           the same as the input number.
C
C           J. Hennessy      ECMWF      07.01.92
C           Call to ABORT changed to ABORTX.
C
C     -----------------------------------------------------------------
C----<
C
C
C
C
C
C
C
C
C
C
C*    Section 0. Definition of variables. Data statements.
C     -----------------------------------------------------------------
C
C*    Prefix conventions for variable names.
C
C     Logical      L (but not LP), global or common.
C                  O, dummy arguments.
C                  G, local variable.
C                  LP, parameter.
C     Character    C, Global or common.
C                  H, dummy arguments.
C                  Y (but not YP), local variables.
C                  YP, parameter.
C     Integer      M and N, global or common.
C                  K, dummy arguments.
C                  I, local variables.
C                  J (but not JP), loop control.
C                  JP, parameter.
C     Real         A to F and Q to X, global or common.
C                  P (but not PP), dummy arguments.
C                  Z, local variables.
C                  PP, parameter.
C
#ifdef CRAY

      IMPLICIT NONE
#endif

C
      INTEGER ICODE
      INTEGER ILII
      INTEGER ILIO
      INTEGER IQUANO
      INTEGER IREGNO
C
      INTEGER JPMAX
C
      INTEGER J210
      INTEGER J220
      INTEGER J225
      INTEGER J230
      INTEGER J240
C
      INTEGER KCODE
      INTEGER KLAT
      INTEGER KLON
      INTEGER KPOINT
C
      REAL    PFIELD
C
      REAL    ZLINE
      REAL    ZTEMP
      REAL    ZWORK
C
C     Maximum number of latitudes (or longitudes), for which arrays
C     are dimensioned.
C
      PARAMETER (JPMAX=320)
C
      DIMENSION PFIELD(*)
      DIMENSION ZTEMP(JPMAX*JPMAX*2)
      DIMENSION ZLINE(JPMAX*2)
      DIMENSION ZWORK(0:JPMAX*2+2,3)
C
      DIMENSION KPOINT(*)
C
C     ------------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C
C*    Section 1. Set initial values.
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
C     Check input parameters.
C
      IF (KCODE.NE.1.AND.KCODE.NE.3.AND.
     C    KCODE.NE.11.AND.KCODE.NE.13)
     C   THEN
             WRITE (*,9001) KCODE
             CALL ABORTX ('QU2REG')
         ENDIF
C
      IF (KLAT.GT.JPMAX)
     C   THEN
             WRITE (*,9002) KLAT , JPMAX
             CALL ABORTX ('QU2REG')
         ENDIF
C
      IF (KLON.GT.JPMAX*2)
     C   THEN
             WRITE (*,9003) KLAT , JPMAX*2
             CALL ABORTX ('QU2REG')
         ENDIF
C
C     Set array indices to 0.
C
      ILII  = 0
      ILIO  = 0
C
C     Establish values of loop parameters.
C
      IF (KCODE.GT.10)
     C   THEN
C
C            Quasi-regular along longitude lines.
C
             IQUANO = KLON
             IREGNO = KLAT
             ICODE  = KCODE - 10
         ELSE
C
C            Quasi-regular along latitude lines.
C
             IQUANO = KLAT
             IREGNO = KLON
             ICODE  = KCODE
         ENDIF
C
C     ------------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C
C*    Section 2. Interpolate field from quasi to regular grid.
C     ------------------------------------------------------------------
C
  200 CONTINUE
C
      DO 230 J230=1,IQUANO
C
         IF (IREGNO.NE.KPOINT(J230))
     C      THEN
C
C               Line contains less values than required,so
C               extract quasi-regular grid values for a line
C
                DO 210 J210=1,KPOINT(J230)
                   ILII        = ILII+1
                   ZLINE(J210) = PFIELD(ILII)
  210           CONTINUE
C
C               and interpolate this line.
C
                CALL ROWINA (ZLINE,IREGNO,KPOINT(J230),ZWORK,ICODE)
C
C               Add regular grid values for this line to the temporary
C               array.
C
                DO 220 J220=1,IREGNO
                   ILIO        = ILIO+1
                   ZTEMP(ILIO) = ZLINE(J220)
  220           CONTINUE
C
            ELSE
C
C               Line contains the required number of values, so add
C               this line to the temporary array.
C
                DO 225 J225=1,IREGNO
                   ILIO        = ILIO+1
                   ILII        = ILII+1
                   ZTEMP(ILIO) = PFIELD(ILII)
  225           CONTINUE
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
C
C
C
C
C
C
C
C
C
C
C*    Section 9. Return to calling routine. Format statements.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
 9001 FORMAT (1H ,'QU2REG : Invalid interpolation type code = ',I3)
C
 9002 FORMAT (1H ,'QU2REG : Number of latitudes is ',I4,', maximum ',
     C                      'allowed is ',I3,'.')
C
 9003 FORMAT (1H ,'QU2REG : Number of longitudes is ',I4,', maximum ',
     C                      'allowed is ',I3,'.')
C
      RETURN
C
      END
