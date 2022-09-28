C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      PROGRAM TEST_INTERPOLATION
      IMPLICIT NONE


C     Parameters
      INTEGER JPGRIB                 ! GRIB size
C     PARAMETER (JPGRIB =  2000000)
C     PARAMETER (JPGRIB =  4284076)  ! for R640/O640 output
C     PARAMETER (JPGRIB =  7000000)  ! for INTUVP2 (requires 2 fields)
      PARAMETER (JPGRIB = 33190420)  ! for R1280/O1280 = 1/16 deg (INTUVP2 not tested)
      INTEGER JPBYTES                ! bytes/integer (8 on 64-bit architecture)
#ifdef INTEGER_8
      PARAMETER (JPBYTES = 8)
#else
      PARAMETER (JPBYTES = 4)
#endif
      LOGICAL IN, OUT
      PARAMETER (IN=.TRUE., OUT=.FALSE.)


C     Locals
      INTEGER R, I
      INTEGER IO ! 0:?, 1:INTIN, 2: INTOUT
      CHARACTER*256 ARG, KEY, VAL, F1, F2
      CHARACTER*1 N
      PARAMETER (N = CHAR(10))


C     Internals & externals
      LOGICAL SPLITARGS
      INTEGER WRAP_INT, WRAP_INTF2, WRAP_INTUVP2, WRAP_INTVECT2
      INTEGER IARGC


C     Initialize
      R = 0
      F1 = ''
      F2 = '/dev/null'


C     Process command line, in order
      DO I = 1, IARGC()
        CALL GETARG(I,ARG)
        R = -1
        IO = 0
        IF (SPLITARGS(ARG,IO,KEY,VAL)) THEN

          IF (IO.EQ.0) THEN
            IF (KEY.EQ.'input')    F1 = VAL
            IF (KEY.EQ.'output')   F2 = VAL
            IF (KEY.EQ.'input' .OR. KEY.EQ.'output') R=0
            IF (KEY.EQ.'intf2')    R=WRAP_INTF2   (F1,F2,JPGRIB,JPBYTES)
            IF (KEY.EQ.'intuvp2')  R=WRAP_INTUVP2 (F1,F2,JPGRIB,JPBYTES)
            IF (KEY.EQ.'intvect2') R=WRAP_INTVECT2(F1,F2,JPGRIB,JPBYTES)
          ELSEIF (IO.EQ.1) THEN
            IF (KEY.EQ.'accuracy')        R=WRAP_INT(IN, KEY,VAL,1,0)
            IF (KEY.EQ.'area')            R=WRAP_INT(IN, KEY,VAL,0,4)
            IF (KEY.EQ.'date')            R=WRAP_INT(IN, KEY,VAL,1,0)
            IF (KEY.EQ.'form')            R=WRAP_INT(IN, KEY,VAL,0,0)
            IF (KEY.EQ.'grid')            R=WRAP_INT(IN, KEY,VAL,0,2)
            IF (KEY.EQ.'gridname')        R=WRAP_INT(IN, KEY,VAL,0,0)
            IF (KEY.EQ.'level')           R=WRAP_INT(IN, KEY,VAL,1,0)
            IF (KEY.EQ.'local')           R=WRAP_INT(IN, KEY,VAL,1,0)
            IF (KEY.EQ.'lsm_param')       R=WRAP_INT(IN, KEY,VAL,0,0)
            IF (KEY.EQ.'lsmset')          R=WRAP_INT(IN, KEY,VAL,0,0)
            IF (KEY.EQ.'matrix')          R=WRAP_INT(IN, KEY,VAL,1,0)
            IF (KEY.EQ.'npts')            R=WRAP_INT(IN, KEY,VAL,2,0)
            IF (KEY.EQ.'parameter')       R=WRAP_INT(IN, KEY,VAL,1,0)
            IF (KEY.EQ.'reduced')         R=WRAP_INT(IN, KEY,VAL,1,0)
            IF (KEY.EQ.'regular')         R=WRAP_INT(IN, KEY,VAL,1,0)
            IF (KEY.EQ.'scan')            R=WRAP_INT(IN, KEY,VAL,1,0)
            IF (KEY.EQ.'table')           R=WRAP_INT(IN, KEY,VAL,1,0)
            IF (KEY.EQ.'truncation')      R=WRAP_INT(IN, KEY,VAL,1,0)
            IF (KEY.EQ.'uselsm')          R=WRAP_INT(IN, KEY,VAL,0,0)
            IF (KEY.EQ.'useprecip')       R=WRAP_INT(IN, KEY,VAL,0,0)
            IF (KEY.EQ.'usewind')         R=WRAP_INT(IN, KEY,VAL,0,0)
            IF (KEY.EQ.'user_reduced_gaussian'.OR.
     .          KEY.EQ.'user_regular_gaussian')
     .        R=WRAP_INT(IN, KEY,VAL,1,0)
          ELSEIF (IO.EQ.2) THEN
            IF (KEY.EQ.'12-point')        R=WRAP_INT(OUT,KEY,VAL,0,0)
            IF (KEY.EQ.'accuracy')        R=WRAP_INT(OUT,KEY,VAL,1,0)
            IF (KEY.EQ.'area')            R=WRAP_INT(OUT,KEY,VAL,0,4)
            IF (KEY.EQ.'autoresol')       R=WRAP_INT(OUT,KEY,VAL,1,0)
            IF (KEY.EQ.'bitmap')          R=WRAP_INT(OUT,KEY,VAL,0,0)
            IF (KEY.EQ.'form')            R=WRAP_INT(OUT,KEY,VAL,0,0)
            IF (KEY.EQ.'frame')           R=WRAP_INT(OUT,KEY,VAL,1,0)
            IF (KEY.EQ.'gauss_from_file') R=WRAP_INT(OUT,KEY,VAL,0,0)
            IF (KEY.EQ.'gaussian')        R=WRAP_INT(OUT,KEY,VAL,1,0)
            IF (KEY.EQ.'grid')            R=WRAP_INT(OUT,KEY,VAL,0,2)
            IF (KEY.EQ.'gridname')        R=WRAP_INT(OUT,KEY,VAL,0,0)
            IF (KEY.EQ.'interpolation')   R=WRAP_INT(OUT,KEY,VAL,0,0)
            IF (KEY.EQ.'level')           R=WRAP_INT(OUT,KEY,VAL,1,0)
            IF (KEY.EQ.'levtype')         R=WRAP_INT(OUT,KEY,VAL,1,0)
            IF (KEY.EQ.'packing')         R=WRAP_INT(OUT,KEY,VAL,0,0)
            IF (KEY.EQ.'parameter')       R=WRAP_INT(OUT,KEY,VAL,1,0)
            IF (KEY.EQ.'red_latlon')      R=WRAP_INT(OUT,KEY,VAL,0,2)
            IF (KEY.EQ.'reduced')         R=WRAP_INT(OUT,KEY,VAL,1,0)
            IF (KEY.EQ.'regular')         R=WRAP_INT(OUT,KEY,VAL,1,0)
            IF (KEY.EQ.'rotation')        R=WRAP_INT(OUT,KEY,VAL,0,2)
            IF (KEY.EQ.'scan')            R=WRAP_INT(OUT,KEY,VAL,1,0)
            IF (KEY.EQ.'specification')   R=WRAP_INT(OUT,KEY,VAL,1,0)
            IF (KEY.EQ.'style')           R=WRAP_INT(OUT,KEY,VAL,0,0)
            IF (KEY.EQ.'table')           R=WRAP_INT(OUT,KEY,VAL,1,0)
            IF (KEY.EQ.'truncation')      R=WRAP_INT(OUT,KEY,VAL,1,0)
            IF (KEY.EQ.'intermediate_gaussian'.OR.
     .          KEY.EQ.'user_reduced_gaussian'.OR.
     .          KEY.EQ.'user_reduced_latlon'  .OR.
     .          KEY.EQ.'user_regular_gaussian')
     .        R=WRAP_INT(OUT,KEY,VAL,1,0)
          ENDIF

        ENDIF
        IF (R.NE.0) THEN
          PRINT *, 'Problem with option "'//ARG(1:INDEX(ARG,' ')-1)//'"'
          PRINT *, '  key:   "'//KEY(1:INDEX(KEY,' ')-1)//'"'
          PRINT *, '  value: "'//VAL(1:INDEX(VAL,' ')-1)//'"'
          CALL EXIT(1)
        ENDIF
      ENDDO

      IF (F1(1:1).EQ.' ' .OR. F2(1:1).EQ.' ') THEN
  666     PRINT *,N
     .//'Usage: int --input=1.grib --output=2.grib [options] ...'//N
     .//'where in/output files and interpolation fn. are mandatory:'//N
     .//'  --input=C    input file'//N
     .//'  --output=C   output file (default: /dev/null)'//N
     .//'  --intf2:     general/MARS interpolation'//N
     .//'               (allows multiple GRIB input)'//N
     .//'  --intuvp2:   u/v fields interpolation'//N
     .//'               (requires vo/d or u/v pair on GRIB input)'//N
     .//'  --intvect2:  u/v fields interpolation'//N
     .//'               (requires u/v pair on GRIB input)'//N
     .//N
     .//'and any of the INTIN/OUT options, interpreted in order'//N
     .//'(some combinations are inconsistent):'//N
     .//'  --INTIN:truncation=I:   SH truncation (input)'//N
     .//'  --INTOUT:12-point=C:    12-point bilinear int. ([yes|no])'//N
     .//'  --INTOUT:accuracy=I:    pck. accuracy ([16|24|...)'//N
     .//'  --INTOUT:area=R/R/R/R:  geographical area (N/W/S/E)'//N
     .//'  --INTOUT:autoresol=I:   SH auto truncation on/off ([1|0])'//N
     .//'  --INTOUT:grid=R/R:          regular lat-lon, or'//N
     .//'  --INTOUT:gridname=[F|N|O]I: specific grid, with:'//N
     .//'      F: regular Gaussian'//N
     .//'      N: reduced Gaussian/"quasi-regular"'//N
     .//'      O: reduced Gaussian/"octahedral"'//N
     .//'      I: points between equator and poles (N)'//N
     .//'  --INTOUT:interpolation=C: interpol. method (ref. code)'//N
     .//'  --INTOUT:packing=C:     pck. method ([second|simple|...])'//N
     .//'  --INTOUT:reduced=I:     reduced Gaussian grid (alternate)'//N
     .//'  --INTOUT:regular=I:     regular Gaussian grid (alternate)'//N
     .//'  --INTOUT:rotation=R/R:  South pole new position (lat/lon)'//N
     .//'  --INTOUT:style=C:       style ([dissemination|...])'//N
     .//'  --INTOUT:truncation=I:  SH truncation (output)'//N
     .//N
     .//'with the following abbreviations:'//N
     .//'  I: integer number'//N
     .//'  R: real number'//N
     .//'  C: text string'//N
        STOP
      ENDIF


      STOP
      ENDPROGRAM


C     ------------------------------------------------------------------
      LOGICAL FUNCTION SPLITARGS(ARG,IN,KEY,VAL)
      IMPLICIT NONE
      CHARACTER*256 ARG, KEY, VAL
      INTEGER IN

      IN  = 0
      KEY = ''
      VAL = ''
      IF (ARG(1:2).EQ.'--') THEN 
        KEY = ARG(3:)
        IF (INDEX(ARG,'=').GT.3) VAL = ARG(INDEX(ARG,'=')+1:)
        IF (INDEX(ARG,'=').GT.3) KEY = ARG(3:INDEX(ARG,'=')-1)
      ENDIF
      IF (KEY(1:6).EQ.'INTIN:') THEN
        IN  = 1
        KEY = KEY(7:)
      ELSEIF (KEY(1:7).EQ.'INTOUT:') THEN
        IN  = 2
        KEY = KEY(8:)
      ENDIF
      SPLITARGS = (INDEX(KEY,' ').GT.1)
      ENDFUNCTION


C     ------------------------------------------------------------------
      INTEGER FUNCTION WRAP_INT(IN,K,C,NI,NR)
      IMPLICIT NONE
      LOGICAL IN
      CHARACTER*256 K, C
      INTEGER NI, NR

      CHARACTER*256 M
      INTEGER I(4)
      REAL R(4)

      INTEGER INTIN, INTOUT
      EXTERNAL INTIN, INTOUT

      I = 0
      R = 0.
      IF (IN) THEN
        M = 'intin'
      ELSE
        M = 'intout'
      ENDIF
      M = M(1:INDEX(M,' ')-1)//'("'//K(1:INDEX(K,' ')-1)//'")'
      IF (NI.LT.0 .OR. NI.GT.4) NI = 0
      IF (NR.LT.0 .OR. NR.GT.4) NR = 0
      IF (NI.GE.1) THEN
        DO WHILE (INDEX(C,'/').GT.0)
          C(INDEX(C,'/'):INDEX(C,'/')) = ' '
        ENDDO
        READ (C,*) I(1:NI)
        C = ''
        PRINT *, M(1:INDEX(M,' ')-1), I(1:NI)
      ELSEIF (NR.GE.1) THEN
        DO WHILE (INDEX(C,'/').GT.0)
          C(INDEX(C,'/'):INDEX(C,'/')) = ' '
        ENDDO
        READ (C,*) R(1:NR)
        C = ''
        PRINT *, M(1:INDEX(M,' ')-1), R(1:NR)
      ELSE
        PRINT *, M(1:INDEX(M,' ')-1), ' "'//C(1:INDEX(C,' ')-1)//'"'
      ENDIF
      IF (IN) THEN
        WRAP_INT = INTIN (K,I,R,C)
      ELSE
        WRAP_INT = INTOUT(K,I,R,C)
      ENDIF
      IF (WRAP_INT.NE.0) PRINT *, M(1:INDEX(M,' ')-1)//' failed.'
      ENDFUNCTION


C     ------------------------------------------------------------------
      INTEGER FUNCTION WRAP_INTF2(INFILE,OUTFILE,LENGRIB,LENBYTES)
      IMPLICIT NONE
      CHARACTER*256 INFILE, OUTFILE
      INTEGER LENGRIB, LENBYTES

      INTEGER INGRIB(LENGRIB), NEWFLD(LENGRIB)
      INTEGER U1, U2, N, LENINT, IREC, IRET

      INTEGER INTF2
      EXTERNAL INTF2

C     Initialize
      INGRIB = 0
      NEWFLD = 0
      CALL EXCEPT()

C     Open input and output files
      IRET = 0
      CALL PBOPEN(U1, INFILE, 'r', IRET)
      CALL CHECK(IRET.NE.0, 'PBOPEN failed (r).')
      CALL PBOPEN(U2, OUTFILE, 'w', IRET)
      CALL CHECK(IRET.NE.0, 'PBOPEN failed (w).')

C     Start of loop on input fields
      N = 0
  120 CONTINUE
      N = N + 1

C     Read next field
      CALL PBGRIB(U1, INGRIB, LENGRIB*LENBYTES, IREC, IRET)
      IF (IRET.EQ.-1) THEN
        IRET = 0
        GOTO 190
      ENDIF
      CALL CHECK(IRET.NE.0, 'PBGRIB failed.')

C     Interpolate
      PRINT *, 'Interpolate field #', N
      LENINT = LENGRIB
      IRET = INTF2(INGRIB,IREC,NEWFLD,LENINT)
      CALL CHECK(IRET.NE.0, 'INTF2 failed')

C     Write the new field to file
      IF (LENINT.GT.0) THEN
        CALL PBWRITE(U2, NEWFLD, LENINT, IRET)
      ELSE
        PRINT *, 'Output same as input'
        CALL PBWRITE(U2, INGRIB, IREC, IRET)
      ENDIF
      CALL CHECK(IRET.LT.0, 'PBWRITE failed')
      IRET = 0

C     Loop back for next field
      IF (IRET.EQ.0) GOTO 120

C     Closedown
  190 CONTINUE
      WRAP_INTF2 = IRET
      CALL PBCLOSE(U1, IRET)
      CALL PBCLOSE(U2, IRET)
      PRINT *, 'All done.'
      ENDFUNCTION


C     ------------------------------------------------------------------
      INTEGER FUNCTION WRAP_INTUVP2(INFILE,OUTFILE,LENGRIB,LENBYTES)
      IMPLICIT NONE
      CHARACTER*256 INFILE, OUTFILE
      INTEGER LENGRIB, LENBYTES

      INTEGER IVOGRIB(LENGRIB), IDVGRIB(LENGRIB)
      INTEGER IUGRIB(LENGRIB), IVGRIB(LENGRIB)
      INTEGER U1, U2, N, LENINT, IREC, IRET

      INTEGER INTUVP2
      EXTERNAL INTUVP2

C     Open input and output files
      CALL PBOPEN(U1, INFILE, 'r', IRET)
      CALL CHECK(IRET.NE.0, 'PBOPEN failed (r).')
      CALL PBOPEN(U2, OUTFILE, 'w', IRET)
      CALL CHECK(IRET.NE.0, 'PBOPEN failed (w).')

C     Start of loop on input vo/d fields
      N = 0
  220 CONTINUE
      N = N + 1

C     Read next vo/d
      CALL PBGRIB(U1, IVOGRIB, LENGRIB*LENBYTES, IREC, IRET)
      IF (IRET.EQ.-1) THEN
        IRET = 0
        GOTO 290
      ENDIF
      CALL CHECK(IRET.NE.0, 'PBGRIB failed (vo).')
      CALL PBGRIB(U1, IDVGRIB, LENGRIB*LENBYTES, IREC, IRET)
      IF (IRET.EQ.-1) THEN
        IRET = 0
        GOTO 290
      ENDIF
      CALL CHECK(IRET.NE.0, 'PBGRIB failed (d).')

C     Interpolate
      PRINT *, 'Interpolate vo/d > u/v #', N
      LENINT = LENGRIB
      IRET = INTUVP2(IVOGRIB, IDVGRIB, LENGRIB, IUGRIB, IVGRIB, LENINT)
      CALL CHECK(IRET.NE.0, 'INTUVP2 failed.')

C     Write the new u/v fields to file
      IF (LENINT.GT.0) THEN
        CALL PBWRITE(U2, IUGRIB, LENINT, IRET)
        CALL CHECK(IRET.LT.LENINT, 'PBWRITE failed.')
        CALL PBWRITE(U2, IVGRIB, LENINT, IRET)
        CALL CHECK(IRET.LT.LENINT, 'PBWRITE failed.')
        IRET = 0
      ENDIF

C     Loop back for next field
      IF (IRET.EQ.0) GOTO 220

C     Closedown
  290 CONTINUE
      WRAP_INTUVP2 = IRET
      CALL PBCLOSE(U1, IRET)
      CALL PBCLOSE(U2, IRET)
      PRINT *, 'All done.'
      ENDFUNCTION


C     ------------------------------------------------------------------
      INTEGER FUNCTION WRAP_INTVECT2(INFILE,OUTFILE,LENGRIB,LENBYTES)
      IMPLICIT NONE
      CHARACTER*256 INFILE, OUTFILE
      INTEGER LENGRIB, LENBYTES

      INTEGER IIAGRIB(LENGRIB), IIBGRIB(LENGRIB)
      INTEGER IOAGRIB(LENGRIB), IOBGRIB(LENGRIB)
      INTEGER U1, U2, N, LENINT, IREC, IRET

      INTEGER INTVECT2
      EXTERNAL INTVECT2

C     Open input and output files
      CALL PBOPEN(U1, INFILE, 'r', IRET)
      CALL CHECK(IRET.NE.0, 'PBOPEN failed (r).')
      CALL PBOPEN(U2, OUTFILE, 'w', IRET)
      CALL CHECK(IRET.NE.0, 'PBOPEN failed (w).')

C     Start of loop on input u/v fields
      N = 0
  220 CONTINUE
      N = N + 1

C     Read next u/v
      CALL PBGRIB(U1, IIAGRIB, LENGRIB*LENBYTES, IREC, IRET)
      IF (IRET.EQ.-1) THEN
        IRET = 0
        GOTO 290
      ENDIF
      CALL CHECK(IRET.NE.0, 'PBGRIB failed (u).')
      CALL PBGRIB(U1, IIBGRIB, LENGRIB*LENBYTES, IREC, IRET)
      IF (IRET.EQ.-1) THEN
        IRET = 0
        GOTO 290
      ENDIF
      CALL CHECK(IRET.NE.0, 'PBGRIB failed (v).')

C     Interpolate
      PRINT *, 'Interpolate u/v #', N
      LENINT = LENGRIB
      IRET = INTVECT2(IIAGRIB,IIBGRIB,LENGRIB,IOAGRIB,IOBGRIB,LENINT)
      CALL CHECK(IRET.NE.0, 'INTVECT2 failed.')

C     Write the new u/v fields to file
      IF (LENINT.GT.0) THEN
        CALL PBWRITE(U2, IOAGRIB, LENINT, IRET)
        CALL CHECK(IRET.LT.LENINT, 'PBWRITE failed.')
        CALL PBWRITE(U2, IOBGRIB, LENINT, IRET)
        CALL CHECK(IRET.LT.LENINT, 'PBWRITE failed.')
        IRET = 0
      ENDIF

C     Loop back for next field
      IF (IRET.EQ.0) GOTO 220

C     Closedown
  290 CONTINUE
      WRAP_INTVECT2 = IRET
      CALL PBCLOSE(U1, IRET)
      CALL PBCLOSE(U2, IRET)
      PRINT *, 'All done.'
      ENDFUNCTION


C     ------------------------------------------------------------------
      SUBROUTINE CHECK(OOPS,MSG)
      IMPLICIT NONE
      LOGICAL OOPS
      CHARACTER MSG*(*)
      IF (OOPS) THEN
        PRINT *, MSG(1:INDEX(MSG,' ')-1)
        CALL EXIT(3)
      ENDIF
      ENDSUBROUTINE

