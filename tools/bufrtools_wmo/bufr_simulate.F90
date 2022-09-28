      PROGRAM BUFR_SIMULATE
!
!**** *BUFR_SIMULATE*
!
!
!     PURPOSE.
!     --------
!        PACKS SIMULATED OBSERVATIONS INTO BUFR FORM.
!
!
!**   INTERFACE.
!     ----------
!
!          NONE.
!
!     METHOD.
!     -------
!
!          INPUT FILE CONTAING SIMULATED DATA IN SPECIFIC
!          FORMAT CREATED BY DATA ASSIMILATION IS READ AND
!          CONVERTED INTO CORRESPONDING BUFR TEMPLATES FOR 
!          OBSERVATIONS ADDING QUALITY CONTROL INFORMATION.
!          
!
!     EXTERNALS.
!     ----------
!          BUFR SOFTWARE FROM EMOSLIB
!
!     REFERENCE.
!     ----------
!
!          NONE.
!
!     AUTHOR.
!     -------
!
!          M. DRAGOSAVAC    *ECMWF*       MAY 1996
!
!
!     MODIFICATIONS.
!     --------------
!
      IMPLICIT NONE

      INTEGER, PARAMETER :: JSUP=9
      INTEGER, PARAMETER :: JSEC0=3
      INTEGER, PARAMETER :: JSEC1=40
      INTEGER, PARAMETER :: JSEC2=4096
      INTEGER, PARAMETER :: JSEC3=4
      INTEGER, PARAMETER :: JSEC4=2
      INTEGER, PARAMETER :: JELEM=160000
      INTEGER, PARAMETER :: JBUFL=4000
#ifdef JBPW_64
      INTEGER, PARAMETER :: JBPW=64
#else
      INTEGER, PARAMETER :: JBPW=32
#endif
      INTEGER, PARAMETER :: JWORK=4096000

      INTEGER, PARAMETER :: KELEM=10000
      INTEGER, PARAMETER :: KVALS=360000


      INTEGER, DIMENSION(JBUFL) :: KBUFR
      INTEGER, DIMENSION(JSUP)  :: KSUP
      INTEGER, DIMENSION(JSEC0) :: KSEC0
      INTEGER, DIMENSION(JSEC1) :: KSEC1
      INTEGER, DIMENSION(JSEC2) :: KSEC2
      INTEGER, DIMENSION(JSEC3) :: KSEC3
      INTEGER, DIMENSION(JSEC4) :: KSEC4
      
      REAL*8,    DIMENSION(KVALS) :: VALUES
      REAL*8                         RVIND
      REAL*8                         EPS

      INTEGER  NBYTPW
      INTEGER  IRET
      INTEGER  HEADER_LENGTH
      INTEGER  LENGTH
      INTEGER  TYPE
      INTEGER  SUBTYPE
      INTEGER  LATITUDE
      INTEGER  LONGITUDE
      INTEGER  STATION_ALTITUDE
      INTEGER  REPORT_DATE
      INTEGER  REPORT_TIME
      INTEGER  NUMBER_OF_LEVELS
      INTEGER  REPORT_QC
      INTEGER  REPORT_BODY_LENGTH
      INTEGER  PRESSURE
      INTEGER  MSL_PRESSURE
      INTEGER  WIND_DIRECTION
      INTEGER  WIND_SPEED
      INTEGER  GEOPOTENTIAL_HEIGHT
      INTEGER  TEMPERATURE
      INTEGER  DEW_POINT_TEMPERATURE
      INTEGER  PRECIPITABLE_WATER
      INTEGER  RADIANCES
      INTEGER  SIGMA_0,LEVELS
      INTEGER  NUMBER_OF_RECORDS
      INTEGER  OFFSET
      INTEGER  JCB
      INTEGER  II
      INTEGER  JJ 
      INTEGER  IUNIT,I,JB,IO,KTDLEN,KDLEN,ILEN,KBUFL

      REAL     ANALYSIS_DATA,ANALYSIS_TIME

      REAL, DIMENSION(10)  :: HEADER,BODY
      REAL, DIMENSION(1360):: REPORT

 
      INTEGER, DIMENSION(JELEM) :: KTDLST
      INTEGER, DIMENSION(JELEM) :: KTDEXP
      INTEGER, DIMENSION(200)   :: KDATA
 
      CHARACTER(256)  CF,CF1,COUTFILE,CINFILE
      CHARACTER(256), DIMENSION(4)     ::  CARG
      CHARACTER(80),  DIMENSION(KVALS) ::  CVALS
      
      INTEGER  NARG,J
      INTEGER IARGC
 
!s      EXTERNAL GETARG


!          1. INITIALIZE CONSTANTS 
!              -------------------
 
!     MISSING VALUE INDICATOR
 
      RVIND=1.7D38
      EPS=10.D-8
      NBYTPW=JBPW/8
 
!          2. GET INPUT AND OUTPUT FILE NAME.
!             -------------------------------
 
      NARG=IARGC()
 
      IF(NARG < 4) THEN
         PRINT*,'USAGE -- bufr_simulate -i infile -o outfile'
         STOP
      END IF
 
      DO  J=1,NARG
        CALL GETARG(J,CARG(J))
      END DO
 
      DO J=1,NARG,2
        IF(CARG(J).EQ.'-i') THEN
           CINFILE=CARG(J+1)
        ELSEIF(CARG(J).EQ.'-o') THEN
           COUTFILE=CARG(J+1)
        ELSE
            PRINT*,'USAGE -- bufr_simulate -i infile -o outfile'
            STOP
        END IF
      END DO
 
      II=LEN_TRIM(CINFILE)
      JJ=LEN_TRIM(COUTFILE)

 
!           3.  OPEN INPUT AND OUTPUT FILE.
!               ---------------------------
 
      OPEN(UNIT=9,FILE=CINFILE(1:II),IOSTAT=IO)
      IF(IO /= 0) THEN
         PRINT*,'OPEN FAILED ON INPUT FILE'
         STOP
      END IF
      
      IRET=0 
      CALL PBOPEN(IUNIT,COUTFILE(1:JJ),'W',IRET)
      IF(IRET.EQ.-1) STOP 'OPEN FAILED ON '
      IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'


      LENGTH=1
      HEADER_LENGTH=10
      TYPE=2
      SUBTYPE=3
      LATITUDE=4
      LONGITUDE=5
      STATION_ALTITUDE=6
      REPORT_DATE=7           ! YYYYMMDD
      REPORT_TIME=8           ! HHMMSS
      NUMBER_OF_LEVELS=9
      REPORT_QC=10
      REPORT_BODY_LENGTH=10
      PRESSURE=1
      MSL_PRESSURE=2
      WIND_DIRECTION=3
      WIND_SPEED=4
      GEOPOTENTIAL_HEIGHT=5
      TEMPERATURE=6
      DEW_POINT_TEMPERATURE=7
      PRECIPITABLE_WATER=8
      RADIANCES=9 
      SIGMA_0=10

!            4. READ ANALYSIS DATE AND TIME
!               ---------------------------

      READ(9,*,IOSTAT=IO) ANALYSIS_DATA,ANALYSIS_TIME

!            5.  READ HEADER AND THE BODY
!                ------------------------


      IO = 0
      DO WHILE (IO == 0 )

        READ(9,*,IOSTAT=IO) (HEADER(I),I=1,HEADER_LENGTH)
        IF(IO == 0 ) THEN
           LEVELS=NINT(HEADER(NUMBER_OF_LEVELS))
           DO I=1,HEADER_LENGTH
             REPORT(I)=HEADER(I)
           END DO

           NUMBER_OF_RECORDS=NUMBER_OF_RECORDS + 1


!             5.1  GET LEVEL DATA
!                   --------------

           OFFSET=HEADER_LENGTH - REPORT_BODY_LENGTH

           DO J=1,LEVELS
             OFFSET = OFFSET + REPORT_BODY_LENGTH
             READ(9,*,IOSTAT=IO) (BODY(JB),JB=1,REPORT_BODY_LENGTH)
             IF(IO == 0) THEN
                DO JCB=1,REPORT_BODY_LENGTH
                  REPORT(OFFSET+JCB)=BODY(JCB)
                END DO
             END IF
           END DO


!             5.2   PACK REPORT INTO BUFR MESSAGE
!                    ------------------------------

           IRET=0 
           CALL BUFR_PACK(REPORT,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,  &
                          KTDLEN,KTDLST,KDLEN,KDATA,KELEM,KVALS, &
                          VALUES,CVALS,IRET)


           KBUFL=JBUFL
           IF(IRET == 0 ) THEN
              CALL BUFREN(KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,   &
                          KTDLEN,KTDLST,KDLEN,KDATA,KELEM, &
                          KVALS,VALUES,CVALS,KBUFL,KBUFR,IRET)
              IF(IRET == 0) THEN


!             5.3   WRITE DATA INTO TARGET FILE
!                   ---------------------------

                 ILEN=KBUFL*NBYTPW

                 IRET=0
                 CALL PBWRITE(IUNIT,KBUFR,ILEN,IRET)
                 IF(IRET.LT.0) THEN
                    PRINT*,'ERROR WRITING INTO TARGET FILE.'
                    CALL EXIT(2)
                 END IF
              ELSE
!                 CALL EXIT(2)
              END IF
           END IF
        END IF
      END DO

      PRINT*,' '
      PRINT*,'NUMBER OF RECORDS PROCESSED',NUMBER_OF_RECORDS

      CONTAINS


      SUBROUTINE BUFR_PACK(REPORT,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,  &
                          KTDLEN,KTDLST,KDLEN,KDATA,KELEM,KVALS,  & 
                          VALUES,CVALS,IRET)
!**** *BUFR_PACK*
!
!
!     PURPOSE.
!     --------
!
!           CREATES BUFR MESSAGE.
!
!
!**   INTERFACE.
!     ----------
!
!           CALL  BUFR_PACK(REPORT,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,
!                           KTDLEN,KTDLST,KDLEN,KDATA,KELEM,KVALS,
!                           VALUES,CVALS,IRET)
!     METHOD.
!     -------
!
!
!           NONE.
!
!
!     EXTERNALS.
!     ----------
!
!           NONE.
!
!     REFERENCE.
!     ----------
!
!
!
!     AUTHOR.
!     -------
!
!          M. DRAGOSAVAC    *ECMWF*       MAY 1996
!
!
!     MODIFICATIONS.
!     --------------
!
!          NONE.
!---------------------------------------------------------------------------

      IMPLICIT NONE


      REAL,        DIMENSION(:), INTENT(INOUT)   :: REPORT
      INTEGER,     DIMENSION(:), INTENT(INOUT)   :: KSEC0
      INTEGER,     DIMENSION(:), INTENT(INOUT)   :: KSEC1
      INTEGER,     DIMENSION(:), INTENT(INOUT)   :: KSEC2
      INTEGER,     DIMENSION(:), INTENT(INOUT)   :: KSEC3
      INTEGER,     DIMENSION(:), INTENT(INOUT)   :: KSEC4
      INTEGER,                   INTENT(INOUT)   :: KTDLEN
      INTEGER,     DIMENSION(:), INTENT(INOUT)   :: KTDLST
      INTEGER,                   INTENT(INOUT)   :: KDLEN 
      INTEGER,     DIMENSION(:), INTENT(INOUT)   :: KDATA
      INTEGER,                   INTENT(IN)      :: KELEM
      INTEGER,                   INTENT(IN)      :: KVALS
      REAL*8,        DIMENSION(:), INTENT(INOUT)   :: VALUES
      CHARACTER(*),DIMENSION(:), INTENT(INOUT)   :: CVALS
      INTEGER,                   INTENT(INOUT)   :: IRET


      INTEGER REPORT_SUBTYPE
      REAL*8, PARAMETER :: MISSING_VALUE=1.7D38
      
      REPORT_SUBTYPE=NINT(REPORT(3))

      SELECT CASE (REPORT_SUBTYPE)
         CASE(  1)
           CALL SECTIONS01234(REPORT,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,IRET)
           CALL CONVERT_1(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                            VALUES,CVALS,IRET)
         CASE( 11)
           CALL SECTIONS01234(REPORT,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,IRET)
           CALL CONVERT_11(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                            VALUES,CVALS,IRET)
         CASE( 21)
           CALL SECTIONS01234(REPORT,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,IRET)
           CALL CONVERT_21(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                            VALUES,CVALS,IRET)
         CASE( 51)
           CALL SECTIONS01234(REPORT,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,IRET)
           CALL CONVERT_51(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                            VALUES,CVALS,IRET)
         CASE( 75)
           CALL SECTIONS01234(REPORT,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,IRET)
           CALL CONVERT_75(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                           VALUES,CVALS,IRET)
         CASE( 82)
           CALL SECTIONS01234(REPORT,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,IRET)
           CALL CONVERT_82(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                           VALUES,CVALS,IRET)
         CASE( 91)
           CALL SECTIONS01234(REPORT,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,IRET)
           CALL CONVERT_91(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                           VALUES,CVALS,IRET)
         CASE(101)
           CALL SECTIONS01234(REPORT,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,IRET)
           CALL CONVERT_101(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                            VALUES,CVALS,IRET)
         CASE(142)
           CALL SECTIONS01234(REPORT,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,IRET)
           CALL CONVERT_142(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                            VALUES,CVALS,IRET)
         CASE(145)
           CALL SECTIONS01234(REPORT,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,IRET)
           CALL CONVERT_145(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                            VALUES,CVALS,IRET)
         CASE(164)
           CALL SECTIONS01234(REPORT,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,IRET)
           CALL CONVERT_164(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                            VALUES,CVALS,IRET)
         CASE DEFAULT
           PRINT*,' '
           PRINT*,'WARNING ++++ NO CONVERSION PERFORMED FOR SUBTYPE ', &
                   REPORT_SUBTYPE,' +++++'
           IRET=1
           RETURN
      END SELECT

      END SUBROUTINE BUFR_PACK

      SUBROUTINE SECTIONS01234(REPORT,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,IRET)
!
!
!**** *SECTIONS01234*
!
!
!     PURPOSE.
!     --------
!
!           FILL IN DATA NEEDED FOR BUFR SECTIONS 1 TO 4
!
!
!**   INTERFACE.
!     ----------
!
!           CALL SECTIONS01234(REPORT,KSEC0,KSEC1,KSEC2,KSEC3,KSEC4,IRET)*
!
!     METHOD.
!     -------
!
!
!           NONE.
!
!
!     EXTERNALS.
!     ----------
!
!
!
!     REFERENCE.
!     ----------
!
!
!
!     AUTHOR.
!     -------
!
!          M. DRAGOSAVAC    *ECMWF*       MAY  1996
!
!
!     MODIFICATIONS.
!     --------------
!
!          NONE.
!---------------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER  I,YEAR,MONTH,DAY,HOUR,MINUTE,SECOND, DATE
      REAL,    DIMENSION(:), INTENT(INOUT)   :: REPORT
      INTEGER, DIMENSION(:), INTENT (INOUT) :: KSEC0
      INTEGER, DIMENSION(:), INTENT (INOUT) :: KSEC1
      INTEGER, DIMENSION(:), INTENT (INOUT) :: KSEC2
      INTEGER, DIMENSION(:), INTENT (INOUT) :: KSEC3
      INTEGER, DIMENSION(:), INTENT (INOUT) :: KSEC4
      INTEGER,               INTENT (INOUT) :: IRET

      INTEGER, DIMENSION(46) :: KEY
      INTEGER  BUFR_TYPE,RDB_TYPE,RDB_SUBTYPE
      INTEGER  REPORT_SUBTYPE

      CALL OBSDATE(NINT(REPORT(7)),YEAR,MONTH,DAY) 
      CALL OBSTIME(NINT(REPORT(8)),HOUR,MINUTE,SECOND)

      REPORT_SUBTYPE=NINT(REPORT(3))

      SELECT CASE (REPORT_SUBTYPE)
         CASE(  1)
            BUFR_TYPE=0
            RDB_TYPE=1
            RDB_SUBTYPE=1
         CASE(  11)
            BUFR_TYPE=1
            RDB_TYPE=1
            RDB_SUBTYPE=11
         CASE(  21)
            BUFR_TYPE=1
            RDB_TYPE=1
            RDB_SUBTYPE=21
         CASE( 51)
            BUFR_TYPE=3
            RDB_TYPE=2
            RDB_SUBTYPE=51
         CASE( 75)
            BUFR_TYPE=3
            RDB_TYPE=2
            RDB_SUBTYPE=75
         CASE( 82)
            BUFR_TYPE=5
            RDB_TYPE=3
            RDB_SUBTYPE=82
         CASE( 91)
            BUFR_TYPE=2
            RDB_TYPE=4
            RDB_SUBTYPE=91
         CASE( 92)
            BUFR_TYPE=2
            RDB_TYPE=4
            RDB_SUBTYPE=92
         CASE(101)
            BUFR_TYPE=2
            RDB_TYPE=5
            RDB_SUBTYPE=101
         CASE(102)
            BUFR_TYPE=2
            RDB_TYPE=5
            RDB_SUBTYPE=102
         CASE(142)
            BUFR_TYPE=4
            RDB_TYPE=7
            RDB_SUBTYPE=142
         CASE(145)
            BUFR_TYPE=4
            RDB_TYPE=7
            RDB_SUBTYPE=145
         CASE(164)
            BUFR_TYPE=253
            RDB_TYPE=253
            RDB_SUBTYPE=164
         CASE DEFAULT
            PRINT*,' '
            PRINT*,'WARNING +++++ NOT SUPPORTED SUBTYPE ',  &
                    REPORT_SUBTYPE,' +++++'
            IRET=1
            RETURN
      END SELECT


      KSEC0(1)=0
      KSEC0(2)=0
      KSEC0(3)=3

      KSEC1(1)=18
      KSEC1(2)=3
      KSEC1(3)=98
      KSEC1(4)=1
      KSEC1(5)=128
      KSEC1(6)=BUFR_TYPE
      KSEC1(7)=RDB_SUBTYPE
      KSEC1(8)=1
      KSEC1(9)=YEAR-1900
      KSEC1(10)=MONTH
      KSEC1(11)=DAY
      KSEC1(12)=HOUR
      KSEC1(13)=MINUTE
      KSEC1(14)=0
      KSEC1(15)=6
      KSEC1(16)=0

      KSEC2( 1)=52

      KSEC3( 1)=0
      KSEC3( 2)=0
      KSEC3( 3)=1
      KSEC3( 4)=128

      KSEC4( 1)=0
      KSEC4( 2)=0

      DO I=1,46
        KEY(I)=0
      END DO

      KEY( 1)=52
      KEY( 2)=RDB_TYPE
      KEY( 3)=RDB_SUBTYPE
      KEY( 4)=YEAR
      KEY( 5)=MONTH
      KEY( 6)=DAY
      KEY( 7)=HOUR
      KEY( 8)=MINUTE
      KEY( 9)=SECOND
      KEY(10)=REPORT(5)*100000+18000000
      KEY(11)=REPORT(4)*100000+9000000
      KEY(12)=KEY(10)
      KEY(13)=KEY(11)
      KEY(14)=1
      IF(RDB_SUBTYPE >= 51 .AND. RDB_SUBTYPE <= 85) THEN
         KEY(15)=999
      ELSE
         KEY(15)=0
      END IF
      IF(RDB_SUBTYPE >=  9 .AND. RDB_SUBTYPE <=  19 .OR.   &
         RDB_SUBTYPE >=142 .AND. RDB_SUBTYPE <= 145 .OR.   &
         RDB_SUBTYPE == 92 .OR.  RDB_SUBTYPE == 102 .OR.   &
         RDB_SUBTYPE == 164 ) THEN
         KEY(16)=83       ! S
         KEY(17)=73       ! I
         KEY(18)=77       ! M
         KEY(19)=85       ! U
         KEY(20)=76       ! L
         KEY(21)=65       ! A
         KEY(22)=84       ! T
         KEY(23)=69       ! E
         KEY(24)=32
      ELSE 
         KEY(16)=57
         KEY(17)=57
         KEY(18)=50
         KEY(19)=53
         KEY(20)=52
         KEY(21)=32
         KEY(22)=32
         KEY(23)=32
         KEY(24)=32
      END IF
      KEY(25)=0
     


      IRET=0
      CALL BUPKEY(KEY,KSEC1,KSEC2,IRET)
      IF(IRET /= 0) THEN
         PRINT*,'ERROR PACKING KEY'
         RETURN
      END IF

      END SUBROUTINE SECTIONS01234




      SUBROUTINE CONVERT_1(REPORT,KTDLEN,KTDLST,KDLEN,KDATA,VALUES,CVALS,IRET)
!**** *CONVERT_1*
!
!
!     PURPOSE.
!     --------
!
!           CREATES BUFR TEMPLATE FOR SYNOP LAND DATA
!
!
!**   INTERFACE.
!     ----------
!
!           CALL CONVERT_1(REPORT,KTDLEN,KTDLST,KDLEN,KDATA,VALUES,CVALS,IRET)
!
!     METHOD.
!     -------
!
!           NONE.
!
!
!
!     EXTERNALS.
!     ----------
!
!           NONE.
!
!     REFERENCE.
!     ----------
!
!           NONE.
!
!     AUTHOR.
!     -------
!
!          M. DRAGOSAVAC    *ECMWF*       
!
!
!     MODIFICATIONS.
!     --------------
!
!          NONE.
!---------------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SECOND,I
      REAL, PARAMETER :: MISSING_VALUE=1.7E38

      REAL,    DIMENSION(:), INTENT(INOUT)  :: REPORT
      INTEGER,               INTENT(OUT) :: KTDLEN 
      INTEGER, DIMENSION(:), INTENT(OUT) :: KTDLST
      INTEGER,               INTENT(OUT) :: KDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KDATA
      REAL*8,    DIMENSION(:), INTENT(OUT) :: VALUES
      CHARACTER(*), DIMENSION(:), INTENT(OUT) :: CVALS
      INTEGER,               INTENT(OUT) :: IRET
      REAL*8 , PARAMETER ::EPS=10D-8
      INTEGER IV

!     INITIALIZE ARRAYS
!     -----------------

      DO I=1,SIZE(VALUES) 
        VALUES(I)=MISSING_VALUE
      END DO

      DO I=1,SIZE(CVALS)
        CVALS(I)=' '
      END DO

      DO I=1,1360
        IF(ABS(REPORT(I)-(-99999.)) < EPS) THEN
           REPORT(I)=MISSING_VALUE
        END IF
      END DO

!     SET LIST OF DATA DESCRIPTORS FOR SECTION 3
!     ------------------------------------------
      KTDLST(1)=307005
      KTDLST(2)=013023
      KTDLST(3)=013013
      KTDLST(4)=222000
      KTDLST(5)=101049
      KTDLST(6)=031031
      KTDLST(7)=001031
      KTDLST(8)=001032
      KTDLST(9)=101049
      KTDLST(10)=033007

      KTDLEN=10

!     SET DELAYED REPLICATION FACTORS IF ANY.
!     ---------------------------------------

      KDLEN=1
      KDATA(1)=0

!     SET VALUES FOR SYNOP OBSERVATION
!     --------------------------------


      CALL OBSDATE(NINT(REPORT(7)),YEAR,MONTH,DAY)
      CALL OBSTIME(NINT(REPORT(8)),HOUR,MINUTE,SECOND)

      VALUES(  1)=99                      ! BLOCK
      VALUES(  2)=254                     ! STATION
      VALUES(  4)=FLOAT(YEAR)               ! YEAR
      VALUES(  5)=FLOAT(MONTH)                   ! MONTH
      VALUES(  6)=FLOAT(DAY)                     ! DAY
      VALUES(  7)=FLOAT(HOUR)
      VALUES(  8)=FLOAT(MINUTE)
      VALUES(  9)=REPORT(4)               ! LATITUDE
      VALUES( 10)=REPORT(5)               ! LONGITUDE
      VALUES( 11)=REPORT(6)               ! STATION HEIGHT
      VALUES( 12)=REPORT(11)              ! PRESSURE
      VALUES( 13)=REPORT(12)              ! MSL PRESSURE
      VALUES( 16)=REPORT(13)              ! WIND DIRECTION
      VALUES( 17)=REPORT(14)              ! WIND SPEED
      VALUES( 18)=REPORT(16)              ! T
      VALUES( 19)=REPORT(17)              ! TD
      VALUES(50)=222000.

      IV=50
      DO I=1,49
        IV=IV+1
        VALUES(IV)=0.
      END DO
      
      VALUES(100)=98.
      VALUES(101)=1.

      IV=101
      DO I=1,49
        IV=IV+1
        VALUES(IV)=70.
      END DO
      
      END SUBROUTINE CONVERT_1
 
      SUBROUTINE CONVERT_11(REPORT,KTDLEN,KTDLST,KDLEN,KDATA,VALUES,CVALS,IRET)
!**** *CONVERT_11*
!
!
!     PURPOSE.
!     --------
!
!            SET UP SYNOP SHIP TEMPLATE.
!
!
!**   INTERFACE.
!     ----------
!
!              CALL CONVERT_11(REPORT,KTDLEN,KTDLST,KDLEN,KDATA,
!                              VALUES,CVALS,IRET)
!
!     METHOD.
!     -------
!
!
!           NONE.
!
!
!     EXTERNALS.
!     ----------
!
!           NONE.
!
!     REFERENCE.
!     ----------
!
!           NONE.
!
!     AUTHOR.
!     -------
!
!          M. DRAGOSAVAC    *ECMWF*       
!
!
!     MODIFICATIONS.
!     --------------
!
!          NONE.
!---------------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SECOND,I
      REAL*8, PARAMETER :: MISSING_VALUE=1.7D38

      REAL,    DIMENSION(:), INTENT(INOUT)  :: REPORT
      INTEGER,               INTENT(OUT) :: KTDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KTDLST
      INTEGER,               INTENT(OUT) :: KDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KDATA
      REAL*8,    DIMENSION(:), INTENT(OUT) :: VALUES
      CHARACTER(*), DIMENSION(:), INTENT(OUT) :: CVALS
      INTEGER,               INTENT(OUT) :: IRET
      REAL*8 , PARAMETER ::EPS=10D-8
      INTEGER IV

!     INITIALIZE ARRAYS
!     -----------------


      DO I=1,SIZE(VALUES)
        VALUES(I)=MISSING_VALUE
      END DO

      DO I=1,SIZE(CVALS)
        CVALS(I)=' '
      END DO


      DO I=1,1360
        IF(ABS(REPORT(I)-(-99999.)) < EPS) THEN
           REPORT(I)=MISSING_VALUE
        END IF
      END DO


!     SET LIST OF DATA DESCRIPTORS FOR SECTION 3
!     ------------------------------------------


      KTDLST(1)=308004
      KTDLST(2)=012005
      KTDLST(3)=222000
      KTDLST(4)=101033
      KTDLST(5)=031031
      KTDLST(6)=001031
      KTDLST(7)=001032
      KTDLST(8)=101033
      KTDLST(9)=033007


      KTDLEN=9

!     SET DELAYED REPLICATION FACTORS IF ANY.
!     ---------------------------------------

      KDLEN=1
      KDATA(1)=0

!     SET VALUES FOR SYNOP SHIP OBSERVATION
!     -------------------------------------


      CALL OBSDATE(NINT(REPORT(7)),YEAR,MONTH,DAY)
      CALL OBSTIME(NINT(REPORT(8)),HOUR,MINUTE,SECOND)


      VALUES( 1)=1009.
      CVALS(  1)='SIMULATE'
      VALUES( 5)=YEAR
      VALUES( 6)=MONTH
      VALUES( 7)=FLOAT(DAY)
      VALUES( 8)=HOUR
      VALUES( 9)=MINUTE
      VALUES(10)=REPORT(4)
      VALUES(11)=REPORT(5)
      VALUES(12)=REPORT(11)
      VALUES(16)=REPORT(13)
      VALUES(17)=REPORT(14)
      VALUES(18)=REPORT(16)
      VALUES(19)=REPORT(17)

      VALUES(34)=222000.

      IV=34
      DO I=1,33
        IV=IV+1
        VALUES(IV)=0.
      END DO

      VALUES(68)=98.
      VALUES(69)=1.

      IV=69
      DO I=1,33
        IV=IV+1
        VALUES(IV)=70.
      END DO
      END SUBROUTINE CONVERT_11


      SUBROUTINE CONVERT_21(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                             VALUES,CVALS,IRET)
!**** *CONVERT_21*
!
!
!     PURPOSE.
!     --------
!
! 
!
!
!**   INTERFACE.
!     ----------
!
!
!     METHOD.
!     -------
!
!
!
!
!
!     EXTERNALS.
!     ----------
!
!
!
!     REFERENCE.
!     ----------
!
!
!
!     AUTHOR.
!     -------
!
!          M. DRAGOSAVAC    *ECMWF*       
!
!
!     MODIFICATIONS.
!     --------------
!
!          NONE.
!---------------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SECOND,I
      REAL*8, PARAMETER :: MISSING_VALUE=1.7D38

      REAL,    DIMENSION(:), INTENT(INOUT)  :: REPORT
      INTEGER,               INTENT(OUT) :: KTDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KTDLST
      INTEGER,               INTENT(OUT) :: KDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KDATA
      REAL*8,    DIMENSION(:), INTENT(OUT) :: VALUES
      CHARACTER(*), DIMENSION(:), INTENT(OUT) :: CVALS
      INTEGER,               INTENT(OUT) :: IRET
      REAL*8 , PARAMETER ::EPS=10D-8
      INTEGER IV


!     INITIALIZE ARRAYS
!     -----------------


      DO I=1,SIZE(VALUES)
        VALUES(I)=MISSING_VALUE
      END DO

      DO I=1,SIZE(CVALS)
        CVALS(I)=' '
      END DO

      DO I=1,1360
        IF(ABS(REPORT(I)-(-99999.)) < EPS) THEN
           REPORT(I)=MISSING_VALUE
        END IF
      END DO



!     SET LIST OF DATA DESCRIPTORS FOR SECTION 3
!     ------------------------------------------
      KTDLST(1)=308003
      KTDLST(2)=222000
      KTDLST(3)=101032
      KTDLST(4)=031031
      KTDLST(5)=001031
      KTDLST(6)=001032
      KTDLST(7)=101032
      KTDLST(8)=033007

      KTDLEN=8

!     SET DELAYED REPLICATION FACTORS IF ANY.
!     ---------------------------------------

      KDLEN=1
      KDATA(1)=0

!     SET VALUES FOR BUOY OBSERVATION
!     --------------------------------

      CALL OBSDATE(NINT(REPORT(7)),YEAR,MONTH,DAY)
      CALL OBSTIME(NINT(REPORT(8)),HOUR,MINUTE,SECOND)


      VALUES(  1)=99254                     ! BUOY/PLATFORM ID
      VALUES(  5)=YEAR               ! YEAR
      VALUES(  6)=MONTH                   ! MONTH
      VALUES(  7)=DAY                     ! DAY
      VALUES(  8)=FLOAT(HOUR)
      VALUES(  9)=FLOAT(MINUTE)
      VALUES( 10)=REPORT(4)               ! LATITUDE
      VALUES( 11)=REPORT(5)               ! LONGITUDE
      VALUES( 12)=REPORT(11)              ! PRESSURE
      VALUES( 13)=REPORT(12)              ! MSL PRESSURE
      VALUES( 16)=REPORT(13)              ! WIND DIRECTION
      VALUES( 17)=REPORT(14)              ! WIND SPEED
      VALUES( 18)=REPORT(16)              ! T
      VALUES( 19)=REPORT(17)              ! TD

      VALUES(33)=222000.
 
      IV=33
      DO I=1,32
        IV=IV+1
        VALUES(IV)=0.
      END DO

      VALUES(66)=98.
      VALUES(67)=1.

      IV=67
      DO I=1,32
        IV=IV+1
        VALUES(IV)=70.
      END DO

      END SUBROUTINE CONVERT_21





      SUBROUTINE CONVERT_51(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                             VALUES,CVALS,IRET)
!**** *CONVERT_51*
!
!
!     PURPOSE.
!     --------
!
! 
!
!
!**   INTERFACE.
!     ----------
!
!
!     METHOD.
!     -------
!
!
!
!
!
!     EXTERNALS.
!     ----------
!
!
!
!     REFERENCE.
!     ----------
!
!
!
!     AUTHOR.
!     -------
!
!          M. DRAGOSAVAC    *ECMWF*       
!
!
!     MODIFICATIONS.
!     --------------
!
!          NONE.
!---------------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SECOND,I
      REAL*8, PARAMETER :: MISSING_VALUE=1.7D38

      REAL,    DIMENSION(:), INTENT(INOUT)  :: REPORT
      INTEGER,               INTENT(OUT) :: KTDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KTDLST
      INTEGER,               INTENT(OUT) :: KDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KDATA
      REAL*8,    DIMENSION(:), INTENT(OUT) :: VALUES
      CHARACTER(*), DIMENSION(:), INTENT(OUT) :: CVALS
      INTEGER,               INTENT(OUT) :: IRET
      REAL*8 , PARAMETER ::EPS=10D-8
      INTEGER IR,IV,IV0,CHANNEL_NUMBER,IC


!     INITIALIZE ARRAYS
!     -----------------


      DO I=1,SIZE(VALUES)
        VALUES(I)=MISSING_VALUE
      END DO

      DO I=1,SIZE(CVALS)
        CVALS(I)=' '
      END DO

      DO I=1,1360
        IF(ABS(REPORT(I)-(-99999.)) < EPS) THEN
           REPORT(I)=MISSING_VALUE
        END IF
      END DO



!     SET LIST OF DATA DESCRIPTORS FOR SECTION 3
!     ------------------------------------------
      KTDLST( 1)=001007
      KTDLST( 2)=004001
      KTDLST( 3)=004002
      KTDLST( 4)=004003
      KTDLST( 5)=004004
      KTDLST( 6)=004005
      KTDLST( 7)=004006
      KTDLST( 8)=005002
      KTDLST( 9)=006002
      KTDLST(10)=007022
      KTDLST(11)=002025
      KTDLST(12)=002022
      KTDLST(13)=027020
      KTDLST(14)=008003
      KTDLST(15)=015001
      KTDLST(16)=008003
      KTDLST(17)=010004
      KTDLST(18)=020010
      KTDLST(19)=008003
      KTDLST(20)=008012
      KTDLST(21)=010001
      KTDLST(22)=012061
      KTDLST(23)=010004
      KTDLST(24)=008003
      KTDLST(25)=103015
      KTDLST(26)=007004
      KTDLST(27)=007004
      KTDLST(28)=012007
      KTDLST(29)=008003
      KTDLST(30)=103003
      KTDLST(31)=007004
      KTDLST(32)=007004
      KTDLST(33)=013016
      KTDLST(34)=008003
      KTDLST(35)=101019
      KTDLST(36)=012062
      KTDLST(37)=201132
      KTDLST(38)=012062
      KTDLST(39)=201000
      KTDLST(40)=101007
      KTDLST(41)=012062
      KTDLST(42)=008003
      KTDLST(43)=010004
      KTDLST(44)=012001

      KTDLEN=44

!     SET DELAYED REPLICATION FACTORS IF ANY.
!     ---------------------------------------

      KDLEN=1
      KDATA(1)=0

!     SET VALUES FOR SYNOP OBSERVATION
!     --------------------------------

      CALL OBSDATE(NINT(REPORT(7)),YEAR,MONTH,DAY)
      CALL OBSTIME(NINT(REPORT(8)),HOUR,MINUTE,SECOND)


      VALUES( 1)=999
      VALUES( 2)=YEAR
      VALUES( 3)=MONTH
      VALUES( 4)=DAY
      VALUES( 5)=HOUR
      VALUES( 6)=MINUTE
      VALUES( 7)=SECOND
      VALUES( 8)=REPORT(4)
      VALUES( 9)=REPORT(5)
      VALUES(21)=REPORT(6)

      IV0=80
      IR=9
      IC=1
      DO I=1,NINT(REPORT(9))
        IR=IR+10
        IC=IC+10
        CHANNEL_NUMBER=NINT(REPORT(IC))
        IV=IV0+CHANNEL_NUMBER
        VALUES(IV)=REPORT(IR)
      END DO

      END SUBROUTINE CONVERT_51




      SUBROUTINE CONVERT_75(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                             VALUES,CVALS,IRET)
!**** *CONVERT_75*
!
!
!     PURPOSE.
!     --------
!
! 
!
!
!**   INTERFACE.
!     ----------
!
!
!     METHOD.
!     -------
!
!
!
!
!
!     EXTERNALS.
!     ----------
!
!
!
!     REFERENCE.
!     ----------
!
!
!
!     AUTHOR.
!     -------
!
!          M. DRAGOSAVAC    *ECMWF*       
!
!
!     MODIFICATIONS.
!     --------------
!
!          NONE.
!---------------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SECOND,I
      REAL*8, PARAMETER :: MISSING_VALUE=1.7D38

      REAL,    DIMENSION(:), INTENT(INOUT)  :: REPORT
      INTEGER,               INTENT(OUT) :: KTDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KTDLST
      INTEGER,               INTENT(OUT) :: KDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KDATA
      REAL*8,    DIMENSION(:), INTENT(OUT) :: VALUES
      CHARACTER(*), DIMENSION(:), INTENT(OUT) :: CVALS
      INTEGER,               INTENT(OUT) :: IRET
      REAL*8 , PARAMETER ::EPS=10D-8
      INTEGER IV
      INTEGER IR

!     INITIALIZE ARRAYS
!     -----------------


      DO I=1,SIZE(VALUES)
        VALUES(I)=MISSING_VALUE
      END DO

      DO I=1,SIZE(CVALS)
        CVALS(I)=' '
      END DO

      DO I=1,1360
        IF(ABS(REPORT(I)-(-99999.)) < EPS) THEN
           REPORT(I)=MISSING_VALUE
        END IF
      END DO



!     SET LIST OF DATA DESCRIPTORS FOR SECTION 3
!     ------------------------------------------

      KTDLST( 1)=301042
      KTDLST( 2)=303031
      KTDLST( 3)=303032
      KTDLST( 4)=101015
      KTDLST( 5)=303023
      KTDLST( 6)=101003
      KTDLST( 7)=303024
      KTDLST( 8)=222000
      KTDLST( 9)=101108
      KTDLST( 10)=031031
      KTDLST( 11)=001031
      KTDLST( 12)=001032
      KTDLST( 13)=101108
      KTDLST( 14)=033007

      KTDLEN=14



!     SET DELAYED REPLICATION FACTORS IF ANY.
!     ---------------------------------------

      KDLEN=1
      KDATA(1)=0

!     SET VALUES FOR TOVS OBSERVATION
!     --------------------------------

      CALL OBSDATE(NINT(REPORT(7)),YEAR,MONTH,DAY)
      CALL OBSTIME(NINT(REPORT(8)),HOUR,MINUTE,SECOND)

      VALUES( 1)=999.
      VALUES( 4)=YEAR
      VALUES( 5)=MONTH
      VALUES( 6)=DAY
      VALUES( 7)=HOUR
      VALUES( 8)=MINUTE
      VALUES( 9)=REPORT(4)
      VALUES(10)=REPORT(5)

!     MEAN LAYER TEMPERATURES

      IV=19
      IR=11
      DO I=1,NINT(REPORT(9))

        IF(ABS(REPORT(IR+5)-MISSING_VALUE) > EPS) THEN
           VALUES(IV)=REPORT(IR)
           VALUES(IV+1)=REPORT(IR+1)
           VALUES(IV+4)=REPORT(IR+5)    ! T
        END IF

        IV=IV+5
        IR=IR+10

      END DO

!     PRECIPITABLE WATER

      IV=94
      IR=11
      DO I=1,NINT(REPORT(9))

        IF(ABS(REPORT(IR+7)-MISSING_VALUE) > EPS) THEN
           VALUES(IV)=REPORT(IR)
           VALUES(IV+1)=REPORT(IR+1)
           VALUES(IV+4)=REPORT(IR+7)    ! T
        END IF

        IV=IV+5
        IR=IR+10

      END DO

      VALUES(109)=222000.
      IV=109
      DO I=1,108
        IV=IV+1
        VALUES(IV)=0.
      END DO

      IV=IV+1
      VALUES(IV)=98.
      IV=IV+1
      VALUES(IV)=1.

      DO I=1,108
        IV=IV+1
        VALUES(IV)=70.
      END DO

      END SUBROUTINE CONVERT_75





      SUBROUTINE CONVERT_82(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                             VALUES,CVALS,IRET)
!**** *CONVERT_82*
!
!
!     PURPOSE.
!     --------
!
! 
!
!
!**   INTERFACE.
!     ----------
!
!
!     METHOD.
!     -------
!
!
!
!
!
!     EXTERNALS.
!     ----------
!
!
!
!     REFERENCE.
!     ----------
!
!
!
!     AUTHOR.
!     -------
!
!          M. DRAGOSAVAC    *ECMWF*       
!
!
!     MODIFICATIONS.
!     --------------
!
!          NONE.
!---------------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SECOND,I
      REAL*8, PARAMETER :: MISSING_VALUE=1.7D38

      REAL,    DIMENSION(:), INTENT(INOUT)  :: REPORT
      INTEGER,               INTENT(OUT) :: KTDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KTDLST
      INTEGER,               INTENT(OUT) :: KDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KDATA
      REAL*8,    DIMENSION(:), INTENT(OUT) :: VALUES
      CHARACTER(*), DIMENSION(:), INTENT(OUT) :: CVALS
      INTEGER,               INTENT(OUT) :: IRET
      REAL*8 , PARAMETER ::EPS=10D-8
      INTEGER IV

!     INITIALIZE ARRAYS
!     -----------------


      DO I=1,SIZE(VALUES)
        VALUES(I)=MISSING_VALUE
      END DO

      DO I=1,SIZE(CVALS)
        CVALS(I)=' '
      END DO

      DO I=1,1360
        IF(ABS(REPORT(I)-(-99999.)) < EPS) THEN
           REPORT(I)=MISSING_VALUE
        END IF
      END DO



!     SET LIST OF DATA DESCRIPTORS FOR SECTION 3
!     ------------------------------------------

      KTDLST(1)=301043
      KTDLST(2)=304001
      KTDLST(3)=222000
      KTDLST(4)=101015
      KTDLST(5)=031031
      KTDLST(6)=001031
      KTDLST(7)=001032
      KTDLST(8)=101015
      KTDLST(9)=033007


      KTDLEN=9

!     SET DELAYED REPLICATION FACTORS IF ANY.
!     ---------------------------------------

      KDLEN=1
      KDATA(1)=0

!     SET VALUES FOR SATOB OBSERVATION
!     --------------------------------

      CALL OBSDATE(NINT(REPORT(7)),YEAR,MONTH,DAY)
      CALL OBSTIME(NINT(REPORT(8)),HOUR,MINUTE,SECOND)


      VALUES( 1)=999
      VALUES( 3)=YEAR
      VALUES( 4)=MONTH
      VALUES( 5)=DAY
      VALUES( 6)=HOUR
      VALUES( 7)=MINUTE
      VALUES( 8)=SECOND
      VALUES( 9)=REPORT(4)
      VALUES(10)=REPORT(5)
      VALUES(12)=REPORT(11)
      VALUES(13)=REPORT(16)
      VALUES(14)=REPORT(13)
      VALUES(15)=REPORT(14)

      VALUES(16)=222000.

      IV=16
      DO I=1,15
        IV=IV+1
        VALUES(IV)=0.
      END DO

      VALUES(32)=98.
      VALUES(33)=1.

      IV=33
      DO I=1,15
        IV=IV+1
        VALUES(IV)=70.
      END DO

      END SUBROUTINE CONVERT_82




      SUBROUTINE CONVERT_91(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                             VALUES,CVALS,IRET)
!**** *CONVERT_91*
!
!
!     PURPOSE.
!     --------
!
! 
!
!
!**   INTERFACE.
!     ----------
!
!
!     METHOD.
!     -------
!
!
!
!
!
!     EXTERNALS.
!     ----------
!
!
!
!     REFERENCE.
!     ----------
!
!
!
!     AUTHOR.
!     -------
!
!          M. DRAGOSAVAC    *ECMWF*       
!
!
!     MODIFICATIONS.
!     --------------
!
!          NONE.
!---------------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SECOND,I
      REAL*8, PARAMETER :: MISSING_VALUE=1.7D38

      REAL,    DIMENSION(:), INTENT(INOUT)  :: REPORT
      INTEGER,               INTENT(OUT) :: KTDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KTDLST
      INTEGER,               INTENT(OUT) :: KDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KDATA
      REAL*8,    DIMENSION(:), INTENT(OUT) :: VALUES
      CHARACTER(*), DIMENSION(:), INTENT(OUT) :: CVALS
      INTEGER,               INTENT(OUT) :: IRET
      REAL*8 , PARAMETER ::EPS=10D-8
      INTEGER IV
      INTEGER IR

!     INITIALIZE ARRAYS
!     -----------------


      DO I=1,SIZE(VALUES)
        VALUES(I)=MISSING_VALUE
      END DO

      DO I=1,SIZE(CVALS)
        CVALS(I)=' '
      END DO

      DO I=1,1360
        IF(ABS(REPORT(I)-(-99999.)) < EPS) THEN
           REPORT(I)=MISSING_VALUE
        END IF
      END DO



!     SET LIST OF DATA DESCRIPTORS FOR SECTION 3
!     ------------------------------------------
      KTDLST( 1)=301001
      KTDLST( 2)=002011
      KTDLST( 3)=002012
      KTDLST( 4)=301011
      KTDLST( 5)=301012
      KTDLST( 6)=301022
      KTDLST( 7)=105000
      KTDLST( 8)=031001
      KTDLST( 9)=007004
      KTDLST(10)=008001
      KTDLST(11)=010003
      KTDLST(12)=011001
      KTDLST(13)=011002
      KTDLST(14)=222000
      KTDLST(15)=101000
      KTDLST(16)=031002
      KTDLST(17)=031031
      KTDLST(18)=001031
      KTDLST(19)=001032
      KTDLST(20)=101000
      KTDLST(21)=031002
      KTDLST(22)=033007

      KTDLEN=22

!     SET DELAYED REPLICATION FACTORS IF ANY.
!     ---------------------------------------

      KDLEN=3
      KDATA(1)=NINT(REPORT(9))
      KDATA(2)=NINT(REPORT(9))*5+13
      KDATA(3)=KDATA(2)

!     SET VALUES FOR PILOT OBSERVATION
!     --------------------------------

      CALL OBSDATE(NINT(REPORT(7)),YEAR,MONTH,DAY)
      CALL OBSTIME(NINT(REPORT(8)),HOUR,MINUTE,SECOND)


      VALUES( 1)=99
      VALUES( 2)=254
      VALUES(  5)=YEAR                                    ! YEAR
      VALUES(  6)=MONTH                                   ! MONTH
      VALUES(  7)=DAY                                     ! DAY
      VALUES(  8)=FLOAT(HOUR)                             ! HOUR
      VALUES(  9)=FLOAT(MINUTE)                           ! MINUTE
      VALUES( 10)=REPORT(4)                               ! LATITUDE
      VALUES( 11)=REPORT(5)                               ! LONGITUDE
      VALUES( 12)=REPORT(6)                               ! HEIGHT OF STATION
      VALUES( 13)=REPORT(9)                               ! REPLICATION


      IV=14
      IR=11
      DO I=1,NINT(REPORT(9))
        VALUES(IV)=REPORT(IR)       ! PRESSURE
        VALUES(IV+1)=32.
        IF(ABS(REPORT(IR+4)-MISSING_VALUE) > EPS) THEN
           VALUES(IV+2)=REPORT(IR+4)*9.80665 ! GEOPOTENTIAL
        END IF
        VALUES(IV+3)=REPORT(IR+2)         ! DD
        VALUES(IV+4)=REPORT(IR+3)         ! FF
        IV=IV+5
        IR=IR+10
      END DO

      IV=KDATA(2)+1
      IR=KDATA(2)


      VALUES(IV)=222000.

      VALUES(IV+1)=FLOAT(IR)
      IV=IV+1

      DO I=1,IR
        IV=IV+1
        VALUES(IV)=0.
      END DO

      IV=IV+1
      VALUES(IV)=98.
      IV=IV+1
      VALUES(IV)=1.
      IV=IV+1
      VALUES(IV)=FLOAT(IR)
     
      DO I=1,IR
        IV=IV+1
        VALUES(IV)=70.
      END DO

      END SUBROUTINE CONVERT_91




      SUBROUTINE CONVERT_101(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                             VALUES,CVALS,IRET)
!**** *CONVERT_101*
!
!
!     PURPOSE.
!     --------
!
! 
!
!
!**   INTERFACE.
!     ----------
!
!
!     METHOD.
!     -------
!
!
!
!
!
!     EXTERNALS.
!     ----------
!
!
!
!     REFERENCE.
!     ----------
!
!
!
!     AUTHOR.
!     -------
!
!          M. DRAGOSAVAC    *ECMWF*       
!
!
!     MODIFICATIONS.
!     --------------
!
!          NONE.
!---------------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SECOND,I
      REAL*8, PARAMETER :: MISSING_VALUE=1.7D38

      REAL,    DIMENSION(:), INTENT(INOUT)  :: REPORT
      INTEGER,               INTENT(OUT) :: KTDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KTDLST
      INTEGER,               INTENT(OUT) :: KDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KDATA
      REAL*8,    DIMENSION(:), INTENT(OUT) :: VALUES
      CHARACTER(*), DIMENSION(:), INTENT(OUT) :: CVALS
      INTEGER,               INTENT(OUT) :: IRET
      REAL*8 , PARAMETER ::EPS=10D-8
      INTEGER IR
      INTEGER IV

!     INITIALIZE ARRAYS
!     -----------------


      DO I=1,SIZE(VALUES)
        VALUES(I)=MISSING_VALUE
      END DO

      DO I=1,SIZE(CVALS)
        CVALS(I)=' '
      END DO


      DO I=1,1360
        IF(ABS(REPORT(I)-(-99999.)) < EPS) THEN
           REPORT(I)=MISSING_VALUE
        END IF
      END DO


!     SET LIST OF DATA DESCRIPTORS FOR SECTION 3
!     ------------------------------------------
      KTDLST(1)=309007
      KTDLST(2)=222000
      KTDLST(3)=101000
      KTDLST(4)=031002
      KTDLST(5)=031031
      KTDLST(6)=001031
      KTDLST(7)=001032
      KTDLST(8)=101000
      KTDLST(9)=031002
      KTDLST(10)=033007
 

      KTDLEN=10

!     SET DELAYED REPLICATION FACTORS IF ANY.
!     ---------------------------------------

      KDATA(1)=NINT(REPORT(9))
      KDATA(2)=NINT(REPORT(9))*7+20
      KDATA(3)=KDATA(2)

      KDLEN=3

!     SET VALUES FOR TEMP  OBSERVATION
!     --------------------------------

      CALL OBSDATE(NINT(REPORT(7)),YEAR,MONTH,DAY)
      CALL OBSTIME(NINT(REPORT(8)),HOUR,MINUTE,SECOND)

      VALUES(  1)=99.
      VALUES(  2)=254.
      VALUES(  5)=YEAR                                    ! YEAR
      VALUES(  6)=MONTH                                   ! MONTH
      VALUES(  7)=FLOAT(DAY)                                     ! DAY
      VALUES(  8)=FLOAT(HOUR)                             ! HOUR
      VALUES(  9)=FLOAT(MINUTE)                           ! MINUTE
      VALUES( 10)=REPORT(4)                               ! LATITUDE
      VALUES( 11)=REPORT(5)                               ! LONGITUDE
      VALUES( 12)=REPORT(6)                               ! HEIGHT OF STATION
      VALUES( 20)=REPORT(9)                               ! REPLICATION


      IV=21
      IR=11
      DO I=1,NINT(REPORT(9))
        VALUES(IV)=REPORT(IR)       ! PRESSURE
        VALUES(IV+1)=32.
        IF(ABS(REPORT(IR+4)-MISSING_VALUE) > EPS) THEN
           VALUES(IV+2)=REPORT(IR+4)*9.80665 ! GEOPOTENTIAL
        END IF
        VALUES(IV+3)=REPORT(IR+5)         ! T
        VALUES(IV+4)=REPORT(IR+6)         ! TD
        VALUES(IV+5)=REPORT(IR+2)         ! DD
        VALUES(IV+6)=REPORT(IR+3)         ! FF
        IV=IV+7
        IR=IR+10
      END DO

      IV=KDATA(2)+1
      IR=KDATA(2)


      VALUES(IV)=222000.

      VALUES(IV+1)=FLOAT(IR)
      IV=IV+1

      DO I=1,IR
        IV=IV+1
        VALUES(IV)=0.
      END DO

      IV=IV+1
      VALUES(IV)=98.
      IV=IV+1
      VALUES(IV)=1.
      IV=IV+1
      VALUES(IV)=FLOAT(IR)

      DO I=1,IR
        IV=IV+1
        VALUES(IV)=70.
      END DO



      END SUBROUTINE CONVERT_101




      SUBROUTINE CONVERT_142(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                             VALUES,CVALS,IRET)
!**** *CONVERT_142*
!
!
!     PURPOSE.
!     --------
!
! 
!
!
!**   INTERFACE.
!     ----------
!
!
!     METHOD.
!     -------
!
!
!
!
!
!     EXTERNALS.
!     ----------
!
!
!
!     REFERENCE.
!     ----------
!
!
!
!     AUTHOR.
!     -------
!
!          M. DRAGOSAVAC    *ECMWF*       
!
!
!     MODIFICATIONS.
!     --------------
!
!          NONE.
!---------------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SECOND,I
      REAL*8, PARAMETER :: MISSING_VALUE=1.7D38

      REAL,    DIMENSION(:), INTENT(INOUT)  :: REPORT
      INTEGER,               INTENT(OUT) :: KTDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KTDLST
      INTEGER,               INTENT(OUT) :: KDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KDATA
      REAL*8,    DIMENSION(:), INTENT(OUT) :: VALUES
      CHARACTER(*), DIMENSION(:), INTENT(OUT) :: CVALS
      INTEGER,               INTENT(OUT) :: IRET
      REAL*8 , PARAMETER ::EPS=10D-8
      INTEGER IV

!     INITIALIZE ARRAYS
!     -----------------


      DO I=1,SIZE(VALUES)
        VALUES(I)=MISSING_VALUE
      END DO

      DO I=1,SIZE(CVALS)
        CVALS(I)=' '
      END DO

      DO I=1,1360
        IF(ABS(REPORT(I)-(-99999.)) < EPS) THEN
           REPORT(I)=MISSING_VALUE
        END IF
      END DO


!     SET LIST OF DATA DESCRIPTORS FOR SECTION 3
!     ------------------------------------------
      KTDLST(1)=311001
      KTDLST(2)=222000
      KTDLST(3)=101018
      KTDLST(4)=031031
      KTDLST(5)=001031
      KTDLST(6)=001032
      KTDLST(7)=101018
      KTDLST(8)=033007


      KTDLEN=8

!     SET DELAYED REPLICATION FACTORS IF ANY.
!     ---------------------------------------

      KDLEN=1
      KDATA(1)=0

!     SET VALUES FOR AIREP OBSERVATION
!     --------------------------------

      CALL OBSDATE(NINT(REPORT(7)),YEAR,MONTH,DAY)
      CALL OBSTIME(NINT(REPORT(8)),HOUR,MINUTE,SECOND)


      VALUES( 1)=1008.
      CVALS( 1)='SIMULATE' 
      VALUES(3)=YEAR
      VALUES(4)=MONTH
      VALUES(5)=DAY
      VALUES(6)=HOUR
      VALUES(7)=MINUTE
      VALUES(8)=REPORT(4)
      VALUES(9)=REPORT(5)
      VALUES(11)=REPORT(6)
      VALUES(12)=REPORT(16)
      VALUES(13)=REPORT(13)
      VALUES(14)=REPORT(14)

      VALUES(19)=222000.

      IV=19
      DO I=1,18
        IV=IV+1
        VALUES(IV)=0.
      END DO

      VALUES(38)=98.
      VALUES(39)=1.

      IV=39
      DO I=1,18
        IV=IV+1
        VALUES(IV)=70.
      END DO

      END SUBROUTINE CONVERT_142





      SUBROUTINE CONVERT_145(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                             VALUES,CVALS,IRET)
!**** *CONVERT_145
!
!
!     PURPOSE.
!     --------
!
! 
!
!
!**   INTERFACE.
!     ----------
!
!
!     METHOD.
!     -------
!
!
!
!
!
!     EXTERNALS.
!     ----------
!
!
!
!     REFERENCE.
!     ----------
!
!
!
!     AUTHOR.
!     -------
!
!          M. DRAGOSAVAC    *ECMWF*       
!
!
!     MODIFICATIONS.
!     --------------
!
!          NONE.
!---------------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SECOND,I
      REAL*8, PARAMETER :: MISSING_VALUE=1.7D38

      REAL,    DIMENSION(:), INTENT(INOUT)  :: REPORT
      INTEGER,               INTENT(OUT) :: KTDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KTDLST
      INTEGER,               INTENT(OUT) :: KDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KDATA
      REAL*8,    DIMENSION(:), INTENT(OUT) :: VALUES
      CHARACTER(*), DIMENSION(:), INTENT(OUT) :: CVALS
      INTEGER,               INTENT(OUT) :: IRET
      REAL*8 , PARAMETER ::EPS=10D-8

!     INITIALIZE ARRAYS
!     -----------------


      DO I=1,SIZE(VALUES)
        VALUES(I)=MISSING_VALUE
      END DO

      DO I=1,SIZE(CVALS)
        CVALS(I)=' '
      END DO

      DO I=1,1360
        IF(ABS(REPORT(I)-(-99999.)) < EPS) THEN
           REPORT(I)=MISSING_VALUE
        END IF
      END DO


!     SET LIST OF DATA DESCRIPTORS FOR SECTION 3
!     ------------------------------------------
      KTDLST( 1)=001006
      KTDLST( 2)=001008
      KTDLST( 3)=002061
      KTDLST( 4)=002062
      KTDLST( 5)=002002
      KTDLST( 6)=002005
      KTDLST( 7)=002070
      KTDLST( 8)=002063
      KTDLST( 9)=002001
      KTDLST(10)=004001
      KTDLST(11)=004002
      KTDLST(12)=004003
      KTDLST(13)=004004
      KTDLST(14)=004005
      KTDLST(15)=005002
      KTDLST(16)=006002
      KTDLST(17)=008004
      KTDLST(18)=007004
      KTDLST(19)=008021
      KTDLST(20)=011001
      KTDLST(21)=011002
      KTDLST(22)=011031
      KTDLST(23)=011034
      KTDLST(24)=011035
      KTDLST(25)=012001
      KTDLST(26)=012003
      KTDLST(27)=013003
      KTDLST(28)=020041
      

      KTDLEN=28

!     SET DELAYED REPLICATION FACTORS IF ANY.
!     ---------------------------------------

      KDLEN=1
      KDATA(1)=0

!     SET VALUES FOR  ACAR OBSERVATION
!     --------------------------------

      CALL OBSDATE(NINT(REPORT(7)),YEAR,MONTH,DAY)
      CALL OBSTIME(NINT(REPORT(8)),HOUR,MINUTE,SECOND)


      VALUES( 1)=1008.
      CVALS(1)='SIMULATE'
      VALUES(2)=2008.
      CVALS(2)='SIMULATE'
      VALUES(10)=YEAR
      VALUES(11)=MONTH
      VALUES(12)=DAY
      VALUES(13)=HOUR
      VALUES(14)=MINUTE
      VALUES(15)=REPORT(4)
      VALUES(16)=REPORT(5)
      VALUES(18)=REPORT(11)
      VALUES(20)=REPORT(13)
      VALUES(21)=REPORT(14)
      VALUES(25)=REPORT(16)
      VALUES(26)=REPORT(17)


      END SUBROUTINE CONVERT_145




      SUBROUTINE CONVERT_164(REPORT,KTDLEN,KTDLST,KDLEN,KDATA, &
                             VALUES,CVALS,IRET)
!**** *CONVERT_164*
!
!
!     PURPOSE.
!     --------
!
! 
!
!
!**   INTERFACE.
!     ----------
!
!
!     METHOD.
!     -------
!
!
!
!
!
!     EXTERNALS.
!     ----------
!
!
!
!     REFERENCE.
!     ----------
!
!
!
!     AUTHOR.
!     -------
!
!          M. DRAGOSAVAC    *ECMWF*       
!
!
!     MODIFICATIONS.
!     --------------
!
!          NONE.
!---------------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER YEAR,MONTH,DAY,HOUR,MINUTE,SECOND,I
      REAL*8, PARAMETER :: MISSING_VALUE=1.7D38

      REAL,    DIMENSION(:), INTENT(INOUT)  :: REPORT
      INTEGER,               INTENT(OUT) :: KTDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KTDLST
      INTEGER,               INTENT(OUT) :: KDLEN
      INTEGER, DIMENSION(:), INTENT(OUT) :: KDATA
      REAL*8,    DIMENSION(:), INTENT(OUT) :: VALUES
      CHARACTER(*), DIMENSION(:), INTENT(OUT) :: CVALS
      INTEGER,               INTENT(OUT) :: IRET
      REAL*8 , PARAMETER ::EPS=10D-8
      INTEGER IV

!     INITIALIZE ARRAYS
!     -----------------

      DO I=1,SIZE(VALUES)
        VALUES(I)=MISSING_VALUE
      END DO

      DO I=1,SIZE(CVALS)
        CVALS(I)=' '
      END DO

      DO I=1,1360
        IF(ABS(REPORT(I)-(-99999.)) < EPS) THEN
           REPORT(I)=MISSING_VALUE
        END IF
      END DO


!     SET LIST OF DATA DESCRIPTORS FOR SECTION 3
!     ------------------------------------------
      KTDLST(1)=301011
      KTDLST(2)=301012
      KTDLST(3)=301021
      KTDLST(4)=010051
      KTDLST(5)=007004
      KTDLST(6)=007004
      KTDLST(7)=012007
      KTDLST(8)=222000
      KTDLST(9)=101011
      KTDLST(10)=031031
      KTDLST(11)=001031
      KTDLST(12)=001032
      KTDLST(13)=101011
      KTDLST(14)=033007


      KTDLEN=14

!     SET DELAYED REPLICATION FACTORS IF ANY.
!     ---------------------------------------

      KDLEN=1
      KDATA(1)=0

!     SET VALUES FOR PAOB OBSERVATION
!     -------------------------------

      CALL OBSDATE(NINT(REPORT(7)),YEAR,MONTH,DAY)
      CALL OBSTIME(NINT(REPORT(8)),HOUR,MINUTE,SECOND)


      VALUES(  1)=YEAR
      VALUES(  2)=MONTH
      VALUES(  3)=DAY
      VALUES(  4)=HOUR
      VALUES(  5)=MINUTE
      VALUES(  6)=REPORT(4)
      VALUES(  7)=REPORT(5)
      VALUES(  8)=REPORT(11)
      
      VALUES(12)=222000.

      IV=12
      DO I=1,12
        IV=IV+1
        VALUES(IV)=0.
      END DO

      VALUES(24)=98.
      VALUES(25)=1.

      IV=25
      DO I=1,12
        IV=IV+1
        VALUES(IV)=70.
      END DO
      END SUBROUTINE CONVERT_164


      SUBROUTINE OBSDATE(DATE,YEAR,MONTH,DAY)

      INTEGER DATE,OFFSET

      INTEGER YEAR,MONTH,DAY

      YEAR=DATE/10000
      OFFSET=DATE-YEAR*10000
      MONTH=OFFSET/100
      DAY=OFFSET-MONTH*100

      END SUBROUTINE OBSDATE

      SUBROUTINE OBSTIME(TIME,HOUR,MINUTE,SECOND)
      INTEGER TIME,HOUR,MINUTE,SECOND,OFFSET

      HOUR=TIME/10000
      OFFSET=TIME-HOUR*10000
      MINUTE=OFFSET/100
      SECOND=OFFSET-MINUTE*100
      END SUBROUTINE OBSTIME


   END PROGRAM BUFR_SIMULATE

