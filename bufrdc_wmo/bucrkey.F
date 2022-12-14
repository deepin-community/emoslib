C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE  BUCRKEY(KELEM,KTDEXP,KSUP,KSEC1,KSEC3,KEY,
     1                     VALUES,CVALS,KERR)
C**** *BUCRKEY*
C
C
C     PURPOSE.
C     --------
C
C          EXTRACT ELEMENTS NEEDED FOR RDB KEY DEFINITION
C          FROM VALUES AND CVALS ARRAYS.
C
C
C**   INTERFACE.
C     ----------
C
C          NONE.
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C
C          M. DRAGOSAVAC    *ECMWF*       06/06/99.
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(O,G), CHARACTER*8(C,H,Y)
C
C     -------------------------------------------------------------
      DIMENSION KEY(*),KSUP(*),KSEC1(*),KTDEXP(*)
#ifndef R_4
      REAL*8 EPS, RVIND
      REAL*8 VALUES(*)
      REAL*8 RMINLAT, RMINLON, RMAXLAT, RMAXLON
#else
      REAL   EPS, RVIND
      REAL   VALUES(*)
      REAL   RMINLAT, RMINLON, RMAXLAT, RMAXLON
#endif
      DIMENSION KSEC3(*)
      CHARACTER*(*)CVALS(*)
      CHARACTER*5 CBLOCK
C
 100  CONTINUE
C
C     IPAID - AIRCRAFT FLIGHT NUMBER INDEX
C     IPSID - SATELLITE IDENTIFIER INDEX
C     IPBUOY - BUOY/PLATFORM IDENTIFIER INDEX
C     IPARN - AIRCRAFT REGISTRATION NUMBER INDEX
C     IPMET - METAR ICAO ID
C     IPSHIP - SHIPS' CALL SIGN INDEX
C     IPBLOK - WMO BLOCK NUMBER INDEX
C     IPSTAT - WMO STATION NUMBER INDEX
C     IPYEAR - YEAR INDEX
C     IPMONTH - MONTH INDEX
C     IPDAY - DAY INDEX
C     IPHOUR - HOUR INDEX
C     IPMINUTE - MINUTE INDEX
C     IPSECOND - SECOND INDEX
C     IPLAT - LATITUDE INDEX
C     IPLON - LONGITUDE INDEX
C     IPLAT - LATITUDE INDEX
C     IPLON - LONGITUDE INDEX
C
      KERR=0
      EPS=10.D-7
      RVIND=1.7D38
      IPAID=0
      IPSID=0
      IPBUOY=0
      IPARN=0
      IPMET=0
      IPSHIP=0
      IPBLOK=0
      IPSTAT=0
      IPYEAR=0
      IPMONTH=0
      IPDAY=0
      IPHOUR=0
      IPMINUTE=0
      IPSECOND=0
      IPLAT=0
      IPLON=0
      DO 107 I=16,24
      KEY(I)=32
 107  CONTINUE
C
      IEND = 26
      IF( KSEC1(7).EQ.57 ) IEND = 36
      DO 101 I=1,IEND
      IF(KTDEXP(I).EQ.001006) IPAID=I
      IF(KTDEXP(I).EQ.001007) IPSID=I
      IF(KTDEXP(I).EQ.001005) IPBUOY=I
C     IF(KTDEXP(I).EQ.001008) IPARN=I
      IF(KTDEXP(I).EQ.001194) IPAID=I
      IF(KTDEXP(I).EQ.001063) IPMET=I
      IF(KTDEXP(I).EQ.001011) IPSHIP=I
      IF(KTDEXP(I).EQ.001001) IPBLOK=I
      IF(KTDEXP(I).EQ.001002) IPSTAT=I
      IF(KTDEXP(I).EQ.004001) IPYEAR=I
      IF(KTDEXP(I).EQ.004002) IPMONTH=I
      IF(KTDEXP(I).EQ.004003) IPDAY=I
      IF(KTDEXP(I).EQ.004004) IPHOUR=I
      IF(KTDEXP(I).EQ.004005) IPMINUTE=I
      IF(KTDEXP(I).EQ.004006) IPSECOND=I
      IF(KTDEXP(I).EQ.005001) IPLAT=I
      IF(KTDEXP(I).EQ.006001) IPLON=I
      IF(KTDEXP(I).EQ.005002) IPLAT=I
      IF(KTDEXP(I).EQ.006002) IPLON=I
 101  CONTINUE
C
      KEY(4)=VALUES(IPYEAR)
      KEY(5)=VALUES(IPMONTH)
      KEY(6)=VALUES(IPDAY)
      KEY(7)=VALUES(IPHOUR)
      KEY(8)=VALUES(IPMINUTE)
      IF(IPSECOND.NE.0) KEY(9)=VALUES(IPSECOND)
C
      IF((ABS(VALUES(IPLON)-RVIND))/RVIND.LT.EPS.OR.
     1   (ABS(VALUES(IPLAT)-RVIND))/RVIND.LT.EPS) THEN
          KERR=1
          PRINT*,'FOUND VALUES(IPLON)=',VALUES(IPLON)
          PRINT*,'FOUND VALUES(IPLAT)=',VALUES(IPLAT)
          RETURN
      ELSE
         CALL FMMH( IPLAT,IPLON,KELEM,VALUES,KSEC3,RMINLAT,RMINLON,
     1           RMAXLAT,RMAXLON)

         KEY(10)=NINT(RMINLON*100000.+18000000)
         KEY(11)=NINT(RMINLAT*100000.+9000000)
         KEY(12)=NINT(RMAXLON*100000.+18000000)
         KEY(13)=NINT(RMAXLAT*100000.+9000000)
      END IF
C
      KEY(14)=KSEC3(3)
C
C        SATELITTE ID
C
      KEY(15)=0
      DO IS=16,24
        KEY(IS)=32
      END DO
      IF(IPSID.NE.0) THEN
         KEY(15)=VALUES(IPSID)
      END IF
C
C        WMO BLOCK AND STATION NUMBER
C
      IF(IPBLOK.NE.0) THEN
         IF(ABS(VALUES(IPBLOK)-RVIND)/RVIND.LT.EPS) THEN
            KERR=1
            PRINT*,'FOUND VALUES(IPBLOK)=',VALUES(IPBLOK)
            RETURN
         END IF
         IF(ABS(VALUES(IPSTAT)-RVIND)/RVIND.LT.EPS) THEN
            KERR=1
            PRINT*,'FOUND VALUES(IPSTAT)=',VALUES(IPSTAT)
            RETURN
         END IF
         KEY(16)=NINT(VALUES(IPBLOK))*1000+NINT(VALUES(IPSTAT))
         WRITE(CBLOCK,'(I5.5)') KEY(16)
         J=15
         DO 105 I=1,5
         KEY(J+I)=ICHAR(CBLOCK(I:I))
 105     CONTINUE
      END IF
C
C        BUOY/PALTFORM ID
C
      IF(IPBUOY.NE.0) THEN
         IF(ABS(VALUES(IPBUOY)-RVIND)/RVIND.LT.EPS) THEN
            KERR=1
            PRINT*,'FOUND VALUES(IPBUOY)=',VALUES(IPBUOY)
            RETURN
         END IF
         KEY(16)=NINT(VALUES(IPBUOY))
         WRITE(CBLOCK,'(I5.5)') KEY(16)
         J=15
         DO 106 I=1,5
         KEY(J+I)=ICHAR(CBLOCK(I:I))
 106     CONTINUE
      END IF
C
C        AIRCRAFT IDENTIFIER
C
      IF(IPAID.NE.0) THEN
         IF(ABS(VALUES(IPAID)-RVIND)/RVIND.LT.EPS) THEN
            KERR=1
            PRINT*,'FOUND VALUES(IPAID)=',VALUES(IPAID)
            RETURN
         END IF
        IP=(VALUES(IPAID)/1000)
        INCH=NINT(VALUES(IPAID)-IP*1000)
        J=15
        DO 102 I=1,INCH
        KEY(J+I)=ICHAR(CVALS(IP)(I:I))
 102    CONTINUE
      END IF
C
C        AIRCRAFT REGISTRATION NUMBER
C
      IF(IPARN.NE.0) THEN
         IF(ABS(VALUES(IPARN)-RVIND)/RVIND.LT.EPS) THEN
            KERR=1
            PRINT*,'FOUND VALUES(IPARN)=',VALUES(IPARN)
            RETURN
         END IF
        IP=(VALUES(IPARN)/1000)
        INCH=NINT(VALUES(IPARN)-IP*1000)
        J=15
        DO 103 I=1,INCH
        KEY(J+I)=ICHAR(CVALS(IP)(I:I))
 103    CONTINUE
      END IF
C
C       SHIP'S CALL SIGN
C
      IF(IPSHIP.NE.0) THEN
         IF(ABS(VALUES(IPSHIP)-RVIND)/RVIND.LT.EPS) THEN
            KERR=1
            PRINT*,'FOUND VALUES(IPSHIP)=',VALUES(IPSHIP)
            RETURN
         END IF
        IP=(VALUES(IPSHIP)/1000)
        INCH=NINT(VALUES(IPSHIP)-IP*1000)
        J=15
        DO 104 I=1,INCH
        KEY(J+I)=ICHAR(CVALS(IP)(I:I))
 104    CONTINUE
      END IF
C
C       METAR ID
C
      IF(IPMET.NE.0) THEN
         IF(ABS(VALUES(IPMET)-RVIND)/RVIND.LT.EPS) THEN
            KERR=1
            PRINT*,'FOUND VALUES(IPMET)=',VALUES(IPMET)
            RETURN
         END IF
        IP=(VALUES(IPMET)/1000)
        INCH=NINT(VALUES(IPMET)-IP*1000)
        J=15
        DO 194 I=1,INCH
        KEY(J+I)=ICHAR(CVALS(IP)(I:I))
 194    CONTINUE
      END IF
C
      RETURN
      END
