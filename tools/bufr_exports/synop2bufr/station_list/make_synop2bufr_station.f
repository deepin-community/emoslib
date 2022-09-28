       PROGRAM MAKESTATION
C
C**** *MAKESTATION*
C
C
C     PURPOSE.
C     --------
c
C         Extract data from Empress WMO VOL A DB
c         to create binary station list for preprocessing.
c
C**   INTERFACE.
C     ----------
C
C         none.
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
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/01/2009
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
c     ------------------------------------------------------------------
c           1. Initialize Empress Data Base.
c              -----------------------------
 100  continue
c
c
c*          2. Select data for analysis.
c              -------------------------
 200  continue
c
      call station
c
c*          3. Clean up.
c              ---------
 300  continue
c
c
c     stop
      end
      SUBROUTINE STATION
C
C
C**** *STATION*
C
C
C     PURPOSE.
C     --------
c
C         CREATE PACKED FORM OF WMO STATION LIST
C         FOR PREPROCESSING. 
c
C**   INTERFACE.
C     ----------
C
C         none.
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
C          *Empress data base routines*
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       03/02/2009
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
c
      character*2 csp00,csp03,csp06,csp09,csp12,csp15
      character*2 csp18,csp21
      character*1 cuat00,cuat06,cuat12,cuat18

      character*32 cstation
c
      character*120 cline
c
c
c
C     ------------------------------------------------------------------
c
c           1.2.1 Open wmo station list
c                 ---------------------
      open(unit=28,file='wmo.txt',iostat=ios,
     1             form='formatted',
     2             recl=120,status='old')
      if(ios .ne.0) then
         print*,'Error opening wmo.txt file.'
         call exit(2)
      end if

      open(unit=29,file='synop2bufr_station.txt',iostat=ios,
     1             form='formatted',
     2             recl=130,status='unknown')
      if(ios .ne.0) then
         print*,'Error opening wmo.txt file.'
         call exit(2)
      end if

      icount=0
c
 200  continue
c
c
c     read(28,'(a)') cline
c
c*            2.2 Read data
c                 --------------
 220  continue
c
      isthp=0
      istha=0
      read(28,fmt=8888,iostat=ios,end=300) irgcoun, iblock,
     1                                     istno, rlat, rlon,
     2                                     isthp, istha ,ipcode,
     3                                     csp00,csp03,csp06,
     4                                     csp09,csp12,csp15,
     5                                     csp18,csp21,
     6                                     cuat00,cuat06,cuat12,
     7                                     cuat18,cstation
      if(ios.ne.0) then
         print*,'Read error on wmo.txt file'
         call exit(2)
      end if

      icount=icount+1
c
 8888 format(i4,1x,i2,1x,i3,1x,f7.2,1x,f7.2,1x,i4,1x,i4,1x,i1,
     1       1x,8(a2,1x),4(a,1x),a)
      rt=99.9
      rvis=99.9
      rprec=99.9
      rvinds=99.9
      write(29,fmt=8889,iostat=ios) irgcoun, iblock,
     1                                     istno, rlat, rlon,
     2                                     isthp, istha ,ipcode,
     3                                     csp00,csp03,csp06,
     4                                     csp09,csp12,csp15,
     5                                     csp18,csp21,
     6                                     cuat00,cuat06,cuat12,
     7                                     cuat18,cstation,rt,rvis,
     8                                     rprec,rvinds
c
 8889 format(i4,1x,i2.2,i3.3,1x,f7.2,1x,f7.2,1x,i4,1x,i4,1x,i1,
     1       1x,8(a2,1x),4(a,1x),a,1x,F4.1,3(3x,F4.1))

      go to 220
c
c
c*          3. Clean table request.
c              --------------------
 300  continue
c
c
      print*,'Number of stations retreived ',icount
c
c
c     -------------------------------------------------------------------
c
      END
