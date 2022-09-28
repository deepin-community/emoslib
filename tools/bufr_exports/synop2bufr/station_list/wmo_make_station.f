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
C          M. DRAGOSAVAC    *ECMWF*       15/01/94.
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
C          M. DRAGOSAVAC    *ECMWF*       03/02/94.
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
      character*12 ctabname
c
      character*2 cwsbn,csp00,csp03,csp06,csp09,csp12,csp15
      character*2 csp18,csp21
      character*1 cpef,cgef,cps
      character*4 cstha,csthp
      character*1 cuat00,cuat06,cuat12,cuat18
c
      character*120 cline
      dimension iclist(50),implis(2000),in(10) 
      dimension iparams(39000),ipoints(128)
c
      integer*2 ipcode,iblock
      integer*4 isthp,istha
c
c              list of countries reporting wind in m/S.
c
      data iclist/2070,6250,2200,2250,4300,3010,6190,4110,1170,1200,
     1            6164,6210,4070,4240,3030,2060,2050,2230,1460,2126,
     2            2140,4040,1440,4190,5420,6200,2176,6240,1260,6320,
     3            2010,6310,3020,2220,6220,4170,14*0/
C 
      data ipoints/128*0/,iparams/39000*0/,implis/2000*0/
c
C     ------------------------------------------------------------------
c
c*          1.  Open target file and input files required
c               for station list creation.
 100  continue
c
c     Get return code from Empress under user control.
c
      ncount=50
c
c*          1.1 Open file containing list of important stations.
c               ------------------------------------------------
 110  continue
c
      open(unit=20,file='important_stations.dat',iostat=ios,
     1             form='formatted',
     2             recl=80,status='old')
      if(ios .ne.0) then
         print*,'Error opening important_stations file.'
         call exit(2)
      end if
c
c
c*          1.2 Open station file for binary station list.
c               ------------------------------------------
 120  continue
c
      open(unit=24,file='station_amend.dat',iostat=ios,
     1             form='unformatted',status='new')
c
      if(ios .ne.0) then
         print*,'Error opening station_amend.dat file.'
         call exit(2)
      end if
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

c
C*           1.3 Read in important station list.
C                ------------------------------
 130  continue
c
      J=0
c
 131  continue
      read(20,'(10(1x,i5))',iostat=ios,err=133,end=134) (in(i),i=1,10)
C
      DO 132 i=1,10
      j=j+1 
      implis(j)=in(i)
 132  continue
C
      go to 131
C
 133  continue
C
      print*,'Read error ',ios,'  on file important_stations.dat'
      call exit(2)
C
 134  continue 

      nimp=j
      icount=0
C   
      nwpt=1
      nbpt=0
C
      ipoints(1)=1
c
c*           2. Open WMO database.
c               ------------------
 200  continue
c
c
      iblockold=0
      i=0
      read(28,'(a)') cline
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
     7                                     cuat18
c
 8888 format(i4,1x,i2,1x,i3,1x,f7.2,1x,f7.2,1x,i4,1x,i4,1x,i1,
     1       1x,8(a2,1x),4(a,1x))
c
          if(iblockold.ne.iblock) then
             i=iblock
             ipoints(i)=nwpt
             do ii=i,1,-1
             if(ipoints(ii).eq.0) ipoints(ii)=nwpt
             end do
             iblockold=iblock
          end if
c
c         wmo region and country
c
          print*,'region_country=',irgcoun
c
c         Wmo region and country number
c
          irn=irgcoun/1000
          icoun=irgcoun-irn*1000
c
c         wmo block number
c
          print*,'block=',iblock
c
c         wmo station number
c
c
          print*,'block/station ',iblock,istno
c
c         type of station
c
c
c         latitude
c
c         latitude in hundreds of degree. All values positive.Sign
c         given by isglat 0-positive 1-negative
c
          isglat=0
          ilat=nint(rlat*100)
          if(ilat.lt.0) then
             isglat=1
             ilat=iabs(ilat)
          end if
c
c         longitude
c
c         longitude in hundreds of degree from 0-360
c
          ilon=nint(rlon*100)
          if(ilon.lt.0) ilon=36000-iabs(ilon)
c
c         check if pole
c
          if(ilat.eq.9000) ilon=0
c
c         pressure elevation
c
c
c         pressure elevation flag
c
c         ground elevation
c
          if(istha.gt.9000) then
             print*,'istha > 9000 error'
             call exit(2)
          end if
c
c         ground elevation flag
c
c
c         pressure level
c
c         observing practice
c
c
          icount=icount+1
c
c
c
c         knms - 0    wind in knots
c              - 1    wind in m/s       
C
          knms=0
          do 221 j=1,ncount
          if(iclist(j).eq.irgcoun) knms=1
 221      continuE
c     
c         station pressure elevation.
c         ---------------------------
c
c         h/p station pressure elevation or ground elevation if h/p
c         does not exist. if both are missing 9999 is given.
c
          ist_height=9999
c
          if(isthp.ne.0) then
             ist_height=isthp
          else 
             if(istha.ne.0) then
                ist_height=istha
             end if
          end if
c
c         sign of station heigth
c
          idsign=0
          if(ist_height.lt.0) then
            ist_height=iabs(ist_height)
            idsign=1
          end if
c
          istind=0
c
          if(iblock.ge.60.and.iblock.le.68) istind=1
          if(iblock.ge.80.and.iblock.le.89) istind=1
c
          if(istind.eq.1) go to 230
c
          do 222 jj=1,nimp
          if(istno.eq.implis(jj)) istind=1
 222      continue
c     
c         type of station.
c         ----------------
c 
 230      continue
c
          itype1=0
          itype2=0
          itype3=0
c
          if(csp00.eq.'X') itype3=1
          if(csp03.eq.'X') itype3=1
          if(csp06.eq.'X') itype3=1
          if(csp09.eq.'X') itype3=1
          if(csp12.eq.'X') itype3=1
          if(csp15.eq.'X') itype3=1
          if(csp18.eq.'X') itype3=1
          if(csp21.eq.'X') itype3=1
c
          if(cuat00.eq.'1'.or.cuat00.eq.'3'.or.
     1       cuat00.eq.'5'.or.cuat00.eq.'6') itype2=1
          if(cuat06.eq.'1'.or.cuat06.eq.'3'.or.
     1       cuat06.eq.'5'.or.cuat06.eq.'6') itype2=1
          if(cuat12.eq.'1'.or.cuat12.eq.'3'.or.
     1       cuat12.eq.'5'.or.cuat12.eq.'6') itype2=1
          if(cuat18.eq.'1'.or.cuat18.eq.'3'.or.
     1       cuat18.eq.'5'.or.cuat18.eq.'6') itype2=1
c
          if(cuat00.eq.'2'.or.cuat00.eq.'4') itype1=1
          if(cuat06.eq.'2'.or.cuat06.eq.'4') itype1=1
          if(cuat12.eq.'2'.or.cuat12.eq.'4') itype1=1
          if(cuat18.eq.'2'.or.cuat18.eq.'4') itype1=1

c
c 
c         pack information into iparams array.
c         every station informatio into three 32 bit words.
c
c
          ipp=ipcode
          call pack(32,iparams(nwpt),istno ,nwpt,nbpt,10)
          call pack(32,iparams(nwpt),0     ,nwpt,nbpt,06)
          call pack(32,iparams(nwpt),ipp   ,nwpt,nbpt,04)
          call pack(32,iparams(nwpt),knms  ,nwpt,nbpt,01)
          call pack(32,iparams(nwpt),0     ,nwpt,nbpt,03)
          call pack(32,iparams(nwpt),ist_height,nwpt,nbpt,14)
c-------------------------------------------------------------2/6
          call pack(32,iparams(nwpt),idsign,nwpt,nbpt,02)
          call pack(32,iparams(nwpt),ilon  ,nwpt,nbpt,16)
          call pack(32,iparams(nwpt),ilat  ,nwpt,nbpt,14)
c-------------------------------------------------------------3/6
          call pack(32,iparams(nwpt),isglat,nwpt,nbpt,01)
          call pack(32,iparams(nwpt),0     ,nwpt,nbpt,01)
          call pack(32,iparams(nwpt),irn   ,nwpt,nbpt,03)
          call pack(32,iparams(nwpt),icoun ,nwpt,nbpt,10)
          call pack(32,iparams(nwpt),0     ,nwpt,nbpt,03)
          call pack(32,iparams(nwpt),1     ,nwpt,nbpt,01)
          call pack(32,iparams(nwpt),istind,nwpt,nbpt,01)
          call pack(32,iparams(nwpt),itype1,nwpt,nbpt,01)
          call pack(32,iparams(nwpt),itype2,nwpt,nbpt,01)
          call pack(32,iparams(nwpt),itype3,nwpt,nbpt,01)
          call pack(32,iparams(nwpt),0     ,nwpt,nbpt,03)
c
          print*,'istno   =',istno
          print*,'ipcode  =',ipcode
          print*,'knms    =',knms
          print*,'id      =',ist_height
          print*,'ilon    =',ilon
          print*,'ilat    =',ilat
          print*,'isglat  =',isglat
          print*,'irn     =',irn
          print*,'icoun   =',icoun
          print*,'istind  =',istind 
          print*,'itype1  =',itype1
          print*,'itype2  =',itype2
          print*,'itype3  =',itype3
          print*,'--------------------------end'
c
          go to 220
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
c*          4.  write amended stations and close the file.
c               ------------------------------------------
 400  continue
c
c     open(unit=24,err=401,iostat=ios,
c    1             file='station_amend.dat',
c    1             form='unformatted',status='new')
c
      print*,ipoints 
      write(24) iparams,ipoints
c
      close(24)
c
      go to 410
c
 401  continue
c
      print*,'Error opening station_amend.dat'
      call exit(2)
c     -------------------------------------------------------------------
c*          4.1 close the file.
c               ---------------
 410  continue
c
      close(24)
c
c 
c
      return
      end
      SUBROUTINE PACK(KBPW,KD,KS,KWPT,KBPT,KSI)
C
C
C**** *PACK*
C
C
C     PURPOSE.
C     --------
C            PURPOSE OF THIS ROUTINE IS TO PACK VALUE *KS* IN
C         *KSI* BITS, STARTED AT WORD KWPT OF ARRAY *KD* AFTER
C         SKIPPING NBPT BITS.  AT THE END
C         POINTERS *NWPT* AND *NBPT* ARE ADJUSTED.
C
C**   INTERFACE.
C     ----------
C
C     *CALL* *PACK(KBPW,KD,KS,KWPT,KBPT,KSI)*
C
C            *KD*    - DESTINATION ARRAY.
C            *KS*    - SOURCE
C            *KSI*   - NUMBER OF BITS ACCUPIED BY KS.
C
C     *METHOD.
C      -------
C
C            NONE.
C
C
C     EXTERNALS.
C     ----------
C
C
C            *CALL SBYTE(KD,KS,KBPT,KSI)*
C
C            *KD*    - DESTINATION ARRAY.
C            *KS*    - SOURCE
C            *KBPT*  - POINTER TO BIT IN THE KD(KWPT)
C            *KSI*   - NUMBER OF BITS ACCUPIED BY KS.
C
C
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
C          M. D. DRAGOSAVAC    *ECMWF*       09/06/86.
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
C
      DIMENSION KD(*)
C
C     ------------------------------------------------------------------
C*          1.   SET UP BIT PATTERN.
C                -------------------
C
 100  CONTINUE
C
      CALL SBYTE(KD,KS,KBPT,KSI)
C
C     ------------------------------------------------------------------
C*          1.1  UPDATE WORD AND BIT POINTERS.
C                -----------------------------
 110  CONTINUE
C
      KBPT = KBPT + KSI
C
      IF(KBPT.GE.KBPW) THEN
                          KW  = KBPT/ KBPW
                          KBPT= KBPT - KW * KBPW
                          KWPT= KWPT +KW
                       END IF
C
      RETURN
      END
