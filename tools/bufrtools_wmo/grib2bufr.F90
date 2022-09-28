      PROGRAM GRIB2BUFR
!
!**** *GRIB2BUFR*
!
!
!     PURPOSE.
!     --------
!         Convert NCEP GRIB snow cover into BUFR
!         inserting grid point height and position
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
!          NONE.
!
!
!     EXTERNALS.
!     ----------
!
!
!     REFERENCE.
!     ----------
!
!          NONE.
!
!     AUTHOR.
!     -------
!
!          M. DRAGOSAVAC    *ECMWF*       JAN 2004
!
!
!     MODIFICATIONS.
!     --------------
!
!          NONE.
!
!
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
!
      integer, parameter :: is0=2
      integer, parameter :: is1=512
      integer, parameter :: is2=512
      integer, parameter :: ip2=128
      integer, parameter :: is3=2
      integer, parameter :: ip3=2
      integer, parameter :: is4=512
      integer, parameter :: ip4=1200000
      integer, parameter :: ileng=1200000
!
      character(len=1) hoper
!
      integer, dimension(is0) :: isec0
      integer, dimension(is1) :: isec1
      integer, dimension(is2) :: isec2
      real   , dimension(ip2) :: psec2
      integer, dimension(is3) :: isec3
      real   , dimension(ip3) :: psec3
      integer, dimension(is4) :: isec4
      real   , dimension(ip4) :: psec4
      integer, dimension(ileng):: igrib
      real   , dimension(ip4) :: fpdata
!
      character(len=256) carg(4)
      character(len=256) input_file
      character(len=256) out_file
!
      REAL               :: LSM(1024,1024), GPH(1024,1024)
      REAL               :: FDATA(1024,1024)
      INTEGER            :: ILON(1024,1024)
      INTEGER            :: ILAT(1024,1024)

!      external getarg
!                                                                       
!     ------------------------------------------------------------------
!*          1. INITIALIZE CONSTANTS AND VARIABLES.
!              -----------------------------------
 100  CONTINUE
!
!     Missing value indicator
! 
      jbyte=ileng*4
      nbytpw=jbpw/8
      RVIND=1.7E38
      nvind=2147483647
      eps=10.E-10
      n=0
      input_file=' '
      out_file=' '
!
!     Input file name
!
!     Get input and output file name.
!
      narg=IARGC()
!
      IF(narg /=4) THEN
         print*,'Usage -- grib2bufr -i infile -o outfile ' 
         stop
      END IF
!
      do j=1,narg
       call getarg(j,carg(j))
      end do
!
      do j=1,narg
      if(carg(j) == '-i') then
         input_file=carg(j+1)
      elseif(carg(j) == '-o') then
         out_file=carg(j+1)
      end if
      end do
!
!
!*          1.2 Open input and output files
!               ---------------------------
 120  CONTINUE
!
      IRET=0 
      CALL PBOPEN(IUNIT,trim(INPUT_FILE),'r',IRET)
      IF(IRET.EQ.-1) STOP 'open failed'
      IF(IRET.EQ.-2) STOP 'Invalid file name'
      IF(IRET.EQ.-3) STOP 'Invalid open mode specified'
!
      IRET=0
      CALL PBOPEN(IUNIT1,trim(OUT_FILE),'w',IRET)
      IF(IRET.EQ.-1) STOP 'open failed'
      IF(IRET.EQ.-2) STOP 'Invalid file name'
      IF(IRET.EQ.-3) STOP 'Invalid open mode specified'

!     ----------------------------------------------------------------- 
!*          2. SET REQUEST FOR EXPANSION.
!              --------------------------
 200  CONTINUE
!
!
!*          2.1 SET REQUEST FOR PARTIAL EXPANSION.
!               ----------------------------------
 210  CONTINUE
!
      
!
!     -----------------------------------------------------------------
!*          3.  READ GRIB MESSAGE.
!               ------------------
 300  CONTINUE
!
      IERR=0
      KBUFL=0
!
      IRET=0
      CALL PBGRIB(IUNIT,igrib,JBYTE,KBUFL,IRET) 
      IF(IRET.EQ.-1) THEN
         print*,'Number of subsets     ',iobs
         print*,'Number of messages    ',n
         STOP 'EOF'
      END IF
      IF(IRET.EQ.-2) STOP 'File handling problem' 
      IF(IRET.EQ.-3) STOP 'Array too small for product'
!
      N=N+1
      print*,'----------------------------------',n
!
!     -----------------------------------------------------------------
!*          4. EXPAND GRIB MESSAGE.
!              --------------------
 400  CONTINUE
!
      hoper='D'
!      hoper='B'     ! images
      i=1
      call grsdbg(i)
      psec3(2)=1.7E38
      call gribex(isec0,isec1,isec2,psec2,isec3,psec3,isec4,psec4, &
&                  ip4,igrib,ileng,iword,hoper,iret)
!
!     The snow cover is needed only ( NCEP parameter 238)
!
      if(isec1(6) /= 238) go to 300
!
      DO II = 1,1024
       IL = (II-1) * 1024 + 1
       IH = (II) * 1024
       FDATA(1:1024,II) = PSEC4(IL:IH)
      ENDDO

!
!
!           4.1  Read Coordinates
!                ----------------
      CALL READ_COORD(ILON,ILAT)
!
!           4.2 Read Land/Sea Mask on polarstereographic projection
!               based on ECMWF 2'30'' DM data
!
      CALL READ_LSM(LSM)
!
!           4.3 Read topography data on polarstereographic projection
!               based on ECMWF 2'30'' DM data
!
      CALL READ_GPH(GPH)
!
!           4.4 Screen and thin data
!               --------------------

!           5.  Pack into BUFR
!               --------------

      CALL BUFR_ENCODE(IUNIT1,ISEC1,ISEC2,FDATA,ILON,ILAT,LSM,GPH,IERR)
      
      GO TO 300
!     -----------------------------------------------------------------
!
 900  CONTINUE
!
      CALL PBCLOSE(IUNIT,IRET)
      CALL PBCLOSE(IUNIT1,IRET)
!
      END
      SUBROUTINE BUFR_ENCODE(KUNIT,IGSEC1,IGSEC2,GPSEC4,KLON,KLAT,LSM,GPH,IERR)
!
!**** *BUFR_ENCODE*
!
!
!     PURPOSE.
!     --------
!         Pack bufr message
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
!          NONE.
!
!
!     EXTERNALS.
!     ----------
!
!         CALL BUSEL
!         CALL BUFREX
!         CALL BUFREN
!         CALL BUPRS0
!         CALL BUPRS1
!         CALL BUPRS2
!         CALL BUPRS3
!         CALL BUPRT
!         CALL BUUKEY
!
!     REFERENCE.
!     ----------
!
!          NONE.
!
!     AUTHOR.
!     -------
!
!          M. DRAGOSAVAC    *ECMWF*       15/06/93.
!
!
!     MODIFICATIONS.
!     --------------
!
!          NONE.
!
!
      IMPLICIT LOGICAL(O), CHARACTER*8(C,H,Y)
!
      integer, parameter :: JSUP =   9
      integer, parameter :: JSEC0=3
      integer, parameter :: JSEC1=40
      integer, parameter :: JSEC2=4096
      integer, parameter :: JSEC3=4
      integer, parameter :: JSEC4=2
      integer, parameter :: JBUFL=100000
      
      integer, parameter :: KDLEN=100
      integer, parameter :: KELEM=9
      integer, parameter :: kdleng=100
      integer, parameter :: KVALS=360000
!
      integer, dimension(JBUFL) :: KBUFR
      integer, dimension(JSUP)  :: KSUP
      integer, dimension(JSEC0) :: KSEC0
      integer, dimension(JSEC1) :: KSEC1
      integer, dimension(JSEC2) :: KSEC2
      integer, dimension(JSEC3) :: KSEC3
      integer, dimension(JSEC4) :: KSEC4
!

      real*8, dimension(KVALS) :: VALUES
!
      integer, dimension(KELEM) :: KTDLST
      integer, dimension(KELEM) :: KTDEXP
      integer, dimension(KDLEN) :: KDATA
!
      integer, dimension(*) :: igsec1,igsec2
      integer, dimension(1024,1024) :: klon,klat
      real   , dimension(1024,1024) :: gph, gpsec4, lsm
!
      character(len=64) :: CNAMES(KELEM)
      character(len=24) :: CUNITS(KELEM)
      character(len=80) :: CVALS(10),YENC

      integer :: year, month, day, hour, minute
!
      real*8, parameter :: RVIND=1.7E38
!     ------------------------------------------------------------------
!*          1. INITIALIZE CONSTANTS AND VARIABLES.
!              -----------------------------------
 100  CONTINUE
!
!
!     INITIALIZE DELAYED REPLICATION FACTORS OR REFERENCE VALUES ETD.
!
      kdata=0
!
      kdata(1)=0
!
!
!     SET DATA DECSRIPTORS
!
      ktdlst(1)=004001         ! year
      ktdlst(2)=004002         ! month
      ktdlst(3)=004003         ! day
      ktdlst(4)=004004         ! hour
      ktdlst(5)=004005         ! minute
      ktdlst(6)=005001         ! latitude
      ktdlst(7)=006001         ! longitude
      ktdlst(8)=007007         ! height
      ktdlst(9)=020065         ! snow cover
!
      KTDLEN=9
!
!     SET VALUES TO BE PACKED
!
      year=igsec1(10)
      month=igsec1(11)
      day=igsec1(12)
      hour=igsec1(13)
      minute=igsec1(14)
!
      do i=1,1024
         isubs=0
      do j=1,1024
       if(lsm(i,j) > 0.5) then
          isubs=isubs+1
          ivv=(isubs-1)*kelem
          if(year < 30 ) then
             values(1+ivv)=year+2000
          else
             values(1+ivv)=year+1900
          end if
          values(2+ivv)=month
          values(3+ivv)=day
          values(4+ivv)=hour
          values(5+ivv)=minute
          values(6+ivv)=klat(i,j)/100.
          values(7+ivv)=klon(i,j)/100.
          values(8+ivv)=gph(i,j)
          values(9+ivv)=gpsec4(i,j)
       end if
      end do

      print*,'Number of subsets=',isubs
      if(isubs == 0 ) cycle
!
!     set ccittia5 call sign 
!
      cvals(1)=' '
!
!     SECTION 0 CONTENT
!
      KSEC0(1)=0      ! TOTAL LENGTH OF SECTION 0
      KSEC0(2)=0      ! TOTAL LENGTH OF BUFR MESSAGE
      KSEC0(3)=3      ! BUFR EDITION NUMBER
!
!     SECTION 1 CONTENT
!
      KSEC1(1)=18    ! TOTTAL LENGTH OF SECTION 1
      KSEC1(2)=3     ! BUFR EDITION NUMBER
      KSEC1(3)=98    ! ORIGINATING CENTRE
      KSEC1(4)=1     ! UPDATE SEQUENCE NUMBER
      KSEC1(5)=0  !128   ! FLAG (PRESENCE OF SECTION 2)
      KSEC1(6)=12     ! BUFR MESSAGE TYPE
      KSEC1(7)=165     ! BUFR_MESSAGE SUBTYPE
      KSEC1(8)=1     ! VERSION NUMBER OF LOCAL TABLE USED
      KSEC1(9)=year
      KSEC1(10)=month
      KSEC1(11)=day
      KSEC1(12)=hour
      KSEC1(13)=minute
      KSEC1(14)=0    ! BUFR MASTER TABLE( ZERO) FOR METEOROLOGICAL DATA)
      KSEC1(15)=11    ! VERSION NUMBER OF MASTER TABLE USED
      KSEC1(16)=0    ! ORIGINATING SUB-CENTRE
      KSEC1(17)=0    ! 
      KSEC1(18)=0    ! Local ADP centre information
!
!     SECTION 2 CONTENT
!
      KSEC2(1)=52
!
      KSEC2=0
!
!     SECTION 3 CONTENT
!
      KSEC3(1)=0     ! TOTAL LENGTH OF SECTION 3
      KSEC3(2)=0     ! RESERVED
      KSEC3(3)=isubs
      KSEC3(4)=192     ! 192 FOR COMPRESSION/ 128 MANY SUBSETS
!
      IREP=0
!
!
!*          6. PACK BUFR MESSAGE
!              -----------------
 600  CONTINUE
!
      KBUFL=JBUFL
      KERR=0
      CALL BUFREN( KSEC0,KSEC1,KSEC2,KSEC3,KSEC4, &
&                  KTDLEN,KTDLST,KDLENG,KDATA,KELEM, &
&                  KVALS,VALUES,CVALS,KBUFL,KBUFR,KERR)
!
      IF(KERR.GT.0) CALL EXIT(2)
!
!           7. WRITE BUFR OUT
!              --------------
 700  CONTINUE
!
      IBYTES=KBUFL*4
!
      IRET=0
      CALL PBWRITE(KUNIT,KBUFR,IBYTES,IRET)
      IF(IRET < 0) THEN
         print*,'Error during writing bufr data ',iret
         call exit(2)
      END IF
!
      end do
!     -----------------------------------------------------------------
!
 900  CONTINUE
!
      RETURN
      END

      SUBROUTINE READ_COORD(ILON,ILAT)
!-------------------------------------------------------------------------------
!     Read in lat / lon data for stereographic projection, return
!     1024 * 1024 data arrray ILON, ILAT
!-------------------------------------------------------------------------------
IMPLICIT NONE
INTEGER :: ILON(1024,1024),jj,ii
INTEGER :: ILAT(1024,1024)
CHARACTER(LEN=256) :: cppbase
!
cppbase=' '
call getenv('PP_BASE',cppbase)


OPEN(15,file =trim(cppbase)//'/dat/'//'nesdis_lat.dat', status='old', form='formatted')
do ii = 1, 1024
 do jj = 1,1024
  READ(15,'(I10)') ILAT(ii,jj)
 enddo
enddo
CLOSE(15)
!
OPEN(16,file =trim(cppbase)//'/dat/'//'nesdis_lon.dat', status='old', form='formatted')
do ii = 1, 1024
 do jj = 1,1024
  READ(16,'(I10)') ILON(ii,jj)
 enddo
enddo
CLOSE(16)
!
      END SUBROUTINE READ_COORD

      SUBROUTINE READ_LSM(LSM)
!-------------------------------------------------------------------------------
!     Read in lat / lon data for stereographic projection, return
!     1024 * 1024 data arrray LSM
!-------------------------------------------------------------------------------
IMPLICIT NONE
INTEGER :: II,JJ
REAL :: LSM(1024,1024)
CHARACTER(LEN=256) :: cppbase
!
cppbase=' '
call getenv('PP_BASE',cppbase)
OPEN(17,file = trim(cppbase)//'/dat/'//'lsm_polar.dat', status='old', form='formatted')
do ii = 1, 1024
 do jj = 1, 1024
   read(17,'(1x,f10.4)') lsm(ii,jj)
 enddo
enddo

CLOSE(17)
!
      END SUBROUTINE READ_LSM
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
      SUBROUTINE READ_GPH(GPH)
!-------------------------------------------------------------------------------
!     Read in lat / lon data for stereographic projection, return
!     1024 * 1024 data arrray GPH
!-------------------------------------------------------------------------------
IMPLICIT NONE
INTEGER :: II,JJ
REAL :: GPH(1024,1024)
CHARACTER(LEN=256) :: cppbase
!
cppbase=' '
call getenv('PP_BASE',cppbase)
OPEN(18,file = trim(cppbase)//'/dat/'//'oro_polar.dat', status='old', form='formatted')
do ii = 1, 1024
 do jj = 1, 1024
   read(18,'(1x,f10.4)') gph(ii,jj)
 enddo
enddo

CLOSE(18)
!
      END SUBROUTINE READ_GPH
