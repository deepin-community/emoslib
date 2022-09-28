 program tc_tracks
!
!
!**** tc_tracks*
!
!
!     Purpose.
!     --------
!
!         Create tropical cyclone tracks
!
!
!**   Interface.
!     ----------
!
!
!     Method.
!     -------
!
!
!
!
!
!     Externals.
!     ----------
!
!
!
!     Reference.
!     ----------
!
!
!
!     Author.
!     -------
!
!          M. Dragosavac    *ECMWF*      22/12/2001 
!
!
!     Modifications.
!     --------------
!
!          NONE.
!---------------------------------------------------------------------------
 USE bufr_module

 implicit none

! Local variables

  integer              :: narg, iargc, n, nn
  integer              :: iii,j,ios,ij,ii
  integer              :: iunit, iret,ierr, ilen
  integer              :: kdlen
  integer              :: second, subset, iunit1

  character(len=256)   :: input_file
  character(len=256)   :: output_file
  character(len=256)   :: det_file
  character(len=256), dimension(10) :: carg
  character(len=1024)   :: line
  character(len=3)     :: tc_identifier
  character(len=10)    :: tc_name

  integer             :: increment(100)
  integer             :: member
  integer             :: originating_centre
  integer             :: number_of_members
  integer             :: member_number(100)
  integer             :: number_of_steps(100)
  integer             :: is_missing_step(100,100)

  real                :: lat_observed
  real                :: lon_observed
  real                :: lat_first,lon_first
  real                :: lat_pressure(100,100)
  real                :: lon_pressure(100,100)
  real                :: lat_wind(100,100)
  real                :: lon_wind(100,100)
  integer             :: year
  integer             :: month
  integer             :: day
  integer             :: hour
  integer             :: minute
  integer             :: key2(46)
  integer             :: key3(46)
  integer             :: kerr
  integer             :: jj                             ! loop variable
  real                :: wind(100,100)
  real                :: pressure(100,100)
  real                :: missing
  integer             :: edition


  integer, parameter :: kel=1000

! Subroutine interface


! ----------------------------------

!  initialization of variables

  missing=rvind
  increment=0
  member_number=0
  number_of_steps=0
  is_missing_step=0
  lat_pressure=missing
  lon_pressure=missing
  lat_wind=missing
  lon_wind=missing
  lat_observed=missing
  lon_observed=missing
  lat_first=missing
  lon_first=missing
  wind=missing
  pressure=missing

!   Get program arguments

    input_file=' '
    output_file=' '

    narg=iargc()

    if(narg < 3) then
       print*,'Usage -- tc_tracks_eps -i input_file -o eps_file '
       print*,'      -4 option to produce BUFR edition 4'
       print*,'      input_file   -- input file name'
       print*,'      eps_file     -- eps file name'
       stop
    end if

    do  j=1,narg
       call getarg(j,carg(j))
    end do


    edition=3
    do j=1,narg
     if(carg(j) == '-i') then
       input_file=carg(j+1)
     elseif(carg(j) == '-o') then
       output_file=carg(j+1)
     elseif(carg(j) == '-4') then
       edition=4
     else
       cycle
     end if
    end do

    write(0,*) 'Input  file name='//trim(input_file)
    write(0,*) 'Output file name='//trim(output_file)


!-- Open input and output files
!   ---------------------------


!   Open input file

    open(21,file=trim(input_file),iostat=ios)
    if(ios /= 0) then
      print*,'Open error on input file '
      stop
    end if

!   Open output file

    iret=0
    call pbopen(iunit,trim(output_file),'w',iret)
    if(iret == -1) then
       print*,'Open failed'
       stop
    elseif(iret == -2) then
       print*,'Invalid input file name'
       stop
    elseif(iret == -3) then
       print*,'Invalid open mode specified'
       stop
    end if

!   Read input file
  
    subset=0
    number_of_members=0
    number_of_steps=0
    values=missing
    
    do while( ios == 0 )
    
    line=' '
    read(21,'(a)', iostat=ios) line
!   write(0,*) trim(line)

    if( ios == 0 ) then
      if(index(line,'originating centre') /= 0 ) then
         read(line,'(47x,i3)',iostat=ios) originating_centre
         write(*,'(47x,i3)',iostat=ios) originating_centre
         if(ios /= 0 ) then
            print*,'Internal read error for originating_centre'
            call exit(2)
         end if
      elseif(index(line,'TC identifier') /= 0 ) then
         read(line,'(16x,a3)',iostat=ios) tc_identifier
         write(*,'(16x,a3)',iostat=ios) tc_identifier
         if(ios /= 0 ) then
            print*,'Internal read error for tc_identifier'
            call exit(2)
         end if
      elseif(index(line,'TC name') /= 0 ) then  
         read(line,'(10x,a10)',iostat=ios) tc_name
         write(*,'(10x,a10)',iostat=ios) tc_name
         if(ios /= 0 ) then
            print*,'Internal read error for tc_name'
            call exit(2)
         end if
      elseif(index(line,'YYYYMMDDHHMM') /= 0 ) then
         read(line,'(36x,i4,i2,i2,i2,i2)',iostat=ios) year, month, day, hour, minute
         write(*,'(36x,i4,i2,i2,i2,i2)',iostat=ios) year, month, day, hour, minute
         if(ios /= 0 ) then
            print*,'Internal read error for date/time'
            call exit(2)
         end if
      elseif(index(line,'obs lat') /= 0 ) then
         read(line,'(14x,f6.2)',iostat=ios) lat_observed
         write(*,'(14x,f6.2)',iostat=ios) lat_observed
         if(ios /= 0 ) then
            print*,'Internal read error for obs lat'
            call exit(2)
         end if
      elseif(index(line,'obs lon') /= 0 ) then
         read(line,'(13x,f7.2)',iostat=ios) lon_observed
         write(*,'(13x,f7.2)',iostat=ios) lon_observed
         if(ios /= 0 ) then
            print*,'Internal read error for obs lon'
            call exit(2)
         end if
      elseif(index(line,'member') /= 0 ) then
         iii=0
         read(line,'(46x,i2)',iostat=ios) member
         write(*,'(46x,i2)',iostat=ios) member
         if(ios /= 0 ) then
            print*,'Internal read error for member'
            call exit(2)
         end if
         number_of_members=number_of_members+1
         member_number(number_of_members)=member
      elseif(index(line,'number of steps') /= 0 ) then
         read(line,'(46x,i2)',iostat=ios) number_of_steps(number_of_members)
         write(*,'(46x,i2)',iostat=ios) number_of_steps(number_of_members)
         if(ios /= 0 ) then
            print*,'Internal read error for number of steps'
            call exit(2)
         end if
      elseif(index(line,'increment') /= 0 ) then
         read(line,'(46x,i2)',iostat=ios) increment(number_of_members)
         write(*,'(46x,i2)',iostat=ios) increment(number_of_members)
         if(ios /= 0 ) then
            print*,'Internal read error for increment'
            call exit(2)
         end if
      elseif(index(line,'missing') /= 0 ) then
           iii=iii+1
           is_missing_step(number_of_members,iii)=1
      else
           iii=iii+1
           is_missing_step(number_of_members,iii)=0
           read(line,'(4x,f6.2,3x,f7.2,3x,f7.2,4x,f6.2,4x,f6.2,3x,f7.2)',iostat=ios) lat_pressure(number_of_members,iii), &
                            lon_pressure(number_of_members,iii),pressure(number_of_members,iii), &
                            wind(number_of_members,iii),lat_wind(number_of_members,iii), &
                            lon_wind(number_of_members,iii)
           if (lat_wind(number_of_members,iii) == 0 .and. ( lon_wind(number_of_members,iii) == -360  &
               .or. lon_wind(number_of_members,iii) == 0)) then
            lat_wind(number_of_members,iii)=missing
            lon_wind(number_of_members,iii)=missing
           endif
           write(*,'(4x,f6.2,3x,f7.2,3x,f7.2,4x,f6.2,4x,f6.2,3x,f7.2)',iostat=ios) lat_pressure(number_of_members,iii), &
                            lon_pressure(number_of_members,iii),pressure(number_of_members,iii), &
                            wind(number_of_members,iii),lat_wind(number_of_members,iii), &
                            lon_wind(number_of_members,iii)
           pressure(number_of_members,iii)=pressure(number_of_members,iii)*100
           if (lat_first == missing) lat_first=lat_pressure(number_of_members,iii)
           if (lon_first == missing) lon_first=lon_pressure(number_of_members,iii)


!          print*,member,' ',iii,'----',lat(member,iii),lon(member,iii),pressure(member,iii),wind(member,iii)
           if(ios /= 0 ) then
              print*,'Internal read error for increment'
              call exit(2)
           end if
      end if
    else
       print*,'END of data....................'
       exit
    end if
    end do

    if (lon_observed /= missing) lon_first=lon_observed
    if (lat_observed /= missing) lat_first=lat_observed


      cvals(1)=tc_identifier
      cvals(2)=tc_name
      subset=0

      do iii=1,number_of_members

        subset=subset+1

        ij=(subset-1)*kel

        n=1+ij
        values(  n)=98.      ! Originating centre
        n=2+ij
        values(  n)=missing  ! Originating subcentre
        n=3+ij
        values(  n)=1.       ! Generation application
        n=4+ij
        values(  n)=1003.    ! Storm identifier
        n=5+ij
        values(  n)=2010.    ! Storm name
        n=6+ij
        values(  n)=2.       ! singular vector
        n=7+ij
        values(  n)=member_number(iii)    ! Ensemble member number
        n=8+ij
        if (member_number(iii)==51) then
          values(  n)=1        ! unperturbed low resol. control forecast
        else if (member_number(iii)==52) then
          values(  n)=0        ! unperturbed high resol. control forecast
        else
          values(  n)=255      ! missing value because pos/neg perurbations do not make sense
        end if
!+++++++++++++++++++++

        n=9+ij
        values(  n)=year
        n=10+ij
        values(  n)=month
        n=11+ij
        values(  n)=day
        n=12+ij
        values(  n)=hour
        n=13+ij
        values(  n)=minute
!+++++++++++++++++++++++++
        n=14+ij
        values(  n)=1.             ! Storm centre
        n=15+ij
        values(  n)=lat_observed
        n=16+ij
        values(  n)=lon_observed
        n=17+ij
        values(  n)=4.             ! Location of the storm in the perturbed analysis
        n=18+ij
        if (is_missing_step(iii,1) == 0) values(  n)=lat_pressure(iii,1)
        n=19+ij
        if (is_missing_step(iii,1) == 0) values(  n)=lon_pressure(iii,1)
        n=20+ij
        if (is_missing_step(iii,1) == 0) values(  n)=pressure(iii,1)
        n=21+ij
        values(  n)=3.
        n=22+ij
        if (is_missing_step(iii,1) == 0) values(  n)=lat_wind(iii,1)
        n=23+ij
        if (is_missing_step(iii,1) == 0) values(  n)=lon_wind(iii,1)
        n=24+ij
        if (is_missing_step(iii,1) == 0) values(  n)=wind(iii,1)
!+++++++++++++++++++++++++
        n=25+ij
        values(  n)=maxval(number_of_steps(1:51))-1       ! delayed replication up to any forecast steps

        nn=25
        do ii=2,maxval(number_of_steps(1:51))
        !do ii=2,number_of_steps(iii)
          !if (is_missing_step(iii,ii) == 1) continue
          nn=nn+1
          n=nn+ij
          values(  n)=4.         ! time significance  008021 forecast
          nn=nn+1
          n=nn+ij
          values(  n)=(ii-1)*increment(iii)  ! time displacement
          nn=nn+1
          n=nn+ij
          values(  n)=1.           ! 008005 storm centre
          nn=nn+1
          n=nn+ij
          values(  n)=lat_pressure(iii,ii)        !
          nn=nn+1
          n=nn+ij
          values(  n)=lon_pressure(iii,ii)
          nn=nn+1
          n=nn+ij
          values(  n)=pressure(iii,ii)
          nn=nn+1
          n=nn+ij
          values(  n)=3.           ! 008005  location of max wind
          nn=nn+1
          n=nn+ij
          values(  n)=lat_wind(iii,ii)        !
          nn=nn+1
          n=nn+ij
          values(  n)=lon_wind(iii,ii)
          nn=nn+1
          n=nn+ij
          values(  n)=wind(iii,ii)
        end do

        end do

        ktdlst( 1)=001033
        ktdlst( 2)=001034
        ktdlst( 3)=001032
        ktdlst( 4)=001025
        ktdlst( 5)=001027
        ktdlst( 6)=001090
        ktdlst( 7)=001091
        ktdlst( 8)=001092
        ktdlst(09)=301011
        ktdlst(10)=301012
        ktdlst(11)=008005
        ktdlst(12)=005002
        ktdlst(13)=006002
        ktdlst(14)=008005
        ktdlst(15)=005002
        ktdlst(16)=006002
        ktdlst(17)=010051
        ktdlst(18)=008005
        ktdlst(19)=301023
        ktdlst(20)=011012
        ktdlst(21)=108000
        ktdlst(22)=031001
        ktdlst(23)=008021
        ktdlst(24)=004024
        ktdlst(25)=008005
        ktdlst(26)=301023
        ktdlst(27)=010051
        ktdlst(28)=008005
        ktdlst(29)=301023
        ktdlst(30)=011012

        ktdlen=30

!          Create bufr message
        if (edition==3) then
          ksec0( 3)=3
          ksec1( 1)=18
          ksec1( 2)=3
       else if (edition==4) then
          ksec0( 3)=4
          ksec1( 1)=22
          ksec1( 2)=4
        end if

        ksec1( 3)=98
        ksec1( 4)=1
        ksec1( 5)=128        ! 128 section 2 exists
        ksec1( 6)=7
        ksec1( 7)=32
        ksec1( 8)=0
        if(values(9) > 2000.) then
           ksec1( 9)=nint(values(9))-2000
        else
           ksec1( 9)=nint(values(9))-1900
        end if
        ksec1(10)=nint(values(10))
        ksec1(11)=nint(values(11))
        ksec1(12)=nint(values(12))
        ksec1(13)=nint(values(13))
        ksec1(14)=0
        ksec1(15)=16
        ksec1(16)=0
        ksec1(17)=0
        ksec1(18)=0

        ksec2(1)=52 ! this is required to encode section 2

! define section 2
! initialise key2 variable
        do jj=1,46
          key2(jj)=0
        enddo

        key2(1)=52                 ! Length of section 2
        key2(2)=8                  ! RDB type
        key2(3)=32                 ! RDB subtype
        key2(4)=nint(values(09))    ! Year
        key2(5)=nint(values(10))   ! Month
        key2(6)=nint(values(11))   ! Day
        key2(7)=nint(values(12))   ! Hour
        key2(8)=nint(values(13))   ! Minute
        key2(9)=0                  ! Second

        key2(10)=NINT(lon_first*100000.+18000000)    ! Longitude 1 location of first storm centre
        key2(11)=NINT(lat_first*100000.+9000000)     ! Latitude 1 location of first storm centre
        key2(12)=NINT(lon_first*100000.+18000000)    ! Longitude 2 location of first storm centre
        key2(13)=NINT(lat_first*100000.+9000000)     ! Latitude 2 location of first storm centre
        key2(14)=subset    ! no of observations/subsets

! identifier	
        key2(16)=ichar(tc_identifier(1:1))           ! first character of short identifier
        key2(17)=ichar(tc_identifier(2:2))           ! second character of short identifier
        key2(18)=ichar(tc_identifier(3:3))           ! third character of short identifier
        key2(19)=32
        key2(20)=32
        key2(21)=32
        key2(22)=32
        key2(23)=32
        key2(24)=32

        ksec3(1)=0
        ksec3(2)=0
        ksec3(3)=subset
        ksec3(4)=0
        if(ksec3(3).gt.1) ksec3(4)=64

        kdlen=1
        kdata(1)=maxval(number_of_steps(1:51))-1  ! first step is pert. anal.
        kbufl=jbufl

! pack rdb key into ksec2 array

        call bupkey(key2,ksec1,ksec2,kerr)

        if(kerr.ne.0) then
          print*,'BUPKEY: error',kerr
          call exit(2)
        end if

! encode

        ierr=0
        call bufren(ksec0,ksec1,ksec2,ksec3,ksec4, &
                    ktdlen,ktdlst,kdlen,kdata,kel, &
                    kvals,values,cvals,kbufl,kbufr,ierr)
        if(ierr > 0) then
           print*,'bufren error ', ierr
           call exit(2)
        elseif(ierr < 0) then
           print*,'Encoding return_code=',ierr
           call exit(2)
        end if


!          Write bufr message into output file

        ilen=kbufl*4

        call pbwrite(iunit,kbufr,ilen,ierr)
        print*,'length=',ilen,' ierr=',ierr
        print*,'Bufr message written into output file'

 end program tc_tracks
