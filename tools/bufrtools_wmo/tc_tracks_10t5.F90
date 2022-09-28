 program tc_track_10t5
!
!
!**** tc_track_10t5*
!
!
!     Purpose.
!     --------
!
!         Create tropical cyclone tracks up to 5 day forecast
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
  integer              :: iii,j,ios,ij,ii, i, ikbufl
  integer              :: iunit, iret, ilen, iunitin
  integer              :: kdlen, kel, idis
  integer              :: i_001091, i_031001, i_004024
  integer              :: ivo,k,iv,maxrep

  character(len=256)   :: input_file
  character(len=256)   :: output_file
  character(len=256)   :: displacement
  character(len=256), dimension(10) :: carg


! ----------------------------------


!   Get program arguments


    input_file=' '
    output_file=' '
    displacement=' '
    values=rvind
    vals=rvind
    narg=iargc()

    if(narg < 2) then
       print*,'Usage -- tc_tracks_10t5 -i input_file -o output_file -d displacement'
       print*,'      input_file   -- input file name'
       print*,'      output_file  -- output file name'
       print*,'      displacement -- step displacement in hours'
       stop
    end if

    do  j=1,narg
       call getarg(j,carg(j))
    end do


    do j=1,narg
     if(carg(j) == '-i') then
       input_file=carg(j+1)
     elseif(carg(j) == '-o') then
       output_file=carg(j+1)
     elseif(carg(j) == '-d') then
       displacement=carg(j+1) 
     else
       cycle
     end if
    end do

    read(displacement,fmt='(i3.3)') idis
    write(0,*) 'Input  file name='//trim(input_file)
    write(0,*) 'Output file name='//trim(output_file)
    write(0,*) 'step            =',idis


!-- Open input and output files
!   ---------------------------

    call buivar(1)

!   Open input file

    iret=0
    call pbopen(iunitin,trim(input_file),'r',iret)
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



 300  continue   

      iret=0
      call pbbufr(iunitin,kbuff,jbufl,kbufl,iret)
      if(iret.eq.-1) then
           call pbclose(iunitin,iret)
           call pbclose(iunit,iret)
           stop 'EOF'
      end if
      if(iret.eq.-2) stop 'file handling problem'
      if(iret.eq.-3) stop 'array too small for product'
 
      ikbufl=kbufl
      kbufl=kbufl/4+1


      call bus012(kbufl,kbuff,ksup,ksec0,ksec1,ksec2,iret)
      if(iret /=0) then
         print*,'error in bus012: ',iret
         print*,' bufr message number ',n,' corrupted.'
         iret=0
         go to 300
      end if
 
      if(ksup(6) > 1) then
         kel=jwork/ksup(6)
      else
         kel=kelem
      end if
 
      call bufrex(kbufl,kbuff,ksup,ksec0 ,ksec1,ksec2 ,ksec3 ,ksec4, &
                  kel,cnames,cunits,kvals,values,cvals,iret)
 
      if(iret /=0) then
         call exit(2)
      end if
 
      call busel(ktdlen,ktdlst,ktdexl,ktdexp,iret)
      if(iret /=0) call exit(2)
!
      i_001091=0
      i_031001=0
      i_004024=0

      do i=1,30
        if(ktdexp(i) == 1091) i_001091=i
        if(ktdexp(i) == 31001) i_031001=i
        if(ktdexp(i) == 4024 .and. i_004024 == 0) i_004024=i
      end do

      ksec1(5)=0
      



      do i=1,ksec3(3)
        ii=(i-1)*kel
      do j=1,24
      vals(j+ii)=values(j+ii)
      end do
      end do

!     The following loop will get rid of unwanted steps


      do i=1,ksec3(3)
        ii=(i-1)*kel
        ivo=i_004024-1+ii
        iv=i_004024-1+ii
        k=0
      do j=i_004024,ktdexl,10
        ivo=j+ii-1
        if(abs(values(ivo+1)-rvind)/rvind > eps ) then
           if(mod(nint(values(ivo+1)),idis) == 0 .and. ktdexp(j) == 004024) then
              vals(iv:iv+9)=values(ivo:ivo+9)
              iv=iv+10
              k=k+1
           end if 
        else
           if(ksec3(4) == 64) then
           vals(iv:iv+9)=values(ivo:ivo+9)
           iv=iv+10
           k=k+1
           end if
        end if
      end do
      kdata(i)=k
      vals(i_031001+ii)=k
      end do
      kdlen=ksec3(3)

      if(ksec3(4) == 64) then
         maxrep=kdata(1)
         do i=2,kdlen
          if(kdata(i) > maxrep) maxrep=kdata(i)
         end do
            
         do i=1,ksec3(3)
            ii=(i-1)*kel
            vals(i_031001+ii)=maxrep
            kdata(i)=maxrep
         end do
      end if

! encode


      iret=0
      
      call bufren(ksec0,ksec1,ksec2,ksec3,ksec4, &
                  ktdlen,ktdlst,kdlen,kdata,kel, &
                  kvals,vals,cvals,kbufl,kbufr,iret)
      if(iret > 0) then
         print*,'bufren error ', iret
         call exit(2)
      elseif(iret < 0) then
         print*,'Encoding return_code=',iret
      end if


!     Write bufr message into output file

      ilen=kbufl*4

      call pbwrite(iunit,kbufr,ilen,iret)
      print*,'length=',ilen,' iret=',iret
      print*,'Bufr message written into output file'

      go to 300


 end program tc_track_10t5
