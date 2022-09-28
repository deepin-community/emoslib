       PROGRAM Msort
!
!
!**** *Msort*
!
!
!     Purpose.
!     --------
!
!           Sort input list in ascending order. Search the list
!           for requested value.
!
!**   Interface.
!     ----------
!
!
!     Method.
!     -------
!
!           A quick_sort method is used for sorting, and
!           Binary search is used for matching.
!
!
!     Externals.
!     ----------
!
!          NONE.
!
!     Reference.
!     ----------
!
!          Fortran 90 for Scientist & Engineers : Brian D. Hahn 1994
!
!     Author.
!     -------
!
!          M. Dragosavac    *ECMWF*       15/01/95.
!
!
!     Modifications.
!     --------------
!
!          NONE.
!------------------------------------------------------------------------------------------

       implicit none


       integer, parameter :: jsup =   9
       integer, parameter :: jsec0=   3
       integer, parameter :: jsec1=  40
       integer, parameter :: jsec2=  4096
       integer, parameter :: jsec3=   4
       integer, parameter :: jsec4=   2 
       integer, parameter :: jelem=160000
       integer, parameter :: jbufl=512000
#ifdef JBPW_64
       integer, parameter :: jbpw =  64
#else
       integer, parameter :: jbpw =  32
#endif
       integer, parameter :: jwork=4096000
       integer, parameter :: jkey=46
       integer, parameter :: jbyte=512000
       integer, parameter :: kelem=400
       integer, parameter :: kvals=400
       integer, parameter :: ill=48/(jbpw/8)+1
       
       integer, dimension(jbufl) :: kbuff, kbufr
       integer, dimension(jsup)  :: ksup
       integer, dimension(jsec0) :: ksec0
       integer, dimension(jsec1) :: ksec1
       integer, dimension(jsec2) :: ksec2
       integer, dimension(jsec3) :: ksec3
       integer, dimension(jsec4) :: ksec4
       integer, dimension(jkey)  :: key
       real*8,    dimension(kvals) :: values, value
       integer, dimension(kelem) :: ktdlst,ktdlst1
       integer, dimension(kelem) :: kdata


       integer, parameter :: nnn=100000         ! size of list
       integer, dimension(nnn,ill) :: list    ! list to be sorted
       integer i,j,n,is,iln,iz                 ! counter
       integer ipos, kval,ij,i1,id,ic

       integer narg,nvind,iy,ilen,kel,jx 
       integer ix,ilength , ktdlen, kdlen
       real    r                                           ! random number
       real*8     eps, rvind                             
       integer iunit,iunit1,iret,ierr,iword
       integer nbytpw,kbufl,ioff,istart,iargc

       character(256) cfin, cfout
       character(256), dimension(10)   :: carg
       character(64), dimension(kelem) :: cnames
       character(24), dimension(kelem) :: cunits
       character(80), dimension(kvals) :: cvals

       external bus012,pbbufr,pbwrite
!       external iargc,getarg



!     ----------------------------------------------------------
!     Get input and output file name.

      cfin=' '
      cfout=' '
      narg=iargc()

      if(narg < 4) then
         print*,'Usage -- bufr_merge -i inputfile -o outputfile'
         stop
      end if

      do  j=1,narg
      call getarg(j,carg(j))
      end do

      do j=1,narg,2
         if(carg(j) == '-i') then
            cfin=carg(j+1)
         elseif(carg(j) == '-o') then
            cfout=carg(j+1)
         else
             print*,'Usage -- bufr_merge -i inputfile -o outputfile'
             stop
         end if
      end do

!     Initialize variables and constants
!     ----------------------------------

      nbytpw=jbpw/8           ! number of bytes per word
      rvind=1.7D38            ! missing value indicator
      nvind=2147483647        ! missing value indicator
      n=0
      eps=10.E-10
      ktdlst1( 1)=301042      ! list of descriptors for merged satellite template
      ktdlst1( 2)=303031
      ktdlst1( 3)=303032
      ktdlst1( 4)=101015
      ktdlst1( 5)=303023
      ktdlst1( 6)=101003
      ktdlst1( 7)=303024
      ktdlst1( 8)=222000
      ktdlst1( 9)=101108
      ktdlst1(10)=031031
      ktdlst1(11)=001031
      ktdlst1(12)=001201
      ktdlst1(13)=101108
      ktdlst1(14)=033007
      ktdlen=14

      do i=1,400
         value(i)=rvind
      end do



!     Open input and output files
!     ---------------------------

      ic=index(cfin,' ')
      id=index(cfout,' ')
      iret=0
      call pbopen(iunit,cfin(1:ic),'r',iret)

      if(iret == -1) stop 'Open failed'
      if(iret == -2) stop 'Invalid input file name'
      if(iret == -3) stop 'Invalid open mode specified'

      call pbopen(iunit1,cfout(1:id),'w',iret)

      if(iret == -1) stop 'Open failed'
      if(iret == -2) stop 'Invalid output file name'
      if(iret == -3) stop 'Invalid open mode specified'

      iret=0




!     Read in bufr file and create list of RDB keys
!     ---------------------------------------------



      do while(iret >= 0)

         call pbbufr(iunit,kbuff,jbyte,kbufl,iret)
         if(iret == -1) exit  

         ierr=0
         ksec1(7)=0
         iword=kbufl/nbytpw+1
         call bus012(iword,kbuff,ksup,ksec0,ksec1,ksec2,ierr)
         if(ierr /= 0) then
             print*,'bus012 : ierr=',ierr
             call my_exit(2)
         end if
 
         is=ksec1(7)

         if(is >= 61 .and. is <= 63 .or. is >= 71 .and. is <= 73) then
 
            n=n+1
            if( n > nnn ) then
               print*,'Too many records to be merged.'
               print*,'Maximum is ',nnn
               call my_exit(2)
            end if

!           Get absolute record offset

            call pbseek(iunit,0,1,iln)
            if(iln < 0) then
                print*,'pbseek error :',ierr
                call my_exit(2)
            end if

            ksec2(1)=iln-kbufl      ! offset for record

            do iz=1,ill
            list(n,iz)=ksec2(iz)
            end do

         else

!           Write bufr message in output file
!           ---------------------------------

            iret=0
            call pbwrite(iunit1,kbuff,kbufl,iret)
            if(iret < 0) then
               print*,'Error writing into ',cfout
               call my_exit(2)
            end if


         end if

      end do


!     Make a quick sort of list array containing RDB keys
!     ---------------------------------------------------

      print*,n,' records to be sorted'


      call quick_sort(list,ill,1,n)

      print*,n,' records sorted'




!      Merge satellite data
!      --------------------



      i1=0
      i=1

      do while( i < n)
!
!        set data present indicator to present for all elements.
!
         i1=0

         do iy=1,327
           value(iy)=rvind
           if(iy >= 110 .and. iy <= 217) value(iy)=0.0
         end do
!

         do while (i1 == 0 )

!
           ioff=list(i,1)
!
           istart=0
           call pbseek(iunit,ioff,istart,iret)
           if(iret < 0) then
              print*,'pbseek: error ',iret
           end if
!
           iret=0
           call pbbufr(iunit,kbuff,jbyte,kbufl,iret)
           if(iret == -1) then
             print*,'pbbufr : error ',iret
             stop 'EOF'
           end if
           if(iret == -2) stop 'File handling problem'
           if(iret == -3) stop 'Array too small for product'

           iword=kbufl/nbytpw+1

           kel=kelem
           if(ksup(6) > 1)  kel=jwork/ksup(6)

           ierr=0
           call bufrex(iword,kbuff,ksup,ksec0 ,ksec1,ksec2 ,ksec3 , &
                       ksec4,kel,cnames,cunits,kvals,values,cvals,ierr)

           if(ierr /= 0)  then
              print*,'bufrex : error=',ierr
              call my_exit(2)
           end if

           if(ksec1(7) < 61 .or. ksec1(7) > 73) then
              print*,'subtype ',ksec1(7),' found for record ',n
              call my_exit(2)
           end if

!            5. Merge Satem or 250 km tovs.
!               ---------------------------

!          copy low level data

           if(ksec1(7) == 61 .or. ksec1(7) == 71) then
              jx=0
              do ix=1,63
                jx=jx+1
                value(jx)=values(ix)
              end do

!             copy data present indicator

              jx=109
              do ix=65,127
                jx=jx+1
                value(jx)=values(ix)
              end do

!             % confidence

              jx=219
              do ix=130,192
                jx=jx+1
                value(jx)=values(ix)
              end do

              value(109)=values(64)
              value(218)=values(128)
              value(219)=values(129)

           elseif(ksec1(7) == 62 .or. ksec1(7) == 72) then

!              copy precipitable water

              jx=0
              do ix=1,18
                jx=jx+1
                if(values(ix) /= rvind) value(jx)=values(ix)
              end do 

              jx=93
              do ix=19,33
                jx=jx+1
                value(jx)=values(ix)
              end do

!             copy data present indicator

              jx=109
              do ix=35,52
                jx=jx+1
                value(jx)=values(ix)
              end do

              jx=202
              do ix=53,67
                jx=jx+1
                value(jx)=values(ix)
              end do

!             copy % confidence

              jx=219
              do ix=70,87
                jx=jx+1
                value(jx)=values(ix)
              end do

              jx=312
              do ix=88,102
                jx=jx+1
                value(jx)=values(ix)
              end do

              value(109)=values(34)
              value(218)=values(68)
              value(219)=values(69)

           elseif(ksec1(7) == 63 .or. ksec1(7) == 73) then

!             copy high level data

              jx=0
              do ix=1,18
                jx=jx+1
                if(values(ix) /= rvind) value(jx)=values(ix)
              end do

              jx=63
              do ix=19,48
                jx=jx+1
                value(jx)=values(ix)
              end do

!             copy data present indicator

              jx=109
              do ix=50,67
                jx=jx+1
                value(jx)=values(ix)
              end do

              jx=172
              do ix=68,97
                jx=jx+1
                value(jx)=values(ix)
              end do

!             copy % confidence

              jx=219
              do ix=100,117
                jx=jx+1
                value(jx)=values(ix)
              end do

              jx=282
              do ix=118,147
                jx=jx+1
                value(jx)=values(ix)
              end do

              value(109)=values(49)
              value(218)=values(98)
              value(219)=values(99)
           else
              print*,'File contain other than satem/250 tovs data.'
              print*,'Subtype ',ksec1(7),' found.'
           end if


           i1=keycomp(list(i,1:ill),list(i+1,1:ill))
           
           if( i1 == 0) i=i+1

         end do

!            6. Pack merged bufr message
!               ------------------------

         ierr=0
         call buukey(ksec1,ksec2,key,ksup,ierr)
         if(ierr /= 0) then
            print*,'buukey : error ',ierr
            call my_exit(2)
         end if

         if(ksec1(7) >= 61 .and. ksec1(7) <= 63) key(3)=65
         if(ksec1(7) >= 71 .and. ksec1(7) <= 73) key(3)=75

         call bupkey(key,ksec1,ksec2,ierr)
         if(ierr /= 0) then
            print*,'bupkey : error ',ierr
            call my_exit(2)
         end if

         ksec1(7)=key(3)
         call bufren( ksec0,ksec1,ksec2,ksec3,ksec4,  &  
                      ktdlen,ktdlst1,kdlen,kdata,kel, & 
                      kvals,value,cvals,kbufl,kbufr,ierr)

         if(ierr > 0) then
            print*,'bufren : error ',ierr
            call my_exit(2)
         end if

!            7. Write merged message into target file
!               -------------------------------------

         ilength=kbufl*nbytpw

         ierr=0
         call pbwrite(iunit1,kbufr,ilength,ierr)
         if(ierr < 0) then
            print*,'Error writing into output file.'
            call my_exit(2)
         END IF

         i=i+1
         i1=0

      end do


       contains

       RECURSIVE SUBROUTINE QUICK_SORT(X,Y,L,R)
!
!
!**** *QUICK_SORT*
!
!
!     Purpose.
!     --------
!
!           Sort input list in ascending order.
!
!**   Interface.
!     ----------
!
!
!     Method.
!     -------
!
!           A quick_sort method is used for sorting.
!
!
!     Externals.
!     ----------
!
!          NONE.
!
!     Reference.
!     ----------
!
!          Fortran 90 for Scientist & Engineers : Brian D. Hahn 1994
!
!     Author.
!     -------
!
!          M. Dragosavac    *ECMWF*       15/01/95.
!
!
!     Modifications.
!     --------------
!
!          NONE.
!

       implicit none



      integer, dimension(:,:), intent(inout) :: x    ! list
      integer, intent(in)               :: l,r  ! left,right bounds
      integer, intent(in)               :: Y
      integer l1, r1
      integer icomp

      if( l<r) then
        l1=l
        r1=r

        icomp=keycomp(x(l1,1:Y),x(l,1:Y))
        do
          do while( l1 < r .and. (icomp ==2 .or. icomp == 0))  ! shift l1 right
             l1=l1+1
             icomp=keycomp(x(l1,1:Y),x(l,1:Y))
          end do

          icomp=keycomp(x(r1,1:Y),x(l,1:Y))
          do while ( l< r1 .and. (icomp ==1 .or. icomp == 0))  !  shift r1 left
             r1=r1-1
             icomp= keycomp(x(r1,1:Y),x(l,1:Y))
          end do

          if( l1<r1 ) call swop( x(l1,1:Y),x(r1,1:Y),Y)     ! swop
          if( l1>=r1) exit                        ! crossower_partition
          
        end do

        call swop( x(l,1:Y),x(r1,1:Y),Y)     ! partition with x(l) at r1
        call quick_sort( x,Y,l,r1-1)         ! now attack left subproblem
        call quick_sort( x,Y,r1+1,r)         ! don't forget right subproblem

      end if

      END SUBROUTINE QUICK_SORT

      SUBROUTINE SWOP(A,B,C)
!
!
!**** *SWOP*
!
!
!     Purpose.
!     --------
!
!           Swops content of two variables.
!        
!
!**   Interface.
!     ----------
!
!
!     Method.
!     -------
!
!           None.
!
!     Externals.
!     ----------
!
!          NONE.
!
!     Reference.
!     ----------
!
!          Fortran 90 for Scientist & Engineers : Brian D. Hahn 1994
!
!     Author.
!     -------
!
!          M. Dragosavac    *ECMWF*       15/01/95.
!
!
!     Modifications.
!     --------------
!
!          NONE.
!

      implicit none


#ifdef JBPW_64
      integer, parameter :: jbpw=64
#else
      integer, parameter :: jbpw=32
#endif
      integer, parameter :: ill=48/(jbpw/8)+1

      integer, dimension(:), intent(inout)   :: a,b
      integer, dimension(ill)    :: temp
      integer C

      do i=1,C


      temp(i) = a(i)
      a(i)=b(i)
      b(i)=temp(i)

      end do

      END SUBROUTINE SWOP

      SUBROUTINE SEARCH(KARRAY,K,KVAL,KINDEX)
!
!
!**** *SEARCH*
!
!
!     Purpose.
!     --------
!
!           Search an input  list for the requested value.
!
!**   Interface.
!     ----------
!
!
!     Method.
!     -------
!
!           Binary search is used for matching.
!
!
!     Externals.
!     ----------
!
!          NONE.
!
!     Reference.
!     ----------
!
!          Fortran 90 for Scientist & Engineers : Brian D. Hahn 1994
!
!     Author.
!     -------
!
!          M. Dragosavac    *ECMWF*       15/01/95.
!
!
!     Modifications.
!     --------------
!
!          NONE.
!

       implicit none



      integer, dimension (*)    :: karray
      integer k
      integer kval 
      integer kindex
      integer ihigh
      integer ilow
      integer i
      integer numbis
      integer count
      logical found
      integer mid

!         1.   Binary search. 

      count=0
      found=.false.
      kindex=0
      numbis= int(log(real(k))/log(2.0)) +1    

      ihigh = K+1
      ilow  = 1

      do while(.not.found .and.count /= numbis) 

            mid = (ihigh + ilow)/2
            if(karray(mid).eq.kval) then
               found=.true.
            elseif(kval .lt.karray(mid)) then
               ihigh= mid
            elseif(kval.gt.karray(mid)) then
               ilow= mid
            end if
            count=count+1
      end do

      if(found) kindex=mid

      END SUBROUTINE SEARCH


      INTEGER FUNCTION KEYCOMP(X,Y)

#ifdef JBPW_64
       integer, parameter    :: jbpw=64
#else
       integer, parameter    :: jbpw=32
#endif
       integer, parameter    :: ilast=32/(jbpw/8)
       integer, parameter    :: ifirst=(jbpw/8-2)*8-1
       integer, dimension(:) :: x 
       integer, dimension(:) :: y 
       integer  j,jj

       logical o1,o2


!      keycomp=0  two arrays are equal
!      keycomp=1  array X > array Y
!      keycomp=2  array Y > array X

       keycomp=0

       do j= ifirst,0,-1

       o1 = btest(x(2),j)
       o2 = btest(y(2),j) 

       if(o1.and..not.o2) then
          keycomp=1
          return
       elseif(.not. o1 .and. o2) then
          keycomp=2
          return
       end if
       end do

       if(keycomp.eq.0) then

          do jj=3,ilast

             do j=jbpw-1,0,-1  

             o1 = btest(x(jj),j)
             o2 = btest(y(jj),j)
   
             if(o1 .and. .not. o2) then

                keycomp=1
                return
             elseif(.not. o1 .and. o2) then
                keycomp=2
                return
             end if

             end do
          end do

       end if

       return
       end function
      END PROGRAM Msort
