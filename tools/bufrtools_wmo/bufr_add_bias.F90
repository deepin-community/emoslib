      PROGRAM BUFR_ADD_BIAS
!
!**** *BUFR_ADD_BIAS*
!
!
!     PURPOSE.
!     --------
!         Add bias information to the existing synop bufr data
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
!          M. DRAGOSAVAC    *ECMWF*       /17/03/2004.
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
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 4096 ,JSEC3= 4, &
            JSEC4=   2,JELEM=160000,JSUBS=400,JCVAL=150 ,JBUFL=512000, &
#ifdef JBPW_64
            JBPW =  64,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200, &
#else
            JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200, &
#endif
            JWORK=4096000,JKEY=46,JBYTE=512000)
!
      PARAMETER (KELEM=40000)
      PARAMETER (KVALS=360000)
      PARAMETER (NSTATIONS=15000)
! 
      DIMENSION KBUFF(JBUFL)
      DIMENSION KBUFR(JBUFL)
      DIMENSION KSUP(JSUP)  ,KSEC0(JSEC0),KSEC1(JSEC1)
      DIMENSION KSEC2(JSEC2),KSEC3(JSEC3),KSEC4(JSEC4)
      DIMENSION KEY  (JKEY),KREQ(2)
!
      REAL*8 VALUES(KVALS),VALUE(KVALS),RQV(KELEM)
      REAL*8 RVIND,EPS
      REAL*8 BIAS_VALUE0, BIAS_VALUE1
!
      DIMENSION KTDLST(KELEM),KTDEXP(KELEM),KRQ(KELEM)
      DIMENSION KDATA(200)
      DIMENSION IOUT(12800)
!
      CHARACTER*256 CF(100),COUT,CFIN,CLIST
      CHARACTER*64 CNAMES(KELEM)
      CHARACTER*24 CUNITS(KELEM)
      CHARACTER*80 CVALS(KVALS)
      CHARACTER*80 YENC
      CHARACTER*256 CARG(10)
      CHARACTER*8   CIDENT, CTEMP
      INTEGER DATE
      DIMENSION ITYPE(NSTATIONS)
      DIMENSION ISBT(NSTATIONS)
      DIMENSION IPC(NSTATIONS)
      DIMENSION BIAS(NSTATIONS)
      CHARACTER*8 CID(NSTATIONS)
!
!s      EXTERNAL GETARG
!                                                                       
!     ------------------------------------------------------------------
!*          1. INITIALIZE CONSTANTS AND VARIABLES.
!              -----------------------------------
 100  CONTINUE
!
!     MISSING VALUE INDICATOR
! 
      icount=0
      ITLEN=6400
      ITL=0
      JZ=0
      NW=0
      N=0
      NBYTES=JBPW/8
      RVIND=1.7E38
      NVIND=2147483647
      IOBS=0
      KRQL=0
      NR=0
      KREQ(1)=0
      KREQ(2)=0
      DO 102 I=1,KELEM
      RQV(I)=RVIND
      KRQ(I)=NVIND
 102  CONTINUE
!
!     INPUT FILE NAMES
!
      NARG=IARGC()
      IF(NARG.LT.4) THEN
         PRINT*,'USAGE -- bufr_add_bias -i infile -o outfile -l list'
         STOP
      END IF
      NFILE=NARG
!
      DO 104 J=1,NARG
      CALL GETARG(J,CARG(J))
 104  CONTINUE

      II=0
      IO=0
      IN=0
      IL=0
      DO 105 J=1,NARG
      IF(CARG(J).EQ.'-i') THEN
         IN=J
      ELSEIF(CARG(J).EQ.'-o') THEN
         IO=J
      ELSEIF(CARG(J).EQ.'-l') THEN
         IL=J
      END IF
 105  CONTINUE
      IF(IO.EQ.0.OR.IN.EQ.0) THEN
         PRINT*,'USAGE -- bufr_add_bias -i infile -o outfile -l list'
         STOP
      END IF
!
      COUT=CARG(IO+1)
      CFIN=CARG(IN+1)
      CLIST=CARG(IL+1)
!
      JJ=INDEX(COUT,' ')
!
      CALL PBOPEN(IUNIT1,COUT(1:JJ),'W',IRET)
      IF(IRET.EQ.-1) STOP 'OPEN FAILED ON BUFR.DAT'
      IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'
!
      ILN=INDEX(CFIN,' ')
!
!*          1.2 OPEN FILE CONTAINING BUFR DATA.
!               -------------------------------
 120  CONTINUE
!
      IRET=0 
      CALL PBOPEN(IUNIT,CFIN(1:ILN),'R',IRET)
      IF(IRET.EQ.-1) STOP 'OPEN FAILED'
      IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'
!
      IF(IL.NE.0) THEN
         ILL=INDEX(CLIST,' ')
!
         OPEN(UNIT=37,FILE=CLIST(1:ILL-1),IOSTAT=IOS, &
              STATUS='OLD',FORM='FORMATTED')
!
         READ(37,'(A)') CLINE
         READ(37,'(10x,I10.10)') DATE
         READ(37,'(16x,I12.12)') NUM_STATIONS
         READ(37,'(19x,F8.2)')   TRESHOLD
         READ(37,'(A)') CLINE
         READ(37,'(A)') CLINE
!
         if(NUM_STATIONS.GT.NSTATIONS) THEN
            print*,'Error: too many stations in the list.'
            call exit(2)
         end if
         DO I=1,NUM_STATIONS
         READ(37,'(A8,I4,I5,I3,F11.2)') CID(I), ITYPE(I),ISBT(I), &
                                        IPC(I),BIAS(I)
!        write(*,*) CID(I), ITYPE(I),ISBT(I),IPC(I),BIAS(I)
         END DO

      END IF
!
!     ----------------------------------------------------------------- 
!*          2. SET REQUEST FOR EXPANSION.
!              --------------------------
 200  CONTINUE
!
      OPRT=.FALSE.
      OENC=.TRUE.
      NCOM=1
      OCOMP=.FALSE.
      NR=0
      OSEC3=.FALSE.
!
!*          2.1 SET REQUEST FOR PARTIAL EXPANSION.
!               ----------------------------------
 210  CONTINUE
!
!     SET VARIABLE TO PACK BIG VALUES AS MISSING VALUE INDICATOR
!
      KPMISS=1
      KPRUS=0
      NOKEY=0
      CALL BUPRQ(KPMISS,KPRUS,NOKEY)
!
!     -----------------------------------------------------------------
!*          3.  READ BUFR MESSAGE.
!               ------------------
 300  CONTINUE
!
      IERR=0
      KBUFL=0
!
      IRET=0
      icount=icount+1
!     print *, '++++ processing message #',icount
      CALL PBBUFR(IUNIT,KBUFF,JBYTE,KBUFL,IRET) 
      IF(IRET.EQ.-1) THEN
          GO TO 900
      END IF
      IF(IRET.EQ.-2) STOP 'FILE HANDLING PROBLEM' 
      IF(IRET.EQ.-3) STOP 'ARRAY TOO SMALL FOR PRODUCT'
!
      N=N+1
      IKBUFL=KBUFL
      KBUFL=KBUFL/NBYTES+1
      IF(N.LT.NR) GO TO 300
!
!     -----------------------------------------------------------------
!*          4. EXPAND BUFR MESSAGE.
!              --------------------
 400  CONTINUE
!
      CALL BUS012(KBUFL,KBUFF,KSUP,KSEC0,KSEC1,KSEC2,KERR)
      IF(KERR.NE.0) THEN
         PRINT*,'ERROR IN BUS012: ',KERR
         PRINT*,' BUFR MESSAGE NUMBER ',N,' CORRUPTED.'
         KERR=0
         GO TO 300
      END IF
      KBUFFL=KSEC0(2)
!
      OSURF=.false.
      IF(ksec1(6).ne.0.and.ksec1(6).ne.1) then
         CALL PBWRITE(IUNIT1,KBUFF,IKBUFL,IERR)
         go to 300
      else
         if(ksec1(7).eq.  1) OSURF=.true.
         if(ksec1(7).eq.  3) OSURF=.true.
         if(ksec1(7).eq.  9) OSURF=.true.
         if(ksec1(7).eq. 11) OSURF=.true.
         if(ksec1(7).eq. 13) OSURF=.true.
         if(ksec1(7).eq. 19) OSURF=.true.
         if(ksec1(7).eq. 21) OSURF=.true.
         if(ksec1(7).eq.140) OSURF=.true.
         if(ksec1(7).eq.147) OSURF=.true.
         if(ksec1(7).eq.170) OSURF=.true.
         if(ksec1(7).eq.172) OSURF=.true.
         if(ksec1(7).eq.176) OSURF=.true.
         if(ksec1(7).eq.180) OSURF=.true.
         if(ksec1(7).eq.181) OSURF=.true.
         if(ksec1(7).eq.182) OSURF=.true.
      end if
!
      if(.not.OSURF) then
         CALL PBWRITE(IUNIT1,KBUFF,IKBUFL,IERR)
         go to 300
      end if
!
      IF(KSUP(6).GT.1) THEN
         KEL=JWORK/KSUP(6)
      ELSE
         KEL=KELEM
      END IF
!
      CALL BUFREX(KBUFL,KBUFF,KSUP,KSEC0 ,KSEC1,KSEC2 ,KSEC3 ,KSEC4,&
                  KEL,CNAMES,CUNITS,KVALS,VALUES,CVALS,IERR)
!
      IF(IERR.NE.0) THEN
         IF(IERR.EQ.45) GO TO 300
         CALL EXIT(2)
      END IF
      IOBS=IOBS+KSEC3(3)
!
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
      IF(KERR.NE.0) CALL EXIT(2)
!
!*          4.1 PRINT CONTENT OF EXPANDED DATA.
!               -------------------------------
 410  CONTINUE
!
      IF(.NOT.OPRT) GO TO 500
      IF(.NOT.OSEC3) GO TO 450
!
!*          4.2 PRINT SECTION ZERO OF BUFR MESSAGE.
!               -----------------------------------
 420  CONTINUE
!

      CALL BUPRS0(KSEC0)
!
!*          4.3 PRINT SECTION ONE OF BUFR MESSAGE.
!               -----------------------------------
 430  CONTINUE
!
      CALL BUPRS1(KSEC1)
!
!
!*          4.4 PRINT SECTION TWO OF BUFR MESSAGE.
!               -----------------------------------
 440  CONTINUE
!
!              AT ECMWF SECTION 2 CONTAINS RDB KEY.
!              SO UNPACK KEY
!
      CALL BUUKEY(KSEC1,KSEC2,KEY,KSUP,KERR)
!
!              PRINT KEY
!
      CALL BUPRS2(KSUP ,KEY)
!
!*          4.5 PRINT SECTION 3 OF BUFR MESSAGE.
!               -----------------------------------
 450  CONTINUE
!
!               FIRST GET DATA DESCRIPTORS
!
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
      IF(KERR.NE.0) CALL EXIT(2)
!
!               PRINT  CONTENT
!
      IF(OSEC3) THEN
         CALL BUPRS3(KSEC3,KTDLEN,KTDLST,KTDEXL,KTDEXP,KEL,CNAMES)
      END IF
!
!*         4.6 PRINT SECTION 4 (DATA).
!              -----------------------
 460  CONTINUE
!
!          IN THE CASE OF MANY SUBSETS DEFINE RANGE OF SUBSETS
!
      IF(.NOT.OO) THEN
      WRITE(*,'(A,$)') ' STARTING SUBSET TO BE PRINTED : '
      READ(*,'(BN,I4)')   IST
      WRITE(*,'(A,$)') ' ENDING SUBSET TO BE PRINTED : '
      READ(*,'(BN,I4)')   IEND
      OO=.FALSE.
      END IF
!
!              PRINT DATA
!
      ICODE=0
      CALL BUPRT(ICODE,IST,IEND,KEL,CNAMES,CUNITS,CVALS,&
                 KVALS,VALUES,KSUP,KSEC1,IERR)
!
!              RESOLVE BIT MAPS 
!
!     IF(IEND.GT.KSEC3(3)) IEND=KSEC3(3)
!
!     DO 461 IK=IST,IEND
! 
!     CALL BUBOX(IK,KSUP,KEL,KTDEXP,CNAMES,CUNITS,KVALS,VALUES,
!    1           KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU,IERR)
!
!     CALL BUPRTBOX(KBOX,KAPP,KLEN,KBOXR,VALS,CBOXN,CBOXU)
!
!461  CONTINUE
!
!     -----------------------------------------------------------------
!*          5. COLLECT DATA FOR REPACKING.
!              ---------------------------
 500  CONTINUE
!      
      IF(.NOT.OENC) GO TO 300
!
!               FIRST GET DATA DESCRIPTORS
!
      CALL BUSEL(KTDLEN,KTDLST,KTDEXL,KTDEXP,KERR)
      IF(KERR.NE.0) CALL EXIT(2)
!
!     -----------------------------------------------------------------
!*          6. PACK BUFR MESSAGE BACK INTO BUFR.
!              ---------------------------------
 600  CONTINUE
!
    
      KKK=0
      KBUFL=JBUFL
!
!     GET REPLICATION FACTORS
!
      KK=0
      DO 601 K=1,KSUP(5)
      IF(KTDEXP(K).EQ.31001.OR.KTDEXP(K).EQ.31002.OR.&
         KTDEXP(K).EQ.31000.OR.&
         KTDEXP(K).EQ.31000) THEN
         KK=KK+1
         KDATA(KK)=NINT(VALUES(K))
      END IF
 601  CONTINUE
!
      KDLEN=2
      IF(KK.NE.0) KDLEN=KK
!
!     --------------------------------
!     |Modification to sections 3
!     --------------------------------
!
      i_end=ksup(5)
      do i=1,ksup(5)
        if(ktdexp(i) .eq. 222000) then
          i_end=i-1
          exit 
        endif
        if(ktdexp(i) .eq. 225000) then
          i_end=i-1
          exit 
        endif
      end do
! skip message if bitmap cannot be built due to YYY too big in 101YYY
      if (i_end>255) then
        print*,'message #',icount,' skipped. Too many elements'
        GO TO 300
      endif
      ip=ktdlen
!
      ip=ip+1
      ktdlst(ip)=225000
      ip=ip+1
      ktdlst(ip)=236000
      ip=ip+1
      ktdlst(ip)=101000+i_end
      ip=ip+1
      ktdlst(ip)=031031
      ip=ip+1
      ktdlst(ip)=001031
      ip=ip+1
      ktdlst(ip)=001032
      ip=ip+1
      ktdlst(ip)=008024
      ip=ip+1
      if(ksec1(7).eq.140.OR.ksec1(7).eq.147) then
         ktdlst(ip)=101001
      else
         ktdlst(ip)=101002
      end if
      ip=ip+1
      ktdlst(ip)=225255
!
      ktdlen=ip
!
!     Add a new bit map and bias
!
      ip=ksup(5)

      ip=ip+1
      values(ip)=0.0
      ip=ip+1
      values(ip)=0.0
!
      i_010004=0
      i_010051=0
      i_007004=0
      nqcentries=0
      do i=1,i_end
        if(ktdexp(i) .eq. 010004.and.nqcentries.lt.2) then 
          i_010004=i
          nqcentries=nqcentries+1
        endif
        if(ktdexp(i) .eq. 010051.and.nqcentries.lt.2) then
          i_010051=i
          nqcentries=nqcentries+1
        endif
        if(ktdexp(i) .eq. 007004.and.nqcentries.lt.2) then
          i_007004=i
          nqcentries=nqcentries+1
        endif
      end do
!
      do iz=1,i_end
       ip=ip+1
       values(ip)=1.
       if(iz.eq.i_010004) values(ip)=0.
       if(iz.eq.i_010051) values(ip)=0.
       if(iz.eq.i_007004) values(ip)=0.
      end do

      ip=ip+1
      values(ip)=98.
      ip=ip+1
      values(ip)=10.
      ip=ip+1
      values(ip)=40.      ! bias
      ip=ip+1
      if(IL.eq.0) then
         values(ip)=rvind
         if(ksec1(7).ne.140.OR.ksec1(7).eq.147) then
           ip=ip+1
           values(ip)=rvind
         end if
      else
!        create identifier
         if(ksec1(7).eq.1.or.ksec1(7).eq.3.or.ksec1(7).eq.170 &
            .or.ksec1(7).eq.172.or.ksec1(7).eq.176) then
            cident=' '
            if(values(1).eq.rvind.or.values(2).eq.rvind) then
               cident=' '
               PRINT*,'Missing block and/or station number'
!              CALL PBOPEN(IUERR,'error.bufr','W',IRET)
!              CALL PBWRITE(IUERR,KBUFF,KBUFFL,IERR)
!              CALL PBCLOSE(IUERR)
            else
              write(cident,'(i2.2,i3.3)',iostat=ios) nint(values(1)),&
              nint(values(2))
              if(ios.ne.0) then
                 print*,'internal write error=',ios
                 call exit(2)
              end if
            end if
         elseif(ksec1(7).eq.9.or.ksec1(7).eq.11.or.&
             ksec1(7).eq.13.or.ksec1(7).eq.19.or.ksec1(7).eq.180) then
             cident=cvals(1)
!        elseif(ksec1(7).eq.178) then
!           if(values(1).eq.rvind.or.values(2).eq.rvind) then
!              print*,values(1),' ',values(2)
!              cident=' '
!           else
!             write(cident,'(i4.4,i10.10)',iostat=ios) nint(values(1)),
!    1        nint(values(2))
!             if(ios.ne.0) then
!                print*,'internal write error=',ios
!                call exit(2)
!             end if
!           end if
         elseif(ksec1(7).eq.9.or.ksec1(7).eq.11.or.&
            ksec1(7).eq.13.or.ksec1(7).eq.19.or.ksec1(7).eq.180) then
             cident=cvals(1)
         elseif(ksec1(7).eq.21) then
             cident=' '
             if(values(1).eq.rvind) then
                cident=' '
             else
                write(cident,'(i5.5)',iostat=ios) nint(values(1))
                if(ios.ne.0) then
                  print*,'internal write error=',ios
                  call exit(2)
               end if
            end if
         elseif((ksec1(7).eq.181).or.(ksec1(7).eq.182)) then
             cident=' '
             if(values(1).eq.rvind) then
                cident=' '
             else
                write(cident,'(i7.7)',iostat=ios) nint(values(1))
                if(ios.ne.0) then
                  print*,'internal write error=',ios
                  call exit(2)
               end if
            end if
         elseif(ksec1(7).eq.140.OR.ksec1(7).eq.147) then
             cident=' '
             cident=cvals(1)
         else
             cident=' '
         end if
         ctemp=adjustr(cident)
         call get_bias(ctemp,ksec1,NUM_STATIONS,cid,itype,&
              isbt,ipc,bias,bias_value0,bias_value1,ierr)
         values(ip)=bias_value1
         if(ksec1(7).ne.140.OR.ksec1(7).eq.147) then
            ip=ip+1
            values(ip)=bias_value0
         end if 
      end if

      kel =ip
!
!*          6.2 ENCODE DATA INTO BUFR MESSAGE.
!               ------------------------------
 620  CONTINUE
!
      KSEC3(4)=128
      IF(KSEC3(3).GT.1) KSEC3(4)=192
      CALL BUFREN( KSEC0,KSEC1,KSEC2,KSEC3,KSEC4, &
                   KTDLEN,KTDLST,KDLEN,KDATA,KEL, &       
                   KVALS,VALUES,CVALS,KBUFL,KBUFR,KERR)
!
      IF(KERR.GT.0) THEN
         PRINT*,'ERROR DURING ENCODING. Message skipped'
         CALL PBOPEN(IUERR,'error.bufr','W',IRET)
         CALL PBWRITE(IUERR,KBUFF,KBUFFL,IERR)
         CALL PBCLOSE(IUERR)
!        CALL EXIT(2)
         GO TO 300
      END IF

      NW=NW+1
!
!           6.3 WRITE PACKED BUFR MESSAGE INTO FILE.
!               ------------------------------------
 630  CONTINUE
!
      IKBUFL=KBUFL*4
      CALL PBWRITE(IUNIT1,KBUFR,IKBUFL,IERR)
      IF(IERR.LT.0) THEN
         PRINT*,'ERROR WRITING INTO TARGET FILE.'
         CALL EXIT(2)
      END IF
!
!
      GO TO 300
!     -----------------------------------------------------------------
!
 810  CONTINUE
!
      WRITE(*,'(1H ,A)') 'OPEN ERROR ON INPUT FILE'
      GO TO 900
!      
 800  CONTINUE
!
      IF(IRET.EQ.-1) THEN
         PRINT*,'NUMBER OF RECORDS PROCESSED ',N
         PRINT*,'NUMBER OF RECORDS CONVERTED ',NW
!
      ELSE
         PRINT*,' BUFR : ERROR= ',IERR
      END IF
!
 900  CONTINUE
!
      PRINT*,'NUMBER OF RECORDS PROCESSED ',N
      PRINT*,'NUMBER OF RECORDS CONVERTED ',NW
!
      CALL PBCLOSE(IUNIT,IRET)
!
      CALL PBCLOSE(IUNIT1,IRET)
!
      END
      SUBROUTINE CUTZERO(CHARPAR,KMIN)
!
!**** CUTZERO - SUBROUTINE TO REMOVE ZERO CHARACTERS.
!
!**   PURPOSE
!     -------
!
!     TO REMOVE ZERO-FILL CHARACTERS FROM THE END OF A CHARACTER
!     VARIABLE.
!
!     INTERFACE
!     ---------
!
!     CALL CUTZERO(CHARPAR,KMIN)
!
!          CHARPAR  - CHARACTER VARIABLE WHICH MAY HAVE ZEROS AT THE
!                     END OF THE VALID CHARACTERS, WHICH NEED TO BE
!                     REMOVED;
!                     RETURNED WITH THE ZERO CHARACTERS CONVERTED TO
!                     BLANK.
!
!          KMIN     - INTEGER VARIABLE INDICATING A MINIMUM NUMBER OF
!                     CHARACTERS AT THE BEGINNING OF THE STRING WHICH
!                     MUST NOT BE CHANGED
!
!     THUS:
!
!     CHARPAR='ABC0000'
!     CALL CUTZERO(CHARPAR,4)
!
!     WOULD RETURN THE VALUE 'ABC0   ' IN CHAR, AND WOULD NOT
!     ALTER THE FIRST 4 CHARACTERS.
!
!     METHOD
!     ------
!
!     THE STRIG IS TESTED FOR THE EXISTANCE OF A ZERO CHARACTER.
!     IF NONE IS FOUND, NO CHANGE TAKES PLACE.
!     IF ONE OR MORE ZERO CHARACTERS ARE PRESENT, THE END OF THE
!     STRING IS LOCATED. WORKING BACKWARDS FROM THE END TO THE
!     KMIN-1 POSITION, CHARACTERS ARE TESTED FOR ZERO. IF A ZERO
!     IS FOUND, IT IS REPLACED BY BLANK. IF A NON-ZERO IS FOUND,
!     THE REPLACEMENT LOOP TERMINATES.
!
!     MODIFICATIONS
!     -------------
!
!     ORIGINAL VERSION - 25.01.95 - REX GIBSON - ECMWF.
!
      CHARACTER*(*) CHARPAR
      CHARACTER*1   YZERO
      INTEGER LEN
!
!     -----------------------------------------------------------
!
!*     1.     FIND AND REPLACE THE ZERO CHARACTERS.
!
  100 CONTINUE
      YZERO=CHAR(0)
      I1=INDEX(CHARPAR,'0')
      IF (I1.GT.0) THEN
         I2=MAX(I1,KMIN+1)
         I3=INDEX(CHARPAR,' ')-1
         IF (I3.LE.0) THEN
            I3=LEN(CHARPAR)
         ENDIF
         DO 112 J=I3,I2,-1
         IF (CHARPAR(J:J).EQ.'0') THEN
             CHARPAR(J:J)=' '
         ELSEIF (CHARPAR(J:J).EQ.YZERO) THEN
             GO TO 112
         ELSE
             GO TO 114
         ENDIF
  112    CONTINUE
!
  114    CONTINUE
      ENDIF
!
!     -----------------------------------------------------------
!
!*     2.     RETURN.
!
  200 CONTINUE
!
      END
      SUBROUTINE  GET_BIAS(CIDENT,KSEC1,K_STATIONS,CID,KTYPE,KSBT,&
                KPC,BIAS,BIAS_VALUE0,BIAS_VALUE1,KERR)
!**** *GET_BIAS*
!
!
!     PURPOSE.
!     --------
!
!          Get bias value for particular station
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
!          NONE.
!
!     REFERENCE.
!     ----------
!
!          NONE.
!
!     AUTHOR.
!     -------
!
!
!          M. DRAGOSAVAC    *ECMWF*       06/11/2004.
!
!
!     MODIFICATIONS.
!     --------------
!
!          NONE.
!
!
!
!     -------------------------------------------------------------

      CHARACTER*8 CIDENT
      DIMENSION KSEC1(*), KTYPE(*), KSBT(*), KPC(*),BIAS(*)
      CHARACTER*(*) CID(*)
      REAL*8 BIAS_VALUE0, BIAS_VALUE1
      
      KERR=0
      BIAS_VALUE0=1.7D38
      BIAS_VALUE1=1.7D38
!
      IF(K_STATIONS.EQ.0) RETURN

      DO I=1,K_STATIONS
        if(CIDENT.eq.CID(I)) THEN
           IF(KSEC1(6).EQ.KTYPE(I).AND.KSEC1(7).EQ.KSBT(I).AND.&
              KPC(i).EQ.0) THEN
              BIAS_VALUE0=BIAS(I)
           ELSEIF(KSEC1(6).EQ.KTYPE(I).AND.KSEC1(7).EQ.KSBT(I).AND.&
              KPC(I).EQ.1) THEN
              BIAS_VALUE1=BIAS(I)
           END IF
        END IF
      END DO
      RETURN
      END
