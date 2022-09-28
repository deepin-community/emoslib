      PROGRAM BUFRCHECK
C
C**** *BUFR*
C
C
C     PURPOSE.
C     --------
C         CHECKS UP BUFR MESSAGE. THE SIZES OF SECTION AND POSITION OF 
C         7777 IS CHECKED
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
C         CALL BUSEL
C         CALL BUFREX
C         CALL BUFREN
C         CALL BUPRS0
C         CALL BUPRS1
C         CALL BUPRS2
C         CALL BUPRS3
C         CALL BUPRT
C         CALL BUUKEY
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/09/87.
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
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2=4096 ,JSEC3= 4,
     1       JSEC4=   2,JELEM=160000,JSUBS=400,JCVAL=150 ,JBUFL=512000,
#ifdef JBPW_64
     2       JBPW =  64,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200,
#else
     2       JBPW =  32,JTAB =3000,JCTAB=120,JCTST=1800,JCTEXT=1200,
#endif
     3       JWORK=4096000,JKEY=46,JBYTE=512000)
C
C 
      DIMENSION KBUFF(JBUFL)
      REAL*8 RVIND,EPS
C
      CHARACTER*256 CF,COUT,CARG(4)
C
cs      EXTERNAL GETARG
C                                                                       
C     ------------------------------------------------------------------
C*          1. INITIALIZE CONSTANTS AND VARIABLES.
C              -----------------------------------
 100  CONTINUE
C
C     MISSING VALUE INDICATOR
C 
      NBYTPW=JBPW/8
      RVIND=1.7D38
      NVIND=2147483647
      N=0
      CF=' '
C
C     INPUT FILE NAME
C
C     GET INPUT AND OUTPUT FILE NAME.
C
      NARG=IARGC()
C
      IF(NARG.NE.2) THEN
         PRINT*,'USAGE -- bufr_check -i infile ' 
         STOP
      END IF
C
      DO 101 J=1,NARG
      CALL GETARG(J,CARG(J))
 101  CONTINUE
C
      IF(CARG(1).NE.'-i'.AND.CARG(1).NE.'-I'.OR.
     1   CARG(2).EQ.' ') THEN
         PRINT*,'USAGE -- bufr_check -i infile '
         STOP
      END IF
C
      CF=CARG(2)
      II=INDEX(CF,' ')
C
C*          1.2 OPEN FILE CONTAINING BUFR DATA.
C               -------------------------------
 120  CONTINUE
C
      IRET=0 
      CALL PBOPEN(IUNIT,CF(1:II),'R',IRET)
      IF(IRET.EQ.-1) STOP 'OPEN FAILED'
      IF(IRET.EQ.-2) STOP 'INVALID FILE NAME'
      IF(IRET.EQ.-3) STOP 'INVALID OPEN MODE SPECIFIED'
C
C
C     ----------------------------------------------------------------- 
C*          3.  READ BUFR MESSAGE.
C               ------------------
 300  CONTINUE
C
      IERR=0
      KBUFL=0
      IRET=0
C
      CALL PBBUFR(IUNIT,KBUFF,JBYTE,KBUFL,IRET) 
      IF(IRET.EQ.-1) THEN
         PRINT*,'NUMBER OF BUFR MESSAGES PROCCESSED-',N
         STOP 'EOF'
      END IF
      IF(IRET.EQ.-2) STOP 'FILE HANDLING PROBLEM' 
      IF(IRET.EQ.-3) STOP 'ARRAY TOO SMALL FOR PRODUCT'
C
      N=N+1
C
C     -----------------------------------------------------------------
C*          4. EXPAND BUFR MESSAGE.
C              --------------------
 400  CONTINUE
C
      CALL RECL(N,KBUFL,KBUFF,KLEN,IERR)
      GO TO 300
C
      END
      SUBROUTINE RECL(K,KBUFL,KBUFF,KLEN,KERR)
C                                                                       
C**** *RECL*                                                         
C                                                                       
C                                                                       
C     PURPOSE.                                                          
C     --------                                                          
C          GET TOTAL BUFR MESSAGE LENGTH.
C                                                                       
C**   INTERFACE.                                                        
C     ----------                                                        
C                                                                       
C          *CALL* *RECL(KBUFL,KBUFF,KLEN,KERR)*
C
C        INPUT :
C               *KBUFL*   -  LENGTH OF BUFR MESSAGE (WORDS)
C               *KBUFF*   -  ARRAY CONTAINING BUFR MESSAGE
C        OUTPUT:
C               *KLEN*    -  TOTAL BUFR LENGTH
C               *KERR*    -  RETURNED ERROR CODE
C                            0 - OK.
C                            
C     METHOD.                                                           
C     --------                                                          
C                                                                       
C          NONE.                                                       
C                                                                       
C     EXTERNALS.                                                        
C     ----------                                                        
C                                                                       
C          BUNPKS          - UNPACK BIT PATTERN IN REPEATED WAY
C          BUNPCK          - UNPACK BIT PATTERN         
C                                                                       
C     REFERENCE.                                                        
C     ----------                                                        
C                                                                       
C          NONE.                                                        
C                                                                       
C     AUTHOR.                                                           
C     -------                                                           
C                                                                       
C          M. DRAGOSAVAC    *ECMWF*       15/09/91.                  
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
      PARAMETER(JSUP =   9,JSEC0=   3,JSEC1= 40,JSEC2= 4096 ,JSEC3= 4,
     1          JSEC4=   2,JELEM=20000,JSUBS=400,JCVAL=150 ,JBUFL= 8192,
     2          JBPW =  32,JTAB =1000,JCTAB=120,JCTST=1800,JCTEXT= 200,
     3          JWORK=80000,JKEY=46)
C
      COMMON /BCOMWORK/ NBPW,NWPT,NBPT,NWPTB,NBPTB,NMASK(JBPW)
     1,                 NVIND,RVIND,NBENP,NLTVNP,NWWP,NXXP,NYYP,NZZP,EPS
C
      DIMENSION KBUFF(*)
      DIMENSION IBUFR(4)                                                
C
      CHARACTER*4 YBUFR
C
C     ------------------------------------------------------------------
C*          1.   EXPAND SECTION 0.                                      
C                -----------------                                      
 100  CONTINUE                                                          
C                                                                       
      KERR=0
      NBPW=JBPW
      IBPW=JBPW/8
      NWPT = 1                                                          
      NBPT = 0                                                          
      KLEN=0
      ILEN0=0
      ILEN1=0
      ILEN2=0
      ILEN3=0
      ILEN4=0
      ILEN5=0
C                                                                       
C*          1.2  UNPACK FIRST FOUR OCTETS CONTAINING *BUFR*.            
C                -------------------------------------------            
C                                                                       
      CALL BUNPKS(NBPW,KBUFF,IBUFR,NWPT,NBPT,8,0,4,KERR)
      IF(KERR.NE.0) RETURN
C                                                                       
C*          1.3  CHECK IF FIRST FOUR OCTETS ARE 'BUFR'.
C                --------------------------------------
      YBUFR=CHAR(IBUFR(1))//CHAR(IBUFR(2))//
     1      CHAR(IBUFR(3))//CHAR(IBUFR(4))
      IF(YBUFR.NE.'BUFR') THEN
         PRINT*,'FIRST FOUR CHARACTERS ARE NOT BUFR.'
         KERR=1
         RETURN                                 
      END IF                                    
C
C*          1.4 UNPACK BUFR EDITION NUMBER (IT IS 8TH BYTE ).
C               ---------------------------------------------
C
      IWPT=56/NBPW+1
      IBPT=56-(IWPT-1)*NBPW
C
      CALL BUNPCK(NBPW,KBUFF,IED,IWPT,IBPT,8,KERR)
C
      IF(IED.GE.2) THEN
C                                                                       
C*          1.5 UNPACK TOTAL LENGTH OF BUFR MESSAGE.
C               ------------------------------------
C
         CALL BUNPCK(NBPW,KBUFF,KLEN,NWPT,NBPT,24,KERR)
C
C
C*          2.9  CHECK IF LAST FOUR OCTETS ARE '7777'.
C                --------------------------------------
         NWPT = KLEN/IBPW
         NBPT = (KLEN-NWPT*IBPW)*8
         CALL BUNPKS(NBPW,KBUFF,IBUFR,NWPT,NBPT,8,0,4,KERR)
         YBUFR=CHAR(IBUFR(1))//CHAR(IBUFR(2))//
     1         CHAR(IBUFR(3))//CHAR(IBUFR(4))
         IF(YBUFR(1:4).NE.'7777') THEN
            PRINT*,'WRONG BUFR MESSAGE---------------------------',K
            PRINT*,'7777 NOT AT WRITE PLACE.',YBUFR,' FOUND INSTEAD.'
            PRINT*,'TOTAL LENGTH UNPACKED  ',KLEN,' BYTES'
            IB=KBUFL
            PRINT*,'INPUT BUFR MESSAGE LENGTH ',IB,' BYTES'
            RETURN                                 
         END IF                                    
         RETURN
      END IF
C                                                                       
C*          1.7 SET LENGTH OF SECTION 0.
C               ------------------------
C
      ILEN0= 4
C
C                                                                       
C*          1.8 UNPACK LENGTH OF SECTION 1.                             
C               ----------------------------                            
C
      CALL BUNPCK(NBPW,KBUFF,ILEN1,NWPT,NBPT,24,KERR)
C                                                                       
C*          1.9 SET THE POINTERS NWPTB AND NBPTB.                     
C               ---------------------------------                     
C               TO BEGINING OF THE NEXT SECTION.                      
C               --------------------------------                      
C
      CALL BUNEXS(ILEN1)                                             
C                                                                       
C
C*          2.0  UNPACK BUFR EDITION NUMBER/MASTER TABLE USED.
C                ---------------------------------------------
C
      CALL BUNPCK(NBPW,KBUFF,IDUMMY,NWPT,NBPT, 8,KERR)
C                                                                       
C*          2.1  UNPACK ORIGINATING CENTRE.                             
C                --------------------------                             
      CALL BUNPCK(NBPW,KBUFF,IDUMMY,NWPT,NBPT,16,KERR)
C                                                                       
C*          2.2  UNPACK UPDATE SEQUENCE NUMBER.                         
C                ------------------------------                         
C
      CALL BUNPCK(NBPW,KBUFF,IDUMMY,NWPT,NBPT, 8,KERR)
C                                                                       
C*          2.3  UNPACK INTEGER VALUE OF THE OCTET CONTAINING           
C                --------------------------------------------           
C                 FLAG BITS(ZERO IF SECTION TWO IS NOT PRESENT).        
C                 ----------------------------------------------        
C
      CALL BUNPCK(NBPW,KBUFF,IFLAG,NWPT,NBPT, 8,KERR)
C                            
C*          2.4 SET THE POINTERS NWPT AND NBPT AT THE BEGINING OF SECTION.
C              -----------------------------------------------------------
C
      NWPT = NWPTB                                                      
      NBPT = NBPTB                                                      
C                                                                       
      IF(IFLAG.NE.0) THEN
C
C*          2.5  UNPACK LENGTH OF SECTION 2.                            
C                ---------------------------                            
         CALL BUNPCK(NBPW,KBUFF,ILEN2,NWPT,NBPT,24,KERR)
C                                                                       
         CALL BUNEXS(ILEN2)                                             
C
      END IF
C                                                                       
C                                                                       
C*          2.6   UNPACK LENGTH OF SECTION 3.                           
C                 ---------------------------                           
C                                                                       
      NWPT = NWPTB                                                      
      NBPT = NBPTB                                                      
      CALL BUNPCK(NBPW,KBUFF,ILEN3,NWPT,NBPT,24,KERR)
C
      CALL BUNEXS(ILEN3)                                             
C                                                                       
C                                                                       
C*          2.7  UNPACK LENGTH OF SECTION 4.                            
C                ---------------------------                            
      NWPT = NWPTB                                                      
      NBPT = NBPTB                                                      
      CALL BUNPCK(NBPW,KBUFF,ILEN4,NWPT,NBPT,24,KERR)
C
      CALL BUNEXS(ILEN4) 
C
C*          2.8  UNPACK LAST FOUR OCTETS CONTAINING '7777'.            
C                ------------------------------------------            
C                                                                       
      NWPT = NWPTB                                                      
      NBPT = NBPTB                                                      
      CALL BUNPKS(NBPW,KBUFF,IBUFR,NWPT,NBPT,8,0,4,KERR)
C                                                                       
C*          2.9  CHECK IF FIRST FOUR OCTETS ARE 'BUFR'.
C                --------------------------------------
      YBUFR=CHAR(IBUFR(1))//CHAR(IBUFR(2))//
     1      CHAR(IBUFR(3))//CHAR(IBUFR(4))
      IF(YBUFR(1:4).NE.'7777') THEN
         PRINT*,'WRONG BUFR MESSAGE---------------------------',K
         PRINT*,' LENGTH OF SECTION 0',ILEN0
         PRINT*,' LENGTH OF SECTION 1',ILEN1
         PRINT*,' LENGTH OF SECTION 2',ILEN2
         PRINT*,' LENGTH OF SECTION 3',ILEN3
         PRINT*,' LENGTH OF SECTION 4',ILEN4
         PRINT*,' LENGTH OF SECTION 5',ILEN5
         IB=KBUFL
         PRINT*,' INPUT RECORD LENGTH',IB,' BYTES'
         RETURN                                 
      END IF                                    
C
      ILEN5=4
      KLEN=ILEN0+ILEN1+ILEN2+ILEN3+ILEN4+ILEN5
C
      IBUFL=KBUFL
      IF(KLEN.GT.IBUFL) THEN
         PRINT*,'WRONG BUFR MESSAGE---------------------------',K
         PRINT*,' LENGTH OF SECTION 0',ILEN0
         PRINT*,' LENGTH OF SECTION 1',ILEN1
         PRINT*,' LENGTH OF SECTION 2',ILEN2
         PRINT*,' LENGTH OF SECTION 3',ILEN3
         PRINT*,' LENGTH OF SECTION 4',ILEN4
         PRINT*,' LENGTH OF SECTION 5',ILEN5
         IB=KBUFL
         PRINT*,' INPUT RECORD LENGTH',IB,' BYTES'
         PRINT*,' INPUT RECORD TOO SHORT.'
         RETURN
      END IF
C
C     	  -----------------------------------------------------------------
C
      RETURN                                      
      END                                                               
