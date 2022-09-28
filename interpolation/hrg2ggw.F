C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HRG2GGW(L12PNT,OLDFLDU,OLDFLDV,KSIZEUV,
     X                         KGNOLD,AREA,POLE,KGNNEW,HTYPE,
     X                         NEWFLDU,NEWFLDV,KSIZE,NUMPTS)
C
C---->
C**** HRG2GGW
C
C     Purpose
C     -------
C
C     This routine creates rotated gaussian U and V fields from reduced U and V
C     gaussian fields using 12-point horizontal interpolation.
C
C
C     Interface
C     ---------
C
C     INTEGER FUNCTION HRG2GGW(L12PNT,OLDFLDU,OLDFLDV,KSIZEUV,
C    X                         KGNOLD,AREA,POLE,KGNNEW,HTYPE,
C    X                         NEWFLDU,NEWFLDV,KSIZE,NUMPTS)
C
C
C     Input parameters
C     ----------------
C
C     L12PNT  - Chooses between 12-point and 4-point interpolation
C     OLDFLDU - Array of values from the old (reduced) gaussian U field
C     OLDFLDV - Array of values from the old (reduced) gaussian V field
C     KSIZEUV - Number of values in each old (reduced) gaussian field
C     KGNOLD  - Gaussian number for the old gaussian field
C     AREA    - Limits of area (N/W/S/E)
C     POLE    - Pole of rotation (lat/long)
C     KGNNEW  - Gaussian grid number (for the new field)
C     HTYPE   - Gaussian grid type (for the new field)
C               = 'R' for reduced ("quasi-regular"),
C               = 'O' for reduced/octahedral,
C               = 'F' for full
C     KSIZE   - The size of the array to fill with the regular
C               lat/long field
C
C
C     Output parameters
C     -----------------
C
C     NEWFLDU - The array of values for the new U gaussian field
C     NEWFLDV - The array of values for the new V gaussian field
C     NUMPTS  - Number of points in the new gaussian fields.
C
C     Returns 0 if function successful, non-zero otherwise.
C
C
C     Common block usage
C     ------------------
C
C     None
C
C
C     Method
C     ------
C
C     Numbering of the points (I is the interpolation point):
C
C                   13       5       6      14
C
C                    7       1       2       8
C                               (I)
C                    9       3       4      10
C
C                   15      11      12      16
C
C     The 12-point interpolation is not possible if either of the top
C     two rows is above the original field northern latitude. The
C     nearest neighbour is used if both rows are above, and a 4-pt
C     bilinear interpolation is used if the top row is above.
C     Similarily, if either of the bottom two rows is below the original
C     field southern latitude.
C
C
C     Externals
C     ---------
C
C     INTLOG  - Logs messages
C     JMALLOC - Dynamically allocate memory
C     JFREE   - Free dynamically allocated memory
C     JGETGG  - Reads the definition of a gaussian grid
C     HGENGG  - Calculates original lat/long (before rotation) for
C               a rotated gaussian grid
C     HNEI12  - Finds neighbours for points for interpolation
C     HWTS12  - Calculates weightings for points for interpolation
C
C
C     Reference
C     ---------
C
C     None.
C
C
C     Comments
C     --------
C
C     None.
C
C
C     Author
C     ------
C
C     J.D.Chambers      ECMWF      November 2001
C
C
C     Modifications
C     -------------
C
C     None.
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "parim.h"
C
C     Parameters
C
      INTEGER JNORTH, JSOUTH, JWEST, JEAST, JW_E, JN_S, JLAT, JLON
      INTEGER JP12PT, JP4PT, JPNEARN
      PARAMETER (JP12PT  = 0)
      PARAMETER (JP4PT   = 1)
      PARAMETER (JPNEARN = 2)
      PARAMETER (JNORTH = 1 )
      PARAMETER (JWEST  = 2 )
      PARAMETER (JSOUTH = 3 )
      PARAMETER (JEAST  = 4 )
      PARAMETER (JW_E  = 1 )
      PARAMETER (JN_S  = 2 )
      PARAMETER (JLAT  = 1 )
      PARAMETER (JLON  = 2 )
C
C     Function arguments
C
      LOGICAL L12PNT
      INTEGER KGNOLD, KGNNEW, KSIZE, NUMPTS
      REAL AREA(4), POLE(2), OLDFLDU(*), OLDFLDV(*)
      REAL NEWFLDU(KSIZE), NEWFLDV(KSIZE)
      INTEGER KSIZEUV
      CHARACTER*1 HTYPE
C
C     Local variables
C
      INTEGER NEXT, LOOP, IRET, NLEN, NOPREV, NNPREV, NBYTES, NUMBER
      CHARACTER*1 NNTYPE
C
      LOGICAL LNEW, LFIRST
      INTEGER KSCHEME(1),NEIGH(12,1), KLA(1)
      REAL PWTS(12,1)
      POINTER (IPKSCHE, KSCHEME)
      POINTER (IPNEIGH, NEIGH)
      POINTER (IPKLA,   KLA)
      POINTER (IPPWTS,  PWTS)
C
      REAL PDLO0(1),PDLO1(1),PDLO2(1),PDLO3(1),PDLAT(1)
      POINTER (IPPDLO0, PDLO0)
      POINTER (IPPDLO1, PDLO1)
      POINTER (IPPDLO2, PDLO2)
      POINTER (IPPDLO3, PDLO3)
      POINTER (IPPDLAT, PDLAT)
C
      INTEGER IGG, IGGOLD, INN, INNOLD
      INTEGER KPTS(1), KNPTS(1)
      REAL GLATS(1), GNLATS(1)
      INTEGER IOFFS(1)
      POINTER (IPKPTS,  KPTS)
      POINTER (IPKNPTS,  KNPTS)
      POINTER (IPIOFFS, IOFFS)
      POINTER (IPGLATS, GLATS)
      POINTER (IPGNLATS, GNLATS)
C
      INTEGER ILL, ILLOLD
      REAL RCLAT(1),RCLON(1),RLAT(1),RLON(1),RDIR(1),C(1),S(1),U(1),V(1)
      POINTER (IPRCLAT, RCLAT)
      POINTER (IPRCLON, RCLON)
      POINTER (IPRLAT, RLAT)
      POINTER (IPRLON, RLON)
      POINTER (IPRDIR, RDIR)
      POINTER (IPC, C)
      POINTER (IPS, S)
      POINTER (IPU, U)
      POINTER (IPV, V)
C
      REAL OLDU(1)
      POINTER (IOLDU, OLDU)
      REAL OLDV(1)
      POINTER (IOLDV, OLDV)
C
      DATA NOPREV/-1/, NNPREV/-1/, NNTYPE/' '/
      DATA LNEW/.FALSE./, LFIRST/.TRUE./
      DATA IGGOLD/-1/, INNOLD/-1/, ILLOLD/-1/, IOLDU/0/, IOLDV/0/
C
      SAVE LNEW, LFIRST
      SAVE IPKSCHE, IPNEIGH, IPKLA, IPPWTS
      SAVE IPPDLO0, IPPDLO1, IPPDLO2, IPPDLO3, IPPDLAT
      SAVE NOPREV,NNPREV,NNTYPE
      SAVE IGGOLD,INNOLD,IPKPTS,IPKNPTS,IPIOFFS,IPGLATS,IPGNLATS
      SAVE ILLOLD, IPRLAT, IPRLON, IOLDU, IOLDV
      SAVE IPC, IPS, IPU, IPV
C
C     Externals
C
      INTEGER HNEI12, HGENGG
#ifdef POINTER_64
      INTEGER*8 JMALLOC
#else
      INTEGER JMALLOC
#endif
C
C     Statement function
C
      REAL A, RADIAN
C
C     Convert degrees to radians
C
      RADIAN(A) = (A) * 0.0174532925
C
C     -----------------------------------------------------------------|
C     Section 1.  Initialise.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      IPRCLAT = 0
      IPRCLON = 0
      IPRDIR  = 0
      HRG2GGW = 0
C
      CALL JDEBUG()
C
C     Dynamically allocate memory for old gaussian grid information.
C
      IGG = KGNOLD*2
C
      IF( IGG.GT.IGGOLD ) THEN
C
        IF( IGGOLD.GT.0 ) CALL JFREE(IPKPTS)
C
        NBYTES = (IGG*JPRLEN) + (2*IGG+1)*JPBYTES
C
        IPKPTS = JMALLOC(NBYTES)
#ifdef hpR64
        IPKPTS = IPKPTS/(1024*1024*1024*4)
#endif
        IF( IPKPTS.EQ.0 ) THEN
      CALL INTLOG(JP_ERROR,'HRG2GGW: Memory allocation fail',JPQUIET)
          HRG2GGW = 1
          GOTO 900
        ENDIF
C
        IPGLATS = IPKPTS  + (IGG*JPBYTES)
        IPIOFFS = IPGLATS + (IGG*JPRLEN)
C
        IGGOLD = IGG
        NOPREV = -1
C
      ENDIF
C
C     Build up offsets to start of each latitude in the original field.
C
      IF( KGNOLD.NE.NOPREV ) THEN
        CALL JGETGG(KGNOLD,HTYPE,GLATS,KPTS,IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'HRG2GGW: JGETGG failed to get gaussian data',JPQUIET)
          HRG2GGW = 2
          GOTO 900
        ENDIF
C
        IOFFS(1) = 1
        DO LOOP = 2, (KGNOLD*2+1)
          IOFFS(LOOP) = IOFFS(LOOP-1) + KPTS(LOOP-1)
        ENDDO
C
C       Allocate memory to hold the input fields
C       (in case OLDFLDU/OLDFLDV and NEWFLD are the same arrays)
C
        IF( IOLDU.GT.0 ) CALL JFREE(IOLDU)
        IF( IOLDV.GT.0 ) CALL JFREE(IOLDV)
C
        NUMBER = (IOFFS(KGNOLD*2+1) - 1)
        NBYTES = NUMBER * JPRLEN
C
        IOLDU = JMALLOC(NBYTES)
        IOLDV = JMALLOC(NBYTES)
#ifdef hpR64
        IOLDU = IOLDU/(1024*1024*1024*4)
        IOLDV = IOLDV/(1024*1024*1024*4)
#endif
        IF( (IOLDU.EQ.0).OR.(IOLDV.EQ.0) ) THEN
      CALL INTLOG(JP_ERROR,'HRG2GGW: Memory allocation fail',JPQUIET)
          HRG2GGW = 3
          GOTO 900
        ENDIF
C
        NOPREV = KGNOLD
      ENDIF
C
C     Preserve the input fields
C
      NUMBER = (IOFFS(KGNOLD*2+1) - 1)
      DO LOOP = 1, NUMBER
        OLDU(LOOP) = OLDFLDU(LOOP)
        OLDV(LOOP) = OLDFLDV(LOOP)
      ENDDO
C
C     -----------------------------------------------------------------|
C     Section 2.  Generate the lat/long points for the output grid
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
C
C     Dynamically allocate memory for gaussian grid information.
C
      INN = KGNNEW*2
C
      IF( INN.GT.INNOLD ) THEN
C
        IF( INNOLD.GT.0 ) CALL JFREE(IPKNPTS)
C
        NBYTES = (INN*JPRLEN) + (INN*JPBYTES)
C
        IPKNPTS = JMALLOC(NBYTES)
#ifdef hpR64
        IPKNPTS = IPKNPTS/(1024*1024*1024*4)
#endif
        IF( IPKNPTS.EQ.0 ) THEN
      CALL INTLOG(JP_ERROR,'HRG2GGW: Memory allocation fail',JPQUIET)
          HRG2GGW = 1
          GOTO 900
        ENDIF
C
        IPGNLATS = IPKNPTS  + (INN*JPBYTES)
C
        INNOLD = INN
        NNPREV = -1
      ENDIF
C
      IF( (KGNNEW.NE.NNPREV).OR.(HTYPE.NE.NNTYPE) ) THEN
        IF( HTYPE.NE.'F' .AND. HTYPE.NE.'f' .AND.
     X      HTYPE.NE.'O' .AND. HTYPE.NE.'o' ) THEN
          HTYPE = 'R'
        ENDIF
        CALL JGETGG(KGNNEW,HTYPE,GNLATS,KNPTS,IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,
     X      'HRG2GGW: JGETGG failed to get gaussian data',JPQUIET)
          HRG2GGW = 2
          GOTO 900
        ENDIF
C
        IF( HTYPE.EQ.'F' .OR. HTYPE.EQ.'f' ) THEN
          NLEN = KGNNEW * KGNNEW * 8
        ELSE
          NLEN = 0
          DO LOOP = 1, KGNNEW*2
            NLEN = NLEN + KNPTS(LOOP)
          ENDDO
        ENDIF
        NNPREV = KGNNEW
        NNTYPE = HTYPE
      ENDIF
C
C     Check that given array is big enough for the new field.
C
      IF( NLEN.GT.KSIZE ) THEN
        CALL INTLOG(JP_ERROR,'HRG2GGW: Given array size = ',KSIZE)
        CALL INTLOG(JP_ERROR,'HRG2GGW: Required size = ',NLEN)
        HRG2GGW = 4
        GOTO 900
      ENDIF
C
C     Dynamically allocate memory for lat/long arrays.
C
      ILL = NLEN
      IF( ILL.GT.ILLOLD ) THEN
C
        LNEW = .TRUE.
C
        IF( ILLOLD.GT.0 ) CALL JFREE(IPRLON)
C
        NBYTES = 9*ILL*JPRLEN
C
        IPRLON = JMALLOC(NBYTES)
#ifdef hpR64
        IPRLON = IPRLON/(1024*1024*1024*4)
#endif
        IF( IPRLON.EQ.0 ) THEN
      CALL INTLOG(JP_ERROR,'HRG2GGW: Memory allocation fail',JPQUIET)
          HRG2GGW = 5
          GOTO 900
        ENDIF
C
        IPRLAT  = IPRLON  + (ILL*JPRLEN)
        IPRCLON = IPRLAT  + (ILL*JPRLEN)
        IPRCLAT = IPRCLON + (ILL*JPRLEN)
        IPRDIR  = IPRCLAT + (ILL*JPRLEN)
        IPC     = IPRDIR  + (ILL*JPRLEN)
        IPS     = IPC     + (ILL*JPRLEN)
        IPU     = IPS     + (ILL*JPRLEN)
        IPV     = IPU     + (ILL*JPRLEN)
C
        ILLOLD = ILL
C
      ENDIF
C
      IRET = HGENGG(AREA,POLE,KGNNEW,HTYPE,KNPTS,GNLATS,ILL,
     X              RLAT,RLON,NUMPTS)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'HRG2GGW: HGENGG failed to get lat/lon grid data',JPQUIET)
        HRG2GGW = 6
        GOTO 900
      ENDIF
C
      NLEN = NUMPTS
C
C     -----------------------------------------------------------------|
C     Section 3.  Find neighbours for each point for interpolation.
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
C     Dynamically allocate memory for interpolation arrays.
C
      IF( LNEW ) THEN
C
        IF( .NOT.LFIRST ) CALL JFREE(IPPDLO0)
C
        NBYTES = (17*JPRLEN + 14*JPBYTES) * ILL
C
        IPPDLO0 = JMALLOC(NBYTES)
#ifdef hpR64
        IPPDLO0 = IPPDLO0/(1024*1024*1024*4)
#endif
        IF( IPPDLO0.EQ.0 ) THEN
      CALL INTLOG(JP_ERROR,'HRG2GGW: Memory allocation fail',JPQUIET)
          HRG2GGW = 7
          GOTO 900
        ENDIF
C
        IPPDLO1 = IPPDLO0 + (ILL*JPRLEN)
        IPPDLO2 = IPPDLO1 + (ILL*JPRLEN)
        IPPDLO3 = IPPDLO2 + (ILL*JPRLEN)
        IPPDLAT = IPPDLO3 + (ILL*JPRLEN)
        IPPWTS  = IPPDLAT + (ILL*JPRLEN)
        IPKSCHE = IPPWTS  + (12*ILL*JPRLEN)
        IPKLA   = IPKSCHE + (ILL*JPBYTES)
        IPNEIGH = IPKLA   + (ILL*JPBYTES)
C
        LFIRST = .FALSE.
        LNEW   = .FALSE.
C
      ENDIF
C
C     Find neighbours.
C
      IRET = HNEI12(L12PNT,NLEN,RLAT,RLON,KGNOLD,KPTS,GLATS,
     X              KSCHEME,PDLAT,PDLO0,PDLO1,PDLO2,PDLO3,KLA,NEIGH)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'HRG2GGW: HNEI12 failed to find neighbours',JPQUIET)
        HRG2GGW = 8
        GOTO 900
      ENDIF
C
C     -----------------------------------------------------------------|
C     Section 4.  Perform the 12-point horizontal interpolation.
C     -----------------------------------------------------------------|
C
  400 CONTINUE
C
C     Setup the 12-point horizontal interpolation weights
C
      CALL HWTS12
     X  (NLEN,KSCHEME,KLA,PDLAT,GLATS,PDLO0,PDLO1,PDLO2,PDLO3,NEIGH,
     X   PWTS)
C
C     Calculate the interpolated grid point values
C
      DO LOOP = 1, NLEN
        IF( KSCHEME(LOOP).EQ.JP12PT ) THEN
          U(LOOP) =
     X      OLDU(NEIGH( 1,LOOP)) * PWTS( 1,LOOP) +
     X      OLDU(NEIGH( 2,LOOP)) * PWTS( 2,LOOP) +
     X      OLDU(NEIGH( 3,LOOP)) * PWTS( 3,LOOP) +
     X      OLDU(NEIGH( 4,LOOP)) * PWTS( 4,LOOP) +
     X      OLDU(NEIGH( 5,LOOP)) * PWTS( 5,LOOP) +
     X      OLDU(NEIGH( 6,LOOP)) * PWTS( 6,LOOP) +
     X      OLDU(NEIGH( 7,LOOP)) * PWTS( 7,LOOP) +
     X      OLDU(NEIGH( 8,LOOP)) * PWTS( 8,LOOP) +
     X      OLDU(NEIGH( 9,LOOP)) * PWTS( 9,LOOP) +
     X      OLDU(NEIGH(10,LOOP)) * PWTS(10,LOOP) +
     X      OLDU(NEIGH(11,LOOP)) * PWTS(11,LOOP) +
     X      OLDU(NEIGH(12,LOOP)) * PWTS(12,LOOP)
          V(LOOP) =
     X      OLDV(NEIGH( 1,LOOP)) * PWTS( 1,LOOP) +
     X      OLDV(NEIGH( 2,LOOP)) * PWTS( 2,LOOP) +
     X      OLDV(NEIGH( 3,LOOP)) * PWTS( 3,LOOP) +
     X      OLDV(NEIGH( 4,LOOP)) * PWTS( 4,LOOP) +
     X      OLDV(NEIGH( 5,LOOP)) * PWTS( 5,LOOP) +
     X      OLDV(NEIGH( 6,LOOP)) * PWTS( 6,LOOP) +
     X      OLDV(NEIGH( 7,LOOP)) * PWTS( 7,LOOP) +
     X      OLDV(NEIGH( 8,LOOP)) * PWTS( 8,LOOP) +
     X      OLDV(NEIGH( 9,LOOP)) * PWTS( 9,LOOP) +
     X      OLDV(NEIGH(10,LOOP)) * PWTS(10,LOOP) +
     X      OLDV(NEIGH(11,LOOP)) * PWTS(11,LOOP) +
     X      OLDV(NEIGH(12,LOOP)) * PWTS(12,LOOP)
C
        ELSE IF( KSCHEME(LOOP).EQ.JP4PT ) THEN
          U(LOOP) =
     X      OLDU(NEIGH( 1,LOOP)) * PWTS( 1,LOOP) +
     X      OLDU(NEIGH( 2,LOOP)) * PWTS( 2,LOOP) +
     X      OLDU(NEIGH( 3,LOOP)) * PWTS( 3,LOOP) +
     X      OLDU(NEIGH( 4,LOOP)) * PWTS( 4,LOOP)
          V(LOOP) =
     X      OLDV(NEIGH( 1,LOOP)) * PWTS( 1,LOOP) +
     X      OLDV(NEIGH( 2,LOOP)) * PWTS( 2,LOOP) +
     X      OLDV(NEIGH( 3,LOOP)) * PWTS( 3,LOOP) +
     X      OLDV(NEIGH( 4,LOOP)) * PWTS( 4,LOOP)
C
        ELSE
          DO NEXT = 1, 4
            IF( NEIGH(NEXT,LOOP).NE.0 ) THEN
              U(LOOP) = OLDU(NEIGH(NEXT,LOOP))
              V(LOOP) = OLDV(NEIGH(NEXT,LOOP))
            ENDIF
          ENDDO
C
        ENDIF
      ENDDO
C
C     -----------------------------------------------------------------|
C     Section 5.  Adjust the U and V components to new (rotated) axes
C     -----------------------------------------------------------------|
C
  500 CONTINUE
C
      DO LOOP = 1, NLEN
        RDIR(LOOP) = 0.0
      ENDDO
C
      CALL HPSHGPW(RLON,RLAT,RCLON,RCLAT,RDIR,NLEN,POLE)
C
      DO LOOP = 1, NLEN
        RDIR(LOOP) = -RADIAN(RDIR(LOOP))
      ENDDO
C
      DO LOOP = 1, NLEN
        C(LOOP) = COS(RDIR(LOOP))
        S(LOOP) = SIN(RDIR(LOOP))
      ENDDO
C
      DO LOOP = 1, NLEN
        NEWFLDU(LOOP) = U(LOOP)*C(LOOP) - V(LOOP)*S(LOOP)
        NEWFLDV(LOOP) = U(LOOP)*S(LOOP) + V(LOOP)*C(LOOP)
      ENDDO
C
C     -----------------------------------------------------------------|
C     Section 9.  Return.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      RETURN
      END

