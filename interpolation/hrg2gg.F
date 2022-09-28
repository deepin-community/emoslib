C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HRG2GG(L12PNT,OLDFLD,KGNOLD,AREA,POLE,
     X                        KGNNEW,HTYPE,NEWFLD,KSIZE,NUMPTS)
C
C---->
C**** HRG2GG
C
C     Purpose
C     -------
C
C     This routine creates a rotated gaussian field from a reduced
C     gaussian field using 12-point horizontal interpolation.
C
C
C     Interface
C     ---------
C
C     IRET = HRG2GG(L12PNT,OLDFLD,KGNOLD,AREA,POLE,
C    X              KGNNEW,HTYPE,NEWFLD,KSIZE,NUMPTS)
C
C
C     Input parameters
C     ----------------
C
C     L12PNT  - Chooses between 12-point and 4-point interpolation
C     OLDFLD  - Array of values from the old (reduced) gaussian field
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
C     NEWFLD  - The array of values for the gaussian field
C     NUMPTS  - Number of points in the new gaussian field.
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
C     FORCED_NEAREST_NEIGHBOUR - check forced interpolation method
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
C     J.D.Chambers      ECMWF      February 2001
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
#include "nifld.common"
#include "nofld.common"
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
      REAL AREA(4), POLE(2), OLDFLD(*), NEWFLD(KSIZE)
      CHARACTER*1 HTYPE
C
C     Local variables
C
      INTEGER NEXT, LOOP, IRET, NLEN, NOPREV, NNPREV, NBYTES, NUMBER
      INTEGER NEAREST
      CHARACTER*1 NNTYPE
C
      LOGICAL LNEW, LFIRST, LVEGGY
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
      REAL RLAT(1),RLON(1)
      POINTER (IPRLAT, RLAT)
      POINTER (IPRLON, RLON)
C
      REAL OLD(1)
      POINTER (IOLD,   OLD)
C
      DATA NOPREV/-1/, NNPREV/-1/, NNTYPE/' '/
      DATA LNEW/.FALSE./, LFIRST/.TRUE./
      DATA IGGOLD/-1/, INNOLD/-1/, ILLOLD/-1/, IOLD/-1/
C
      SAVE LNEW, LFIRST
      SAVE IPKSCHE, IPNEIGH, IPKLA, IPPWTS
      SAVE IPPDLO0, IPPDLO1, IPPDLO2, IPPDLO3, IPPDLAT
      SAVE NOPREV,NNPREV,NNTYPE
      SAVE IGGOLD,INNOLD,IPKPTS,IPKNPTS,IPIOFFS,IPGLATS,IPGNLATS
      SAVE ILLOLD, IPRLAT, IPRLON, IOLD
C
C     Externals
C
      LOGICAL FORCED_NEAREST_NEIGHBOUR
      INTEGER HNEI12, HGENGG
#ifdef POINTER_64
      INTEGER*8 JMALLOC
#else
      INTEGER JMALLOC
#endif
C
C     -----------------------------------------------------------------|
C     Section 1.  Initialise.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      HRG2GG = 0
C
      CALL JDEBUG()

      IF( L12PNT ) THEN
        CALL INTLOG(JP_DEBUG,'HRG2GG: 12-pt interpolation',JPQUIET)
      ELSE
        CALL INTLOG(JP_DEBUG,'HRG2GG:  4-pt interpolation',JPQUIET)
      ENDIF
C
      CALL CHKPREC()
      IF( LPREC )THEN
        CALL INTLOG(JP_DEBUG,
     X   'HRG2GG: precipitation threshold applied',JPQUIET)
      ELSE
        CALL INTLOG(JP_DEBUG,
     X   'HRG2GG: precipitation threshold not applied',JPQUIET)
      ENDIF

C     Use nearest neighbour if required
      LVEGGY = FORCED_NEAREST_NEIGHBOUR(LMETHOD,NITABLE,NIPARAM)
      IF( LVEGGY ) CALL INTLOG(JP_DEBUG,
     X  'HRG2GG: nearest neighbour processing',JPQUIET)


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
          CALL INTLOG(JP_ERROR,'HRG2GG: Memory allocation fail',JPQUIET)
          HRG2GG = 1
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
     X      'HRG2GG: JGETGG failed to get gaussian data',JPQUIET)
          HRG2GG = 1
          GOTO 900
        ENDIF
C
        IOFFS(1) = 1
        DO LOOP = 2, (KGNOLD*2+1)
          IOFFS(LOOP) = IOFFS(LOOP-1) + KPTS(LOOP-1)
        ENDDO
C
C       Allocate memory to hold the input field
C       (in case OLDFLD and NEWFLD are the same arrays)
C
        IF( IOLD.GT.0 ) CALL JFREE(IOLD)
C
        NUMBER = (IOFFS(KGNOLD*2+1) - 1)
        NBYTES = NUMBER * JPRLEN
C
        IOLD = JMALLOC(NBYTES)
#ifdef hpR64
        IOLD = IOLD/(1024*1024*1024*4)
#endif
        IF( IOLD.EQ.0 ) THEN
          CALL INTLOG(JP_ERROR,'HRG2GG: Memory allocation fail',JPQUIET)
          HRG2GG = 1
          GOTO 900
        ENDIF
C
        NOPREV = KGNOLD
      ENDIF
C
C     Preserve the input field
C
      NUMBER = (IOFFS(KGNOLD*2+1) - 1)
      DO LOOP = 1, NUMBER
        OLD(LOOP) = OLDFLD(LOOP)
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
          CALL INTLOG(JP_ERROR,'HRG2GG: Memory allocation fail',JPQUIET)
          HRG2GG = 2
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
     X      'HRG2GG: JGETGG failed to get gaussian data',JPQUIET)
          HRG2GG = 2
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
        CALL INTLOG(JP_ERROR,'HRG2GG: Given array size = ',KSIZE)
        CALL INTLOG(JP_ERROR,'HRG2GG: Required size = ',NLEN)
        HRG2GG = 2
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
        NBYTES = 2*ILL*JPRLEN
C
        IPRLON = JMALLOC(NBYTES)
#ifdef hpR64
        IPRLON = IPRLON/(1024*1024*1024*4)
#endif
        IF( IPRLON.EQ.0 ) THEN
          CALL INTLOG(JP_ERROR,'HRG2GG: Memory allocation fail',JPQUIET)
          HRG2GG = 2
          GOTO 900
        ENDIF
C
        IPRLAT = IPRLON + (ILL*JPRLEN)
C
        ILLOLD = ILL
C
      ENDIF
C
      IRET = HGENGG(AREA,POLE,KGNNEW,HTYPE,KNPTS,GNLATS,ILL,
     X              RLAT,RLON,NUMPTS)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'HRG2GG: HGENGG failed to get lat/lon grid data',JPQUIET)
        HRG2GG = 2
        GOTO 900
      ENDIF
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
          CALL INTLOG(JP_ERROR,'HRG2GG: Memory allocation fail',JPQUIET)
          HRG2GG = 3
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
     X    'HRG2GG: HNEI12 failed to find neighbours',JPQUIET)
        HRG2GG = 3
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
       IF( LVEGGY) THEN
            NEAREST = 1
            IF( PWTS( 2,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 2
            IF( PWTS( 3,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 3
            IF( PWTS( 4,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 4
            IF( PWTS( 5,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 5
            IF( PWTS( 6,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 6
            IF( PWTS( 7,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 7
            IF( PWTS( 8,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 8
            IF( PWTS( 9,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST = 9
            IF( PWTS(10,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST =10
            IF( PWTS(11,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST =11
            IF( PWTS(12,LOOP).GT.PWTS(NEAREST,LOOP) ) NEAREST =12
            NEWFLD(LOOP) = OLD(NEIGH( NEAREST,LOOP))
        ELSE
          IF( KSCHEME(LOOP).EQ.JP12PT ) THEN
            NEWFLD(LOOP) =
     X        OLD(NEIGH( 1,LOOP)) * PWTS( 1,LOOP) +
     X        OLD(NEIGH( 2,LOOP)) * PWTS( 2,LOOP) +
     X        OLD(NEIGH( 3,LOOP)) * PWTS( 3,LOOP) +
     X        OLD(NEIGH( 4,LOOP)) * PWTS( 4,LOOP) +
     X        OLD(NEIGH( 5,LOOP)) * PWTS( 5,LOOP) +
     X        OLD(NEIGH( 6,LOOP)) * PWTS( 6,LOOP) +
     X        OLD(NEIGH( 7,LOOP)) * PWTS( 7,LOOP) +
     X        OLD(NEIGH( 8,LOOP)) * PWTS( 8,LOOP) +
     X        OLD(NEIGH( 9,LOOP)) * PWTS( 9,LOOP) +
     X        OLD(NEIGH(10,LOOP)) * PWTS(10,LOOP) +
     X        OLD(NEIGH(11,LOOP)) * PWTS(11,LOOP) +
     X        OLD(NEIGH(12,LOOP)) * PWTS(12,LOOP)
C
          ELSE IF( KSCHEME(LOOP).EQ.JP4PT ) THEN
            NEWFLD(LOOP) =
     X        OLD(NEIGH( 1,LOOP)) * PWTS( 1,LOOP) +
     X        OLD(NEIGH( 2,LOOP)) * PWTS( 2,LOOP) +
     X        OLD(NEIGH( 3,LOOP)) * PWTS( 3,LOOP) +
     X        OLD(NEIGH( 4,LOOP)) * PWTS( 4,LOOP)
C
          ELSE
            DO NEXT = 1, 4
              IF( NEIGH(NEXT,LOOP).NE.0 )
     X          NEWFLD(LOOP) = OLD(NEIGH(NEXT,LOOP))
            ENDDO
C
         ENDIF
        ENDIF
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

