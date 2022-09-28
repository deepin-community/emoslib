C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HLL2LLW(L12PNT,OLDFLDU,OLDFLDV,
     X                         OLDGRID,AREA,POLE,GRID,
     X                         NEWFLDU,NEWFLDV,KSIZE,NLON,NLAT)
C
C---->
C**** HLL2LLW
C
C     Purpose
C     -------
C
C     This routine creates rotated U and V regular lat/long fields from
C     U and V global regular lat/long field using 12-point horizontal
C     interpolation.
C
C
C     Interface
C     ---------
C
C     IRET = HLL2LLW(L12PNT,OLDFLDU,OLDFLDV,
C    X               OLDGRID,AREA,POLE,GRID,
C    X               NEWFLDU,NEWFLDV,KSIZE,
C    X               NLON,NLAT)
C
C
C     Input parameters
C     ----------------
C
C     L12PNT  - Chooses between 12-point and 4-point interpolation
C     OLDFLDU - Array of values from the old global lat/long U field
C     OLDFLDV - Array of values from the old global lat/long V field
C     OLDGRID - Grid increments (i/j) for the old global lat/long field
C     AREA    - Limits of area (N/W/S/E) for the new lat/long fields
C     POLE    - Pole of rotation (lat/long) for the new lat/long fields
C     GRID    - Grid increments (i/j) for the new lat/long fields
C     KSIZE   - The size of the array to fill with the new
C               lat/long fields
C
C
C     Output parameters
C     -----------------
C
C     NEWFLDU - The array of values for the new U lat/long field
C     NEWFLDV - The array of values for the new V lat/long field
C     NLON    - Number of longitudes in the regular lat/long fields
C     NLAT    - Number of latitudes in the regular lat/long fields
C
C     Returns 0 if function successful, non-zero otherwise.
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
C     INTLOG  - Log error message.
C     JMALLOC - Dynamically allocate memory
C     JFREE   - Free dynamically allocated memory
C     HGENLL  - Calculates original lat/long (before rotation) for
C               a rotated grid
C     HNEILL  - Finds neighbours for points for interpolation
C     HWTSLL  - Calculates weightings for points for interpolation
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
C     J.D.Chambers      ECMWF      December 2001
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
      INTEGER KSIZE, NLON, NLAT
      REAL OLDGRID(2),AREA(4),POLE(2),GRID(2)
      REAL OLDFLDU(*),OLDFLDV(*),NEWFLDU(KSIZE),NEWFLDV(KSIZE)
C
C     Local variables
C
      INTEGER NEXT, LOOP, IRET, NLEN, NPREV, NBYTES, NUMBER
      INTEGER NOLDLAT, NOLDLON
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
      REAL PREGRID(2)
      INTEGER KPTS(1)
      REAL GLATS(1)
      INTEGER IOFFS(1)
      POINTER (IPKPTS,  KPTS)
      POINTER (IPIOFFS, IOFFS)
      POINTER (IPGLATS, GLATS)
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
      DATA NPREV/-1/
      DATA LNEW/.FALSE./, LFIRST/.TRUE./
      DATA ILLOLD/-1/, IOLDU/-1/, IOLDV/-1/
      DATA PREGRID/2*0.0/
C
      SAVE LNEW, LFIRST
      SAVE IPKSCHE, IPNEIGH, IPKLA, IPPWTS
      SAVE IPPDLO0, IPPDLO1, IPPDLO2, IPPDLO3, IPPDLAT
      SAVE NPREV, IPKPTS, IPIOFFS, IPGLATS
      SAVE ILLOLD, IPRLAT, IPRLON, IOLDU, IOLDV
      SAVE IPC, IPS, IPU, IPV
      SAVE PREGRID
C
C     Externals
C
      INTEGER HNEILL, HGENLL
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
      IPRDIR  = 0
      IPRCLON = 0
      IPRCLAT = 0
      HLL2LLW = 0
C
      CALL JDEBUG()
      NOLDLAT = 1 + NINT(180.0/OLDGRID(1))
      NOLDLON = NINT(360.0/OLDGRID(2))
      NUMBER  = NOLDLAT * NOLDLON
C
      IF( (OLDGRID(1).NE.PREGRID(1)).OR.
     X    (OLDGRID(2).NE.PREGRID(2)) ) THEN
C
C       Allocate memory to hold the input fields
C       (in case OLDFLDU/OLDFLDV and NEWFLDU/NEWFLDV are the same arrays)
C
        IF( IOLDU.GT.0 ) THEN
          CALL JFREE(IOLDU)
          CALL JFREE(IOLDV)
        ENDIF
C
        NBYTES = NUMBER * JPRLEN
C
        IOLDU = JMALLOC(NBYTES)
        IOLDV = JMALLOC(NBYTES)
#ifdef hpR64
        IOLDU = IOLDU/(1024*1024*1024*4)
        IOLDV = IOLDV/(1024*1024*1024*4)
#endif
        IF( (IOLDU.EQ.0).OR.(IOLDV.EQ.0) ) THEN
          CALL INTLOG(JP_ERROR,'HLL2LLW: Memory allocate fail',JPQUIET)
          HLL2LLW = 3
          GOTO 900
        ENDIF
C
        PREGRID(1) = OLDGRID(1)
        PREGRID(2) = OLDGRID(2)
C
      ENDIF
C
C     Preserve the input fields
C
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
      NLON = 1 + INT((AREA(JEAST) - AREA(JWEST)) / GRID(JW_E))
      NLAT = 1 + INT((AREA(JNORTH) - AREA(JSOUTH)) / GRID(JN_S))
C
      NLEN = NLON * NLAT

      NOWE = NLON
      NONS = NLAT
C
C     Check that given array is big enough for the new field.
C
      IF( NLEN.GT.KSIZE ) THEN
        CALL INTLOG(JP_ERROR,'HLL2LLW: Given array size = ',KSIZE)
        CALL INTLOG(JP_ERROR,'HLL2LLW: Required size = ',NLEN)
        HLL2LLW = 4
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
          CALL INTLOG(JP_ERROR,'HLL2LLW: Memory allocate fail',JPQUIET)
          HLL2LLW = 5
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
      IRET = HGENLL(AREA,POLE,GRID,NLON,NLAT,RLAT,RLON)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'HLL2LLW: HGENLL failed to get lat/lon grid data',JPQUIET)
        HLL2LLW = 6
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
          CALL INTLOG(JP_ERROR,'HLL2LLW: Memory allocate fail',JPQUIET)
          HLL2LLW = 7
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
      IRET = HNEILL(L12PNT,NLEN,RLAT,RLON,OLDGRID,
     X              KSCHEME,PDLAT,PDLO0,PDLO1,PDLO2,PDLO3,KLA,NEIGH)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,
     X    'HLL2LLW: HNEILL failed to find neighbours',JPQUIET)
        HLL2LLW = 8
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
      CALL HWTSLL
     X  (NLEN,KSCHEME,KLA,PDLAT,OLDGRID(2),PDLO0,PDLO1,PDLO2,PDLO3,
     X   NEIGH,PWTS)
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
