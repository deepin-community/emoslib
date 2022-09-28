C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

#ifndef USE_NO_POINTERS
      INTEGER FUNCTION GBITMAP(KNUM, KVALS, KNONMIS, KBTPTR, KBYTE)
C
C---->
C**** GBITMAP - Read bitmap from a file
C
C     Purpose.
C     --------
C
C     Reads a bitmap from a file.
C
C**   Interface.
C     ----------
C
C     IRET = GBITMAP(KNUM, KVALS, KNONMIS, KBTPTR, KBYTE)
C
C
C     Input Parameters.
C     -----------------
C
C     KNUM    - The predetermined bitmap number (in range 0 to 999)
C     KBYTE   - Number of bits in an INTEGER.
C
C     Output Parameters.
C     ------------------
C
C     KVALS   - The number of bits in the bitmap
C     KNONMIS - The number of non-missing points in the bitmap
C     KBTPTR  - Pointer to the start of the bitmap
C
C     Returns zero if all OK (otherwise an error).
C
C
C     Method.
C     -------
C
C     Opens a file containing the bitmap, allocates memory for the
C     bitmap, reads the bitmap into memory, and closes the file.
C
C     If the bitmap number is the same as in the previous call,
C     the open/allocate/read sequence is skipped.
C
C     The file pathname may be given by the environment variable
C     LOCAL_BITMAP_PATH, otherwise a hardcoded default pathname is used.
C
C
C     Externals.
C     ----------
C
C     PBOPEN   - Opens the binary bitmap file
C     PBREAD   - Reads the binary bitmap file
C     PBCLOSE  - Closes the binary bitmap file
C     INXBIT   - Extract values from bit-stream
C     JMALLOC  - Routine to acquire memory
C
C
C     Reference.
C     ----------
C
C     None.
C
C
C     Comments.
C     ---------
C
C     None.
C
C
C     Author.
C     -------
C
C     J.D.Chambers       ECMWF      Oct 1996
C
C
C     Modifications.
C     --------------
C
C     J.D.Chambers     ECMWF        Feb 1997
C     Allow for 64-bit pointers
C
C     J.Clochard, Meteo France, for ECMWF - January 1998.
C     Use JMALLOC for all systems.
C
C
C----<
C     ----------------------------------------------------------------
C*    Section 0 . Definition of variables. Data statements.
C     ----------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "common/grprs.h"
#include "grbcom.h"
C
C     Function arguments
C
      INTEGER KNUM, KVALS, KNONMIS, KBYTE
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 KBTPTR
#else
      INTEGER*4 KBTPTR
#endif
#else
      INTEGER*8 KBTPTR
#endif
C
C     Local variables
C
      INTEGER IRET
C
C     Predetermined bitmask variables
C
      CHARACTER*256 HBTMASK
      INTEGER IBTMAP,IBTMASK,IBTSIZE,IBTSIZP,IBTOPEN,IBTVALS,NONMISS
      INTEGER IOFFSET
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IBTPTR
#endif
#endif
      POINTER (IBTPTR,IBTMAP)
      DIMENSION IBTMAP(1)
      SAVE IBTPTR, IBTSIZP, IBTOPEN, IBTVALS, NONMISS
C
C     External functions
C
#ifdef POINTER_64
      INTEGER*8 JMALLOC
#else
      INTEGER JMALLOC
#endif
      EXTERNAL JMALLOC
C
      DATA IBTSIZP/0/
      DATA IBTOPEN/0/
C
C     ----------------------------------------------------------------
C     Section 1 . Initialise.
C     ----------------------------------------------------------------
C
  100 CONTINUE
C
C     Clear return values
      KVALS   = 0
      KNONMIS = 0
      KBTPTR  = 0
C
C     Check the bitmap number is valid
      IF( (KNUM.LT.0) .OR. (KNUM.GT.999) ) THEN
        WRITE(GRPRSM,*) 'GBITMAP: Invalid bitmap number given = ', KNUM
        GBITMAP = 727
        GOTO 900
      ENDIF
C
C     ----------------------------------------------------------------
C     Section 2 . Open the bitmask file if first time.
C     ----------------------------------------------------------------
C
  200 CONTINUE
C
      IF( IBTOPEN.NE.KNUM ) THEN
C 
C       Build filename
        HBTMASK = ELBPATH
        IOFFSET = INDEX( HBTMASK, ' ')
        WRITE(HBTMASK(IOFFSET:IOFFSET+10),'(A8,I3.3)')
     X    'bitmask_', KNUM
        IOFFSET = INDEX( HBTMASK, ' ')
C 
C       Open the file
        CALL PBOPEN(IBTMASK,HBTMASK(1:IOFFSET-1),'r',IRET)
        IF( IRET.NE.0 ) THEN
          WRITE(GRPRSM,*) 'GBITMAP: Open predetermined bitmap failed'
          GBITMAP = 728
          GOTO 900
        ENDIF
C 
C       Read file size (number of bits)
        CALL PBREAD(IBTMASK,IBTSIZE,KBYTE,IRET)
        IF( IRET.NE.KBYTE ) THEN
          WRITE(GRPRSM,*) 'GBITMAP: Problem getting bitmap size'
          GBITMAP = 729
          GOTO 900
        ENDIF
C 
C       Get memory to read bitmask unless sufficient previously
C       allocated
        IBTVALS = IBTSIZE
        IBTSIZE = (IBTSIZE+8-1)/8
C 
C       Check whether this bitmap is bigger than the previous
C       bitmap (if any)
        IF( IBTSIZE.GT.IBTSIZP ) THEN
C 
C         Return previously allocated memory (if any)
          IF( IBTSIZP.NE.0 ) CALL JFREE(IBTPTR)
C 
C         Get memory
          IBTPTR  = JMALLOC(IBTSIZE)
#ifdef hpR64
          IBTPTR = IBTPTR/(1024*1024*1024*4)
#endif
          IF( IBTPTR.EQ.0 ) THEN
            WRITE(GRPRSM,*) 'GBITMAP: Problem getting memory for bitmap'
            GBITMAP = 730
            GOTO 900
          ENDIF
        ENDIF
C 
C       Read number of non-missing points
        CALL PBREAD(IBTMASK,NONMISS,KBYTE,IRET)
        IF( IRET.NE.KBYTE ) THEN
          WRITE(GRPRSM,*)
     X      'GBITMAP: Problem getting no. non-missing points'
          GBITMAP = 731
          GOTO 900
        ENDIF
C 
C       Read the bitmap
        CALL PBREAD(IBTMASK,IBTMAP,IBTSIZE,IRET)
        IF( IRET.NE.IBTSIZE ) THEN
          WRITE(GRPRSM,*) 'GBITMAP: Problem reading bitmap'
          GBITMAP = 732
          GOTO 900
        ENDIF
C 
C       Close the bitmap file
        CALL PBCLOSE(IBTMASK,IRET)
        IF( IRET.NE.0 ) THEN
          WRITE(GRPRSM,*) 'GBITMAP: Problem closing bitmap file'
          GBITMAP = 733
          GOTO 900
        ENDIF
C
        IBTOPEN = KNUM
C
      ENDIF
C
C     Setup return values
      KVALS   = IBTVALS
      KNONMIS = NONMISS
      KBTPTR  = IBTPTR
      GBITMAP = 0
C 
C     ----------------------------------------------------------------
C     Section 9 . Closedown.
C     ----------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END
#endif
