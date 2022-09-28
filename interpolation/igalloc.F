C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGALLOC (KLEVEL, KREQUEST, KADDR, KPR, KERR)
C
C---->
C**** *IGALLOC*
C
C     PURPOSE
C     _______
C
C     This routine allocates heap space.
C
C     INTERFACE
C     _________
C
C     IERR = IGALLOC (KLEVEL, KREQUEST, KADDR, KPR, KERR)
C
C     Input parameters
C     ________________
C
C     KLEVEL     - The heap block number.
C
C     KREQUEST   - The amount of heap space required.
C
C     KPR        - The debug print switch.
C                  0  , No debugging output.
C                  1  , Produce debugging output.
C
C     KERR       - The error control flag.
C                  -ve, No error message. Return error code.
C                  0  , Hard failure with error message.
C                  +ve, Print error message. Return error code.
C
C     Output parameters
C     ________________
C
C     KADDR      - The base address of the heap space allocated.
C
C     Return value
C     ____________
C
C     The error indicator (INTEGER).
C
C     Error and Warning Return Values
C     _______________________________
C
C     Cray error codes
C
Cray  -1    HPDEALLC block is already free (Warning only).
Cray
Cray  24301 HPDEALLC attempt to free block at address outside the
Cray        bounds of the heap.
Cray  24302 HPDEALLC attempt to free block at address which was not
Cray        the beginning of a block.
Cray  24303 HPDEALLC returning any other non zero code.
Cray  24304 HPALLOC request was not greater than 0 words.
Cray  24305 HPALLOC called and not enough memory was available.
Cray  24306 HPALLOC The memory arena has been truncated by a user
Cray        ssbreak(2) call.
Cray  24307 HPALLOC returning any other non zero code.
Cray
Cray  Errors 24301, 24302 and 24304 should not occur and any occurrence
Cray  of one of these errors should be reported to Meteorological
Cray  Applications.
C
C     Sun and SGI error codes
C
C     24301 MALLOC memory allocation failed.
C
C     Common block usage
C     __________________
C
C     memreq.h  - This file contains the memory request definition
C                 variables.
C
C     MADDR     - The base addresses of the currently allocated
C                 memory segments are modified.
C     MREQUEST  - The sizes of the current memory requests are modified.
C
C     EXTERNALS
C     _________
C
C     INTLOG(R)    - Logs messages.
C
C     Cray externals
C
Cray  HPALLOC   - Cray library routine to allocate heap space.
Cray  HPDEALLC  - Cray library routine to de-allocate heap space.
C
C     Sun and SGI externals
C
C     JFREE     - Unix routine to free heap space.
C     JMALLOC   - Unix routine to acquire heap space.
C
C     METHOD
C     ______
C
C     The heap block number is used to indicate which heap is being
C     modified. Currently heap 1 is used to acquire space for
C     expanding GRIB arrays into real arrays and heap 2 is used for
C     internal space during the interpolation process.
C
C     REFERENCE
C     _________
C
C     None
C
C     COMMENTS
C     ________
C
C     Program contains sections 0 to 2 and 9
C
C     AUTHOR
C     ______
C
C     K. Fielding      *ECMWF*      Jan 1994
C
C     MODIFICATIONS
C     _____________
C
C     J.D.Chambers     ECMWF        Feb 1997
C     Allow for 64-bit pointers
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "parim.h"
#include "memreq.h"
C
C     Function arguments
C
      INTEGER KLEVEL, KREQUEST, KPR, KERR
#if (defined POINTER_64)
      INTEGER*8 KADDR
#else
      INTEGER KADDR
#endif
C
C     Local variables
C
      INTEGER IABORT, IERROR
#ifdef POINTER_64
      INTEGER*8 IDUMMY
#else
      INTEGER IDUMMY
#endif
      DATA IDUMMY/0/
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 24300)
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
C     -----------------------------------------------------------------|
C*    Section 1. Initialisation
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      IGALLOC = 0
C
      IF( KPR.GE.1 ) THEN
        CALL INTLOG(JP_DEBUG,'IGALLOC: Input parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IGALLOC: Heap number = ',KLEVEL)
        CALL INTLOG(JP_DEBUG,'IGALLOC: Heap request = ',KREQUEST)
        CALL INTLOG(JP_DEBUG,'IGALLOC: Current heap =',MREQUEST(KLEVEL))
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2. Calculate arrays of weights
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IABORT = 0
      IERROR = 0
C
      IF( MREQUEST(KLEVEL).LT.KREQUEST.AND.MREQUEST(KLEVEL).GT.0) THEN
C
C       Memory required is greater than that currently allocated
C
C       Return any heap in use for spectral -> grid point operations
C
        CALL JMEMHAN(1,IDUMMY,IDUMMY,0,IERROR)
        CALL JMEMHAN(2,IDUMMY,IDUMMY,0,IERROR)
C
        CALL JFREE (MADDR (KLEVEL) )
      ENDIF
C
C     First request for memory.
C
      IF( MREQUEST(KLEVEL).LT.KREQUEST ) THEN
C
C       Return any heap in use for spectral -> grid point operations
C
        CALL JMEMHAN(1,IDUMMY,IDUMMY,0,IERROR)
        CALL JMEMHAN(2,IDUMMY,IDUMMY,0,IERROR)
C
        MADDR(KLEVEL) = JMALLOC(KREQUEST)
#ifdef hpR64
        MADDR(KLEVEL) = MADDR(KLEVEL)/(1024*1024*1024*4)
#endif
        IF( MADDR(KLEVEL).EQ.0 ) THEN
C
          IGALLOC = JPROUTINE + 8
          CALL INTLOG(JP_ERROR,'IGALLOC: Memory requested = ',KREQUEST)
          CALL INTLOG(JP_ERROR,
     X      'IGALLOC: No more memory available from system.',JPQUIET)
          IF( KERR.EQ.0 ) CALL INTLOG(JP_FATAL,
     X      'IGALLOC: Interpolation failed.',IGALLOC)
          GOTO 900
        ENDIF
C
        KADDR = MADDR(KLEVEL)
        MREQUEST(KLEVEL) = KREQUEST
C
      ELSE
C
C       Memory required is <= to that already allocated
C
        KADDR = MADDR(KLEVEL)
C
      ENDIF
C
      IF( KPR.GE.1 ) THEN
        CALL INTLOG(JP_DEBUG,'IGALLOC: Return heap address = ',KADDR)
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9. Return to calling routine. Format statements
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGALLOC: Section 9.',JPQUIET)

      RETURN
      END
