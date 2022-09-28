C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGLSM01 (KUNIT, KNWE, KLONG, KLATV,
     1   KEXPAND, KLEN, KLSM)
C
C---->
C**** *IGLSM01*
C
C     PURPOSE
C     _______
C
C     Read and process one latitude row from the 10 minute land sea
C     mask.
C
C     INTERFACE
C     _________
C
C     IERR = IGLSM01 (KUNIT, KNWE, KLONG, KLATV, KEXPAND,
C    1   KLEN, KLSM)
C
C     Input parameters
C     ________________
C
C     KUNIT      - The unit number of the land sea mask file.
C     KNWE       - The number of points in the West-East direction
C                  for this line of latitude.
C     KLONG      - The array of longitude points for this line of
C                  latitude.
C     KLATV      - The latitude for which the land sea mask is
C                  required.
C     KEXPAND    - Fixed array of length JPLG01 (The number of items
C                  in one latitude of the 10 minute land sea mask).
C                  This is used to improve efficiency.
C     KLEN       - The length of a full line of latitude in the file.
C
C     Output parameters
C     ________________
C
C     KLSM       - The land-sea mask for the specified longitude
C                  points at this line of latitude.
C
C     Return value
C     ____________
C
C     The error indicator (INTEGER).
C     0 + OK
C
C     Error and Warning Return Values
C     _______________________________
C
C     25501 Unexpected end of file found when positioning or reading
C           land sea mask file.
C     25502 Error return code when positioning or reading land sea
C           mask file.
C     25503 Attempt to extract bits beyond array bounds in routine
C           INXBIT.
C       :
C       :
C
C     Any error in this routine should be reported to Meteorological
C     Applications.
C
C     Common block usage
C     __________________
C
C     NONE
C
C     EXTERNALS
C     _________
C
C     INXBIT     - Standard library routine to extract bits from
C                  a packed array.
C     PBREAD     - Standard library routine to read an unstructured
C                  file.
C     PBSEEK     - Standard library routine to position a file
C                  before performing a transfer.
C     INTLOG(R)  - Logs messages.
C
C     METHOD
C     ______
C
C     The latitude value is used to select which line is required
C     from the 10 minute land sea mask file. This line is then read
C     into a buffer and the bit extraction routine used to expand the
C     line to contain one bit per word. The required words are then
C     extracted from this array.
C
C     The 10 minute land sea mask file contains 1080 lines of 2160
C     values with each value stored as one bit. (Each line of latitude
C     is packed in 68*32 = 2176 bits). Each value consists
C     of a 0 for sea or a 1 for land. The bits are assumed to be in
C     the centre of a 10 minute square (e.g. the first value is
C     assumed to be at 0  5' East and 89  55' North).
C
C     To reduce system overheads involved in multiple PBSEEKS and
C     PBREADS, the whole file is read into an array on the first call.
C
C     REFERENCE
C     _________
C
C     None
C
C     COMMENTS
C     ________
C
C     Program contains sections 0 to 3 and 9
C
C     AUTHOR
C     ______
C
C     K. Fielding      *ECMWF*      Nov 1993
C
C     MODIFICATIONS
C     _____________
C
C     J.D.Chambers      ECMWF       Oct 1996
C     The whole file is read into an array on the first call.
C     Number of function arguments reduced.
C
C     J.D.Chambers     ECMWF        Feb 1997
C     Allow for 64-bit pointers
C
C----<
C     _______________________________________________________
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
      IMPLICIT NONE
C
#include "parim.h"
#include "jparams.h"
C
C     Parameters
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 25500)
      INTEGER JPRAWSZ
      PARAMETER (JPRAWSZ = 73440 )
C
C     Function arguments
      INTEGER KUNIT, KLATV, KLEN, KNWE
      INTEGER KLONG (KNWE), KLSM (KNWE)
      INTEGER KEXPAND (JPLG01)
C
C     Local variables
      INTEGER ILAT, IOFF, IERR, JLON
      INTEGER LENRAW, RAWMASK, RAWSIZE, IFIRST
      INTEGER ISIZE
      DATA ISIZE/0/
      DIMENSION RAWMASK(1)
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IRAWMSK
#endif
#endif
      POINTER ( IRAWMSK, RAWMASK )
      SAVE IRAWMSK, ISIZE
C
      DATA RAWSIZE/JPRAWSZ/
C
      DATA IFIRST/0/
      SAVE IFIRST
C
C     _______________________________________________________
C
C*    Section 1. Initialisation
C     _______________________________________________________
C
  100 CONTINUE
C
      IGLSM01 = 0
C
      IF( NDBG.GT.2 ) THEN
        CALL INTLOG(JP_DEBUG,'IGLSM01: Section 1.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IGLSM01: Input parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IGLSM01: Latitude value = ',KLATV)
        CALL INTLOG(JP_DEBUG,'IGLSM01: Input rec len(words) = ',KLEN)
        CALL INTLOG(JP_DEBUG,'IGLSM01: No.long pts = ',KNWE)
      ENDIF
C
C     If it is the first time through, read in the whole land-sea
C     mask file

      IF( IFIRST.EQ.0 ) THEN
        IFIRST = 10
C
C       Find the file length
C
        CALL PBSEEK (KUNIT, 0, 2, LENRAW)
        IF ( LENRAW.LT.0 ) THEN
          IGLSM01 = JPROUTINE + 1
          CALL INTLOG(JP_FATAL,
     X      'IGLSM01: Error finding length of LSM file',JPQUIET)
          GO TO 900
        ENDIF
C
C       Rewind the file
C
        CALL INTLOG(JP_DEBUG,'IGLSM01: Rewind the LSM file.',JPQUIET)
        CALL PBSEEK (KUNIT, 0, 0, IERR)
        IF (IERR .NE. 0) THEN
          IGLSM01 = JPROUTINE + 2
          CALL INTLOG(JP_FATAL,
     X      'IGLSM01: Error rewinding LSM file',JPQUIET)
          GO TO 900
        ENDIF
C
C       Allocate memory for the array (first time only)
C
          IF( ISIZE.EQ.0 ) THEN
            ISIZE = ((JPRAWSZ*2+JPRLEN-1)*JPILEN)/JPRLEN
            CALL JMEMHAN( 13, IRAWMSK, ISIZE, 1, IERR)
            IF( IERR.NE.0 ) THEN
              CALL INTLOG(JP_ERROR,
     X          'IGLSM01: Memory allocation error.',IRAWMSK)
              IGLSM01 = JPROUTINE + 4
              GOTO 900
            ENDIF
          ENDIF
C
C       Read the file
C
        CALL INTLOG(JP_DEBUG,
     X    'IGLSM01: Read the LSM file of byte size = ',LENRAW)
        CALL PBREAD (KUNIT, RAWMASK, LENRAW, IERR)
        IF (IERR .NE. LENRAW) THEN
          IGLSM01 = JPROUTINE + 3
          CALL INTLOG(JP_FATAL,'IGLSM01: Unexpected EOF while',JPQUIET)
          CALL INTLOG(JP_FATAL,'IGLSM01: reading LSM file.',JPQUIET)
          GO TO 900
        ENDIF
      ENDIF
C     _______________________________________________________
C
C*    Section 2. Read one line of 10 minute land-sea mask
C                and expand into an unpacked array
C     _______________________________________________________
C
  200 CONTINUE
C
      IF(NDBG.GT.2) CALL INTLOG(JP_DEBUG,'IGLSM01: Section 2.',JPQUIET)
C
      ILAT = MIN ( ( (JP90 - KLATV) * JPML01) / JPMULT + 1, JPLT01)
      IOFF = (ILAT - 1)*KLEN*JPBYTES*8
C
      CALL INXBIT (RAWMASK, RAWSIZE, IOFF, KEXPAND, JPLG01, JPBITS, 1,
     X   'D', IERR)
C
      IF (IERR .NE. 0) THEN
        IGLSM01 = JPROUTINE + 5
        CALL INTLOG(JP_FATAL,'IGLSM01: Attempt to extract LSM',JPQUIET)
        CALL INTLOG(JP_FATAL,'IGLSM01: beyond array bounds.',JPQUIET)
        CALL INTLOG(JP_FATAL,'IGLSM01: Interpolation failed.',IGLSM01)
        GO TO 900
      ENDIF
C
C     Transfer expanded bitmask line to land sea mask array
C
      DO 210 JLON = 1, KNWE
        IOFF = (KLONG (JLON) * JPML01) / JPMULT + 1
        KLSM (JLON) = KEXPAND (IOFF)
  210 CONTINUE
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine.
C     _______________________________________________________
C
  900 CONTINUE
C
      IF(NDBG.GT.2) CALL INTLOG(JP_DEBUG,'IGLSM01: Section 9.',JPQUIET)
C
      RETURN
      END
