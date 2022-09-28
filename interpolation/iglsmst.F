C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGLSMST (KUNIT, KOFF, KNWE, KFIRST, KSTRIDE,
     1   KLEN, KLSM, NEWFILE)
C
C---->
C**** *IGLSMST*
C
C     PURPOSE
C     _______
C
C     Read and process one line of latitude from a standard land sea
C     mask file.
C
C     INTERFACE
C     _________
C
C     IERR = IGLSMST (KUNIT, KOFF, KNWE, KFIRST, KSTRIDE,
C    1   KLEN, KLSM)
C
C     Input parameters
C     ________________
C
C     KUNIT      - The unit number of the land sea mask file.
C     KOFF       - The word offset within the file of the required
C                  line. This is used to position the file.
C     KNWE       - The number of points in the West-East direction
C                  for this line of latitude.
C     KFIRST     - The position of the first land sea mask point
C                  required within the array.
C     KSTRIDE    - The distance between the longitude points required.
C     KLEN       - The length of a full latitude row of the land sea
C                  mask file.
C     NEWFILE    - 'YI' if the input land-sea mask file is to be
C                       accessed and it has changed
C                  'NI' if the input land-sea mask file is to be
C                       accessed and it has not changed
C                  'YO' if the output land-sea mask file is to be
C                       accessed and it has not changed
C                  'NO' if the output land-sea mask file is to be
C                       accessed and it has not changed
C
C     Output parameters
C     ________________
C
C     KLSM       - The array of land sea mask values for this
C                  latitude value and set of longitude points.
C
C     Return value
C     ____________
C
C     The error indicator (INTEGER).
C
C     Error and Warning Return Values
C     _______________________________
C
C     25401 Unexpected end of file found when positioning or reading
C           land sea mask file.
C     25402 Error return code when positioning or reading land sea
C           mask file.
C
C     Any error in this routine should be reported to Meteorological
C     Applications.
C
C     Common block usage
C     __________________
C
C     None
C
C     EXTERNALS
C     _________
C
C     PBREAD     - Standard library routine to read unstructured
C                  files.
C     PBSEEK     - Standard library routine to position an
C                  unstructured file.
C     INTLOG(R)  - Logs messages.
C
C     METHOD
C     ______
C
C     A full standard latitude line is transferred to the work array
C     and then the required points copied into the output array.
C
C     The land sea mask file is assumed to use one integer word for
C     each value even though these values can only be 0 or 1. This
C     gives a gain in efficiency at a cost in file size.
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
C     J.D.Chambers     ECMWF        June 1999
C     Use bitmaps for landsea masks
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
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 25400)
      INTEGER JPRAWSZ
      PARAMETER (JPRAWSZ = (JPLONG*JPLAT+(JPBITS-1))/JPBITS )
C
C     Function arguments
C
      INTEGER KUNIT, KOFF, KFIRST, KSTRIDE, KLEN, KNWE
      INTEGER KLSM (KNWE)
      CHARACTER*2 NEWFILE
C
C     Local variables
C
      INTEGER JDCSIZE, JDCINDX, JDCPNT, JDCRET, JLSM
      DIMENSION JLSM(1440)  ! good upto 0.25 degree latitude step
C
      INTEGER IPOINT, IERR, ISIZE
      DATA ISIZE/0/
      INTEGER JLON
      INTEGER LENRAW, IFIRST, IFILE, INDEX
      DIMENSION LENRAW(2)
      DIMENSION IFILE(2)
      INTEGER NRAWMSK
      DIMENSION NRAWMSK(1)
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IRAWMSK
#endif
#endif
      POINTER (IRAWMSK, NRAWMSK )
      DATA IFILE/0,0/
      SAVE IFILE, LENRAW, IRAWMSK, ISIZE
C     _______________________________________________________
C
C*    Section 1. Initialisation
C     _______________________________________________________
C
  100 CONTINUE
C
      IGLSMST = 0
C
      IF( NDBG.GT.2) THEN
        CALL INTLOG(JP_DEBUG,'IGLSMST: Section 1.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IGLSMST: Input parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IGLSMST: Word offset in file = ',KOFF)
        CALL INTLOG(JP_DEBUG,'IGLSMST: Input rec len(words)= ',KLEN)
        CALL INTLOG(JP_DEBUG,'IGLSMST: No. of longitude pts = ',KNWE)
        CALL INTLOG(JP_DEBUG,'IGLSMST: First point in line = ',KFIRST)
        CALL INTLOG(JP_DEBUG,'IGLSMST: Stride along line = ',KSTRIDE)
      ENDIF
C
C     If the input or output land-sea mask file has changed, reset
C     the stored unit
C
      IF( NEWFILE(2:2).EQ.'I' ) THEN
        IF( NEWFILE(1:1).EQ.'Y' ) THEN
          INDEX = 1
          IFILE(INDEX) = KUNIT
          IFIRST = 0
          CALL INTLOG(JP_DEBUG,'IGLSMST: New input LSM file.',JPQUIET)
        ELSE
          INDEX = 1
          IFIRST = 1
        ENDIF
      ENDIF
C
      IF( NEWFILE(2:2).EQ.'O' ) THEN
        IF( NEWFILE(1:1).EQ.'Y' ) THEN
          INDEX = 2
          IFILE(INDEX) = KUNIT
          IFIRST = 0
          CALL INTLOG(JP_DEBUG,'IGLSMST: New output LSM file.',JPQUIET)
        ELSE
          INDEX = 2
          IFIRST = 2
        ENDIF
      ENDIF
C
C     If it is the first time through, read in the whole land-sea
C     mask file
C
      IF( IFIRST.EQ.0 ) THEN
C
C       Find the file length
C
        CALL PBSEEK (KUNIT, 0, 2, LENRAW(INDEX))
        IF ( LENRAW(INDEX).LT.0 ) THEN
          IGLSMST = JPROUTINE + 1
          CALL INTLOG(JP_FATAL,
     X      'IGLSMST: Error finding length of LSM file',JPQUIET)
          GO TO 900
        ENDIF
C
C       Rewind the file
C
        CALL INTLOG(JP_DEBUG,'IGLSMST: Rewind the LSM file.',JPQUIET)
        CALL PBSEEK (KUNIT, 0, 0, IERR)
        IF ( IERR.NE.0 ) THEN
          IGLSMST = JPROUTINE + 2
          CALL INTLOG(JP_FATAL,
     X      'IGLSMST: Error rewinding LSM file',JPQUIET)
          GO TO 900
        ENDIF
C
C     Allocate memory for the array (first time only)
C
        IF( ISIZE.EQ.0 ) THEN
          ISIZE = JPRAWSZ*2
          CALL JMEMHAN( 12, IRAWMSK, ISIZE, 1, IERR)
          IF( IERR.NE.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'IGLSMST: Memory allocation error.',IRAWMSK)
            IGLSMST = JPROUTINE + 3
            GOTO 900
          ENDIF
        ENDIF
C
C       Read the file (ignore end-of-file).
C
        CALL INTLOG(JP_DEBUG,'IGLSMST: Read the LSM file',LENRAW(INDEX))
        CALL PBREAD(KUNIT, NRAWMSK((INDEX-1)*JPRAWSZ+1),
     X              LENRAW(INDEX), IERR)
C
        IF( IERR.NE.LENRAW(INDEX) ) THEN
          CALL INTLOG(JP_ERROR,
     X      'IGLSMST: Land-sea mask file read error.',JPQUIET)
          IGLSMST = JPROUTINE + 4
          GOTO 900
        ENDIF
C
      ENDIF
C
C     _______________________________________________________
C
C     Section 2. Transfer file to land sea mask array
C     _______________________________________________________
C
  200 CONTINUE
C
      IF( NDBG.GT.2) CALL INTLOG(JP_DEBUG,'IGLSMST: Section 2.',JPQUIET)
C
C     Needs whole line of land sea mask with no adjustment
C
      IF( (KLEN.EQ.KNWE).AND.(KFIRST.EQ.1) ) THEN
        JDCSIZE = (LENRAW(INDEX)*8+(JPBITS-1))/JPBITS
        JDCINDX = (INDEX-1)*JPRAWSZ+(KOFF/JPBITS) + 1
        JDCPNT = MOD(KOFF,JPBITS)
        CALL INXBIT(NRAWMSK(JDCINDX), JDCSIZE, JDCPNT,
     X              KLSM, KNWE, JPBITS, 1, 'D', JDCRET)
        IF( JDCRET.NE.0 ) THEN
          IGLSMST = JPROUTINE + 4
          GOTO 900
        ENDIF
C
      ELSE
C
C     Needs part of land sea mask array or different starting
C     point
C
C       Unpack whole latitude
C
        JDCSIZE = (LENRAW(INDEX)*8+(JPBITS-1))/JPBITS
        JDCINDX = (INDEX-1)*JPRAWSZ+(KOFF/JPBITS) + 1
        JDCPNT = MOD(KOFF,JPBITS)
        CALL INXBIT(NRAWMSK(JDCINDX), JDCSIZE, JDCPNT,
     X              JLSM, KLEN, JPBITS, 1, 'D', JDCRET)
        IF( JDCRET.NE.0 ) THEN
          IGLSMST = JPROUTINE + 4
          GOTO 900
        ENDIF
C
C       Select required points
C
        DO 220 JLON = 1, KNWE
          IPOINT = MOD(KLEN + KFIRST + KSTRIDE*(JLON-1) - 1, KLEN) + 1
          KLSM(JLON) = JLSM(IPOINT)
  220   CONTINUE


      ENDIF
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
  900 CONTINUE
C
      IF( NDBG.GT.2) CALL INTLOG(JP_DEBUG,'IGLSMST: Section 9.',JPQUIET)
C
      RETURN
      END
