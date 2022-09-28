C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JOPNLLF( IALEG, PINTVL, KTRUNC, KUNIT, PBUILD, KRET)
C
C---->
C**** JOPNLLF
C
C     Purpose
C     _______
C
C     This routine finds a file of legendre polynomials corresponding
C     to a given grid interval and truncation, allocates memory, reads
C     the coefficients into the memory and returns a unit number.
C
C     Interface
C     _________
C
C     CALL JOPNLLF( IALEG, PINTVL, KTRUNC, KUNIT, PBUILD, KRET)
C
C     Input parameters
C     ________________
C
C     PINTVL  - Grid interval in degrees
C     KTRUNC  - Spherical truncation
C
C     Output parameters
C     _________________
C
C     IALEG   - Pointer for memory array for legendre coefficients
C     KUNIT   - Unit number from PBOPEN
C               0 , open failed
C     PBUILD  - Grid interval used to build the legendre 
C               coefficients file (degrees)
C     KRET    - Return status code
C               0 = OK
C
C     Common block usage
C     __________________
C
C     Uses common JDCSPGP for size NISIZE6
C
C     Method
C     ______
C
C     Builds a file name from the truncation and grid interval and
C     tries to open a file of that name.
C
C     If the file is already open (from a previous call), the
C     previous unit number is returned.
C     If a different file is already open (from a previous call), the
C     the existing file is closed.
C
C     If no file can be located, a file is created. Memory is dynamically
C     allocted and the coefficients are read into the memory array.
C
C     Externals
C     _________
C
C     PBOPEN3  - Open binary file and return unit number
C     PBREAD3  - Read binary file
C     PBCLOSE3 - Close binary file
C     JFINDIR  - Find the file of legendre coefficients
C     JMAKLL   - Makes a file of legendre coefficients
C     INTLOG   - Output log message
C     JCHMOD   - Change file permissions
C     RENAME   - Rename file
C
C     Reference
C     _________
C
C     NONE
C
C     Comments
C     ________
C
C     The Legendre polynomials for the transforms may be held in
C     a ready-made file whose name is held in an environment variable:
C         cf_txxxx_raabbbbb       Txxxx   aa.bbbbb degrees
C     For example,
C         cf_t0213_r0050000       T213    0.5 degrees
C         cf_t0106_r0250000       T106    2.5 degrees
C
C     On the C90, the file of polynomials may be cached in /owrk/marsint
C
C     Otherwise the file is located in (or will be created in) the first
C     directory given by one of the following (in the order listed, if
C     they exist):
C         environment variable PPDIR
C     or
C         the current working directory.
C
C     Author
C     ______
C
C     J.D.Chambers      *ECMWF*      Jun 1996
C
C     Modifications
C     _____________
C
C     J.D.Chambers     ECMWF        Feb 1997
C     Allow for 64-bit pointers
C
C----<
C     _______________________________________________________
C
      IMPLICIT NONE
C
#include "parim.h"
#include "jparams.h"
#include "jparam2.h"
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER ( JPROUTINE = 30910 )
C
C     Subroutine arguments
C
      REAL      PINTVL, PBUILD
      INTEGER   KTRUNC, KUNIT, KRET
C
C     Local variables
C
      INTEGER IRET, IOFFSET, ISIZE, NPBYTES
      INTEGER*8 IRET1, ITEMP
      CHARACTER*12 YFLAG
#ifdef REAL_8
      DATA NPBYTES/8/
#else
      DATA NPBYTES/4/
#endif
      LOGICAL LFOUND
      CHARACTER*256 DIRNAME
      CHARACTER*256 FILENAME, FILEDUM
      CHARACTER*512 FULLPATH
      CHARACTER*20 YPFN
#ifdef REAL_8
      DATA YPFN/'CF_Txxxx_Raabbbbb'/
#else
      DATA YPFN/'cf_txxxx_raabbbbb'/
#endif
      INTEGER NUNIT
      DATA NUNIT/0/
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IALEG
#endif
#endif
      REAL ALEG
      DIMENSION ALEG(1)
      POINTER ( IALEG, ALEG )
C
      SAVE NUNIT
C
C     Externals
C
      LOGICAL JFINDIR
      LOGICAL JFINDFN3
      INTEGER JCHMOD, RENAME
C
C     _______________________________________________________
C
C*    Section 1. See if required file already in use.
C     _______________________________________________________
C
  100 CONTINUE
C
C     Setup the filename: cf_txxxx_raabbbbb
C
      WRITE(YPFN(5:8),'(I4.4)') KTRUNC
      WRITE(YPFN(11:17),'(I7.7)') NINT(PINTVL*JPMULT)
      CALL INTLOG(JP_DEBUG,
     X  'JOPNLLF: Coefficients file to open is:', JPQUIET)
      CALL INTLOG(JP_DEBUG, YPFN, JPQUIET)
      PBUILD = PINTVL
C
C     If file already open, return the existing unit number
C
      IF( YPFN.EQ.YOLDLL ) THEN
        CALL INTLOG(JP_DEBUG,'JOPNLLF: File already open.',JPQUIET)
        KUNIT = NUNIT
        GOTO 900
      ENDIF
C
C     Otherwise, ensure existing unit is closed
C
      IF( NUNIT.NE.0 ) THEN
        CALL PBCLOSE3(NUNIT, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'JOPNLLF: PBCLOSE3 error',IRET)
          KRET = JPROUTINE + 1
          GOTO 990
        ENDIF
        NUNIT = 0
      ENDIF
C     _______________________________________________________
C
C*    Section 2. See if the file has already been created.
C     _______________________________________________________
C
  200 CONTINUE
C
C     Set appropriate build inteval 
C
      WRITE(YPFN(11:17),'(I7.7)') NINT(PBUILD*JPMULT)
C
      IOFFSET = INDEX(YPFN,' ') - 1
      FILENAME = YPFN(1:IOFFSET)
C
      LFOUND = JFINDIR('PPDIR', DIRNAME)
      IF( LFOUND ) THEN
        LFOUND = JFINDFN3(DIRNAME,FILENAME,IOFFSET,NUNIT)
        IF( LFOUND ) GOTO 500
      ENDIF
C
C     Try present working directory
C
      IOFFSET = INDEX(YPFN, ' ') - 1
      FILENAME = YPFN(1:IOFFSET)
      CALL PBOPEN3( NUNIT, FILENAME(1:IOFFSET), 'r', IRET)
      IF( IRET.EQ.0 ) GOTO 500
C     _______________________________________________________
C
C*    Section 3. File doesn't exist, find a suitable directory for it.
C     _______________________________________________________
C
  300 CONTINUE
C
      LFOUND = JFINDIR('PPDIR', DIRNAME)
      IF ( LFOUND ) THEN
        IOFFSET = INDEX(DIRNAME, ' ') - 1
        FULLPATH = DIRNAME(1:IOFFSET) // '/' // FILENAME
        IOFFSET = INDEX(FULLPATH, ' ') - 1
        FILENAME(1:IOFFSET) = FULLPATH(1:IOFFSET)
        FILEDUM(1:IOFFSET) = FILENAME(1:IOFFSET)
#ifdef REAL_8
        FILEDUM(IOFFSET-16:IOFFSET-15) = 'XX'
#else
        FILEDUM(IOFFSET-16:IOFFSET-15) = 'xx'
#endif
        CALL PBOPEN3(NUNIT, FILEDUM(1:IOFFSET), 'w', IRET)
        IF( IRET.EQ.0 ) GOTO 400
      ENDIF
C
C     Try present working directory
C
      IOFFSET = INDEX(YPFN,' ') - 1
      FILENAME = YPFN(1:IOFFSET)
      FILEDUM(1:IOFFSET) = FILENAME(1:IOFFSET)
#ifdef REAL_8
      FILEDUM(IOFFSET-16:IOFFSET-15) = 'XX'
#else
      FILEDUM(IOFFSET-16:IOFFSET-15) = 'xx'
#endif
      CALL PBOPEN3(NUNIT, FILEDUM(1:IOFFSET), 'w', IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_FATAL,'JOPNLLF: PBOPEN3 error.',IRET)
        KRET = JPROUTINE + 2
        GOTO 990
      ENDIF
C     _______________________________________________________
C
C*    Section 4. Create the coefficients file.
C     _______________________________________________________
C
  400 CONTINUE
C
C     Let user know that a new file is being created.
C
      IOFFSET = INDEX(FILENAME, ' ') - 1
      CALL INTLOG(JP_DEBUG,'JOPNLLF: Creating new coefficients file:',
     X            JPQUIET)
      CALL INTLOG(JP_DEBUG,FILENAME(1:IOFFSET), JPQUIET)
C
C     Change access mode to 'read only' for all users.
C
      IRET = JCHMOD(FILEDUM(1:IOFFSET),'0444')
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_FATAL,'JOPNLLF: JCHMOD error.',IRET)
        KRET = JPROUTINE + 3
        GOTO 990
      ENDIF
C
C     Allocate memory for the array used in making the file
C
      ISIZE = (KTRUNC+1)*(KTRUNC+4)*(NINT(90.0/PBUILD)+1)/2
      IF( ISIZE.GT.NISIZE6 ) THEN
        CALL JMEMHAN( 6, IALEG, ISIZE, 1, KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_FATAL,'JOPNLLF: memory allocation error',IALEG)
          KRET = JPROUTINE + 3
          GOTO 990
        ENDIF
        NISIZE6 = ISIZE
      ENDIF
C
C     Make coefficients file
C
      CALL JMAKLL3( NUNIT, KTRUNC, PBUILD, 0.0, ALEG, KRET)
      IF ( KRET .NE. 0 ) GOTO 990
C
C     De-allocate memory for the array used in making the file
C
      CALL JMEMHAN( 6, IALEG, ISIZE, 0, KRET)
      IF( KRET.NE.0 ) THEN
        CALL INTLOG(JP_FATAL,'JOPNLLF: memory dealloc error',IALEG)
        KRET = JPROUTINE + 4
        GOTO 990
      ENDIF
      NISIZE6 = 0
C
C     Close rename it, re-open for reading, leave it open.
C
      CALL PBCLOSE3( NUNIT, IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_FATAL,'JOPNLLF: PBCLOSE3 error.',IRET)
        KRET = JPROUTINE + 5
        GOTO 990
      ENDIF
      NUNIT=0
C
      IRET = RENAME(FILEDUM(1:IOFFSET),FILENAME(1:IOFFSET))
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_FATAL,'JOPNLLF: RENAME of file failed',JPQUIET)
        KRET = JPROUTINE + 5
        GOTO 990
      ENDIF
C
      CALL PBOPEN3( NUNIT, FILENAME(1:IOFFSET), 'r', IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_FATAL,'JOPNLLF: PBOPEN3 error.',IRET)
        KRET = JPROUTINE + 6
        GOTO 990
      ENDIF
C     _______________________________________________________
C
C*    Section 5. File now open with read access.
C     _______________________________________________________
C
  500 CONTINUE
C
C     Allocate memory for the file and read it into memory
C
      ISIZE = (KTRUNC+1)*(KTRUNC+4)*(NINT(90.0/PBUILD)+1)/2
      IF( ISIZE.GT.NISIZE6 ) THEN
        CALL JMEMHAN2( 6, IALEG, ISIZE, 1, KRET)
cs        CALL JMEMHAN( 6, IALEG, ISIZE, 1, KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_FATAL,'JOPNLLF: memory allocation error',IALEG)
          KRET = JPROUTINE + 7
          GOTO 990
        ENDIF
        NISIZE6 = ISIZE
      ENDIF
      ITEMP = ISIZE
      ITEMP = ITEMP*8
C
      CALL GETENV('USE_PBREAD4', YFLAG)
      IF(YFLAG(1:1).EQ.'1') THEN
           CALL PBREAD4(NUNIT, ALEG, ITEMP, IRET1)
      ELSE
           print*,'PBREAD5 ',ITEMP
           CALL PBREAD5(NUNIT, ALEG, ITEMP, IRET1)
      ENDIF
C
      IF( IRET1.NE.ITEMP ) THEN
        CALL INTLOG(JP_FATAL,'JOPNLLF: PBREAD5 error.',IRET1)
        KRET = JPROUTINE + 8
        GOTO 990
      ENDIF
cs      CALL PBREAD3(NUNIT, ALEG, (ISIZE*NPBYTES), IRET)
cs      IF( IRET.NE.(ISIZE*NPBYTES) ) THEN
cs        CALL INTLOG(JP_FATAL,'JOPNLLF: PBREAD3 error.',IRET)
cs        KRET = JPROUTINE + 8
cs        GOTO 990
cs      ENDIF
C
C     The file is no longer required
C
      CALL PBCLOSE3( NUNIT, IRET)
      IF ( IRET .NE. 0 ) THEN
        CALL INTLOG(JP_FATAL,'JOPNLLF: PBCLOSE3 error.',IRET)
        NUNIT = 0
        KRET = JPROUTINE + 9
        GOTO 990
      ENDIF
      NUNIT=0
C
      KUNIT = NUNIT
      YOLDLL  = YPFN
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
  900 CONTINUE
C
      KRET = 0
C
 990  CONTINUE
C
      RETURN
      END
