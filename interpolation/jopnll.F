C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JOPNLL( PLEG, PINTVL, KTRUNC, KUNIT, PBUILD, KRET)
C
C---->
C**** JOPNLL
C
C     Purpose
C     _______
C
C     This routine finds a file of legendre polynomials corresponding
C     to a given grid interval and truncation and returns a unit number.
C
C     Interface
C     _________
C
C     CALL JOPNLL( PLEG, PINTVL, KTRUNC, KUNIT, PBUILD, KRET)
C
C     Input parameters
C     ________________
C
C     PLEG    - Array for legendre function
C     PINTVL  - Grid interval in degrees
C     KTRUNC  - Spherical truncation
C
C     Output parameters
C     _________________
C
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
C     None
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
C     existing file is closed first.
C
C     If no file can be located, a file is created.
C
C     Externals
C     _________
C
C     PBOPEN   - Open binary file and return unit number
C     PBCLOSE  - Close binary file
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
C         cf_txxxx_raabbbbb       Txxx   aa.bbbbb degrees
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
C     J.D.Chambers      *ECMWF*      Nov 1993
C
C     Modifications
C     _____________
C
C     J.D.Chambers      *ECMWF*      Mar 1996
C     Standardise the search order for the environment variables.
C
C----<
C     _______________________________________________________
C
      IMPLICIT NONE
C
#include "parim.h"
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER( JPROUTINE = 30900 )
C
C     Subroutine arguments
C
      REAL      PLEG, PINTVL, PBUILD
      DIMENSION PLEG(*)
      INTEGER   KTRUNC, KUNIT, KRET
C
C     Local variables
C
      INTEGER IRET, IOFFSET
      LOGICAL LFOUND
      CHARACTER*256 DIRNAME
      CHARACTER*256 FILENAME, FILEDUM
      CHARACTER*512 FULLPATH
      CHARACTER*20 YPFN
      CHARACTER*20 YOLD
#ifdef REAL_8
      DATA YPFN/'CF_Txxxx_Raabbbbb'/
#else
      DATA YPFN/'cf_txxxx_raabbbbb'/
#endif
      DATA YOLD/'xxxxxxxxxxxxxxxxxxxx'/
      INTEGER NUNIT
      DATA NUNIT/0/
C
      SAVE NUNIT, YOLD
C
C     Externals
C
      LOGICAL JFINDIR
      LOGICAL JFINDFN
      INTEGER JCHMOD, RENAME
C
C     _______________________________________________________
C
C*    Section 1. See if required file already in use.
C     _______________________________________________________
C
  100 CONTINUE
C
C     Setup the file name
C
      WRITE(YPFN(5:8),'(I4.4)') KTRUNC
      WRITE(YPFN(11:17),'(I7.7)') NINT(PINTVL*JPMULT)
      CALL INTLOG(JP_DEBUG,
     X  'JOPNLL: Coefficients file to open is:', JPQUIET)
      CALL INTLOG(JP_DEBUG, YPFN, JPQUIET)
      PBUILD = PINTVL
C
C     If file already open, return the existing unit number
C
      IF( YPFN.EQ.YOLD ) THEN
        CALL INTLOG(JP_DEBUG,'JOPNLL: File already open.',JPQUIET)
        KUNIT = NUNIT
        GOTO 900
      ENDIF
C
C     Otherwise, close the existing file
C
      IF( NUNIT.NE.0 ) THEN
        CALL PBCLOSE(NUNIT, IRET)
        IF( IRET.NE.0 ) THEN
          CALL INTLOG(JP_ERROR,'JOPNLL: PBCLOSE error',IRET)
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
        LFOUND = JFINDFN(DIRNAME,FILENAME,IOFFSET,NUNIT)
        IF( LFOUND ) GOTO 500
      ENDIF
C
C     Try present working directory
C
      IOFFSET = INDEX(YPFN, ' ') - 1
      FILENAME = YPFN(1:IOFFSET)
      CALL PBOPEN( NUNIT, FILENAME(1:IOFFSET), 'r', IRET)
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
        CALL PBOPEN(NUNIT, FILEDUM(1:IOFFSET), 'w', IRET)
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
      CALL PBOPEN(NUNIT, FILEDUM(1:IOFFSET), 'w', IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_FATAL,'JOPNLL: PBOPEN error.',IRET)
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
      CALL INTLOG(JP_DEBUG,'JOPNLL: Creating new coefficients file:',
     X            JPQUIET)
      CALL INTLOG(JP_DEBUG,FILENAME(1:IOFFSET), JPQUIET)
C
C     Change access mode to 'read only' for all users.
C
      IRET = JCHMOD(FILEDUM(1:IOFFSET),'0444')
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_FATAL,'JOPNLL: JCHMOD error.',IRET)
        KRET = JPROUTINE + 3
        GOTO 990
      ENDIF
C
C     Make coefficients file
C
      CALL JMAKLL( NUNIT, KTRUNC, PBUILD, 0.0, PLEG, KRET)
      IF( KRET.NE.0 ) GOTO 990
C
C     Close it, rename it, re-open for reading, leave it open.
C
      CALL PBCLOSE(NUNIT, IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_ERROR,'JOPNLL: PBCLOSE error.',IRET)
        KRET = JPROUTINE + 4
        GOTO 990
      ENDIF
C
      FILEDUM(IOFFSET+1:IOFFSET+1) = CHAR(0)
      FILENAME(IOFFSET+1:IOFFSET+1) = CHAR(0)
      IRET = RENAME(FILEDUM(1:IOFFSET),FILENAME(1:IOFFSET))
#ifndef hpR64
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_FATAL,'JOPNLL: RENAME of file failed',JPQUIET)
        KRET = JPROUTINE + 5
        GOTO 990
      ENDIF
#endif
C
      CALL PBOPEN(NUNIT, FILENAME(1:IOFFSET), 'r', IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_FATAL,'JOPNLL: PBOPEN error.',IRET)
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
      KUNIT = NUNIT
      YOLD  = YPFN
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
