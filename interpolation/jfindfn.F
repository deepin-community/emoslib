C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      LOGICAL FUNCTION JFINDFN( DIRNAME, FILENAME, KLEN, KUNIT)
C
C---->
C**** JFINDFN
C
C     Purpose
C     _______
C
C     Completes the pathname given by a directory and filename, and
C     checks if the file exists.
C
C     Interface
C     _________
C
C     LFOUND = JFINDFN( DIRNAME, FILENAME, KLEN, KUNIT)
C
C     Input parameters
C     ________________
C
C     DIRNAME  - the directory name.
C     FILENAME - the file name.
C     KLEN     - number of characters in the name
C
C     Output parameters
C     _________________
C
C     The function returns .TRUE. if the file is opened; in which case:
C     FILENAME - the full pathname.
C     KLEN     - number of characters in the full pathname
C     KUNIT    - the opened file descriptor (from PBOPEN).
C
C     Otherwise it returns .FALSE. and FILENAME and KLEN are unchanged
C
C     Common block usage
C     __________________
C
C     None.
C
C     Externals
C     _________
C
C     PBOPEN  - opens a file.
C
C     Reference
C     _________
C
C     None.
C
C     Comments
C     ________
C
C     None.
C 
C     Author
C     ______
C
C     J.D.Chambers      *ECMWF*      Mar 1996
C
C----<
C     _______________________________________________________
C
      IMPLICIT NONE
C
C     Parameters
Cjdc  INTEGER JPROUTINE
Cjdc  PARAMETER ( JPROUTINE = 30800 )
C
C     Subroutine arguments
      CHARACTER*(*) DIRNAME, FILENAME
      INTEGER KLEN, KUNIT
C
C     Local variables
      INTEGER IBLANK, IRET
      CHARACTER*512 FULLPATH
      CHARACTER*256 SAVENAME
C

C     _______________________________________________________
C
C*    Section 1. Try the full path name..
C     _______________________________________________________
C
  100 CONTINUE
C
      JFINDFN = .TRUE.
      SAVENAME = FILENAME
C
      IBLANK = INDEX(DIRNAME, ' ') - 1
      FULLPATH  = DIRNAME(1:IBLANK) // '/' // FILENAME(1:KLEN)
      IBLANK = INDEX(FULLPATH, ' ') - 1
      FILENAME(1:IBLANK) = FULLPATH(1:IBLANK)
C
Cjdc  print*,'***',FILENAME(1:IBLANK),'***'
      CALL PBOPEN(KUNIT, FILENAME(1:IBLANK), 'r', IRET)
      IF( IRET.NE.0 ) THEN
        FILENAME = SAVENAME
        JFINDFN = .FALSE.
      ELSE
        KLEN = INDEX(FULLPATH, ' ') - 1
        FILENAME(1:KLEN) = FULLPATH(1:KLEN)
      ENDIF
C
C     _______________________________________________________
C
C*    Section 9. Return.
C     _______________________________________________________
C
  900 CONTINUE
C
      RETURN
      END
