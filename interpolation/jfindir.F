C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      LOGICAL FUNCTION JFINDIR( ENVNAME, DIRNAME)
C
C---->
C**** JFINDIR
C
C     Purpose
C     _______
C
C     This routine finds the directory specified by an environment
C     variable.
C
C     Interface
C     _________
C
C     LFOUND = JFINDIR( ENVNAME, DIRNAME)
C
C     Input parameters
C     ________________
C
C     ENVNAME - The environment variable.
C
C     Output parameters
C     _________________
C
C     DIRNAME - The directory name (if found).
C
C     The function returns .TRUE. if the environment variable gives
C     a directory name; otherwise it returns .FALSE.
C
C     Common block usage
C     __________________
C
C     None.
C
C     Externals
C     _________
C
C     Reference
C     _________
C
C     GETENV  - to read the envirment variable.
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
      CHARACTER*(*) ENVNAME, DIRNAME
C
C     Local variables
      INTEGER IBLANK
C
C     _______________________________________________________
C
C*    Section 1. Try the environment variable.
C     _______________________________________________________
C
  100 CONTINUE
C
      JFINDIR = .TRUE.
C
      CALL GETENV(ENVNAME, DIRNAME)
C
C     See if a name was returned.
      IBLANK = INDEX(DIRNAME, ' ')
      IF ( IBLANK.LE.1 ) JFINDIR = .FALSE.
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
