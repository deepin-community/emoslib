! Copyright 1981-2016 ECMWF.
!
! This software is licensed under the terms of the Apache Licence
! Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.
!

      INTEGER FUNCTION DPATH_TABLES_INTERPOL(DPATH)
!
!---->
!**** DPATH_TABLES_INTERPOL
!
!     Purpose
!     -------
!
!     This routine returns the directory path for interpolation tables.
!     Returned path (as argument) has path separator suffixed (/).
!
!
!     Interface
!     ---------
!
!     IRET = DPATH_TABLES_INTERPOL(DPATH)
!
!
!     Input parameters
!     ----------------
!
!     DPATH - argument in which tables directory path is returned
!
!
!     Output parameters
!     -----------------
!
!     DPATH_TABLES_INTERPOL - length of path string, 0 if not successful
!
!
!     Method
!     ------
!
!     It respects this order (each overriding the previous, if set):
!     - suitable default value
!     - compile-time definition of INTERPOL_TABLES_PATH
!     - run-time env. variable EMOSLIB_FILES
!     - ...                    HIRLAM_LSM_PATH
!     - ...                    MARS_LSM_PATH
!
!
!     Externals
!     ---------
!
!     GETENV  - Read environment variable
!
!
!     Author
!     ------
!
!     P. Maciel      ECMWF      February 2015
!
!----<


!*    Section 0. Definition of variables
      IMPLICIT NONE

      CHARACTER*256 DPATH
      CHARACTER*256 TMPDPATH
      INTEGER IDX

      CHARACTER*13  ENVPATH1
      CHARACTER*15  ENVPATH2
      CHARACTER*13  ENVPATH3
      CHARACTER*39  DEFPATH1
      CHARACTER*256 DEFPATH2

      PARAMETER (ENVPATH1 = 'EMOSLIB_FILES')
      PARAMETER (ENVPATH2 = 'HIRLAM_LSM_PATH')
      PARAMETER (ENVPATH3 = 'MARS_LSM_PATH')
      PARAMETER (DEFPATH1 = '/usr/local/apps/libemos/tables/interpol')
#ifdef INTERPOL_TABLES_PATH
      DATA DEFPATH2 / &
       INTERPOL_TABLES_PATH &
      /
#else
      DATA DEFPATH2 / ' ' /
#endif


!*    Section 1. initialize: reset paths and return value
  100 CONTINUE
      DPATH = ' '
      TMPDPATH = ' '
      DPATH_TABLES_INTERPOL = 0;


!*    Section 2. Attempt reading env. variables, in order of priority
  200 CONTINUE
      CALL GETENV(ENVPATH3,TMPDPATH)
      IF (INDEX(TMPDPATH,' ').EQ.1) CALL GETENV(ENVPATH2,TMPDPATH)
      IF (INDEX(TMPDPATH,' ').EQ.1) CALL GETENV(ENVPATH1,TMPDPATH)

      IDX = INDEX(TMPDPATH,' ') - 1
      IF (IDX.GT.0) THEN
        DPATH = TMPDPATH(1:IDX)
        GOTO 900
      END IF


!*    Section 3. set default, possibly overriden by compile definition
!     (definition is for base directory, so interpol/ need appending)
  300 CONTINUE
      DPATH = DEFPATH1
      IDX = INDEX(DEFPATH2,' ') - 1
      IF (IDX.GT.0) DPATH = DEFPATH2(1:IDX) // '/interpol'


!*    Section 9. Append /, return path string length (otherwise zero)
  900 CONTINUE
      IDX = INDEX(DPATH,' ')
      IF (IDX.GT.1) THEN
        DPATH(IDX:IDX) = '/'
        DPATH_TABLES_INTERPOL = IDX
      END IF
      RETURN
      END

