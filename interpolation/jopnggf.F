C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JOPNGGF(IPLEG,KNUM,KTRUNC,KUNIT,PLAT,KRET)
C
C---->
C**** JOPNGGF
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
C     CALL JOPNGGF(IPLEG,KNUM,KTRUNC,KUNIT,PLAT,KRET)
C
C     Input parameters
C     ________________
C
C     KNUM    - Gaussian grid number
C     KTRUNC  - Spherical truncation
C     PLAT    - Array of latitudes for the gaussian grid
C
C     Output parameters
C     _________________
C
C     IPLEG   - Pointer to memory array used for legendre coefficients
C     KUNIT   - Unit number from PBOPEN
C               0 , open failed
C     KRET    - Return status code
C               0 = OK
C
C     Common block usage
C     __________________
C
C     Uses common JDCSPGP for size NISIZE7
C
C     Method
C     ______
C
C     Builds a file name from the truncation and grid interval and
C     tries to open a file of that name.
C
C     If the file is already open (from a previous call) the
C     previous unit number is returned.
C     If a different file is already open (from a previous call), the
C     existing file is closed.
C
C     If no file can be found, a file is created. Memory is dynamically
C     allocted and the coefficients are read into the memory array.
C
C     Externals
C     _________
C
C     PBOPEN3  - Open binary file and return unit number
C     PBREAD3  - Read binary file
C     PBCLOSE3 - Close binary file
C     JFINDIR  - Find the file of legendre coefficients
C     JMAKGG   - Makes a file of legendre coefficients
C     INTLOG   - Output log message
C
C     Reference
C     _________
C
C     NONE
C
C     Comments
C     ________
C
C     The filename for the legendre polynomials has the form:
C         cf_xxxx_nnnn   Truncation xxxx, Gaussian number nnnn
C     For example,
C         cf_0213_0160          T213    N160
C         cf_0106_0080          T106    N80
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
C     S.Curic      *ECMWF*      Apr 2005
C     Change filename format because of N1048
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
      PARAMETER ( JPROUTINE = 30810 )
C
C     Subroutine arguments
C
      REAL      PLAT(*)
      INTEGER   KNUM, KTRUNC, KUNIT, KRET
C
C     Local variables
C
      INTEGER IRET
      INTEGER IOFFSET
      INTEGER NPBYTES
      INTEGER ISIZE
      INTEGER*8 ITEMP
      INTEGER IRET1
      CHARACTER*12 YFLAG
#ifdef REAL_8
      DATA NPBYTES/8/
#else
      DATA NPBYTES/4/
#endif
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IRPLEG
#endif
#endif
      REAL RPLEG(1)
      POINTER ( IRPLEG, RPLEG )
      LOGICAL LFOUND
      CHARACTER*256 DIRNAME
      CHARACTER*256 FILENAME
      CHARACTER*512 FULLPATH
      CHARACTER*20 YPFN
#ifdef REAL_8
      DATA YPFN/'CF_xxxx_nnnn'/
#else
      DATA YPFN/'cf_xxxx_nnnn'/
#endif
      INTEGER NUNIT
      DATA NUNIT/0/
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IPLEG
#endif
#endif
      REAL ALEG
      DIMENSION ALEG(1)
      POINTER ( IPLEG, ALEG )
C
      SAVE NUNIT
C
C     Externals
C
      LOGICAL JFINDIR, JFINDFN3
      INTEGER JCHMOD
      EXTERNAL JFINDIR, JFINDFN3
      EXTERNAL JCHMOD
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
      WRITE(YPFN(4: 7), '(I4.4)') KTRUNC
      WRITE(YPFN(9:12), '(I4.4)') KNUM
      CALL INTLOG(JP_DEBUG,
     X  'JOPNGGF: Coefficients file to open is:', JPQUIET)
      CALL INTLOG(JP_DEBUG, YPFN, JPQUIET)
C
C     If file already open, return the existing unit number
C
      IF( YPFN.EQ.YOLDGG ) THEN
        CALL INTLOG(JP_DEBUG,'JOPNGGF: File already open.',JPQUIET)
        KUNIT = NUNIT
        GOTO 900
C
      ELSE
C
C       Otherwise, ensure existing unit is closed
C
        IF( NUNIT.NE.0 ) THEN
          CALL PBCLOSE3( NUNIT, IRET)
          NUNIT=0
        ENDIF
      ENDIF
C
C     _______________________________________________________
C
C*    Section 2. See if the file has already been created.
C     _______________________________________________________
C
  200 CONTINUE
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
      IOFFSET = INDEX(YPFN,' ') - 1
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
      IF( LFOUND ) THEN
        IOFFSET = INDEX(DIRNAME, ' ') - 1
        FULLPATH  = DIRNAME(1:IOFFSET) // '/' // FILENAME
        IOFFSET = INDEX(FULLPATH, ' ') - 1
        FILENAME(1:IOFFSET) = FULLPATH(1:IOFFSET)
        CALL PBOPEN3( NUNIT, FILENAME(1:IOFFSET), 'w', IRET)
        IF( IRET.EQ.0 ) GOTO 400
      ENDIF
C
C     Try present working directory
C
      IOFFSET = INDEX(YPFN,' ') - 1
      FILENAME = YPFN(1:IOFFSET)
      CALL PBOPEN3( NUNIT, FILENAME(1:IOFFSET), 'w', IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_FATAL,'JOPNGGF: PBOPEN3 error.',IRET)
        KRET = JPROUTINE + 1
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
      CALL INTLOG(JP_DEBUG,'JOPNGGF: Creating new coefficients file:',
     X            JPQUIET)
      CALL INTLOG(JP_DEBUG,FILENAME(1:IOFFSET), JPQUIET)
C
C     Change access mode to 'read only' for all users.
C
      IRET = JCHMOD(FILENAME(1:IOFFSET),'0444')
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_FATAL,'JOPNGGF: JCHMOD error.',IRET)
        KRET = JPROUTINE + 2
        GOTO 990
      ENDIF
C
C     Allocate memory for the array used in making the file
C
      ISIZE = (KTRUNC+1)*(KTRUNC+4)/2
      IF( ISIZE.GT.NISIZE7 ) THEN
        CALL JMEMHAN( 7, IRPLEG, ISIZE, 1, KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_FATAL,'JOPNGGF: memory allocate error',IRPLEG)
          KRET = JPROUTINE + 3
          GOTO 990
        ENDIF
        NISIZE7 = ISIZE
      ENDIF
C
C     Make coefficients file
C
      CALL JMAKGG3( NUNIT, KTRUNC, KNUM, PLAT, RPLEG, KRET)
      IF( KRET.NE.0 ) GOTO 990
C
C     De-allocate memory for the array used in making the file
C
      CALL JMEMHAN( 7, IRPLEG, ISIZE, 0, KRET)
      IF( KRET.NE.0 ) THEN
        CALL INTLOG(JP_FATAL,'JOPNGGF: memory dealloc error',IRPLEG)
        KRET = JPROUTINE + 4
        GOTO 990
      ENDIF
      NISIZE7 = 0
C
C     Close coefficients file
C
      CALL PBCLOSE3( NUNIT, IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_FATAL,'JOPNGGF: PBCLOSE error.',IRET)
        KRET = JPROUTINE + 5
        GOTO 990
      ENDIF
      NUNIT=0
C
C     Reopen with read access
C
      CALL PBOPEN3( NUNIT, FILENAME(1:IOFFSET), 'r', IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_FATAL,'JOPNGGF: PBOPEN error.',IRET)
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
C     Allocate memory for the file and read the file into memory
C
      ISIZE = ((KTRUNC+1)*(KTRUNC+4)/2)*KNUM
      IF( ISIZE.GT.NISIZE7 ) THEN
            CALL JMEMHAN2( 7, IPLEG, ISIZE, 1, KRET)
cs            CALL JMEMHAN( 7, IPLEG, ISIZE, 1, KRET)
        IF( KRET.NE.0 ) THEN
          CALL INTLOG(JP_FATAL,'JOPNGGF: memory allocation error',IPLEG)
          KRET = JPROUTINE + 7
          GOTO 990
        ENDIF
        NISIZE7 = ISIZE
      ENDIF
C
       ITEMP = ISIZE
cs       ITEMP = ISIZE*NPBYTES
       ITEMP = ITEMP*8

      CALL GETENV('USE_PBREAD4', YFLAG)
      IF(YFLAG(1:1).EQ.'1') THEN
           CALL PBREAD4(NUNIT, ALEG, ITEMP, IRET1)
      ELSE
cs          print*,'PBREAD5 -----------'
           CALL PBREAD5(NUNIT, ALEG, ITEMP, IRET1)
      ENDIF
           IF( IRET1.NE.ITEMP ) THEN
              CALL INTLOG(JP_FATAL,'JOPNGGF: PBREAD4 error.',IRET1)
              KRET = JPROUTINE + 8
              GOTO 990
           ENDIF
cs           CALL PBREAD3(NUNIT, ALEG, (ISIZE*NPBYTES), IRET)
cs           IF( IRET.NE.(ISIZE*NPBYTES) ) THEN
cs             CALL INTLOG(JP_FATAL,'JOPNGGF: PBREAD3 error.',IRET)
cs             KRET = JPROUTINE + 8
cs             GOTO 990
cs           ENDIF
C
C     The file is no longer required
C
      CALL PBCLOSE3( NUNIT, IRET)
      IF( IRET.NE.0 ) THEN
        CALL INTLOG(JP_FATAL,'JOPNGGF: PBCLOSE3 error.',IRET)
        NUNIT = 0
        KRET = JPROUTINE + 9
        GOTO 990
      ENDIF
      NUNIT=0
C
      KUNIT = NUNIT
      YOLDGG = YPFN
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine.
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
