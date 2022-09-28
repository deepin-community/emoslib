      SUBROUTINE GRSDEF
C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C
C
C---->
C**** GRSDEF
C
C     Purpose.
C     --------
C     Sets initial values for common area variables and environment
C     variables for the GRIBEX package, if not already done.
C
C
C     Interface.
C     ----------
C     CALL GRSDEF

C
C     Input Parameters.
C     -----------------
C     None.
C
C
C     Output Parameters.
C     ------------------
C     None.
C
C
C     Method.
C     -------
C     Self-explanatory.
C
C
C     Externals.
C     ----------
C     EMOSNUM - Returns current EMOSLIB version number
C
C
C     Reference.
C     ----------
C     See subroutine GRIBEX.
C
C
C     Comments.
C     ---------
C     None
C
C
C     Author.
C     -------
C     J. Clochard, Meteo France, for ECMWF - March 1998.
C
C
C     Modifications.
C     --------------
C     J. Clochard, Meteo France, for ECMWF - June 1999.
C     Add variable NSUBCE.
C     Use a static variable to determine if initialisation has already
C     been done. NUSER removed .
C     Reverse defaults for NEXT2O and NLOC2O, for consistency with
C     version 13.023 of software .
C
C     J.D.Chambers     ECMWF   September 2002
C     Read environment variables first time through.
C
C     E. Fucile   ECMWF  October 2008
C     added environment variable GRIBEX_DUMP_DATA_ON_ERROR
C     to dump data files in a file when an error occurs
C
C----<
C     ----------------------------------------------------------------
C*    Section 0 . Definition of variables.
C     ----------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "common/grprs.h"
#include "grbcom.h"
C
C     Local variables
C
      INTEGER IOFFSET
      CHARACTER*10 HNDBG, HNVCK, YNUMBER
      CHARACTER*256 HDEFAULT, HBTDEFT
      LOGICAL LFIRST
      SAVE LFIRST
C
#ifdef TABLE_PATH
      DATA HDEFAULT 
     X / TABLE_PATH /
#else
      DATA HDEFAULT / '' /
#endif

#ifdef __uxp__
      DATA HBTDEFT /'/vpp700/mrfs/postproc/'/
#else
c      DATA HBTDEFT /'/home/ma/emos/tables/'/
      DATA HBTDEFT /'/usr/local/apps/libemos/tables/'/
#endif
C
      DATA LFIRST / .TRUE. /
C
C     External(s)
C
      INTEGER EMOSNUM
C
C     ----------------------------------------------------------------
C*    Section 1 . Set values, conditionally.
C     ----------------------------------------------------------------
C
  100 CONTINUE
C
      IF( .NOT. LFIRST) GOTO 900
C
C     Mark common area values set.
      LFIRST = .FALSE.
C
C     Common area variables have not been set. Set them.
C
C     User supplied reference value.
      FREF   = 0.0
C
C     Reference value supplied by user flag. Set to off.
      NFREF  = 0
C
C     User supplied maximum value.
      FMAX   = 0.0
C
C     Maximum value supplied by user flag. Set to off.
      NFMAX  = 0
C
C     Set rounding to 120 bytes on.
      NRND   = 1
C
C     Set P factor switch to default, user supplies the P factor.
      NONOFF = 0
C
C     Set abort flag to NO abort
      NOABORT = 1
C
C     Exhaustive use of all possible second-order packing methods
C     for HOPER='K'. Set to off.
      NUM2OK  = 0
C
C     Use of extended second-order packing methods for grid-point
C     encoding (HOPER='C' and 'K'). Set to on.
      NEXT2O  = 1
C
C     Use of non-local second-order packing methods for grid-point
C     encoding (HOPER='C' and 'K'). Set to on.
      NLOC2O  = 1
C
C     Use of (all valid) sub-centre values for ECMWF fields encoding .
C     encoding. Set to off.
      NSUBCE  = 0
C
C     ----------------------------------------------------------------
C*    Section 2. Set up values controlled by environment variables
C     ----------------------------------------------------------------
C
  200 CONTINUE
C
C     Set debug print on/off.
C
      NDBG   = 0
      CALL GETENV('GRIBEX_DEBUG',HNDBG)
      IOFFSET = INDEX( HNDBG, ' ')
      IF( IOFFSET.GT.1 ) THEN
        IF( HNDBG(1:2).EQ.'ON') THEN
          NDBG = 1
        ELSE IF( HNDBG(1:1).EQ.'1') THEN
          NDBG = 1
        ELSE IF( HNDBG(1:1).EQ.'2') THEN
          NDBG = 2
        ELSE
          NDBG = 0
        ENDIF
      ENDIF
C
C     Set DUMPDATA to dump data values on error
C
      DUMPDATA   = 0
      CALL GETENV('GRIBEX_DUMP_DATA_ON_ERROR',DUMPPATH)
      IOFFSET = INDEX( DUMPPATH, ' ')
      IF( IOFFSET.GT.2 ) THEN
        IF( DUMPPATH(1:3).EQ.'OFF') THEN
          DUMPDATA=0
        ELSE
          DUMPDATA=1
        ENDIF
      ENDIF

C
C     Set GRIB value checking on/off.
C
      NVCK   = 1
      CALL GETENV('GRIBEX_CHECK',HNVCK)
      IOFFSET = INDEX( HNVCK, ' ')
      IF( IOFFSET.GT.2 ) THEN
        IF( HNVCK(1:3).EQ.'OFF') THEN
          NVCK = 0
        ELSE
          NVCK = 1
        ENDIF
      ENDIF
C
C     See if output stream needs changing
C
      CALL GETENV('GRPRS_STREAM', YNUMBER)
      IOFFSET = INDEX( YNUMBER, ' ')
      IF( IOFFSET.GT.1 ) THEN
        IF( IOFFSET.EQ.2 ) THEN
          READ(YNUMBER,'(I1.1)') GRPRSM
        ELSE IF( IOFFSET.EQ.3 ) THEN
          READ(YNUMBER,'(I2.2)') GRPRSM
        ELSE
          WRITE(GRPRSM,*)
     X    'GRSDEF: Invalid value for GRPRS_STREAM: ' // YNUMBER
          GRPRSM = 6
        ENDIF
      ELSE
        GRPRSM = 6
      ENDIF
C
      IF( GRPRSM.LT.1 ) THEN
        WRITE(*,*) 'GRSDEF: Invalid number for GRPRS_STREAM: ', GRPRSM
        GRPRSM = 6
      ENDIF
C
C     Set EMOSLIB version number
C
      IF( (NEMOSLB.EQ.0).OR.(NEMOSET.EQ.0) ) THEN
        IF( NDBG.GT.0 ) THEN
          NEMOSLB = EMOSNUM(0)
        ELSE
          NEMOSLB = EMOSNUM(1)
        ENDIF
        NEMOSET = 1
      ENDIF
C
C     Set pathname for parameter table lookup
C
      CALL GETENV( 'ECMWF_LOCAL_TABLE_PATH', ELTPATH)
      IOFFSET = INDEX( ELTPATH, ' ')
      IF( IOFFSET.LE.1 ) THEN
      IOFFSET = INDEX(HDEFAULT,' ') - 1
       IF(IOFFSET.GT.0) THEN
           ELTPATH = HDEFAULT(1:IOFFSET)//'/gribtables/'
       ELSE 
c           ELTPATH = '/usr/local/lib/metaps/tables/gribex/'
c           ELTPATH = '/home/ma/emos/tables/gribex/'
           ELTPATH = '/usr/local/apps/libemos/tables/gribex/'
       ENDIF
      ENDIF
C
C     Set pathname for bitmask file
C
      CALL GETENV( 'LOCAL_BITMAP_PATH', ELBPATH)
      IOFFSET = INDEX( ELBPATH, ' ')
      IF(IOFFSET.LE. 1) THEN
        ELBPATH = HBTDEFT
      ENDIF
C
C     Display values if debug is ON
C
      IF( NDBG.GT.0 ) THEN
C
        WRITE(GRPRSM,*) 'GRSDEF: ***********************************'
        WRITE(GRPRSM,*) 'GRSDEF: Environment variable settings:'
C
        WRITE(GRPRSM,*) 'GRSDEF: GRIBEX_DEBUG is ON'
C
        IF( NVCK.GT.0 ) THEN
          WRITE(GRPRSM,*) 'GRSDEF: GRIBEX_CHECK is ON'
        ELSE
          WRITE(GRPRSM,*) 'GRSDEF: GRIBEX_CHECK is OFF'
        ENDIF
C
        WRITE(GRPRSM,*) 'GRSDEF: GRPRS_STREAM is ', GRPRSM
C
        WRITE(GRPRSM,9000) NEMOSLB
C
        IOFFSET = INDEX( ELTPATH, ' ')
        WRITE(GRPRSM,*)
     X    'GRSDEF: ECMWF_LOCAL_TABLE_PATH is ' // ELTPATH(1:IOFFSET)
C
        IOFFSET = INDEX( ELBPATH, ' ')
        WRITE(GRPRSM,*)
     X    'GRSDEF: LOCAL_BITMAP_PATH is ' // ELBPATH(1:IOFFSET)
C
        WRITE(GRPRSM,*) 'GRSDEF: ***********************************'
      ENDIF
C
C     ----------------------------------------------------------------
C*    Section 9 . Return to calling routine.
C     ----------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
 9000 FORMAT(' GRSDEF: EMOSLIB version number (EMOS_CYCLE) is ',(I6.6))
C
      END

