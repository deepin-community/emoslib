C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE INXBIT (KGRIB,KLENG,KNSPT,KPARM,KNUM,KBIT,
     C                   KBLEN,HFUNC,KRET)
C
C---->
C**** INXBIT - Insert/extract bits consecutively in/from a given array
C
C     Purpose.
C     --------
C
C     Take rightmost KBLEN bits from KNUM words of KPARM
C     and insert them consecutively in KGRIB, starting at
C     bit after KNSPT or vice versa.
C
C**   Interface.
C     ----------
C
C     CALL INXBIT (KGRIB,KLENG,KNSPT,KPARM,KNUM,KBIT, KBLEN,KRET)
C
C
C     Input Parameters.
C     -----------------
C
C     KGRIB      - Array containing bitstream.
C     KLENG      - Length (words) of this array.
C     KNSPT      - Bit number after which insertion/extraction starts.
C     KPARM      - Array from which bits are taken for
C                  insertion in the bitstream or to which
C                  bits are extracted from the bitstream.
C     KBIT       - Number of bits in computer word.
C     KNUM       - Number of bit fields inserted/extracted.
C     KBLEN      - Number of bits per bit field.
C     HFUNC      - Requested function.
C                  'C' to insert bits in bitstream,
C                  'D' to extract bits from bitstream.
C
C     Output Parameters.
C     ------------------
C
C     KNSPT      - Bit number of last bit inserted/extracted.
C
C     KRET       - Return code.
C                  0 , No error encountered.
C                  1 , Insertion/extraction exceeded array boundary.
C
C     Method.
C     -------
C
C     Word and offset pointer calculated before calling
C     insertion/extraction routines.
C
C     Externals.
C     ----------
C
C
C     Reference.
C     ----------
C
C     ECLIB documentation on SBYTES and GBYTES.
C
C     Comments.
C     ---------
C
#if (defined CRAY) || (defined __uxp__ ) || (defined IBM_POWER4)
C     Vectorized Fortran version of routine.
C     This routine contains a call to GSBITE, a vectorising
C     version of GBYTE(S) and SBYTE(S).
#else
C     Generic version of routine.
#endif
C     Author.
C     -------
C
C     J. Hennessy      ECMWF      18.06.91
C
C     Modifications.
C     --------------
C
C     J. Hennessy      ECMWF      08.11.91
C     Parameter KMACH removed from list of input parameters.
C
C     J. Hennessy      ECMWF      12.10.92
C     Dimension of IMASK changed from 64 to 65.
C
C     J.D.Chambers     ECMWF      21.05.96
C     Defines put in for Fujtsu.
C
C----<
C     ----------------------------------------------------------------
C*    Section 0 . Definition of variables. Data statements.
C     ----------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
      CHARACTER*1 HFUNC
      INTEGER IND, IOFF, IWORD
      INTEGER KBIT, KBLEN, KGRIB, KLENG, KNSPT, KNUM, KPARM, KRET
#ifdef DEBUG
      INTEGER INUM, J901
#endif
C
      DIMENSION KGRIB(KLENG)
      DIMENSION KPARM(*)
#if (defined CRAY) || (defined __uxp__ ) || (defined IBM_POWER4)
      INTEGER IMASK
      DIMENSION IMASK(65)
C
C     Values in IMASK are set in the first call to routine GSBITE, and
C     are used in subsequent calls.
C
      SAVE IMASK
C
C     Force routine GSBITE to calculate bit-masks first time through.
C
      DATA IMASK(2) /0/
#endif
C
C     ----------------------------------------------------------------
C*    Section 1 . Set initial values.
C     ----------------------------------------------------------------
C
  100 CONTINUE
C
#ifdef DEBUG
      WRITE(GRPRSM,*) 'INXBIT: Input values used -'
      WRITE(GRPRSM,*) 'KLENG = ', KLENG
      WRITE(GRPRSM,*) 'KNSPT = ', KNSPT
      WRITE(GRPRSM,*) 'KBIT = ', KBIT
      WRITE(GRPRSM,*) 'HFUNC = ', HFUNC
#endif
C
      KRET = 0
C
C     ----------------------------------------------------------------
C*    Section 2 . Bit insertion/extraction.
C     ----------------------------------------------------------------
C
  200 CONTINUE
C
C*    Calculate word pointer and offset.
C
      IWORD = KNSPT / KBIT
      IOFF  = KNSPT - IWORD * KBIT
      IWORD = IWORD + 1
#ifdef DEBUG
      WRITE(GRPRSM,*) 'INXBIT: Word is ',IWORD,', bit offset is ',IOFF
#endif
C
C     Insert/extract bits.
C
#if (defined CRAY) || (defined __uxp__ ) || (defined IBM_POWER4)
      IF (KNUM.GE.8) THEN
C
C       Vectorising routine GSBITE performs the same
C       functions as SBYTE(S) and GBYTE(S).
C
        CALL GSBITE (KGRIB(IWORD),KPARM,IOFF,KBLEN,0,KNUM,
     X                    KBIT,IMASK,HFUNC)
      ELSE
C
C       Scalar faster.
C
        IF (HFUNC.EQ.'C') THEN
          CALL SBYTES (KGRIB(IWORD),KPARM,IOFF,KBLEN,0,KNUM)
        ELSE
          CALL GBYTES (KGRIB(IWORD),KPARM,IOFF,KBLEN,0,KNUM)
        ENDIF
      ENDIF
#elif (defined CYBER)
      IF (HFUNC.EQ.'C') THEN
        CALL SBYTES6 (KGRIB(IWORD),KPARM,IOFF,KBLEN,0,KNUM)
      ELSE
        CALL GBYTES6 (KGRIB(IWORD),KPARM,IOFF,KBLEN,0,KNUM)
      ENDIF
#elif (defined IBM) || (defined VAX)
      IF (HFUNC.EQ.'C') THEN
        CALL SBYTES (KGRIB(IWORD),KPARM,IOFF,KBLEN,0,KNUM)
      ELSE
        CALL GBYTES (KGRIB(IWORD),KPARM,IOFF,KBLEN,0,KNUM)
      ENDIF
#else
      IF (HFUNC.EQ.'C') THEN
        CALL SBYTES (KGRIB(IWORD),KPARM,IOFF,KBLEN,0,KNUM)
      ELSE
        CALL GBYTES (KGRIB(IWORD),KPARM,IOFF,KBLEN,0,KNUM)
      ENDIF
#endif
C
C     Update pointer.
C
      KNSPT = KNSPT + KBLEN * KNUM
C
C     ----------------------------------------------------------------
C*    Section 3 . Check out of range.
C    -----------------------------------------------------------------
C
  300 CONTINUE
C
      IND = KNSPT / KBIT
      IF (IND.GT.KLENG) THEN
        KRET = 1
        WRITE(GRPRSM,*) 'INXBIT : Word ', IND ,
     X              ' is outside array bounds ', KLENG
      ENDIF
C
C     ----------------------------------------------------------------
C*    Section 9 . Return to calling routine.
C     ----------------------------------------------------------------
C
  900 CONTINUE
C
#ifdef DEBUG
      INUM = KNUM
      IF( INUM.GT.360 ) THEN
        INUM = 360
        WRITE(GRPRSM,*) 'First ', INUM,' values.'
      ENDIF
      DO 901 J901=1,INUM
        IF( HFUNC.EQ.'C' ) THEN
          WRITE(GRPRSM,*) 'Inserted value = ', KPARM(J901)
        ELSE
          WRITE(GRPRSM,'(1H ,Z)') KGRIB(IWORD)
          WRITE(GRPRSM,*) 'Extracted value = ', KPARM(J901)
          WRITE(GRPRSM,'(1H ,Z)') KPARM(J901)
        ENDIF
  901 CONTINUE
      WRITE(GRPRSM,*) '         Output values set -'
      WRITE(GRPRSM,*) 'KNSPT = ', KNSPT
#endif
C
      RETURN
      END
