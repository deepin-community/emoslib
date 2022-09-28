C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE ECDEF2( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS,
     X                   KPR, KRET)
C
C---->
C**** ECDEF2
C
C     Purpose.
C     --------
C
C     GRIB coding/decoding of ECMWF local use definition 2.
C     (Extensions for clusters).
C
C
C**   Interface.
C     ----------
C
C     CALL ECDEF2( HFUNC, KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPR, KRET)
C
C
C     Input Parameters.
C     -----------------
C
C     HFUNC      - Requested function.
C                  'C' to code data.
C                  'D' to decode data.
C
C     KSEC1      - Array containing Grib Section 1 data.
C
C     KGRIB      - Array containing Grib coded data.
C
C     KLENG      - Length (words) of KGRIB.
C
C     KNSPT      - Bit number after which insertion/extraction starts.
C
C     KBITS      - Number of bits in computer word.
C
C
C     KPR        - Debug print switch.
C                  0  , No printout.
C                  >0 , Debug printout.
C
C     KRET       - Response to error indicator.
C                  0        , Abort if error encountered.
C                  Non-zero , Return to calling routine
C                             even if error encountered.
C
C     Output Parameters.
C     ------------------
C
C     KSEC1      - Array containing Grib Section 1 data.(Updated)
C
C     KGRIB      - Array containing Grib coded data.(Updated)
C
C     KNSPT      - Number of bit after last one inserted/extracted.(Updated)
C
C     KRET       - Return code.
C                  0   , No error encountered.
C                  2   , Error reported by routine INXBIT.
C
C     Method.
C     -------
C
C     Input data packed/unpacked in accordance with ECMWF usage of
C     local part of section 1 of Grib code, definition 2.
C
C
C     Externals.
C     ----------
C
C     INXBIT
C     ABORTX
C     CSGNBT
C     DSGNBT
C
C     Reference.
C     ----------
C
C     WMO Manual On Codes for Grib Code.
C
C
C     Comments.
C     ---------
C
C     Adapted from ECLOC1.F - simplified to handle just definition 2.
#include "ecdef2.h"
C
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      18th January 1995
C
C
C     Modifications.
C     --------------
C
C     None.
C
C
C----<
C     ------------------------------------------------------------------
C*    Section 0 . Definition of variables. Data statements.
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Parameters
      CHARACTER*1   HFUNC
      INTEGER KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPR, KRET
      DIMENSION KGRIB(*)
      DIMENSION KSEC1(*)
C
C     Local variables
      INTEGER IRET, IZERO, INSPT
C
      INTEGER ILALO
      DIMENSION ILALO(8)
C
      LOGICAL LENCODE, LDECODE
C
C     ------------------------------------------------------------------
C*    Section 1 . Set initial values.
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
      IRET = KRET
      LENCODE = ( HFUNC .EQ. 'C' )
      LDECODE = .NOT. LENCODE
C
C     ------------------------------------------------------------------
C*    Section 2 . Extensions for clusters (definition 2).
C     ------------------------------------------------------------------
C
  200 CONTINUE
C
      IF( KPR.GE.1 ) WRITE(GRPRSM,*) 'ECDEF2 : Section 2.'
C
C*    Octet 50 : Cluster number.
C     Octet 51 : Total number of clusters.
C     Two 8 bit fields.
C
C     Insert/extract fields.
C
      CALL INXBIT (KGRIB,KLENG,KNSPT,KSEC1(42),2,KBITS, 8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
        GO TO 900
      ENDIF
C
C*    Octet 52 : Reserved
C     Set to 0.
C
      IF ( LENCODE ) THEN
        IZERO = 0
        CALL INXBIT(KGRIB,KLENG,KNSPT, IZERO,1,KBITS,8,HFUNC,KRET)
        IF (KRET.NE.0) THEN
          KRET = 2
          WRITE(GRPRSM,9004)
        ENDIF
      ELSE
        KNSPT = KNSPT + 8
      ENDIF
C
C*    Octet 53 : Clustering method.
C     One 8 bit field.
C
C     Insert/extract field.
C
      CALL INXBIT (KGRIB,KLENG,KNSPT,KSEC1(44),1,KBITS, 8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
        GO TO 900
      ENDIF
C
C*    Octets 54-55 : Start timestep when clustering.
C     Octets 56-57 : End timestep when clustering.
C     Two 16 bit fields.
C
C     Insert/extract fields.
C
      CALL INXBIT (KGRIB,KLENG,KNSPT,KSEC1(45),2,KBITS, 16,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
        GO TO 900
      ENDIF
C
C*    Octets 58-60 : Northern latitude of domain of clustering
C     Octets 61-63 : Western longitude of domain of clustering
C     Octets 64-66 : Southern latitude of domain of clustering
C     Octets 67-69 : Eastern longitude of domain of clustering
C     Four 24 bit fields.
C
C     When coding values, set sign bit to 1, if value is negative.
C
      IF ( LENCODE ) THEN
        CALL CSGNBT( ILALO(1), KSEC1(47), 24, KRET)
        CALL CSGNBT( ILALO(2), KSEC1(48), 24, KRET)
        CALL CSGNBT( ILALO(3), KSEC1(49), 24, KRET)
        CALL CSGNBT( ILALO(4), KSEC1(50), 24, KRET)
      ENDIF
C
C     Insert/extract fields.
C
      CALL INXBIT (KGRIB,KLENG,KNSPT,ILALO(1),4,KBITS, 24,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
        GO TO 900
      ENDIF
C
C     When decoding values, if sign bit is 1, value is
C     negative.
C
      IF ( LDECODE ) THEN
        CALL DSGNBT( KSEC1(47), ILALO(1), 24, KRET)
        CALL DSGNBT( KSEC1(48), ILALO(2), 24, KRET)
        CALL DSGNBT( KSEC1(49), ILALO(3), 24, KRET)
        CALL DSGNBT( KSEC1(50), ILALO(4), 24, KRET)
      ENDIF
C
C*    Octet 70 : Number of cluster to which operational forecast
C                  belongs.
C     Octet 71 : Number of cluster to which control forecast
C                  belongs.
C     Octet 72 : Number of forecasts belonging to the cluster.
C     Three 8 bit fields.
C
C     Insert/extract fields.
C
      CALL INXBIT (KGRIB,KLENG,KNSPT,KSEC1(51),3,KBITS, 8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
        GO TO 900
      ENDIF
C
C*    The number of forecasts belonging to a cluster may vary,
C     but section 1 is kept a fixed length.
C
      INSPT = KNSPT + 2048
C
C*    Octets 73-328 : List of N ensemble forecast numbers.
C     KSEC1(53) 8 bit fields.
C
C     Insert/extract fields.
C
      CALL INXBIT (KGRIB,KLENG,KNSPT,KSEC1(54),KSEC1(53),KBITS, 
     X             8,HFUNC,KRET)
      IF (KRET.NE.0) THEN
        KRET = 2
        WRITE(GRPRSM,9004)
        GO TO 900
      ENDIF
C
      KNSPT = INSPT
C
      GOTO 900
C
C     ------------------------------------------------------------------
C*    Section 9 . Abort/return to calling routine. Format statements.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
      IF( KPR.GE.1 ) THEN
        WRITE(GRPRSM,*) 'ECDEF2 : Section 9.'
        WRITE(GRPRSM,*) '         Output values set -'
        WRITE(GRPRSM,9003) KNSPT
      ENDIF
C
C     Abort if requested to do so when an error has been encountered.
C
      IF (IRET.EQ.0.AND.KRET.NE.0) THEN
        CALL ABORTX ('ECDEF2')
      ELSE
        RETURN
      ENDIF
C
 9003 FORMAT (1H ,'         KNSPT  = ',I12)
C
 9004 FORMAT (' ECDEF2: Error reported by routine INXBIT.')
C
      END
