C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE KWLOC1(KSEC1,KGRIB,KLENG,KNSPT,KBITS,KPLEN,KPR,KRET)
C
C---->
C**** KWLOC1
C
C     Purpose.
C     --------
C
C     GRIB coding/decoding of Washington ensemble product local use of
C     Section 1.
C
C     Interface.
C     ----------
C
C     CALL KWLOC1 (KSEC1,KGRIB,KLENG,KNSPT,KBITS, KPLEN,KPR,KRET)
C
C
C     Input Parameters.
C     -----------------
C
C     KSEC1      - Array containing Grib Section 1 data.
C
C     KGRIB      - Array containing Grib coded data.
C
C     KLENG      - Length (words) of KGRIB.
C
C     KNSPT      - Bit number after which insertion or
C                  extraction starts.
C
C     KBITS      - Number of bits in computer word.
C
C     KPLEN      - Pointer to field containing length of Section 1.
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
C     KSEC1      - Array containing Grib Section 1 data.
C
C     KGRIB      - Array containing Grib coded data.
C
C     KNSPT      - Number of bit after last one inserted or
C                              extracted.
C
C     KRET       - Return code.
C                  0   , No error encountered.
C                  1   , Error reported by routine INXBIT.
C
C     Method.
C     -------
C
C     Input data packed/unpacked in accordance with Washington ensemble
C     product usage of local part of section 1 of Grib code.
C
C
C     Externals.
C     ----------
C
C     INXBIT - insert/extract bit data
C     ABORTX - abort program
C
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
C     Washington ensemble product usage of local part of section 1 of
C     GRIB:
C
C     Octet 41 : Identifies the application:
C                1   Ensemble.
C
C     Octet 42 : Type:
C                1   Unperturbed control forecast,
C                2   Individual negatively perturbed forecast,
C                3   Individual positively perturbed forecast,
C                4   Cluster,
C                5   Whole ensemble.
C
C     Octet 43 : Identification number:
C                1   If byte 42=1, identifies high resolution.
C                2   If byte 42=1, identifies low resolution.
C                3   Etc.(!?).
C                    (1 to 5 for perturbed 00Z forecast.)
C                    (1 to 2 for perturbed 12Z forecast.)
C
C     Octet 44 : Products:
C                1   Full field (individual forecast)/unweighted mean.
C                2   Weighted mean.
C                11  Standard deviation wrt ensemble mean.
C                12  Standard deviation wrt ensemble mean, normalized.
C
C     Octet 45 : Spatial smoothing of product (number of highest total
C                wavenumber included):
C                255 Original resolution retained.
C
Cjdc  Octet 46 : Dummy.    (Not being used!)
C
C
C     Author.
C     -------
C
C     J.D.Chambers     ECMWF      09.10.95
C
C
C     Modifications.
C     --------------
C
C     None.
C
C----<
C     ------------------------------------------------------------------
C     Section 0 . Definition of variables. Data statements.
C     ------------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Parameters
      INTEGER KSEC1, KGRIB, KLENG, KNSPT, KBITS, KPLEN, KPR, KRET
      DIMENSION KGRIB(*)
      DIMENSION KSEC1(*)
C
C     Local variables
      INTEGER IRET
      CHARACTER*1   YFUNC
C
C     ------------------------------------------------------------------
C     Section 1 . Set initial values.
C     ------------------------------------------------------------------
C
  100 CONTINUE
C
      IF (KPR.GE.1) THEN
        WRITE(GRPRSM,*) 'KWLOC1 : Section 1.'
        WRITE(GRPRSM,*) 'KWLOC1 : On input, KNSPT = ',KNSPT
      ENDIF
C
C     Reset return code to 0, retaining input value to decide
C     on abort / no abort, if error encountered later.
C
      IRET = KRET
      KRET = 0
      YFUNC = 'D'
C
C     ------------------------------------------------------------------
C     Section 2 . Washington ensemble product local use of Grib
C                 Section 1.
C     ------------------------------------------------------------------
C
  200 CONTINUE
C
      IF (KPR.GE.1) WRITE(GRPRSM,*) 'ECLOC1 : Section 2.'
C
C     Extract fields.
C
C
C     Octet 41 : Identifies the application:
C                1   Ensemble.
C     One 8 bit field.
C
      CALL INXBIT (KGRIB,KLENG,KNSPT,KSEC1(37),1,KBITS, 8,YFUNC,KRET)
      IF (KRET.NE.0) THEN
        WRITE(GRPRSM,*) 'KWLOC1: INXBIT error reported = ',KRET
        KRET = 1
        GO TO 900
      ENDIF
C
C     Octet 42 : Type:
C                1   Unperturbed control forecast,
C                2   Individual negatively perturbed forecast,
C                3   Individual positively perturbed forecast,
C                4   Cluster,
C                5   Whole ensemble.
C     One 8 bit field.
C
      CALL INXBIT (KGRIB,KLENG,KNSPT,KSEC1(38),1,KBITS, 8,YFUNC,KRET)
      IF (KRET.NE.0) THEN
        WRITE(GRPRSM,*) 'KWLOC1: INXBIT error reported = ',KRET
        KRET = 1
        GO TO 900
      ENDIF
C
C     Octet 43 : Identification number:
C                1   If byte 42=1, identifies high resolution.
C                2   If byte 42=1, identifies low resolution.
C                3   Etc.(!?)
C     One 8 bit field.
C
      CALL INXBIT (KGRIB,KLENG,KNSPT,KSEC1(39),1,KBITS, 8,YFUNC,KRET)
      IF (KRET.NE.0) THEN
        WRITE(GRPRSM,*) 'KWLOC1: INXBIT error reported = ',KRET
        KRET = 1
        GO TO 900
      ENDIF
C
C     Octet 44 : Products:
C                1   Full field (individual forecast)/unweighted mean.
C                2   Weighted mean.
C                11  Standard deviation wrt ensemble mean.
C                11  Standard deviation wrt ensemble mean, normalized.
C     One 8 bit field.
C
      CALL INXBIT (KGRIB,KLENG,KNSPT,KSEC1(40),1,KBITS, 8,YFUNC,KRET)
      IF (KRET.NE.0) THEN
        WRITE(GRPRSM,*) 'KWLOC1: INXBIT error reported = ',KRET
        KRET = 1
        GO TO 900
      ENDIF
C
C     Octet 45 : Spatial smoothing of product (number of highest total
C                wavenumber included):
C                255 Original resolution retained.
C     One 8 bit field.
C
      CALL INXBIT (KGRIB,KLENG,KNSPT,KSEC1(41),1,KBITS, 8,YFUNC,KRET)
      IF (KRET.NE.0) THEN
        WRITE(GRPRSM,*) 'KWLOC1: INXBIT error reported = ',KRET
        KRET = 1
        GO TO 900
      ENDIF
C
C     Octet 46 : Dummy.
C     One 8 bit field.
C
Cjdc  KNSPT = KNSPT + 8
C
C     ------------------------------------------------------------------
C     Section 9 . Abort/return to calling routine. Format statements.
C     ------------------------------------------------------------------
C
  900 CONTINUE
C
      IF (KPR.GE.1) THEN
        WRITE(GRPRSM,*) 'KWLOC1 : Section 9.'
        WRITE(GRPRSM,*) 'KWLOC1 : On output, KNSPT = ',KNSPT
      ENDIF
C
C     Abort if requested to do so when an error has been encountered.
C
      IF ( (IRET.EQ.0) .AND. (KRET.NE.0) ) THEN
        CALL ABORTX ('KWLOC1')
      ELSE
        RETURN
      ENDIF
C
      END
