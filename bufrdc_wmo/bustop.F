C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE BUSTOP(KSTOP)
C
C**** *BUSTOP*
C
C
C     PURPOSE.
C     --------
C            SETS VARIABLE KPMISS,KPRUS,KOKEY,KOFL INTO COMMON BLOCK.
C
C**   INTERFACE.
C     ----------
C
C          *CALL* *BUSTOP(KSTOP)*
C
C        INPUT :
C               *KSTOP*   -  INTEGER CONSTANT
C
C
C     METHOD.
C     -------
C
C
C     EXTERNALS.
C     ----------
C
C          NONE
C
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC    *ECMWF*       15/09/2007
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)
C
#     include "parameter.F"
#     include "bcomwt.F"
#     include "bcomdefc.F"
C
      CHARACTER*4 CECMWF      
C
C     ------------------------------------------------------------------
C
C*          1.   SET CONSTANTS.
C                --------------
 100  CONTINUE
C
      IF(CECMWF.NE.'ECMF') THEN
         IERR=0
         CALL BUEVAR(IERR)
         CECMWF='ECMF'
      END IF
C
      NSTOP=KSTOP
C
      RETURN
      END
