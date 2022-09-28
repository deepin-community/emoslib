C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

       SUBROUTINE BUSBYTESR8(DESTINATION,KSOURCE,KSKIP,KWIDTH,
     1                       KSKIPBETWEEN,KNUMBER)
C
C**** *BUSBYTESR8*
C
C
C     PURPOSE.
C     --------
C          
C
C**   INTERFACE.
C     ----------
C          NONE.
C
C
C
C
C     *METHOD.
C      -------
C          NONE.
C
C
C
C     EXTERNALS.
C     ----------
C          NONE.
C
C
C
C
C     REFERENCE.
C     ----------
C
C
C     AUTHOR.
C     -------
C
C          M. DRAGOSAVAC       *ECMWF*       JULY 2008
C
C
C     MODIFICATIONS.
C     --------------
C
C          NONE.
C
C
      IMPLICIT LOGICAL(L,O,G), CHARACTER*8(C,H,Y)

      REAL*8 DESTINATION

      CALL SBYTES(DESTINATION,KSOURCE,KSKIP,KWIDTH,KSKIPBETWEEN,KNUMBER)
      RETURN
      END
