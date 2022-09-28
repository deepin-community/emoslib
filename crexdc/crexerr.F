C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CREXERR(KERR)
C**** *CREXERR*
C
C
C     PURPOSE.
C     --------
C         PRINT ERROR CODE.
C
C
C**   INTERFACE.
C     ----------
C
C         *CALL* *CREXERR(KERR)*
C
C         INPUT:  *NONE*
C
C         OUTPUT: *KERR* - Return error code
C
C     METHOD.
C     -------
C
C          NONE.
C
C
C     EXTERNALS.
C     ----------
C
C          NONE.
C
C     REFERENCE.
C     ----------
C
C          NONE.
C
C     AUTHOR.
C     -------
C
C          MILAN DRAGOSAVAC    *ECMWF*       07/01/2004.
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
C
      CHARACTER*80 CERROR(100)
C
      DATA CERROR/100*' '/
C
C     ------------------------------------------------------------------
C
C*          1.   INITIALIZE MESSAGES.
C                --------------------
 100  CONTINUE
C
      CERROR( 1)=' Start of CREX message not found.'
      CERROR( 2)=' End of CREX message not found.'
      CERROR( 3)=' Array to receive CREX message too small.'
      CERROR( 6)=' Error during read CREX table B.'
      CERROR( 7)=' Error during read CREX table C.'
      CERROR( 8)=' Error during read CREX table D.'
      CERROR( 9)=' Open error.'
      CERROR(10)=' Error during close CREX table B.'
      CERROR(11)=' Error during close CREX table C.'
      CERROR(12)=' Error during close CREX table D.'
      CERROR(13)=' Too many elements in the table'
      CERROR(14)=' Argument KVALS too small.'
      CERROR(19)=' Delayed replication factor too big.'
      CERROR(20)=' Table D reference not found.'
      CERROR(21)=' Data descriptor operator not found.'
      CERROR(22)=' CREX Opeartor name not found.'
      CERROR(23)=' Table B reference not found.'
      CERROR(25)=' KELEM argument too small.'
      CERROR(26)=' Word pointer out of range.'
      CERROR(30)=' Number of elements greater than JELEM.'
      CERROR(31)=' Too few elements in KDATA array.'
      CERROR(32)=' Number of subsets equal to zero.'
      CERROR(33)=' Internal write error'
      CERROR(34)=' Decimal to octal conversion error'
      CERROR(35)=' Internal read error'
      CERROR(36)=' Bad order of data descriptors.'
      CERROR(37)=' Wrong data descriptor.'
      CERROR(38)=' Open error on units.cinfig.dat file'
      CERROR(39)=' Read error on units.cinfig.dat file'
      CERROR(40)=' Could not find next section'
      CERROR(41)=' '
      CERROR(42)=' Wrong start of section 1 of CREX message '
      CERROR(43)=' Could not find the end of section 1'
      CERROR(44)=' Error during extraction of Master table'
      CERROR(45)=' Error during extraction of Edition number'
      CERROR(46)=' Error during extraction of version number'
      CERROR(47)=' Unit not found'
C
      IF(KERR.GE.0.AND.KERR.LE.100) THEN
        WRITE(*,'(1h ,A)') CERROR(KERR)
      ELSE
        WRITE(*,'(1h ,A)') ' Error number out of range !'
      END IF
C
      RETURN
      END
