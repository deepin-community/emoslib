C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GRPRS4 (KSEC0,KSEC4,PSEC4)
C
C---->
C**** GRPRS4 - Print information from Section 4 of GRIB code.
C
C     Purpose.
C     --------
C
C     Print the information in the Binary data section
C     Section (Section 4) of decoded GRIB data.
C
C**   Interface.
C     ----------
C
C     CALL GRPRS4 (KSEC0,KSEC4,PSEC4)
C
C
C     Input Parameters.
C     -----------------
C     KSEC0 - Array of decoded integers from Section 0.
C     KSEC4 - Array of decoded integers from Section 4.
C     PSEC4 - Array of decoded reals from Section 4.
C
C     Output Parameters.
C     ------------------
C
C     None.
C
C     Method.
C     -------
C
C     Fields printed as integers or reals.
C
C     Externals.
C     ----------
C
C     SETPAR
C     INXBIT.
C
C     Common block GRPRSCM.
C
C     Reference.
C     ----------
C
C     WMO Manual on Codes re GRIB Code.
C     See also routine GRIBEX.
C
C     Comments.
C     ---------
C
C     Checks environment variable GRPRS_STREAM for output stream number
C     for printing (default is 6)
C
C     Author.
C     -------
C
C     J. Hennessy    ECMWF 11.09.91
C
C     Modifications.
C     --------------
C
C     J. Hennessy    ECMWF 21.07.92
C
C     J.D.Chambers   ECMWF 20.10.93
C     Distinguish between real and integer values
C     in print of gridpoint values
C
C     J.D.Chambers   ECMWF 09.05.94
C     Print complex packing information
C
C     J.D.Chambers   ECMWF 14.06.95
C     Change format for printing missing data indicator to
C     allow for MAGICS value (-1.5E+21)
C
C     J. Clochard, Meteo France, for ECMWF - January 1998.
C     Take into account second-order packing for grid-point data.
C
C     J.D.Chambers   ECMWF    June 2002
C     Add option to set output stream number using environment variable
C     GRPRS_STREAM
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
      INTEGER KSEC0(*)
      INTEGER KSEC4(*)
      REAL PSEC4(*)
C
      INTEGER INUM, J210, J212, IVALUE, IBIT, IDUM, INSPT, IRET
C
C     -----------------------------------------------------------------|
C*    Section 1 . Print integer information from KSEC4.
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      WRITE(GRPRSM,9000)
      WRITE(GRPRSM,9001)
      WRITE(GRPRSM,9002)
C
      WRITE(GRPRSM,9003) KSEC4(1)
      WRITE(GRPRSM,9004) KSEC4(2)
      WRITE(GRPRSM,9005) KSEC4(3)
      WRITE(GRPRSM,9006) KSEC4(4)
      WRITE(GRPRSM,9007) KSEC4(5)
      WRITE(GRPRSM,9008) KSEC4(6)
      WRITE(GRPRSM,9009) KSEC4(7)
      WRITE(GRPRSM,9010) KSEC4(8)
      WRITE(GRPRSM,9011) KSEC4(9)
      WRITE(GRPRSM,9012) KSEC4(10)
C     If complex packing ..
      IF ( KSEC4(4).EQ.64 ) THEN
C
        IF ( KSEC4(3).EQ.128 ) THEN
          WRITE(GRPRSM,9116) KSEC4(16)
          WRITE(GRPRSM,9117) KSEC4(17)
          WRITE(GRPRSM,9118) KSEC4(18)
          WRITE(GRPRSM,9119) KSEC4(19)
          WRITE(GRPRSM,9120) KSEC4(20)
        ELSE
          WRITE(GRPRSM,9013) KSEC4(11)
          WRITE(GRPRSM,9014) KSEC4(12)
          WRITE(GRPRSM,9015) KSEC4(13)
          WRITE(GRPRSM,9016) KSEC4(14)+KSEC4(15)
        ENDIF
C
      ENDIF
C
C     Number of non-missing values
C
      IF ( KSEC4(21).NE.0 )  WRITE(GRPRSM,9017) KSEC4(21)
C
C     Information on matrix of values , if present.
C
      IF (KSEC4(8).EQ.64)
     C   THEN
             WRITE(GRPRSM,9020) KSEC4(50)
             WRITE(GRPRSM,9021) KSEC4(51)
             WRITE(GRPRSM,9022) KSEC4(52)
             WRITE(GRPRSM,9023)
             WRITE(GRPRSM,9024) KSEC4(53)
             WRITE(GRPRSM,9025) KSEC4(54)
             WRITE(GRPRSM,9023)
             WRITE(GRPRSM,9026) KSEC4(55)
             WRITE(GRPRSM,9027) KSEC4(56)
             WRITE(GRPRSM,9028) KSEC4(57)
         ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2. Print values from PSEC4.
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      WRITE(GRPRSM,9000)
C
      INUM = KSEC4(1)
      IF (INUM.LT.0)  INUM = - INUM
      IF (INUM.GT.20) INUM = 20
C
C     Print first INUM values.
C
      WRITE(GRPRSM,9031) INUM
C
      IF ( KSEC4(5) .EQ. 0 ) THEN
C
C     Print real values ...
        DO 210 J210=1,INUM
          WRITE(GRPRSM,9034) PSEC4(J210)
  210   CONTINUE
C
      ELSE
C
C     Print integer values ...
        CALL SETPAR(IBIT,IDUM,IDUM)
        DO 212 J212=1,INUM
           INSPT = 0
           CALL INXBIT(IVALUE,1,INSPT,PSEC4(J212),1,IBIT,IBIT,'C',IRET)
           WRITE(GRPRSM,9033) IVALUE
  212   CONTINUE
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 9 . Format statements. Return to calling routine.
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
 9000 FORMAT(' ')
 9001 FORMAT(' Section 4 - Binary Data  Section.')
 9002 FORMAT(' -------------------------------------')
 9003 FORMAT(' Number of data values coded/decoded.         ',I9)
 9004 FORMAT(' Number of bits per data value.               ',I9)
 9005 FORMAT(' Type of data       (0=grid pt, 128=spectral).',I9)
 9006 FORMAT(' Type of packing    (0=simple, 64=complex).   ',I9)
 9007 FORMAT(' Type of data       (0=float, 32=integer).    ',I9)
 9008 FORMAT(' Additional flags   (0=none, 16=present).     ',I9)
 9009 FORMAT(' Reserved.                                    ',I9)
 9010 FORMAT(' Number of values   (0=single, 64=matrix).    ',I9)
 9011 FORMAT(' Secondary bit-maps (0=none, 32=present).     ',I9)
 9012 FORMAT(' Values width       (0=constant, 16=variable).',I9)
 9013 FORMAT(' Bits number of 2nd order values    (none=>0).',I9)
 9014 FORMAT(' General extend. 2-order packing (0=no,8=yes).',I9)
 9015 FORMAT(' Boustrophedonic ordering        (0=no,4=yes).',I9)
 9016 FORMAT(' Spatial differencing order          (0=none).',I9)
 9017 FORMAT(' Number of non-missing values                 ',I9)
 9120 FORMAT(' Pentagonal resolution parameter M for subset.',I9)
 9020 FORMAT(' First dimension (rows) of each matrix.       ',I9)
 9021 FORMAT(' Second dimension (columns) of each matrix.   ',I9)
 9022 FORMAT(' First dimension coordinate values definition.',I9)
 9023 FORMAT(' (Code Table 12)')
 9024 FORMAT(' NC1 - Number of coefficients for 1st dimension.',I7)
 9025 FORMAT(' Second dimension coordinate values definition.',I8)
 9026 FORMAT(' NC2 - Number of coefficients for 2nd dimension.',I7)
 9027 FORMAT(' 1st dimension physical signifance (Table 13). ',I8)
 9028 FORMAT(' 2nd dimension physical signifance (Table 13). ',I8)
 9031 FORMAT(' First ',I4,' data values.')
 9032 FORMAT(' ',F30.15)
C
C931020
 9033 FORMAT(' ',I15)
 9034 FORMAT(' ',G20.8)
C
C940509
 9116 FORMAT(' Byte offset of start of packed data (N).     ',I9)
 9117 FORMAT(' Power (P * 1000).                            ',I9)
 9118 FORMAT(' Pentagonal resolution parameter J for subset.',I9)
 9119 FORMAT(' Pentagonal resolution parameter K for subset.',I9)
      RETURN
C
      END
