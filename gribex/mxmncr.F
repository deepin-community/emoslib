C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE MXMN(PARR,KLEN,PMAX,PMIN)
C
C---->
C**   *SUBROUTINE* *MXMN* - *FIND THE MAXIMUM AND MINIMUM ELEMENTS OF
C                       AN ARRAY
C
C          *JAN *HASELER     *ECMWF*     6/10/82
C
C     PURPOSE
C     -------
C
C          *FIND THE MAXIMUM AND MINIMUM ELEMENTS OF THE ARRAY *PARR*
C
C     INTERFACE
C     ---------
C
C           *CALL* *MXMN(PARR,KLEN,PMAX,PMIN)*
C
C     WHERE *PARR(KLEN)* = INPUT ARRAY
C           *PMAX*       = OUTPUT MAXIMUM ELEMENT OF *PARR*
C           *PMIN*       = OUTPUT MINIMUM ELEMENT OF *PARR*
C
C     *MXMN* IS CALLED FROM SUBROUTINES *OUTSHC,* *OUTSHG,* *OUT2D* AND
C     *OUTDIA*
C
C     RESULTS
C     -------
C
C          *THE VALUES *PMAX* AND *PMIN* ARE RETURNED
C
C     EXTERNALS
C     ---------
C
C          *ISMAX* - *CAL* FUNCTION TO FIND MAX ARRAY ELEMENT
C          *ISMIN* - *CAL* FUNCTION TO FIND MIN ARRAY ELEMENT
C
C     ------------------------------------------------------------------
C----<
C
      DIMENSION PARR(KLEN)
C
C-----------------------------------------------------------------------
C*
C          1.           FIND MAX AND MIN
C                       ----------------
C
  100 CONTINUE
C
C*            1.1       SEARCH THROUGH 4096 ELEMENTS AT A TIME
  110 CONTINUE
      ZMIN=PARR(1)
      ZMAX=PARR(1)
      ID=1
      I1=KLEN/4096
      I2=KLEN-I1*4096
C
      DO 115 J=1,I1
      IX=ISMAX(4096,PARR(ID),1)+ID-1
      IN=ISMIN(4096,PARR(ID),1)+ID-1
      ZMAX=AMAX1(ZMAX,PARR(IX))
      ZMIN=AMIN1(ZMIN,PARR(IN))
      ID=ID+4096
  115 CONTINUE
C
C
C*            1.2       REMAINING ELEMENTS
  120 CONTINUE
      IF (I2.GT.0) THEN
           IX=ISMAX(I2,PARR(ID),1)+ID-1
           IN=ISMIN(I2,PARR(ID),1)+ID-1
           ZMAX=AMAX1(ZMAX,PARR(IX))
           ZMIN=AMIN1(ZMIN,PARR(IN))
      ENDIF
C
      PMIN=ZMIN
      PMAX=ZMAX
C
      RETURN
      END

