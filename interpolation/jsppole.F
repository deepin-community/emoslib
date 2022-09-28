C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JSPPOLE(PSHUP,KNUMB,KTRUNC,OMARS,PXF)
      IMPLICIT NONE
C
C---->
C**** *JSPPOLE* - Calculates fourier coefficient for U or V at pole
C
C     Purpose
C     -------
C
C     Calculates fourier coefficient for first harmonic only
C     for U and V wind component at the pole.
C
C     Interface
C     ---------
C
C     CALL JSPPOLE(PSHUP,KNUMB,KTRUNC,OMARS,PXF)
C
C     Input parameters
C     ----------------
C
C     PSHUP    - Unpacked harmonics field, unpacked
C     KNUMB    - 1 for North Pole, otherwise South Pole
C     KTRUNC   - Number (value) of the trucation 
C     OMARS    - .TRUE. if data is from MARS 
C     PXF      - Fourier coefficients (zero on input)
C
C
C     Output parameters
C     -----------------
C
C     PXF(2)   - Single fourier coefficient calculated
C
C
C     Common block usage
C     -----------------
C
C     None.
C
C
C     Externals
C     ---------
C
C     None.
C
C
C     Author
C     ------
C
C     J.D.Chambers     *ECMWF*      Oct 1993
C
C
C     Modifications
C     -------------
C
C     None.
C
C
C     Comments
C     --------
C
C     Created from SPPOLE. 
C     Changed to provide all parameters in the call, i.e. no common
C     blocks are used.
C
C
C     Method
C     ------
C
C     None.
C
C
C     Reference
C     _________
C
C     None.
C
C----<
C     _______________________________________________________
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
C*    Prefix conventions for variable names
C
C     Logical      L (but not LP), global or common.
C                  O, dummy argument
C                  G, local variable
C                  LP, parameter.
C     Character    C, global or common.
C                  H, dummy argument
C                  Y (but not YP), local variable
C                  YP, parameter.
C     Integer      M and N, global or common.
C                  K, dummy argument
C                  I, local variable
C                  J (but not JP), loop control
C                  JP, parameter.
C     REAL         A to F and Q to X, global or common.
C                  P (but not PP), dummy argument
C                  Z, local variable
C                  PP, parameter.
C
C     Dummy arguments
C
      COMPLEX   PSHUP
      INTEGER   KNUMB
      INTEGER   KTRUNC
      LOGICAL   OMARS
      COMPLEX   PXF
      DIMENSION PSHUP(*)
      DIMENSION PXF(*)
C
C     Local variables
C
      INTEGER   I1, ITIN1, ITOUT1, JN
      REAL      Z1, Z2, ZNORM, ZP1, ZP2, ZPOL
C 
C     -----------------------------------------------------------
C
C*    1.    Set initial values
C           ------------------
C
 100  CONTINUE
C
      ITIN1  = KTRUNC + 1
      ITOUT1 = KTRUNC
C
      ZPOL = 1.
      IF (KNUMB .NE. 1) ZPOL = -1.0
C
      ZP1  = -1.0
      ZP2  = -3.0 * ZPOL
      I1   = ITIN1 + 1
C
C*    2.    Change normalisation (if flagged as necessary)
C           --------------------
C
 200  CONTINUE
C
      IF (OMARS) THEN
         ZNORM = -SQRT(2.0)
      ELSE
         ZNORM = 1
      ENDIF
C
C
C*    3.    Calculation
C           -----------
C
 300  CONTINUE
      PXF(2) = (0.0,0.0)
C
C     Calculate the fourier coefficient for the first harmonic only.
      DO 310 JN = 1,ITOUT1,2
C
        Z1 = SQRT( (2.0*JN + 1.0)/(2.0*JN*(JN + 1.0)) )
        Z2 = SQRT( (2.0*(JN + 1.0) +1.0)/(2.0*(JN +1.0)*(JN +2.0)) )
C
        IF (JN .EQ. ITOUT1) Z2 = 0.0
C
        PXF(2) = PXF(2) +(Z1*ZP1*PSHUP(I1) +Z2*ZP2*PSHUP(I1+1))*ZNORM
        ZP1   = ZP1 - 2.0*(JN + 1.0) - 1.0
        ZP2   = ZP2 - (2.0*(JN + 2.0) + 1.0)*ZPOL
        I1    = I1 + 2
C
 310  CONTINUE
C
C     -------------------------------------------------------------
C
      RETURN
C
      END
