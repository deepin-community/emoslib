C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE SH2SH(UFLDIN, KRESIN, UFLDOU, KRESOU)
      IMPLICIT NONE
C
C---->
C**** SH2SH
C
C     Purpose
C     _______
C
C     This routine produces a field of spectral coefficients in UFLDOU
C     of truncation KRESOU from a field of spectral coeeficients in
C     UFLDIN of truncation KRESIN.
C
C
C     Interface
C     _________
C
C     CALL SH2SH(UFLDIN, KRESIN, UFLDOU, KRESOU)
C
C
C     Input parameters
C     ________________
C
C     UFLDIN       - field of spectral coefficients
C     KRESIN       - truncation of input field
C     KRESOU       - truncation of output field
C
C
C     Output parameters
C     ________________
C
C     UFLDOU       - field of spectral coefficients
C
C
C     Common block usage
C     __________________
C
C     None
C
C
C     Method
C     ______
C
C     If the input truncation is greater than (or equal to) the output
C     truncation, coefficients are transferred reduced for the output.
C
C     If the input truncation is less than the output truncation,
C     all coefficients are transferred and padded with zeroes for the
C     output.
C
C
C     Externals
C     _________
C
C     None
C
C
C     Reference
C     _________
C
C     None
C
C
C     Comments
C     ________
C
C     Arrays for the input and output fields must be defined large
C     enough for the coefficients implied by the truncations.  Thus
C     the dimension for UFLDOU must be at least:
C         ( KRESOU + 1) * ( KRESOU + 2 ) /2
C
C
C     Author
C     ______
C
C     J.D.Chambers       ECMWF      8th Nov 1993
C
C
C     Modifications
C     _____________
C
C     None
C
C----<
C     _______________________________________________________
C
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
C     Real         A to F and Q to X, global or common.
C                  P (but not PP), dummy argument
C                  Z, local variable
C                  PP, parameter.
C     Complex      U, dummy argument
C
C     _______________________________________________________
C
C     Subroutine arguments
      COMPLEX UFLDIN(*),UFLDOU(*)
      INTEGER KRESIN, KRESOU
C
C     Local variables
      INTEGER ITINP1, ITOUP1, ILIM, IMLIM, IMN, IMP, IADD
      INTEGER JM, JN
C
C     _______________________________________________________
C
C
C*    Section 1. Initialization
C     _______________________________________________________
C
  100 CONTINUE
C
C     Initialize loop control variables
C
      ITINP1 = KRESIN + 1
      ITOUP1 = KRESOU + 1
      ILIM   = ITOUP1
      IMLIM  = ITOUP1
      IMN = 1
      IMP = 1
C
C     _______________________________________________________
C
C
C*    Section 2. Computation.
C     _______________________________________________________
C
  200 CONTINUE
C
C     Check if desired output truncation greater than input ...
C
      IADD = KRESIN - KRESOU
      IF ( IADD .GE. 0 ) THEN
C
C     ... input truncation not less than desired output ...
C     ... move truncated lines of coefficients
        DO 230 JM = 1, IMLIM
          DO 220 JN = JM, ILIM
            UFLDOU(IMP) = UFLDIN(IMN)
            IMP = IMP + 1
            IMN = IMN + 1
220       CONTINUE
C                                Skip coefficients being truncated
        IMN = IMN + IADD
230     CONTINUE
C
C     ... input truncation is less than desired output ...
C     ... pad each output line of coefficients with zeroes
      ELSE
        DO 250 JM = 1, IMLIM
          DO 240 JN = JM, ILIM
            IF ( JN .GT. ITINP1 .OR. JM .GT. ITINP1) THEN
C                                At end of input coefficients in
C                                current row, then set values to
C                                zero in output
              UFLDOU(IMP) = 0.0
            ELSE
              UFLDOU(IMP) = UFLDIN(IMN)
              IMN = IMN + 1
            ENDIF
            IMP = IMP + 1
240       CONTINUE
250     CONTINUE
      ENDIF
C
C     _______________________________________________________
C
C
C*    Section 9. Return to calling routine.
C     _______________________________________________________
C
  900 CONTINUE
      RETURN
      END
