C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JVOD2UV( VOR, DIV, KTIN, U, V, KTOUT)
C
C---->
C**** JVOD2UV
C
C     Purpose
C     -------
C
C     Calculate spherical harmonic coefficients of U*cos(lat) and
C     V*cos(lat) at triangular truncation KTOUT, given the coefficients
C     of divergence and vorticity at truncation KTIN.
C 
C
C     Interface
C     ---------
C
C     CALL JVOD2UV( VOR, DIV, KTIN, U, V, KTOUT)
C
C     Input
C     -----
C
C     VOR(KDIM1) = array of vorticity coefficients
C     DIV(KDIM1) = array of divergence coefficients
C     KTIN       = triangular truncation
C                  where KDIM1 = (KTIN+1)*(KTIN+2)
C
C
C     Output
C     ------
C
C     U(KDIM2)   = output array of U*cos(lat) coefficients
C     V(KDIM2)   = output array of V*cos(lat) coefficients
C     KTOUT      = output triangular truncation (KTOUT.le.KTIN)
C                  where KDIM2 = (KTOUT+1)*(KTOUT+4)
C
C
C     Method
C     ------
C
C     ??
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
C     Jan Haseler       ECMWF      5th January 1981
C
C
C     Modifications
C     -------------
C
C     D. R. Roskilly    ECMWF      5th May 1987
C     Avoid calculating the extra values for U and V components so
C     that vorticity and divergence can be re-calculated if required.
C
C     ??                ??         15th February 1988
C     Subroutine renamed VOD2UV from DVTOUV.
C
C     K.Rider           ECMWF      21st August 1991
C     Version 2 inconsistently truncated U and V to T(ITOUT-1).
C     Changed so that vorticity and divergence are consistently truncated
C     to T105 (sic).
C     U and V are calculated with truncation T(ITOUT),with zero values at
C     the corner.
C     Vorticity and divergence can be recalculated if required, but only
C     truncated to T(ITOUT) since the higher coefficients have been lost.
C
C     J.D.Chambers       ECMWF      26th January 1995
C       Cleaned up code (?!)
C
C----<
C
      IMPLICIT NONE
C
C     Subroutine arguments
      COMPLEX VOR(*), DIV(*), U(*), V(*)
      INTEGER KTIN, KTOUT
C
C     Local variables
      INTEGER K, IMN, ITOUT, JM, JN, JMP, LOOP, SIZE
      REAL REARTH, ZM, ZN
      COMPLEX ZI
C
C     Statement functions
      REAL DD, SS
      REAL PM, PN
      DD(PM,PN) = -SQRT((PN*PN-PM*PM)/(4.*PN*PN-1.))/PN
      SS(PM,PN) = -PM/(PN*(PN+1.))
C
C ------------------------------------------------------------------
C*    Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
C
      ZI     = (0.0, 1.0)
      REARTH = 6.371E6
      K      = 1
      IMN    = 1
Cjdc  ITOUT  = MIN0(KTOUT,KTIN)
      ITOUT  = KTOUT
      SIZE = (ITOUT+1)*(ITOUT+2)/2
      DO LOOP = 1, SIZE
        U(LOOP) = 0.0
        V(LOOP) = 0.0
      ENDDO
C
C ------------------------------------------------------------------
C*    Section 2.   Handle coefficients for M < ITOUT; N = M
C ------------------------------------------------------------------
C
  200 CONTINUE
C
      DO 255 JM = 1,ITOUT
C
        ZM = JM - 1.0
        ZN = ZM
C
        IF ( JM.GT.1 ) THEN
          U(K) = (-DD(ZM,ZN+1.)*VOR(IMN+1)+ZI*SS(ZM,ZN)*DIV(IMN))*REARTH
          V(K) = ( DD(ZM,ZN+1.)*DIV(IMN+1)+ZI*SS(ZM,ZN)*VOR(IMN))*REARTH
        ELSE
          U(K) = (-DD(ZM,ZN+1.)*VOR(IMN+1))*REARTH
          V(K) = ( DD(ZM,ZN+1.)*DIV(IMN+1))*REARTH
        ENDIF
C
        IMN = IMN + 1
          K =   K + 1
        JMP =  JM + 1
C
C       When N < ITOUT-1
C
        IF (JMP.LT.ITOUT) THEN
          DO 235 JN = JMP, ITOUT-1
            ZN   = JN - 1.0
            U(K) = ( DD(ZM,ZN)*VOR(IMN-1)-DD(ZM,ZN+1)*VOR(IMN+1)
     X              +ZI*SS(ZM,ZN)*DIV(IMN))*REARTH
            V(K) = (-DD(ZM,ZN)*DIV(IMN-1)+DD(ZM,ZN+1)*DIV(IMN+1)
     X              +ZI*SS(ZM,ZN)*VOR(IMN))*REARTH
C
              K  =   K + 1
            IMN  = IMN + 1
C
 235      CONTINUE
C
C         When N = ITOUT - 1
C
          ZN   = ITOUT - 1
          U(K) = ( DD(ZM,ZN)*VOR(IMN-1)+ZI*SS(ZM,ZN)*DIV(IMN))*REARTH
          V(K) = (-DD(ZM,ZN)*DIV(IMN-1)+ZI*SS(ZM,ZN)*VOR(IMN))*REARTH
          K    = K + 1
          IMN  = IMN + 1
        ENDIF
C
C       When N = ITOUT
C
        ZN   = ITOUT
        U(K) = ( DD(ZM,ZN)*VOR(IMN-1))*REARTH
        V(K) = (-DD(ZM,ZN)*DIV(IMN-1))*REARTH
        K    = K + 1
C
C       When N = ITOUT + 1
C
        IMN  = IMN + 1 + KTIN-ITOUT
C
 255  CONTINUE
C
C ------------------------------------------------------------------
C*    Section 3.   Handle coefficients for M = ITOUT.
C ------------------------------------------------------------------
C
  300 CONTINUE
C
C     When N = ITOUT
C
      U(K) = 0.0
      V(K) = 0.0
C
C     Ignore M = ITOUT; N = ITOUT + 1
C
C     K    = K + 1
C
      RETURN
      END
