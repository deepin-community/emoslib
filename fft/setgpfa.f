c Copyright 1981-2016 ECMWF.
c
c This software is licensed under the terms of the Apache Licence 
c Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
c
c In applying this licence, ECMWF does not waive the privileges and immunities 
c granted to it by virtue of its status as an intergovernmental organisation 
c nor does it submit to any jurisdiction.
c

      SUBROUTINE SETGPFA(TRIGS,N)
*
      DIMENSION TRIGS(*)
      DIMENSION NJ(3)
*
*     DECOMPOSE N INTO FACTORS 2,3,5
*     ------------------------------
      NN = N
      IFAC = 2
*
      DO 30 LL = 1 , 3
      KK = 0
   10 CONTINUE
      IF (MOD(NN,IFAC).NE.0) GO TO 20
      KK = KK + 1
      NN = NN / IFAC
      GO TO 10
   20 CONTINUE
      NJ(LL) = KK
      IFAC = IFAC + LL
   30 CONTINUE
*
      IF (NN.NE.1) THEN
         WRITE(6,40) N
   40    FORMAT(' *** WARNING!!!',I10,' IS NOT A LEGAL VALUE OF N ***')
         RETURN
      ENDIF
*
      IP = NJ(1)
      IQ = NJ(2)
      IR = NJ(3)
*
*     COMPUTE LIST OF ROTATED TWIDDLE FACTORS
*     ---------------------------------------
      NJ(1) = 2**IP
      NJ(2) = 3**IQ
      NJ(3) = 5**IR
*
      TWOPI = 4.0 * ASIN(1.0)
      I = 1
*
      DO 60 LL = 1 , 3
      NI = NJ(LL)
      IF (NI.EQ.1) GO TO 60
*
      DEL = TWOPI / FLOAT(NI)
      IROT = N / NI
      KINK = MOD(IROT,NI)
      KK = 0
*
      DO 50 K = 1 , NI
      ANGLE = FLOAT(KK) * DEL
      TRIGS(I) = COS(ANGLE)
      TRIGS(I+1) = SIN(ANGLE)
      I = I + 2
      KK = KK + KINK
      IF (KK.GT.NI) KK = KK - NI
   50 CONTINUE
   60 CONTINUE
*
      RETURN
      END
