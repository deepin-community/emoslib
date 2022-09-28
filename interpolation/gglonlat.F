C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      REAL FUNCTION GGLONLAT(KNUM,PLAT,KPTS,FIELD,MISSVALUE,NN,LON,LAT)
C
C---->
C**** GGLONLAT
C
C     PURPOSE
C     _______
C
C     Returns the bilinear/nearest neighbour-interpolated value at a
C     specific position, given a global, unrotated Gaussian grid
C     description. Weights/distances are calculated in lon/lat space.
C
C
C     INTERFACE
C     _________
C
C     RRET = GGLONLAT(KNUM,PLAT,KPTS,FIELD,MISSVALUE,NN,LON,LAT)
C
C
C     Input parameters
C     ________________
C
C     KNUM      - Gaussian grid number
C     PLAT      - ... latitudes array (length 2*KNUM), both hemispheres
C     KPTS      - ... number of points array at each latitude (length
C                 2*KNUM), both hemispheres
C     FIELD     - Gaussian field values array (length SUM(KPTS(:)))
C     MISSVALUE - Value to return if interpolation fails
C     NN        - If non-zero, perform nearest neighbour interpolation
C     LON       - Requested interpolation point longitude
C     LAT       - ... latitude
C
C     Return value
C     ____________
C
C     Bilinear/nearest neighbour interpolated value, missing value if
C     not successful or any interpolating values missing.
C
C
C----<
      IMPLICIT NONE

C     Arguments
      INTEGER KNUM, KPTS(*)
      INTEGER NN
      REAL PLAT(*), FIELD(*), MISSVALUE, LON, LAT

C     Local variables
      INTEGER J1, J3, I, J, M
      REAL DLAT1, DLAT3, PLON(4), A, ZSOUTH, ZNORTH !, B
      REAL V(4), W(4)  ! field values and weights around int. point


C     Initialize
      GGLONLAT = MISSVALUE
      V    = MISSVALUE
      PLON = 0.


C     Set North-neighboring indices:
C     row idices J1:                LAT <= PLAT(J1)
C     abs indices I1/I2: LON(I1) <= LON <= LON(I2)
C     longitudes:        PLON(1) <= LON <= PLON(2)
      I  = 1
      J1 = 1
      DO J = 2, KNUM*2
        IF (PLAT(J)<LAT) GOTO 100
        I  = I + KPTS(J-1)
        J1 = J
      ENDDO
100   CONTINUE

      A = REAL(360.)/REAL(KPTS(J1))  ! increment length
      M = FLOOR(LON/A)               ! n. full increments up to LON
      DO WHILE (M.GE.KPTS(J1))
        M = M - KPTS(J1)             ! guarantee M*increment < 360.
      ENDDO
      PLON(1) = REAL( M )*A
      PLON(2) = REAL(M+1)*A

      V(1) = FIELD( I+M )
      V(2) = FIELD( I+MOD(M+1,KPTS(J1)) )


C     Set South-neighboring indices (start with previous row results):
C     row idices J3:     PLAT(J3) <= LAT
C     abs indices I3/I4: LON(I3)  <= LON <= LON(I4)
C     longitudes:        PLON(3)  <= LON <= PLON(4)
      J3 = J1
      PLON(3:4) = PLON(1:2)
      V   (3:4) = V   (1:2)
      IF ( LAT.LT.PLAT(J1) .AND. LAT.GT.PLAT(MIN(J1+1,2*KNUM)) ) THEN
        I  = I + KPTS(J1)
        J3 = MIN(J1+1,2*KNUM)

        A = REAL(360.)/REAL(KPTS(J3))  ! increment length
        M = FLOOR(LON/A)
        DO WHILE (M.GE.KPTS(J3))
          M = M - KPTS(J3)             ! guarantee M*increment < 360.
        ENDDO
        PLON(3) = REAL( M )*A
        PLON(4) = REAL(M+1)*A

        V(3) = FIELD( I+M )
        V(4) = FIELD( I+MOD(M+1,KPTS(J3)) )
      ENDIF


C     Calculate weights (nn/any-missing/bilinear/linear cases)
      IF (NN.NE.0) THEN

C       Nearest neighbour interpolation
        W(1:2) = (PLAT(J1)-LAT)**2 + (PLON(1:2)-LON)**2  ! sq. distance
        W(3:4) = (PLAT(J3)-LAT)**2 + (PLON(3:4)-LON)**2  ! ...
        J = 1
        DO I = 2, 4
          IF ( V(I).NE.MISSVALUE .AND. W(I).LT.W(J) ) J = I
        ENDDO
        GGLONLAT = V(J)

      ELSEIF ( V(1).EQ.MISSVALUE .OR. V(2).EQ.MISSVALUE .OR.
     .         V(3).EQ.MISSVALUE .OR. V(4).EQ.MISSVALUE ) THEN

C       (some values are missing, nothing to do)

      ELSEIF (J1.NE.J3) THEN

#if 0
C       Method 1:
C       Bi-linear interpolation is calculated in lon/lat space, with the
C       following distribution of weights:
C
C
C            (V(1))--------(X12)------------(V(2))      (row J1)
C              /    W(4)     |        W(3)     \
C           (X13)--------(LON,LAT)-----------(X24)
C             /              |                    \
C            /     W(2)      |          W(1)       \
C            /               |                      \
C       (V(3))-------------(X34)--------------------(V(4))    (row J3)
C
C       with weights corresponding to the relative areas of the opposite
C       quadrilateral subdivisions:
C         W(1) = A( P,      X(2,4), X(3,4), 4      ) / A(1,2,3,4)
C         W(2) = A( X(1,3), P,      3,      X(3,4) ) / A(1,2,3,4)
C         W(3) = A( X(1,2), 2,      P,      X(2,4) ) / A(1,2,3,4)
C         W(4) = A( 1,      X(1,2), X(1,3), P      ) / A(1,2,3,4)

        A = (PLON(2)-PLON(4))/(PLAT(J1)-PLAT(J3))
        B = (PLAT(J1)*PLON(4)-PLAT(J3)*PLON(2))/(PLAT(J1)-PLAT(J3))
        W(1) = (LAT-PLAT(J3)) * (A*(LAT+PLAT(J3))/2.+B-LON)
        W(3) = (PLAT(J1)-LAT) * (A*(LAT+PLAT(J1))/2.+B-LON)

        A = (PLON(1)-PLON(3))/(PLAT(J1)-PLAT(J3))
        B = (PLAT(J1)*PLON(3)-PLAT(J3)*PLON(1))/(PLAT(J1)-PLAT(J3))
        W(2) = (LAT-PLAT(J3)) * (LON-A*(LAT+PLAT(J3))/2.+B)
        W(4) = (PLAT(J1)-LAT) * (LON-A*(LAT+PLAT(J1))/2.+B)
#elif 0
C       Method 2:
C       Bi-linear interpolation is calculated in lon/lat space, with the
C       following distribution of weights:
C
C
C            (V(1))----------+--------------(V(2))      (row J1)
C              |    W(4)     |        W(3)     |
C          +---+---------(LON,LAT)-------------+------+
C          |                 |                        |
C          |       W(2)      |          W(1)          |
C          |                 |                        |
C       (V(3))---------------+----------------------(V(4))    (row J3)
C
C       with weights corresponding to the relative areas of the opposite
C       quadrilateral subdivisions.

        W(1) = (LAT-PLAT(J3))*(PLON(4)-LON)
        W(2) = (LAT-PLAT(J3))*(LON-PLON(3))
        W(3) = (PLAT(J1)-LAT)*(PLON(2)-LON)
        W(4) = (PLAT(J1)-LAT)*(LON-PLON(1))
#else
C       Method 3:
C       Bi-linear interpolation is calculated in lon/lat space, with the
C       following distribution of weights (Emoslib emulation):
C
C
C            (V(1))----------+--------------(V(2))      (row J1)
C              |    W(4)     |        W(3)     |
C          +---+---------(LON,LAT)-------------+------+
C          |                 |                        |
C          |       W(2)      |          W(1)          |
C          |                 |                        |
C       (V(3))---------------+----------------------(V(4))    (row J3)
C
C       with weights corresponding to the relative areas of the opposite
C       quadrilateral subdivisions.

        DLAT1 =         REAL(NINT( PLAT(J1) * 100000 )) !PLAT(J1)
        DLAT3 =         REAL(NINT( PLAT(J3) * 100000 )) !PLAT(J3)
        DLAT1 = DLAT1 - REAL(NINT(   LAT    * 100000 ))
        DLAT3 =         REAL(NINT(   LAT    * 100000 )) - DLAT3

        ZSOUTH = PLON(4)-PLON(3)
        ZNORTH = PLON(2)-PLON(1)

        W(1) = DLAT3 * (PLON(4)-LON) !* ZSOUTH
        W(2) = DLAT3 * (LON-PLON(3)) !* ZSOUTH
        W(3) = DLAT1 * (PLON(2)-LON) !* ZNORTH
        W(4) = DLAT1 * (LON-PLON(1)) !* ZNORTH

#endif

        A = W(1)+W(2)+W(3)+W(4)
        W = W/A
        GGLONLAT = V(1)*W(1) + V(2)*W(2) + V(3)*W(3) + V(4)*W(4)

      ELSE

C       Linear interpolation
        W(1) = (PLON(2)-LON) / (PLON(2)-PLON(1))
        W(2) = 1-W(1)

        GGLONLAT = V(1)*W(1) + V(2)*W(2)

      ENDIF


      END
