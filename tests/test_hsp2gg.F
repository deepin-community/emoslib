C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C
      PROGRAM HSP2GG_TESTS
      IMPLICIT NONE

C     Externals
      INTEGER HSP2GG, HSP2GG2, HSP2GG3

C     Local variables
      INTEGER IRET(3)
      INTEGER KTRUNC, KGAUSS, KSIZE
      INTEGER KPTS(96)
      REAL LL(2), PLATS(96)

      IRET = 0

      KTRUNC = 63  ! input
      KGAUSS = 0   ! output
      KPTS  = 0
      PLATS = 0.
      IRET(1) = HSP2GG(KTRUNC,KGAUSS,KPTS,PLATS,KSIZE)
      IF (IRET(1).EQ.0) THEN
          PRINT *, 'HSP2GG in:  KTRUNC = ', KTRUNC
          PRINT *, 'HSP2GG out: KGAUSS = ', KGAUSS
          IF (KGAUSS.NE.48)   IRET(1) = -1
          IF (KSIZE.NE.13280) IRET(1) = -2
      ENDIF

      LL(:)  = 2.5  ! input
      KGAUSS = 0    ! output
      KTRUNC = 0    ! output
      KPTS  = 0
      PLATS = 0.
      IRET(2) = HSP2GG2(KTRUNC,LL(1),LL(2),KGAUSS,KPTS,PLATS,KSIZE)
      IF (IRET(2).EQ.0) THEN
          PRINT *, 'HSP2GG2 in:  NS/EW  = ', LL(1:2)
          PRINT *, 'HSP2GG2 out: KGAUSS = ', KGAUSS
          PRINT *, 'HSP2GG2 out: KTRUNC = ', KTRUNC
          IF (KTRUNC.NE.63)   IRET(2) = -3
          IF (KGAUSS.NE.48)   IRET(2) = -4
          IF (KSIZE.NE.13280) IRET(1) = -2
      ENDIF

      KGAUSS = 48  ! input
      KTRUNC = 0   ! output
      KPTS  = 0
      PLATS = 0.
      IRET(3) = HSP2GG3(KTRUNC,KGAUSS,KPTS,PLATS,KSIZE)
      IF (IRET(3).EQ.0) THEN
          PRINT *, 'HSP2GG3 in:  KGAUSS = ', KGAUSS
          PRINT *, 'HSP2GG3 out: KTRUNC = ', KTRUNC
          IF (KTRUNC.NE.95)   IRET(3) = -6
          IF (KSIZE.NE.13280) IRET(1) = -2
      ENDIF

      IF ( IRET(1).NE.0 .OR.
     .     IRET(2).NE.0 .OR.
     .     IRET(3).NE.0 ) CALL EXIT(-1)
      END

