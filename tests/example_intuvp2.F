C
C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C Unless required by applicable law or agreed to in writing, software
C distributed under the License is distributed on an "AS IS" BASIS,
C WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      PROGRAM EXAMPLE_INTUVP2
      IMPLICIT NONE


C     Parameters
      INTEGER JPGRIB   ! GRIB size (up to 1/16 deg)
      INTEGER JPBYTES  ! bytes/integer
      PARAMETER (JPGRIB = 33190420)
#ifdef INTEGER_8
      PARAMETER (JPBYTES = 8)
#else
      PARAMETER (JPBYTES = 4)
#endif


C     Local variables
      INTEGER      INTV(4)
      REAL         REALV(4)
      CHARACTER*20 CHARV

      CHARACTER*128 INFILE, OUTFILE, ARG
      INTEGER INLEN, OUTLEN, U1, U2, IRET, N
      INTEGER IVOGRIB(JPGRIB), IDVGRIB(JPGRIB)
      INTEGER IUGRIB(JPGRIB), IVGRIB(JPGRIB)


C     Externals
      INTEGER INTOUT, INTUVP2, IARGC


C     ------------------------------------------------------------------


C     Pick up file names from command line
      INFILE  = ' '
      OUTFILE = ' '
      IF( IARGC().EQ.4 ) THEN
        DO N = 1, 4, 2
          CALL GETARG(N,ARG)
          IF (ARG.EQ.'-i') THEN
            CALL GETARG(N+1,INFILE)
          ELSEIF (ARG.EQ.'-o') THEN
            CALL GETARG(N+1,OUTFILE)
          ENDIF
        ENDDO
      ENDIF
      CALL CHECK(
     _  INDEX(INFILE,' ').EQ.1 .OR. INDEX(OUTFILE,' ').EQ.1,
     _  'Usage: example_intuvp2 -i infile -o outfile' )

      INTV  = 0
      REALV = 0.
      CHARV = ''


C     Define the grid interval for the output
      REALV(1) = 3.0
      REALV(2) = 3.0
      IRET = INTOUT('grid', INTV, REALV, CHARV)
      CALL CHECK(IRET.NE.0, 'INTOUT (grid) failed')
      REALV = 0.


C     Open input and output files
      CALL PBOPEN(U1, INFILE, 'r', IRET)
      CALL CHECK(IRET.NE.0, 'PBOPEN failed (r)')
      CALL PBOPEN(U2, OUTFILE, 'w', IRET)
      CALL CHECK(IRET.NE.0, 'PBOPEN failed (w)')


C     Start of loop on input vo/d pairs
      PRINT *, 'Start interpolation...'
      N = 0
 220  CONTINUE
      N = N + 1

C     Read next vo/d pair
      CALL PBGRIB(U1, IVOGRIB, JPGRIB * JPBYTES, INLEN, IRET)
      IF (IRET.EQ.-1) THEN
        IRET = 0
        GOTO 290
      ENDIF
      CALL CHECK(IRET.NE.0, 'PBGRIB failed (vo)')

      CALL PBGRIB(U1, IDVGRIB, JPGRIB * JPBYTES, INLEN, IRET)
      IF (IRET.EQ.-1) THEN
        IRET = 0
        GOTO 290
      ENDIF
      CALL CHECK(IRET.NE.0, 'PBGRIB failed (d)')

C     Interpolate
      PRINT *, 'Interpolate vo/d pair #', N
      OUTLEN = JPGRIB
      IRET = INTUVP2(IVOGRIB, IDVGRIB, JPGRIB, IUGRIB, IVGRIB, OUTLEN)
      CALL CHECK(IRET.NE.0, 'INTUVP failed')

C     Write the new u/v to file
      IF (OUTLEN.GT.0) THEN
        CALL PBWRITE(U2, IUGRIB, OUTLEN, IRET)
        CALL CHECK(IRET.LT.OUTLEN,'PBWRITE failed (vo)')
        CALL PBWRITE(U2, IVGRIB, OUTLEN, IRET)
        CALL CHECK(IRET.LT.OUTLEN,'PBWRITE failed (d)')
      ENDIF


C     Loop back for next vo/d pair
      GOTO 220


C     Close
 290  CONTINUE
      CALL PBCLOSE(U1, IRET)
      CALL PBCLOSE(U2, IRET)

      PRINT *, 'Interpolated ', (N-1), ' vo/d pair(s).'
      END


C     ------------------------------------------------------------------


      SUBROUTINE CHECK(OOPS,MSG)
      IMPLICIT NONE
      LOGICAL OOPS
      CHARACTER MSG*(*)
      IF (OOPS) THEN
        PRINT *, MSG
        CALL EXIT(3)
      ENDIF
      END

