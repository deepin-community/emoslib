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

      PROGRAM EXAMPLE_INTF2
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
      INTEGER INGRIB(JPGRIB), OUTGRIB(JPGRIB)


C     Externals
      INTEGER INTOUT, INTF2, IARGC


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
     _  'Usage: example_intf2 -i infile -o outfile' )

      INTV  = 0
      REALV = 0.
      CHARV = ''


C     Define the packing accuracy for the new field(s)
      INTV(1) = 24
      IRET = INTOUT('accuracy', INTV, REALV, CHARV)
      CALL CHECK(IRET.NE.0, 'INTOUT (accuracy) failed')
      INTV(1) = 0


C     Define the geographical area for the new field(s)
      REALV(1) =  60.
      REALV(2) = -10.
      REALV(3) =  40.
      REALV(4) =  15.
      IRET = INTOUT('area', INTV, REALV, CHARV)
      CALL CHECK(IRET.NE.0, 'INTOUT (area) failed')
      REALV = 0.


C     Define the grid interval for the new field(s)
      REALV(1) = 1.5
      REALV(2) = 1.5
      IRET = INTOUT('grid', INTV, REALV, CHARV)
      CALL CHECK(IRET.NE.0, 'INTOUT (grid) failed')
      REALV = 0.


C     Open input and output files
      CALL PBOPEN(U1, INFILE, 'r', IRET)
      CALL CHECK(IRET.NE.0, 'PBOPEN failed (r)')
      CALL PBOPEN(U2, OUTFILE, 'w', IRET)
      CALL CHECK(IRET.NE.0, 'PBOPEN failed (w)')


C     Start of loop on input fields
      PRINT *, 'Start interpolation...'
      N = 0
 220  CONTINUE
      N = N + 1

C     Read next field
      CALL PBGRIB(U1, INGRIB, JPGRIB*JPBYTES, INLEN, IRET)
      IF (IRET.EQ.-1) THEN
        IRET = 0
        GOTO 290
      ENDIF
      CALL CHECK(IRET.NE.0, 'PBGRIB failed')

C     Interpolate
      PRINT *, 'Interpolate field #', N
      OUTLEN = JPGRIB
      IRET = INTF2(INGRIB,INLEN,OUTGRIB,OUTLEN)
      CALL CHECK(IRET.NE.0, 'INTF failed')

C     Write the new field to file
      IF (OUTLEN.GT.0) THEN
        CALL PBWRITE(U2, OUTGRIB, OUTLEN, IRET)
      ELSE
        PRINT *, 'Output same as input'
        CALL PBWRITE(U2, INGRIB, INLEN, IRET)
      ENDIF
      CALL CHECK(IRET.LT.0, 'PBWRITE failed')


C     Loop back for next field
      GOTO 220


C     Close
 290  CONTINUE
      CALL PBCLOSE(U1, IRET)
      CALL PBCLOSE(U2, IRET)

      PRINT *, 'Interpolated ', (N-1), ' field(s).'
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

