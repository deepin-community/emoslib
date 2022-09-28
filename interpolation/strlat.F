C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE STRLAT( OLDLAT, STRETCH, KFLAG, NEWLAT, MFACTOR)
C
C---->
C**** STRLAT
C
C     PURPOSE
C     _______
C
C     This routine transforms between stretched and real latitudes
C     and calculates the map factor.
C
C     INTERFACE
C     _________
C
C     CALL STRLAT( OLDLAT, STRETCH, KFLAG, NEWLAT, MFACTOR)
C
C     Input parameters
C     ________________
C
C     OLDLAT  - Input latitude
C     STRETCH - Stretching factor
C     KFLAG   - Flag to show direction of transform (real <-> stretched)
C               0 = transform from real latitudes to stretched latitudes
C               1 = transform from stretched latitudes to real latitudes
C
C     Output parameters
C     ________________
C
C     NEWLAT  - Input latitude
C     MFACTOR - Map factor
C
C
C     Common block usage
C     __________________
C
C     None
C
C     Method
C     ______
C
C     Stretching factor S
C
C     Map factor,  m = S**2 + 1 + (S**2 -1)*mu
C                      -----------------------
C                              2*S
C
C     where mu = sin(lat) on the real sphere
C
C     mu = (S**2+1)*md + (S**2-1)
C          ----------------------
C          (S**2+1) + (S**2-1)*md
C
C     where md = sin(lat) on the stretched sphere.
C
C     By rearrangement:
C
C     md = (S**2+1)*mu - (S**2-1)
C          ----------------------
C          (S**2+1) - (S**2-1)*mu
C
C     Wind U and V components are transformed as
C
C       vreal = m * vtransformed
C
C     Vorticity and divergence fields are transformed as
C
C       vreal = (m*m) * vtransformed
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
C     None.
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers      ECMWF    July 1999
C
C
C     MODIFICATIONS
C     _____________
C
C     None
C
C----<
C     _______________________________________________________
C
C
      IMPLICIT NONE
C
C     Subroutine arguments
C
      REAL OLDLAT, STRETCH, NEWLAT, MFACTOR
      INTEGER KFLAG
C
C     Parameters
C
      INTEGER JPRL2TR
      PARAMETER (JPRL2TR = 0)
      REAL PPI, DEG2RAD, RAD2DEG
      PARAMETER ( PPI = 3.14159265358979 )
      PARAMETER (DEG2RAD = PPI / 180.0)
      PARAMETER (RAD2DEG = 180.0 / PPI)
C
C     Local variables
C
      REAL A, B, ANGLE, MU, MUPRIME
C     _______________________________________________________
C
C*    Section 1.    Initialization.
C     _______________________________________________________
C
  100 CONTINUE
C
      A     = STRETCH * STRETCH + 1
      B     = A - 2
      ANGLE = OLDLAT * DEG2RAD
C
C     _______________________________________________________
C
C*    Section 2.    Work out geometry
C     _______________________________________________________
C
 200  CONTINUE
C
C     Transform from real latitudes to stretched latitudes
C
      IF( KFLAG.EQ.JPRL2TR ) THEN
        MUPRIME = SIN(ANGLE)
        MU      = (A*MUPRIME + B) / (A + B*MUPRIME)
        NEWLAT  = ASIN(MU) * RAD2DEG
C
C     Transform from stretched latitudes to real latitudes
C
      ELSE
        MU      = SIN(ANGLE)
        MUPRIME = (A*MU - B) / (A - B*MU)
        NEWLAT  = ASIN(MUPRIME) * RAD2DEG
      ENDIF
C
C     Calculate the map factor
C
      MFACTOR = (A + B*MU) / (2*STRETCH)
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine.
C     _______________________________________________________
C
      RETURN
      END
