C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE HPSHGPW(OLDLON,OLDLAT,NEWLON,NEWLAT,
     X                   PDIRI,NUMPTS,POLE)
C
C---->
C**** HPSHGPW
C
C     Purpose
C     -------
C
C     Rotates a grid of points about the y-axis and calculates the
C     angular change between local axes at each grid point.
C
C
C     Interface
C     ---------
C
C     CALL HPSHGPW(OLDLON,OLDLAT,NEWLON,NEWLAT,PDIRI,NUMPTS,POLE)
C
C     Input
C     -----
C
C     OLDLON - Array of longitudes to be rotated (degrees)
C     OLDLAT - Array of latitudes to be rotated (degrees)
C     NUMPTS - Number of gridpoints to be rotated
C     PDIRI  - Array of initial orientation angles, eg wind directions
C              (degrees)
C     POLE   - (Lat,long) of new south pole, measured in old grid
C
C
C     Output
C     ------
C
C     NEWLON - Array of rotated longitudes (degrees, -180 < angle <= 180 )
C     NEWLAT - Array of rotated latitudes (degrees)
C     PDIRI  - Array of modified orientation angles, changed to allow for
C              the angles between coordinate axes at each point before and
C              after rotation (degrees, -180 < angle <= 180 )
C
C
C     Method
C     ------
C
C     Rotates a grid of points about the y-axis and calculates the
C     angular change to local axes at each grid point.
C
C     The z-axis is the polar axis, increasing from south to north.
C     The x-axis is line from the centre of the globe through (0N, 0E),
C     increasing from the centre out to (0N, 0E).
C     The y-axis is line from the centre of the globe through (0N, 90E),
C     increasing from the centre out to (0N, 90E).
C
C     The sign of the angular change between local axes at each grid point
C     is such that it denotes the angle of rotation to be applied to the
C     old coordinate system to let its axes coincide with the new one. The
C     angle is measured in the old coordinate system, so increments to
C     angles in PDIRI are positive for anti-clockwise rotation from old
C     x-axis to new one.
C
C     As a consequence, if the initial values in PDIRI are wind directions
C     ddd in the old coordinate system (measured to conform to meteorological
C     use), the output values of PDIRI will be ddd in the new coordinate system
C     (modulo 360).
C
C     Note that the meteorological use of ddd has effectively the opposite
C     sign to the mathematically usual way of defining angles.
C
C     The angles between the coordinate axes at each point may be obtained
C     by calling the routine with the values in PDIRI all set to zero on input.
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
C     G.J.Cats            15 DEC 88.
C
C
C     Modified
C     --------
C
C     P.O'Sullivan        30/3/93 (wind and nonwind)
C
C     J.D.Chambers        October 1999
C
C
C----<
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
C     Parameters
C
      REAL EPSILON
      PARAMETER( EPSILON = 1E-5)
C                 `---------------> tolerance used for checking
C                                   proximity to +/-90.0 and 180.0
C
C     Subroutine arguments
C
      REAL OLDLON, OLDLAT, NEWLON, NEWLAT, PDIRI, POLE(2)
      DIMENSION OLDLON(*), OLDLAT(*), NEWLON(*), NEWLAT(*), PDIRI(*)
      INTEGER NUMPTS
C
C     Local variables
C
      INTEGER NXTPT
      REAL SINTHE, COSTHE, THETA
      REAL ZLAT, SINLAT, COSLAT
      REAL ZLON, SINLON, COSLON
      REAL ZNEW, LATNEW, NCOSLAT, COSNEW, LONNEW, COSDEL, DELTA, ZDIV
C
C     Statement functions
C
      REAL A
      REAL RADIAN, DEGREE, NORMAL
      REAL LONGMOD
C
C     Convert degrees to radians
C
      RADIAN(A) = (A) * 0.01745329252
C
C     Convert radians to degrees
C
      DEGREE(A) = (A) * 57.295779513
C
C     Normalise value such as sin or cos to range (-1,1)
C
      NORMAL(A) = MAX(MIN((A),1.0),-1.0)
C
C     -----------------------------------------------------------------|
C*    Section 1.   Initialise
C     -----------------------------------------------------------------|
C
      LONGMOD = -POLE(2)
      THETA  = RADIAN(POLE(1))
      SINTHE = -SIN(THETA)
      COSTHE = -COS(THETA)
C
C     -----------------------------------------------------------------|
C*    Section 2.   Handle each point separately (longitude,latitude)
C     -----------------------------------------------------------------|
C
      DO NXTPT = 1, NUMPTS
C
        ZLAT   = RADIAN(OLDLAT(NXTPT))
        SINLAT = SIN(ZLAT)
        COSLAT = COS(ZLAT)
C
        ZLON = OLDLON(NXTPT) + LONGMOD
        IF( ZLON.GT.180.0 ) ZLON = ZLON - 360.0
        IF( ABS(ZLON + 180.0).LT.EPSILON ) ZLON = 180.0
        ZLON   = RADIAN(ZLON)
        SINLON = SIN(ZLON)
        COSLON = COS(ZLON)
C
C       Calculate z coordinate of point after rotation and hence
C       its latitude after rotation since:
C         z = sin(lat)
C
        ZNEW = SINTHE*SINLAT+COSTHE*COSLAT*COSLON
        ZNEW = NORMAL(ZNEW)
        IF( ABS(ZNEW -1.0).LT.EPSILON ) Then
          LATNEW = 1.5707963268
          NEWLAT(NXTPT) = 90.0
          NCOSLAT = 0.0
        ELSE IF( ABS(ZNEW +1.0).LT.EPSILON ) THEN
          LATNEW = -1.5707963268
          NEWLAT(NXTPT) = -90.0
          NCOSLAT = 0.0
        ELSE
          LATNEW = ASIN(ZNEW)
          NEWLAT(NXTPT) = DEGREE(LATNEW)
          NCOSLAT = COS(LATNEW)
        ENDIF
C
C       Calculate x coordinate of point after rotation and hence
C       its longitude after rotation since:
C         x = cos(lat).cos(lon)
C         cos(lon) = x / cos(lat)
C
        IF( NCOSLAT.EQ.0.0 ) NCOSLAT = 1.0
        ZDIV = 1.0 / NCOSLAT
C
        COSNEW = ( (SINTHE*COSLAT*COSLON-COSTHE*SINLAT) ) * ZDIV
        COSNEW = NORMAL(COSNEW)
        LONNEW = SIGN(ACOS(COSNEW),ZLON)
        NEWLON(NXTPT) = DEGREE(LONNEW)
C
C       Use cosine rule from spherical trigonometry to find angle
C       between the directions of the meridians before and after
C       rotation.
C
        COSDEL = SINTHE*SINLON*SIN(LONNEW)+COSLON*COSNEW
        COSDEL = NORMAL(COSDEL)
        DELTA  = SIGN(ACOS(COSDEL),-COSTHE*ZLON)
C
C       Change the orientation angle (keep it in the range -180 to 180)
C
        PDIRI(NXTPT) = PDIRI(NXTPT) - DEGREE(DELTA)
        IF( PDIRI(NXTPT).GT.180.0 ) PDIRI(NXTPT) = PDIRI(NXTPT) - 360.0
        IF( PDIRI(NXTPT).LE.-180.0) PDIRI(NXTPT) = PDIRI(NXTPT) + 360.0
C
      ENDDO
C
C     -----------------------------------------------------------------|
C*    Section 9.   Closedown.
C     -----------------------------------------------------------------|
C
      RETURN
      END
