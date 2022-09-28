C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION QGOCT(NLAT)
C
C---->
C**** QGOCT
C
C     PURPOSE
C     _______
C
C     Returns number of points at each Gaussian latitude (to fill pl array)
C     for a reduced Gaussian grid, in octahedral progression series
C
C
C     Input parameters
C     ________________
C
C     NLAT - latitude index starting from pole (1-based)
C
C----<
      IMPLICIT NONE
      INTEGER NLAT
      QGOCT = 16+4*MAX(NLAT,1)
      END FUNCTION
