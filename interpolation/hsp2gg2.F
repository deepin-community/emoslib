C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION HSP2GG2(KTRUNC,NS,EW,KGAUSS,KPTS,PLATS,KSIZE)
C
C---->
C**** HSP2GG2
C
C     Purpose
C     -------
C     This routine finds a suitable Gaussian grid and spectral
C     truncation for given North-South/East-West increments.
C
C
C     Interface
C     ---------
C     IRET = HSP2GG2(KTRUNC,NS,EW,KGAUSS,KPTS,PLATS,KSIZE)
C
C
C     Input parameters
C     ----------------
C     NS     - North-South increment
C     EW     - East-West increment
C
C
C     Output parameters
C     -----------------
C     KTRUNC - spectral truncation
C     KGAUSS - Gaussian grid number
C     HTYPE  - Gaussian grid type
C              = 'R' for reduced ("quasi-regular"),
C              = 'O' for reduced/octahedral,
C              = 'F' for full,
C              = 'U' for a user-defined Gaussian grid
C     KPTS   - array giving number of points along each line of latitude
C              in the reduced Gaussian grid (both hemispheres)
C     PLATS  - array giving Gaussian latitudes (both hemispheres)
C     KSIZE  - number of points in the reduced Gaussian grid
C
C     Returns 0 if all OK, otherwise there is an error.
C
C
C     Externals
C     ---------
C     HSH2GG - Finds a suitable Gaussian grid and/or spectral truncation
C     for a given spectral truncation and/or lat/long increments.
C
C----<
      IMPLICIT NONE

!#include "parim.h"
!#include "jparams.h"

C     Arguments
      INTEGER KTRUNC, KGAUSS, KPTS(*), KSIZE
      REAL NS, EW, PLATS(*)

C     Externals
      INTEGER HSH2GG

C     Local variables
      CHARACTER*1 HTYPE

      KTRUNC = 0   ! output
      KGAUSS = 0   ! output
      HTYPE  = ''  ! output
      HSP2GG2 = HSH2GG(NS,EW,KTRUNC,KGAUSS,HTYPE,KPTS,PLATS,KSIZE)
      END
