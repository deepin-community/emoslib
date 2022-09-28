C
C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C
C**** "grfixed.h"
C
C     PURPOSE
C     _______
C
C     This file contains all the fixed work space array definitions for
C     grid point to grid point interpolation.
C
C     INTERFACE
C     _________
C
C     #include "grfixed.h"
C
C     Common block usage
C     __________________
C
C     FIXED_INTS
C
C     MEXPAND      - Array of length JPLG01 (see parim.h) used to
C                    expand one latitude line of the 10 minute land
C                    sea mask file to have one word per bit for
C                    improved efficiency.
C     MILLEN       - Array of length JPLAT (see parim.h) containing
C                    a quasi regular Gaussian field latitude line
C                    length definition. This array may be provided by
C                    the interpolation definition routines.
C     MWORK        - Array of length JPLONG (see parim.h) used to
C                    read one latitude line of a standard land sea
C                    mask file.
C
C     FIXED_REAL
C
C     RIGAUSS      - Array of length JPLAT (see parim.h) containing a
C                    full definition of the Gaussian latitudes for an
C                    input field Gaussian truncation.
C     ROGAUSS      - Array of length JPLAT (see parim.h) containing a
C                    full definition of the Gaussian latitudes for an
C                    output field Gaussian truncation.
C
C     Author
C     ______
C
C     K. Fielding      *ECMWF*      Jan 1994
C
C     MODIFICATIONS
C     _____________
C
C     J.D.Chambers     ECMWF        June 1996
C
C
C     _______________________________________________________
C
C*    Section 1. Fixed length arrays
C     _______________________________________________________
C
      INTEGER MILLEN(JPLAT), MWORK(JPLONG), MEXPAND(JPLG01)
C
      REAL RIGAUSS(JPLAT), ROGAUSS(JPLAT), ROREDLL(JPLAT)
C
      COMMON /FIXED_INTS/ MWORK, MILLEN, MEXPAND
C
      SAVE /FIXED_INTS/
C
      COMMON /FIXED_REAL/ RIGAUSS, ROGAUSS, ROREDLL
C
      SAVE /FIXED_REAL/
C
