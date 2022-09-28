C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE JWSCAL( PZFA, KLATS, KLONS, KFIRST, PLATS)
C
C---->
C**** JWSCAL
C
C     PURPOSE
C     _______
C
C     This routine applies a scale factor to all latitudes except
C     the poles
C
C     INTERFACE
C     _________
C
C     CALL JWSCAL( PZFA, KLATS, KLONS, KFIRST, PLATS)
C
C     Input parameters
C     ________________
C
C     PZFA     - Array of wind field (U or V) fourier coefficients
C     KLATS    - Number of latitude lines in PZFA
C     KLONS    - Number of longitude points in each line of latitude
C     KFIRST   - First latitude to process in PLATS
C     PLATS    - Array of gaussian latitudes (degrees)
C
C     Output parameters
C     ________________
C
C     PZFA     - Modified array of fourier coefficients
C
C     Common block usage
C     __________________
C
C     None.
C
C     Method
C     ______
C
C     Apply latitude-dependent scale factor to all coefficients.
C     It is assumed that no latitude is at the North or South pole.
C
C     Externals
C     _________
C
C     EMOSLIB_SSCAL  - Apply scalar correction to row of values
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
C     PZFA contains pairs of north/south latitudes.
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers      *ECMWF*      Jan 1994
C
C     MODIFICATIONS
C     _____________
C
C     None.
C
C----<
C     _______________________________________________________
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
C*    Prefix conventions for variable names
C
C     Logical      L (but not LP), global or common.
C                  O, dummy argument
C                  G, local variable
C                  LP, parameter.
C     Character    C, global or common.
C                  H, dummy argument
C                  Y (but not YP), local variable
C                  YP, parameter.
C     Integer      M and N, global or common.
C                  K, dummy argument
C                  I, local variable
C                  J (but not JP), loop control
C                  JP, parameter.
C     Real         A to F and Q to X, global or common.
C                  P (but not PP), dummy argument
C                  Z, local variable
C                  PP, parameter.
C
      IMPLICIT NONE
C
#include "jparams.h"
C
C     Subroutine arguments
C
      REAL   PZFA
      DIMENSION PZFA(JPLONO+2, 64)
      INTEGER   KLATS, KLONS, KFIRST
      REAL      PLATS
      DIMENSION PLATS(*)
C
C     Local variables
C
      INTEGER JNEXTLAT, INORTH, ISOUTH
      REAL ZCOSI, ZLAT, ZPIBY2, ZDEGR
C     _______________________________________________________
C
C*    Section 1.    Initialization.
C     _______________________________________________________
C
  100 CONTINUE
      ZPIBY2 = PPI / 2.0
      ZDEGR  = PPI / 180.0
C
C     _______________________________________________________
C
C*    Section 2.    Apply scale factor to all latitudes.
C     _______________________________________________________
C
 200  CONTINUE
      INORTH = -1
      DO 220 JNEXTLAT = 1, KLATS
        ZLAT  = PLATS( KFIRST + JNEXTLAT - 1 )
        INORTH = INORTH + 2
        ISOUTH = INORTH + 1
        ZCOSI = 1.0 / COS( ZLAT * ZDEGR )
        CALL EMOSLIB_SSCAL( KLONS, ZCOSI, PZFA( 2, INORTH), 1)
        CALL EMOSLIB_SSCAL( KLONS, ZCOSI, PZFA( 2, ISOUTH), 1)
 220  CONTINUE
C     _______________________________________________________
C
C*    Section 9. Return to calling routine.
C     _______________________________________________________
C
 900  CONTINUE
C
      RETURN
      END
