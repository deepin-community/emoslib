C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

C     Determine if parameter is a wave direction parameter and
C     therefore requiring special interpolation treatment, given its ID
C     (warning: parameter table is not checked.)

      LOGICAL FUNCTION IS_WAVE_DIRECTION(PARAM)

      IMPLICIT NONE
      INTEGER PARAM

      IS_WAVE_DIRECTION =
     X  ((PARAM.EQ.113) .OR. !wefxd (140113, Wave energy flux mean direction)
     X   (PARAM.EQ.122) .OR. !mwd1
     X   (PARAM.EQ.125) .OR. !mwd2
     X   (PARAM.EQ.128) .OR. !mwd3
     X   (PARAM.EQ.230) .OR. !mwd
     X   (PARAM.EQ.235) .OR. !mdww
     X   (PARAM.EQ.238) .OR. !mdts
     X   (PARAM.EQ.242) .OR. !mdwi
     X   (PARAM.EQ.249))     !dwi

      END

