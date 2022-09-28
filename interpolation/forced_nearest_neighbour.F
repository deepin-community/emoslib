C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

C     Determine if parameter needs to be interpolated with nearest
C     neighbour-type method, given its ID and table.

      LOGICAL FUNCTION FORCED_NEAREST_NEIGHBOUR(FORCED,TABLE,PARAM)

      IMPLICIT NONE

      LOGICAL FORCED
      INTEGER TABLE
      INTEGER PARAM
      CHARACTER*12 YFLAG

C     Use method if FORCED, is a special param ID, or set by env
      FORCED_NEAREST_NEIGHBOUR =
     x
     x FORCED
     x .OR.
     x
     x (TABLE.EQ.128).AND.
     x   ((PARAM.EQ.27).OR.  !cvl
     x    (PARAM.EQ.28).OR.  !cvh
     x    (PARAM.EQ.29).OR.  !tvl
     x    (PARAM.EQ.30).OR.  !tvh
     x    (PARAM.EQ.43) )    !slt
     x .OR.
     x
     x (TABLE.EQ.260).AND.
     x   ((PARAM.EQ.15))     !ptype
     x .OR.
     x
     x (TABLE.EQ.228).AND.
     x   ((PARAM.EQ.217).OR. !ilspf
     x    (PARAM.EQ.218).OR. !crr
     x    (PARAM.EQ.219).OR. !lsrr
     x    (PARAM.EQ.220).OR. !csfr
     x    (PARAM.EQ.221).OR. !lssfr
     x    (PARAM.EQ.222).OR. !mxtpr3
     x    (PARAM.EQ.223).OR. !mntpr3
     x    (PARAM.EQ.224).OR. !mxtpr6
     x    (PARAM.EQ.225).OR. !mntpr6
     x    (PARAM.EQ.226).OR. !mxtpr
     x    (PARAM.EQ.227) )   !mntpr

      IF (.NOT.FORCED_NEAREST_NEIGHBOUR) THEN
        CALL GETENV('NEAREST_NEIGHBOUR', YFLAG)
        FORCED_NEAREST_NEIGHBOUR = (YFLAG(1:1).EQ.'1')
      END IF

      END

