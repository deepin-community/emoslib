C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

C     Determine if FFTW should be used, instead of the usual FFT99 (as
C     per the environment)

      LOGICAL FUNCTION FORCED_FFTW(FORCED)

      IMPLICIT NONE

      LOGICAL FORCED
      CHARACTER*12 YFLAG

C     Set if FORCED, or set by env
      FORCED_FFTW = FORCED
      IF( .NOT. FORCED_FFTW ) THEN
        CALL GETENV('EMOSLIB_FORCED_FFTW', YFLAG)
        FORCED_FFTW = (YFLAG(1:1).EQ.'1')
      ENDIF

      END

