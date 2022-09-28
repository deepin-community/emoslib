C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE OFFSET2(IOFF,NLAST,IWORD,NBIT,ILEN,IERR)
C
C----> OFFSET2
C 
C     Purpose.
C     --------
C
C     Calculates the word and bit offset of the start of the next
C     bit-field in an array of binary data, following the
C     insertion/extraction of one or more consecutive bit-fields.
C
C     Interface.
C     ----------
C
C     CALL OFFSET2(IOFF,NLAST,IWORD,NBIT,ILEN,IERR)
C 
C
C     Input Parameters.
C     -----------------
C
C     IOFF  - Bit offset at which last insertion or extraction started.
C     NLAST - Number of bits in fields inserted or extracted in 
C             previous operation.
C     IWORD - Word number of array at which previous operation started.
C     NBIT  - Number of bits in computer word.
C     ILEN  - Number of words in array.
C
C     Output Parameters.
C     -----------------
C
C     IOFF  - Bit offset at which next insertion or extraction starts.
C     IWORD - Word number of array at which next operation starts.
C 
C     IERR  - = 0 if no error.
C             = -2 if next operation is outside array bounds.

C
C
C     Method.
C     -------
C
C     See below.
C
C
C     Externals.
C     ----------
C
C     None.
C
C
C     Reference.
C     ----------
C
C     None.
C
C
C     Comments.
C     ---------
C
C     Rewrite of offset.F to speed processing.
C
C
C     Author.
C     -------
C
C     J.D.Chambers      ECMWF     September 2001
C
C
C     Modifications.
C     --------------
C
C     None.
C 
C
C     ---------------------------------------------------------------
C----<
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Subroutine arguments
C
      INTEGER IOFF,NLAST,IWORD,NBIT,ILEN,IERR
C
C     Clear error indicator
C
      IERR = 0
C
C     Calculate next word and bit positions.
C
      IOFF = IOFF + (NLAST-(NLAST/NBIT)*NBIT)
C
      IF( IOFF.GE.NBIT ) THEN
        IWORD = IWORD + (NLAST/NBIT) + 1
        IOFF  = IOFF  - NBIT
      ELSE
        IWORD = IWORD + (NLAST/NBIT)
      ENDIF
C
C     Check that next word to be accessed lies within the array bounds.
C
      IF( IWORD.GT.ILEN ) THEN
        IERR = -2
        WRITE(GRPRSM,*) 'Word ',IWORD,' is outside array bounds ',ILEN
      ENDIF
C
      RETURN
      END
