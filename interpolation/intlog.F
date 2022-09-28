C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE INTLOG(KLEVEL, MESSAGE, KNUM)
C
C---->
C**** INTLOG
C
C     PURPOSE
C     _______
C
C     This routine logs error messages.
C
C
C     INTERFACE
C     _________
C
C     CALL INTLOG(KLEVEL, MESSAGE, KNUM)
C
C
C     Input parameters
C     ________________
C
C     KLEVEL   - Severity level for reported message
C                = 0 for debug
C                = 1 for information
C                = 2 for warning
C                = 3 for error
C                = 4 for fatal
C     MESSAGE  - Message text
C     KNUM     - Message number
C
C
C     Output parameters
C     ________________
C
C     None.
C
C
C     Common block usage
C     __________________
C
C     LDEBUG in /INTLOGC/ controls display of message
C                 = 0 for no display
C                 = 1 to display
C
C
C     Method
C     ______
C
C     Prints message and number if debug flag is 'on'.
C
C
C     Externals
C     _________
C
C     INTLOGT  - sends any ERROR, FATAL or WARN message to the
C                MARS server.
C
C
C     Comments
C     ________
C
C     LDEBUG is toggled by a call to INTLOGD.
C
C
C     AUTHOR
C     ______
C
C     J.D.Chambers      *ECMWF*      Jul 1995
C
C
C     MODIFICATIONS
C     _____________
C
C     J.D.Chambers      *ECMWF*      March 1996
C     Prepare error message for MARS server.
C
C
C----<
C     _______________________________________________________
C
      IMPLICIT NONE
C
#include "parim.h"
C
C     Subroutine arguments.
C
      INTEGER KLEVEL
      INTEGER KNUM
      CHARACTER *(*) MESSAGE
C
#include "intlog.h"

#ifdef MPI_DEBUG
#include "mpif.h" 
#endif

C
C     Local variables.
C
      CHARACTER*120 NEWMESS
      INTEGER NLEV, LOOP
      CHARACTER*5 TITLE(JP_FATAL+1)
      DATA TITLE/'DEBUG',
     X           'INFO ',
     X           'WARN ',
     X           'ERROR',
     X           'FATAL'/
      INTEGER ILEN

#ifdef MPI_DEBUG
      INTEGER my_id, ierr
      Data my_id /-1/
      Save my_id
#endif

C
C ------------------------------------------------------------------
C*    Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
C
C     Ensure valid level is used.
      NLEV = KLEVEL + 1
      IF ( KLEVEL .GT. JP_FATAL) NLEV = JP_FATAL + 1
C
      DO LOOP = 1, 120
        NEWMESS(LOOP:LOOP) = ' '
      ENDDO
C
C ------------------------------------------------------------------
C*    Section 2.   Prepare ERROR or FATAL message for MARS server.
C ------------------------------------------------------------------
C
  200 CONTINUE
C
      ILEN = LEN(MESSAGE)
      IF( ILEN.GT.105 ) ILEN = 105

#ifdef MPI_DEBUG
      if (my_id.eq.-1) Then
      call MPI_COMM_RANK (MPI_COMM_WORLD, my_id, ierr)
      my_id = my_id + 1
      end if
#endif

      NEWMESS(1:ILEN) = MESSAGE(1:ILEN)
      ILEN = ILEN + 1
      IF(KNUM.NE.JPQUIET) THEN
        IF( ABS(KNUM).LT.1000 ) THEN
          WRITE(NEWMESS(ILEN:),'(I4)') KNUM
        ELSE IF ( ABS(KNUM).LT.100000 ) THEN
          WRITE(NEWMESS(ILEN:),'(I7)') KNUM
        ELSE IF ( ABS(KNUM).LT.100000000 ) THEN
          WRITE(NEWMESS(ILEN:),'(I10)') KNUM
        ELSE
          WRITE(NEWMESS(ILEN:),'(I15)') KNUM
        ENDIF
      ENDIF

C
C     Send the message
C
      IF( KLEVEL.GE.JP_WARN ) CALL INTLOGT(NEWMESS)
C
#ifdef MPI_DEBUG
      IF( LDEBUG ) WRITE(*,9001) my_id,TITLE(NLEV),NEWMESS
 9001 FORMAT(i4,' INTLOG ',A5,': ',A120)
#else
      IF( LDEBUG ) WRITE(*,9001) TITLE(NLEV),NEWMESS
 9001 FORMAT('INTLOG ',A5,': ',A120)

#endif
C
C ------------------------------------------------------------------
C*    Section 9.   Closedown.
C ------------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END
