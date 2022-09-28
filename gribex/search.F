C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE SEARCH (KEYS,MAXKEY,STRING,LEN,POS,IS,IF,
     C                    ISUP,IP,OP,IA,MIN)
C
C---->
C**** SEARCH - Search for given character string.
C 
C     Purpose.
C     --------
C
C           Search for a match, in an array of character
C           variables, for a given character string.
C 
C**   Interface.
C     ----------
C
C           CALL SEARCH (KEYS,MAXKEY,STRING,LEN,POS,IS,IF,
C    C                    ISUP,IP,OP,IA,MIN)
C
C               Input Parameters.
C               -----------------
C
C                KEYS   = Array of character variables
C 
C                MAXKEY = Size of array
C 
C                STRING = Character string for which match is
C                         sought
C 
C                LEN    = Length of this string
C 
C                ISUP   = 0 , Print turned on, when no match found.
C                         1 , Print suppressed
C 
C                IS     = Search start position in array "KEYS"
C 
C                IF     = Search end position in array KEYS.
C 
C                OP     = Unit number of file output
C 
C                IP     = Unit number of file input
C 
C                IA     = 1 , Interactive use
C                         0 , Batch use.
C 
C                MIN    = Minimum number of characters needed for match.
C 
C               Output Parameters.
C               ------------------
C
C                POS    = 0 , Match not found
C                         -1 , Multiple matches found
C                         Positive value, indicates array element
C                             which matches
C 
C     Author.
C     -------
C
C           J. Hennessy      ECMWF      16.04.85
C
C     Modifications.
C     --------------
C
C           J. Hennessy      ECMWF      07.12.93
C           Code added to remove ambiguous entries when possible.
C
C           J.D.Chambers  ECMWF  13.12.93
C           Increase parameter value table size 1200 -> 1500
C 
C     ----------------------------------------------------------------
C----<
C
      IMPLICIT INTEGER (A-Z)
C
      PARAMETER (ASIZE=1500)
C
      DIMENSION KEYS(MAXKEY)
      CHARACTER*(*) KEYS, STRING
      CHARACTER*1   PAGE
C
      DIMENSION ATAB(ASIZE)
C
      IF (MAXKEY.GT.ASIZE)
     C   THEN
             WRITE (*,*) ' SEARCH : Array limits exceeded.'
             WRITE (*,*) ' SEARCH : MAXKEY = ', MAXKEY
             WRITE (*,*) '          Notify MARS group.'
             CALL ABORT
         ENDIF
C
      SIZE = MAX (LEN,MIN)
C
C
C
C
C
C****************************************************************
C*
C*    Flag and count matching entries
C*
C****************************************************************
C
C
      N = MIN
  100 K = 0
      DO 200 I=IS,IF
         ATAB(I) = 0
         IF (STRING(1:N).EQ.KEYS(I)(1:N))
     C      THEN
                ATAB(I) = 1
                K = K + 1
                J = I
            ENDIF
  200 CONTINUE
C
      N = N + 1
      IF (N.LE.SIZE.AND.K.GT.1) GO TO 100
C
C
C****************************************************************
C*
C*    No match found. print message (if required) and return.
C*
C****************************************************************
C
      IF (K.EQ.0)
     C THEN
           POS = 0
           IF (ISUP.EQ.1) RETURN
C
           IF (STRING(1:1).NE.'?')
     C      THEN
              WRITE (OP,9000) STRING(1:SIZE)
 9000         FORMAT (' Unrecognised entry - ',A)
            ENDIF
           WRITE (OP,*) 'Valid entries are :-'
           N = 0
           DO 300 I=IS,IF
                  N = N + 1
                  WRITE (OP,'(1H ,20X,A)') KEYS(I)
                  IF (IA.EQ.1.AND.N.EQ.19)
     C              THEN
  250                   WRITE(OP,*)'Type page to continue -->'
                        N = 0
                        READ (IP,'(A1)') PAGE
                        CALL L2U1CR (PAGE)
                        IF (PAGE.NE.'P') GO TO 250
                        GO TO 300
                    ENDIF
  300      CONTINUE
           POS = 0
           RETURN
       END IF
C
C****************************************************************
C*
C*    Return if only 1 match found.
C*
C****************************************************************
C
      IF (K.EQ.1) THEN
                      POS = J
                      RETURN
                  ENDIF
C
C****************************************************************
C*
C*    Number of entries found. Try to resolve ambiguity.
C*
C****************************************************************
C
      DO 350 I=IS,IF
           IF (ATAB(I).EQ.1) 
     C        THEN
                  IJ = RTB(KEYS(I))
                  IF (STRING(1:LEN).EQ.KEYS(I)(1:IJ))
     C               THEN
                         POS = I
                         RETURN
                     ENDIF
              ENDIF
  350 CONTINUE
C
C****************************************************************
C*
C*    Ambiguous entry found. Print/display message
C*
C****************************************************************
C
      WRITE (OP,9001) STRING(1:SIZE)
 9001 FORMAT (' Ambiguous entry  - ',A)
      DO 400 I=IS,IF
           IF (ATAB(I).EQ.1) WRITE (OP,9002) KEYS(I)
 9002      FORMAT ('     ',A)
  400 CONTINUE
C
      POS = - 1
      RETURN
      END
