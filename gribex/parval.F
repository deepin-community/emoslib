C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C


      SUBROUTINE PARVAL (KCSP,KNSP,HLINE,HPAR,KLPAR,HVAL,KLVAL)
C
C---->
C**** PARVAL - Extract next parameter and value pair.
C
C     Purpose.
C     --------
C
C         Get next parameter and value from line.
C
C**   Interface.
C     ----------
C
C         CALL PARVAL (KCSP,KNSP,HLINE,HPAR,KLPAR,HVAL,KLVAL)
C
C         INTEGER     K.
C         REAL        P.
C         LOGICAL     O.
C         CHARACTER   H.
C
C             Input Parameters.
C             -----------------
C
C                KCSP  = Current search start position
C                HLINE = Line being examined
C
C             Output Parameters.
C             -----------------
C
C                KNSP   = Next search position (for next call)
C                         It is set to zero if the last set of
C                         PARAMETER=VALUE pairs is found, and
C                         to -1 if further parameters and values
C                         are to be read from a continuation line.
C
C                KLPAR  = Length of parameter string
C
C                HPAR   = Parameter
C
C                KLVAL  = Length of value string
C
C                HVAL   = Value
C
C     Method.
C     -------
C
C          The separators / , = and " are found to identify
C          the parameters and values.
C
C     Externals.
C     ----------
C
C          RTB
C          REMSP
C          DELSP
C          REPCHR
C
C     Reference.
C     ----------
C
C          MARS User Guide for information on parameters and
C          value pairs.
C
C     Comments.
C     ---------
C
C          Routine contains sections 0-4 and section 9.
C
C     Author.
C     -------
C
C          J. Hennessy 16.04.85
C
C     Modifications.
C     --------------
C
C          J. Hennessy 08.04.86
C
C                "     15.09.86
C
C          J. Hennessy 03.07.90
C          Character variable HVAL made to agree in length with
C          that in calling routine.
C
C          J. Hennessy 23.11.90
C          Allow COMMENT values to include commas.
C
C          J. Hennessy 18.02.91
C          Adjust YLINE length to match HLINE. Allow for / in error
C          at start of list of parameters.
C
C          J. Hennessy 15.10.92
C          Check on input string length added.
C
C----<
C     ---------------------------------------------------------------
C
C
C*    Section 0 . Definition of variables.
C     ---------------------------------------------------------------
C
      IMPLICIT LOGICAL     ( L, O, G )
      IMPLICIT CHARACTER*8 ( C, H, Y )
      IMPLICIT INTEGER     ( I, J, K, M, N )
C
      PARAMETER (JPMAX=960)
C
      INTEGER RTB
C
      CHARACTER*(*) HLINE
      CHARACTER*8   HPAR
      CHARACTER*(*) HVAL
      CHARACTER*(JPMAX) YLINE
C
C     ---------------------------------------------------------------
C
C
C*    Section 1 . Set initial values.
C     ---------------------------------------------------------------
C
 1000 CONTINUE
C
C     Check length of input string.
C
      ILEN = LEN (HLINE)
      IF (ILEN.GT.JPMAX)
     C   THEN
             WRITE (*,9001) ILEN , JPMAX
             CALL ABORTX ('PARVAL')
         ENDIF
C
C     Set parameter and value to 'space' and their lengths to 0.
C
      HPAR  = ' '
      HVAL  = ' '
      KLPAR = 0
      KLVAL = 0
C
C
C
C*    Section 2 . Handle request from HELP facility.
C     ---------------------------------------------------------------
C
 2000 CONTINUE
C
C     First check if help is being requested by use of ?.
C
      IF (HLINE(KCSP:KCSP).EQ.'?')
     C     THEN
               HPAR  = '?'
               KLPAR = 1
               KNSP  = KCSP + 1
               GO TO 9000
           ENDIF
C
C*     Go to section 9.
C
C     ---------------------------------------------------------------
C
C
C
C*    Section 3 . Tidy up input format a little.
C     ---------------------------------------------------------------
C
 3000 CONTINUE
C
C     Change any ' characters to " characters.
C
c--      CALL REPCHR (HLINE,'''','"')
C
C     Remove any spaces before or after / separators.
C
c--      CALL DELSP (HLINE)
C
C     ---------------------------------------------------------------
C
C
C
C*    Section 4 . Extract next parameter and its value.
C     ---------------------------------------------------------------
C
 4000 CONTINUE
C
C     Look for next '=' and ',' in line. Examine
C     the last character in the line. If this is a ',' a
C     continuation line will follow. Otherwise the last
C     PARAMETER=VALUE pair for the current command has been
C     read.
C
      II = INDEX (HLINE(KCSP:),'=')
      IJ = INDEX (HLINE(KCSP:),',')
      IK = RTB(HLINE)
      KNSP = 0
      IF (HLINE(IK:IK).EQ.',') KNSP = -1
C
      IF (II.EQ.0.AND.IJ.EQ.0)
C
C*    No = or , found. Go to section 9.
C
     C    THEN
C
C             Check that no entry with missing = has been made.
C             Set parameter and value to unrecognised string,
C
              IF (IK.GT.KCSP)
     C        THEN
                  HPAR = HLINE(KCSP:IK)
                  CALL REMSP (HPAR)
                  IF (HPAR(1:1).NE.' ')
     C                      THEN
                                HPAR = HLINE(KCSP:IK)
                                KLPAR = IK - KCSP + 1
                                HVAL = HLINE(KCSP:IK)
                                KLVAL = IK - KCSP + 1
                                KNSP = IK + 1
                            ENDIF
              ENDIF
C
              GO TO 9000
          ENDIF
C
      IF (II.EQ.0.AND.IJ.NE.0)
C
C*    , found but no = found. Check that no erroneous entry
C     has been made. Go to section 9.
C
     C     THEN
C
C              Check that no erroneous entry with missing =
C              exists.
C
               HPAR = HLINE(KCSP:KCSP+IJ-2)
               CALL REMSP (HPAR)
               IF (HPAR(1:1).NE.' ')
     C             THEN
                       KLPAR = RTB(HPAR)
                       HVAL = HPAR
                       KLVAL = KLPAR
                       KNSP = KCSP + IJ- 1
                   ENDIF
               GO TO 9000
           ENDIF
C
C     Extract parameter entry and its length.
C
      KLPAR = II - 1
      HPAR = HLINE(KCSP:KCSP+KLPAR-1)
C
C     Remove any leading/trailing spaces.
C
      IF (HPAR(1:1).EQ.' ') THEN
                               CALL REMSP(HPAR)
                               KLPAR = RTB (HPAR)
                           ENDIF
C
C     Find end of value entry. This can be a comma, space or
C     a full stop.
C     If none is found the remaining characters are taken as
C     the value, and treated as the last entry for this command.
C
      KCSP = KCSP + II
C
C     Remove any spaces between = and value.
C     Remove any / put at the start of a list of values.
C
      YLINE(1:) = HLINE(KCSP:)
      IF (YLINE(1:1).EQ.'/') YLINE(1:1) = ' '
      CALL REMSP (YLINE)
      HLINE (KCSP:) = YLINE
C
      II = INDEX(HLINE(KCSP:),',')
C
C     If the value is a character string it may contain a ",".
C     Currently only the COMMENT parameter in a CREATE command
C     can validly have an embedded , .
C
      CALL L2U1CR (HPAR)
      IF (HLINE(KCSP:KCSP).EQ.'"'.AND.HPAR(1:7).EQ.'COMMENT')
     C   THEN
             ISTR = INDEX(HLINE(KCSP+1:),'"')
             IF (ISTR.GE.II) II = ISTR + 2
         ENDIF
C
C     Extract value entry and length
C
      IF (II.EQ.0)
     C    THEN
              II = RTB (HLINE)
              IF (HLINE(II:II).EQ.'.') II = II - 1
              KLVAL = II - KCSP + 1
          ELSE
              KLVAL = II - 1
          ENDIF
C
C     Handle missing value (eg date=, entered) where length is 0.
C
      KNSP = KCSP + KLVAL + 1
C
      IF (KLVAL.GT.0)
     C   THEN
             HVAL = HLINE(KCSP:KCSP+KLVAL-1)
         ELSE
             HVAL = ' '
             KLVAL = 1
         ENDIF
C
C     ---------------------------------------------------------------
C
C
C
C*    Section 9. Return to calling routine.
C     ---------------------------------------------------------------
C
 9000 CONTINUE
C
 9001 FORMAT (1H ,'PARVAL : String length is ',I3,'. Maximum',
     C            ' allowed is ',I3,'.')
      RETURN
C
      END
