C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C


      SUBROUTINE MODVAL   (LENIN,LENOUT,MULT)
C
C---->
C*                                                                    *
C*    NAME      : MODVAL                                              *
C*                                                                    *
C*    FUNCTION  : MAKES ONE VALUE A MULTIPLE OF ANOTHER.              *
C*                                                                    *
C*    INPUT     : LENIN = INPUT  VALUE                                *
C*                MULT  = MODULO VALUE                                *
C*                                                                    *
C*    OUTPUT    : LENOUT= OUTPUT VALUE                                *
C*                                                                    *
C*    GENERAL   : MODVAL CALLS  MOD                                   *
C*                              ABS                                   *
C*                                                                    *
C*    AUTHOR    : B.V. GURETZKY    V.E. DAY                           *
C*                                                                    *
C*    MODIFIED  : J. HENNESSY      25.11.85                           *
C*                                                                    *
C*                                                                    *
C     ---------------------------------------------------------------
C----<
C
      LENG  = ABS (MULT)
      INTER = MOD (ABS(LENIN),LENG)
C
      IF (INTER.NE.0)
     C   THEN
             LENG = LENG - INTER
             IF (LENIN.LT.0)
     C           THEN
                     LENOUT = LENIN - LENG
                 ELSE
                     LENOUT = LENIN + LENG
                 ENDIF
         ELSE
             LENOUT = LENIN
         ENDIF
C
C
      RETURN
      END
