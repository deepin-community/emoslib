C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      SUBROUTINE GETSYS (HSYS)
      IMPLICIT LOGICAL     ( L , O , G )
      IMPLICIT CHARACTER*8 ( C , H , Y )
      IMPLICIT INTEGER     ( I , J , K , M , N )
C
C---->
C*    MARS common areas containing job environment information,
C     parameter settings etc.
C
C     NFTNOS - 12 Fortran unit numbers used by MARS.
C              NFTNOS(1) is MARSLOG file. Cracked MARS directives.
C              NFTNOS(2) is MARSPAR file. MARS directives in
C                        network transmission format.
C              NFTNOS(5) is unit 5 , standard input.
C              NFTNOS(6) is unit 6 , standard output.
C              NFTNOS(7) is MARSDEF or MARSDEFNEXT, language
C                        definition, operational or test.
C              Set by routine JENVXX.
C     NUMREQ - Total number of requests.
C     NUMFIL - Numbers of different files to be accessed.
C              NUMFIL(1) , Total number of files to accessed.
C              NUMFIL(2) , Number of IBM disk files to accessed.
C              NUMFIL(3) , Number of IBM MSS files to be accessed.
C              NUMFIL(4) , Number of IBM tape files to be accessed.
C              NUMFIL(5) , Number of worker machine files to accessed.
C     NCNDAY - Century day.
C              Set by routine JENVXX.
C     NJMODE - Mode in which MARS job is running.
C              -1, Quit issued.
C              0 , Checkout directives only.
C              1 , Connected mode (normal mode retains connection
C                  with IBM at all times).
C              2 , Disconnected mode (send requests to IBM and
C                  terminate).
C                  Not yet implemented.
C     NJOBSQ - Unique number identifying job or process.
C              Set by routine JENVXX.
C     NUMBIT - Number of bits in computer word.
C              Set by routine JENVXX.
C     NDBUGS - Debug print switch for IBM.
C              0 , No debug printout.     .
C              8 , Debug print on IBM.
C              Set by routine JENVXX.
C     NDBUGL - Debug level indicator for worker machine.
C              0 , No debug print.
C              4 , Trace through subroutine sections and
C                  values of parameters on entry as well as
C                  other print statements used when actually
C                  debugging.
C              Set by routine JENVXX.
C     NJBCAT - Category assigned to job by MARS.
C              0 , Archive request.
C              1 , Manage request.
C              2 , Checkout request.
C              3 , Costonly request.
C              4 , Retrieval from online Operational database on the
C                  worker machine.
C              5 , Retrieve guaranteed online (on IBM ) data.
C              6 , Retrieve online (on IBM ) data.
C                  Not yet implemented. Treated as category 7.
C              7 , Retrieve off-line (on IBM ) data.
C              8 , Retrieval of research department data from
C                  online Research database on worker machine.
C     NPRITY - Priority assigned to job by MARS.
C              1 , High.
C              2 , Normal.
C              3 , Low.
C     NIAORB - 0 , Current job is an active mode job.
C              1 , Current job is in interactive edit mode.
C              Set by routine JENVXX.
C
C----<
      INTEGER NFTNOS
      INTEGER NCNDAY
      INTEGER NJMODE
      INTEGER NJOBSQ
      INTEGER NUMBIT
      INTEGER NDBUGS
      INTEGER NDBUGL
      INTEGER NJBCAT
      INTEGER NPRITY
      INTEGER NIAORB
      INTEGER NFLTYP
      INTEGER NUMFIL
      INTEGER NUMREQ
C
      DIMENSION NFTNOS(12)
      DIMENSION NUMFIL(5)
C
      COMMON /INTGCOM/ NFTNOS , NCNDAY , NJMODE , NJOBSQ , NUMBIT ,
     C                 NDBUGS , NDBUGL , NJBCAT , NPRITY , NIAORB ,
     C                 NFLTYP , NUMFIL , NUMREQ
C
C     COPSYS - Name of computer operating system.
C              UNICOS
C              COS
C              NOS/VE
C              VMS
C              MVS/XA
C              Set by routine calling MARSXX.
C     CWMACH - Name of machine on which MARS is running.
C              CRAY
C              CYBER
C              VAX
C              IBM
C              Set by routine JENVXX.
C     CCDATE - Date at start of job in yymmdd format.
C              Set by routine JENVXX.
C     CCTIME - Time at start of job in hhmmss format.
C              Set by routine JENVXX.
C     CJNAME - Name of job or process.
C              Set by routine JENVXX.
C     CACCNT - Account under which job is running.
C     CWMARS - Connect to test or operational MARS/IBM.
C              MATS , Test version.
C              MARS , Operational version.
C              Set by routine JENVXX.
C     CWLANG - Use test or operational version of MARS language.
C              MARSDEF - Operational version.
C              MATSDEF - Test version.
C              Set by routine JENVCO for COS version.
C     CWLAPA - Use test or operational version of MARS language.
C              .../.../marsdef - Operational version.
C              .../.../matsdef - Test version.
C              Set by routine JENVUN for UNICOS version.
C     CWTMDR - Temporary file directory.
C              Set by JENVUN.
C     CWUSER - User name.
C              Set by JENVUN.
C     CUSFDB - FDB Usage indicator.
C              NEVER , Never attempt to access FDB.
C              ALWAYS, Always attempt to access FDB.
C              DEFAULT, Analyse request before deciding usage.
C              Set by JENVUN.
C     CWEDNO - GRIB Code Edition indicator.
C              OFF , use current edition.
C              ON  , use edition 0.
C
      CHARACTER*8   COPSYS
      CHARACTER*8   CWMACH
      CHARACTER*3   CWUSER
      CHARACTER*6   CCDATE
      CHARACTER*6   CCTIME
      CHARACTER*4   CWMARS
      CHARACTER*7   CWLANG
      CHARACTER*7   CUSFDB
      CHARACTER*48  CWLAPA
      CHARACTER*48  CWTMDR
      CHARACTER*16  CJNAME
      CHARACTER*16  CACCNT
      CHARACTER*3   CWEDNO
C
      COMMON /CHARCOM/ COPSYS , CWMACH , CCDATE , CCTIME , CJNAME ,
     C                 CACCNT , CWMARS , CWLANG , CWLAPA , CWTMDR ,
     C                 CWUSER , CUSFDB , CWEDNO
C
      CHARACTER*6 HSYS
      HSYS = COPSYS(1:6)
      RETURN
      END
