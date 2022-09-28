C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION LSM_RED( KGAUSS, KDATE, KBITS, HPATH )
C
C---->
C**** LSM_RED
C
C     PURPOSE
C     -------
C
C     Generates the land sea mask file pathname.
C
C
C     INTERFACE
C     ---------
C
C     IRET = LSM_RED( KGAUSS, KDATE, KBITS, HPATH )
C
C
C     Input
C     -----
C     KGAUSS - The gaussian number (80 or 160)
C     KDATE  - The data date in YYYYMMDD format.
C     KBITS  - Number of bits per land-sea mask value (eg 32 or 64)
C
C     Output
C     ------
C     HPATH  - The full file pathname (with gaussian number
C              number of bits and date).
C
C
C     Return value
C     ------------
C
C     Function returns the number of characters in the file pathname,
C     or zero if no match found.
C
C
C     Common block usage
C     ------------------
C
C     None
C
C
C     EXTERNALS
C     ---------
C
C     GETENV     - Standard routine to get environmental variable.
C     INDEX      - Intrinsic routine to find position of substring.
C     EMOSNUM    - Gives current EMOSLIB version number.
C
C
C     METHOD
C     ------
C     None
C
C
C     REFERENCE
C     ---------
C
C     None
C
C     COMMENTS
C     --------
C
C     None
C
C
C     AUTHOR
C     ------
C
C     J.D.Chambers      *ECMWF*      ??? 1996
C
C
C     MODIFICATIONS
C     -------------
C
C     J.D.Chambers      *ECMWF*      July 1998
C     Use dates in YYYYMMDD format to handle year 2000 etc.
C
C----<
C     _______________________________________________________
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER JPD160, JPD80
      PARAMETER (JPD160=6)
      PARAMETER (JPD80=2)
C
C     Function parameters
C
      INTEGER KGAUSS, KDATE, KBITS
      CHARACTER*(*) HPATH
C
C     Local variables
C
      INTEGER LOOP
C
C     External functions
C
      INTEGER DPATH_TABLES_INTERPOL
      EXTERNAL DPATH_TABLES_INTERPOL
C
C     Change dates for N160
C
      INTEGER DATE160(JPD160)
      DATA DATE160/
     X            19790930,
     X            19910917,
     X            19930804,
     X            19940302,
     X            19940823,
     X            19950404
     X           /
C
C     Change dates for N80
C
      INTEGER DATE80(JPD80)
      DATA DATE80/
     X            19790930,
     X            19790930
     X           /
C
      CHARACTER*256 YBASE
      INTEGER IBASELEN

      CHARACTER*50 PATH160(JPD160)
      CHARACTER*50 PATH80(JPD80)
C
C     Pathnames for N160
C
      DATA PATH160/
     X            '_19790930',
     X            '_19910917',
     X            '_19930804',
     X            '_19940302',
     X            '_19940823',
     X            '_19950404'
     X          /
C
C
C     Pathnames for N80
      DATA PATH80/
     X            '_19790930',
     X            '_19790930'
     X          /
C
      INTEGER INDEX
C
C     External functions
C
      INTEGER EMOSNUM
      EXTERNAL EMOSNUM
C
C***************************************************************
C     Section 1. Initialize and check input values.
C***************************************************************
C
  100 CONTINUE
C
      LSM_RED = 0
      HPATH = ' '
C
C     Only reduced N160 gaussian fields handled.
C
      IF( KGAUSS.NE.160 ) GOTO 900
C
C     Only 32 bit and 64 bit land-sea masks handled.
C
      IF( (KBITS.NE.32).AND.(KBITS.NE.64) ) GOTO 900
C
C     Check environment variable for path of land sea masks.
C     (no error is raised, since 0 is a possible resulting value)
C
      IBASELEN = DPATH_TABLES_INTERPOL(YBASE)
C
C***************************************************************
C     Section 2. Build pathnames.
C***************************************************************
C
  200 CONTINUE
C
C     Handle N160
C
      IF( KGAUSS.EQ.160 ) THEN
        DO 210 LOOP = 1, JPD160-1
          IF( (KDATE.GE.DATE160(LOOP) ) .AND.
     X         (KDATE.LT.DATE160(LOOP+1)) ) THEN
            HPATH(1:) = YBASE(1:IBASELEN) //
     X                  'r160_' // 'xx' // PATH160(LOOP)
            LSM_RED = INDEX(HPATH,' ')
            GOTO 900
          ENDIF
  210   CONTINUE
C
C       Dropthrough -> take latest.
C
        HPATH(1:) = YBASE(1:IBASELEN) //
     X              'r160_' // 'xx' // PATH160(JPD160)
        LSM_RED = INDEX(HPATH,' ')
C
C     Handle N80
C
      ELSE
        DO 220 LOOP = 1, JPD80-1
          IF( (KDATE.GE.DATE80(LOOP) ) .AND.
     X         (KDATE.LT.DATE80(LOOP+1)) ) THEN
            HPATH(1:) = YBASE(1:IBASELEN) //
     X                  'r80_' // 'xx' // PATH80(LOOP)
            LSM_RED = INDEX(HPATH,' ')
            GOTO 900
          ENDIF
  220   CONTINUE
C
C       Dropthrough -> take latest.
C
        HPATH(1:) = YBASE(1:IBASELEN) //
     X              'r80_' // 'xx' // PATH80(JPD80)
        LSM_RED = INDEX(HPATH,' ')
      ENDIF
C
C***************************************************************
C     Section 9. Return.
C***************************************************************
C
  900 CONTINUE
C
      RETURN
      END
