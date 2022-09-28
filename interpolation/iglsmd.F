C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGLSMD(KDATE,KREPR,KGRID,KGAUSS,KAREA,KLN,
     X   HFILE, OSTAND, KOGRID, KOLINE, KNREC, KPR, KERR)
C
C---->
C**** *IGLSMD*
C
C     PURPOSE
C     _______
C
C     Generate the land sea mask file information.
C
C     INTERFACE
C     _________
C
C     IERR = IGLSMD(KDATE,KREPR,KGRID,KGAUSS,KAREA,KLN,
C    X          HFILE, OSTAND, KOGRID, KOLINE, KNREC, KPR, KERR)
C
C     Input parameters
C     ________________
C
C     KDATE      - The field date in YYMMDD format
C
C     KREPR      - The field representation.
C
C     KGRID      - The latitude/longitude field grid definition
C                  (WE, NS) or Gaussian truncation.
C
C     KAREA      - The field area definition (N, W, S, E).
C
C     KLN        - The Northern line number. This is mainly used for
C                  Gaussian grids.
C
C     KPR        - The debug print switch.
C                  0  , No debugging output.
C                  1  , Produce debugging output.
C
C     KERR       - The error control flag.
C                  -ve, No error message. Return error code.
C                  0  , Hard failure with error message.
C                  +ve, Print error message. Return error code.
C
C     Output parameters
C     ________________
C
C     HFILE      - The filename for the land-sea mask.
C
C     OSTAND     - Flag indicating whether the land-sea mask is
C                  a prestored "standard" field.
C
C     KOGRID     - The grid stride (WE, NS) in a pre-stored
C                  "standard" land sea mask file.
C
C     KOLINE     - The offsets (N, W) of the first points in a
C                  pre-stored "standard" land sea mask file.
C
C     KNREC      - The file line length for the input array. This is
C                  used for positioning and transfer purposes.
C
C     Return value
C     ____________
C
C     The status indicator (INTEGER).
C          = 0 if OK,
C          = 25201 if data representation type is not valid.
C          = 25202 if unable to locate lsm file for reduced gaussian
C
C     EXTERNALS
C     _________
C
C     GETENV     - Standard routine to get environmental variable.
C     INTLOG(R)  - Logs messages.
C
C     METHOD
C     ______
C
C     NOTE This routine is highly machine dependent as the path and
C     file names for all land sea mask files are in the text. GETENV
C     is used to allow the specification of an alternate directory
C     for the storage of land sea masks (variable MARS_LSM_PATH).
C
C     The grid definition is compared with the definitions of the
C     pre-stored land sea mask files. If a match is found then the
C     appropriate filename is returned and the stride and offset
C     arrays are set to appropriate values. If no match is found then
C     the name of the basic 10 minute 0-1 land sea mask file is
C     returned.
C
C     AUTHOR
C     ______
C
C     K. Fielding      *ECMWF*      Nov 1993
C
C     MODIFICATIONS
C     _____________
C
C     J.D.Chambers     ECMWF        7 Feb 96
C     Add lookup for reduced gaussian fields
C
C     J.D.Chambers     ECMWF       Nov 98
C     Change default pathnames.
C
C----<
C     _______________________________________________________
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
      IMPLICIT NONE
C
#include "parim.h"
C
C     Arguments
C
      CHARACTER*256 HFILE
      LOGICAL OSTAND
      INTEGER KDATE, KREPR, KGAUSS, KLN, KNREC, KPR, KERR
      INTEGER KGRID(2), KAREA(4), KOGRID(2), KOLINE(2)
C
C     Parameters
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 25200)
      INTEGER JPFILES
      INTEGER JPN80, JPR160, JP0P5LL, JP0P25LL
      PARAMETER (JPFILES = 4)
      PARAMETER (JPN80 = 1)
      PARAMETER (JPR160 = 2)
      PARAMETER (JP0P5LL = 3)
      PARAMETER (JP0P25LL = 4)
C
C     Local variables
C
      CHARACTER*6 YFLAG
      INTEGER IRET
      LOGICAL LDEBUG, L10MIN, L025
      REAL RLSTRIDE
C
      CHARACTER*256 YBASE
      CHARACTER*256 YENVBACK
C
C     List of standard filenames       - YFILES
C     File for 10 minute land sea mask - YLSM10
C
      CHARACTER*40 YFILES (JPFILES), YLSM10
C
      INTEGER IBASELEN, ILSM10LEN
      INTEGER IFILELEN (JPFILES)
C
C     External functions
C
      INTEGER LSM_RED, DPATH_TABLES_INTERPOL
      EXTERNAL LSM_RED, DPATH_TABLES_INTERPOL
C
      DATA YLSM10 / 'lsm_xx_lsm10m01' /
      DATA ILSM10LEN / 15 /
      DATA YFILES /
     X  'lsm_xx_lsmn080',
     X  'lsm_xx_lsmr160',
     X  'lsm_xx_lsm0p5deg',
     X  'lsm_xx_lsm0p25deg'
     X  /
      DATA IFILELEN/ 14, 14, 16, 17 /
C
C     _______________________________________________________
C
C*    Section 1. Initialisation
C     _______________________________________________________
C
  100 CONTINUE
C
C     Force processing with 10min lsm with env variable
      CALL GETENV('LSM_10MIN', YFLAG)
      L10MIN = (YFLAG(1:1).EQ.'1')
      IF( L10MIN ) CALL INTLOG(JP_DEBUG,
     X  'IGLSMD: Forcing processing with 10 min',JPQUIET)

C     Processing with 0.25 Predefined lsm Enable
      CALL GETENV('LSM_025', YFLAG)
      L025 = (YFLAG(1:1).EQ.'1')
      IF( L025 ) CALL INTLOG(JP_DEBUG,
     X 'IGLSMD: Processing with 0.25 Predefined LSM Enabled',JPQUIET)

      IGLSMD = 0
      LDEBUG = ( KPR.GE.1 )
C
      IF( LDEBUG ) THEN
        CALL INTLOG(JP_DEBUG,'IGLSMD: Input parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IGLSMD: Date = ',KDATE)
        CALL INTLOG(JP_DEBUG,'IGLSMD: Grid representation = ',KREPR)
        CALL INTLOG(JP_DEBUG,'IGLSMD: N line number = ', KLN)
C
        IF( (KREPR.EQ.JPGAUSSIAN).OR.
     X      (KREPR.EQ.JPQUASI)   .OR.
     X      (KREPR.EQ.JPQGGROT)  .OR.
     X      (KREPR.EQ.JPFGGROT) ) THEN
          CALL INTLOG(JP_DEBUG,'IGLSMD: Gaussian trunc = ',KGAUSS)
        ELSE IF( (KREPR.EQ.JPREGULAR).OR.(KREPR.EQ.JPREGROT) ) THEN
          CALL INTLOG(JP_DEBUG,'IGLSMD: Grid WE = ',KGRID(1))
          CALL INTLOG(JP_DEBUG,'IGLSMD: Grid NS = ',KGRID(2))
        ENDIF
C
        CALL INTLOG(JP_DEBUG,'IGLSMD: Area North = ', KAREA(1))
        CALL INTLOG(JP_DEBUG,'IGLSMD: Area West  = ', KAREA(2))
        CALL INTLOG(JP_DEBUG,'IGLSMD: Area South = ', KAREA(3))
        CALL INTLOG(JP_DEBUG,'IGLSMD: Area East  = ', KAREA(4))
C
      ENDIF
C
C     _______________________________________________________
C
C*    Section 2. Check environment variable for path of land sea masks.
C     _______________________________________________________
C
  200 CONTINUE
C
      IF ( LDEBUG ) CALL INTLOG(JP_DEBUG,'IGLSMD: Section 2.',JPQUIET)
C
      IBASELEN = DPATH_TABLES_INTERPOL(YENVBACK)
      IF( IBASELEN.EQ.0 ) THEN
        IF( LDEBUG ) CALL INTLOG(JP_ERROR,
     X    'IGLSMD: unable to build LSM directory path.',JPQUIET)
        IGLSMD = JPROUTINE + 3
        GOTO 900
      ENDIF
      YBASE = YENVBACK
C
C     _______________________________________________________
C
C*    Section 3. Standard Gaussian grids - F80
C     _______________________________________________________
C
  300 CONTINUE
C
      IF( (KREPR.EQ.JPGAUSSIAN).OR.
     X    (KREPR.EQ.JPFGGROT) ) THEN
        IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'IGLSMD: Section 3.',JPQUIET)
C
c       ILSTRIDE = JP90 / KGAUSS
        RLSTRIDE = 90./REAL(KGAUSS) * PPMULT
        KOGRID (JPWESTEP) = 1
        KOGRID (JPNSSTEP) = 1
c       KOLINE (JPWEST)  = KAREA (JPWEST) / ILSTRIDE + 1
        KOLINE (JPWEST)  = REAL(KAREA(JPWEST))/RLSTRIDE + 1
        KOLINE (JPNORTH) = KLN
C
        IF( KGAUSS.EQ.80 ) THEN
C
          HFILE = YBASE(1:IBASELEN) // YFILES(JPN80)(1:IFILELEN(JPN80))
          OSTAND = .TRUE.
          KNREC = 320
C
        ELSE
C
          HFILE = YBASE(1:IBASELEN) // YLSM10(1:ILSM10LEN)
          OSTAND = .FALSE.
          KNREC = JPPACK
C
        ENDIF
C
C     _______________________________________________________
C
C*    Section 4. Standard reduced Gaussian grids - N160
C     _______________________________________________________
C
  400 CONTINUE
C
      ELSE IF( KREPR.EQ.JPQUASI ) THEN
        IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'IGLSMD: Section 4.',JPQUIET)
C
        KOGRID(JPWESTEP) = 1
        KOGRID(JPNSSTEP) = 1
        KOLINE(JPWEST)   = 1
        KOLINE(JPNORTH)  = KLN
C
        IF( KGAUSS.EQ.160 ) THEN
          IRET = LSM_RED(KGAUSS,KDATE,32,HFILE)
          IF( IRET.EQ.0 ) THEN
            IF( LDEBUG ) CALL INTLOG(JP_ERROR,
     X        'IGLSMD: LSM_RED unable to locate lsm file.',JPQUIET)
            IGLSMD = JPROUTINE + 2
            GOTO 900
          ENDIF
          OSTAND = .TRUE.
          KNREC = 0
        ELSE
C
          HFILE = YBASE(1:IBASELEN) // YLSM10(1:ILSM10LEN)
          OSTAND = .FALSE.
          KNREC = JPPACK
C
        ENDIF
C
C     _______________________________________________________
C
C*    Section 5. Latitude/longitude grids
C
C                The standard 0.5 degree grid is used for any
C                grid which has a stride which is a multiple of
C                0.5 and North and South limits which are a
C                multiple of 0.5
C     _______________________________________________________
C
  500 CONTINUE
C
      ELSE IF( (KREPR.EQ.JPREGULAR).OR.(KREPR.EQ.JPREGROT) ) THEN
        IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'IGLSMD: Section 5.',JPQUIET)
C
        IF( MOD(KGRID(JPNSSTEP),JP0P5).EQ.0 .AND.
     1      MOD(KGRID(JPWESTEP),JP0P5).EQ.0 .AND.
     2      MOD(KAREA(JPNORTH), JP0P5).EQ.0 .AND.
     3      MOD(KAREA(JPWEST),  JP0P5).EQ.0 .AND.
     4      .NOT.L10MIN ) THEN
C
        HFILE=YBASE(1:IBASELEN)//YFILES(JP0P5LL)(1:IFILELEN(JP0P5LL))
          OSTAND = .TRUE.
          KNREC = 720
C
          KOGRID(JPWESTEP) = KGRID(JPWESTEP) / JP0P5
          KOGRID(JPNSSTEP) = KGRID(JPNSSTEP) / JP0P5
          KOLINE(JPWEST)   = KAREA(JPWEST) / JP0P5 + 1
          KOLINE(JPNORTH)  = (JP90 - KAREA(JPNORTH) ) / JP0P5 + 1
C
        ELSE
C
          HFILE = YBASE(1:IBASELEN) // YLSM10(1:ILSM10LEN)
          OSTAND = .FALSE.
          KNREC = JPPACK
C
          KOGRID (JPWESTEP) = 1
          KOGRID (JPNSSTEP) = 1
          KOLINE (JPWEST) = 1
          KOLINE (JPNORTH) = KLN
C
        ENDIF
      ELSE
        IGLSMD = JPROUTINE + 1
        IF( KERR.GE.0 ) CALL INTLOG(JP_ERROR,
     X     'IGLSMD: Invalid data representation = ',KREPR)
C
        IF( KERR.EQ.0 ) CALL INTLOG(JP_FATAL,
     X     'IGLSMD: Interpolation failed.',IGLSMD)
C       GOTO 900
      ENDIF

      IF( L025 ) THEN
        IF( KGRID(JPNSSTEP).EQ.JP0P25.AND.
     1      KGRID(JPWESTEP).EQ.JP0P25.AND.
     2      MOD(KAREA(JPNORTH), JP0P25).EQ.0 .AND.
     3      MOD(KAREA(JPWEST),  JP0P25).EQ.0 .AND.
     4      .NOT.L10MIN ) THEN
C
        HFILE=YBASE(1:IBASELEN)//YFILES(JP0P25LL)(1:IFILELEN(JP0P25LL))
          OSTAND = .TRUE.
          KNREC = 1440
C
            KOGRID(JPWESTEP) = 1
            KOGRID(JPNSSTEP) = 1
            KOLINE(JPWEST)   = KAREA(JPWEST) / JP0P25 + 1
            KOLINE(JPNORTH)  = (JP90 - KAREA(JPNORTH) ) / JP0P25 + 1
        ENDIF
      ENDIF
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine. Format statements
C     _______________________________________________________
C
  900 CONTINUE
C
      IF( LDEBUG ) CALL INTLOG(JP_DEBUG,'IGLSMD: Section 9.',JPQUIET)
C
      RETURN
      END
