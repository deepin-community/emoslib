C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION PDDEFS()
C
C---->
C**   PDDEFS
C
C     Purpose
C     -------
C
C     Adjust the interpolation parameters according to parameter
C     dependent requirements.
C
C     Interface
C     ---------
C
C     IRET = PDDEFS()
C
C     Input
C     -----
C
C     Input file format:
C
C       Param  lsm  wind  prec  lsm interp          ) First 2 lines
C       -----  ---  ----  ----  ----------          ) are ignored
C       131     n     y     n     n            )
C       132     n     y     n     n            )
C       140     y     n     n     n            )
C       141     y     n     n     n            )    I3,4(5X,A1)
C       142     n     n     y     n            )
C       143     n     n     y     n            )
C       144     n     n     y     n            )
C       165     n     y     n     n            )
C        :
C        :
C
C     Output
C     ------
C
C     IRET = 0 if OK.
C
C
C     Method
C     ------
C
C     Values are taken from a (text) file in a directory defined
C     by the environment variable:
C
C        "PARAMETER_PROCESSING_DEFAULTS" (if defined),
C
C     or from the directory:
C
C        /owrk/marsint/new                      (CRAY)
C        /mrfs/postproc                         (Fujitsu)
C        /usr/local/lib/metaps/tables/interpol
C        /usr/local/apps/libemos/tables/        (since version 000394)
C
C     or from an internal default array.
C
C     The file used has name defaults_for_table_nnn, where
C     nnn is the 3-digit local code table number (eg 128 for
C     ECMWF, 001 for WMO, etc ).
C
C
C     Externals
C     ---------
C
C     INTLOG  - Logs messages.
C     GETENV  - Gets environment variable information.
C     INDEX   - Locates a character in a character variable.
C     PRECIP  - Says if field is to have 'precipitation' treatment
C
C
C     Author
C     ------
C
C     J.D.Chambers       ECMWF        August 1994.
C
C----<
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER JPROUTINE, JPNUMDF, JPND001, JPND128, JPND129
      PARAMETER (JPROUTINE = 28000)
      PARAMETER (JPNUMDF = 100)
      PARAMETER (JPND001 =   9)
      PARAMETER (JPND128 =  13)
      PARAMETER (JPND129 =  12)
C
#include "parim.h"
#include "nifld.common"
C
C     Local variables
C
      CHARACTER*256 FILENAME
      CHARACTER*256 HLINE
      INTEGER IPARAM, INEXT, NUMDFS, LOOP, INDX, IOTABLE
      CHARACTER*1 HLSM, HWIND, HPREC, HLSMI
      LOGICAL LNLSM, LNWIND, LNPREC, LNSMPAR
      DATA IOTABLE/0/
C
C     Default array
C
      CHARACTER*27 HDEFS(JPNUMDF)
C
C     External functions
C
      INTEGER DPATH_TABLES_INTERPOL
      EXTERNAL DPATH_TABLES_INTERPOL
C
C     Specified defaults
C
C     WMO table 1
C
      CHARACTER*27 TAB001(JPND001)
      DATA TAB001/
     X          '002     n     n     n     n',
     X          '033     y     y     n     n',
     X          '034     y     y     n     n',
     X          '061     y     n     y     n',
     X          '062     y     n     y     n',
     X          '063     y     n     y     n',
     X          '064     y     n     y     n',
     X          '065     y     n     y     n',
     X          '081     n     n     n     y'
     X         /
C
C     ECMWF table 128
C
      CHARACTER*27 TAB128(JPND128)
      DATA TAB128/
     X          '131     y     y     n     n',
     X          '132     y     y     n     n',
     X          '142     y     n     y     n',
     X          '143     y     n     y     n',
     X          '144     y     n     y     n',
     X          '151     n     n     n     n',
     X          '165     y     y     n     n',
     X          '166     y     y     n     n',
     X          '169     y     n     n     n',
     X          '172     n     n     n     y',
     X          '228     y     n     y     n',
     X          '239     y     n     y     n',
     X          '240     y     n     y     n'
     X         /
C
C     ECMWF table 129
C
      CHARACTER*27 TAB129(JPND129)
      DATA TAB129/
     X          '131     y     y     n     n',
     X          '132     y     y     n     n',
     X          '142     y     n     y     n',
     X          '143     y     n     y     n',
     X          '144     y     n     y     n',
     X          '151     n     n     n     n',
     X          '165     y     y     n     n',
     X          '166     y     y     n     n',
     X          '172     n     n     n     y',
     X          '228     y     n     y     n',
     X          '239     y     n     y     n',
     X          '240     y     n     y     n'
     X         /
C
      SAVE HDEFS, NUMDFS, IOTABLE, FILENAME
C
C     External functions
C
      LOGICAL PRECIP
      EXTERNAL PRECIP
C
C     Statement function
C
      LOGICAL NOTSAME, A, B
C
C     XOR or NE for logicals
C
      NOTSAME(A,B) = ((A).AND.(.NOT.(B))).OR.((B).AND.(.NOT.(A)))
C
C     ------------------------------------------------------------------
C*    Section 1.   Initialise
C     ------------------------------------------------------------------
C
 100  CONTINUE
C
      PDDEFS = 0
C
C     Start with the generic settings.
C
      LNLSM   = .TRUE.
      LNWIND  = .FALSE.
      LNPREC  = .FALSE.
      LNSMPAR = .FALSE.
C
C     ------------------------------------------------------------------
C*    Section 2.   If parameter table has changed, treat as first time
C                  through: open and read the file of default values.
C     ------------------------------------------------------------------
C
 200  CONTINUE
C
      IF( IOTABLE.NE.NITABLE ) THEN
C
        FILENAME(:) = ' '
C
        CALL INTLOG(JP_DEBUG,
     X   'PDDEFS: Try to get processing defaults file.',JPQUIET)
C
C       Get the directory name
C
        CALL GETENV( 'PARAMETER_PROCESSING_DEFAULTS', FILENAME)
        IF( FILENAME(1:1).EQ.' ' ) THEN
          INDX = DPATH_TABLES_INTERPOL(FILENAME)
          IF( INDX.EQ.0 ) THEN
            CALL INTLOG(JP_ERROR,
     X        'PDDEFS: unable to build LSM directory path.',JPQUIET)
            PDDEFS = JPROUTINE + 2
            GOTO 900
          ENDIF
        ENDIF
C
C       Build the complete file pathname
C
        INDX = INDEX(FILENAME, ' ')
        FILENAME(INDX:) = 'defaults_for_table_'
        INDX = INDEX(FILENAME, ' ')
        WRITE(FILENAME(INDX:),'(I3.3)') NITABLE
        INDX = INDX + 2
        CALL INTLOG(JP_DEBUG, FILENAME(1:INDX), JPQUIET)
        IOTABLE = NITABLE
C
        OPEN( UNIT = 1,
     X        FILE = FILENAME(1:INDX),
     X        STATUS = 'OLD',
     X        FORM = 'FORMATTED',
     X        ERR = 300)
C
C       Skip first 2 lines in the file
C
        READ(1,'(A)', END = 900) HLINE
        READ(1,'(A)', END = 900) HLINE
C
C       Read the file into the defaults array
C
        NUMDFS = 1
        DO LOOP = 1, JPNUMDF
          READ(1,'(A)', END = 220) HDEFS(NUMDFS)
          NUMDFS = NUMDFS + 1
        ENDDO
C
  220   CONTINUE
C
C       Close the file.
C
        NUMDFS = NUMDFS - 1
        CLOSE(1, ERR = 920)
C
        GOTO 400
C
C     ------------------------------------------------------------------
C*    Section 3.   If file problem, use default arrays.
C     ------------------------------------------------------------------
C
 300    CONTINUE
C
        CALL INTLOG(JP_DEBUG,
     X   'PDDEFS: No parameter processing defaults file found.',JPQUIET)
C
C       Use appropriate table
C           1 = WMO table 1
C         128 = ECMWF local code table 128
C         129 = ECMWF local code table 129
C
        IF( NITABLE.EQ.1 ) THEN
C
          DO LOOP = 1, JPND001
            HDEFS(LOOP) = TAB001(LOOP)
          ENDDO
          NUMDFS = JPND001
C
        ELSE IF( NITABLE.EQ.128 ) THEN
C
          DO LOOP = 1, JPND128
            HDEFS(LOOP) = TAB128(LOOP)
          ENDDO
          NUMDFS = JPND128
C
C
        ELSE IF( NITABLE.EQ.129 ) THEN
C
          DO LOOP = 1, JPND129
            HDEFS(LOOP) = TAB129(LOOP)
          ENDDO
          NUMDFS = JPND129
C
        ELSE
C
C         .. other (unspecified)
C
          NUMDFS = 0
C
        ENDIF
C
      ENDIF
C
C     ------------------------------------------------------------------
C*    Section 4.   Read lines in file to see if current parameter
C                  is mentioned.
C     ------------------------------------------------------------------
C
  400 CONTINUE
C
      CALL INTLOG(JP_DEBUG,'PDDEFS: Table number = ', NITABLE)
      CALL INTLOG(JP_DEBUG,'PDDEFS: Number of definitions = ', NUMDFS)
      CALL INTLOG(JP_DEBUG,'PDDEFS: Parameter number = ', NIPARAM)
      INEXT = 0
C
  410 CONTINUE
C
      INEXT = INEXT + 1
      IF( INEXT.GT.NUMDFS ) GOTO 900
      READ(HDEFS(INEXT), 9000) IPARAM, HLSM, HWIND, HPREC, HLSMI
C
C     If the current parameter, use the values defined in the table.
C
      IF( IPARAM.EQ.NIPARAM ) THEN
C
        IF( HLSM .EQ.'n' ) LNLSM   = .FALSE.
        IF( HWIND.EQ.'y' ) LNWIND  = .TRUE.
        IF( HPREC.EQ.'y' ) LNPREC  = .TRUE.
        IF( HLSMI.EQ.'y' ) LNSMPAR = .TRUE.
        GOTO 900
C
      ENDIF
C
C     Go back for next line in the array
C
      GOTO 410
C
C     ------------------------------------------------------------------
C*     Section 9.   Closedown.
C     ------------------------------------------------------------------
C
 900  CONTINUE
C
C     Only change the value if the user has not already set it.
C
      IF( .NOT. LSMSET ) THEN
        IF( NOTSAME(LNLSM,LSM) ) THEN
          LCHANGE = .TRUE.
          LSMCHNG = .TRUE.
          LSM = LNLSM
        ENDIF
      ENDIF
C
      IF( .NOT. LWINDSET ) THEN
        IF( NOTSAME(LNWIND,LWIND) ) LCHANGE = .TRUE.
        LWIND = LNWIND
      ENDIF
C
      IF( .NOT. LPRECSET ) THEN
        IF( NOTSAME(LNPREC,LPREC) ) LCHANGE = .TRUE.
        LPREC = LNPREC
      ENDIF
C
      IF( .NOT. LSMPARSET ) THEN
        IF( NOTSAME(LNSMPAR,LSMPAR) ) LCHANGE = .TRUE.
        LSMPAR = LNSMPAR
      ENDIF
C
      LPREC = PRECIP()
C
      RETURN
C
 920  CONTINUE
C
      PDDEFS = JPROUTINE + 1
      CALL INTLOG(JP_ERROR,'PDDEFS: Error closing file:',JPQUIET)
      CALL INTLOG(JP_ERROR,FILENAME,JPQUIET)
      RETURN
C
9000  FORMAT( I3,4(5X,A1))
9001  FORMAT( 1X,I3,4(5X,A1))
C
      END
