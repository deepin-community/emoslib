C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IRLSMB (OISTAND, HIFILE, KIREC, KIGRID, KILINE,
     X   OOSTAND, HOFILE, KOREC, KOGRID, KOLINE, KPR, KERR)
C
C---->
C**** *IRLSMB*
C
C     Purpose
C     -------
C
C     Calculate the effects of the land-sea masks on the unnormalised
C     interpolation weights for a quasi regular input field and a
C     regular output field.
C
C
C     Interface
C     ---------
C
C     IERR = IRLSMB (OISTAND, HIFILE, KIREC, KIGRID, KILINE,
C    X   OOSTAND, HOFILE, KOREC, KOGRID, KOLINE, KPR, KERR)
C
C
C     Input parameters
C     ----------------
C
C     OISTAND - Flag indicating whether the input land-sea mask
C               is a pre-stored "standard" field.
C
C     HIFILE  - The filename for the input land-sea mask.
C
C     KIREC   - The length of one latitude record in the input file.
C
C     KIGRID  - An array of length 2 giving the row and column
C               strides in a pre-stored land sea mask file for the
C               input grid.
C
C     KILINE  - An array of length 2 giving the offsets of the
C               Northern and Western starting points in a
C               pre-stored land sea mask file for the input grid.
C
C     OOSTAND - Flag indicating whether the output land-sea mask
C               is a prestored "standard" field.
C
C     HOFILE  - The filename for the output land-sea mask.
C
C     KOREC   - The length of one latitude record in the output file.
C
C     KOGRID  - An array of length 2 giving the row and column
C               strides in a pre-stored land sea mask file for the
C               output grid.
C
C     KOLINE  - An array of length 2 giving the offsets of the
C               Northern and Western starting points in a
C               pre-stored land sea mask file for the output grid.
C
C     KPR     - The debug print switch.
C               0  , No debugging output.
C               1  , Produce debugging output.
C
C     KERR    - The error control flag.
C               -ve, No error message. Return error code.
C               0  , Hard failure with error message.
C               +ve, Print error message. Return error code.
C
C
C     Output parameters
C     -----------------
C
C     The common variable WFACT is modified by this routine.
C
C     An error indicator
C
C     23401 An error exit was returned from the I/O routine PBOPEN.
C     23402 An error exit was returned from the I/O routine PBCLOSE.
C
C
C     Common block usage
C     ------------------
C
C     nifld.common - This file contains all the input field
C                    definition variables.
C
C     NINS         - Number of grid points in NS direction for input
C                    field (used in grspace.h).
C     NIWE         - Number of grid points in WE direction for input
C                    field (used in grspace.h).
C
C     nofld.common - This file contains all the output field
C                    definition variables.
C
C     NONS         - Number of grid points in NS direction for output
C                    field is used.
C     NOWE         - Number of grid points in WE direction for output
C                    field is used.
C
C     grspace.h    - This file contains all the work space array
C                    definitions for grid point to grid point
C                    interpolation.
C
C     MEXPAND      - Array used to expand one latitude line of the
C                    10 minute land sea mask file to have one word
C                    per bit for improved efficiency.
C     MILLEN       - Array containing a quasi regular line length
C                    definition.
C     MWORK        - Array used to read one latitude line of a
C                    standard land sea mask file.
C
C     MILATG       - The input field latitudes.
C     MILONG       - The input field longitudes.
C     MILSM        - Work array used in calculating the effects of
C                    the land sea mask on interpolation.
C     MISTRT       - The array offsets of the start of each latitude
C                    line for a quasi regular Gaussian input field.
C     MNSIND       - The latitude line numbers (array offset) of the
C                    input field associated with each line of
C                    latitude in the output field.
C     MOLATG       - The output field latitudes.
C     MOLONG       - The output field longitudes.
C     MOLSM        - Work array used in calculating the effects of
C                    the land sea mask on interpolation.
C     MWEIND       - This array holds the longitude points (array
C                    offset) from the input field associated with
C                    each longitude point in the output field.
C     WFACT        - The interpolation weights for each point in the
C                    output field.
C
C
C     Externals
C     ---------
C
C     IGLSM01   - Read and process one latitude row from the 10
C                 minute land sea mask.
C     IGLSMR    - Calculate the effects of the land-sea masks for
C                 one line of latitude on the unnormalised
C                 interpolation weights.
C     IGLSMST   - Read and process one line of latitude from a
C                 standard land sea mask file.
C     PBCLOSE   - Close a land sea mask file after processing.
C     PBOPEN    - Open a land sea mask file for processing.
C     INTLOG(R) - Log messages.
C     JINDEX    - Returns length of character string
C
C
C     Method
C     ------
C
C     All the land sea mask data is in (0-1) form. If a point in the
C     input field has a different land sea mask value from the
C     corresponding point in the output field then its interpolating
C     weight is multiplied by a scaling factor. This scaling factor
C     is currently 0.2 so that the effect of such points in the final
C     interpolation is reduced.
C
C     The land-sea mask files are kept open between calls to this
C     routine.
C
C
C     Reference
C     ---------
C
C     None
C
C
C     Comments
C     --------
C
C     None
C
C
C     Author
C     ------
C     K. Fielding      *ECMWF*      Nov 1993
C
C     Modifications
C     -------------
C
C     J.D.Chambers      ECMWF       Oct 1996
C     Reduced number of parameters in call to IGLSMST and IGLSM01
C
C----<
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------|
C
      IMPLICIT NONE
C
#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "grspace.h"
C
C     Function arguments
C
      LOGICAL OISTAND, OOSTAND
      CHARACTER *(*) HIFILE, HOFILE
      INTEGER KIREC, KOREC, KPR, KERR
      INTEGER KIGRID (2), KILINE (2), KOGRID (2), KOLINE (2)
C
C     Local variables
C
      INTEGER IIUNIT, IOUNIT, IIFILE, IOFILE
      INTEGER ILATN, ILATS, ILINEN, ILINES, ISTRIDEN, ISTRIDES
      INTEGER IOSTRIDE, INDEXN, INDEXS, IZERO, IOFF, IPR, IERR
      INTEGER JLON, JOLAT
      CHARACTER*256 XHIFILE, XHOFILE
      CHARACTER*2 NEWFILE, MSKFILE
      INTEGER XIIUNIT, XIOUNIT, II
      DATA XIIUNIT/0/
      DATA XIOUNIT/0/
      SAVE  XHIFILE, XHOFILE, XIIUNIT, XIOUNIT
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 23400)
C
C     External functions
C
      INTEGER IGLSMR, IGLSMST, IGLSM01, JINDEX
C
C     -----------------------------------------------------------------|
C*    Section 1. Initialisation
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IRLSMB: Section 1.',JPQUIET)
C
      IRLSMB = 0
C
      IIFILE = JINDEX(HIFILE)
      IOFILE = JINDEX(HOFILE)
C
      IF( KPR.GE.1 ) THEN
        CALL INTLOG(JP_DEBUG,'IRLSMB: Input field parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IRLSMB: LSM filename is.',JPQUIET)
        CALL INTLOG(JP_DEBUG, HIFILE(1:IIFILE), JPQUIET)
        IF( OISTAND ) THEN
            CALL INTLOG(JP_DEBUG,'IRLSMB: Standard fld: TRUE',JPQUIET)
        ELSE
            CALL INTLOG(JP_DEBUG,'IRLSMB: Standard fld: FALSE',JPQUIET)
        ENDIF
        CALL INTLOG(JP_DEBUG,'IRLSMB: LSM file rec len = ',KIREC)
        CALL INTLOG(JP_DEBUG,'IRLSMB: Grid stride WE = ',KIGRID(1))
        CALL INTLOG(JP_DEBUG,'IRLSMB: Grid stride NS = ',KIGRID(2))
        CALL INTLOG(JP_DEBUG,'IRLSMB: Grid start N = ',KILINE(1))
        CALL INTLOG(JP_DEBUG,'IRLSMB: Grid start W = ',KILINE(2))
C
        CALL INTLOG(JP_DEBUG,'IRLSMB: Output field parameters.',JPQUIET)
        CALL INTLOG(JP_DEBUG, HOFILE(1:IOFILE),JPQUIET)
        IF( OOSTAND ) THEN
            CALL INTLOG(JP_DEBUG,'IRLSMB: Standard fld: TRUE',JPQUIET)
        ELSE
            CALL INTLOG(JP_DEBUG,'IRLSMB: Standard fld: FALSE',JPQUIET)
        ENDIF
        CALL INTLOG(JP_DEBUG,'IRLSMB: LSM file rec len = ',KOREC)
        CALL INTLOG(JP_DEBUG,'IRLSMB: Grid stride WE = ',KOGRID(1))
        CALL INTLOG(JP_DEBUG,'IRLSMB: Grid stride NS = ',KOGRID(2))
        CALL INTLOG(JP_DEBUG,'IRLSMB: Grid start N = ',KOLINE(1))
        CALL INTLOG(JP_DEBUG,'IRLSMB: Grid start W = ',KOLINE(2))
      ENDIF
C
      IF( MOLONG(2).GE.MOLONG(1) ) THEN
         IOSTRIDE = MOLONG(2) - MOLONG(1)
      ELSE
         IOSTRIDE = MOLONG(2) + JP360 - MOLONG(1)
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 2. Open files for input and output land sea masks
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IRLSMB: Section 2.',JPQUIET)
C
C     See if first time through or input land sea mask filename has
C     changed since last access
C
      II = JINDEX(HIFILE)
      IF( XHIFILE(1:II).NE.HIFILE(1:II) ) THEN
C
C       Open input land sea mask file
C
        IF(XIIUNIT.NE.0) CALL PBCLOSE(XIIUNIT,IERR)
        CALL PBOPEN(IIUNIT, HIFILE, 'r', IERR)
        IF( IERR.NE.0 ) THEN
          IRLSMB = JPROUTINE + 1
C
          IF( KERR.GE.0 ) THEN
            CALL INTLOG(JP_ERROR,'IRLSMB: PBOPEN return code = ',IERR)
            CALL INTLOG(JP_ERROR,'IRLSMB: trying to open file',JPQUIET)
            CALL INTLOG(JP_ERROR, HIFILE(1:IIFILE),JPQUIET)
          ENDIF
C
          IF( KERR.EQ.0 ) CALL INTLOG(JP_FATAL,
     X      'IRLSMB: Interpolation failed.',JPQUIET)
          GOTO 900
        ENDIF
        XIIUNIT = IIUNIT
        XHIFILE(1:II) = HIFILE(1:II)
        NEWFILE(1:1) = 'Y'
C
C     Just rewind if same file still in use
C
      ELSE
        IIUNIT = XIIUNIT
        NEWFILE(1:1) = 'N'
      ENDIF
C
C     See if first time through or output land sea mask filename has
C     changed since last access
C
      II = JINDEX(HOFILE)
      IF( XHOFILE(1:II).NE.HOFILE(1:II) ) THEN
C
C       Open output land sea mask file
C
        IF(XIOUNIT.NE.0) CALL PBCLOSE(XIOUNIT,IERR)
        CALL PBOPEN(IOUNIT, HOFILE, 'r', IERR)
        IF( IERR.NE.0 ) THEN
          IRLSMB = JPROUTINE + 1
C
          IF( KERR.GE.0 ) THEN
            CALL INTLOG(JP_ERROR,'IRLSMB: PBOPEN return code = ',IERR)
            CALL INTLOG(JP_ERROR,'IRLSMB: trying to open file',JPQUIET)
            CALL INTLOG(JP_ERROR, HOFILE(1:IOFILE),JPQUIET)
          ENDIF
C
          IF( KERR.EQ.0 ) CALL INTLOG(JP_FATAL,
     X      'IRLSMB: Interpolation failed.',JPQUIET)
          GOTO 900
        ENDIF
        XIOUNIT = IOUNIT
        XHOFILE(1:II) = HOFILE(1:II)
        NEWFILE(2:2) = 'Y'
C
C     Just rewind if same file still in use
C
      ELSE
        IOUNIT = XIOUNIT
        NEWFILE(2:2) = 'N'
      ENDIF
C
C     -----------------------------------------------------------------|
C*    Section 3. Calculate arrays of weights
C     -----------------------------------------------------------------|
C
  300 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IRLSMB: Section 3.',JPQUIET)
C
      IPR = KPR
C
      DO JOLAT = 1, NONS
C
        INDEXN = ( (JOLAT - 1) * 2 + JP_I_N - 1) * NOWE + 1
        INDEXS = ( (JOLAT - 1) * 2 + JP_I_S - 1) * NOWE + 1
C
C       Get line for output array
C
        IF( OOSTAND ) THEN
C
          IOFF = (KOLINE(JPNORTH) + (JOLAT - 1) * KOGRID(JPNSSTEP)
     X         - 1) * KOREC
C
          MSKFILE(2:2) = 'O'
          MSKFILE(1:1) = NEWFILE(2:2)
          IERR = IGLSMST(IOUNIT, IOFF, NOWE, KOLINE(JPWEST),
     X         KOGRID(JPWESTEP), KOREC, MOLSM, MSKFILE)
          NEWFILE(2:2) = 'N'
C
        ELSE
          IERR = IGLSM01(IOUNIT, NOWE, MOLONG, MOLATG(JOLAT),
     X         MEXPAND, KOREC, MOLSM)
        ENDIF
C
        IF( IERR.GT.0 ) THEN
          IRLSMB = IERR
          GOTO 900
        ENDIF
C
        ILATN = MNSIND(JP_I_N, JOLAT)
        ILATS = MNSIND(JP_I_S, JOLAT)
C
        ILINEN = MILLEN(ILATN)
        ISTRIDEN = JP360 / ILINEN
C
        ILINES = MILLEN(ILATS)
        ISTRIDES = JP360 / ILINES
C
        IZERO = 0
C
C       Get lines for input array
C
        IF( OISTAND ) THEN
C
          MSKFILE(2:2) = 'I'
          MSKFILE(1:1) = NEWFILE(1:1)
          IERR = IGLSMST(IIUNIT, MISTRT(ILATN) - 1, ILINEN,
     X         KILINE(JPWEST), KIGRID(JPWESTEP), ILINEN,
     X         MILSM(1, JP_I_N), MSKFILE)
          NEWFILE(1:1) = 'N'
C
          IF( IERR.GT.0 ) THEN
            IRLSMB = IERR
            GOTO 900
          ENDIF
C
          MSKFILE(2:2) = 'I'
          MSKFILE(1:1) = NEWFILE(1:1)
          IERR = IGLSMST(IIUNIT, MISTRT(ILATS) - 1, ILINES,
     X         KILINE(JPWEST), KIGRID(JPWESTEP), ILINES,
     X         MILSM(1, JP_I_S), MSKFILE)
          NEWFILE(1:1) = 'N'
C
        ELSE
C
          DO JLON = 0, ILINEN - 1
            MILONG(JLON + 1) = ISTRIDEN * JLON
          ENDDO
C
          IERR = IGLSM01(IIUNIT, ILINEN, MILONG, MILATG(ILATN),
     X         MEXPAND, KIREC, MILSM(1, JP_I_N))
C
          IF( IERR.GT.0 ) THEN
            IRLSMB = IERR
            GOTO 900
          ENDIF
C
          DO JLON = 0, ILINES - 1
            MILONG(JLON + 1) = ISTRIDES * JLON
          ENDDO
C
          IERR = IGLSM01(IIUNIT, ILINES, MILONG, MILATG(ILATS),
     X         MEXPAND, KIREC, MILSM(1, JP_I_S))
C
        ENDIF
C
        IF( IERR.GT.0 ) THEN
          IRLSMB = IERR
          GOTO 900
        ENDIF
C
C       Now modify the unormalised weight for land-sea mask
C
        IERR = IGLSMR(MILSM(1, JP_I_N), MILSM(1, JP_I_S),
     X      MOLSM, MWEIND(1, INDEXN), MWEIND(1, INDEXS), NOWE,
     X      WFACT(1,(JOLAT - 1) * NOWE + 1), IPR, KERR)
C
        IF( IERR.GT.0 ) THEN
          IRLSMB = IERR
          GOTO 900
        ENDIF
C
        IPR = KPR - 1
C
      ENDDO
C
C     -----------------------------------------------------------------|
C*    Section 9. Return to calling routine. Format statements
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IRLSMB: Section 9.',JPQUIET)
C
      RETURN
      END
