C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C


      SUBROUTINE DECOGB (FPDATA,ILENF,IBITS,NBIT,IB1PAR,
     C              IB2PAR,FVERT,ILENV,IGRIB,ILENG,IWORD,
     C              JLENV,JLENF,IERR)
C
C---->
C**** DECOGB - Interface using GRIBEX to decode GRIB format data.
C
C     Purpose.
C     --------
C
C           Provides a compatible interface between the old (DECOGB)
C           and new (GRIBEX) GRIB decoding software.
C
C           GRIB Code Edition Number 0 has been in use at ECMWF since
C           the end of 1985. GRIB Code Edition Number 1 came into use
C           with the introduction of the T213 model, which uses a
C           quasi-regular Gaussian grid for surface fields.
C
C           This interface decodes GRIB Code Edition Number 1 in
C           Edition 0 format and converts the quasi-regular Gaussian
C           grid to a regular grid. When this conversion is done, no
C           West/East increment is included in IB2PAR(9). The increment
C           must be calculated ie ZEWINC = 360.0 / IB2PAR(2). For
C           Gaussian N160 data the increment is .5625 and this
C           cannot be accurately represented in the required units
C           of millidegrees for IB2PAR(9).
C
C**   Interface.
C     ---------
C
C           CALL DECOGB (FPDATA,ILENF,IBITS,NBIT,IB1PAR,
C    C              IB2PAR,FVERT,ILENV,IGRIB,ILENG,IWORD,
C    C              JLENV,JLENF,IERR)
C
C               Input Parameters.
C               -----------------
C
C                 NBIT    - Number of bits in computer word
C
C                 IGRIB   - Integer array containing data in grib code.
C                 ILENG   - Length of array IGRIB.
C
C                 FVERT   - Real array to receive vertical coordinate
C                           parameters.
C                 ILENV   - Length of array FVERT.
C
C                 FPDATA  - Real array to receive decoded data values.
C                 ILENF   - Length of array FPDATA.
C
C                           !!!!!  If this is 1 only
C                           the product and grid definition sections
C                           are decoded. !!!!!
C
C                 IB1PAR  - Integer array (dimension 18) to receive
C                           product definition information.
C
C                 IB2PAR  - Integer array (dimension 11) to receive grid
C                           definition information.
C
C               Output Parameters.
C               -----------------
C
C                 Parameters from section 1 , product definition section
C                 ------------------------------------------------------
C
C                 IB1PAR  - Integer array (dimension 18) of product
C                           definition information.
C
C                 Word      Contents
C                 ----      --------
C                   1       Originating centre identifier.
C                   2       Model identification.
C                   3       Grid definition.
C                   4       Flag ( Code Table 1)
C                   5       Parameter identifier ( Code Table 2 ).
C                   6       Type of level (Code Table 3).
C                  7-8      Value(s) of levels (Code Table 3).
C                   9       Year of data
C                  10       Month of data
C                  11       Day of data
C                  12       Hour of data
C                  13       Minute of data
C                  14       Time unit (Code Table 4).
C                  15       Time range one
C                  16       Time range two
C                  17       Time range flag (Code Table 5).
C                  18       Number averaged.
C
C                           Value(s) of level can occupy 2 words.
C                           For a layer the first word defines the top
C                           and the second the bottom of the layer.
C                           For a single level, only the first word is
C                           used.
C
C                 Parameters from section 2 , grid definition section.
C                 -----------------------------------------------
C
C                 IB2PAR - Integer array (dimension 11) containing grid
C                          definition information.
C                          Use varies with data representation type.
C
C                 Word      Latitude/longitude grid
C                 ----      -----------------------
C                  1        Data representation type (Code Table 6).
C                  2        Number of points along a latitude
C                  3        Number of points along a meridian.
C                  4        Latitude of origin (South is negative).
C                  5        Longitude of origin (West is negative).
C                  6        Resolution flag. (Code Table 7)
C                  7        Latitude of extreme point (South negative).
C                  8        Longitude of extreme point (West negative).
C                  9        West/East increment
C                 10        North/South increment
C                 11        Scanning mode flags (Code Table 8)
C
C                 Word      Gaussian grid
C                 ----      -------------
C                 1-9       As for latitude/longitude grid.
C                 10        The number of latitude lines between a pole
C                           and the equator.
C                 11        Scanning mode flags (Code Table 8)
C
C                 Word      Spherical harmonics
C                 ----      -------------------
C                   1       Data representation type (Code Table 6)
C                   2       J - pentagonal resolution parameter
C                   3       K - pentagonal resolution parameter
C                   4       M - pentagonal resolution parameter
C                   5       Representation type (Code Table 9)
C                   6       Representation mode (Code Table 10)
C                 7-11      not used
C
C
C                 Use for other data representation types is
C
C                 FVERT  - Real array of vertical coordinate parameters
C                 JLENV  - Number of values in this array.
C
C                 Used for hybrid levels only.
C
C                 Parameters from section 3 , bit-map definition section
C                 --------------------------------------------------
C
C                 Not handled by this routine.
C
C                 Parameters from section 4 , binary data section.
C                 -------------------------------------------
C
C                 FPDATA  - Array of floating point values.
C                 JLENF   - Number of values in this array.
C                           JLENF is negative if missing data coded
C
C                 IBITS   - Number of bits for coded data values.
C
C                 IWORD   - Number of words decoded.
C
C                 IERR    - Error indicator.
C
C                           0, No error.
C
C                          -5, Bit-map not yet catered for.
C
C                          -6, Vertical coordinate parameter array
C                              too small.
C
C                              Other error codes as for GRIBEX.
C
C     Method.
C     -------
C
C           Subroutine GRIBEX, which can decode all editions of the
C           GRIB code, is called for the actual decoding. Sections
C           1 and 2 information is then put in Edition Number 0
C           format, and any quasi-regular Gaussian grid transformed
C           into a regular grid.
C
C     Externals.
C     ----------
C
C           GRIBEX
C           QU2REG
C
C     Reference.
C     ----------
C
C           WMO Manual on Codes for GRIB code definition.
C
C     Comments.
C     ---------
C
C           Parameter names do not conform to current programming
C           conventions, but are the same as in the old DECOGB routine.
C
C     Author.
C     -------
C
C           J. Hennessy     ECMWF     18.06.91
C
C     Modifications.
C     -------------
C
C           J. Hennessy     ECMWF     19.06.91
C           Fix to accommodate analysis section's incorrect
C           use of old DECOGB.
C
C           J. Hennessy     ECMWF     26.06.91
C           Check on size of vertical coordinate parameters array
C           added. T213 model has 12 extra levels.
C
C     -----------------------------------------------------------------
C----<
C
C
C
C
C
C
C
C
C
C
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------
C
      DIMENSION ISEC0(2)
      DIMENSION ISEC1(32)
      DIMENSION ISEC2(512)
      DIMENSION ISEC3(2)
      DIMENSION ISEC4(64)
C
      DIMENSION ZSEC2(128)
      DIMENSION ZSEC3(2)
C
      CHARACTER*1 YOPER
C
      DIMENSION FPDATA(*)
      DIMENSION FVERT(*)
      DIMENSION IGRIB(*)
      DIMENSION IB1PAR(18)
      DIMENSION IB2PAR(11)
C
C     -----------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C
C
C
C*    Section 1. Decode data and convert to Edition 0 format.
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
      IERR = 0
C
C     Decode sections 1 and 2 or all sections, as required.
C
      YOPER = 'Z'
C
C     Fix for incorrect usage of old DECOGB by analysis section, who
C     have always called DECOGB with ILENF equal to 0, when they
C     should have used 1. They got away with it with the old DECOGB
C     because sections 1 and 2 were decoded before the error in the
C     parameter was discovered, and they never checked the return code.
C
      IF (ILENF.EQ.0) ILENF = 1
C
      IF (ILENF.EQ.1) YOPER = 'I'
      IERX = 1
      CALL GRIBEX (ISEC0,ISEC1,ISEC2,ZSEC2,ISEC3,ZSEC3,ISEC4,
     C             FPDATA,ILENF,IGRIB,ILENG,IWORD,YOPER,IERX)
C
      IERR = IERX
C
C     Bit map not catered for by DECOGB arguments.
C
      IF (IERR.EQ.-5) WRITE (*,9001)
C
C     Error encountered.
C
      IF (IERR.GT.0) GO TO 900
C
C     Transfer information to DECOGB arguments.
C
      JLENF = ISEC4(1)
      IBITS = ISEC4(2)
      JLENV = ISEC2(12)
C
C     Section 1.
C
      DO 110 J110 = 1,18
         IB1PAR(J110) = ISEC1(J110+1)
  110 CONTINUE
C
C     Section 2.
C
      DO 111 J111 = 1,11
         IB2PAR(J111) = ISEC2(J111)
  111 CONTINUE
C
C     Go to section 9, if pseudo-GRIB data.
C
      IF (IB1PAR(5).EQ.127.OR.IB1PAR(5).EQ.128)
     C   THEN
             WRITE (*,9002) IB1PAR(5)
             GO TO 900
         ENDIF
C
C     Check size of vertical coordinate parameter array.
C
      IF (JLENV.GT.ILENV)
     C   THEN
             IERR = -6
             WRITE (*,9003) JLENV , ILENV
             GO TO 900
         ENDIF
C
C     Vertical coordinate parameters, if any,
C
      DO 112 J112 = 1,JLENV
         FVERT(J112) = ZSEC2(J112+10)
  112 CONTINUE
C
C     Go to section 9, if decoding of sections 1 and 2 only.
C
      IF (YOPER.EQ.'I')
     C   THEN
             IF (ISEC2(1).NE.4) GO TO 900
             IF (ISEC2(17).EQ.1) IB2PAR(2) = 4 * ISEC2(10)
             GO TO 900
         ENDIF
C
C     If quasi-regular Gaussian grid, convert to regular and
C     adjust section 1 and 2 information accordingly.
C
      IF (ISEC2(1).NE.4) GO TO 900
C
      IF (ISEC2(17).EQ.1)
     C   THEN
             INOLAT = ISEC2(10) * 2
             INOLNG = ISEC2(10) * 4
             CALL QU2REG (FPDATA,ISEC2(23),INOLAT,INOLNG,1)
             JLENF      = INOLAT * INOLNG
             IB2PAR(2)  = 4 * ISEC2(10)
         ENDIF
C
C     -----------------------------------------------------------------
C
C
C
C
C
C
C
C
C
C
C*    Section 9. Return to calling routine. Format statements.
C     -----------------------------------------------------------------

C
  900 CONTINUE
C
 9001 FORMAT (1H ,'DECOGB : Bit map not catered for.')
C
 9002 FORMAT (1H ,'DECOGB : Pseudo-GRIB field number ',I3)
C
 9003 FORMAT (1H ,'DECOGB : ',I3,' vertical coordinate parameters, ',
     C                      'but array size is only ',I3,'.')
C
      RETURN
C
      END
