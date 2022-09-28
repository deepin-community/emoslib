c
C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C
C**** "parim.h"
C
C     PURPOSE
C     _______
C
C     This file contains a set of constants used during interpolation.
C
C     INTERFACE
C     _________
C
C     #include "parim.h"
C
C     Common block usage
C     __________________
C
C     NONE
C
C     METHOD
C     ______
C
C     NONE
C
C     REFERENCE
C     _________
C
C     NONE
C
C     COMMENTS
C     ________
C
C     Contains sections 1 to 8
C
C     AUTHOR
C     ______
C
C     K. Fielding      *ECMWF*      Oct 1993
C
C     Modifications
C     -------------
C
C     S.Curic     ECMWF      March 2005
C     Changed constants JPSTRUNC=2047 JPGTRUNC=2048
C     in order to allow T2047 and N1024
C     Changed buffer size PGRIB_ISEC1 PGRIB_ISEC2 PGRIB_ISEC4
C
C     S.Curic     ECMWF      March 2005
C     Changed constants JPDISTP=0.25
C
C     S.Curic     ECMWF      March 2011
C     Changed constants JPSTRUNC=3999 JPGTRUNC=400
C     in order to allow T3999 and N2000
C     _______________________________________________________
C
C
C*    Section 1. Machine definition constants
C     _______________________________________________________
C
C     JPBYTES       - The number of bytes in an INTEGER word.
C     JPBITS        - The number of bits in an INTEGER word.
C     JPILEN        - The number of heap allocation units in an
C                     INTEGER word.
C     JPRLEN        - The number of heap allocation units in a REAL
C                     word.
C
      INTEGER JPBITS, JPBYTES, JPILEN, JPRLEN
C
#ifdef INTEGER_8
      PARAMETER (JPBYTES = 8)
#else
      PARAMETER (JPBYTES = 4)
#endif
      PARAMETER (JPILEN = JPBYTES)
#ifdef REAL_8
      PARAMETER (JPRLEN = 8)
#else
      PARAMETER (JPRLEN = 4)
#endif
      PARAMETER (JPBITS = 8 * JPBYTES)
C
C     _______________________________________________________
C
C
C*    Section 2. Library Limit Constants
C     _______________________________________________________
C
C     JPLONG        - The maximum number of points along a line of
C                     latitude.
C     JPLAT         - The maximum number of lines of latitude
C     JPGTRUNC      - The maximum Gaussian truncation allowed.
C     JPSTRUNC      - The maximum spherical harmonic truncation
C                     allowed.
C
C     JPML01        - 10 minute land sea mask - Number of values per
C                     degree.
C     JPLG01        - 10 minute land sea mask - Number of values long
C                     a line of latitude.
C     JPLT01        - 10 minute land sea mask - Number of lines of
C                     latitude.
C     JPPACK        - 10 minute land sea mask - Length of packed
C                     record in words.
C
C     PPDEGMIN      - The minimum grid size
C                     (Real (360) / REAL (JPLONG) ).
C
      INTEGER JPLONG, JPLAT, JPGTRUNC, JPSTRUNC
C
C     PARAMETER (JPLONG = 1440)
      PARAMETER (JPLONG = 4096)
C     PARAMETER (JPLAT  = 2048)
      PARAMETER (JPLAT  = 4000)
C     PARAMETER (JPGTRUNC = 2048)
      PARAMETER (JPGTRUNC = 4000)
C     PARAMETER (JPSTRUNC = 2047)
      PARAMETER (JPSTRUNC = 3999)
C
      INTEGER JPLG01, JPLT01, JPPACK, JPML01
C
      PARAMETER (JPML01 = 6)
      PARAMETER (JPLG01 = 360 * JPML01)
      PARAMETER (JPLT01 = 180 * JPML01)
#ifdef rs6000
      PARAMETER (JPPACK = (JPLG01 - 1) / 32 + 1)
#else
      PARAMETER (JPPACK = (JPLG01 - 1) / JPBITS + 1)
#endif
C
      REAL PPDEGMIN
C
      PARAMETER (PPDEGMIN = 0.5E0)
C
C     _______________________________________________________
C
C
C*    Section 3. Library Interface Constants
C     _______________________________________________________
C
C
C     Data representation type values:
C
C     JPGAUSSIAN - Gaussian
C     JPFGGROT   - Rotated full (regular) gaussian
C     JPQUASI    - Quasi-regular gaussian
C     JPQGGROT   - Rotated quasi-regular (reduced) gaussian
C     JPREDLL    - Quasi-regular latitude/longitude
C     JPREDLROT  - Rotated quasi-regular latitude/longitude
C     JPREGULAR  - Regular latitude longitude
C     JPREGROT   - Rotated regular latitude longitude
C     JPSPHERE   - Spherical harmonic
C     JPSPHROT   - Rotated spherical harmonic
C     JPSTRGG    - Streched quasi-regular gaussian
C     JPSTRSH    - Streched spherical harmonic
C     JPNOTYPE   - Dummy data.
C
C     Field parameter numbers required (All field numbers refer to
C     ECMWF local Code Table 2 with NITABLE .eq. 128).
C
C     JP_U          - Wind parameter U
C     JP_V          - Wind parameter V
C     JP_10U        - Wind parameter 10U
C     JP_10V        - Wind parameter 10V
C
C     JP_LSP        - Large scale precipitation LSP
C     JP_CP         - Convective precipitation CP
C     JP_SF         - Snowfall SF
C     JP_TP         - Total precipitation TP
C
C     JP_MSL        - Mean sea level pressure MSL
C     JP_LSM        - Land sea mask LSM
C
C     Field parameter numbers required (All field numbers refer to
C     the standard WMO parameter number with NITABLE .eq. 1)
C
C     JP_WMO_U      - Wind parameter U
C     JP_WMO_V      - Wind parameter V
C     JP_WMO_10U    - Wind parameter 10U
C     JP_WMO_10V    - Wind parameter 10V
C
C     JP_WMO_LSP    - Large scale precipitation LSP
C     JP_WMO_CP     - Convective precipitation CP
C     JP_WMO_SF     - Snowfall SF
C     JP_WMO_TP     - Total precipitation TP
C
C     JP_WMO_MSL    - Mean sea level pressure MSL
C     JP_WMO_LSM    - Land sea mask LSM
C
      INTEGER JPGAUSSIAN,JPQUASI,JPREGULAR,JPREGROT,JPSPHERE,JPSPHROT
      INTEGER JPFGGROT, JPQGGROT, JPSTRGG, JPSTRSH
      INTEGER JPREDLL, JPREDLROT
      INTEGER JPNOTYPE
C
      PARAMETER (JPGAUSSIAN =  4)
      PARAMETER (JPFGGROT   = 14)
      PARAMETER (JPQUASI    = 25)
      PARAMETER (JPQGGROT   = 27)
      PARAMETER (JPREDLL    = 26)
      PARAMETER (JPREDLROT  = 28)
      PARAMETER (JPREGULAR  =  0)
      PARAMETER (JPREGROT   = 10)
      PARAMETER (JPSPHERE   = 50)
      PARAMETER (JPSPHROT   = 60)
      PARAMETER (JPSTRGG    = 24)
      PARAMETER (JPSTRSH    = 70)
      PARAMETER (JPNOTYPE   = 9999)
C
      INTEGER JP_U, JP_V, JP_10U, JP_10V
C
      PARAMETER (JP_U   = 131)
      PARAMETER (JP_V   = 132)
      PARAMETER (JP_10U = 165)
      PARAMETER (JP_10V = 166)
C
      INTEGER JP_LSP, JP_CP, JP_SF, JP_TP
C
      PARAMETER (JP_LSP = 142)
      PARAMETER (JP_CP  = 143)
      PARAMETER (JP_SF  = 144)
      PARAMETER (JP_TP  = 228)
C
      INTEGER JP_MSL, JP_LSM
C
      PARAMETER (JP_MSL = 151)
      PARAMETER (JP_LSM = 172)
C
      INTEGER JP_WMO_U, JP_WMO_V
C
      PARAMETER (JP_WMO_U   = 33)
      PARAMETER (JP_WMO_V   = 34)
C
      INTEGER JP_WMO_LSP, JP_WMO_CP, JP_WMO_SF, JP_WMO_TP, JP_WMO_WESF
C
      PARAMETER (JP_WMO_TP  = 61)
      PARAMETER (JP_WMO_LSP = 62)
      PARAMETER (JP_WMO_CP  = 63)
      PARAMETER (JP_WMO_SF  = 64)
      PARAMETER (JP_WMO_WESF  = 65)
C
      INTEGER JP_WMO_MSL, JP_WMO_LSM
C
      PARAMETER (JP_WMO_MSL = 2)
      PARAMETER (JP_WMO_LSM = 81)
C
C
C     _______________________________________________________
C
C
C*    Section 4. Library Formulation Constants
C
C     These constants handle the fact that all latitudes and
C     longitudes are represented as integer values. The values of
C     longitude or latitude degrees are represented as
C     NINT (real_degree * PPMULT).
C
C     _______________________________________________________
C
C     JPMULT        - The integer version of the multiplier PPMULT
C                     (currently 10000).
C     JP90          - The integer value (90 * JPMULT).
C     JP180         - The integer value (180 * JPMULT).
C     JP360         - The integer value (360 * JPMULT).
C     JP0P5         - The integer value (JPMULT / 2).
C     JP0P25        - The integer value (JPMULT / 4). 0.25 resol
C     PPEPS         - Small value to force latitude rounding.
C
C     PPMULT        - The multiplying factor for all degree values
C                     (REAL (JPMULT) )
C
C     JPMICRO       -  scale factor from micro-degrees (from GRIB)
C                      to working scale (JPMULT).
C
      INTEGER JPMULT, JP90, JP180, JP360, JP0P5, JP0P25, JPMIN, JPMICRO
      REAL PPEPS
C
      PARAMETER (JPMULT = 100000)
      PARAMETER ( JP90 =  90 * JPMULT)
      PARAMETER (JP180 = 180 * JPMULT)
      PARAMETER (JP360 = 360 * JPMULT)
      PARAMETER (JP0P5 = JPMULT / 2)
      PARAMETER (JP0P25 = JPMULT / 4)
      PARAMETER (PPEPS = 0.9)
      PARAMETER (JPMIN = JP0P5)
      PARAMETER (JPMICRO = (JPMULT/1000) )
C
      REAL PPMULT
C
      PARAMETER (PPMULT = 1.0E5)
C
C     _______________________________________________________
C
C
C*    Section 5. Grib Section Size Constants
C
C     All these constants are in terms of expanded arrays after
C     GRIBEX has been used to unpack a field.
C
C     _______________________________________________________
C
C     JPENSEMBLE    - The number of ensemble forecasts (Used for
C                     handling clusters).
C     JPVERTICAL    - The number of model levels.
C     JPBASE2       - The basic size of Grib Section 2.
C
C     JPGRIB_ISEC0  - The size of Grib integer Section 0.
C     JPGRIB_ISEC1  - The size of Grib integer Section 1.
C     JPGRIB_ISEC2  - The size of Grib integer Section 2.
C     JPGRIB_ISEC3  - The size of Grib integer Section 3.
C     JPGRIB_ISEC4  - The size of Grib integer Section 4.
C
C     JPGRIB_RSEC2  - The size of Grib real Section 2.
C     JPGRIB_RSEC3  - The size of Grib real Section 3.
C
      INTEGER JPENSEMBLE, JPVERTICAL, JPBASE2
C
      PARAMETER (JPENSEMBLE = 100)
C     PARAMETER (JPVERTICAL = 100)
      PARAMETER (JPVERTICAL = 128)
      PARAMETER (JPBASE2 = 22)
C
      INTEGER JPGRIB_ISEC0, JPGRIB_ISEC1, JPGRIB_ISEC2, JPGRIB_ISEC3,
     1   JPGRIB_ISEC4
C
      PARAMETER (JPGRIB_ISEC0 = 2)
      PARAMETER (JPGRIB_ISEC1 = 2048 )
      PARAMETER (JPGRIB_ISEC2 = 5000)
      PARAMETER (JPGRIB_ISEC3 = 2)
      PARAMETER (JPGRIB_ISEC4 = 2512)
C
      INTEGER JPGRIB_RSEC2, JPGRIB_RSEC3
C
      PARAMETER (JPGRIB_RSEC2 = 10 + 2 * (JPVERTICAL + 1) )
      PARAMETER (JPGRIB_RSEC3 = 2)
C
C
C     _______________________________________________________
C
C
C*    Section 6. Grib Section 1, 2 and 4 Offsets
C
C     All these constants are used for expanded arrays after GRIBEX
C     has been used to unpack a field.
C
C     has been used to unpack a field.  C
C     _______________________________________________________
C
C     General offsets.
C     ________________
C
C     JPSEC1_TABLE  - The offset of the version number of code table 2.
C     JPSEC1_PARAM  - The offset of the parameter value in Section 1.
C
C     JPSEC2_REP    - The offset of the Data Representation Type in
C                     Section 2.
C     JPSEC2_QUASI  - The offset of the Quasi Regular Flag in
C                     Section 2.
C     JPSEC2_QUDEF  - The offset of the end of a standard Section 2.
C                     This may be used to simplify access to quasi
C                     regular Gaussian field latitude line length
C                     definitions.
C
C     JPSEC4_NVALUE - The offset of the number of data values in the
C                     field.
C     JPSEC4_NBITS  - The offset of the number of bits used for each
C                     data value in the field.
C
      INTEGER JPSEC1_PARAM, JPSEC1_TABLE
C
      PARAMETER (JPSEC1_TABLE = 1)
      PARAMETER (JPSEC1_PARAM = 6)
C
      INTEGER JPSEC4_NVALUE, JPSEC4_NBITS
C
      PARAMETER (JPSEC4_NVALUE = 1)
      PARAMETER (JPSEC4_NBITS = 2)
C
      INTEGER JPSEC2_REP, JPSEC2_QUASI, JPSEC2_QUDEF
C
      PARAMETER (JPSEC2_REP = 1)
      PARAMETER (JPSEC2_QUASI = 17)
      PARAMETER (JPSEC2_QUDEF = JPBASE2)
C
C     Offsets for Spherical Harmonic Data.
C     ____________________________________
C
C     JPSEC2_STRUNC - The offset of the Triangular truncation factor
C                     in Section 2.
C     JPSEC2_JTRUNC - The offset of the J-Pentagonal resolution
C                     parameter in Section 2.
C     JPSEC2_KTRUNC - The offset of the K-Pentagonal resolution
C                     parameter in Section 2.
C     JPSEC2_MTRUNC - The offset of the M-Pentagonal resolution
C                     parameter in Section 2.
C     JPSEC2_SHRES  - The offset of the Representation Type in
C                     Section 2.
C     JPSEC2_SHMODE - The offset of the Representation Mode in
C                     Section 2.
C
      INTEGER JPSEC2_STRUNC
      INTEGER JPSEC2_JTRUNC
      INTEGER JPSEC2_KTRUNC
      INTEGER JPSEC2_MTRUNC
      INTEGER JPSEC2_SHRES
      INTEGER JPSEC2_SHMODE
C
      PARAMETER (JPSEC2_STRUNC = 2)
      PARAMETER (JPSEC2_JTRUNC = 2)
      PARAMETER (JPSEC2_KTRUNC = 3)
      PARAMETER (JPSEC2_MTRUNC = 4)
      PARAMETER (JPSEC2_SHRES = 5)
      PARAMETER (JPSEC2_SHMODE = 6)
C
C     Offsets for all grid point data
C     _______________________________
C
C     JPSEC2_NLONG  - The offset of the Number of points along a line
C                     of latitude in Section 2. This value is unset
C                     for a quasi regular Gaussian field.
C     JPSEC2_NLAT   - The offset of the Number of lines of latitude
C                     in Section 2.
C     JPSEC2_NORTH  - The offset of the Northern line of latitude in
C                     Section 2.
C     JPSEC2_SOUTH  - The offset of the Southern line of latitude in
C                     Section 2.
C     JPSEC2_WEST   - The offset of the Western longitude limit in
C                     Section 2.
C     JPSEC2_EAST   - The offset of the Eastern longitude limit in
C                     Section 2. This value should be ignored for a
C                     quasi regular Gaussian field.
C     JPSEC2_RESOL  - The offset of the Resolution flag in Section 2.
C     JPSEC2_DLONG  - The offset of the Increment along a line of
C                     latitude in Section 2. This value is unset for
C                     a quasi regular Gaussian field or a field where
C                     Grib cannot describe this value accurately
C                     (0.5625 degrees).
C     JPSEC2_SCAN   - The offset of the Scanning mode flag in
C                     Section 2.
C     JPSEC2_VERT   - The offset of the Number of vertical coordinate
C                     parameters in Section 2.
C
      INTEGER JPSEC2_NLONG
      INTEGER JPSEC2_NLAT
      INTEGER JPSEC2_NORTH
      INTEGER JPSEC2_WEST
      INTEGER JPSEC2_SOUTH
      INTEGER JPSEC2_EAST
      INTEGER JPSEC2_RESOL
      INTEGER JPSEC2_DLONG
      INTEGER JPSEC2_SCAN
      INTEGER JPSEC2_VERT
C
      PARAMETER (JPSEC2_NLONG = 2)
      PARAMETER (JPSEC2_NLAT = 3)
      PARAMETER (JPSEC2_NORTH = 4)
      PARAMETER (JPSEC2_WEST = 5)
      PARAMETER (JPSEC2_SOUTH = 7)
      PARAMETER (JPSEC2_EAST = 8)
      PARAMETER (JPSEC2_RESOL = 6)
      PARAMETER (JPSEC2_DLONG = 9)
      PARAMETER (JPSEC2_SCAN = 11)
      PARAMETER (JPSEC2_VERT = 12)
C
C     Offset for Gaussian grids only
C     ______________________________
C
C     JPSEC2_GTRUNC - The offset of Gaussian truncation value in
C                     Section 2. This would be the number of points
C                     between a pole and the equator for a global
C                     grid.
C
      INTEGER JPSEC2_GTRUNC
C
      PARAMETER (JPSEC2_GTRUNC = 10)
C
C     Offset for latitude longitude grids only
C     ________________________________________
C
C     JPSEC2_DLAT   - The offset of the Increment along a line of
C                     meridian in Section 2. This value is unset for
C                     a field where Grib cannot describe this value
C                     accurately (0.5625 degrees).
C
      INTEGER JPSEC2_DLAT
C
      PARAMETER (JPSEC2_DLAT = 10)
C
C     _______________________________________________________
C
C
C*    Section 7. Useful constants
C     _______________________________________________________
C
C     PPZERO        - The constant real zero
C     PPONE         - The constant real one
C
C     JPABS         - I/O parameter to PBSEEK causing absolute file
C                     positioning.
C     JPEOF         - End of file return value from PBSEEK or PBREAD.
C     JPERR         - Error return value from PBSEEK or PBREAD.
C
      REAL PPZERO, PPONE
C
      PARAMETER (PPZERO = 0.0E0)
      PARAMETER (PPONE = 1.0E0)
C
      INTEGER JPABS, JPEOF, JPERR
C
      PARAMETER (JPABS = 0)
      PARAMETER (JPEOF = - 1)
      PARAMETER (JPERR = - 2)
C
      INTEGER JPLEVEL, JPINNER, JPGRIB_SPACE
C
      PARAMETER (JPLEVEL = 2)
      PARAMETER (JPINNER = JPLEVEL)
      PARAMETER (JPGRIB_SPACE = 1)
C
C     _______________________________________________________
C
C
C*    Section 8. Grid to Grid Constant Definitions
C     _______________________________________________________
C
C     JP_I_N        - An array index used to access the input
C                     latitude line North of the current output
C                     point.
C     JP_I_S        - An array index used to access the input
C                     latitude line South of the current output
C                     point.
C     JP_I_W        - An array index used to access the input points
C                     West of the current output point.
C     JP_I_E        - An array index used to access the input points
C                     East of the current output point.
C
C     JP_I_NW       - An array index used to access the weight array
C                     for the point North-West of the current output
C                     point.
C     JP_I_NE       - An array index used to access the weight array
C                     for the point North-East of the current output
C                     point.
C     JP_I_SW       - An array index used to access the weight array
C                     for the point South-West of the current output
C                     point.
C     JP_I_SE       - An array index used to access the weight array
C                     for the point South-East of the current output
C                     point.
C
C     JPNORTH       - An array index used to access the Northern
C                     value of a MARS AREA definition.
C     JPSOUTH       - An array index used to access the Southern
C                     value of a MARS AREA definition.
C     JPWEST        - An array index used to access the Western
C                     value of a MARS AREA definition.
C     JPEAST        - An array index used to access the Eastern
C                     value of a MARS AREA definition.
C
C     JPWESTEP      - An array index used to access the West-East
C                     stride of a MARS GRID definition.
C     JPNSSTEP      - An array index used to access the North-South
C                     stride of a MARS GRID definition.
C
      INTEGER JP_I_N, JP_I_S, JP_I_W, JP_I_E
C
      PARAMETER (JP_I_N = 1)
      PARAMETER (JP_I_S = 2)
      PARAMETER (JP_I_W = 1)
      PARAMETER (JP_I_E = 2)
C
      INTEGER JP_I_NW, JP_I_NE, JP_I_SW, JP_I_SE
C
      PARAMETER (JP_I_NW = 1)
      PARAMETER (JP_I_NE = 2)
      PARAMETER (JP_I_SW = 3)
      PARAMETER (JP_I_SE = 4)
C
      INTEGER JPNORTH, JPSOUTH, JPWEST, JPEAST
C
      PARAMETER (JPNORTH = 1)
      PARAMETER (JPWEST  = 2)
      PARAMETER (JPSOUTH = 3)
      PARAMETER (JPEAST  = 4)
C
      INTEGER JPWESTEP, JPNSSTEP
C
      PARAMETER (JPWESTEP = 1)
      PARAMETER (JPNSSTEP = 2)
C
C --------------------------------------------------------------------
C     Values for message logging
C
      INTEGER JP_DEBUG, JP_INFO, JP_WARN, JP_ERROR, JP_FATAL, JPQUIET
      PARAMETER ( JP_DEBUG = 0 )
      PARAMETER ( JP_INFO  = 1 )
      PARAMETER ( JP_WARN = 2 )
      PARAMETER ( JP_ERROR = 3 )
      PARAMETER ( JP_FATAL = 4 )
      PARAMETER ( JPQUIET  = -999999999 )
C
C --------------------------------------------------------------------
C     Values grid point generation styles
C
      REAL*8 JPDISTP
      INTEGER JPSMARS, JPSDISM
C
C     JPSMARS => Mars style, eg area checking
C     JPSDISM => Dissemination style, eg point selection
C     JPDISTP = default dissemination lat/long grid step (= 0.25 degrees)
C
      PARAMETER ( JPSMARS = 0 )
      PARAMETER ( JPSDISM = 1 )
      PARAMETER ( JPDISTP = 0.25 )
C
C --------------------------------------------------------------------
C     Arrays allocation sizes
C
      INTEGER JPARRAYDIM_WAVE
C
C     JPARRAYDIM_WAVE => dimensions of wave field interpolation arrays
C     (see WAVEXX2, WV2DXX2, WV2DINT, WVQLIN2, WVQLINT, JPEXPAND)
C
CC    PARAMETER ( JPARRAYDIM_WAVE = 2880*1442)   !EMOS 000396
      PARAMETER ( JPARRAYDIM_WAVE = 3600*1802*2) !EMOS 4.0.0  <-- PRODGEN change
C
C --------------------------------------------------------------------
C     Dimension for internal array for gaussian grid definitions
      INTEGER JPMAXNG
      PARAMETER ( JPMAXNG = 4000 )

