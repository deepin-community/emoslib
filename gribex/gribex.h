C
C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C
C**** GRIBEX - Coding and decoding of GRIB format data.
C
C     Purpose.
C     --------
C
C     1) Code data in FM-92 GRIB code, Edition 1.
C     2) Decode data from FM-92 GRIB code.
C     3) Decode only identification sections of GRIB
C        coded data ie Sections 0, 1 and 2.
C     4) Return length of GRIB message, in bytes, and GRIB
C        Edition number only.
C
C        A number of options exist when coding or decoding -
C        see values allowed for requested function, HOPER, below.
C
C        Decoding functions work on Experimental Edition,
C        Edition 0 and Edition 1 of GRIB code. Decoded values
C        for Sections 0 to 2 are always in Edition 1 format.
C
C**   Interface.
C     ----------
C
C     CALL GRIBEX (KSEC0,KSEC1,KSEC2,PSEC2,KSEC3,PSEC3,KSEC4,
C    X                   PSEC4,KLENP,KGRIB,KLENG,KWORD,HOPER,KRET)
C
C     Integer    K.
C     Real       P.
C     Logical    O.
C     Character  H.
C
C     Input Parameters for all functions.
C     -----------------------------------
C
C     HOPER      - Requested function.
C
C
C                  'A' To encode 8-bit data into GRIB code
C
C                  'B' To decode 8-bit data from GRIB code
C
C
C                  'C' To code data in GRIB code, with or
C                      without bit-maps.
C
C                  'D' To decode data from GRIB code. If
C                      ECMWF pseudo-Grib data is encountered,
C                      only sections 0 and 1 are decoded and
C                      the return code is set to -6.
C
C                  'G' Special decoding for graphics purposes.
C                      Reference value returned in PSEC4(1)
C                      Binary scale factor returned in PSEC4(2
C                      Bit pointer to data increments in
C                      KSEC4(34).
C
C                  'I' To decode only identification
C                      sections 0, 1 and 2 of GRIB or
C                      pseudo-Grib data.
C
C                  'J' To decode only identification
C                      sections 0, 1, 3 and 4 of GRIB.
C
C                  'K' Special "aggressive" coding: try all
C                      the relevant methods of packing,
C                      especially second-order packing,
C                      in order to get the shortest
C                      GRIB message. Effective only for
C                      grid-point fields. No feedback in
C                      KSEC4 descriptors, to preserve the
C                      "read-only" status of this array.
C
C                  'L' Return length of GRIB message, in
C                      bytes, and GRIB Edition number only.
C                      Length does not include any bytes
C                      added to round message length to a
C                      multiple of 120 bytes. Works also for
C                      pseudo-Grib data.
C
C                  'M' To code data in GRIB code and, if a
C                      bit-map is encountered, make GRIB
C                      message full length ie the same length
C                      as if all data values were given.
C
C                  'R' To decode data from GRIB code, and if
C                      a quasi-regular Gaussian grid or a
C                      quasi-regular latitude-longitude grid
C                      is encountered, convert it to regular.
C
C                  'S' To decode initialised analysis data
C                      from GRIB code, and if data is in the
C                      Experimental Edition of GRIB, set the
C                      Time Range Indicator flag. In the
C                      Experimental Edition there was no
C                      distinction between initialised and
C                      uninitialised analyses.
C
C                  'X' To extract data values for up to 4
C                      points from a GRIB coded Gaussian or
C                      Latitude/longitude field, without
C                      unpacking the data at other points.
C                      See words 34 to 42 of KSEC4 below.
C
C                  'Z' To decode data from GRIB code.
C                      If a bit-map is encountered,
C                      only sections 0,1 and 2 are decoded and
C                      the return code is set to -5.
C
C     KLENP      - Length of array PSEC4.
C
C     KLENG      - Length of array KGRIB.
C
C     KRET       - Response to error indicator.
C                  0         , Abort if error encountered.
C                              Negative return codes are
C                              informative and do not cause
C                              an abort.
C                  Non- zero , Return to calling routine
C                              even if error encountered.
C
C
C
C
C
C     Input parameters for coding function.
C     Output Parameters for decoding functions.
C     -----------------------------------------
C
C     KSEC1      - Integer parameters of Section 1 (Product
C                  Definition Section) of GRIB code.
C                  Integer array of at least 25 words.
C
C                  If Section 1 of the GRIB code contains
C                  data for ECMWF local use, KSEC1 should
C                  be sized accordingly eg 53 + N , where
C                  N is the number of ensemble forecasts.
C
C         Word   Contents.
C         ----   ---------
C           1    Version number of Code Table 2.
C           2    Identification of centre (Code Table 0).
C           3    Generating process identification number
C                ( allocated by originating centre ).
C           4    Grid definition (NNN -  Catalogue number
C                of grid used by originating centre. See
C                Volume B of publication WMO - No.9).
C           5    Flag indication relative to Section 2
C                (Grid Description Section) and Section
C                3 (Bit Map Section). Code Table 1.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Sections 2 and 3 omitted.
C                 128      Section 2 included, Section 3
C                          omitted.
C                  64      Section 2 omitted, Section 3
C                          included.
C                 192      Sections 2 and 3 included.
C
C           6    Indicator of parameter (Code Table 2).
C           7    Indicator of type of level (Code Table 3).
C                 (or satellite identifier)
C                 Satellite useage as defined by INPE/CPTEC
C                 and used by ECMWF, pending final definition
C                 by WMO.
C           8    Height, pressure etc of level (Code Table 3).
C                Single level or top of layer.
C                 (or satellite spectral band)
C                 Satellite useage as defined by INPE/CPTEC
C                 and used by ECMWF, pending final definition
C                 by WMO.
C           9    Height, pressure etc of level (Code Table 3).
C                Bottom of layer, if word 6 indicates a layer.
C          10    Year of century  }
C          11    Month            } Reference time of data -
C          12    Day              } Date and time of start of
C          13    Hour             } averaging or accumulation
C          14    Minute           } period.
C          15    Indicator of unit of time (Code Table 4).
C          16    P1 - Period of time (number of time units)
C                (0 for analyses or initialised analyses).
C          17    P2 - Period of time (number of time units);
C                or time interval between successive
C                analyses, initialised analyses or forecasts
C                undergoing averaging or accumulation;
C                otherwise set to zero.
C          18    Time range indicator (Code Table 5).
C          19    Number included in average, when time range
C                indicator indicates an average or
C                accumulation; otherwise set to zero.
C          20    Number missing from average, when time range
C                indicator indicates an average or
C                accumulation; otherwise set to zero.
C          21    Century of reference time of data.
C          22    Identification of sub-centre(Code Table C-1).
C          23    Decimal scale factor.
C          24    Flag field to indicate local use in
C                Section 1.
C                0 - No local use of section 1.
C                1 - Local use of section 1.
C       25-36    Reserved for WMO reserved fields. Set to 0.
C          37    ECMWF local usage identifier.This is a number
C                which indicates the contents of words 38-nn.
C
C                1 - ECMWF local GRIB use definition 1.
C                    Ensemble forecast data.
C                2 - ECMWF local GRIB use definition 2.
C                    Cluster means and standard deviations.
C                3 - ECMWF local GRIB use definition 3.
C                    Satellite image data.
C                4 - ECMWF local GRIB use definition 4.
C                    Ocean model data.
C                5 - ECMWF local GRIB use definition 5.
C                    Forecast probability data.
C                6 - ECMWF local GRIB use definition 6.
C                    Surface temperature data.
C                7 - ECMWF local GRIB use definition 7.
C                    Sensitivity gradient/Trajectory
C                    forecast and Sensitivity forecast data.
C                8 - ECMWF local GRIB use definition 8.
C                    ECMWF re-analysis data.
C                9 - ECMWF local GRIB use definition 9.
C                    Singular vectors and ensemble perturbations.
C
C
#include "ecdef1.h"
#include "ecdef2.h"
#include "ecdef3.h"
#include "ecdef4.h"
#include "ecdef5.h"
#include "ecdef6.h"
#include "ecdef7.h"
#include "ecdef8.h"
#include "ecdef9.h"
#include "ecdef10.h"
#include "ecdef11.h"
#include "ecdef12.h"
#include "ecdef13.h"
#include "ecdef14.h"
C
C
C
C
C
C     KSEC2      - Integer parameters of Section 2 (Grid
C                  Description Section) of GRIB code.
C                  Integer array of at least 22 + n words,
C                  where n is the number of parallels or
C                  meridians in a quasi-regular (reduced)
C                  Gaussian or latitude/longitude grid.
C
C          Notes:- 1) Latitudes, longitudes are in
C                     millidegrees.
C                  2) Latitude values in the range 0-90000.
C                  3) Longitude values in the range 0-360000.
C                  4) Southern latitudes and western
C                     longitudes are negative.
C
C         Word   Contents for latitude/longitude grids or
C                equidistant cylindrical or Plate Carree.
C         ----   ----------------------------------------
C           1    Data representation type (Code Table 6).
C           2    Ni - Number of points along a parallel.
C           3    Nj - Number of points along a meridian.
C           4    La1 - Latitude of first grid point.
C           5    Lo1 - Longitude of first grid point.
C           6    Resolution flag.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Direction increments not given.
C                          Used for quasi-regular grids, but
C                          can also be used for regular grids.
C                 128      Direction increments given.
C                          Grids must be regular.
C
C           7    La2 - Latitude of last grid point.
C           8    Lo2 - Longitude of last grid point.
C           9    Di - i direction increment.
C          10    Dj - j direction increment.
C          11    Scanning mode flags (Code Table 8).
C          12    Number of vertical coordinate parameters.
C          13    Latitude of the southern pole of rotation.
C          14    Longitude of the southern pole of rotation.
C          15    Latitude of the the pole of stretching.
C          16    Longitude of the the pole of stretching.
C          17    0 , Regular grid.
C                1 , Quasi-regular (reduced) grid.
C
C             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C             !                                               
C             !      At the moment quasi-regular latitude/    
C             !      longitude grids are not properly defined.
C             !      The Resolution flag field indicates both 
C             !      direction increments are given or not.   
C             !      One increment needs to be given. Grids   
C             !      can be irregular in one direction only.  
C             !                                               
C             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C          18    Earth flag.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Earth assumed spherical with
C                          radius of 6367.47 km.
C                  64      Earth assumed oblate spheroidal
C                          with size as determined by IAU in
C                          1965 :
C                          (6378.160km,6356.775km,f=1/297.0)
C
C          19    Components flag.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Resolved u and v components of
C                          vector quantities relative to
C                          easterly and northerly directions.
C
C                   8      Resolved u and v components of
C                          vector quantities relative to the
C                          defined grid in the direction of
C                          increasing x and y (or i and j)
C                          coordinates respectively.
C
C       20-22    Reserved. Set to 0.
C       23-nn    Number of points along each parallel
C                in a Quasi-regular grid. Number of parallels
C                is given by Nj above.
C                or
C                Number of points along each meridian
C                in a Quasi-regular grid. Number of  meridians
C                is given by Ni above.
C
C                Scanning mode flags (Code Table 8) indicate
C                whether points are consecutive on a meridian
C                or a parallel.
C
C          Notes:- 1) Increments are in millidegrees.
C
C
C
C
C
C         Word   Contents for Gaussian grids .
C         ----   ---------------------------------------
C           1    Data representation type (Code Table 6).
C           2    Ni - Number of points along a parallel.
C                    Cannot be used for quasi-regular grids.
C           3    Nj - Number of points along a meridian.
C           4    La1 - Latitude of first grid point.
C           5    Lo1 - Longitude of first grid point.
C           6    Resolution flag.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Direction increments not given.
C                          Used for quasi-regular grids, but
C                          can also be used for regular grids.
C                 128      Direction increments given.
C                          Grids must be regular.
C
C           7    La2 - Latitude of last grid point.
C           8    Lo2 - Longitude of last grid point.
C           9    Di - i direction increment.
C                     Cannot be used for quasi-regular grids.
C          10    N - Number of parallels between a Pole and
C                the Equator.
C          11    Scanning mode flags (Code Table 8).
C          12    Number of vertical coordinate parameters.
C          13    Latitude of the southern pole of rotation.
C          14    Longitude of the southern pole of rotation.
C          15    Latitude of the the pole of stretching.
C          16    Longitude of the the pole of stretching.
C          17    0 , Regular grid.
C                1 , Quasi-regular (reduced) grid.
C          18    Earth flag.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Earth assumed spherical with
C                          radius of 6367.47 km.
C                  64      Earth assumed oblate spheroidal
C                          with size as determined by IAU in
C                          1965 :
C                          (6378.160km,6356.775km,f=1/297.0)
C
C          19    Components flag.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Resolved u and v components of
C                          vector quantities relative to
C                          easterly and northerly directions.
C
C                   8      Resolved u and v components of
C                          vector quantities relative to the
C                          defined grid in the direction of
C                          increasing x and y (or i and j)
C                          coordinates respectively.
C
C       20-22    Reserved. Set to 0.
C       23-nn    Number of points along each parallel
C                in a Quasi-regular grid. Number of parallels
C                is given by Nj above.
C
C          Notes:- 1) Increments are in millidegrees.
C
C
C
C
C
C         Word   Contents for Spherical Harmonic Coefficients.
C         ----   --------------------------------------------
C           1    Data representation type (Code Table 6).
C           2    J - Pentagonal resolution parameter.
C           3    K - Pentagonal resolution parameter.
C           4    M - Pentagonal resolution parameter.
C           5    Representation type ( Code Table 9 ).
C           6    Representation mode ( Code Table 10 ).
C        7-11    Reserved. Set to 0.
C          12    Number of vertical coordinate parameters.
C          13    Latitude of the southern pole of rotation.
C          14    Longitude of the southern pole of rotation.
C          15    Latitude of the the pole of stretching.
C          16    Longitude of the the pole of stretching.
C       17-22    Reserved. Set to 0.
C
C
C
C
C
C         Word   Contents for Polar Stereographic.
C         ----   --------------------------------------------
C           1    Data representation type (Code Table 6).
C           2    Nx - Number of points along X-axis.
C           3    Ny - Number of points along Y-axis.
C           4    La1 - Latitude of first grid point.
C           5    Lo1 - Longitude of first grid point.
C           6    Reserved. Set to 0. Resolution flag is
C                not applicable to Polar stereographic.
C           7    LoV - Orientation of the grid ie the
C                longitude of the meridian which is parallel
C                to the Y-axis along which latitude increases
C                as the Y-coordinate increases.
C           8    Reserved. Set to 0.
C           9    Dx - X-direction grid length.
C          10    Dy - Y-direction grid length.
C          11    Scanning mode flag (Code Table 8).
C          12    Number of vertical coordinate parameters.
C          13    Projection centre flag.
C                0 , North pole is on projection plane.
C                1 , South pole is on projection plane. ??????
C                128 , South pole is on projection plane. ????
C       14-16    Reserved. Set to 0.
C          17    0 , Regular grid.
C                1 , Quasi-regular (reduced) grid.
C          18    Earth flag.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Earth assumed spherical with
C                          radius of 6367.47 km.
C                  64      Earth assumed oblate spheroidal
C                          with size as determined by IAU in
C                          1965 :
C                          (6378.160km,6356.775km,f=1/297.0)
C
C          19    Components flag.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Resolved u and v components of
C                          vector quantities relative to
C                          easterly and northerly directions.
C
C                   8      Resolved u and v components of
C                          vector quantities relative to the
C                          defined grid in the direction of
C                          increasing x and y (or i and j)
C                          coordinates respectively.
C               20-22      Reserved. Set to 0.
C
C
C          Notes   1) Grid lengths are in metres, at the 60-
C                     degree parallel nearest to the pole on
C                     the projection plane.
C
C
C
C
C
C         Word   Contents for Mercator.
C         ----   ---------------------------------------
C           1    Data representation type (Code Table 6).
C           2    Ni - Number of points along a parallel.
C           3    Nj - Number of points along a meridian.
C           4    La1 - Latitude of first grid point.
C           5    Lo1 - Longitude of first grid point.
C           6    Resolution flag.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Direction increments not given.
C                 128      Direction increments given.
C
C           7    La2 - Latitude of last grid point.
C           8    Lo2 - Longitude of last grid point.
C           9    Latin - latitude at which the Mercator
C                projection cylinder intersects the earth.
C          10    Reserved. set to 0.
C          11    Scanning mode flags (Code Table 8).
C          12    Number of vertical coordinate parameters.
C          13    Di - i direction grid length.
C          14    Dj - j direction grid length.
C       15-16    Reserved. Set to 0.
C          17    0 , Regular grid.
C                1 , Quasi-regular (reduced) grid.
C          18    Earth flag.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Earth assumed spherical with
C                          radius of 6367.47 km.
C                  64      Earth assumed oblate spheroidal
C                          with size as determined by IAU in
C                          1965 :
C                          (6378.160km,6356.775km,f=1/297.0)
C
C          19    Components flag.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Resolved u and v components of
C                          vector quantities relative to
C                          easterly and northerly directions.
C
C                   8      Resolved u and v components of
C                          vector quantities relative to the
C                          defined grid in the direction of
C                          increasing x and y (or i and j)
C                          coordinates respectively.
C
C       20-22    Reserved. Set to 0.
C
C          Notes   1) Grid lengths are in units of metres,
C                     at the parallel specified by Latin.
C
C
C
C
C
C         Word   Contents for Lambert conformal, secant or
C                tangent, conical or bi-polar (normal or
C                oblique) or
C                Albers equal-area, secant or tangent,
C                conical or bi-polar (normal or oblique).
C         ----   --------------------------------------------
C           1    Data representation type (Code Table 6).
C           2    Nx - Number of points along X-axis.
C           3    Ny - Number of points along Y-axis.
C           4    La1 - Latitude of first grid point.
C           5    Lo1 - Longitude of first grid point.
C           6    Resolution flag.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Direction increments not given.
C                 128      Direction increments given.
C
C           7    LoV - Orientation of the grid ie the  East
C                longitude of the meridian which is parallel
C                to the Y-axis along which latitude increases
C                as the Y-coordinate increases.
C           8    Reserved. Set to 0.
C           9    Dx - X-direction grid length.
C          10    Dy - Y-direction grid length.
C          11    Scanning mode flag (Code Table 8).
C          12    Number of vertical coordinate parameters.
C          13    Projection centre flag.
C                  0 , North pole is on projection plane.
C                      Only one projection centre is used.
C                128 , South pole is on projection plane.
C                      Only one projection centre is used.
C                 64 , North pole is on projection plane.
C                      Projection is bi-polar and symmetric.
C                192 , South pole is on projection plane.
C                      Projection is bi-polar and symmetric.
C          14    Latin 1 - First latitude from the pole at
C                which the secant cone cuts the sphere.
C          15    Latin 2 - Second latitude from the pole at
C                which the secant cone cuts the sphere.
C          16    Reserved. Set to 0.
C          17    0 , Regular grid.
C                1 , Quasi-regular (reduced) grid.
C          18    Earth flag.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Earth assumed spherical with
C                          radius of 6367.47 km.
C                  64      Earth assumed oblate spheroidal
C                          with size as determined by IAU in
C                          1965 :
C                          (6378.160km,6356.775km,f=1/297.0)
C
C          19    Components flag.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Resolved u and v components of
C                          vector quantities relative to
C                          easterly and northerly directions.
C
C                   8      Resolved u and v components of
C                          vector quantities relative to the
C                          defined grid in the direction of
C                          increasing x and y (or i and j)
C                          coordinates respectively.
C
C          20    Latitude of the southern pole.
C          21    Longitude of the southern pole.
C          22    Reserved. Set to 0.
C
C          Notes   1) Grid lengths are in metres, at the 60-
C                     degree parallel nearest to the pole on
C                     the projection plane.
C
C
C
C
C
C         Word   Contents for Space view perspective
C                or orthographic.
C         ----   ---------------------------------------
C           1    Data representation type (Code Table 6).
C           2    Nx - Number of points along x-axis.
C           3    Ny - Number of points along y-axis.
C           4    Lap - Latitude of sub-satellite point.
C           5    Lop - Longitude of sub-satellite point.
C           6    Resolution flag.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Direction increments not given.
C                 128      Direction increments given.
C
C           7    dx - Apparent diameter of the earth in
C                grid lengths in the x direction.
C           8    dy - Apparent diameter of the earth in
C                grid lengths in the y direction.
C           9    Xp X-coordinate of sub-satellite point
C          10    Yp Y-coordinate of sub-satellite point
C          11    Scanning mode flag (Code Table 8).
C          12    Number of vertical coordinate parameters.
C          13    The orientation of the grid.
C          14    nr - the altitude of the camera from the
C                earth's centre.
C                For orthographic view from infinite
C                distance 16777215.
C          15    Xo - X coordinate of origin of sector
C                     image.
C          16    Yo - Y coordinate of origin of sector
C                     image.
C          17    0 , Regular grid.
C                1 , Quasi-regular (reduced) grid.
C          18    Earth flag.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Earth assumed spherical with
C                          radius of 6367.47 km.
C                  64      Earth assumed oblate spheroidal
C                          with size as determined by IAU in
C                          1965 :
C                          (6378.160km,6356.775km,f=1/297.0)
C
C          19    Components flag.
C                Valid values are :-
C
C                Decimal
C                value     Meaning
C                -----     -------
C                   0      Resolved u and v components of
C                          vector quantities relative to
C                          easterly and northerly directions.
C
C                   8      Resolved u and v components of
C                          vector quantities relative to the
C                          defined grid in the direction of
C                          increasing x and y (or i and j)
C                          coordinates respectively.
C
C       20-22    Reserved. Set to 0.
C
C
C
C
C
C     PSEC2      - Real parameters for Section 2 (Grid
C                  Definition Section) of GRIB Code.
C                  Real array of at least 10 + nn words, where
C                  nn is the number of vertical coordinate
C                  parameters.
C
C         Word   Contents.
C         ----   --------------------------------------------
C           1      Angle of rotation.
C           2      Stretching factor.
C         3-10     Reserved. Set to 0.
C        11-nn     Vertical coordinate parameters.
C                  Number given in KSEC2(12)
C
C
C
C
C
C     KSEC3      - Integer parameters for Section 3 (Bit Map
C                  Section) of GRIB code.
C                  Integer array of at least 2 words.
C
C         Word   Contents.
C         ----   --------------------------------------------
C           1      0 , Bit map included in the GRIB message.
C                      Binary data array (PSEC4) contains the
C                      missing data indicator at the points
C                      where no data is given.
C                  Non-zero, Number of predetermined bit-map.
C                      Bit map is not included in the message.
C                      Binary data array contains only valid
C                      data values.
C
C           2      The value used to indicate missing data in
C                  an integer binary data array is indicated
C                  here.
C                  This value is user supplied for both
C                  coding and decoding, and redefined at each
C                  call.
C
C                  On ouput, whenever a masked field is found,
C                  the effective number of non-missing values
C                  is returned.
C
C
C
C
C     PSEC3      - Real parameters for Section 3 (Bit Map
C                  Section) of GRIB code.
C                  Real array of at least 2 words.
C
C         Word   Contents.
C         ----   --------------------------------------------
C           1      Not used.
C
C           2      The value used to indicate missing data in
C                  a real binary data array is indicated here.
C                  This value is user supplied for both
C                  coding and decoding.
C
C
C
C
C
C     KSEC4      - Integer parameters for Section 4 (Binary
C                  Data Section) of GRIB code.
C                  Integer array of at least 42 words.
C
C         Word   Contents.
C         ----   --------------------------------------------
C           1    Number of data values in array PSEC4 to be
C                packed in GRIB code or which have been
C                unpacked from GRIB code. Where a bit-map
C                is used this number includes the number of
C                mising data values.
C
C             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C             !                                              !
C             ! If this number is NEGATIVE, it indicates     !
C             ! ENTIRE FIELD IS MISSING. All values in PSEC4 !
C             ! are 0. This is an ECMWF convention - coded   !
C             ! data has scale factor, exponent and mantissa !
C             ! of reference value with all bits set to 1.   !
C             !                                              !
C             !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C           2    Number of bits used for each packed value.
C           3    Type of data. Used only if Section 2 is
C                not included when coding data.
C                  0 - Grid point data.
C                128 - Spherical harmonic coefficients.
C           4    Type of packing.
C                  0 - Simple packing.
C                 64 - Complex or second order packing.
C           5    Data representation.
C                  0 - Floating point data.
C                 32 - Integer data.
C           6    Additional flags indicator.
C                  0 - No additional flags.
C                 16 - additional flags.
C           7    Reserved. Set to 0.
C           8    Number of values indicator.
C                  0 - Single datum at each grid point.
C                 64 - Matrix of values at each grid point.
C           9    Secondary bit maps indicator.
C                  0 - No secondary bit maps.
C                 32 - Secondary bit maps present.
C          10    Values width indicator.
C                  0 - Second order values constant width.
C                 16 - Second order values different widths.
C          11    Number of bits for second order values,
C                when of constant width.
C
C          If not zero, words 12 to 15 indicate specific use of
C          2nd-order packing methods which are extensions
C          to current WMO standard for GRIB edition 1.
C
C          12    General extended 2nd-order packing indicator.
C                  0 - Method not used.
C                  8 - Method in use.
C          13    Boustrophedonic ordering indicator.
C                  0 - Sub-method not used.
C                  4 - Sub-method in use.
C          14    Spatial differencing 1st indicator.
C                  0 - Sub-method used at order KSEC4(15) if equal to 1.
C                  2 - Sub-method used, at order 2+KSEC4(15).
C          15    Spatial differencing 2nd indicator.
C                  0 - Sub-method used at order KSEC4(14) if equal to 2.
C                  1 - Sub-method used, at order KSEC4(14)+1.
C
C          Spatial differencing feature is used at order
C          KSEC4(14)+KSEC4(15), provided this sum is not 0, and the
C          valid entries mentioned above are used.
C
C     Contents of words 16-20 for simple packing cases :
C
C       16-20    Set to 0.
C
C     Contents of words 16-20 for complex packing of spectral data :
C
C          16    Pointer to the start of packed data values.
C          17    Scaling factor P, stored as the integer value P*1000
C                (in the range -10000 to +10000) for GRIB edition 1.
C                (and as P for GRIB edition 0 decoding)
C          18    Pentagonal resolution parameter J for data subset
C                stored as 32-bit floating-point values.
C          19    Pentagonal resolution parameter K for data subset.
C          20    Pentagonal resolution parameter M for data subset.
C
C     Contents of words 16-20 for complex packing of grid-point data
C     (second-order packing) :
C
C          16    Pointer (relative to section 4) to first-order values.
C          17    Pointer (relative to section 4) to 2nd-order values.
C          18    Number of first-order values.
C          19    Number of second-order values.
C          20    Group width (8 bits in standard cases).
C
C     Words 21 to 24 are supplied back both for decoding and encoding
C     functions.
C
C          21    Number of non-missing points if masked field .
C                (set to 0 otherwise)
C          22    Bit-map pointer to explicit bit-map if any.
C                (set to 0 otherwise)
C          23    Full length of section 4 (octets).
C          24    Unused bit count at end of section 4.
C
C       25-33    Reserved. Set to 0.
C
C                Words 34 to 42 are used only for the 'X'
C                function. Scanning mode must be from West
C                to East and from North to South.
C          34    Number of points (maximum 4) from which
C                data is to be unpacked.
C          35    Number of latitude row of first value.
C          36    Number of longitude point of first value.
C          37    Number of latitude row of second value.
C          38    Number of longitude point of second value.
C          39    Number of latitude row of third value.
C          40    Number of longitude point of third value.
C          41    Number of latitude row of fourth value.
C          42    Number of longitude point of fourth value.
C
C                For grid point packing, with a matrix of
C                values at each grid point, words 50 to
C                50+NC1+NC2 are used as follows.
C          50    First dimension (rows) of each matrix.
C          51    Second dimension (columns) of each matrix.
C          52    First dimension coordinate values definition
C                See Code Table 12.
C          53    NC1- Number of coefficients or values used to
C                specify first dimension coordinate function.
C          54    Second dimension coordinate values definition
C                See Code Table 12.
C          55    NC2- Number of coefficients or values used to
C                specify second dimension coordinate function.
C          56    First dimension physical significance. See
C                Code Table 13.
C          57    Second dimension physical significance. See
C                Code Table 13.
C     58 - 59    Reserved. Set to 0.
C
C                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C                !                                           !
C                ! In the WMO specification the following    !
C                ! fields are integer values. ECMWF uses     !
C                ! floating point values for the wave models !
C                ! so these fields contain real values on    !
C                ! both input and output.
C                !                                           !
C                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C     60 - (59+NC1)    Coefficients to define first dimension
C                coordinate values in functional form, or the
C                the explicit coordinate values.
C (60+NC1)-(59+NC1+NC2)    Coefficients to define second dimension
C                coordinate values in functional form, or the
C                the explicit coordinate values.
C
C
C
C
C
C     PSEC4      - Array of data values to be packed in GRIB
C                  code or which have been unpacked. Where a
C                  bit-map is included in the GRIB message
C                  this array contains missing data indicator
C                  ( value supplied by the user in PSEC3(2)
C                  or KSEC3(2) ) at the appropriate places.
C
C                  Although declared as real in GRIBEX this
C                  can be an array of integer data. The value
C                  in KSEC4(5) indicates whether data is in
C                  integer or floating point format.
C
C              !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C              !                                            !
C              !   When CODING data, PSEC4 is OVERWRITTEN.  !
C              !                                            !
C              !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C     Output parameters for coding function.
C     Input Parameters for decoding functions.
C     -----------------------------------------
C
C     KGRIB      - Array containing GRIB coded data.
C
C     KWORD      - Number of words of KGRIB occupied by
C                  coded data. Output parameter for coding
C                  function only. Not required as input
C                  for decoding.
C
C     Output Parameters for all functions.
C     -----------------------------------
C
C     KSEC0      - Word 1 contains number of octets in
C                  GRIB message (not including padding to
C                  a word boundary or rounding to a multiple
C                  of 120 octets).
C                - Word 2 contains GRIB edition number.
C
C     KRET       - Return code.
C
C                  Informative codes for decoding functions.
C
C                  -2  , Bit-map encountered with all bits
C                        set to 1. Array PSEC4 contains all
C                        real data values.
C                  -3  , Predetermined bit-map encountered.
C                        Data has not been fully decoded ie
C                        array PSEC4 contains only real data
C                        values. The user must use this data
C                        in conjunction with the defined
C                        bit-map.
C                  -4  , Bit-map encountered. The data has
C                        been fully decoded ie array PSEC4
C                        contains real values and missing
C                        data indicators where appropriate.
C                  -5  , Bit-map encountered. The data has
C                        not been decoded. This return code
C                        is set only by the 'Z' function.
C                  -6  , ECMWF pseudo-grib data encountered.
C
C                  Error codes.
C
C                  0   , No error encountered.
C                  201 , Invalid function requested.
C                  202 , Number of bits per data value exceeds
C                        word length.
C                  203 , Missing data indicated and data field
C                        contains non-zero values.
C                  204 , Number of bits for coding value is zero
C                        or negative.
C                  301 , Error in inserting/extracting
C                        letters GRIB.
C                  302 , Error extracting length of GRIB
C                        message.
C                  303 , Error inserting/extracting GRIB
C                        Edition Number.
C                  304 , Error extracting octets 22 and 23
C                        Experimental Edition check.
C                  305 , Input data is not GRIB or pseudo-
C                        grib.
C                  401 , Error inserting/extracting length of
C                        Section 1.
C                  402 , Error inserting/extracting Parameter
C                        Version Number.
C                  403 , Error inserting/extracting six fields
C                        from Identification of Centre to
C                        Indicator of type of level.
C                  404 , Error inserting/extracting Height,
C                        pressure, etc of levels.
C                  405 , Error inserting/extracting six fields
C                        from Year of century to Indicator
C                        of unit of time range.
C                  406 , Error inserting/extracting Period of
C                        time.
C                  407 , Error inserting/extracting time range
C                        indicator.
C                  408 , Error inserting/extracting number
C                        averaged.
C                  409 , Error inserting/extracting number
C                        missing from averages etc.
C                  410 , Error inserting/extracting century of
C                        data or reserved field.
C                  411 , Error inserting/extracting units
C                        decimal scale factor.
C                  412 , Error inserting/extracting ECMWF
C                        local data.
C                  413 , Grib Edition not catered for.
C                  414 , Error inserting/extracting sub-centre.
C                  499 , Error found when checking values for
C                        Section 1 against valid GRIB values.
C
C                  501 , Error inserting/extracting length of
C                        Section 2.
C                  502 , Error inserting/extracting number of
C                        Vertical coordinate parameters.
C                  503 , Error inserting/extracting location
C                        of List of vertical coordinate
C                        parameters or List of numbers of
C                        points.
C                  504 , Error inserting/extracting data
C                        representation type.
C                  505 , Error inserting/extracting number of
C                        points along a parallel or meridian.
C                  506 , Error inserting/extracting latitude
C                        or longitude of first grid point.
C                  507 , Error inserting/extracting components
C                        flag.
C                  508 , Error inserting/extracting latitude
C                        or longitude of last grid point.
C                  509 , Error inserting/extracting i
C                        direction increment.
C                  510 , Error inserting/extracting number of
C                        parallels between pole and Equator.
C                  511 , Error inserting/extracting scanning
C                        mode flags.
C                  513 , Error inserting/extracting j
C                        direction increment.
C                  514 , Error inserting/extracting J,K,M
C                        pentagonal resolution parameters.
C                  515 , Error inserting/extracting
C                        representation type or mode.
C                  517 , Error inserting/extracting latitude
C                        or longitude of southern pole.
C                  518 , Error inserting/extracting angle
C                        of rotation.
C                  519 , Error inserting/extracting latitude
C                        or of pole of stretching.
C                  520 , Error inserting/extracting
C                        stretching factor.
C                  521 , Error inserting/extracting
C                        vertical coordinate parameters.
C                  522 , Error inserting/extracting list of
C                        numbers of points.
C                  523 , Error inserting/extracting number of
C                        points along X or Y axis.
C                  524 , Error inserting/extracting X or Y
C                        axis grid lengths.
C                  525 , Error inserting/extracting Projection
C                        centre flag.
C                  526 ,  Error inserting/extracting latitude
C                        or  longitude of sub-satellite point.
C                  527 , Error inserting/extracting diameter
C                        of the earth in x or y direction.
C                  528 , Error inserting/extracting X or Y
C                        coordinate of sub-satellite point.
C                  529 , Error inserting/extracting orientatio
C                        of the grid or camera angle.
C                  530 , Error inserting/extracting X or Y
C                        coordinates of origin of sector.
C                  598 , Representation type not catered for.
C                  599 , Error found when checking values for
C                        Section 2 against valid GRIB values.
C                  601 , Error inserting/extracting length of
C                        Section 3.
C                  602 , Error inserting/extracting number of
C                        unused bits at end of section 3.
C                  603 , Error inserting/extracting bit-map
C                        reference table.
C                  604 , Error inserting/extracting primary
C                        Bit-map.
C                  605 , Cannot convert Quasi-regular
C                        Gaussian grid with a bit-map.
C                  699 , Error found when checking values for
C                        Section 3 against valid GRIB values.
C                  701 , Error inserting/extracting length of
C                        Section 4.
C                  703 , Second-order packing implies
C                        additional flags.
C                  704 , Functions 'A','B','G' invalid with
C                        second-order packing.
C                  705 , Only simple packing catered for.
C                  706 , Error in extracting section 4 flag
C                        field.
C                  707 , Error inserting/extracting scale
C                        factor.
C                  708 , Error inserting/extracting reference
C                        value.
C                  709 , Error inserting/extracting number of
C                        bits per data value.
C                  710 , Output array too small.
C                  711 , Error inserting/extracting real
C                        coefficient.
C                  712 , Error inserting/extracting data
C                        values.
C                  713 , Error inserting/extracting flag
C                        and unused bit field.
C                  714 , Function is 'X' and number of
C                        values is illegal.
C                  715 , Function is 'X' and scanning mode is
C                        not North to South and West to East.
C                  716 , Function is 'X' and field is not
C                        Gaussian or Latitude/longitude grid.
C                  717 , Function is 'X' and a bit-map is
C                        included.
C                  720 , Error inserting/extracting octet
C                        number at which packed data begins.
C                  721 , Error inserting/extracting extended
C                        extended flag field.
C                  722 , Error inserting/extracting first or
C                        second dimension of matrix.
C                  723 , Error inserting/extracting six fields
C                        from first dimension coordinate value
C                        onwards.
C                  724 , Error inserting/ectracting first or
C                        second dimension coefficients.
C                  725 , Error inserting secondary bit-maps.
C                  726 , Explicitly constant field not
C                        supported without a section 2.
C                  727 , Explicitly constant field not
C                        supported for spectral field.
C                  728 , Functions 'G' or 'B' not supported
C                        for an explicitly constant field.
C                  798 , Function is 'X' and no section 2 is
C                        included.
C                  799 , Error found when checking values for
C                        Section 4 against valid GRIB values.
C                  801 , Error inserting/extracting 7777 group
C                  802 , Error inserting/extracting length of
C                        GRIB message.
C                  805 , End of message 7777 group not found.
C                  806 , Error in extracting primary or
C                        secondary bit maps.
C                  808 , Error converting quasi-regular
C                        gaussian grid to regular.
C                  810 , Error inserting dummy zero.
C
C
C     Method.
C     -------
C
C     Input data packed in GRIB code in accordance with
C     parameters given or set by user or fully or partially
C     unpacked, depending on function used.
C
C     Externals.
C     ----------
C
C     ABORTX
C     C2ORDR
C     CONFP3
C     CSECT4
C     CSGNBT
C     D2ORDR
C     DECFP2
C     DSECT4
C     DSGNBT
C     ECLOC1
C     EXSCAL
C     EXTMAP
C     GBITMAP
C     GRCHK1
C     GRCHK2
C     GRCHK3
C     GRCHK4
C     GRPRS0
C     GRPRS1
C     GRPRS2
C     GRPRS3
C     GRPRS4
C     INSCAL
C     INSMP1
C     INSMP2
C     INXBIT
C     MAXMIN
C     MAXMN2
C     QU2REG2
C     RORINT
C     SETPAR
C
C     Reference.
C     ----------
C
C     WMO Manual on Codes for GRIB definition.
C     WMO Publication No. 9, Volume B, for grid catalogue numbers.
C
C     Comments.
C     ---------
C
C     All machine dependent code is in 3 low level routines.
C     Versions of these exist for the VAX, CYBER, IBM
C     and SUN workstation, as well as the CRAY.
C     INXBIT - contains calls to the routines GBYTE(S)
C              and SBYTE(S) or their equivalents.
C     SETPAR - to set number of bits in the computer word
C              and largest negative number.
C     ABORTX - to terminate execution of the job.
C
C     This routine codes/decodes:
C      -  regular and quasi-regular latitude/longitude grids,
C      -  regular and quasi-regular gaussian grids,
C      -  spherical harmonics,
C      -  space view perspective or orthographic fields,
C      -  polar stereographic data,
C      -  lambert conformal grids.
C     Grids may be rotated, stretched, or rotated and stretched.
C     They may have primary and secondary bit-maps.
C     Only simple packing of integer data is allowed, but
C     real data may use simple or complex packing.
C     Matrices of values at grid points are supported and the
C     additional flag field is allowed.
C
C     Apart from the values which can be passed to  this
C     routine, other values are held in a common area and
C     are used by default, unless changed by calls to the
C     appropriate routines before calling GRIBEX. The
C     following defaults are used. They have been selected
C     to facilitate the most frequent usage at ECMWF and
C     to ease the transition to the next version of the
C     GRIB code.
C
C     1) By default debug printout is switched off.
C        CALL GRSDBG (I) where
C                    I = Non-zero to switch on debug printout.
C                        0, to switch off debug printout.
C
C     2) By default the reference value used is the minimum
C        of the data values supplied.
C        CALL GRSREF (ZREF) to change the value, where ZREF
C        is real and the required value.
C
C     3) By default GRIB messages are rounded to a
C        multiple of 120 octets.
C        CALL GRSRND (I) where
C                    I = 0, to switch off rounding.
C                        Non-zero to switch on rounding.
C
C     4) By default, the values given  are checked for
C        consistency with GRIB code values as currently
C        defined, when coding data. Data values are never
C        checked.
C        CALL GRSVCK (I) where
C                    I = 0, to switch off checking.
C                        Non-zero to switch on checking.
C
C
C     5) By default P factor calculation switch is switched
C        off, i.e. the user supplies the P factor for complex
C        packing.
C        CALL GRSMKP (I) where
C                     I = 0 , user supplies the P factor
C                       = Non-zero , GRIBEX calculates the P
C                         factor.
C
C
C     Ancillary print routines are available for the
C     various sections of the GRIB code.
C
C     CALL GRPRS0 (KSEC0) To print section 0.
C
C     CALL GRPRS1 (KSEC0,KSEC1) To print section 1.
C
C     CALL GRPRS2 (KSEC0,KSEC2,PSEC2) To print section 2.
C
C     CALL GRPRS3 (KSEC0,KSEC3,PSEC3) To print section 3.
C
C     CALL GRPRS4 (KSEC0,KSEC4,PSEC4) To print section 4.
C
C     Routine contains Sections 0 to 9.
C
C     Author.
C     -------
C
C     J. Hennessy      ECMWF      25.06.91
C
C     Modifications.
C     --------------
C
C     J. Hennessy      ECMWF      09.07.91
C     Functions 'R' and 'L' added.
C     Release 1 of software.
C
C     J. Hennessy      ECMWF      14.08.91
C     Functions 'S' and 'X' added.
C
C     J. Hennessy      ECMWF      23.08.91
C     Bit-map handling added.
C
C     J. Hennessy      ECMWF      04.09.91
C     Polar stereographic representation added.
C     Flag bit handling modified. Various bugs fixed.
C
C     J. Hennessy      ECMWF      11.09.91
C     Function 'M' added.
C
C     J. Hennessy      ECMWF      24.09.91
C     Space view perspective representation added.
C
C     J. Hennessy      ECMWF      01.10.91
C     Integer data handling added. Length of GRIB message
C     for Experimental Edition and Edition 0 included
C     when function 'I' is used.
C
C     J. Hennessy      ECMWF      25.10.91
C     When decoding data with bit-maps, include the number of
C     missing data values in the number of values decoded.
C     Release 2 of software.
C
C     J. Hennessy      ECMWF      30.10.91
C     Check for ECMWF pseudo-grib data made specific for
C     parameter numbers 127 and 128.
C     Bug in 'X' function fixed.
C
C     J. Hennessy      ECMWF      22.11.91
C     A number of machine dependent features removed.
C     Bug fixed in Polar Stereographic representation.
C     Release 3 of software.
C
C     J. Hennessy      ECMWF      23.01.92
C     Handling of missing fields modified.
C     Print of sections 2 and 3 suppressed, if not used.
C
C     J. Hennessy      ECMWF      21.07.92
C     Consistency checks moved from end to start of sections.
C     Secondary bit-maps and matrix of values at each grid
C     point  and additional flag field added.
C     X and Y coordinates of origin of sector image added
C     to space view perspective representation.
C     Release 4 of software.
C
C     J. Hennessy      ECMWF      24.09.92
C     Bugfix. KSEC4(3) set to 0 before checking type of data.
C
C     J. Hennessy      ECMWF      12.10.92
C     Check for 'GRIB` added when decoding data.
C     Return correct error code, if 7777 group not found and
C     GRIB message contains bit-maps.
C
C     J. Hennessy      ECMWF      06.11.92
C     ECMWF use of part of Section 1 reserved for local use.
C     Release 5 of software.
C
C     J. Hennessy      ECMWF      25.02.93
C     Check on valid Grib Edition number added.
C
C     J. Hennessy      ECMWF      19.05.93
C     ECMWF local uses 3 and 4 added.
C     Function 'G' added.
C     Release 6 of software. (NEXT VERSION on Cray and IBM only)
C
C     J.D.Chambers     ECMWF      18.10.93
C     ECMWF local use 5. (Forecast probabilities).
C
C     J.D.Chambers     ECMWF      09.03.94
C     Add check that given number of bits for packing is positive
C
C     J.D.Chambers     ECMWF      15.04.94
C     Add options 'A' and 'B' for handling 8-bit data.
C
C     J.D.Chambers     ECMWF      09.04.04
C     Add complex packing for spherical harmonics.
C
C     J.D.Chambers     ECMWF      21.06.94
C     Set encode/decode flag allowing for possible use of 'L' option
C
C     J.D.Chambers     ECMWF      07.07.94
C     Put zeroes in all reserved fields (don't assume arrays have
C     been zeroed).
C     Use csgnbt and dsgnbt to code/decode sign bits.
C
C     J.D.Chambers     ECMWF      13.07.94
C     Replace call to QU2REG by call to QU2REG2 to allow linear
C     interpolation of fields with missing data values.
C
C     J.D.Chambers     ECMWF      06.09.94
C     Add user option to supply the P factor for complex packing
C     This is switched on/off by a user call to GRSMKP.
C
C     J.D.Chambers     ECMWF      11.11.94
C     Fix type of ZS for NOS/VE and assign of LENCODE/LDECODE
C     for 'L' option
C
C     J.D.Chambers     ECMWF      21.11.94
C     Use JNINT on VAX
C
C     J.D.Chambers     ECMWF      19.01.95
C     Allow for section 1 local use definition 8 (ERA)
C
C     J.D.Chambers     ECMWF      21.02.95
C     Allow for quasi-regular lat/long grids (c.f.Washington SSTs)
C
C     J.D.Chambers     ECMWF      22.02.95
C     Changes for sensitivity forecast.
C
C     J.D.Chambers     ECMWF      23.05.95
C     Add check that output array is big enough for filling with
C     missing data values.
C     Add return warning (-7) when all data values are missing.
C
C     J.D.Chambers     ECMWF      29.06.95
C     Allow for global product with wraparound, eg last longitude
C     repeats first in composite Washington SSTs.
C
C     J.D.Chambers     ECMWF      29.06.95
C     Allow Lambert conformal.
C
C     J.D.Chambers     ECMWF      30.06.95
C     Remove return warning (-7) when all data values are missing.
C
C     J.D.Chambers     ECMWF      13.09.95
C     Fail decoding if number of bits per packed value = number
C     of bits per computer word (error 205).
C
C     J.D.Chambers     ECMWF      09.10.95
C     Add decoding of Washington ensemble products local usage.
C
C     J.D.Chambers     ECMWF      11.01.96
C     Use section 1 octet 26 for designated sub-centres:
C       =  98 for EPS products from Washington with
C         ECMWF extensions.
C       = 240 for ECMWF seasonal forecast centre.
C       = 241 for Meteo France.
C       = 242 for EDF.
C       = 243 for UKMO.
C       = 0   otherwise.
C
C     J.D.Chambers     ECMWF      09.02.96
C     Add 'J' decoding option.
C
C     J.D.Chambers     ECMWF      24.10.96
C     Add handling of predetermined bitmap
C
C     J.D.Chambers     ECMWF        Feb 1997
C     Allow for 64-bit pointers
C
C     J. Clochard, Meteo France, for ECMWF - January 1998.
C     Add coding and decoding of second-order packed grid-point data.
C     Function 'K' added.
C     Enables decoding of explicitly constant fields (0-bit number)
C     for grid-point fields encoded with a section 2.
C     Update table 3 for level/layer management.
C     KSEC1(22) enforced only when coding ECMWF data.
C     For 'R' option, secure when no primary bit-map,
C     and take into account periodicity (or not) of input grid.
C     Default setting of global variables through GRSDEF routine.
C     For masked fields, effective number of points returned
C     in KSEC3(2).
C
C     J. Clochard, Meteo France, for ECMWF - September 1998.
C     Enable decoding of complex packing spectral data in GRIB edition 0
C     and/or without section 2.
C     KSEC0 added in DSECT4 calling syntax.
