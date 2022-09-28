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
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C                   ECMWF local GRIB use definition 4.
C                          Ocean model data.
C                   ----------------------------------
C
C   For use in storing ocean model data within the GRIB framework.
C   T.Stockdale, 17 May 1993
C
C   Numbers given are words of KSEC1
C
C
C   Standard ECMWF supplementary data (as per local use definition 1)
C   for words 38-41 ie
C
C      38  Class  (ops/research)
C      39  Type   (analysis/forecast/etc)
C      40  Stream
C      41  Expver (expt identifier)
C
C      42  Ensemble forecast number
C      43  Total number of forecasts in ensemble
C
C   If stream = 1090 (ECMWF ensemble seasonal forecasts),
C      42  Ensemble forecast number
C      43  0
C
C   Data specific to ocean model requirements
C
C    ** A separate version of code table 2 is used for ocean data
C       ECMWF local Code Table 2, Version Number 150 for FM92-VIII
C
C    ** Note that all coordinates in this local use GRIB definition are
C       given as 4-byte integers in the following units:
C
C          Latitudes/longitudes:   microdegrees
C          Distance (general):     metres   (default: can be changed)
C          Time:                   seconds  (default: can be changed)
C
C          Depth below sea level:  millimetres  (positive downwards)
C          Isopycnic level:        (potential density - 1000)*1.0E6
C
C    ** Permitted range of values is  -2.147E9 < i < 2.147E9 approx.
C
C    ** At several points in the definition, there is a potential need
C       for a variable amount of supplementary data. This is indicated
C       with '(+INFO)', and in such cases a supplementary data block is
C       specified at the end of the local use section.
C
C
C   1. Coordinate structure definition
C
C       The physical meaning of the (x,y,z,t) coordinate system is
C       defined here.
C
C      44  Fundamental spatial reference system (Planet flag)
C               0 = Earth  (centre/north pole/Greenwich)
C             200 = Geocentric RA/dec
C             201 = Heliocentric coordinates
C             255 = Unspecified
C
C      45  Fundamental time reference
C               0 = Reference time given in standard GRIB header
C                  (use for forecasts to indicate start of prediction)
C               1 = C.E. (ie 0 AD)
C               2 = Julian Day number = 0.0
C             100 = Zero at start of arbitrary expt
C             255 = Unspecified
C
C      46  Space unit flag (applies only if lengths NOT otherwise
C            labelled)
C               0 = metres
C           i<128 = 10**i metres
C           i>128 = 10**(i-256) metres
C
C      47  Vertical coordinate definition (z)
C               0 = z above origin
C               1 = R from origin
C               2 = h above mean sea level geopotential
C               3 = h above ground surface
C             160 = geopotential depth below mean sea level     (mm)
C             161 = ocean isopycnic surface  (pot. dens.: see above)
C
C      48  Horizontal coordinate definition (x,y)
C             0 = latitude/longitude           (microdegrees)
C             1 = cartesian (fundamental origin)
C             2 = cartesian (shifted,rotated origin)           (+INFO)
C             3 = regular gaussian grid        (microdegrees)  (+INFO)
C             4 = polar stereographic                          (+INFO)
C             5 = spherical harmonic coefficients  (integers)  (+INFO)
C
C      49  Time unit flag
C               0 = seconds
C               1 = minutes
C               2 = hours
C               3 = days
C               4 = years
C         5<i<128 = 10**(i-4) years
C           i>128 = 10**(i-256) seconds
C
C      50  Time coordinate definition (t)
C             0 = real earth time (UTC)
C             1 = ideal earth time (360 * 86400s days per year)
C
C   2. Position definition
C
C       A 2-dimensional field located in 4-dimensional space-time needs
C       2 coordinates to define where the field is located, and 2
C       coordinates internal to the field. The locating coordinates are
C       specified first. Each coordinate is identified according to the
C       following table, and should be in this order where possible:
C
C            1   t
C            2   z
C            3   x
C            4   y
C
C       It is possible to specify mixed coordinates, to allow sections
C       at angles to the coordinate system.
C
C      51  Mixed coordinate field flag
C             0 = No mixed coordinates
C             1 = x,y coordinates mixed (+INFO)
C             2 = x,z coordinates mixed (+INFO)
C             3 = y,z coordinates mixed (+INFO)
C             4 = x,t coordinates mixed (+INFO)
C             5 = y,t coordinates mixed (+INFO)
C             6 = z,t coordinates mixed (+INFO)
C
C
C       Now define the location of the 2-dimensional field:
C
C      52  Coordinate 1 flag  (usually time)
C      53  Averaging flag
C             0 = no averaging (data on/at level 1)
C             1 = inclusive average between level 1 and 2
C             2 = exclusive average between level 1 and 2
C      54  Position of level 1   (4 byte integer)
C      55  Position of level 2   (4 byte integer) (or zero if not used)
C
C      56  Coordinate 2 flag  (usually z-coordinate)
C      57  Averaging flag
C             0 = no averaging (data on/at level 1)
C             1 = inclusive average between level 1 and 2
C             2 = exclusive average between level 1 and 2
C      58  Position of level 1   (4 byte integer)
C      59  Position of level 2   (4 byte integer) (or zero if not used)
C
C       An inclusive average discards land points when calculating the
C       average, and will produce a value where there is at least one
C       valid ocean point. An exclusive average will only produce a
C       value if all of the points being averaged are valid.
C
C
C   3. Grid definition
C
C      60  Coordinate 3 flag (x-axis, usually longitude)
C      61  Coordinate 4 flag (y-axis, usually latitude)
C
C      62  Coordinate 4 of first grid point     (4-byte integer)
C      63  Coordinate 3 of first grid point     (4-byte integer)
C      64  Coordinate 4 of last grid point      (4-byte integer)
C      65  Coordinate 3 of last grid point      (4-byte integer)
C      66  i-increment         (also stored as a 4-byte integer)
C      67  j-increment         (also stored as a 4-byte integer)
C
C
C      68  Flag for irregular grid coordinate list
C             0 = none
C             1 = x-axis values    (typically longitude)
C             2 = y-axis values    (typically latitude)
C             3 = First x-axis, then y-axis values given
C      69  Flag for normal or staggered grid
C             0 = normal grid (all rows have same x-coordinate system)
C             1 = staggered grid (odd and even rows have different
C                 x-coordinate systems, eg Arakawa E grid)
C
C   4. Further information
C
C      70  Flag for any further information
C             0 = none
C             1 = auxiliary array contains x-axis topographic
C                 depths/heights
C
C   5. Auxiliary information
C
C      71  Number of entries in horizontal coordinate definition
C           supplement (1 byte only)
C      72  Number of entries in mixed coordinate definition  (2 bytes)
C      73  Number of entries in grid coordinate list  (2 bytes)
C      74  Number of entries in auxiliary array (2 bytes)
C
C                                                     [ 64 bytes fixed ]
C
C     A. Horizontal coordinate supplement  (4-byte integers)
C
C      A1-nn  Not yet defined
C
C     B. Mixed coordinate definition  (4-byte integers)
C
C         [  For the linear case, assume a transformation of the form
C              x' =  alpha * (x - x0) + (1-alpha) * (y - y0)  ]
C
C      B1  Flag for regularity of x-y coordinate mixing
C             0 = Mixing is regular and linear
C             1 = Mixing is irregular, code given
C        For regular, linear mixing:
C           B2  Alpha  (*1E9)
C           B3  X0
C           B4  Y0
C        Irregular mixing with code given:
C           B2  )
C           B3  ) Locally defined codes to define section
C           B4  )
C
C     C. Grid coordinate list  (4-byte integers)
C
C      C1-nn  Coordinates of irregular x and y-axes. The x-axis is
C             given first, then the second x-axis (if staggered grid),
C             and then the y-axis. If the x-axis is regular but
C             staggered, then the longitude of the first point of the
C             second row should be specified as the first entry of this
C             list.
C
C     D. Auxiliary array  (4-byte integers)
C
C      D1-nn  Auxiliary array values
C
C     E. Post-auxiliary array  (4-byte integers)
C
C      P0-n  Post-auxiliary array values
C
C      P0 - Count of items in post-auxiliary array
C           ( = n+1, including the count)
C      P1 - First item.
C      P2 - Second item.
C      "  - "
C      Pn - nth item.
C
C
C   Comments on the setting of other GRIB parameters:
C
C       The time and time-range data should be set properly in the main
C       section, if possible.
C
C       The indicator of type of level should be set to 160 (depth below
C       sea level) for all oceanographic data. The depth in metres
C       should be set to the appropriate value if the field is a
C       horizontal section, and to zero in all other cases.
C
C       It is assumed that the grid description section of the GRIB code
C       is set for a latitude/longitude grid, with correct values for
C       the number of points along parallels/meridians and the
C       coordinates of the first and last data points in millidegrees
C       (or the appropriate units from this section, divided by 1000).
C       The coordinate increments should be given to the extent that
C       the field is regular.
C
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
