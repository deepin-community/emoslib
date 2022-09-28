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
C**** "grdynam.h"
C
C     PURPOSE
C     _______
C
C     This file contains all the dynamic work space array definitions
C     for grid point to grid point interpolation.
C
C     INTERFACE
C     _________
C
C     #include "grdynam.h"
C
C     Common block usage
C     __________________
C
C     GRID_POINT
C
C     NILATGP      - POINTER to array MILATG.
C     NILONGP      - POINTER to array MILONG.
C     NILSMP       - POINTER to array MILSM.
C     NINPNTP      - POINTER to array RINPNT.
C     NISTRTP      - POINTER to array MISTRT.
C     NNSDISTP     - POINTER to array MNSDIST.
C     NNSINDP      - POINTER to array MNSIND.
C     NMAXP        - POINTER to array RMAX.
C     NOLATGP      - POINTER to array MOLATG.
C     NOLONGP      - POINTER to array MOLONG.
C     NOLSMP       - POINTER to array MOLSM.
C     NWEDISTP     - POINTER to array MWEDIST.
C     NWEINDP      - POINTER to array MWEIND.
C     NWFACTP      - POINTER to array WFACT.
C
C     MILATG       - Dynamic array of length (NINS) (see nifld.common)
C                    which holds the values of the input field
C                    latitudes.
C     MILONG       - Dynamic array of length (NIWE + 1) (see
C                    nifld.common) which holds the values of the
C                    input field longitudes.
C     MILSM        - Dynamic array of length (NIWE, 2) (see
C                    nifld.common) used in calculating the effects of
C                    the land sea mask on interpolation.
C     MISTRT       - Dynamic array of length (NINS) (see nifld.common)
C                    which holds the array offsets of the start of
C                    each latitude line for a quasi regular Gaussian
C                    input field. No space is assigned to this array
C                    for a regular input field.
C     MNSDIST      - Dynamic array of length (2, NONS) (see
C                    nofld.common) which holds the distances to
C                    neighbouring latitude lines of the input field
C                    from the associated line of latitude in the
C                    output field.
C     MNSIND       - Dynamic array of length (2, NONS) (see
C                    nofld.common) which holds the latitude line
C                    numbers (array offset) of the input field
C                    associated with each line of latitude in the
C                    output field.
C     MOLATG       - Dynamic array of length (NONS) (see nofld.common)
C                    which holds the values of the output field
C                    latitudes.
C     MOLONG       - Dynamic array of length (NOWE) (see nofld.common)
C                    which holds the values of the output field
C                    longitudes.
C     MOLSM        - Dynamic array of length (NOWE) (see nofld.common)
C                    used in calculating the effects of the land sea
C                    mask on interpolation.
C     MWEDIST      - Dynamic array of length (2, NONS) for a regular
C                    input field and of length (2, 2 * NONS * NOWE)
C                    (see nofld.common) for a quasi regular Gaussian
C                    input field. This array holds the distances to
C                    neighbouring longitude points of the input field
C                    from the associated longitude points in the
C                    output field.
C     MWEIND       - Dynamic array of length (2, NONS) for a regular
C                    input field and of length (2, 2 * NONS * NOWE)
C                    (see nofld.common) for a quasi regular Gaussian
C                    input field. This array holds the longitude
C                    points (array offset) from the input field
C                    associated with each longitude point in the
C                    output field.
C     RINPNT       - Dynamic array of length (NOWE) (see nofld.common)
C                    used to aid vectorisation in processing
C                    precipitation fields.
C     RMAX         - Dynamic array of length (NOWE) (see nofld.common)
C                    used to aid vectorisation in processing
C                    precipitation fields.
C     WFACT        - Dynamic array of length (4, NOWE * NONS) (see
C                    nofld.common) which holds the interpolation
C                    weights for each point in the output field.
C
C     AUTHOR
C     ______
C
C     K. Fielding      *ECMWF*      Jan 1994
C
C     MODIFICATIONS
C     _____________
C
C     J.D.Chambers     ECMWF        June 1996
C
C
C     -----------------------------------------------------------------|
C*    Section 2. Pointer variable declaration and common
C     -----------------------------------------------------------------|
C
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8
#else
      INTEGER
#endif
     1   NILONGP, NILATGP, NOLONGP, NOLATGP, NNSINDP, NNSDISTP,
     2   NWEINDP, NWEDISTP, NISTRTP, NILSMP, NOLSMP, NWFACTP,
     3   NMAXP, NINPNTP
#endif
C
      COMMON /GRID_POINT/
     1   NILONGP, NILATGP, NOLONGP, NOLATGP, NNSINDP, NNSDISTP,
     2   NWEINDP, NWEDISTP, NISTRTP, NILSMP, NOLSMP, NWFACTP,
     3   NMAXP, NINPNTP
C
      SAVE /GRID_POINT/
C
C     -----------------------------------------------------------------|
C*    Section 3. Pointer and array combination
C                Array dimensions are in nifld.common and
C                nofld.common
C     -----------------------------------------------------------------|
C
      INTEGER MILONG, MOLONG, MILATG, MOLATG, MNSIND, MNSDIST,
     1   MWEIND, MWEDIST, MISTRT, MILSM, MOLSM
C
      REAL WFACT, RMAX, RINPNT
C
      POINTER (NILONGP, MILONG (NIWE + 1) )
      POINTER (NOLONGP, MOLONG (NOWE) )
      POINTER (NILATGP, MILATG (NINS) )
      POINTER (NOLATGP, MOLATG (NONS) )
      POINTER (NNSINDP, MNSIND (2, NONS) )
      POINTER (NNSDISTP, MNSDIST (2, NONS) )
C
      POINTER (NWFACTP, WFACT (4, NOWE * NONS) )
 
C     These arrays are only used as (2, NOWE) for regular input
C
      POINTER (NWEINDP, MWEIND (2, 2 * NOWE * NONS) )
      POINTER (NWEDISTP, MWEDIST (2, 2 * NOWE * NONS) )
C
C     These arrays are not used for regular input
C
      POINTER (NISTRTP, MISTRT (NINS) )
C
C     These arrays are not used unless OLSM is .TRUE.
C
      POINTER (NILSMP, MILSM (NIWE, 2) )
      POINTER (NOLSMP, MOLSM (NOWE) )
C
C     Works space for precipitation
C
      POINTER (NMAXP, RMAX (NOWE) )
      POINTER (NINPNTP, RINPNT (NOWE) )
