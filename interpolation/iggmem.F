C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGGMEM (KPR, KERR)
C
C---->
C**** *IGGMEM*
C
C     PURPOSE
C     _______
C
C     This routine acquires heap space for regular_gg to regular_ll
C     interpolations.
C
C     INTERFACE
C     _________
C
C     IERR = IGGMEM (KPR, KERR)
C
C     Input parameters
C     ________________
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
C     None
C
C     Return value
C     ____________
C
C     The error indicator (INTEGER).
C
C     Error and Warning Return Values
C     _______________________________
C
C     None
C
C     Common block usage
C     __________________
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
C                    field (used in grspace.h).
C     NOWE         - Number of grid points in WE direction for output
C                    field (used in grspace.h).
C
C     grspace.h    - This file contains all the work space array
C                    definitions for grid point to grid point
C                    interpolation.
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
C                    input field.
C                    This array holds the distances to neighbouring
C                    longitude points of the input field from the
C                    associated longitude points in the output field.
C     MWEIND       - Dynamic array of length (2, NONS) for a regular
C                    input field and of length (2, 2 * NONS * NOWE)
C                    (see nofld.common) for a quasi regular Gaussian
C                    input field.
C                    This array holds the longitude points (array
C                    offset) from the input field associated with each
C                    longitude point in the output field.
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
C     EXTERNALS
C     _________
C
C     IGALLOC    - Get a heap allocation for array space.
C     INTLOG     - Logs messages.
C
C     METHOD
C     ______
C
C     This routine calculates how much heap space is required. IGALLOC
C     is then used to acquire the required space. Finally the POINTERs
C     in grspace.h are initialised to allow using the dynamic arrays.
C
C     REFERENCE
C     _________
C
C     None
C
C     COMMENTS
C     ________
C
C     Program contains sections 0 to 3 and 9
C
C     AUTHOR
C     ______
C
C     K. Fielding      *ECMWF*      Jan 1994
C
C     MODIFICATIONS
C     _____________
C
C     None
C
C----<


C     Section 0. Definition of variables
      IMPLICIT NONE

#include "parim.h"
#include "nifld.common"
#include "nofld.common"
#include "grspace.h"

C     Dummy arguments
      INTEGER KPR, KERR

C     Local variables
#ifdef POINTER_64
      INTEGER*8 IBASE
#else
      INTEGER IBASE
#endif
      INTEGER*8 SIZES(14)
      INTEGER IREQUEST, IERR, I

C     External functions
      INTEGER IGALLOC


C     Section 1. Initialisation
  100 CONTINUE
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGGMEM: Section 1.',JPQUIET)
      IGGMEM = 0


C     Section 2. Calculate memory required and get heap
  200 CONTINUE

C     Specify array allocation sizes:
C     - uses JPRLEN for REAL arrays
C     - uses JPILEN for INTEGER arrays
      SIZES( 1) = JPRLEN * (NOWE*NONS*4)    ! NWFACTP  / WFACT
      SIZES( 2) = JPRLEN * (NOWE)           ! NMAXP    / RMAX   (#1 precipitation)
      SIZES( 3) = JPRLEN * (NOWE)           ! NINPNTP  / RINPNT (#2)
      SIZES( 4) = JPILEN * (NIWE+1)         ! NILONGP  / MILONG
      SIZES( 5) = JPILEN * (NINS)           ! NILATGP  / MILATG
      SIZES( 6) = JPILEN * (NOWE)           ! NOLONGP  / MOLONG
      SIZES( 7) = JPILEN * (NONS)           ! NOLATGP  / MOLATG
      SIZES( 8) = JPILEN * (NONS*2)         ! NNSINDP  / MNSIND
      SIZES( 9) = JPILEN * (NONS*2)         ! NNSDISTP / MNSDIST
      SIZES(10) = JPILEN * (NOWE*2)         ! NWEINDP  / MWEIND (*)
      SIZES(11) = JPILEN * (NOWE*2)         ! NWEDISTP / MWEDIST (*)
      SIZES(12) = JPILEN * 0                ! NISTRTP  / MISTRT (*)
      SIZES(13) = JPILEN * (NIWE*2)         ! NILSMP   / MILSM (#1 lsm)
      SIZES(14) = JPILEN * (NOWE)           ! NOLSMP   / MOLSM (#2)

C     Get the required memory
      IREQUEST = 0
      DO I = 1, 14
        IREQUEST = IREQUEST + SIZES(I)
      ENDDO
      IERR = IGALLOC(JPINNER,IREQUEST,IBASE,KPR,KERR)
      IF (IERR.GT.0) THEN
         IGGMEM = IERR
         GOTO 900
      ENDIF

C     Set POINTER addresses
      NWFACTP  = IBASE
      NMAXP    = SIZES( 1) + NWFACTP
      NINPNTP  = SIZES( 2) + NMAXP
      NILONGP  = SIZES( 3) + NINPNTP
      NILATGP  = SIZES( 4) + NILONGP
      NOLONGP  = SIZES( 5) + NILATGP
      NOLATGP  = SIZES( 6) + NOLONGP
      NNSINDP  = SIZES( 7) + NOLATGP
      NNSDISTP = SIZES( 8) + NNSINDP
      NWEINDP  = SIZES( 9) + NNSDISTP
      NWEDISTP = SIZES(10) + NWEINDP
      NISTRTP  = SIZES(11) + NWEDISTP
      NILSMP   = SIZES(12) + NISTRTP
      NOLSMP   = SIZES(13) + NILSMP

C     Display pointers information
      IF( KPR.GE.1 ) THEN
        CALL INTLOG(JP_DEBUG,'IGGMEM: ************************',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IGGMEM: * Output addresses:',     JPQUIET)
        CALL INTLOG(JP_DEBUG,'IGGMEM: * (print might overflow)',JPQUIET)
        CALL INTLOG(JP_DEBUG,'IGGMEM: - WFACT        = ',NWFACTP)
        CALL INTLOG(JP_DEBUG,'IGGMEM: - RMAX         = ',NMAXP)
        CALL INTLOG(JP_DEBUG,'IGGMEM: - RINPNT       = ',NINPNTP)
        CALL INTLOG(JP_DEBUG,'IGGMEM: - Input long.  = ',NILONGP)
        CALL INTLOG(JP_DEBUG,'IGGMEM: - Input lat.   = ',NILATGP)
        CALL INTLOG(JP_DEBUG,'IGGMEM: - Output long. = ',NOLONGP)
        CALL INTLOG(JP_DEBUG,'IGGMEM: - Output lat.  = ',NOLATGP)
        CALL INTLOG(JP_DEBUG,'IGGMEM: - NS offset    = ',NNSINDP)
        CALL INTLOG(JP_DEBUG,'IGGMEM: - NS distance  = ',NNSDISTP)
        CALL INTLOG(JP_DEBUG,'IGGMEM: - WE offset    = ',NWEINDP)
        CALL INTLOG(JP_DEBUG,'IGGMEM: - WE distance  = ',NWEDISTP)
        CALL INTLOG(JP_DEBUG,'IGGMEM: - Line start   = ',NISTRTP)
        CALL INTLOG(JP_DEBUG,'IGGMEM: - Input LSM    = ',NILSMP)
        CALL INTLOG(JP_DEBUG,'IGGMEM: - Output LSM   = ',NOLSMP)
        CALL INTLOG(JP_DEBUG,'IGGMEM: ***********************', JPQUIET)
      ENDIF


C     Section 9. Return to calling routine
  900 CONTINUE
      IF (KPR.GE.1)  CALL INTLOG(JP_DEBUG,'IGGMEM: Section 9.',JPQUIET)


      END

