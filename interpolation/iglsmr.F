C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION IGLSMR (KILSMN, KILSMS, KOLSM, KNINDEX,
     1   KSINDEX, KOWE, PWFACT, KPR, KERR)
C
C---->
C**** *IGLSMR*
C
C     PURPOSE
C     -------
C
C     Calculate the effects of the land-sea masks for one line of
C     latitude on the unnormalised interpolation weights.
C
C     INTERFACE
C     ---------
C
C     IERR = IGLSMR (KILSMN, KILSMS, KOLSM, KNINDEX, KSINDEX,
C    1   KOWE, PWFACT, KPR, KERR)
C
C     Input parameters
C     ----------------
C
C     KILSMN     - The land sea mask for the line of latitude of the
C                  input field North of the output row.
C
C     KILSMS     - The land sea mask for the line of latitude of the
C                  input field South of the output row.
C
C     KOLSM      - The land sea mask for the current line of latitude
C                  of the output field.
C
C     KNINDEX    - This array contains the array offsets of the West
C                  and East points in the input line of latitude North
C                  of the current output field line of latitude.
C
C     KSINDEX    - This array contains the array offsets of the West
C                  and East points in the input line of latitude South
C                  of the current output field line of latitude.
C
C     KOWE       - The number of output field points in the West-East
C                  direction for this line of latitude.
C
C     PWFACT     - The array of interpolating weights to the four
C                  neighbouring points for every output point in this
C                  line of latitude.
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
C     PWFACT     - The modified array of interpolating weights to the
C                  four neighbouring points for every output point in
C                  this line of latitude.
C
C     Return value
C     ------------
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
C     None
C
C     Externals
C     ---------
C
C     INTLOG(R) - Logs messages.
C
C
C     Method
C     ------
C
C     Every time that a point in the input array has a land-sea mask
C     value (0-1) that does not match the corresponding output point
C     its interpolating value is multiplied by a scaling factor
C     (currently 0.2). This reduces the influence of such points on
C     the final interpolation.
C
C     The parameter list is designed to allow for quasi regular input
C     fields with different grid spacing in the Northern and Southern
C     lines of latitude.
C
C
C     Reference
C     ---------
C     None
C
C
C     Comments
C     --------
C     None
C
C
C     AUTHOR
C     ------
C     K. Fielding      *ECMWF*      Oct 1993
C
C
C     Modifications
C     -------------
C
C     None
C
C----<
C
C     -----------------------------------------------------------------|
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------|
C
C
      IMPLICIT NONE
C
#include "parim.h"
C
C     Dummy arguments
      INTEGER KOWE, KPR, KERR
      INTEGER KNINDEX (2, KOWE), KSINDEX (2, KOWE)
      INTEGER KILSMN (*), KILSMS (*), KOLSM (KOWE)
      REAL PWFACT (4, KOWE)
C
C     Local variables
      INTEGER JOLON
C
C     Multiplication factor for unlike land-sea masks
C
      REAL PPLSM
      PARAMETER (PPLSM = 0.2E0)
C
      INTEGER JPROUTINE
      PARAMETER (JPROUTINE = 25300)
C
C     -----------------------------------------------------------------|
C*    Section 1. Initialisation
C     -----------------------------------------------------------------|
C
  100 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGLSMR: Section 1.',JPQUIET)
C
      IGLSMR = 0
C
      IF (KPR .GE. 1) 
     X  CALL INTLOG(JP_DEBUG,'IGLSMR: No. of output longitudes = ',KOWE)
C
C     -----------------------------------------------------------------|
C*    Section 2. Calculate arrays of weights
C     -----------------------------------------------------------------|
C
  200 CONTINUE
C
      IF (KPR.GE.1) CALL INTLOG(JP_DEBUG,'IGLSMR: Section 2.',JPQUIET)
C
C     Now modify the unormalised weight for land-sea mask
C
      DO 210 JOLON = 1, KOWE
C
         IF (KILSMN (KNINDEX (JP_I_W, JOLON)) .NE.
     1      KOLSM (JOLON) )
     2         PWFACT (JP_I_NW, JOLON) =
     3            PWFACT (JP_I_NW, JOLON) * PPLSM
C
         IF (KILSMN (KNINDEX (JP_I_E, JOLON)) .NE.
     1      KOLSM (JOLON) )
     2         PWFACT (JP_I_NE, JOLON) =
     3            PWFACT (JP_I_NE, JOLON) * PPLSM
C
         IF (KILSMS (KSINDEX (JP_I_W, JOLON)) .NE.
     1      KOLSM (JOLON) )
     2         PWFACT (JP_I_SW, JOLON) =
     3            PWFACT (JP_I_SW, JOLON) * PPLSM
C
         IF (KILSMS (KSINDEX (JP_I_E, JOLON)) .NE.
     1      KOLSM (JOLON) )
     2         PWFACT (JP_I_SE, JOLON) =
     3            PWFACT (JP_I_SE, JOLON) * PPLSM
C
  210 CONTINUE
C
C     -----------------------------------------------------------------|
C*    Section 9. Return to calling routine. Format statements
C     -----------------------------------------------------------------|
C
  900 CONTINUE
C
      IF( KPR.GE.1 ) CALL INTLOG(JP_DEBUG,'IGLSMR: Section 9.',JPQUIET)
C
      RETURN
      END
