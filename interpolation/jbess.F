C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C
      INTEGER FUNCTION JBESS (KTRUNC, PBESJ0)
C
C**** *JBESS* - Routine to return zeros of the J0 Bessel function.
C
C     PURPOSE
C     _______
C
C     This routine returns up to JPLOOK zeros of the J0 Bessel function.
C     If more than JPLOOK zeros are required then approximate zeros are
C     returned after the JPLOOK-th.
C
C     INTERFACE
C     _________
C
C     IERR = JBESS (KTRUNC, PBESJ0)
C
C     Input parameters
C     ________________
C
C     KTRUNC     - This is the number of zeros of the J0 Bessel
C                  function required.
C
C     Output parameters
C     ________________
C
C     PBESJ0     - The output array of zeros of the J0 Bessel function.
C
C     Return Value
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
C     None
C
C     EXTERNALS
C     _________
C
C     None
C
C     METHOD
C     ______
C
C     The first JPLOOK values are obtained from a look-up table (ZPBES).
C     Any additional values requested are approximated by adding
C     PI (3.14159...) to the previous value.
C
C     REFERENCE
C     _________
C
C     None
C
C     COMMENTS
C     ________
C
C     This routine is adapted from that in the old Marsint library.
C     The interface and the variable names have been modified.
C
C     Program contains sections 0 to 2 and 9
C
C     AUTHOR
C     ______
C
C     K. Fielding      *ECMWF*      Oct 1993
C
C     MODIFICATIONS
C     _____________
C
C     None
C
C     _______________________________________________________
C
C
C*    Section 0. Definition of variables.
C     _______________________________________________________
C
C*    Prefix conventions for variable names
C
C     Logical      L (but not LP), global or common.
C                  O, dummy argument
C                  G, local variable
C                  LP, parameter.
C     Character    C, global or common.
C                  H, dummy argument
C                  Y (but not YP), local variable
C                  YP, parameter.
C     Integer      M and N, global or common.
C                  K, dummy argument
C                  I, local variable
C                  J (but not JP), loop control
C                  JP, parameter.
C     REAL         A to F and Q to X, global or common.
C                  P (but not PP), dummy argument
C                  Z, local variable
C                  PP, parameter.
C
C     Implicit statement to force declarations
C
      IMPLICIT NONE
C
#include "jparams.h"
C
C     Dummy arguments
C
      INTEGER KTRUNC
C
      REAL PBESJ0 (*)
C
C     Local variables
C
C
      INTEGER ILOOK
      INTEGER JAPPRX, JLOOK
C
      REAL ZPBES (JPLOOK)
C
      DATA ZPBES        /   2.4048255577E0,   5.5200781103E0,
     1    8.6537279129E0,  11.7915344391E0,  14.9309177086E0,
     2   18.0710639679E0,  21.2116366299E0,  24.3524715308E0,
     3   27.4934791320E0,  30.6346064684E0,  33.7758202136E0,
     4   36.9170983537E0,  40.0584257646E0,  43.1997917132E0,
     5   46.3411883717E0,  49.4826098974E0,  52.6240518411E0,
     6   55.7655107550E0,  58.9069839261E0,  62.0484691902E0,
     7   65.1899648002E0,  68.3314693299E0,  71.4729816036E0,
     8   74.6145006437E0,  77.7560256304E0,  80.8975558711E0,
     9   84.0390907769E0,  87.1806298436E0,  90.3221726372E0,
     A   93.4637187819E0,  96.6052679510E0,  99.7468198587E0,
     1  102.8883742542E0, 106.0299309165E0, 109.1714896498E0,
     2  112.3130502805E0, 115.4546126537E0, 118.5961766309E0,
     3  121.7377420880E0, 124.8793089132E0, 128.0208770059E0,
     4  131.1624462752E0, 134.3040166383E0, 137.4455880203E0,
     5  140.5871603528E0, 143.7287335737E0, 146.8703076258E0,
     6  150.0118824570E0, 153.1534580192E0, 156.2950342685E0 /
C
C     _______________________________________________________
C
C
C*    Section 1. Extract initial values from look up table
C
C     _______________________________________________________
C
  100 CONTINUE
C
      JBESS = 0
C
      ILOOK = MIN(KTRUNC, JPLOOK)
C
      DO 110 JLOOK = 1, ILOOK
         PBESJ0(JLOOK) = ZPBES(JLOOK)
  110 CONTINUE
C
C     _______________________________________________________
C
C*    Section 2. Approximate any extra values required
C     _______________________________________________________
C
  200 CONTINUE
C
      DO 210 JAPPRX = ILOOK + 1, KTRUNC
         PBESJ0(JAPPRX) = PBESJ0(JPLOOK) + PPI * (JAPPRX - ILOOK)
  210 CONTINUE
C
C     _______________________________________________________
C
C*    Section 9. Return to calling routine.
C     _______________________________________________________
C
  900 CONTINUE
C
      RETURN
      END

