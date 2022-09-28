C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION LOCAL2K(KSEC1, HFIRST, HSECOND, HTHIRD, HFOURTH)
C
C---->
C**** LOCAL2K
C
C     Purpose
C     -------
C
C     Looks up the description of a parameter in known table 2 versions
C     for WMO code FM 92-X Ext. GRIB.
C
C
C     Interface
C     ---------
C
C     IRET = LOCAL2K(KSEC1, HFIRST, HSECOND, HTHIRD, HFOURTH)
C
C     Input
C     -----
C
C     KSEC1  - Section 1 values for a GRIB product.
C
C
C     Output
C     ------
C
C     HFIRST  - First line of descriptor:  MARS mnemonic
C     HSECOND - Second line of descriptor: Parameter description
C     HTHIRD  - Third line of descriptor:  Units for the parameter
C     HFOURTH - Fourth line of descriptor: Description of units
C
C     Function return value is
C            0 if no error detected.
C        20001 if no match found for the parameter number in
C              the given table.
C        20002 if there is an error opening the file holding
C              the given table.
C
C
C     Method
C     ------
C
C     Calls LOCAL2.
C
C
C     Externals
C     ---------
C     LOCAL2  - looks up information in standard tables.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     Sept 1995
C
C
C     MODIFICATIONS
C     _____________
C
C     J. Clochard, Meteo France, for ECMWF - June 1999.
C     Argument KCENTR added to LOCAL2 call for code tables management.
C
C     _______________________________________________________
C----<
C
C
      IMPLICIT NONE
C
C     Function arguments
      INTEGER KSEC1
      DIMENSION KSEC1(*)
      CHARACTER*(*) HFIRST, HSECOND, HTHIRD, HFOURTH
C
C     Externals
      INTEGER LOCAL2
C
C
C ------------------------------------------------------------------
C*    Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
C
      LOCAL2K = LOCAL2(KSEC1(1), KSEC1(6), KSEC1(2),
     X                 HFIRST, HSECOND, HTHIRD, HFOURTH)
C
C ------------------------------------------------------------------
C*    Section 9.   Closedown.
C ------------------------------------------------------------------
C
C     Normal return.
 900  CONTINUE
C
      RETURN
      END
