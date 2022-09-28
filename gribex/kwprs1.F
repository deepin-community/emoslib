C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE KWPRS1 (KSEC0,KSEC1)
C
C---->
C**** KWPRS1
C
C     Purpose.
C     --------
C
C     Print the information in the Product Definition
C     Section (Section 1) of decoded Washington ensemble GRIB data.
C
C**   Interface.
C     ----------
C
C     CALL KWPRS1 (KSEC0,KSEC1)
C
C
C     Input Parameters.
C     -----------------
C
C     KSEC0 - Array of decoded parameters from Section 0.
C
C     KSEC1 - Array of decoded parameters from Section 1.
C
C
C     Output Parameters.
C     ------------------
C
C     None.
C
C
C     Method.
C     -------
C
C     None.
C
C
C     Externals.
C     ----------
C
C     PRTBIN
C
C
C     Reference.
C     ----------
C
C     WMO Manual on Codes re GRIB Code.
C     See also routine GRIBEX.
C     http://www.nco.ncep.noaa.gov/pmb/docs/on388/appendixc.html. 
C
C
C     Comments.
C     ---------
C
C
C     Washington ensemble product usage of local part of section 1 of
C     GRIB:
C
C     KSEC1(37) decoded from Octet 41.
C     Identifies the application:
C                1   Ensemble.
C
C     KSEC1(38) decoded from Octet 42.
C     Type:
C                1   Unperturbed control forecast,
C                2   Individual negatively perturbed forecast,
C                3   Individual positively perturbed forecast,
C                4   Cluster,
C                5   Whole ensemble.
C
C     KSEC1(39) decoded from Octet 43.
C     Identification number:
C                1   If byte 42 = 1, identifies high resolution.
C                2   If byte 42 = 1, identifies low resolution.
C                1-5 If byte 42 = 2 or 3, number of a member of a pair
C                    of (negative perturbation, positive perturbation) 
C
C     KSEC1(40) decoded from Octet 44.
C     Product identifier:
C                1   Full field (individual forecast)/unweighted mean.
C                2   Weighted mean.
C                11  Standard deviation wrt ensemble mean.
C                11  Standard deviation wrt ensemble mean, normalized.
C
C     KSEC1(41) decoded from Octet 45.
C     Spatial smoothing of product (number of highest total
C     wavenumber included):
C                255 Original resolution retained.
C
C     Following are used only if needed.
C
C     Probability Section
C     -------------------
C
C     KSEC1(42) decoded from Octet 46.
C     Probability product definition.
C     If octet 9 = 191 or 192, defines variable in terms of octet 9 table
C
C     KSEC1(43) decoded from Octet 47.
C     Probability type 
C                1 = Probability of event below lower limit 
C                2 = Probability of event above upper limit 
C                3 = Probability of event between lower and upper limits 
C
C     KSEC1(44) decoded from Octets 48 - 51
C     Probability lower limit (unit as in octet 9; floating point number)
C
C     KSEC1(45) decoded from Octets 52 - 55
C     Probability upper limit (unit as in octet 9; floating point number)
C
C     Octets 56 - 60 are reserved.
C
C     Following are used only if octet 42 = 4 or 5.
C
C     Clustering Section
C     ------------------
C
C     KSEC1(46) decoded from Octet 61.
C     Ensemble size (number of members, including controls)
C
C     KSEC1(47) decoded from Octet 62.
C     Cluster size (number of members)
C
C     KSEC1(48) decoded from Octet 63.
C     Number of clusters 
C
C     KSEC1(49) decoded from Octet 64.
C     Clustering Method
C                1 = AC 
C                2 = RMS 
C
C     KSEC1(50) decoded from Octets 65 - 67
C     Northern latitude of clustering domain (in thousandths of degree) 
C
C     KSEC1(51) decoded from Octets 68 - 70
C     Southern latitude of clustering domain (in thousandths of degree) 
C
C     KSEC1(52) decoded from Octets 71 - 73
C     Eastern longitude of clustering domain (in thousandths of degree) 
C
C     KSEC1(53) decoded from Octets 74 - 76
C     Western longitude of clustering domain (in thousandths of degree) 
C
C     KSEC1(54 - 63) decoded from Octets 77 - 86
C     Cluster Membership (If octet 42 = 4)
C     Indicates in binary (on/off) format, which members of the ensemble
C     belong to the current cluster. Membership table defined by product
C     generators. 
C
C
C
C     Author.
C     -------
C
C     J.D.Chambers   ECMWF 09.10.95
C
C
C     Modifications.
C     --------------
C
C     None.
C
C
C----<
C     -----------------------------------------------------------------
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------
C
      IMPLICIT NONE
C
#include "common/grprs.h"
C
C     Parameters
      INTEGER KSEC0
      INTEGER KSEC1
      DIMENSION KSEC0(*)
      DIMENSION KSEC1(*)
C
C     Local variables
C
      INTEGER LOOP
      INTEGER*4 ILIMIT
      REAL*4    RLIMIT
      EQUIVALENCE(ILIMIT,RLIMIT)
C
C     -----------------------------------------------------------------
C*    Section 1 . Print required information.
C     ----------------------------------------------------------------- C
  100 CONTINUE
C
C     -----------------------------------------------------------------
C*    Section 2 . Print Washington ensemble product information.
C     -----------------------------------------------------------------
C
  200 CONTINUE
C
C     Application Identifier.
C
      WRITE(GRPRSM,9000) KSEC1(37)
C
C     Type.
C
      IF(KSEC1(38).GT.5) WRITE(GRPRSM,9100) KSEC1(38)
      IF(KSEC1(38).EQ.1) WRITE(GRPRSM,9101) KSEC1(38)
      IF(KSEC1(38).EQ.2) WRITE(GRPRSM,9102) KSEC1(38)
      IF(KSEC1(38).EQ.3) WRITE(GRPRSM,9103) KSEC1(38)
      IF(KSEC1(38).EQ.4) WRITE(GRPRSM,9104) KSEC1(38)
      IF(KSEC1(38).EQ.5) WRITE(GRPRSM,9105) KSEC1(38)
C
C     Identification number.
C
      IF(KSEC1(38).NE.1) THEN
        WRITE(GRPRSM,9200) KSEC1(39)
      ELSE
        IF(KSEC1(39).EQ.1) THEN
          WRITE(GRPRSM,9201) KSEC1(39)
        ELSE IF(KSEC1(39).EQ.2) THEN
          WRITE(GRPRSM,9202) KSEC1(39)
        ELSE
          WRITE(GRPRSM,9200) KSEC1(39)
        ENDIF
      ENDIF
C
C     Product.
C
      IF(KSEC1(40).EQ.1) THEN
        WRITE(GRPRSM,9301) KSEC1(40)
      ELSE IF(KSEC1(40).EQ.2) THEN
        WRITE(GRPRSM,9302) KSEC1(40)
      ELSE IF(KSEC1(40).EQ.11) THEN
        WRITE(GRPRSM,9311) KSEC1(40)
      ELSE IF(KSEC1(40).EQ.12) THEN
        WRITE(GRPRSM,9312) KSEC1(40)
      ELSE
        WRITE(GRPRSM,9300) KSEC1(40)
      ENDIF
C
C     Spatial smoothing of product.
C
      IF( KSEC1(41).EQ.255) THEN
        WRITE(GRPRSM,9400) KSEC1(41)
      ELSE
        WRITE(GRPRSM,9401) KSEC1(41)
      ENDIF
C
C     Probability Section
C
      IF( KSEC1(42).NE.0) THEN
C
C       Probability type
C
        WRITE(GRPRSM,9500) KSEC1(42)
        IF( KSEC1(43).EQ.1 ) THEN
          WRITE(GRPRSM,9501) KSEC1(43)
        ELSE IF( KSEC1(43).EQ.2 ) THEN
          WRITE(GRPRSM,9502) KSEC1(43)
        ELSE IF( KSEC1(43).EQ.3 ) THEN
          WRITE(GRPRSM,9503) KSEC1(43)
        ELSE
          WRITE(GRPRSM,9504) KSEC1(43)
        ENDIF
C
C       Limits
C
        ILIMIT = KSEC1(44)
        WRITE(GRPRSM,9505) RLIMIT
C
        ILIMIT = KSEC1(45)
        WRITE(GRPRSM,9506) RLIMIT
      ENDIF
C
C     Clustering Section
C
      IF( KSEC1(46).NE.0) THEN
C
C       Ensemble size
C
        WRITE(GRPRSM,9507) KSEC1(46)
C
        IF( KSEC1(47).NE.0 ) THEN 
C
C         Cluster size
C         Number of clusters
C         Clustering Method
C
          WRITE(GRPRSM,9508) KSEC1(47)
          WRITE(GRPRSM,9509) KSEC1(48)
          WRITE(GRPRSM,9510) KSEC1(49)
C
C         North/South/East/West limits of clustering domain
C
          WRITE(GRPRSM,9511) KSEC1(50)
          WRITE(GRPRSM,9511) KSEC1(51)
          WRITE(GRPRSM,9511) KSEC1(52)
          WRITE(GRPRSM,9511) KSEC1(53)
C
C         Cluster Membership
C
          IF( KSEC1(38).EQ.4 ) THEN
            WRITE(GRPRSM,9515)
            WRITE(GRPRSM,9516)
            DO LOOP = 1, KSEC1(46)
              IF( KSEC1(53+LOOP).EQ.1 ) THEN
                WRITE(GRPRSM,9517) LOOP
              ELSE
                WRITE(GRPRSM,9518) LOOP
              ENDIF
            ENDDO
          ENDIF
C
        ENDIF
C
      ENDIF
C
C     -----------------------------------------------------------------
C*    Section 9 . Format statements. Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
C
 9000 FORMAT(1H ,'Application Identifier.(1=Ensemble)  ',I9)
C
 9100 FORMAT(1H ,'Type:                                ',I9)
 9101 FORMAT(1H ,'Type: Unperturbed control forecast.  ',I9)
 9102 FORMAT(1H ,'Type: Individual -vely perturbed fc. ',I9)
 9103 FORMAT(1H ,'Type: Individual +vely perturbed fc. ',I9)
 9104 FORMAT(1H ,'Type: Cluster.                       ',I9)
 9105 FORMAT(1H ,'Type: Whole ensemble.                ',I9)
C
 9200 FORMAT(1H ,'Identification number.               ',I9)
 9201 FORMAT(1H ,'Identification number: (1=high res)  ',I9)
 9202 FORMAT(1H ,'Identification number: (2=low res)   ',I9)
C
 9300 FORMAT(1H ,'Product.                             ',I9)
 9301 FORMAT(1H ,'Full field/unweighted mean.          ',I9)
 9302 FORMAT(1H ,'Weighted mean.                       ',I9)
 9311 FORMAT(1H ,'Standard deviation wrt ensemble mean.',I9)
 9312 FORMAT(1H ,'Stand.dev. wrt ens. mean, normalized.',I9)
C
 9400 FORMAT(1H ,'Original spatial smoothing retained. ',I9)
 9401 FORMAT(1H ,'Spatial smoothing of product:        ',I9)
C
 9500 FORMAT(1H ,'Probability product definition       ',I9)
 9501 FORMAT(1H ,'Prob. of event below lower limit     ',I9)
 9502 FORMAT(1H ,'Prob. of event above upper limit     ',I9)
 9503 FORMAT(1H ,'Prob. of event between limits        ',I9)
 9504 FORMAT(1H ,'Unknown probability type             ',I9)
 9505 FORMAT(1H ,'Probability lower limit        ',F15.6)
 9506 FORMAT(1H ,'Probability upper limit        ',F15.6)
 9507 FORMAT(1H ,'Ensemble size (members + controls)   ',I9)
 9508 FORMAT(1H ,'Cluster size (number of members)     ',I9)
 9509 FORMAT(1H ,'Number of clusters                   ',I9)
 9510 FORMAT(1H ,'Clustering Method (1 = AC, 2 = RMS)  ',I9)
 9511 FORMAT(1H ,'Northern latitude of domain (x1000)  ',I9) 
 9512 FORMAT(1H ,'Southern latitude of domain (x1000)  ',I9) 
 9513 FORMAT(1H ,'Eastern longitude of domain (x1000)  ',I9) 
 9514 FORMAT(1H ,'Western longitude of domain (x1000)  ',I9) 
 9515 FORMAT(1H ,'Cluster Membership')
 9516 FORMAT(1H ,'------------------')
 9517 FORMAT(1H ,'Ensemble member',I3,' in cluster')
 9518 FORMAT(1H ,'Ensemble member',I3,' not in cluster')
C
      END
