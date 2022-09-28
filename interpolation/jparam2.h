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
C     Handling of spectral -> grid interpolation coefficients memory
C
      LOGICAL LFREECF
      INTEGER NFREECF
      INTEGER*8 NISIZE6, NISIZE7
      CHARACTER*20 YOLDGG, YOLDLL
      COMMON /JDCSPGP/ NISIZE6,NISIZE7,NFREECF,LFREECF,YOLDGG,YOLDLL
