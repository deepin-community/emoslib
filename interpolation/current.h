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
C**** "current.h"
C
C     PURPOSE
C     _______
C
C     This file contains information about gaussian field definitions
C     currently held in common blocks.
C
C     INTERFACE
C     _________
C
C     #include "current.h"
C
C     Common block usage
C     __________________
C
C     NIGAUSO - Resolution of input gaussian field definition
C               currently held in RIGAUSS, MILLEN.
C     NOGAUSO - Resolution of output gaussian field definition
C               currently held in ROGAUSS, NOLPTS.
C     HIGAUST - Type (R,O,F,U) of input gaussian field definition
C               currently held in RIGAUSS, MILLEN.
C     HOGAUST - Type (R,O,F,U) of output gaussian field definition
C               currently held in ROGAUSS, NOLPTS.
C
C     Author
C     ______
C
C     J.D.Chambers     ECMWF        Sep 1996
C
C
C     MODIFICATIONS
C     _____________
C
C     None
C
C     _______________________________________________________
C
C
      COMMON /CURRENT_STATE/ NIGAUSO, NOGAUSO, HIGAUST, HOGAUST
      SAVE /CURRENT_STATE/
      INTEGER NIGAUSO,NOGAUSO
      CHARACTER*1 HIGAUST, HOGAUST
