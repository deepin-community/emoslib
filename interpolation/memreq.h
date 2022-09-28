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
C**** "memreq.h"
C
C     PURPOSE
C     _______
C
C     This file contains the memory request definition variables.
C
C     INTERFACE
C     _________
C
C     #include "memreq.h"
C
C     Common block usage
C     __________________
C
C     MEMORY
C
C     MADDR        - The base addresses of the currently allocated
C                    memory segments.
C     MREQUEST     - The sizes of the current memory requests.
C
C     METHOD
C     ______
C
C     NONE
C
C     REFERENCE
C     _________
C
C     NONE
C
C     COMMENTS
C     ________
C
C     MREQUEST and MADDR are arrays to allow control of different
C     memory requests at different levels of the interpolation
C     software.
C
C     Contains section 1
C
C     AUTHOR
C     ______
C
C     K. Fielding      *ECMWF*      Jan 1994
C
C     MODIFICATIONS
C     _____________
C
C     NONE
C
C     _______________________________________________________
C
C
C*    Section 1. Input field description
C     _______________________________________________________
C
C     MREQUEST - The size of the current memory request.
C     MADDR    - The base address for the current memory allocation.
C
#ifdef POINTER_64
      INTEGER*8 MADDR(JPLEVEL)
#else
      INTEGER MADDR(JPLEVEL)
#endif
      INTEGER MREQUEST(JPLEVEL)

      SAVE MREQUEST, MADDR

