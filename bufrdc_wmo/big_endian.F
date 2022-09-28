C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      logical function big_endian()

      implicit none

      integer*4            icharacter
      character*4 testcharacter
      integer*4             newint


      testcharacter="ABCD"

      newint=0
      read(testcharacter,'(a4)') icharacter
c     icharacter=transfer(testcharacter,0)

      call mvbits(icharacter, 0, 8, newint, 0)

      if(char(newint) .eq. 'A') then
c         little endian
          big_endian=.false.
      else
c         big endian
          big_endian=.true.
      end if
      end
