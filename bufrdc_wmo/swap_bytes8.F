C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      subroutine swap_bytes8(r1,r2)
c     ---------------------------------
      implicit none

      real*8 r1, r2
      real*8 r18
      real*8 r28
      integer*8 kint18,kint28

      equivalence(r18,kint18)
      equivalence(r28,kint28)



c Reverse order of bytes

      r18=r1

      call mvbits(kint18, 56, 8, kint28, 0)
      call mvbits(kint18, 48, 8, kint28, 8)
      call mvbits(kint18, 40, 8, kint28,16)
      call mvbits(kint18, 32, 8, kint28,24)
      call mvbits(kint18, 24, 8, kint28,32)
      call mvbits(kint18, 16, 8, kint28,40)
      call mvbits(kint18,  8, 8, kint28,48)
      call mvbits(kint18,  0, 8, kint28,56)


      r2=r28

      return
      end
