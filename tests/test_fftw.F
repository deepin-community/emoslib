C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C
      program fftw
      implicit none
      
      integer n
      integer m
      integer i
      integer j
      parameter (n=2*2*3*5)
      parameter (m=20)

      real    a     ((n+2)*m)
      real    trigs (n)
      real    work  (m*(n+1))
      integer ifax  (10)

#include "fftw3.f"
      integer*8 plan


      integer KLONO
      integer JPLONO
      real del
      real angle
      real PZFA((n+2)*m)
      real FFTWOUT(n)
      REAL PPI
      PARAMETER ( PPI = 3.14159265358979 )


C     Generate sines and cosines for angles 0 to pi in N steps
C     (possibly the only surviving part of set99)
      del = (2.0*ppi) / float(n)
      do i = 0, (n/2)-1
        angle        = float(i)*del
        trigs(2*i+1) = cos(angle)
        trigs(2*i+2) = sin(angle)
      enddo


      call set99(trigs,ifax,n)

      a = 0
      do j = 1, m
        do i = 1, n
          a((j-1)*(n+2)+i) = j
        enddo
      enddo

      call fft99(a,work,trigs,ifax,1,n+2,n,m,-1)

      KLONO  = n
      JPLONO = n
      PZFA   = a

      FFTWOUT(1:KLONO) = 0.
      CALL DFFTW_PLAN_DFT_C2R_1D( PLAN, KLONO, PZFA, FFTWOUT,
     .  FFTW_ESTIMATE
     .+ FFTW_UNALIGNED
     .+ FFTW_NO_SIMD )
      CALL DFFTW_EXECUTE( PLAN )
      PZFA( 1:KLONO              ) = FFTWOUT(1:KLONO) 
      PZFA( KLONO+1:(JPLONO+2)*m ) = 0.
      a = PZFA
      CALL DFFTW_DESTROY_PLAN( PLAN )

      write (*,'(4(f20.12,2x))') (a(i),i=1,n)
      end

