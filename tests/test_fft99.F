C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C
      program fft
      parameter (n=2*2*3*5,m=20)
      dimension a((n+2)*m),trigs(n),work(m*(n+1))
      integer ifax(10)
      call set99(trigs,ifax,n)
      do 11 i=1,(n+2)*m
      a(i)=0
11    continue
      do 1 j=1,m
      do 1 i=1,n
      a((j-1)*(n+2)+i)=j
1     continue
c     write (2,*)(a(i),i=1,n)
c      t1=secnds(real(0.))
c     write(*,*)t1
      call fft99(a,work,trigs,ifax,1,n+2,n,m,-1)
c      t2=secnds(real(t1))
c      write(*,*)t2
      write (*,2)(a(i),i=1,n)
 2     format(4(f20.12,2x))
      end

