c Copyright 1981-2016 ECMWF.
c
c This software is licensed under the terms of the Apache Licence 
c Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
c
c In applying this licence, ECMWF does not waive the privileges and immunities 
c granted to it by virtue of its status as an intergovernmental organisation 
c nor does it submit to any jurisdiction.
c

      subroutine gpfa3(a,b,trigs,inc,jump,n,mm,lot,isign)
*     fortran version of *gpfa3* -
*     radix-3 section of self-sorting, in-place
*        generalized PFA
*
*-------------------------------------------------------------------
*
      dimension a(*), b(*), trigs(*)
      data sin60/0.866025403784437/
      data lvr/128/
*
*     ***************************************************************
*     *                                                             *
*     *  N.B. LVR = LENGTH OF VECTOR REGISTERS, SET TO 128 FOR C90. *
*     *  RESET TO 64 FOR OTHER CRAY MACHINES, OR TO ANY LARGE VALUE *
*     *  (GREATER THAN OR EQUAL TO LOT) FOR A SCALAR COMPUTER.      *
*     *                                                             *
*     ***************************************************************
*
      n3 = 3**mm
      inq = n/n3
      jstepx = (n3-n) * inc
      ninc = n * inc
      ink = inc * inq
      mu = mod(inq,3)
      if (isign.eq.-1) mu = 3-mu
      m = mm
      mh = (m+1)/2
      s = float(isign)
      c1 = sin60
      if (mu.eq.2) c1 = -c1
*
      nblox = 1 + (lot-1)/lvr
      left = lot
      s = float(isign)
      istart = 1
*
*  loop on blocks of lvr transforms
*  --------------------------------
      do 500 nb = 1 , nblox
*
      if (left.le.lvr) then
         nvex = left
      else if (left.lt.(2*lvr)) then
         nvex = left/2
         nvex = nvex + mod(nvex,2)
      else
         nvex = lvr
      endif
      left = left - nvex
*
      la = 1
*
*  loop on type I radix-3 passes
*  -----------------------------
      do 160 ipass = 1 , mh
      jstep = (n*inc) / (3*la)
      jstepl = jstep - ninc
*
*  k = 0 loop (no twiddle factors)
*  -------------------------------
      do 120 jjj = 0 , (n-1)*inc , 3*jstep
      ja = istart + jjj
*
*  "transverse" loop
*  -----------------
      do 115 nu = 1 , inq
      jb = ja + jstepl
      if (jb.lt.istart) jb = jb + ninc
      jc = jb + jstepl
      if (jc.lt.istart) jc = jc + ninc
      j = 0
*
*  loop across transforms
*  ----------------------
cdir$ ivdep, shortloop
      do 110 l = 1 , nvex
      t1 = a(jb+j) + a(jc+j)
      t2 = a(ja+j) - 0.5 * t1
      t3 = c1 * ( a(jb+j) - a(jc+j) )
      u1 = b(jb+j) + b(jc+j)
      u2 = b(ja+j) - 0.5 * u1
      u3 = c1 * ( b(jb+j) - b(jc+j) )
      a(ja+j) = a(ja+j) + t1
      b(ja+j) = b(ja+j) + u1
      a(jb+j) = t2 - u3
      b(jb+j) = u2 + t3
      a(jc+j) = t2 + u3
      b(jc+j) = u2 - t3
      j = j + jump
  110 continue
      ja = ja + jstepx
      if (ja.lt.istart) ja = ja + ninc
  115 continue
  120 continue
*
*  finished if n3 = 3
*  ------------------
      if (n3.eq.3) go to 490
      kk = 2 * la
*
*  loop on nonzero k
*  -----------------
      do 150 k = ink , jstep-ink , ink
      co1 = trigs(kk+1)
      si1 = s*trigs(kk+2)
      co2 = trigs(2*kk+1)
      si2 = s*trigs(2*kk+2)
*
*  loop along transform
*  --------------------
      do 140 jjj = k , (n-1)*inc , 3*jstep
      ja = istart + jjj
*
*  "transverse" loop
*  -----------------
      do 135 nu = 1 , inq
      jb = ja + jstepl
      if (jb.lt.istart) jb = jb + ninc
      jc = jb + jstepl
      if (jc.lt.istart) jc = jc + ninc
      j = 0
*
*  loop across transforms
*  ----------------------
cdir$ ivdep,shortloop
      do 130 l = 1 , nvex
      t1 = a(jb+j) + a(jc+j)
      t2 = a(ja+j) - 0.5 * t1
      t3 = c1 * ( a(jb+j) - a(jc+j) )
      u1 = b(jb+j) + b(jc+j)
      u2 = b(ja+j) - 0.5 * u1
      u3 = c1 * ( b(jb+j) - b(jc+j) )
      a(ja+j) = a(ja+j) + t1
      b(ja+j) = b(ja+j) + u1
      a(jb+j) = co1*(t2-u3) - si1*(u2+t3)
      b(jb+j) = si1*(t2-u3) + co1*(u2+t3)
      a(jc+j) = co2*(t2+u3) - si2*(u2-t3)
      b(jc+j) = si2*(t2+u3) + co2*(u2-t3)
      j = j + jump
  130 continue
*-----( end of loop across transforms )
      ja = ja + jstepx
      if (ja.lt.istart) ja = ja + ninc
  135 continue
  140 continue
*-----( end of loop along transforms )
      kk = kk + 2*la
  150 continue
*-----( end of loop on nonzero k )
      la = 3*la
  160 continue
*-----( end of loop on type I radix-3 passes)
*
*  loop on type II radix-3 passes
*  ------------------------------
  400 continue
*
      do 480 ipass = mh+1 , m
      jstep = (n*inc) / (3*la)
      jstepl = jstep - ninc
      laincl = la*ink - ninc
*
*  k=0 loop (no twiddle factors)
*  -----------------------------
      do 430 ll = 0 , (la-1)*ink , 3*jstep
*
      do 420 jjj = ll , (n-1)*inc , 3*la*ink
      ja = istart + jjj
*
*  "transverse" loop
*  -----------------
      do 415 nu = 1 , inq
      jb = ja + jstepl
      if (jb.lt.istart) jb = jb + ninc
      jc = jb + jstepl
      if (jc.lt.istart) jc = jc + ninc
      jd = ja + laincl
      if (jd.lt.istart) jd = jd + ninc
      je = jd + jstepl
      if (je.lt.istart) je = je + ninc
      jf = je + jstepl
      if (jf.lt.istart) jf = jf + ninc
      jg = jd + laincl
      if (jg.lt.istart) jg = jg + ninc
      jh = jg + jstepl
      if (jh.lt.istart) jh = jh + ninc
      ji = jh + jstepl
      if (ji.lt.istart) ji = ji + ninc
      j = 0
*
*  loop across transforms
*  ----------------------
cdir$ ivdep, shortloop
      do 410 l = 1 , nvex
      t1 = a(jb+j) + a(jc+j)
      t2 = a(ja+j) - 0.5 * t1
      t3 = c1 * ( a(jb+j) - a(jc+j) )
      a(jb+j) = a(jd+j)
      u1 = b(jb+j) + b(jc+j)
      u2 = b(ja+j) - 0.5 * u1
      u3 = c1 * ( b(jb+j) - b(jc+j) )
      b(jb+j) = b(jd+j)
      a(ja+j) = a(ja+j) + t1
      b(ja+j) = b(ja+j) + u1
      a(jd+j) = t2 - u3
      b(jd+j) = u2 + t3
      a(jc+j) = t2 + u3
      b(jc+j) = u2 - t3
*----------------------
      t1 = a(je+j) + a(jf+j)
      t2 = a(jb+j) - 0.5 * t1
      t3 = c1 * ( a(je+j) - a(jf+j) )
      a(jf+j) = a(jh+j)
      u1 = b(je+j) + b(jf+j)
      u2 = b(jb+j) - 0.5 * u1
      u3 = c1 * ( b(je+j) - b(jf+j) )
      b(jf+j) = b(jh+j)
      a(jb+j) = a(jb+j) + t1
      b(jb+j) = b(jb+j) + u1
      a(je+j) = t2 - u3
      b(je+j) = u2 + t3
      a(jh+j) = t2 + u3
      b(jh+j) = u2 - t3
*----------------------
      t1 = a(jf+j) + a(ji+j)
      t2 = a(jg+j) - 0.5 * t1
      t3 = c1 * ( a(jf+j) - a(ji+j) )
      t1 = a(jg+j) + t1
      a(jg+j) = a(jc+j)
      u1 = b(jf+j) + b(ji+j)
      u2 = b(jg+j) - 0.5 * u1
      u3 = c1 * ( b(jf+j) - b(ji+j) )
      u1 = b(jg+j) + u1
      b(jg+j) = b(jc+j)
      a(jc+j) = t1
      b(jc+j) = u1
      a(jf+j) = t2 - u3
      b(jf+j) = u2 + t3
      a(ji+j) = t2 + u3
      b(ji+j) = u2 - t3
      j = j + jump
  410 continue
*-----( end of loop across transforms )
      ja = ja + jstepx
      if (ja.lt.istart) ja = ja + ninc
  415 continue
  420 continue
  430 continue
*-----( end of double loop for k=0 )
*
*  finished if last pass
*  ---------------------
      if (ipass.eq.m) go to 490
*
      kk = 2*la
*
*     loop on nonzero k
*     -----------------
      do 470 k = ink , jstep-ink , ink
      co1 = trigs(kk+1)
      si1 = s*trigs(kk+2)
      co2 = trigs(2*kk+1)
      si2 = s*trigs(2*kk+2)
*
*  double loop along first transform in block
*  ------------------------------------------
      do 460 ll = k , (la-1)*ink , 3*jstep
*
      do 450 jjj = ll , (n-1)*inc , 3*la*ink
      ja = istart + jjj
*
*  "transverse" loop
*  -----------------
      do 445 nu = 1 , inq
      jb = ja + jstepl
      if (jb.lt.istart) jb = jb + ninc
      jc = jb + jstepl
      if (jc.lt.istart) jc = jc + ninc
      jd = ja + laincl
      if (jd.lt.istart) jd = jd + ninc
      je = jd + jstepl
      if (je.lt.istart) je = je + ninc
      jf = je + jstepl
      if (jf.lt.istart) jf = jf + ninc
      jg = jd + laincl
      if (jg.lt.istart) jg = jg + ninc
      jh = jg + jstepl
      if (jh.lt.istart) jh = jh + ninc
      ji = jh + jstepl
      if (ji.lt.istart) ji = ji + ninc
      j = 0
*
*  loop across transforms
*  ----------------------
cdir$ ivdep, shortloop
      do 440 l = 1 , nvex
      t1 = a(jb+j) + a(jc+j)
      t2 = a(ja+j) - 0.5 * t1
      t3 = c1 * ( a(jb+j) - a(jc+j) )
      a(jb+j) = a(jd+j)
      u1 = b(jb+j) + b(jc+j)
      u2 = b(ja+j) - 0.5 * u1
      u3 = c1 * ( b(jb+j) - b(jc+j) )
      b(jb+j) = b(jd+j)
      a(ja+j) = a(ja+j) + t1
      b(ja+j) = b(ja+j) + u1
      a(jd+j) = co1*(t2-u3) - si1*(u2+t3)
      b(jd+j) = si1*(t2-u3) + co1*(u2+t3)
      a(jc+j) = co2*(t2+u3) - si2*(u2-t3)
      b(jc+j) = si2*(t2+u3) + co2*(u2-t3)
*----------------------
      t1 = a(je+j) + a(jf+j)
      t2 = a(jb+j) - 0.5 * t1
      t3 = c1 * ( a(je+j) - a(jf+j) )
      a(jf+j) = a(jh+j)
      u1 = b(je+j) + b(jf+j)
      u2 = b(jb+j) - 0.5 * u1
      u3 = c1 * ( b(je+j) - b(jf+j) )
      b(jf+j) = b(jh+j)
      a(jb+j) = a(jb+j) + t1
      b(jb+j) = b(jb+j) + u1
      a(je+j) = co1*(t2-u3) - si1*(u2+t3)
      b(je+j) = si1*(t2-u3) + co1*(u2+t3)
      a(jh+j) = co2*(t2+u3) - si2*(u2-t3)
      b(jh+j) = si2*(t2+u3) + co2*(u2-t3)
*----------------------
      t1 = a(jf+j) + a(ji+j)
      t2 = a(jg+j) - 0.5 * t1
      t3 = c1 * ( a(jf+j) - a(ji+j) )
      t1 = a(jg+j) + t1
      a(jg+j) = a(jc+j)
      u1 = b(jf+j) + b(ji+j)
      u2 = b(jg+j) - 0.5 * u1
      u3 = c1 * ( b(jf+j) - b(ji+j) )
      u1 = b(jg+j) + u1
      b(jg+j) = b(jc+j)
      a(jc+j) = t1
      b(jc+j) = u1
      a(jf+j) = co1*(t2-u3) - si1*(u2+t3)
      b(jf+j) = si1*(t2-u3) + co1*(u2+t3)
      a(ji+j) = co2*(t2+u3) - si2*(u2-t3)
      b(ji+j) = si2*(t2+u3) + co2*(u2-t3)
      j = j + jump
  440 continue
*-----(end of loop across transforms)
      ja = ja + jstepx
      if (ja.lt.istart) ja = ja + ninc
  445 continue
  450 continue
  460 continue
*-----( end of double loop for this k )
      kk = kk + 2*la
  470 continue
*-----( end of loop over values of k )
      la = 3*la
  480 continue
*-----( end of loop on type II radix-3 passes )
*-----( nvex transforms completed)
  490 continue
      istart = istart + nvex * jump
  500 continue
*-----( end of loop on blocks of transforms )
*
      return
      end
