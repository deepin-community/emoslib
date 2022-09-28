C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION C2RNGE ( KNPACK, KPKLEN, KXBITS, KDATA,  KDIAGO,
     X                          KWIDTH, KREFER, KPOWER, KMXPWR, KPCWID,
     X                          KXCWID )
C
C---->
C**** C2RNGE
C
C     Purpose.
C     --------
C
C     Utility to assist computing of range diagnostics for spatial
C     differencing.
C
C**   Interface.
C     ----------
C
C     IOFF = C2RNGE ( KNPACK, KPKLEN, KXBITS, KDATA, KDIAGO, KWIDTH,
C    X                KREFER, KPOWER, KMXPWR, KPCWID, KXCWID )
C
C     Function result value is the number of values processed within
C     KDATA during call.
C
C
C     Input Parameters.
C     -----------------
C
C     KNPACK     - Number of packets to process.
C     KPKLEN     - Length of packets.
C     KXBITS     - MaXimum number of BITS estimated.
C     KDATA      - Array containing values to process.
C     KDIAGO     - Diagnostic of cumulated ranges.
C     KWIDTH     - Width of packets.
C     KREFER     - Reference value.
C     KPOWER     - Auxilary array, precomputed (powers of 2)-1.
C     KMXPWR     - Maximum number of bits per original scaled value.
C     KPCWID     - Auxilary array, precomputed widths.
C     KXCWID     - Maximum width associated to KPCWID.
C
C
C     Output Parameters (updates).
C     ------------------
C
C     KDIAGO     - Diagnostic of cumulated ranges.
C     KWIDTH     - Width of packets.
C     KREFER     - Reference value.
C
C
C     Method.
C     -------
C
C     Loop on packets for range diagnostics.
C
C
C
C     Externals.
C     ----------
C
C     None.
C
C
C     Reference.
C     ----------
C
C     None.
C
C
C     Comments.
C     --------
C
C     None.
C
C
C     Author.
C     -------
C
C     J. Clochard, Meteo France, for ECMWF - February 1998.
C
C
C     Modifications.
C     _____________
C
C     J. Clochard, June 1999.
C     Use precomputed array to determine widths . Dummy-arguments
C     KPCWID and KXCWID added . Assumption is made that KXCWID
C     is at least (KMXPWR+1)/2 .
C
C
C----<
C     -----------------------------------------------------------------
C*    Section 0. Definition of variables.
C     -----------------------------------------------------------------
C
      IMPLICIT NONE
C
C     Subroutine arguments
C
      INTEGER KNPACK, KPKLEN, KXBITS, KDIAGO, KWIDTH, KREFER, KMXPWR
      INTEGER KXCWID
      INTEGER KPOWER (0:KMXPWR), KDATA (*), KPCWID (0:*)
C
C     Local variables.
C
      INTEGER IOFF, J, J2, IMAX, IMIN, IRANGE, IPOWER, IWIDTH, ILCWID
C
C     -----------------------------------------------------------------
C*    Section 1 . Direct computing.
C     -----------------------------------------------------------------
C
  100 CONTINUE
C
      IOFF = 0
      ILCWID = KPOWER(KXCWID)+1
C
      DO 104 J2=1,KNPACK
C
      IMAX=KDATA(IOFF+1)
      IMIN=KDATA(IOFF+1)
C
      DO 101 J=2,KPKLEN
      IMAX=MAX (IMAX,KDATA(IOFF+J))
      IMIN=MIN (IMIN,KDATA(IOFF+J))
  101 CONTINUE
C
      IRANGE=IMAX-IMIN
C
      IF (IRANGE.LT.ILCWID) THEN
        IWIDTH=KPCWID(IRANGE)
      ELSE
        IWIDTH=KXCWID+KPCWID(IRANGE/ILCWID)
      ENDIF
C
      IPOWER=KPOWER(IWIDTH)
C
      KDIAGO=KDIAGO+KPKLEN*IWIDTH
      KWIDTH=MAX (KWIDTH,IWIDTH)
      KREFER=MAX (KREFER,IMIN-(IPOWER-IRANGE))
      IOFF=IOFF+KPKLEN
C
  104 CONTINUE
C
C     -----------------------------------------------------------------
C*    Section 9 . Return to calling routine.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      C2RNGE = IOFF
C
      RETURN
C
      END
