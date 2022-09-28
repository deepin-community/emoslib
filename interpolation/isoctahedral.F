C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      LOGICAL FUNCTION ISOCTAHEDRAL(KNUM,KPTS)
C
C---->
C**** *ISOCTAHEDRAL*
C
C     PURPOSE
C     _______
C
C     Check if KPTS (pl array) represents a octahedral progression series
C
C
C     Input parameters
C     ________________
C
C     KNUM - Gaussian grid number
C
C     KPTS - Array giving number of points at each gaussian 
C            latitude, both hemispheres (pl array)
C
C----<
      IMPLICIT NONE


C     Arguments
      INTEGER KNUM, KPTS(*)


C     Local variables
      INTEGER LOOP, IMAGE, NLAT
      CHARACTER*1 HTYPEO
      INTEGER QGOCT
      EXTERNAL QGOCT
      LOGICAL ISGGVALID
      EXTERNAL ISGGVALID


C     Check octahedral progression series
C     (symmetry is checked as well, return on first failure)
      HTYPEO = 'O'
      ISOCTAHEDRAL = ISGGVALID(HTYPEO,KNUM)
      IF( ISOCTAHEDRAL ) THEN
        DO LOOP = 1, KNUM
          IMAGE = 2 * KNUM - LOOP + 1
          NLAT  = QGOCT(LOOP)
          ISOCTAHEDRAL = ( NLAT.EQ.KPTS(LOOP)  )
     X             .AND. ( NLAT.EQ.KPTS(IMAGE) )
          IF( .NOT.ISOCTAHEDRAL ) GOTO 900
        ENDDO
      ENDIF

 900  CONTINUE
      ENDFUNCTION

