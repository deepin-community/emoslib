C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION AURESOL(NS,EW)
C
C---->
C**** AURESOL
C
C     Purpose
C     -------
C
C     Returns the default truncation for a given
C     latitude--longitude grid resolution.
C
C
C     Interface
C     ---------
C
C     KTRUNC = AURESOL(NS,EW)
C
C     Input
C     -----
C
C     NS     = north-south grid increment in degrees
C     EW     = east-west   grid increment in degrees
C
C     Output
C     ------
C
C     KTRUNC = Triangular truncation (e.g. 106 for T106)
C
C
C     Method
C     ------
C
C       Grid increment           Truncation
C       --------------           ----------
C
C       [2.5 and greater         T63
C
C       [1.5 to 2.5[             T106
C
C       [0.6 to 1.5[             T213
C
C       [0.4 to 0.6[             T319
C
C       [0.3 to 0.4[             T511
C
C       [0.15 to 0.3[            T799
C
C       [0.09 to 0.15[           T1279
C
C       [0 to 0.09[              T2047
C
C
C     Externals
C     ---------
C
C     None.
C
C     Author
C     ------
C
C     J.D.Chambers ECMWF Mar 1998
C
C
C     Reference.
C     ----------
C
C     None.
C
C     Modifications
C     -------------
C
C     S.Curic     ECMWF        April 2005
C     Add T799 and T1023
C
C ------------------------------------------------------------------
C----<
C
      IMPLICIT NONE

C     Function arguments
      REAL NS, EW

C     Local variables
      REAL STEP

      STEP = MIN(ABS(NS),ABS(EW))
      IF( STEP.GE.2.5 ) THEN       ! 2.5  <= step        -> T63
        AURESOL =   63
      ELSEIF( STEP.GE.1.5 ) THEN   ! 1.5  <= step < 2.5  -> T106
        AURESOL =  106
C     ELSEIF( STEP.GE.0.4 ) THEN   ! 1.5  <= step < 2.5  -> T191
C       AURESOL =  191
      ELSEIF( STEP.GE.0.6 ) THEN   ! 0.6  <= step < 1.5  -> T213
        AURESOL =  213
      ELSEIF( STEP.GE.0.4 ) THEN   ! 0.4  <= step < 0.6  -> T319
        AURESOL =  319
      ELSEIF( STEP.GE.0.3 ) THEN   ! 0.3  <= step < 0.4  -> T511
        AURESOL =  511
      ELSEIF( STEP.GE.0.15 ) THEN  ! 0.15 <= step < 0.3  -> T799
        AURESOL =  799
      ELSEIF( STEP.GE.0.09 ) THEN  ! 0.09 <= step < 0.15 -> T1279
        AURESOL = 1279
      ELSE                         ! highest truncation/resolution
        AURESOL = 2047
      ENDIF

      END

