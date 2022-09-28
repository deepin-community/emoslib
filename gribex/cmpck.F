C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE CMPCK(PSPEC,KTRUNC,KTRUNCNP)
C
C---->
C**** CMPCK
C
C     Purpose.
C     --------
C
C     Puts spectral coefficients in contiguous locations.
C
C
C**   Interface.
C     ----------
C
C     CALL CMPCK(PSPEC,KTRUNC,KTRUNCNP)
C
C
C     Input Parameters.
C     -----------------
C
C     PSPEC      - Array of spectral coefficients.
C     KTRUNC     - Truncation of the spherical harmonic coefficients.
C     KTRUNCNP   - Truncation of the subset of spherical harmonic
C                  coefficients to be ignored.
C
C
C     Output Parameters.
C     -----------------
C
C     PSPEC      - Array of spectral coefficients rearranged.
C
C
C     Method.
C     -------
C
C                            <subset>|
C
C     Given numbers:         cccccccc|CCCCCC
C                              cccccc|CCCCCC
C                                cccc|CCCCCC
C                                  cc|CCCCCC
C                                    |CCCCCC
C                                       CCCC
C                                         CC
C
C    Removes the subset and makes the CCCCCC...C values contiguous
C    in array PSPEC.
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
C     M.Hamrut         ECMWF      09:05:94
C
C
C     Modifications.
C     _____________
C
C     None.
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
      INTEGER KTRUNC, KTRUNCNP
      REAL PSPEC
      DIMENSION PSPEC((KTRUNC+1)*(KTRUNC+2))
C
C     Local variables.
      INTEGER IUC, IC, JM, JN, IL2, IL1, ILEN
C
C
C     -----------------------------------------------------------------
C*    Section 1. Move only the required values (overwriting).
C     -----------------------------------------------------------------
C
      IUC=0
      IC =0
C
C     Loop through rows.
      DO 102 JM=0,KTRUNC
C
C       In each row calculate value to start with.
C       For early rows there are some value to skip (in the subset
C       being ignored).
        IL2=MAX(JM,KTRUNCNP+1)
        IL1=IL2-1
        IUC=IUC+MAX((IL1-JM+1)*2,0)
        ILEN=2*(KTRUNC-IL2+1)
C
C       Move values for the row.
CDIR$ IVDEP      
!OCL NOVREC
        DO 104 JN=1,ILEN
          IUC = IUC+1
          IC  = IC+1
          PSPEC(IC) = PSPEC(IUC)
 104    CONTINUE     
 102  CONTINUE
C
C     -----------------------------------------------------------------
C*    Section 9. Return.
C     -----------------------------------------------------------------
C
 900  CONTINUE
      RETURN
      END        
