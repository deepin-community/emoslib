C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      SUBROUTINE UNCMPCK(PSPEC,KTRUNC,KTRUNCNP)
C
C---->
C**** UNCMPCK
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
C     CALL UNCMPCK(PSPEC,KTRUNC,KTRUNCNP)
C
C
C     Input Parameters.
C     -----------------
C
C     PSPEC      - Array of spectral coefficients.
C     KTRUNC     - Truncation of the spherical harmonic coefficients.
C     KTRUNCNP   - Truncation of the subset of spherical harmonic
C                  coefficients to be re-inserted..
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
C     Given the values CCCCCC...C , contiguous in array PSPEC,
C     rearranges them to leave slots for the subset of spectral
C     coefficients to be re-inserted..
C
C                            <subset>|
C
C     Thus:                  cccccccc|CCCCCC
C                              cccccc|CCCCCC
C                                cccc|CCCCCC
C                                  cc|CCCCCC
C                                    |CCCCCC
C                                       CCCC
C                                         CC
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
      INTEGER KTRUNC,KTRUNCNP
      REAL PSPEC((KTRUNC+1)*(KTRUNC+2))
C
C     Local variables.
      INTEGER IMAXP, IMAXUP, IUC, IC, JM, JN, IL2, IL1, ILEN
C
C     -----------------------------------------------------------------
C*    Section 1. Rearrange the values.
C     -----------------------------------------------------------------
C
      IMAXP  = (KTRUNC+1)*(KTRUNC+2)
      IMAXUP = (KTRUNC+1)*(KTRUNC+2)-(KTRUNCNP+1)*(KTRUNCNP+2) 
      IUC = IMAXP+1
      IC  = IMAXUP+1
C
C     Loop through rows, starting at end and working backwards.
      DO 102 JM=KTRUNC,0,-1
C
C       In each row calculate value to start with.
C       For early rows, there are some values to skip to leave slots
C       for the subset being re-instated.
        IL2=MAX(JM,KTRUNCNP+1)
        IL1=IL2-1
        ILEN=2*(KTRUNC-IL2+1)
C
C       Move values for the row.
CDIR$ IVDEP      
        DO 104 JN=1,ILEN
          IUC = IUC-1
          IC  = IC-1
          PSPEC(IUC) = PSPEC(IC)
 104    CONTINUE     
C
        IUC = IUC-MAX((IL1-JM+1)*2,0)
 102  CONTINUE
C
C     -----------------------------------------------------------------
C*    Section 9. Return.
C     -----------------------------------------------------------------
C
  900 CONTINUE
C
      RETURN
      END        
