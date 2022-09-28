C
C     Parameters
C
      INTEGER JPEXPAND
cs      PARAMETER (JPEXPAND = 1440 * 721 * 6)
cs expanded for 0.1X0.1 resolution
c      PARAMETER (JPEXPAND = 7200*1801)
c  expanded for N=1280 to regular grid requirements
       PARAMETER (JPEXPAND = 1280*2*1280*4)
C
C     Local variables
C
      INTEGER ISEC0 (JPGRIB_ISEC0), ISEC1 (JPGRIB_ISEC1)
      INTEGER ISEC2 (JPGRIB_ISEC2), ISEC3 (JPGRIB_ISEC3)
      INTEGER ISEC4 (JPGRIB_ISEC4)
      REAL ZSEC2 (JPGRIB_RSEC2), ZSEC3 (JPGRIB_RSEC3)
      INTEGER IZNJDCI
      LOGICAL LUSEHIR
#ifndef _CRAYFTN
#ifdef POINTER_64
      INTEGER*8 IZNFLDI
#endif
#endif
C
      POINTER ( IZNFLDI, ZNFELDI )
C
C     Pointer for dynamically allocated array:
C
C     REAL ZNFELDI (JPEXPAND)
C
      COMMON /INTFCM/ ZSEC2, ZSEC3, IZNFLDI, IZNJDCI,
     X                ISEC0, ISEC1, ISEC2, ISEC3, ISEC4, LUSEHIR
C
      REAL ZNFELDI(1)
