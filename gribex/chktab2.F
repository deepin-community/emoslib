C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION CHKTAB2 ( KSEC1,
     X                           HFIRST, HSECOND, HTHIRD, HFOURTH)
C
C---->
C**** CHKTAB2
C
C     Purpose
C     -------
C
C     Checks whether or not a parameter appears in a given code
C     table 2 for WMO code FM 92-X Ext. GRIB (WMO standard, ECMWF local,
C     or even local from another centre).
C
C
C     Interface
C     ---------
C
C     IRET = CHKTAB2(KSEC1, HFIRST, HSECOND, HTHIRD, HFOURTH)
C
C     Input
C     -----
C
C     KSEC1  - Section 1 values for a GRIB product.
C
C
C     Output
C     ------
C
C     HFIRST  - First line of descriptor:  MARS mnemonic
C     HSECOND - Second line of descriptor: Parameter description
C     HTHIRD  - Third line of descriptor:  Units for the parameter
C     HFOURTH - Fourth line of descriptor: Description of units
C
C     Function return value is:
C            0 - Parameter found in table
C        20001 - No match found for the parameter number in the given table.
C        20002 - Error opening the file holding the given table.
C        20003 - Error finding an unused fortran unit number for reading table.
C
C
C     Method
C     ------
C
C     Use TAB2FIL to determine file name for code table.
C
C     Entry for each parameter in table has the format (without quotes):
C        "......................"         Separator
C        "051"                            Paramter number
C        "MX2T24"                         MARS mnemonic
C        "Max 2m temp"                    Parameter description
C        "K"                              Units for the parameter
C        "During previous 24 hours"       Further description
C
C     There is a termination "......................" line in the file
C     and there may be lines of comment before the first parameter entry.
C
C     The current parameter definition file is kept open between calls.
C
C
C     Externals
C     ---------
C     TAB2FIL - Builds code table 2 file name.
C
C
C     Author
C     ------
C
C     J.D.Chambers     ECMWF     March 1999
C
C
C     MODIFICATIONS
C     _____________
C
C     J. Clochard, Meteo France, for ECMWF - June 1999.
C     Argument KSEC1 instead of KPARAM and KTABLE for replacement of
C     LOCAL2K, and code tables management.
C     Use TAB2FIL function for file name building.
C
C     _______________________________________________________
C----<
C
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER JPROUTINE, JPMAXTB, JPDITB
      PARAMETER ( JPROUTINE = 20000, JPMAXTB = 10, JPDITB = 256 )
C
C     Function arguments
C
      INTEGER KSEC1 (*)
C
      CHARACTER*(*) HFIRST, HSECOND, HTHIRD, HFOURTH
C
C     Local variables
C
      LOGICAL LNEWTAB
      INTEGER TABLE2(JPDITB,JPMAXTB), INDTAB, NUMTAB, LOOP, NEXT
      INTEGER IPR, IRET, ISTATUS, ITABLE, IPARAM, ICENTR
C
      CHARACTER*81 YFIRST(2:JPDITB,JPMAXTB), YSECOND(2:JPDITB,JPMAXTB)
      CHARACTER*81 YTHIRD(2:JPDITB,JPMAXTB), YFOURTH(2:JPDITB,JPMAXTB)
      CHARACTER*256 YFILE, YDUMMY
C
      INTEGER NUNIT
C
      SAVE NUNIT, NUMTAB, TABLE2, YFIRST, YSECOND, YTHIRD, YFOURTH
C
C     External functions
C
      INTEGER TAB2FIL
      EXTERNAL TAB2FIL
C
      DATA NUMTAB/0/
C
C
C ------------------------------------------------------------------
C*    Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
C
C     See if a new table is requested.
C
      ITABLE = KSEC1(1)
      IF( KSEC1(22).EQ.98 ) THEN
        ICENTR = 98
      ELSE
        ICENTR = KSEC1(2)
      ENDIF
      IPARAM = KSEC1(6)
C
      IF( MAX( ITABLE, IPARAM) .GT. 127 ) THEN
        ITABLE = 1000*ICENTR + ITABLE
      ENDIF
C
      LNEWTAB = .TRUE.
C     print *, 'CHKTAB2: TABLE2(1,:)=', TABLE2(1,:NUMTAB)
      DO LOOP = 1, NUMTAB
        IF( ITABLE.EQ.TABLE2(1,LOOP) ) THEN
          LNEWTAB = .FALSE.
          INDTAB = LOOP
          GOTO 300
        ENDIF
      ENDDO
C
C     Read values in the new table.
C
      IF( LNEWTAB ) THEN
        NUMTAB = NUMTAB + 1
        IF( NUMTAB.GT.JPMAXTB ) NUMTAB = 1
        INDTAB = NUMTAB
        TABLE2(1,INDTAB) = ITABLE
C
        DO LOOP = 2, JPDITB
          TABLE2(LOOP,INDTAB) = 0
        ENDDO
C
C       Find an unused fortran unit number
C
        IPR = 0
        CALL FTN1CR(NUNIT, IPR, IRET)
        IF( IRET.NE.0 ) THEN
          NUMTAB = NUMTAB - 1
          CHKTAB2 = JPROUTINE + 3
          RETURN
        ENDIF
C
C       Build filename
C
        IRET = TAB2FIL( KSEC1(1), IPARAM, ICENTR, YFILE )
C
C       Open file
C
        OPEN(UNIT=NUNIT, FILE=YFILE, ERR=910, IOSTAT=ISTATUS,
     X       STATUS='OLD',FORM='FORMATTED')
        REWIND(UNIT=NUNIT)
C
C ------------------------------------------------------------------
C*    Section 2.   Fill the new parameter table.
C ------------------------------------------------------------------
C
  200   CONTINUE
        NEXT = 1
C
C       Skip lines until '......................' separator found
C
  210   CONTINUE
        READ(NUNIT,'(A)',END=230) YDUMMY
        IF( YDUMMY(1:22).EQ.'......................') GOTO 225
        GOTO 210
C
C       Loop through file
C
  220   CONTINUE
          READ(NUNIT,'(A)',END=230) YDUMMY
  225     CONTINUE
          NEXT = NEXT + 1
          READ(NUNIT,'(I3.3)',END=230) TABLE2(NEXT,INDTAB)
          READ(NUNIT,'(A)',END=230) YFIRST(NEXT,INDTAB)
          READ(NUNIT,'(A)',END=230) YSECOND(NEXT,INDTAB)
          READ(NUNIT,'(A)',END=230) YTHIRD(NEXT,INDTAB)
          READ(NUNIT,'(A)',END=230) YFOURTH(NEXT,INDTAB)
        GOTO 220
C
 230    CONTINUE
C
        CLOSE(UNIT=NUNIT)
C
      ENDIF
C
C ------------------------------------------------------------------
C*    Section 3.   Search for parameter in the table.
C ------------------------------------------------------------------
C
  300 CONTINUE
C
      CHKTAB2 = JPROUTINE + 1
C
      DO LOOP = 2,JPDITB
        IF( IPARAM.EQ.TABLE2(LOOP,INDTAB) ) THEN
          HFIRST = YFIRST(LOOP,INDTAB)
          HSECOND = YSECOND(LOOP,INDTAB)
          HTHIRD = YTHIRD(LOOP,INDTAB)
          HFOURTH = YFOURTH(LOOP,INDTAB)
          CHKTAB2 = 0
          RETURN
        ENDIF
      ENDDO
C
C ------------------------------------------------------------------
C*    Section 9.   Closedown.
C ------------------------------------------------------------------
C
C     Drop through if not found.
C
      RETURN   
C
C     Error opening table file
C
 910  CONTINUE
      TABLE2(1,NUMTAB) = -1
      NUMTAB = NUMTAB - 1
      CHKTAB2 = JPROUTINE + 2
      RETURN
      END
