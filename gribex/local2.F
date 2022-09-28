C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION LOCAL2(KTABLE, KPARAM, KCENTR,
     X                        HFIRST, HSECOND, HTHIRD, HFOURTH)
C
C---->
C**** LOCAL2
C
C     Purpose
C     -------
C
C     Looks up the description of a parameter in known table 2 versions
C     for WMO code FM 92-X Ext. GRIB.
C
C
C     Interface
C     ---------
C
C     IRET = LOCAL2(KTABLE, KPARAM, KCENTR,
C                   HFIRST, HSECOND, HTHIRD, HFOURTH)
C
C     Input
C     -----
C
C     KTABLE - Local table 2 version number.
C     KPARAM - Parameter number.
C     KCENTR - Centre number.
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
C     Function return value is
C            0 if no error detected.
C        20001 if no match found for the parameter number in
C              the given table.
C        20002 if there is an error opening the file holding
C              the given table.
C        20003 if there is an error finding an unused fortran unit
C              number for reading the table.
C
C
C     Method
C     ------
C
C     Checks if environment variable ECMWF_LOCAL_TABLE_PATH exists to
C     define the location of the parameter tables.
C
C     Otherwise, looks for table xxx in:
C       /home/ecmwf/emos_sms/tables/gribex/nnnnnn/local_table_2_version_xxx.
C     where nnnnnn is the current EMOSLIB version.
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
C     The parameter definition file is kept open between calls.
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
C     J.D.Chambers     ECMWF     Sept 1995
C
C
C     MODIFICATIONS
C     _____________
C
C     J. Clochard, Meteo France, for ECMWF - June 1999.
C     Argument KCENTR added for code tables management.
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
      INTEGER JPROUTINE
      PARAMETER ( JPROUTINE = 20000 )
C
C     Function arguments
C
      INTEGER KPARAM, KTABLE, KCENTR
      CHARACTER*(*) HFIRST, HSECOND, HTHIRD, HFOURTH
C
C     Local variables
C
      INTEGER IPR, IRET, ISTATUS, IPARAM, ITABLE
      CHARACTER*256 YFILE, YDUMMY
C
      INTEGER NOLDTAB, NUNIT
      SAVE NOLDTAB, NUNIT
C
C     External functions
C
      INTEGER TAB2FIL
      EXTERNAL TAB2FIL
C
      DATA NOLDTAB/-1/
C
C
C ------------------------------------------------------------------
C*    Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
C
      LOCAL2 = 0
C
C     Put phoney fields in return parameters
C
      HFIRST  = 'NONE'
      HSECOND = 'Undefined parameter'
      HTHIRD  = 'Undefined parameter'
      HFOURTH = 'Undefined parameter'
C
C     See if a new table is requested.
C
      IF( MAX( KTABLE, KPARAM) .LE. 127 ) THEN
        ITABLE = KTABLE
      ELSE
        ITABLE = 1000*KCENTR + KTABLE
      ENDIF
C
      IF( ITABLE.NE.NOLDTAB ) THEN
        IF( NOLDTAB.NE.-1 ) CLOSE(UNIT=NUNIT)
        NOLDTAB = ITABLE
C
C      Find an unused fortran unit number
C
        IPR = 0
        CALL FTN1CR(NUNIT, IPR, IRET)
        IF( IRET.NE.0 ) THEN
          LOCAL2 = JPROUTINE + 3
          RETURN
        ENDIF
C
C       Build filename
C
        IRET = TAB2FIL( KTABLE, KPARAM, KCENTR, YFILE )
C
C       Open file
C
        OPEN(UNIT=NUNIT, FILE=YFILE, ERR=910, IOSTAT=ISTATUS,
     X       STATUS='OLD',FORM='FORMATTED')
C
C     Otherwise, use the existing open parameter table file.
C
      ELSE
        REWIND NUNIT
      ENDIF
C
C ------------------------------------------------------------------
C*    Section 2.   Search for the parameter in the table.
C ------------------------------------------------------------------
C
  200 CONTINUE
C
C     Skip lines until '......................' separator found
C
  210 CONTINUE
      READ(NUNIT,'(A)',END=230) YDUMMY
      IF( YDUMMY(1:22).EQ.'......................') GOTO 225
      GOTO 210
C
C     Loop through file
C
  220 CONTINUE
        READ(NUNIT,'(A)',END=230) YDUMMY
  225 CONTINUE
        READ(NUNIT,'(I3.3)',END=230) IPARAM
        IF ( IPARAM .EQ. KPARAM ) THEN
          READ(NUNIT,'(A)',END=230) HFIRST
          READ(NUNIT,'(A)',END=230) HSECOND
          READ(NUNIT,'(A)',END=230) HTHIRD
          READ(NUNIT,'(A)',END=230) HFOURTH
          LOCAL2 = 0
          GOTO 900
        ELSE
          READ(NUNIT,'(A)',END=230) YDUMMY
          READ(NUNIT,'(A)',END=230) YDUMMY
          READ(NUNIT,'(A)',END=230) YDUMMY
          READ(NUNIT,'(A)',END=230) YDUMMY
        ENDIF
      GOTO 220
C
C     Dropthrough if no match found
C
 230  CONTINUE
      LOCAL2 = JPROUTINE + 1
      GOTO 900
C
C ------------------------------------------------------------------
C*    Section 9.   Closedown.
C ------------------------------------------------------------------
C
C     Error opening table file
C
 910  CONTINUE
      LOCAL2 = JPROUTINE + 2
      RETURN
C
C     Normal return.
C
 900  CONTINUE
C
      RETURN
      END
