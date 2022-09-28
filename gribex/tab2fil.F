C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C

      INTEGER FUNCTION TAB2FIL ( KTABLE, KPARAM, KCENTR, HFILE )
C
C---->
C**** TAB2FIL
C
C     Purpose
C     -------
C
C     Build name of file that should contain, if present, the GRIB code
C     table 2 (parameters) associated to data (with valid entries) .
C
C
C     Interface
C     ---------
C
C     IRET = TAB2FIL ( KTABLE, KPARAM, KCENTR )
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
C     HFILE - File name to look for and contact.
C
C     Function return value is:
C            0 - In all current cases .
C
C
C     Method
C     ------
C
C     Checks if environment variable ECMWF_LOCAL_TABLE_PATH exists to
C     define the location of the parameter tables.
C
C     Otherwise, looks for table xxx in:
C
C     ${ELTPATH}/wmostd/table_2_version_xxx for WMO standard entries ;
C
C     ${ELTPATH}/nnnnnn/local_table_2_version_xxx for ECMWF home data
C     where nnnnnn is the current EMOSLIB version ;
C
C     ${ELTPATH}/cenCCC/local_table_2_version_xxx for home data of
C     centre number CCC .
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
C     EMOSNUM - Gives current EMOSLIB version number.
C
C
C     Author
C     ------
C
C     J. Clochard, Meteo France, for ECMWF - June 1999.
C
C
C     MODIFICATIONS
C     _____________
C
C     None.
C
C     _______________________________________________________
C----<
C
C
      IMPLICIT NONE
C
#include "common/grprs.h"
#include "grbcom.h"
C
C     Function arguments
C
      INTEGER KPARAM, KTABLE, KCENTR
C
      CHARACTER*256 HFILE
C
C     Local variables
C
      INTEGER IEMOSNM, IOFFSET
C
C     External functions
C
      INTEGER EMOSNUM
      EXTERNAL EMOSNUM
C
C
C ------------------------------------------------------------------
C*    Section 1.   Initialise
C ------------------------------------------------------------------
C
  100 CONTINUE
C
      TAB2FIL = 0
C
C     Build filename
C
      HFILE = ELTPATH
      IOFFSET = INDEX( HFILE, ' ')
C
      IF( MAX( KTABLE,KPARAM) .LE. 127 ) THEN
C
        HFILE(IOFFSET:)='wmostd/table_2_version_'
        IOFFSET = INDEX( HFILE, ' ')
C
      ELSE
C
        IF( KCENTR .EQ. 98 ) THEN
C
          IEMOSNM = EMOSNUM(1)
          WRITE(HFILE(IOFFSET:IOFFSET+5),'(I6.6)') IEMOSNM
C
        ELSE
C
          HFILE(IOFFSET:IOFFSET+3)='cen'
          WRITE(HFILE(IOFFSET+3:IOFFSET+5),'(I3.3)') KCENTR
C
        ENDIF
C
        IOFFSET=IOFFSET+6
        HFILE(IOFFSET:)='/local_table_2_version_'
C
      ENDIF
C
      IOFFSET = INDEX( HFILE, ' ')
      WRITE(HFILE(IOFFSET:IOFFSET+2),'(I3.3)') KTABLE
C     print *,'TAB2FIL: HFILE = "' // HFILE(:IOFFSET+2) // '"'
C
C
C ------------------------------------------------------------------
C*    Section 9.   Closedown.
C ------------------------------------------------------------------
C
C     Drop through if not found.
C
      RETURN   
C
      END
