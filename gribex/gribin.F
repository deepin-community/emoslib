C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

        subroutine gribin(kunit,karray,kinlen,koutlen,keof,kret)
C---->
C
C       This subroutine mimics the behaviour of the original GRIBIN
C       but makes a call to PBGRIB to retrieve GRIB products.  Because
C       MAGICS calls GRIBIN with an invalid buffer size, this mimic
C       version has to call PBGRIB with a phoney buffer size - dumlen,
C       currently 1000000.  In effect this subverts the check that the
C       buffer is big enough for the retrieved product!!!
C
C----<
        integer icalls
        integer dumlen
        save icalls, dumlen
        data icalls/0/
        data dumlen/1000000/
C
C First time through, print a warning message
        if (icalls .eq. 0) then
          icalls = icalls + 1
          write(*,*) ' *********************************************'
          write(*,*) ' *********************************************'
          write(*,*) ' You are using an obsolete subroutine (GRIBIN)'
          write(*,*) ' for retrieving GRIB products.'
          write(*,*) ' There is NO check your buffer is big enough.'
          write(*,*) ' Please change to using PBGRIB.'
          write(*,*) ' *********************************************'
          write(*,*) ' *********************************************'
        endif
C
C Note the use of dumlen (not kinlen) in the following call ...
        call pbgrib(kunit,karray,DUMLEN,koutlen,kret)
        if ( kret .eq. -3) then
          write(*,*) ' Buffer not big enough for GRIB product'
          kret = -5
        endif
C
        return
        end
