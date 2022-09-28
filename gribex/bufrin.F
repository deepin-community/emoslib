C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities
C granted to it by virtue of its status as an intergovernmental organisation
C nor does it submit to any jurisdiction.
C

        subroutine bufrin(kunit,karray,kinlen,koutlen,keof,kret)
C---->
C
C       This subroutine mimics the behaviour of the original BUFRIN
C       but makes a call to PBBUFR to retrieve BUFR products.
C
C----<
        integer icalls
        data icalls/0/
C
C First time through, print a warning message
        if (icalls .eq. 0) then
          icalls = icalls + 1
          write(*,*) ' *********************************************'
          write(*,*) ' *********************************************'
          write(*,*) ' You are using an obsolete subroutine (BUFRIN)'
          write(*,*) ' for retrieving BUFR products.'
          write(*,*) ' Please change to using PBBUFR.'
          write(*,*) ' *********************************************'
          write(*,*) ' *********************************************'
        endif
C
        call pbbufr(kunit,karray,kinlen,koutlen,kret)
        if ( kret .eq. -3) then
          write(*,*) ' Buffer not big enough for BUFR product'
          kret = -5
        endif
C
        return
        end
