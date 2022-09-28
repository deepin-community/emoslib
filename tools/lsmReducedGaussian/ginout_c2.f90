
program ginout_c2
    use grib_api

    implicit none
    integer IARGC


    integer, parameter :: JPRLEN=8  ! for -DREAL_8, otherwise should be JPRLEN=4
    integer       F1, F2, r, IG
    character*256 N1, N2

    integer (kind=4) :: valuesN
    real    (kind=8) :: missingValue  ! TODO: nothing is done about the missing values
    real    (kind=8), dimension(:), allocatable :: values


    ! Get input/output files
    if (IARGC().lt.2) stop 'Usage: ginout_c2 inputfile outputfile'
    call getarg(1,N1)
    call getarg(2,N2)


    ! Open input file, get the 'values' array and close the file
    call grib_open_file(F1,N1,'r')
    call grib_new_from_file(F1,IG)

    call grib_get_size(IG,'values',valuesN)
    allocate(values(valuesN))
    call grib_get(IG,'values',values)

    call grib_get(IG,'missingValue',missingValue)
!   print *, 'missingValue=', missingValue

    call grib_release(IG)
    call grib_close_file(F1)


    ! Open output file
    open (unit=16, file=N2, status='UNKNOWN', form='UNFORMATTED', iostat=R)
    if (R.ne.0) stop 'Error: OPEN'


    ! Write the unpacked values
    R=1;  call pbopen  (F2,N2(1:index(N2,' ')-1), "W", R);  if (R.ne.0) stop 'Error: PBOPEN'
    R=0;  call pbwrite (F2,values,valuesN*JPRLEN,R);        if (R.lt.0) stop 'Error: PBWRITE'
    R=1;  call pbclose (F2,R);                              if (R.ne.0) stop 'Error: PBCLOSE'


    ! Cleanup
    deallocate(values)

end program

