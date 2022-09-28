set( CMAKE_C_FLAGS
     "-Dlinux"
     CACHE STRING "C flags" )

set( CMAKE_Fortran_FLAGS
     "-Dlinux -fcray-pointer -fdefault-real-8 -fdefault-double-8"
     CACHE STRING "Fortran flags" )
