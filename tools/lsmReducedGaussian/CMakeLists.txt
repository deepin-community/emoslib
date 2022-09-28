
# configure Fortran-based targets for double precision (compilation & linking)
if (NOT emos_double_precision_flags STREQUAL "")
  ecbuild_add_fortran_flags(${emos_double_precision_flags})
endif()

ecbuild_add_executable(
  TARGET      ginout_c2 NOINSTALL
  SOURCES     ginout_c2.f90
  CONDITION   HAVE_FORTRAN
  DEFINITIONS ${LIBEMOS_DEFINITIONS}
  INCLUDES    ${GRIB_API_INCLUDE_DIRS}
  LIBS        emos )

