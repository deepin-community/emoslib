
# configure Fortran-based targets for double precision (compilation & linking)
if (NOT emos_double_precision_flags STREQUAL "")
  ecbuild_add_fortran_flags(${emos_double_precision_flags})
endif()


# executable
ecbuild_add_executable(
  TARGET  singlePointInterpolation
  SOURCES singlePointInterpolation.F
  LIBS    emos )

