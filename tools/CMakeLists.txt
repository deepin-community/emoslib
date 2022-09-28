# configure Fortran-based targets for double precision (compilation & linking)
if (NOT emos_double_precision_flags STREQUAL "")
  ecbuild_add_fortran_flags(${emos_double_precision_flags})
endif()

if( HAVE_LIBEMOS_BUFRDC )
  add_subdirectory( bufrtools_wmo )
endif()

if( HAVE_LIBEMOS_INTERPOLATION )

  add_subdirectory( lsmReducedGaussian )

  if( NOT HAVE_GRIBEX_ABORT )
    add_subdirectory( singlePointInterpolation )
  endif()

  ecbuild_add_executable(
    TARGET      gg_latitudes NOINSTALL
    SOURCES     gg_latitudes.F
    DEFINITIONS ${LIBEMOS_DEFINITIONS}
    LIBS        emos )

  if( CMAKE_Fortran_COMPILER_ID MATCHES "PGI" )

    set_source_files_properties( gg_gridname.c PROPERTIES COMPILE_DEFINITIONS "FORTRAN_LINKER_PGI" )
    ecbuild_add_executable(
      TARGET      gg_gridname NOINSTALL
      SOURCES     gg_gridname.c
      DEFINITIONS ${LIBEMOS_DEFINITIONS}
      INCLUDES    ${GRIB_API_INCLUDE_DIRS}
      LIBS        emos
      LINKER_LANGUAGE Fortran )

    set_source_files_properties( gg_single.c PROPERTIES COMPILE_DEFINITIONS "FORTRAN_LINKER_PGI" )
    ecbuild_add_executable(
      TARGET      gg_single NOINSTALL
      SOURCES     gg_single.c
      DEFINITIONS ${LIBEMOS_DEFINITIONS}
      INCLUDES    ${GRIB_API_INCLUDE_DIRS}
      LIBS        emos
      LINKER_LANGUAGE Fortran )

  else()
    ecbuild_add_executable(
      TARGET      gg_gridname NOINSTALL
      SOURCES     gg_gridname.c
      DEFINITIONS ${LIBEMOS_DEFINITIONS}
      INCLUDES    ${GRIB_API_INCLUDE_DIRS}
      LIBS        emos )
    ecbuild_add_executable(
      TARGET      gg_single NOINSTALL
      SOURCES     gg_single.c
      DEFINITIONS ${LIBEMOS_DEFINITIONS}
      INCLUDES    ${GRIB_API_INCLUDE_DIRS}
      LIBS        emos )
  endif()

  ecbuild_add_executable(
    TARGET      int NOINSTALL
    SOURCES     int.F except.c
    DEFINITIONS ${LIBEMOS_DEFINITIONS}
    INCLUDES    ${GRIB_API_INCLUDE_DIRS}
    LIBS        emos )

endif()
