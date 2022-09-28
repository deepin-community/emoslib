# library sources
foreach( _src ${libemos_srcs} )
  list( APPEND emos_dp_srcs ../${_src} )
endforeach()


# compiler definitions:
# - configure Fortran-based targets for double precision (compilation & linking)
# - allow/disallow GRIBEX with configuration
if (NOT emos_double_precision_flags STREQUAL "")
  ecbuild_add_fortran_flags(${emos_double_precision_flags})
endif()
if( HAVE_GRIBEX_ABORT )
  set_source_files_properties(
    "../gribex/gribex.F"
    PROPERTIES COMPILE_DEFINITIONS "GRIBEX_ABORT" )
endif()
if( HAVE_LIBEMOS_INTERPOLATION AND HAVE_FFTW )
  set_source_files_properties(
    "../interpolation/jagggp.F"
    "../interpolation/jsymgg.F"
    PROPERTIES COMPILE_DEFINITIONS "HAVE_FFTW" )
endif()


# library target
set( CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )
include_directories( ${LIBEMOS_BASE_DIR}/interpolation ${LIBEMOS_BASE_DIR}/bufrdc_wmo )

ecbuild_add_library(
  TYPE        ${LIB_TYPE}
  TARGET      emos
  OUTPUT_NAME "emosR64"
  DEFINITIONS ${LIBEMOS_DEFINITIONS} ${emos_interpol_tables_def}
  LIBS        "${GRIB_API_LIBRARIES}" "${FFTW_LIB}" ${FORTRAN_LIBRARIES}
  SOURCES     ../libemos_version.c ${emos_dp_srcs}
  GENERATED   ../libemos_version.c )


#if( BUILD_SHARED_LIBS )
#  install(CODE "execute_process(COMMAND \"${CMAKE_COMMAND}\" -E create_symlink
#               libemosR64.so ${CMAKE_INSTALL_PREFIX}/${INSTALL_LIB_DIR}/libemos.R64.D64.I32.so)" )
#else()
  install(CODE "execute_process(COMMAND \"${CMAKE_COMMAND}\" -E create_symlink
               libemosR64.a ${CMAKE_INSTALL_PREFIX}/${INSTALL_LIB_DIR}/libemos.R64.D64.I32.a)" )
#endif()

