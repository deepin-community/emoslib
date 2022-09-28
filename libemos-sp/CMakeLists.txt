# library sources (copy)
foreach( _src ${libemos_srcs} )
  add_custom_command(
    OUTPUT  ${CMAKE_CURRENT_BINARY_DIR}/${_src}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${LIBEMOS_BASE_DIR}/${_src} ${CMAKE_CURRENT_BINARY_DIR}/${_src}
    COMMENT "copying ${_src} from emos (sp)"
    DEPENDS ${LIBEMOS_BASE_DIR}/${_src} )
  list( APPEND emos_sp_srcs ${CMAKE_CURRENT_BINARY_DIR}/${_src} )
endforeach()


# compiler definitions:
# - exclude double-precision specific definitions
# - allow/disallow GRIBEX with configuration
list( REMOVE_ITEM LIBEMOS_DEFINITIONS REAL_8 REAL_BIGGER_THAN_INTEGER )
if( HAVE_GRIBEX_ABORT )
  set_source_files_properties(
    "${CMAKE_CURRENT_BINARY_DIR}/gribex/gribex.F"
    PROPERTIES COMPILE_DEFINITIONS "GRIBEX_ABORT" )
endif()
if( HAVE_LIBEMOS_INTERPOLATION AND HAVE_FFTW )
  set_source_files_properties(
    "${CMAKE_CURRENT_BINARY_DIR}/interpolation/jagggp.F"
    "${CMAKE_CURRENT_BINARY_DIR}/interpolation/jsymgg.F"
    PROPERTIES COMPILE_DEFINITIONS "HAVE_FFTW" )
endif()


# library target
set( CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR} )
include_directories( ${LIBEMOS_BASE_DIR}/interpolation ${LIBEMOS_BASE_DIR}/bufrdc_wmo )

ecbuild_add_library(
  TYPE        ${LIB_TYPE}
  TARGET      emos_sp
  OUTPUT_NAME "emos"
  DEFINITIONS ${LIBEMOS_DEFINITIONS} ${emos_interpol_tables_def}
  LIBS        "${GRIB_API_LIBRARIES}" "${FFTW_LIB}" "${FORTRAN_LIBRARIES}"
  SOURCES     ../libemos_version.c ${emos_sp_srcs}
  GENERATED   ../libemos_version.c )


#if( BUILD_SHARED_LIBS )
#  install(CODE "execute_process(COMMAND \"${CMAKE_COMMAND}\" -E create_symlink
#               libemos.so ${CMAKE_INSTALL_PREFIX}/${INSTALL_LIB_DIR}/libemos.R32.D64.I32.so)" )
#else()
  install(CODE "execute_process(COMMAND \"${CMAKE_COMMAND}\" -E create_symlink
               libemos.a ${CMAKE_INSTALL_PREFIX}/${INSTALL_LIB_DIR}/libemos.R32.D64.I32.a)" )
#endif()

