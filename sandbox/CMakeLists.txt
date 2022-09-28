if( CMAKE_Fortran_COMPILER_ID MATCHES "PGI" )

  set_source_files_properties( emos_tool.c PROPERTIES COMPILE_DEFINITIONS "FORTRAN_LINKER_PGI" )
  ecbuild_add_executable(
    TARGET    emos_tool
    SOURCES   emos_tool.c
    CONDITION HAVE_LIBEMOS_INTERPOLATION
    INCLUDES  ${GRIB_API_INCLUDE_DIRS}
    LIBS      emos
    LINKER_LANGUAGE Fortran )

else()

  ecbuild_add_executable(
    TARGET    emos_tool
    SOURCES   emos_tool.c
    CONDITION HAVE_LIBEMOS_INTERPOLATION
    INCLUDES  ${GRIB_API_INCLUDE_DIRS}
    LIBS      emos )

endif()
