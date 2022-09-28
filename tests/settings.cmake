# Helper functions to abstract interpolation tools/environments


# Set testing tools/environments:
# - int (internal)
# - gg_gridname (internal)
# - grib_compare (bundle, on provided path, or on system path)
# - cmp (strict binary comparison)

if( ENABLE_INSTALL_TOOLS )
  get_target_property( CMD_INT         int         LOCATION )
  get_target_property( CMD_GG_GRIDNAME gg_gridname LOCATION )
endif()

if( TARGET grib_compare )
  get_target_property( CMD_GRIB_COMPARE grib_compare LOCATION )
endif()
if( NOT CMD_GRIB_COMPARE )
  find_program( CMD_GRIB_COMPARE grib_compare NO_DEFAULT_PATH HINTS "${eccodes_BASE_DIR}" PATH_SUFFIXES "bin" )
endif()
if( NOT CMD_GRIB_COMPARE )
  find_program( CMD_GRIB_COMPARE grib_compare )
endif()

find_program( CMD_CMP cmp )


if( NOT eccodes_BASE_DIR AND CMD_GRIB_COMPARE )
  get_filename_component( _grib_compare_dir "${CMD_GRIB_COMPARE}" PATH )
  get_filename_component( eccodes_BASE_DIR "${_grib_compare_dir}" ABSOLUTE )
  set( eccodes_BASE_DIR "${eccodes_BASE_DIR}/../" )
endif()

unset( _grib_environment )
if( grib_api_BASE_DIR )
  set( _grib_environment
    GRIB_DEFINITION_PATH=${grib_api_BASE_DIR}/share/${grib_handling_pkg}/definitions
    GRIB_SAMPLES_PATH=${grib_api_BASE_DIR}/share/${grib_handling_pkg}/samples )
endif()
set( _emos_environment
  "${_grib_environment}"
# JDCNDBG=2
# INTF2_DEBUG=1
  CONFIG_INTERP=ON_FLY
  MARS_LSM_PATH=${PROJECT_SOURCE_DIR}/tables/interpol
  ECMWF_LOCAL_TABLE_PATH=${PROJECT_SOURCE_DIR}/gribtables
  LOCAL_DEFINITION_TEMPLATES=${PROJECT_SOURCE_DIR}/gribtemplates )


# log information
ecbuild_info( "Test tool int:          ${CMD_INT}" )
ecbuild_info( "Test tool gg_gridname:  ${CMD_GG_GRIDNAME}" )
ecbuild_info( "Test tool grib_compare: ${CMD_GRIB_COMPARE}" )
ecbuild_info( "Test tool cmp:          ${CMD_CMP}" )
if( NOT CMD_INT OR NOT CMD_GG_GRIDNAME OR NOT CMD_GRIB_COMPARE OR NOT CMD_CMP )
  ecbuild_warn( "(some tools not found, some tests disabled)" )
endif()
ecbuild_debug( "Test _grib_environment: ${_grib_environment}" )
ecbuild_debug( "Test _emos_environment: ${_emos_environment}" )


# Easily add interpolation tests
# (optional arg #5: test dependencies, default none)
# (optional arg #6: environment variables, default none additional)
function( interpolation_add_test_interpol
        _label
        _file1
        _file2
        _options )
    set( _depends     "" )
    set( _environment "" )
    if( ${ARGC} GREATER 4 )
        set( _depends "${ARGV4}" )
    endif()
    if( ${ARGC} GREATER 5 )
        set( _environment "${ARGV5}" )
    endif()
    ecbuild_add_test(
        TARGET       ${_label}_interpol
        TEST_DEPENDS ${_depends}
        DEPENDS      int
        COMMAND      ${CMD_INT}
        ARGS         --input=${_file1} --output=${_file2} ${_options}
        ENVIRONMENT  ${_environment} ${_emos_environment} )
endfunction()


# Easily add comparison-to-reference results tests
# (optional arg #4: test dependencies, default none)
function( interpolation_add_test_compare
        _label
        _file1
        _file2 )
    set( _depends "" )
    if( ${ARGC} GREATER 3 )
        set( _depends "${ARGV3}" )
    endif()
    set( _tolerate "" )
    if(    (label MATCHES "F640_to_(F48|F80_sub-area|regular_ll)$")
        OR (label MATCHES "(N640|O1280)_to_(F48|F80|regular_ll)(|_sub-area)$")
        OR (label MATCHES "(N640|O1280)_to_(N80|O80)$")
        OR (label MATCHES "O1280_to_regular_ll_1-16$")
        OR (label MATCHES "regular_ll_to_regular_ll$") )
        set( _tolerate -P -T 2 )
    endif()
    ecbuild_add_test(
        TARGET       ${_label}_compare
        TEST_DEPENDS ${_label}_interpol ${_depends}
        COMMAND      ${CMD_GRIB_COMPARE}
        ARGS         ${_tolerate} ${_file1} ${_file2}
        ENVIRONMENT  ${_grib_environment} )
endfunction()


# Easily add expected-gridname tests
function( interpolation_add_test_gridname
        _label
        _file
        _gridname )
    ecbuild_add_test(
        TARGET       ${_label}_gridname
        TEST_DEPENDS ${_label}_interpol
        CONDITION    ENABLE_INSTALL_TOOLS
        COMMAND      ${CMD_GG_GRIDNAME}
        ARGS         --eq=${_gridname} ${_file}
        ENVIRONMENT  ${_grib_environment} )
endfunction()

