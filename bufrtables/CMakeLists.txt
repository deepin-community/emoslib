############################################################
# CMake options:
#
#   -DCMAKE_INSTALL_PREFIX="/usr/local/apps/libemos/tables/bufr/text/XXXYYY"  (optional: default is /usr/local )

cmake_minimum_required( VERSION 2.8.11 FATAL_ERROR )

project( bufrdc_tables C )

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild/cmake")

set( ENABLE_OS_TESTS OFF )
set( ENABLE_OS_FUNCTIONS_TEST OFF )

include( ecbuild_system )

ecbuild_requires_macro_version( 2.0 )

###############################################################################
# project

ecbuild_declare_project()

math( EXPR TABLES_VERSION "100*${BUFRDC_TABLES_MAJOR_VERSION} + 10*${BUFRDC_TABLES_MINOR_VERSION} + ${BUFRDC_TABLES_PATCH_VERSION}")
set( TABLES_VERSION "000${TABLES_VERSION}" )

file(GLOB links_txt RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.TXT )
file(GLOB distincts RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *.distinct )

# debug (warning: lots of output!)
# ecbuild_debug_var( links_txt )
# ecbuild_debug_var( distincts )

install( FILES ${distincts} DESTINATION ${INSTALL_DATA_DIR} )
install( FILES ${links_txt} DESTINATION ${INSTALL_DATA_DIR} )

ecbuild_info( "${PROJECT_NAME} version: ${TABLES_VERSION}" )
ecbuild_debug( "${PROJECT_NAME} installing to: ${CMAKE_INSTALL_PREFIX}/${INSTALL_DATA_DIR}" )

if( NOT PROJECT_NAME STREQUAL CMAKE_PROJECT_NAME )
  set( TEST_BUFR_TABLES_PATH "${PROJECT_SOURCE_DIR}" PARENT_SCOPE )
endif()

################################################################################
# finalize

ecbuild_install_project( NAME ${PROJECT_NAME} )

ecbuild_print_summary()
