
# set bufr tables path (for testing):
# (check if bufrtables/ is bundled, otherwise set to best guess)
if( NOT TEST_BUFR_TABLES_PATH AND EXISTS "${PROJECT_SOURCE_DIR}/bufrtables/B0000000000000000000.TXT" )
  set( TEST_BUFR_TABLES_PATH "${PROJECT_SOURCE_DIR}/bufrtables" )
elseif ( NOT TEST_BUFR_TABLES_PATH )
  set( TEST_BUFR_TABLES_PATH "${BUFR_TABLES_PATH}" )
endif()
ecbuild_info( "TEST_BUFR_TABLES_PATH:  ${TEST_BUFR_TABLES_PATH}" )
ecbuild_info( "BUFR_TABLES_PATH:       ${BUFR_TABLES_PATH}" )

if( NOT EXISTS "${TEST_BUFR_TABLES_PATH}" )
  ecbuild_warn("Could not find bufr tables path for testing! Please set a valid TEST_BUFR_TABLES_PATH (or BUFR_TABLES_PATH) and re-run cmake" )
endif()


############################################################

# This must be here or set_tests_properties() may fail
# FIXME: add PROPERTIES to ecbuild_add_test()
ecbuild_add_test(
    TARGET       bufr_decode_all_NotFoundTestFile
    COMMAND      ${CMAKE_BINARY_DIR}/bin/bufr_decode_all
    ARGS         -i "NotFoundTestFile.bufr"
    ENVIRONMENT  "BUFR_TABLES=${TEST_BUFR_TABLES_PATH}" )
set_tests_properties( bufr_decode_all_NotFoundTestFile PROPERTIES WILL_FAIL TRUE )

ecbuild_add_test(
    TARGET       bufr_decode_all_NotFoundTablesPath
    COMMAND      ${CMAKE_BINARY_DIR}/bin/bufr_decode_all
    ARGS         -i ${PROJECT_SOURCE_DIR}/tests/bufr/data/temp_101.bufr
    ENVIRONMENT  "BUFR_TABLES=NotFoundTablesPath/" )
set_tests_properties( bufr_decode_all_NotFoundTablesPath PROPERTIES WILL_FAIL TRUE )


############################################################

set( data ${PROJECT_SOURCE_DIR}/tests/bufr/data )
ecbuild_add_test(
    TARGET       emoslib_bufr_filter_NotFoundTablesPath
    COMMAND      ${CMAKE_BINARY_DIR}/bin/emoslib_bufr_filter
    ARGS         -r "${data}/request" -i "${data}/CRYO00012015020412.DC1.DAT" -o /dev/null
    ENVIRONMENT  "BUFR_TABLES=NotFoundTablesPath/" )
set_tests_properties(
    emoslib_bufr_filter_NotFoundTablesPath
    PROPERTIES WILL_FAIL TRUE )

# unit tests: bufr_filter
ecbuild_add_test(
    TARGET       emoslib_bufr_filter_NotFoundTestFile
    COMMAND      ${CMAKE_BINARY_DIR}/bin/emoslib_bufr_filter
    ARGS         -r "${data}/request" -i "NotFoundTestFile" -o /dev/null
    ENVIRONMENT  "BUFR_TABLES=${TEST_BUFR_TABLES_PATH}" )
ecbuild_add_test(
    TARGET       emoslib_bufr_filter_NotFoundRequest
    COMMAND      ${CMAKE_BINARY_DIR}/bin/emoslib_bufr_filter
    ARGS         -r "NotFoundRequest" -i "${data}/CRYO00012015020412.DC1.DAT" -o /dev/null
    ENVIRONMENT  "BUFR_TABLES=${TEST_BUFR_TABLES_PATH}" )
ecbuild_add_test(
    TARGET       emoslib_bufr_filter_CRYO00012015020412.DC1.DAT
    COMMAND      ${CMAKE_BINARY_DIR}/bin/emoslib_bufr_filter
    ARGS         -r "${data}/request" -i "${data}/CRYO00012015020412.DC1.DAT" -o /dev/null
    ENVIRONMENT  "BUFR_TABLES=${TEST_BUFR_TABLES_PATH}" )

set_tests_properties(
    emoslib_bufr_filter_NotFoundTestFile
    emoslib_bufr_filter_NotFoundRequest
    PROPERTIES WILL_FAIL TRUE )

# unit tests: bufr_decode_all
foreach( bufr_file
        "ISMD01_OKPR.bufr"
        "synop_4.bufr"
        "temp_101.bufr"
        "tracks_15U_ITA_2014041012.det.bufr"
        "tracks_15U_ITA_2014041012.det.bufr4" )
    ecbuild_add_test(
        TARGET       "bufr_decode_all_${bufr_file}"
        COMMAND      ${CMAKE_BINARY_DIR}/bin/bufr_decode_all
        ARGS         -i "${data}/${bufr_file}"
        TEST_DEPENDS download_tests_bufr_src_files
        ENVIRONMENT  "BUFR_TABLES=${TEST_BUFR_TABLES_PATH}" )
endforeach()

unset( data )

# ----

set( data ${PROJECT_SOURCE_DIR}/tests/bufr/data )
set( cmd  ${CMAKE_BINARY_DIR}/bin/bufr_filter )

execute_process( COMMAND ${CMAKE_COMMAND} -E copy_if_different
    ${PROJECT_SOURCE_DIR}/tools/bufrtools_wmo/bufr_filter
    ${CMAKE_BINARY_DIR}/bin/ )

ecbuild_add_test(
    TARGET            script_bufr_filter_NotFoundTablesPath
    TYPE              SCRIPT
    COMMAND           ${cmd}
    ARGS              -r ${data}/request -i ${data}/CRYO00012015020412.DC1.DAT -o /dev/null
    ENVIRONMENT       BUFR_TABLES=NotFoundTablesPath/ )
set_tests_properties(
    script_bufr_filter_NotFoundTablesPath
    PROPERTIES WILL_FAIL TRUE )

ecbuild_add_test(
    TARGET            script_bufr_filter_NotFoundTestFile
    TYPE              SCRIPT
    COMMAND           ${cmd}
    ARGS              -r ${data}/request -i NotFoundTestFile -o /dev/null
    ENVIRONMENT       BUFR_TABLES=${TEST_BUFR_TABLES_PATH} )
ecbuild_add_test(
    TARGET            script_bufr_filter_NotFoundRequest
    TYPE              SCRIPT
    COMMAND           ${cmd}
    ARGS              -r NotFoundRequest -i ${data}/CRYO00012015020412.DC1.DAT -o /dev/null
    ENVIRONMENT       BUFR_TABLES=${TEST_BUFR_TABLES_PATH} )
ecbuild_add_test(
    TARGET            script_bufr_filter_CRYO00012015020412.DC1.DAT
    TYPE              SCRIPT
    COMMAND           ${cmd}
    ARGS              -r ${data}/request -i ${data}/CRYO00012015020412.DC1.DAT -o /dev/null
    ENVIRONMENT       BUFR_TABLES=${TEST_BUFR_TABLES_PATH} )
set_tests_properties(
    script_bufr_filter_NotFoundTestFile
    script_bufr_filter_NotFoundRequest
    PROPERTIES WILL_FAIL TRUE )

unset( cmd )
unset( data )


############################################################

# tc_tracks series of tests:
# FIXME: problems with the compare tests, so not running yet (as before)
if( FALSE )

    set( in  "${PROJECT_SOURCE_DIR}/tests/bufr/data/tracks_05W_PEIPAH_2014041000.fm" )
    set( ref "${PROJECT_SOURCE_DIR}/tests/bufr/data/tracks_05W_PEIPAH_2014041000.eps.bufr" )
    set( out                                       "tracks_05W_PEIPAH_2014041000.eps.bufr" )
    ecbuild_add_test( TARGET tc_tracks_eps_${out}  DEPENDS tc_tracks_eps COMMAND ${CMAKE_BINARY_DIR}/bin/tc_tracks_eps ARGS    -i "${in}" -o "${out}"  ENVIRONMENT "BUFR_TABLES=${TEST_BUFR_TABLES_PATH}" )
    ecbuild_add_test( TARGET tc_tracks_eps_${out}4 DEPENDS tc_tracks_eps COMMAND ${CMAKE_BINARY_DIR}/bin/tc_tracks_eps ARGS -4 -i "${in}" -o "${out}4" ENVIRONMENT "BUFR_TABLES=${TEST_BUFR_TABLES_PATH}" )
    ecbuild_add_test( TARGET tc_tracks_eps_${out}_cmp  TEST_DEPENDS tc_tracks_eps_${out}  CONDITION CMD_CMP COMMAND ${CMD_CMP} ARGS "${ref}" "${out}"  )
    ecbuild_add_test( TARGET tc_tracks_eps_${out}4_cmp TEST_DEPENDS tc_tracks_eps_${out}4 CONDITION CMD_CMP COMMAND ${CMD_CMP} ARGS "${ref}" "${out}4" )

    set( in  "${PROJECT_SOURCE_DIR}/tests/bufr/data/tracks_15U_ITA_2014041012.fm" )
    set( ref "${PROJECT_SOURCE_DIR}/tests/bufr/data/tracks_15U_ITA_2014041012.det.bufr" )
    set( out                                       "tracks_15U_ITA_2014041012.det.bufr" )
    ecbuild_add_test( TARGET tc_tracks_det_${out}  DEPENDS tc_tracks_det COMMAND ${CMAKE_BINARY_DIR}/bin/tc_tracks_det ARGS    -i "${in}" -o "${out}"  ENVIRONMENT "BUFR_TABLES=${TEST_BUFR_TABLES_PATH}" )
    ecbuild_add_test( TARGET tc_tracks_det_${out}4 DEPENDS tc_tracks_det COMMAND ${CMAKE_BINARY_DIR}/bin/tc_tracks_det ARGS -4 -i "${in}" -o "${out}4" ENVIRONMENT "BUFR_TABLES=${TEST_BUFR_TABLES_PATH}" )
    ecbuild_add_test( TARGET tc_tracks_det_${out}_cmp  TEST_DEPENDS tc_tracks_det_${out}  CONDITION CMD_CMP COMMAND ${CMD_CMP} ARGS "${ref}" "${out}"  )
    ecbuild_add_test( TARGET tc_tracks_det_${out}4_cmp TEST_DEPENDS tc_tracks_det_${out}4 CONDITION CMD_CMP COMMAND ${CMD_CMP} ARGS "${ref}" "${out}4" )

endif()


############################################################

# regression tests
if( HAVE_LIBEMOS_TESTS_REGRESS )
    add_subdirectory( "regress" )
endif()
