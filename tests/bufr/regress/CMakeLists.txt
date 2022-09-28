
# download test files for bufr_decode_all
# note: some bufr_decode_all output tests exceed several MiB
file(STRINGS "download_tests_bufr_src_files.txt" files)
ecbuild_get_test_multidata(
    TARGET download_tests_bufr_src_files NOCHECK
    NAMES ${files} )

foreach( bufr_file ${files} )
    ecbuild_add_test(
        TARGET       "regress_bufr_decode_all_${bufr_file}"
        COMMAND      ${CMAKE_BINARY_DIR}/bin/bufr_decode_all
        ARGS         -i "${bufr_file}"
        TEST_DEPENDS download_tests_bufr_src_files
        ENVIRONMENT  "BUFR_TABLES=${TEST_BUFR_TABLES_PATH}" )
endforeach()

