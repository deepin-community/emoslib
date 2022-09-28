
# test settings and helper functions
include("${PROJECT_SOURCE_DIR}/tests/settings.cmake")

# configure Fortran-based targets for double precision (compilation & linking)
if (NOT emos_double_precision_flags STREQUAL "")
  ecbuild_add_fortran_flags(${emos_double_precision_flags})
endif()


################ I N T E R P O L A T I O N ################

if( HAVE_LIBEMOS_INTERPOLATION AND ENABLE_INSTALL_TOOLS )

  # intuvp2: interpolation to different formats
  foreach( rotated "" "rotated_" )
    foreach( out "regular_ll" "F80" "N80" "O80" )
      set( in "vo-d_sh" )
      set( label "intuvp2_${in}_to_${rotated}${out}" )
      if(     NOT (label MATCHES "sh_to_O" AND NOT HAVE_FFTW)
          AND NOT (label MATCHES "_to_rotated_[NOF]") )
        set( input "${PROJECT_SOURCE_DIR}/tests/data/${in}.grib" )
        set( refer "${PROJECT_SOURCE_DIR}/tests/data/${label}.grib.ref" )
        set( output "${label}.grib" )
        unset( opt )
        if( out MATCHES "^regular_ll" )
          list( APPEND opt "--INTOUT:grid=3.0/3.0" )
        else()
          list( APPEND opt "--INTOUT:gridname=${out}" )
        endif()
        if( "${rotated}" STREQUAL "rotated_" )
          list( APPEND opt "--INTOUT:rotation=30/30" )
        endif()
        list( APPEND opt "--intuvp2" )

        interpolation_add_test_interpol( ${label} "${input}" "${output}" "${opt}" )
        interpolation_add_test_compare(  ${label} "${refer}" "${output}" )

        unset( opt )
        unset( output )
        unset( refer )
        unset( input )
      endif()
      unset( label )
      unset( in )
    endforeach()
  endforeach()

  # intf2: interpolation to different formats
  # not possible:
  # - regular_ll to reduced Gaussian (G) (but regular_ll to regular G is possible)
  # - regular G to reduced G
  # - any to rotated reduced/regular G
  # - regular_ll to rotated regular_ll
  foreach( rotated "" "rotated_" )
    foreach( in "vo-d_sh" "z_sh" "msl_regular_ll" "msl_N640" "2t_O640" "q_F640" )
      foreach( out "regular_ll" "F80" "N80" "O80" "F48" )
        set( label "intf2_${in}_to_${rotated}${out}" )
        if(     NOT (label MATCHES "regular_ll_to_[NO]")
            AND NOT (label MATCHES "F.*_to_[NO]")
            AND NOT (label MATCHES "_to_rotated_[NOF]")
            AND NOT (label MATCHES "regular_ll_to_rotated_regular_ll$")
            AND NOT (label MATCHES "sh_to_O" AND NOT HAVE_FFTW) )
          set( input "${PROJECT_SOURCE_DIR}/tests/data/${in}.grib" )
          set( refer "${PROJECT_SOURCE_DIR}/tests/data/${label}.grib.ref" )
          set( output "${label}.grib" )
          unset( opt )
          if( out MATCHES "^regular_ll" )
            list( APPEND opt "--INTOUT:grid=0.5/0.5" )
          else()
            list( APPEND opt "--INTOUT:gridname=${out}" )
          endif()
          if( "${rotated}" STREQUAL "rotated_" )
            list( APPEND opt "--INTOUT:rotation=30/30" )
          endif()
          list( APPEND opt "--INTOUT:autoresol=1" "--INTOUT:area=0/0/0/0" "--intf2" )

          interpolation_add_test_interpol( ${label} "${input}" "${output}" "${opt}" )
          interpolation_add_test_compare( ${label} "${output}" "${refer}" )

          unset( opt )
          unset( output )
          unset( refer )
          unset( input )
        endif()
        unset( label )
      endforeach()
    endforeach()
  endforeach()


  # intf2: interpolate to N/O grids with the same N
  # (distinguish between N640 <> O640)
  foreach( in "N640" "O640" )
    foreach( out "N640" "O640" )
      set( label "intf2_${in}_to_${out}" )
      set( output "${label}.grib" )
      unset( opt )
      list( APPEND opt "--INTOUT:gridname=${out}" "--INTOUT:autoresol=1" "--INTOUT:area=0/0/0/0" "--intf2" )

      if( in MATCHES "N640" )
        set( input "${PROJECT_SOURCE_DIR}/tests/data/msl_${in}.grib" )
      else()
        set( input "${PROJECT_SOURCE_DIR}/tests/data/2t_${in}.grib" )
      endif()
      interpolation_add_test_interpol( ${label} "${input}" "${output}" "${opt}" )
      interpolation_add_test_compare(  ${label} "${input}" "${output}" )
      interpolation_add_test_gridname( ${label} ${output} ${out} )
      if( NOT in STREQUAL out )
        set_tests_properties( ${label}_compare PROPERTIES WILL_FAIL TRUE )
      endif()

      unset( opt )
      unset( input )
      unset( output )
      unset( label )
    endforeach()
  endforeach()


  # intf2: compare FFT99 vs FFTW exactly (no tolerance!)
  if( HAVE_FFTW )
    foreach( in "vo-d_sh" "z_sh" )
      set( input "${PROJECT_SOURCE_DIR}/tests/data/${in}.grib" )
      set( output_fftw  "intf2_${in}_to_N80-fftw.grib" )
      set( output_fft99 "intf2_${in}_to_N80.grib" )
      unset( opt )
      list( APPEND opt "--INTOUT:gridname=N80" "--INTOUT:autoresol=1" "--intf2" )

      interpolation_add_test_interpol( "intf2_${in}_to_N80_fft99_vs_fftw" "${input}" "${output_fftw}" "${opt}" "" "EMOSLIB_FORCED_FFTW=1" )
      interpolation_add_test_compare( "intf2_${in}_to_N80_fft99_vs_fftw" "${output_fftw}" "${output_fft99}" )

      unset( opt )
      unset( output_fft99 )
      unset( output_fftw )
      unset( input )
    endforeach()
  endif( HAVE_FFTW )


  # intf2: test SH truncation
  set( in "vo-d_sh" )
  set( opt "--INTOUT:truncation=63" "--intf2" )
  interpolation_add_test_interpol(
      "intf2_${in}_to_${in}_trunc"
      "${PROJECT_SOURCE_DIR}/tests/data/${in}.grib"
      "intf2_${in}_to_${in}_trunc.grib"
      "${opt}" )
  unset( opt )
  unset( in )


endif( HAVE_LIBEMOS_INTERPOLATION AND ENABLE_INSTALL_TOOLS )


####################### G R I B E X #######################

if( HAVE_LIBEMOS_GRIBEX )

    ecbuild_add_test( TARGET gribex_abort SOURCES gribex_abort.F LIBS emos ENVIRONMENT ${_emos_environment} )
    ecbuild_add_test( TARGET gribex_demo  SOURCES gribex_demo.F  LIBS emos ENVIRONMENT ${_emos_environment}
      ARGS -i "${PROJECT_SOURCE_DIR}/tests/data/msl_regular_ll.grib" )

    set_tests_properties( gribex_abort PROPERTIES WILL_FAIL ${HAVE_GRIBEX_ABORT} )
    set_tests_properties( gribex_demo  PROPERTIES WILL_FAIL ${HAVE_GRIBEX_ABORT} )

endif( HAVE_LIBEMOS_GRIBEX )


######################### B U F R #########################

if( HAVE_LIBEMOS_BUFRDC AND ENABLE_INSTALL_TOOLS )
  add_subdirectory( bufr )
endif( HAVE_LIBEMOS_BUFRDC AND ENABLE_INSTALL_TOOLS )


############ T E S T S :   R E G R E S S I O N ############

if( HAVE_LIBEMOS_INTERPOLATION AND ENABLE_INSTALL_TOOLS )
  if( HAVE_LIBEMOS_TESTS_REGRESS )
    add_subdirectory( regress )
  endif()
endif( HAVE_LIBEMOS_INTERPOLATION AND ENABLE_INSTALL_TOOLS )


###################### M S   A P P S ######################

add_subdirectory( ms_apps )


################ S P E C I A L   T E S T S ################

if( HAVE_LIBEMOS_INTERPOLATION )

    # linking (build-time) test(s)
    foreach( src "example_intf2.F" "example_intuvp2.F" "link_mars.F" "link_prodgen.F" )
        get_filename_component( test ${src} NAME_WE )
        ecbuild_add_executable( TARGET ${test} NOINSTALL SOURCES ${src} LIBS emos )
        unset( test )
    endforeach()
    ecbuild_add_executable( TARGET "example_intf" NOINSTALL SOURCES "example_intf.F90" LIBS emos CONDITION (HAVE_FORTRAN AND NOT CMAKE_Fortran_COMPILER_ID MATCHES "PGI") )

    # run-time test(s)
    foreach( test "test_fft99" "test_hsp2gg" )
        ecbuild_add_test( TARGET ${test} SOURCES ${test}.F LIBS emos )
    endforeach()
    ecbuild_add_test( TARGET "test_fftw" SOURCES "test_fftw.F" LIBS emos CONDITION HAVE_FFTW )

endif( HAVE_LIBEMOS_INTERPOLATION )


