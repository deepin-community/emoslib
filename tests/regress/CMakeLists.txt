
# Download interpolation source files/reference results

file(STRINGS "download_tests_regress_src_files.txt" files)
ecbuild_get_test_multidata(
    TARGET download_tests_regress_src_files NOCHECK
    NAMES ${files} )

file(STRINGS "download_tests_regress_ref_files.txt" files)
ecbuild_get_test_multidata(
    TARGET download_tests_regress_ref_files NOCHECK
    NAMES ${files} )


######################### EMOS-216 #########################

# some interpolations are not possible:
# - regular_ll to reduced_gg (but regular_ll to regular_gg is possible)
# - regular_gg to reduced_gg
# - any to rotated reduced_gg/regular_gg
# - regular_ll to rotated regular_ll
# - ... HIRLAM restrictions?
# - any to octahedral without FFTW
# - swh_reduced_ll interpolations (SIGFPE, Arithmetic exception in INTIN) (FIXME: temporary disabling)
foreach( area "" "_sub-area" )
  foreach( in "vo-d_sh" )
    foreach( out "regular_ll" "F80" "N80" "O80" )
      set( label "regress_EMOS-216_intuvp2_${in}_to_${out}${area}" )
      set( input "${in}.grib" )
      set( output "${label}.grib" )
      if( NOT (label MATCHES "_sh_to_O" AND NOT HAVE_FFTW) )
        unset( opt )
        if( out MATCHES "^regular_ll" )
          list( APPEND opt "--INTOUT:grid=3/3" )
        else()
          list( APPEND opt "--INTOUT:gridname=${out}" )
        endif()
        if( "${area}" STREQUAL "_sub-area" )
          list( APPEND opt "--INTOUT:area=70/-60/20/60" )
        endif()
        list( APPEND opt "--intuvp2" )

        interpolation_add_test_interpol( ${label} "${input}" "${output}" "${opt}" download_tests_regress_src_files )
        interpolation_add_test_compare( ${label} "${output}" "${output}.ref"      download_tests_regress_ref_files )

        unset( opt )
      endif()
      unset( output )
      unset( input )
      unset( label )
    endforeach()
  endforeach()
endforeach()

foreach( area "" "_sub-area" )
  foreach( rotated "" "rotated_" "hirlam_non-rotated_" )
    foreach( in
        "2t_F640"
        "2t_F640_sub-area"
        "2t_N640"
        "2t_O1280"
        "2t_regular_ll"
        "2t_regular_ll_staggered"
        "2t_regular_ll_sub-area"
        "msl_F640"
        "msl_F640_sub-area"
        "msl_N640"
        "msl_O1280"
        "msl_regular_ll"
        "msl_regular_ll_sub-area"
        "mwd3_reduced_ll"
        "swh_reduced_ll"
        "swh_reduced_ll_sub-area"
        "u-v_sh"
        "z_sh" )
      foreach( out "regular_ll" "F80" "N80" "O80" "F48" )
        set( label "regress_EMOS-216_intf2_${in}_to_${rotated}${out}${area}" )
        set( input "${in}.grib" )
        set( output "${label}.grib" )
        if(     NOT (label MATCHES "_sub-area_to_" AND NOT label MATCHES "_to_.*_sub-area")
            AND NOT (label MATCHES "_regular_ll.*_to_[NO]")
            AND NOT (label MATCHES "_F.+_to_[NO]")
            AND NOT (label MATCHES "(F.+|reduced_ll)_to_.*rotated_regular_ll")
            AND NOT (label MATCHES "_to_[NO].*_sub-area")
            AND NOT (label MATCHES "_to_.*rotated_[NOF]")
            AND NOT (label MATCHES "_to_.*rotated_.*sub-area")
            AND NOT (label MATCHES "_regular_ll.*_to_.*rotated_regular_ll$")
            AND NOT (label MATCHES "_reduced_ll_sub-area_to_.*")  # FIXME
            AND NOT (label MATCHES "_reduced_ll_to_[FNO]")        # FIXME: EMOS-286)
            AND NOT (label MATCHES "_reduced_ll_to_regular_ll")   # FIXME: problems comparing gnu/pgi results (opensuse131)
            AND NOT (label MATCHES "_sh_to_O" AND NOT HAVE_FFTW) )
          unset( opt )
          if( out MATCHES "^regular_ll" )
            list( APPEND opt "--INTOUT:grid=3/3" )
          else()
            list( APPEND opt "--INTOUT:gridname=${out}" )
          endif()
          if( "${rotated}" STREQUAL "hirlam_non-rotated_" )
            list( APPEND opt "--INTOUT:rotation=-90/0" )
          elseif( "${rotated}" STREQUAL "rotated_" )
            list( APPEND opt "--INTOUT:rotation=30/30" )
          endif()
          if( "${area}" STREQUAL "_sub-area" )
            list( APPEND opt "--INTOUT:area=70/-60/20/60" )
          endif()
          list( APPEND opt "--intf2" )

          interpolation_add_test_interpol( ${label} "${input}" "${output}" "${opt}" download_tests_regress_src_files )
          interpolation_add_test_compare( ${label} "${output}" "${output}.ref"      download_tests_regress_ref_files )

          unset( opt )
        endif()
        unset( output )
        unset( input )
        unset( label )
      endforeach()
    endforeach()
  endforeach()
endforeach()


######################### EMOS-260 #########################

set( label "regress_EMOS-260_intf2_2t_N640_to_regular_ll_sub-area" )
set( input "2t_N640.grib" )
set( output "${label}.grib" )
set( opt "--INTOUT:area=47.1/-13.05/33/8.1" "--INTOUT:grid=0.15/0.15" )
list( APPEND opt "--intf2" )

interpolation_add_test_interpol( ${label} "${input}" "${output}" "${opt}" download_tests_regress_src_files )
interpolation_add_test_compare( ${label} "${output}" "${output}.ref"      download_tests_regress_ref_files )

unset( opt )
unset( output )
unset( input )
unset( label )


######################### EMOS-262 #########################

set( label "regress_EMOS-262_intf2_msl_N640_to_regular_ll_sub-area_single-point" )
set( input "msl_N640.grib" )
set( output "${label}.grib" )
set( opt "--INTOUT:area=45.5/9.25/45.5/9.25" "--INTOUT:grid=0.125/0.125" )
list( APPEND opt "--intf2" )

interpolation_add_test_interpol( ${label} "${input}" "${output}" "${opt}" download_tests_regress_src_files )
interpolation_add_test_compare( ${label} "${output}" "${output}.ref"      download_tests_regress_ref_files )

unset( opt )
unset( output )
unset( input )
unset( label )


######################### EMOS-269 #########################

foreach( singleopt "--INTOUT:accuracy=16" "--INTOUT:interpolation=\"nearest neighbour\"" )
  if( singleopt MATCHES "accuracy" )
    set( label "regress_EMOS-269_intf2_msl_O1280_to_O1280_nointerpol_accuracy" )
  elseif( singleopt MATCHES "nearest neighbour" )
    set( label "regress_EMOS-269_intf2_msl_O1280_to_O1280_nointerpol_nearestn" )
  endif()
  set( input "msl_O1280.grib" )
  set( output "${label}.grib" )
  set( opt ${singleopt} )
  list( APPEND opt "--intf2" )

  interpolation_add_test_interpol( ${label} "${input}" "${output}" "${opt}" download_tests_regress_src_files )
  interpolation_add_test_gridname( ${label} "${output}" "O1280" )

  unset( opt )
  unset( output )
  unset( input )
  unset( label )
endforeach()


######################### EMOS-274 #########################

set( label "regress_EMOS-274_intf2_2t_N128_custompl_to_F64" )
set( input "2t_N128_custompl.grib" )
set( output "${label}.grib" )
set( opt "--INTOUT:gridname=F64" )
list( APPEND opt "--intf2" )

interpolation_add_test_interpol( ${label} "${input}" "${output}" "${opt}" download_tests_regress_src_files )
interpolation_add_test_compare(  ${label} "${output}" "${output}.ref"     download_tests_regress_ref_files )
interpolation_add_test_gridname( ${label} "${output}" "F64" )

unset( opt )
unset( output )
unset( input )
unset( label )


################### GRIB-863 / EMOS-214 ####################

# GRIB1 interpolate to regular_ll (1/16)/(1/16) degree
foreach( in "2t_O1280" )
  set( label "regress_GRIB-863_intf2_${in}_to_regular_ll_1-16" )
  set( input "${in}.grib" )
  set( output "${label}.grib" )
  set( opt "--INTOUT:grid=0.0625/0.0625" )
  list( APPEND opt "--intf2" )

  interpolation_add_test_interpol( ${label} "${input}" "${output}" "${opt}" download_tests_regress_src_files )
  interpolation_add_test_compare( ${label} "${output}" "${output}.ref"      download_tests_regress_ref_files )

  unset( opt )
  unset( output )
  unset( input )
  unset( label )
endforeach()


######################### GRIB-864 #########################

# GRIB1 sub-milli degree encoding limit
# snap-milli-degree:     snaps to 0.001 because grid is 0.00025/0.00025, and is GRIB1-encodable
# snap-sub-milli-degree: snaps to 0.0005, not GRIB1-encodable (failure expected)
foreach( snap "snap-milli-degree" "snap-sub-milli-degree_bad" "snap-sub-milli-degree_sub-area_bad" )
  set( label "regress_GRIB-864_intf2_msl_N640_to_regular_ll_${snap}" )
  set( input "msl_N640.grib" )
  set( output "${label}.grib" )
  if(     snap STREQUAL "snap-sub-milli-degree_sub-area_bad" )
    set( opt "--INTOUT:grid=0.0625/0.0625"   "--INTOUT:area=1/0.0625/0/0.5625" )
  elseif( snap STREQUAL "snap-sub-milli-degree_bad" )
    set( opt "--INTOUT:grid=0.00025/0.00025" "--INTOUT:area=0.00074/0/0/0.00074" )
  else()
    set( opt "--INTOUT:grid=0.00025/0.00025" "--INTOUT:area=0.00076/0/0/0.00076" )
  endif()
  list( APPEND opt "--intf2" )

  interpolation_add_test_interpol( ${label} "${input}" "/dev/null" "${opt}" download_tests_regress_src_files )

  # newer ecCodes (2.6.0) does not fail on encoding sub-milli degree GRIB1
  #if( snap MATCHES "sub-milli-degree" )
  #  set_tests_properties( ${label}_interpol PROPERTIES WILL_FAIL TRUE )
  #endif()

  unset( opt )
  unset( output )
  unset( input )
  unset( label )
endforeach()


#################### ECC-511 / EMOS-320 ####################

set( label "regress_ECC-511_intf2_sde_regular_ll_complex_spatial_differencing_to_regular_ll" )
set( input "sde_regular_ll_complex_spatial_differencing.grib2" )
set( output "${label}.grib2" )
set( opt "--INTOUT:grid=3/3" )
list( APPEND opt "--intf2" )

interpolation_add_test_interpol( ${label} "${input}" "${output}" "${opt}" download_tests_regress_src_files )
interpolation_add_test_compare(  ${label} "${output}" "${output}.ref"     download_tests_regress_ref_files )

unset( opt )
unset( output )
unset( input )
unset( label )


############################################################

