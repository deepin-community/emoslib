
# https://software.ecmwf.int/wiki/display/MSAPP/Single+Point+Interpolation

if (HAVE_FORTRAN)
  list(APPEND list_samples "singlePointInterpolation")
  if (HAVE_ECCODES)
    list(APPEND list_samples "singlePointInterpolation_eccodes")
  endif()
endif()

foreach(sample ${list_samples})

  ecbuild_add_executable(
    TARGET      "ms_apps_${sample}" NOINSTALL
    SOURCES     "${sample}.f90"
    DEFINITIONS ${LIBEMOS_DEFINITIONS}
    INCLUDES    ${GRIB_API_INCLUDE_DIRS}
    LIBS        emos )

  foreach(in "2t_O640" "msl_N640" "z_sh")
    set(input "${PROJECT_SOURCE_DIR}/tests/data/${in}.grib")
    set(output "ms_apps_${sample}_run1_ouput")
    set(points "${PROJECT_SOURCE_DIR}/tests/ms_apps/input_single_points")

    if (NOT in MATCHES "_sh$" OR HAVE_FFTW)
      ecbuild_add_test(
        TARGET       "ms_apps_${sample}_${in}_bin"
        COMMAND      "ms_apps_${sample}"
        ARGS         "${points}" "${input}" "${output}"
        ENVIRONMENT  "CONFIG_INTERP=ON_FLY" ${_grib_environment} )

      set(output "ms_apps_${sample}_run2_ouput")
      ecbuild_add_test(
        TARGET       "ms_apps_${sample}_${in}_txt"
        COMMAND      "ms_apps_${sample}"
        ARGS         "${points}" "${input}" "${output}" "ascii"
        ENVIRONMENT  "CONFIG_INTERP=ON_FLY" ${_grib_environment} )
    endif()

    unset(points)
    unset(output)
    unset(input)
  endforeach()

endforeach()

