
# configure Fortran-based targets for double precision (compilation & linking)
if (NOT emos_double_precision_flags STREQUAL "")
  ecbuild_add_fortran_flags(${emos_double_precision_flags})
endif()


# if( CMAKE_Fortran_COMPILER_ID MATCHES "GNU" )
#   set_source_files_properties(
#     bufr_add_bias.F90
#     bufr_merg.F90
#     bufr_merge_tovs.F90
#     PROPERTIES COMPILE_FLAGS "-ffixed-form" )
# elseif( CMAKE_Fortran_COMPILER_ID MATCHES "Intel" )
#   set_source_files_properties(
#     bufr_add_bias.F90
#     bufr_merg.F90
#     bufr_merge_tovs.F90
#     PROPERTIES COMPILE_FLAGS "-fixed" )
# elseif( CMAKE_Fortran_COMPILER_ID MATCHES "Cray")
#   set_source_files_properties(
#     bufr_add_bias.F90
#     bufr_merg.F90
#     bufr_merge_tovs.F90
#     PROPERTIES COMPILE_FLAGS "-f fixed" )
# endif()


################################################################################
# sources

list( APPEND bufrtools_bins_F77
  bufr_0t2
  bufr_88t89
  bufr_check
  bufr_compress
  bufr_decode
  bufr_decode_all
  emoslib_bufr_filter
  bufr_key
  bufr_nt1
  bufr_ntm
  bufr_obs_filter
  bufr_repack
  bufr_repack_206t205
  bufr_repack_satid
  bufr_ship_anmh
  bufr_ship_anmh_ERA
  bufr_split
  snow_key_repack )

list( APPEND bufrtools_bins_F90
  bufr_add_bias
  bufr_merg
  bufr_merge_tovs
  bufr_simulate )

list( APPEND bufrtools_bins_tracks
  tc_tracks
  tc_tracks_10t5
  tc_tracks_det
  tc_tracks_eps )


################################################################################
# F77

ecbuild_add_library(
  TARGET  my_exit NOINSTALL
  TYPE    STATIC
  SOURCES my_exit.F )

foreach( tool ${bufrtools_bins_F77} )
  ecbuild_add_executable(
    TARGET      ${tool}
    SOURCES     ${tool}.F
    DEFINITIONS ${LIBEMOS_DEFINITIONS}
    LIBS        my_exit emos )
endforeach()


################################################################################
# F90

if( ENABLE_FORTRAN90 )

  ecbuild_add_library(
    TARGET  bufr_module NOINSTALL
    TYPE    STATIC
    SOURCES mod_bufr.F90 )

  foreach( tool ${bufrtools_bins_F90} )
    ecbuild_add_executable(
      TARGET      ${tool}
      SOURCES     ${tool}.F90
      DEFINITIONS ${LIBEMOS_DEFINITIONS}
      LIBS        my_exit emos )
  endforeach()

  foreach( tool ${bufrtools_bins_tracks} )
    ecbuild_add_executable(
      TARGET      ${tool}
      SOURCES     ${tool}.F90
      CONDITION   HAVE_LIBEMOS_GRIBEX
      DEFINITIONS ${LIBEMOS_DEFINITIONS}
      LIBS        bufr_module my_exit emos )
  endforeach()

  ecbuild_add_executable(
      TARGET      grib2bufr
      SOURCES     grib2bufr.F90
      CONDITION   HAVE_LIBEMOS_GRIBEX
      DEFINITIONS ${LIBEMOS_DEFINITIONS}
      LIBS        my_exit emos )

endif( ENABLE_FORTRAN90 )


################################################################################
# scripts

# bufr_filter script (See EMOS-242)
install( FILES ${CMAKE_CURRENT_SOURCE_DIR}/bufr_filter
         DESTINATION ${INSTALL_BIN_DIR}
         PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
                     GROUP_EXECUTE GROUP_READ
                     WORLD_EXECUTE WORLD_READ )

