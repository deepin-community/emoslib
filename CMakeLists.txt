################################################################################
# cmake options:
#   -DCMAKE_BUILD_TYPE=Debug|RelWithDebInfo|Release|Production
#   -DCMAKE_INSTALL_PREFIX=/path/to/install
#
#   -DCMAKE_C_COMPILER=gcc
#   -CMAKE_Fortran_COMPILER=gfortran
#
#   -DCMAKE_PREFIX_PATH=/path/to/jasper:/path/to/any/package/out/of/place
#   -DBUILD_SHARED_LIBS=OFF

cmake_minimum_required( VERSION 2.8.11 FATAL_ERROR )

project( libemos )

enable_language( Fortran )

set( CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH} "${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild/cmake")

include( ecbuild_system NO_POLICY_SCOPE )

ecbuild_requires_macro_version( 2.6 )

ecbuild_add_option( FEATURE INSTALL_TABLES   DESCRIPTION "install BUFR/GRIBex/LSM tables"                                               DEFAULT ON )
ecbuild_add_option( FEATURE INSTALL_TOOLS    DESCRIPTION "install BUFR/GRIBex tools"                                                    DEFAULT ON )
ecbuild_add_option( FEATURE FORTRAN90        DESCRIPTION "enable tools which need Fortran 90 (only required for FC=pgf77)"              DEFAULT ON )
ecbuild_add_option( FEATURE SINGLE_PRECISION DESCRIPTION "enable single precision version of library (in addition to double precision)" DEFAULT ON )
ecbuild_add_option( FEATURE GRIBEX_ABORT     DESCRIPTION "abort execution on GRIBex calls"                                              DEFAULT ON )

ecbuild_add_option( FEATURE REQUIRE_FFTW     DESCRIPTION "require package: Fastest Fourier Transform in the West" DEFAULT ON )
ecbuild_add_option( FEATURE FFTW             DESCRIPTION "Fastest Fourier Transform in the West"                  DEFAULT ON REQUIRED_PACKAGES FFTW )
set_package_properties( FFTW PROPERTIES TYPE RECOMMENDED PURPOSE "allows interpolation of spectral to octahedral reduced Gaussian grid" )

ecbuild_add_option( FEATURE LIBEMOS_BUFRDC        DESCRIPTION "enable BUFR decoding functionality" DEFAULT ON )
ecbuild_add_option( FEATURE LIBEMOS_INTERPOLATION DESCRIPTION "enable interpolation functionality" DEFAULT ON )
ecbuild_add_option( FEATURE LIBEMOS_GRIBEX        DESCRIPTION "enable GRIBex functionality"        DEFAULT ON )
ecbuild_add_option( FEATURE LIBEMOS_TESTS_REGRESS DESCRIPTION "additional tests: regression"       DEFAULT OFF )

ecbuild_add_option( FEATURE LIBEMOS_BUILD_SHARED_LIBS DESCRIPTION "build shared libraries (unsupported)"   DEFAULT OFF )

################################################################################
# compiler/linker flags

if( ${CMAKE_SYSTEM_NAME} MATCHES "Linux" )
  set( emos_special_system_def linux FOPEN64 )
elseif( ${CMAKE_SYSTEM_NAME} MATCHES "Darwin" )
  set( emos_special_system_def darwin )
endif()

get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

# double precision flags: only for d.p. Fortran-based targets (compiling & linking)
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(emos_double_precision_flags "-fdefault-real-8 -fdefault-double-8")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(emos_double_precision_flags "-r8 -i4")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
  set(emos_double_precision_flags "-r8 -i4")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Cray")
  set(emos_double_precision_flags "-s integer32 -s real64")
else()
  set(emos_double_precision_flags "")
endif()

# build type flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  ecbuild_add_fortran_flags("-ffixed-line-length-none -fcray-pointer -fno-second-underscore -Wuninitialized -Wunused-variable -DSHAREDMEMORY")
  ecbuild_add_fortran_flags("-O2 -mtune=native" BUILD RELEASE)
  ecbuild_add_fortran_flags("-O2 -mtune=native" BUILD RELWITHDEBINFO)
  ecbuild_add_fortran_flags("-g"                BUILD RELWITHDEBINFO)
  ecbuild_add_fortran_flags("-g"                BUILD DEBUG)
  if(ECBUILD_MACRO_VERSION VERSION_GREATER 1.9)
    ecbuild_remove_fortran_flags("-fcheck=bounds" BUILD DEBUG)
  endif()
  include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})  # EMOS-243: support for older gfortran 4.1 compiler
  set(emos_special_compiler_def "gfortran")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  ecbuild_add_fortran_flags("-DSHAREDMEMORY")
  ecbuild_add_fortran_flags("-f77rtl"    BUILD RELEASE)
  ecbuild_add_fortran_flags("-f77rtl -g" BUILD RELWITHDEBINFO)
  ecbuild_add_fortran_flags("-f77rtl -g" BUILD DEBUG)
  if(ECBUILD_MACRO_VERSION VERSION_GREATER 1.9)
    ecbuild_remove_fortran_flags("-check bounds" BUILD DEBUG)
  endif()
  if(HAVE_LIBEMOS_GRIBEX)
    ecbuild_add_fortran_flags("-O1" BUILD RELEASE)         # otherwise breaks GRIBex second-order packing
    ecbuild_add_fortran_flags("-O1" BUILD RELWITHDEBINFO)  # otherwise breaks GRIBex second-order packing
    ecbuild_add_fortran_flags("-O0" BUILD DEBUG)
  else()
    ecbuild_add_fortran_flags("-O3" BUILD RELEASE)
    ecbuild_add_fortran_flags("-O2" BUILD RELWITHDEBINFO)
    ecbuild_add_fortran_flags("-O0" BUILD DEBUG)
  endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
  ecbuild_add_fortran_flags("-fast -Kieee -Mvect=prefetch -noswitcherror -Mextend")
  ecbuild_add_fortran_flags("-O2"    BUILD RELEASE)
  ecbuild_add_fortran_flags("-g -O2" BUILD RELWITHDEBINFO)
  ecbuild_add_fortran_flags("-g"     BUILD DEBUG)
  include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})  # EMOS-314: support for pgi 16.4
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Cray")
  ecbuild_add_fortran_flags("-ev -Wl,--as-needed -h PIC -emf -hflex_mp=conservative -hfp1 -hadd_paren -DSHAREDMEMORY")
  ecbuild_add_fortran_flags("-G2"       BUILD RELWITHDEBINFO)
  ecbuild_add_fortran_flags("-G0 -hfp0" BUILD DEBUG)
  ecbuild_add_c_flags("-Wl,--as-needed -h PIC -DSHAREDMEMORY")
else()
  ecbuild_warn("No optimized Fortran compiler flags are known, we just try -O2...")
  ecbuild_add_fortran_flags("-O2"    BUILD RELEASE)
  ecbuild_add_fortran_flags("-O2 -g" BUILD RELWITHDEBINFO)
  ecbuild_add_fortran_flags("-O0 -g" BUILD DEBUG)
endif()


if(LIBEMOS_BUILD_SHARED_LIBS)
  ecbuild_warn("Building libemos as shared library is unsupported and at own risk")
  set( LIB_TYPE SHARED )
else()
  set( LIB_TYPE STATIC )
endif()


################################################################################
# extra packages and projects

ecbuild_debug_var(CMAKE_PREFIX_PATH)
set( ECCODES_FAIL_MSG
    "Note: grib_api can be used instead (select with -DENABLE_ECCODES=OFF), "
    "but we strongly advise you to upgrade to ecCodes. "
    "See https://software.ecmwf.int/wiki/display/ECC/ecCodes+Home\n" )
ecbuild_add_option( FEATURE ECCODES DESCRIPTION "Use eccodes instead of grib_api"
                    REQUIRED_PACKAGES "PROJECT eccodes VERSION 2.4.1 REQUIRED FAILURE_MSG ECCODES_FAIL_MSG"
                    DEFAULT ON )

if( HAVE_ECCODES)
  set( GRIB_API_INCLUDE_DIRS ${ECCODES_INCLUDE_DIRS} )
  set( GRIB_API_LIBRARIES    ${ECCODES_LIBRARIES} )
  set( GRIB_API_DEFINITIONS  ${ECCODES_DEFINITIONS} )
  set( grib_api_BASE_DIR     ${eccodes_BASE_DIR} )
  set( grib_handling_pkg eccodes )
else()
  ecbuild_use_package( PROJECT grib_api VERSION 1.23.1 REQUIRED
      FAILURE_MSG "We strongly advise to use ecCodes (enable with -DENABLE_ECCODES=ON)" )
  set( grib_handling_pkg grib_api )
endif()

################################################################################
# contents

ecbuild_declare_project()

# project export variables
set( LIBEMOS_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR} )
set( LIBEMOS_LIBRARIES    emos ${GRIB_API_LIBRARIES} )

set_directory_properties(PROPERTIES COMPILE_DEFINITIONS "${GRIB_API_DEFINITIONS}" )

include_directories( ${LIBEMOS_INCLUDE_DIRS}  )
include_directories( ${GRIB_API_INCLUDE_DIRS} )
if( HAVE_FFTW )
  include_directories( ${FFTW_INCLUDES} )
elseif( HAVE_REQUIRE_FFTW )
  ecbuild_critical("\nCannot build without FFTW support (support is required)!")
endif()

if( BUFR_TABLES_PATH )
  if( NOT IS_ABSOLUTE "${BUFR_TABLES_PATH}" )
    set( BUFR_TABLES_PATH "${CMAKE_INSTALL_PREFIX}/${BUFR_TABLES_PATH}" )
  endif()
else()
  set( BUFR_TABLES_PATH "${CMAKE_INSTALL_PREFIX}/${INSTALL_DATA_DIR}/../bufrdc_tables/" )
endif()
add_definitions( -DBUFR_TABLES_PATH="${BUFR_TABLES_PATH}" )
message( STATUS "BUFR_TABLES_PATH: ${BUFR_TABLES_PATH}" )

# get_directory_property( LIBEMOS_DEFINITIONS COMPILE_DEFINITIONS ) # don't export libemos definitions

# config header
ecbuild_generate_config_headers( DESTINATION ${INSTALL_INCLUDE_DIR}/libemos )

configure_file( libemos_config.h.in  libemos_config.h  @ONLY )
configure_file( libemos_version.h.in libemos_version.h @ONLY )
configure_file( libemos_version.c.in libemos_version.c @ONLY )

install(FILES
  ${CMAKE_CURRENT_BINARY_DIR}/libemos_config.h
  ${CMAKE_CURRENT_BINARY_DIR}/libemos_version.h
  DESTINATION
  ${INSTALL_INCLUDE_DIR}/libemos )


################################################################################
# sources

if( HAVE_LIBEMOS_INTERPOLATION AND NOT HAVE_LIBEMOS_GRIBEX )
  message( FATAL_ERROR "\nCannot build libemos interpolation without GRIBex support!\nPlease pass option to cmake -DENABLE_LIBEMOS_GRIBEX=ON")
endif()

add_subdirectory( common )
add_subdirectory( pbio )

if( HAVE_LIBEMOS_GRIBEX )
  add_subdirectory( gribex )
endif()

if( HAVE_LIBEMOS_INTERPOLATION )
  add_subdirectory( interpolation )
  add_subdirectory( sandbox )
endif()

if( HAVE_LIBEMOS_BUFRDC )
  add_subdirectory( bufrdc_wmo )
endif()

if( ENABLE_INSTALL_TOOLS )
  add_subdirectory( tools )
endif()

if( HAVE_TESTS )
  add_subdirectory( tests )
endif()

#DEFINITIONS  FORTRAN_NO_UNDERSCORE

if( HAVE_LIBEMOS_GRIBEX )
  if( ${Fortran_COMPILER_NAME} MATCHES "ifort" OR CMAKE_Fortran_COMPILER_ID MATCHES "Intel" )
    set_source_files_properties( ${gribex_srcs} PROPERTIES COMPILE_FLAGS "-O0" )
  endif()
endif( HAVE_LIBEMOS_GRIBEX )


list( APPEND libemos_srcs
  ${emos_common_src_files}
  ${interpolation_srcs}
  ${gribex_srcs}
  ${bufrdc_wmo_srcs}
  ${pbio_srcs} )


################################################################################
# tables: BUFR and LSM interpolation

# should we install the old tables?
# probably only for new architectures
if( ENABLE_INSTALL_TABLES )
  install(
    DIRECTORY
      bufrtables
      gribtables
      gribtemplates
    DESTINATION ${INSTALL_DATA_DIR}/tables
    FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
endif()

### interpolation tables are always installed
if( NOT INTERPOL_TABLES_PATH )
  set( INTERPOL_TABLES_PATH "${INSTALL_DATA_DIR}/tables" )
endif()

# tables path to passed as PATH into compilation -D must be absolute
if( IS_ABSOLUTE "${INTERPOL_TABLES_PATH}" )
  set(emos_interpol_tables_def INTERPOL_TABLES_PATH="${INTERPOL_TABLES_PATH}" )
else()
  set(emos_interpol_tables_def INTERPOL_TABLES_PATH="${CMAKE_INSTALL_PREFIX}/${INTERPOL_TABLES_PATH}" )
endif()
ecbuild_info( "INTERPOL_TABLES_PATH:   ${INTERPOL_TABLES_PATH}" )

# copy the tables/interpol to the build directory
file( COPY tables/interpol DESTINATION ${CMAKE_BINARY_DIR}/${INSTALL_DATA_DIR}/tables )

# always install tables/interpol
install( DIRECTORY tables/interpol DESTINATION "${INTERPOL_TABLES_PATH}" FILE_PERMISSIONS OWNER_READ GROUP_READ WORLD_READ )
install( CODE "execute_process(COMMAND \"${CMAKE_COMMAND}\" -E create_symlink interpol ${CMAKE_INSTALL_PREFIX}/${INSTALL_DATA_DIR}/tables/land_sea_mask)" )


################################################################################
# final definitions

list( APPEND LIBEMOS_DEFINITIONS
  ${emos_special_compiler_def}
  ${emos_special_system_def}
  REAL_8
  REAL_BIGGER_THAN_INTEGER
  POINTER_64
  _LARGEFILE64_SOURCE
  _FILE_OFFSET_BITS=64
  LITTLE_ENDIAN
  INTEGER_IS_INT
  ${GRIB_API_DEFINITIONS} )

# FIXME: LITTLE_ENDIAN is not portable!
# (EMOS-172: remove this flag, only used in GRIBEX, temporarily enabled for now)


################################################################################
# targets: libraries (single & douple p.) and version-check executable

set( LIBEMOS_BASE_DIR ${CMAKE_CURRENT_SOURCE_DIR} )

if( HAVE_SINGLE_PRECISION )
  add_subdirectory( libemos-sp )
endif()
add_subdirectory( libemos-dp )

if( CMAKE_Fortran_COMPILER_ID MATCHES "PGI" )
  set_source_files_properties( version.c PROPERTIES COMPILE_DEFINITIONS "FORTRAN_LINKER_PGI" )
  ecbuild_add_executable(
    TARGET      libemos_version
    DEFINITIONS ${LIBEMOS_DEFINITIONS}
    LIBS        emos
    SOURCES     version.c
    LINKER_LANGUAGE Fortran )
else()
  ecbuild_add_executable(
    TARGET      libemos_version
    DEFINITIONS ${LIBEMOS_DEFINITIONS}
    LIBS        emos
    SOURCES     version.c )
endif()

ecbuild_add_resources( TARGET libemos_rcs SOURCES_DONT_PACK compile.sh )


################################################################################
# finalize

ecbuild_pkgconfig(
  NAME        libemosR64
  URL         "https://software.ecmwf.int/wiki/display/EMOS/"
  DESCRIPTION "Interpolation library, including BUFR & CREX encoding/decoding routines"
  LANGUAGES   C FORTRAN
  LIBRARIES   emos )  # (double p. target)

if( HAVE_SINGLE_PRECISION )
  ecbuild_pkgconfig(
    NAME        libemos
    URL         "https://software.ecmwf.int/wiki/display/EMOS/"
    DESCRIPTION "Interpolation library, including BUFR & CREX encoding/decoding routines (single precision)"
    LANGUAGES   C FORTRAN
    LIBRARIES   emos_sp )  # (single p. target)
endif()

ecbuild_install_project( NAME libemos )
ecbuild_print_summary()

