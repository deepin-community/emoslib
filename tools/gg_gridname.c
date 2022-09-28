/*
 * Copyright 1981-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * In applying this licence, ECMWF does not waive the privileges and immunities granted to it by
 * virtue of its status as an intergovernmental organisation nor does it submit to any jurisdiction.
 */


#include <assert.h>
#include <string.h>
#include "grib_api.h"
#include "common/fortint.h"


void jnumgg_(fortint *knum_, const char *htype_, int kpts_[], fortint *kret_);
#ifdef FORTRAN_UPPERCASE
#define jnumgg_ JNUMGG
#endif
#ifdef FORTRAN_NO_UNDERSCORE
#define jnumgg_ jnumgg
#endif


#ifdef FORTRAN_LINKER_PGI
#define main_ MAIN_
#else
#define main_ main
#endif


int main_(int argc, char** argv) {


  FILE        *f = NULL;  /* file handle    */
  grib_handle *h = NULL;  /* GRIB handle    */
  int err;                /* error status   */
  int i;                  /* argument index */

  const char* gridNameUser = NULL;  /* gridname to expect/non-expect */
  long gridNameExpected    = -1;    /* gridname expected/non-expected/unused (1/0/-1) */
  long onlySupported       =  1;    /* do check for supported grids, including comparison of pl array */
  long onlyOne             =  0;    /* do only one grib message */

  unsigned c   = 0;  /* GRIB message counter */
  long ok      = 1;  /* success state holder */
  fortint kret = 0;  /* return code when checking supported grids */

  if (argc<2) {
      fprintf(stdout,
          "Check for supported Gaussian grids short name (\"gridname\").\n"
          "Usage: %s [[--eq|--ne]=gridname] [-a] [-1] file1 [file2 [...]]\n"
          "with:\n"
          "  --[eq|ne]:  test for equality/inequality to gridname\n"
          "  -a:         no test for supported grids\n"
          "  -1:         test only the first GRIB message\n"
          "and gridname is one of the supported grids:\n"
          "  F32    N32    O32\n"
          "  F48    N48    O48\n"
          "  F64    N64    O64\n"
          "  F80    N80    O80\n"
          "  F128   N128   O128\n"
          "  F160   N160   O160\n"
          "  F200   N200   O200\n"
          "  F256   N256   O256\n"
          "  F320   N320   O320\n"
          "  F400   N400   O400\n"
          "  F512   N512   O512\n"
          "  F640   N640   O640\n"
          "  F1280         O1280\n", argv[0] );
      return 0;
  }


  /* turn on support for multi fields messages */
  grib_multi_support_on(0);


  /* check for gridname comparison equal/non-equal */
  for (i=1; i<argc; ++i) {
      const char *a = argv[i];
      if ( strlen(a)>5 && (!strncmp("--eq=",a,5) || !strncmp("--ne=",a,5)) ) {
          gridNameUser     = &a[5];
          gridNameExpected = strncmp("--ne=",a,5)!=0? 1:0;
      }
      else if (!strncmp("-a",a,2)) { onlySupported = 0; }
      else if (!strncmp("-1",a,2)) { onlyOne       = 1; }
  }


  /* for all arguments (files cannot start with "--eq=" nor "--ne=") */
  for (i=1; ok && i<argc; ++i) {
      const char *a = argv[i];
      if (!strncmp("--eq=",a,5) ||
          !strncmp("--ne=",a,5) ||
          !strncmp("-a",a,2) ||
          !strncmp("-1",a,2)) {
          continue;
      }

      /* initialize and open file */
      f   = NULL;
      h   = NULL;
      err = 0;
      if ((NULL==(f = fopen(a,"r")))) {
          fprintf(stderr,"unable to open file \"%s\" (argument #%i).\n", a, i);
          return 2;
      }

      /* iterate on file grib_handle messages, while failures don't happen */
      while ((NULL!=(h = grib_handle_new_from_file(0,f,&err))) && ok) {
          GRIB_CHECK(err,0);
          ok = 0;
          ++c;

          char
              gridName[ 6] = "",  /* accomodate "[FNO][0-9]{1,4}" */
              gridType[80] = "";  /* grib internal gridType */
          size_t size = sizeof(gridType);
          if ((err = grib_get_string(h,"gridType",gridType,&size))) {
              fprintf(stderr,"cannot get gridType, %s\n",grib_get_error_message(err));
              break;
          }

          /* handle only rotated/non-rotated regular/reduced Gaussian grids */
          const long
              isRegularGG = (!strcmp(gridType,"regular_gg") || !strcmp(gridType,"rotated_gg"))?         1:0,
              isReducedGG = (!strcmp(gridType,"reduced_gg") || !strcmp(gridType,"reduced_rotated_gg"))? 1:0;
          if (isRegularGG || isReducedGG) {

              /* get grid N and type */
              long N = 0;
              char T = 'F';

              GRIB_CHECK(grib_get_long(h, "N", &N), 0);
              assert(N > 0);

              if (isReducedGG) {
                  long isOctahedral = 0;
                  GRIB_CHECK(grib_get_long(h, "isOctahedral", &isOctahedral), 0);
                  T = isOctahedral? 'O':'N';
              }

              /* check support of known Nxxx grids */
              /* (grib_api does the check for Oxxx, above) */
              fortint kret = 0;
              if (onlySupported && T=='N') {
                  fortint knum = (fortint) N;
                  char htype[] = {T=='N'? 'R':T,' '};
                  int kpts[4000] = {0,};
                  jnumgg_(&knum,htype,kpts,&kret);
                  if (kret==0) {

                      /* get pl array */
                      size_t pl_len = 0;
                      GRIB_CHECK(grib_get_size(h, "pl", &pl_len), 0);
                      assert(pl_len > 0);
                      if (pl_len != 2*N) {
                          fprintf(stderr, "ERROR: pl array length is %ld but should be 2*N (%ld)!\n", pl_len, 2*N);
                          kret = -1;
                          break;
                      }
                      long *pl = (long*)malloc(pl_len * sizeof(long));
                      assert(pl);
                      GRIB_CHECK(grib_get_long_array(h, "pl", pl, &pl_len), 0);

                      /* compare to pl array of supported grid */
                      int j;
                      for (j=0; kret==0 && j<2*N; ++j)
                          kret = (kpts[j]!=pl[j]);

                  }
              }

              /* set gridname */
              if (kret==0) {
                  snprintf(gridName,6,"%c%li",T,N);
              }

          }

          /* handle success/failure */
          ok = gridNameExpected==1? (strncmp(gridName,gridNameUser,6)==0)
             : gridNameExpected==0? (strncmp(gridName,gridNameUser,6)!=0)
             : 1;
          if (gridNameExpected==-1) {
              fprintf(stdout,"%s\n",gridName);
          }
          if (onlyOne) {
              break;
          }

      }

      /* close handle & file */
      grib_handle_delete(h);
      fclose(f);

  }


  /* return success if all (!) went as expected, warn if not */
  ok = ok && c;
  if (!ok) {
      fprintf(stderr, "%s failure\n", argv[0]);
  }
  return !ok;

}
