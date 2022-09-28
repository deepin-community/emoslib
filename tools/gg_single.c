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
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "grib_api.h"


double gglonlat_(int *knum_, double plat_[], int kpts_[], double field_[], double *badvalue_, int *nn_, double *lon_, double *lat_);
int jgglat_(int *knum_, double pgauss_[]);
#ifdef FORTRAN_UPPERCASE
#define gglonlat_ GGLONLAT
#define jgglat_   JGGLAT
#endif
#ifdef FORTRAN_NO_UNDERSCORE
#define gglonlat_ gglonlat
#define jgglat_   jgglat
#endif


#ifdef FORTRAN_LINKER_PGI
#define main_ MAIN_
#else
#define main_ main
#endif


int main_(int argc, char** argv) {

  int err = 0;     /* error status */
  int i;           /* argument index */
  FILE *f = NULL;  /* lat/lon file handler */
  char line[100];  /* text line buffer */
  int nn = 0;      /* if nearest neighbour interpolation is requested */


  /* check arguments */
  if (argc<=1) {
      fprintf(stdout,
          "Returns the bilinear-interpolated value at a specific position,\n"
          "given a global, unrotated Gaussian grid description (from GRIB file).\n"
          "Usage: %s [--lat=<real> --lon=<real>|--file=<file>] [-n|] grib1 [grib2 [...]]\n"
          "with file containing \"lat lon\" pair lines.\n"
          , argv[0] );
      return 0;
  }

  double
      lon = 0.,
      lat = 0.;
  for (i=1; i<argc; ++i) {
      const char *a = argv[i];
      if      (strlen(a)>6  && (!strncmp("--lon=",a,6))) { lon = atof(&a[6]); }
      else if (strlen(a)>6  && (!strncmp("--lat=",a,6))) { lat = atof(&a[6]); }
      else if (strlen(a)==2 && (!strncmp("-n",a,2)))     { nn = 1; }
      else if (strlen(a)>7  && (!strncmp("--file=",a,7))) {
          f = fopen(&a[7],"r");
          if (f==NULL) {
              fprintf(stderr,"cannot open file \"%s\".\n",&a[7]);
              return 1;
          }
      }
  }


  /* turn on support for multi fields messages */
  grib_multi_support_on(0);


#if 0
  /* print GeoPoints header */
  fprintf(stdout,
          "#GEO\n"
          "#FORMAT XYV\n"
          "x/long y/lat value\n"
          "#DATA\n");
#endif


  /* for all lat/lon pairs, for all file arguments... */
  do {

      /* ... update lat/lon with pair from file */
      if (f!=NULL) {
          err = (NULL==fgets(line,100,f))
             || (2!=sscanf(line,"%lf %lf",&lat,&lon));
      }

      /* check bounds */
      if (lat>90. || lat<-90. || lon!=lon || lat!=lat || isinf(lon) || isinf(lat)) {
          fprintf(stderr,"latitude/longitude not usable, (lat,lon)=(%f,%f).\n",lat,lon);
          err = 2;
      }
      else {
          while (lon>=360.) { lon -= 360.; }
          while (lon<   0.) { lon += 360.; }
      }

      /* ... for all grib files, interpolate */
      for (i=1; !err && i<argc; ++i) {
          const char *a = argv[i];
          if ( !strncmp("--lon=", a,6) ||
               !strncmp("--lat=", a,6) ||
               !strncmp("-n",     a,2) ||
               !strncmp("--file=",a,7) ) {
              continue;
          }

          /* initialize and open file */
          FILE *g        = NULL;  /* file handle */
          grib_handle *h = NULL;  /* grib handle */
          if ((NULL==(g = fopen(a,"r")))) {
              fprintf(stderr,"unable to open file \"%s\" (argument #%i).\n", a, i);
              return 2;
          }

          /* iterate on file grib_handle messages, while failures don't happen */
          while (NULL!=(h = grib_handle_new_from_file(0,g,&err))) {
              GRIB_CHECK(err,0);

              /* get gridType */
              char gridType[80] = "";
              size_t size = sizeof(gridType);
              if ((err = grib_get_string(h,"gridType",gridType,&size))) {
                  fprintf(stderr,"cannot get gridType, %s\n",grib_get_error_message(err));
                  break;
              }

              /* get grid N, Nj, missing value */
              long N = 0;
              long Nj = 0;
              GRIB_CHECK(grib_get_long(h,"N",&N), 0);
              GRIB_CHECK(grib_get_long(h,"Nj",&Nj), 0);
              assert(N > 0);
              assert(Nj > 0);

              /* initialize interfacing structures */
              int knum = (int) N;
              int kpts[4000] = {0,};
              double  plat[4000] = {0,};
              double  missingValue = 0.;
              GRIB_CHECK(grib_get_double(h,"missingValue",&missingValue), 0);

              /* get pl */
              if (!strcmp(gridType,"regular_gg")) {

                  long j;
                  for (j=0; j<Nj; ++j)
                      kpts[j] = (int) (4*N);

              }
              else if (!strcmp(gridType,"reduced_gg")) {

                  size_t pl_len = 0;
                  GRIB_CHECK(grib_get_size(h, "pl", &pl_len), 0);
                  assert(pl_len > 0);
                  if (pl_len != Nj || pl_len != 2*N) {
                      fprintf(stderr, "ERROR: pl array length is %ld but should be 2*N (%ld)!\n", pl_len, 2*N);
                      err = -1;
                      break;
                  }

                  long j;
                  long *pl = (long*)malloc(pl_len * sizeof(long));
                  assert(pl);
                  GRIB_CHECK(grib_get_long_array(h, "pl", pl, &pl_len), 0);
                  for (j=0; j<pl_len; ++j)
                      kpts[j] = (int) pl[j];
                  free(pl);

              }
              else {
                  fprintf(stderr, "ERROR: only regular_gg or reduced_gg supported.\n");
                  err = -2;
                  break;
              }

              /* get grid Gaussian latitudes */
              int Nj_ = (int) Nj;
              GRIB_CHECK(jgglat_(&Nj_,plat), 0);

              /* get (suitable format) field values and interpolate */
              {
                  size_t values_len = 0;
                  GRIB_CHECK(grib_get_size(h,"values",&values_len),0);
                  assert(values_len > 0);

                  double *field = (double*) malloc(values_len*sizeof(double));
                  GRIB_CHECK(grib_get_double_array(h,"values",field,&values_len),0);

                  const double value = gglonlat_(&knum,plat,kpts,field,&missingValue,&nn,&lon,&lat);
                  fprintf(stdout, /*value==missingValue? "%f\t%f\tMISS\n":*/ "%f\t%f\t%f\n", lat, lon, value);

                  free(field);
              }

              /* close handle */
              grib_handle_delete(h);

          }

          /* close grib file */
          fclose(g);

      }

  } while (!err && f!=NULL);  /* ... for all lat/lon, grib files */


  /* close lat/lon file if open */
  if (f!=NULL) {
      err = err || !feof(f);
      fclose(f);
  }


  /* return wether all went as expected */
  return err;

}
