/**
* Copyright 1981-2016 ECMWF.
*
* This software is licensed under the terms of the Apache Licence
* Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
*
* In applying this licence, ECMWF does not waive the privileges and immunities
* granted to it by virtue of its status as an intergovernmental organisation
* nor does it submit to any jurisdiction.
*/


#include "grib_api.h"
#include "emos.h"
#include <assert.h>

fortint int2_gasetup(fortint isec1[], fortint isec2[], fortint isec3[], fortint isec4[], fortfloat rsec2[], fortfloat zsec3[]);

extern int mars_interpolation;

#define SET_EXTRA_LONG(x,y)     do { assert(packing_spec->extra_settings_count<80); packing_spec->extra_settings[packing_spec->extra_settings_count].name = #x; packing_spec->extra_settings[packing_spec->extra_settings_count].type = GRIB_TYPE_LONG;   packing_spec->extra_settings[packing_spec->extra_settings_count].long_value = y; packing_spec->extra_settings_count++; } while(0)
#define SET_EXTRA_DOUBLE(x,y)   do { assert(packing_spec->extra_settings_count<80); packing_spec->extra_settings[packing_spec->extra_settings_count].name = #x; packing_spec->extra_settings[packing_spec->extra_settings_count].type = GRIB_TYPE_DOUBLE; packing_spec->extra_settings[packing_spec->extra_settings_count].double_value = y; packing_spec->extra_settings_count++; } while(0)


int copy_spec_from_ksec(grib_util_grid_spec* spec,grib_util_packing_spec* packing_spec) {
    int err=0;

    fortint isec1[ISECTION_1] = {0,};
    fortint isec2[ISECTION_2] = {0,};
    fortint isec3[ISECTION_3] = {0,};
    fortint isec4[ISECTION_4] = {0,};

    fortfloat rsec2[ISECTION_2] = {0,};
    fortfloat zsec3[ISECTION_3] = {0,};

    long    rg_def[ISECTION_2] = {0,};
    char *intf2_debug = getenv("INTF2_DEBUG");

    if((err = int2_gasetup( isec1, isec2, isec3, isec4, rsec2, zsec3 )) != 0)
    {
        return err;
    }

    memset(spec,         0, sizeof(grib_util_grid_spec));
    memset(packing_spec, 0, sizeof(grib_util_packing_spec));

    switch (isec4[3]) {
        case 1:
            packing_spec->packing_type = GRIB_UTIL_PACKING_TYPE_SPECTRAL_COMPLEX;
            break;
        case 2:
            packing_spec->packing_type = GRIB_UTIL_PACKING_TYPE_SPECTRAL_SIMPLE;
            packing_spec->packing      = GRIB_UTIL_PACKING_USE_PROVIDED;
            break;
        case 3:
            packing_spec->packing_type = GRIB_UTIL_PACKING_TYPE_JPEG;
            break;
        case 4:
            packing_spec->packing_type = GRIB_UTIL_PACKING_TYPE_GRID_SIMPLE;
            break;
        case 7:
            packing_spec->packing_type = GRIB_UTIL_PACKING_TYPE_GRID_SECOND_ORDER;
            break;
        default :
            printf("invalid isec4[3] = %d\n",isec4[3]);
            abort();
    }
    packing_spec->bitsPerValue     = isec4[1];

    /* TODO: get that from emoslib */
    if(intf2_debug){
        printf("YYYYYYYYYY  packing: %d    -----  bitsPerValue: %d \n", isec4[3],isec4[1]);
        printf("missing value XXXXXXXXXX %f\n", zsec3[1]);
    }

    spec->missingValue = zsec3[1];


    if(intf2_debug){
        printf("missing value XXXXXXXXXX %f \n", spec->missingValue);
        printf("SIZES double=%lu  fortfloat=%lu\n", sizeof(double), sizeof(fortfloat));
        printf("is BITMAP present %d\n", isec1[4]);
    }

    switch(isec2[0]) {

        case 0:
        case 10:
            spec->grid_type                          = GRIB_UTIL_GRID_SPEC_REGULAR_LL;
            spec->Nj                                 = isec2[2];
            spec->Ni                                 = isec2[1];

            /* EMOS-214: pick REAL values from parallel data structure rsec2 (the ones requiring sub-millidegree precision) */
            spec->iDirectionIncrementInDegrees       = rsec2[8];  /* isec2[8]/1000.0; */
            spec->jDirectionIncrementInDegrees       = rsec2[9];  /* isec2[9]/1000.0; */

            spec->latitudeOfFirstGridPointInDegrees  = rsec2[3];  /* isec2[3]/1000.0; */
            spec->longitudeOfFirstGridPointInDegrees = rsec2[4];  /* isec2[4]/1000.0; */
            spec->latitudeOfLastGridPointInDegrees   = rsec2[6];  /* isec2[6]/1000.0; */
            spec->longitudeOfLastGridPointInDegrees  = rsec2[7];  /* isec2[7]/1000.0; */

            if (isec2[0]==10) {
                spec->grid_type                        = GRIB_UTIL_GRID_SPEC_ROTATED_LL;
                spec->uvRelativeToGrid                 = isec2[18]==8 ? 1 : 0;
                spec->latitudeOfSouthernPoleInDegrees  = rsec2[12];  /* isec2[12]/1000.0; */
                spec->longitudeOfSouthernPoleInDegrees = rsec2[13];  /* isec2[13]/1000.0; */
            }

            spec->bitmapPresent = (isec1[4]==192 || isec1[4]==64)? 1:0;

            break;

        case 4:
            spec->grid_type                          = GRIB_UTIL_GRID_SPEC_REGULAR_GG;
            spec->Nj                                 = isec2[2];
            spec->Ni                                 = isec2[1];
            spec->N                                  = isec2[9];
            if (isec2[16]!=0) {
                spec->grid_type = GRIB_UTIL_GRID_SPEC_REDUCED_GG;

                /* allocate & copy pl array */
                spec->pl_size = (long)(isec2[2]);
                long* pl = (long*)malloc( spec->pl_size * sizeof(long) );
                assert(pl);
                long i;
                for (i=0; i<spec->pl_size; ++i) {
                    pl[i] = isec2[22+i];
                }
                assert(!spec->pl);
                spec->pl = pl;
            }
            else {

                /* EMOS-214: pick REAL values from parallel data structure rsec2 (the ones requiring sub-millidegree precision) */
                spec->iDirectionIncrementInDegrees   = rsec2[8];  /* isec2[8]/1000.0; */

            }

            /* EMOS-214: pick REAL values from parallel data structure rsec2 (the ones requiring sub-millidegree precision) */
            spec->latitudeOfFirstGridPointInDegrees  = rsec2[3];  /* isec2[3]/1000.0; */
            spec->longitudeOfFirstGridPointInDegrees = rsec2[4];  /* isec2[4]/1000.0; */
            spec->latitudeOfLastGridPointInDegrees   = rsec2[6];  /* isec2[6]/1000.0; */
            spec->longitudeOfLastGridPointInDegrees  = rsec2[7];  /* isec2[7]/1000.0; */

            spec->bitmapPresent = (isec1[4]==192 || isec1[4]==64)? 1:0;


            /* grib_api to set global area in full precision for gaussian grid */
            packing_spec->extra_settings_count=1;
            packing_spec->extra_settings[0].type = GRIB_TYPE_LONG;
            packing_spec->extra_settings[0].name = "global";
            packing_spec->extra_settings[0].long_value = int2_global();

            break;

        case 14:
            spec->grid_type                          = GRIB_UTIL_GRID_SPEC_ROTATED_GG;
            spec->Nj                                 = isec2[2];
            spec->Ni                                 = isec2[1];
            spec->N                                  = isec2[9];

            /* EMOS-214: pick REAL values from parallel data structure rsec2 (the ones requiring sub-millidegree precision) */
            spec->iDirectionIncrementInDegrees       = rsec2[8];  /* isec2[8]/1000.0; */

            if (isec2[16]!=0) {
                /* There is no rotated-reduced, is there ? */
                abort();
            }

            /* EMOS-214: pick REAL values from parallel data structure rsec2 (the ones requiring sub-millidegree precision) */
            spec->latitudeOfFirstGridPointInDegrees  = rsec2[3];  /* isec2[3]/1000.0; */
            spec->longitudeOfFirstGridPointInDegrees = rsec2[4];  /* isec2[4]/1000.0; */
            spec->latitudeOfLastGridPointInDegrees   = rsec2[6];  /* isec2[6]/1000.0; */
            spec->longitudeOfLastGridPointInDegrees  = rsec2[7];  /* isec2[7]/1000.0; */

            spec->latitudeOfSouthernPoleInDegrees    = rsec2[12];  /* isec2[12]/1000.0; */
            spec->longitudeOfSouthernPoleInDegrees   = rsec2[13];  /* isec2[13]/1000.0; */

            spec->bitmapPresent = (isec1[4]==192 || isec1[4]==64)? 1:0;

            break;


        case 50:
            spec->grid_type = GRIB_UTIL_GRID_SPEC_SH;
            spec->truncation = isec2[1]; /* truncationJ */
            packing_spec->truncateLaplacian=1;

            break;

        case 26:
            spec->grid_type = GRIB_UTIL_GRID_SPEC_REDUCED_LL;
            break;

        default:
            fprintf(stdout,"INTF2: ERROR - unknown repres: %d\n", isec2[0]);
            return -2;
            break;
    }

    /* ocean special case */
    if (isec1[36]==4) {
        /* Bitmap always present for ocean */

        SET_EXTRA_LONG(dataRepresentationType , (isec1[59] == 3 && isec1[60] == 4)? 0 : 192);
        SET_EXTRA_LONG(coordinate4OfFirstGridPoint,isec1[61]);
        SET_EXTRA_LONG(coordinate3OfFirstGridPoint,isec1[62]);
        SET_EXTRA_LONG(coordinate4OfLastGridPoint,isec1[63]);
        SET_EXTRA_LONG(coordinate3OfLastGridPoint,isec1[64]);
        SET_EXTRA_LONG(iIncrement,isec1[65]);
        SET_EXTRA_LONG(jIncrement,isec1[66]);
        SET_EXTRA_LONG(flagForNormalOrStaggeredGrid,isec1[68]);
        SET_EXTRA_LONG(flagForIrregularGridCoordinateList,0);
        SET_EXTRA_LONG(numberInTheGridCoordinateList,0);
        SET_EXTRA_LONG(flagShowingPostAuxiliaryArrayInUse,0);
        SET_EXTRA_LONG(flagShowingPostAuxiliaryArrayInUse,1);
        SET_EXTRA_LONG(sizeOfPostAuxiliaryArrayPlusOne,0);

    }

#define D(a) if(intf2_debug) printf("YYYYYYYYYYYY %s -> %g\n", #a, (double)spec->a);
    D(grid_type);
    D(Ni);
    D(Nj);
    D(iDirectionIncrementInDegrees);
    D(jDirectionIncrementInDegrees);
    D(longitudeOfFirstGridPointInDegrees);
    D(longitudeOfLastGridPointInDegrees);
    D(latitudeOfFirstGridPointInDegrees);
    D(latitudeOfLastGridPointInDegrees);
    D(latitudeOfSouthernPoleInDegrees);
    D(longitudeOfSouthernPoleInDegrees);
    D(N);
    D(bitmapPresent);
    D(missingValue);
    D(pl_size);
    D(truncation);
#undef D

    return 0;
}
