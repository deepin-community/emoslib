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


#include <assert.h>
#include <string.h>
#include "grib_api.h"
#include "emos.h"


fortint int2_intin (const char* param, fortint iv[], fortfloat dv[], const char* cv);
fortint int2_intout(const char* param, fortint iv[], fortfloat dv[], const char* cv);


static long levTypeInt(char* typeOfLevel) {
    if (!typeOfLevel || strlen(typeOfLevel) == 0)           return 255;
    if (!strcmp(typeOfLevel,"missing"))                     return 255;
    if (!strcmp(typeOfLevel,"surface"))                     return 1;
    if (!strcmp(typeOfLevel,"cloudBase"))                   return 2;
    if (!strcmp(typeOfLevel,"cloudTop"))                    return 3;
    if (!strcmp(typeOfLevel,"isothermZero"))                return 4;
    if (!strcmp(typeOfLevel,"adiabaticCondensation"))       return 5;
    if (!strcmp(typeOfLevel,"maxWind"))                     return 6;
    if (!strcmp(typeOfLevel,"tropopause"))                  return 7;
    if (!strcmp(typeOfLevel,"nominalTop"))                  return 8;
    if (!strcmp(typeOfLevel,"seaBottom"))                   return 9;

    if (!strcmp(typeOfLevel,"isobaricInhPa"))               return 100;
    if (!strcmp(typeOfLevel,"isobaricLayer"))               return 101;
    if (!strcmp(typeOfLevel,"meanSea"))                     return 102;
    if (!strcmp(typeOfLevel,"heightAboveSea"))              return 103;
    if (!strcmp(typeOfLevel,"heightAboveSeaLayer"))         return 104;
    if (!strcmp(typeOfLevel,"heightAboveGround"))           return 105;
    if (!strcmp(typeOfLevel,"heightAboveGroundLayer"))      return 106;
    if (!strcmp(typeOfLevel,"sigma"))                       return 107;
    if (!strcmp(typeOfLevel,"sigmaLayer"))                  return 108;
    if (!strcmp(typeOfLevel,"hybrid"))                      return 109;
    if (!strcmp(typeOfLevel,"hybridLayer"))                 return 110;

    if (!strcmp(typeOfLevel,"depthBelowLand"))              return 111;
    if (!strcmp(typeOfLevel,"depthBelowLandLayer"))         return 112;
    if (!strcmp(typeOfLevel,"theta"))                       return 113;
    if (!strcmp(typeOfLevel,"thetaLayer"))                  return 114;
    if (!strcmp(typeOfLevel,"pressureFromGround"))          return 115;
    if (!strcmp(typeOfLevel,"pressureFromGroundLayer"))     return 116;
    if (!strcmp(typeOfLevel,"potentialVorticity"))          return 117;
    if (!strcmp(typeOfLevel,"isobaricLayerHighPrecision"))  return 121;

    if (!strcmp(typeOfLevel,"heightAboveGroundHighPrecision"))  return 125;

    if (!strcmp(typeOfLevel,"sigmaLayerHighPrecision"))     return 128;
    if (!strcmp(typeOfLevel,"isobaricLayerMixedPrecision")) return 141;
    if (!strcmp(typeOfLevel,"depthBelowSea"))               return 160;

    if (!strcmp(typeOfLevel,"entireAtmosphere"))            return 200;
    if (!strcmp(typeOfLevel,"entireOcean"))                 return 201;

    if (!strcmp(typeOfLevel,"isobaricInPa"))                return 210;
    if (!strcmp(typeOfLevel,"oceanWave"))                   return 211;
    if (!strcmp(typeOfLevel,"oceanMixedLayer"))             return 212;


    fprintf(stderr,"levTypeInt: Level type NOT recognized %s\n",typeOfLevel);
    return 255;
}


long describe_input_field(grib_handle* handle, long outputRepresentation) {

    char *text = "";
    size_t size;

    long   rll_def[3000];
    fortint   intv[4000];
    fortfloat realv[4];

    char grid_type[80];
    int err = 0;
    int i = 0;

    const void* temp;
    double missingValue;
    long level = 0;
    long levelType;
    long parameter;
    long accuracy;
    long scanningMode;
    long table;
    long date;
    long missingValuesPresent = 0;
    char *intf2_debug = getenv("INTF2_DEBUG");
    long localNumber = 0;
    char typeOfLevel[50]={0,};
    size_t typeOfLevelLen=50;
    long niwe = 0;
    long nins = 0;
    long matrixOfValues = 0;
    long centre = 0;

    if(!intf2_debug) intf2_debug=getenv("EMOSLIB_DEBUG");

    size = sizeof(grid_type);
    if ((err = grib_get_string(handle,"typeOfGrid",grid_type,&size)))
    {
        fprintf(stderr,"Cannot get gridType %s\n",grib_get_error_message(err));
    }

    if(intf2_debug)
    {
        printf("DESCRIBE_INPUT_FIELD:**************************** \n");
        printf("DESCRIBE_INPUT_FIELD: Input Grid Type - %s \n",grid_type);
    }


    /************************************************************/
    /* Describe input unpacked field for INTF */

    /* General features */

    if ((err = int2_intin("form",intv,realv,"unpacked")))
    {
        fprintf(stderr,"Form setup INTIN failed %d\n",err);
    }

    /* Get Local Definition */
    err = grib_get_long(handle,"localDefinitionNumber",&localNumber);
    if (err == GRIB_SUCCESS)
    {
        intv[0] = localNumber;
        if ((err = int2_intin("local",intv,realv,text)))
        {
            fprintf(stderr,"localDefinitionNumber setup INTIN failed %d\n",err);
        }
    }

    if (localNumber == 4)
    {
        /* OCEAN */
        long gridCoordinate[1000];
        size_t o_length = 0;
        long oo_length = 0, vertCooDef = 0;
        long coordinate3Flag,coordinate4Flag,coordinate4OfFirstGridPoint,coordinate3OfFirstGridPoint;
        long coordinate4OfLastGridPoint,coordinate3OfLastGridPoint,iIncrement,jIncrement,flagForIrregularGridCoordinateList,flagForNormalOrStaggeredGrid;
        if(intf2_debug)
        {
            printf("DESCRIBE_INPUT_FIELD:******* OCEAN FIELD ********* \n");
        }
        /* starts with KSEC1(60) */
        if ((err = grib_get_long(handle,"coordinate3Flag",&coordinate3Flag)))
            fprintf(stderr,"Cannot get coordinate3Flag %s\n",grib_get_error_message(err));
        intv[0] = coordinate3Flag;
        if ((err = grib_get_long(handle,"coordinate4Flag",&coordinate4Flag)))
            fprintf(stderr,"Cannot get coordinate4Flag %s\n",grib_get_error_message(err));
        intv[1] = coordinate4Flag;
        if ((err = grib_get_long(handle,"coordinate4OfFirstGridPoint",&coordinate4OfFirstGridPoint)))
            fprintf(stderr,"Cannot get coordinate4OfFirstGridPoint %s\n",grib_get_error_message(err));
        intv[2] = coordinate4OfFirstGridPoint;
        if ((err = grib_get_long(handle,"coordinate3OfFirstGridPoint",&coordinate3OfFirstGridPoint)))
            fprintf(stderr,"Cannot get coordinate3OfFirstGridPoint %s\n",grib_get_error_message(err));
        intv[3] = coordinate3OfFirstGridPoint;
        if ((err = grib_get_long(handle,"coordinate4OfLastGridPoint",&coordinate4OfLastGridPoint)))
            fprintf(stderr,"Cannot get coordinate4OfLastGridPoint %s\n",grib_get_error_message(err));
        intv[4] = coordinate4OfLastGridPoint;
        if ((err = grib_get_long(handle,"coordinate3OfLastGridPoint",&coordinate3OfLastGridPoint)))
            fprintf(stderr,"Cannot get coordinate3OfLastGridPoint %s\n",grib_get_error_message(err));
        intv[5] = coordinate3OfLastGridPoint;
        if ((err = grib_get_long(handle,"iIncrement",&iIncrement)))
            fprintf(stderr,"Cannot get iIncrement %s\n",grib_get_error_message(err));
        if(intf2_debug)
        {
            printf("DESCRIBE_INPUT_FIELD:******* north = %ld, east = %ld , south = %ld, west = %ld ********* \n", coordinate4OfFirstGridPoint,coordinate3OfFirstGridPoint,coordinate4OfLastGridPoint,coordinate3OfLastGridPoint);
        }
        intv[6] = iIncrement;
        if ((err = grib_get_long(handle,"jIncrement",&jIncrement)))
            fprintf(stderr,"Cannot get jIncrement %s\n",grib_get_error_message(err));
        intv[7] = jIncrement;
        if ((err = grib_get_long(handle,"flagForIrregularGridCoordinateList",&flagForIrregularGridCoordinateList)))
            fprintf(stderr,"Cannot get flagForIrregularGridCoordinateList %s\n",grib_get_error_message(err));
        intv[8] = flagForIrregularGridCoordinateList;
        if ((err = grib_get_long(handle,"flagForNormalOrStaggeredGrid",&flagForNormalOrStaggeredGrid)))
            fprintf(stderr,"Cannot get flagForNormalOrStaggeredGrid %s\n",grib_get_error_message(err));
        intv[9] = flagForNormalOrStaggeredGrid;

        if ((err = grib_get_long(handle,"numberInTheGridCoordinateList",&oo_length)))
            fprintf(stderr,"Cannot get numberInTheGridCoordinateList %s\n",grib_get_error_message(err));
        intv[10] = oo_length;
        if ((err = grib_get_long(handle,"verticalCoordinateDefinition",&vertCooDef)))
            fprintf(stderr,"Cannot get verticalCoordinateDefinition %s\n",grib_get_error_message(err));
        intv[11] = vertCooDef;

        /* INTIN for ocean */
        if ((err = int2_intin("ocean",intv,realv,text)))
        {
            fprintf(stderr,"Ocean setup INTIN failed %d\n",err);
        }

        /* if (coordinate3Flag != 3 || coordinate4Flag != 4) { */
        /* if (coordinate3Flag == 3 && coordinate4Flag == 4) { */
        /* Tim and Sinisa  */
        if ((err = grib_get_size(handle,"gridCoordinate",&o_length)))
        {
            fprintf(stderr,"Cannot get size of pl %s\n",grib_get_error_message(err));
        }

        /* Just for Horizontal (lat/long) field interpolation */
        if ((err = grib_get_long_array(handle,"gridCoordinate",gridCoordinate,&o_length)))
            fprintf(stderr,"Cannot get gridCoordinate %s\n",grib_get_error_message(err));

        if(intf2_debug)
            printf("DESCRIBE_INPUT_FIELD:  o_length %lu \n", o_length);

        for( i = 0; i< o_length; i++)
        {
            intv[i] = gridCoordinate[i];
            /* printf("DESCRIBE_INPUT_FIELD: ocean horizontal  %d :  %d  \n", i,intv[i]); */
        }
        /* INTIN for ocean gridCoordinate */
        if ((err = int2_intin("coord_ocean",intv,realv,text)))
        {
            fprintf(stderr,"Ocean setup INTIN failed %d\n",err);
        }
        /* } */
        err = grib_get_double(handle,"missingValue",&missingValue);

        if (err != GRIB_SUCCESS && err != GRIB_NOT_FOUND) {
            fprintf(stderr,"Missing value for OCEAN %s\n",grib_get_error_message(err));
        }
        if(intf2_debug)
            printf("DESCRIBE_INPUT_FIELD: OCEAN Field Missing Value %f \n",missingValue);
        if(err == GRIB_SUCCESS)
        {
            realv[0] = missingValue;
            text = "yes";
            if ((err = int2_intin("missingvalue",intv,realv,text)))
            {
                fprintf(stderr,"Missing Value setup INTIN failed %d\n",err);
            }
        }
    }


    /* get Date */
    if ((err = grib_get_long(handle,"dataDate",&date)))
    {
        fprintf(stderr,"Cannot get date %s\n",grib_get_error_message(err));
    }
    intv[0] = date;
    if ((err = int2_intin("date",intv,realv,text)))
    {
        fprintf(stderr,"Date setup INTIN failed %d\n",err);
    }

    /* get Level Type */
    if((err = grib_get_string(handle,"typeOfLevel",typeOfLevel,&typeOfLevelLen))!= GRIB_SUCCESS)
    {
        fprintf(stderr,"Cannot get Level Type %s\n",grib_get_error_message(err));
        return err;
    }

    levelType=levTypeInt(typeOfLevel);
    intv[0] = levelType;
    if ((err = int2_intin("levtype",intv,realv,text)))
    {
        fprintf(stderr,"Level Type setup INTIN failed %d\n",err);
    }
    /* printf("Level Type setup INTIN  %d\n",levelType); */
    /* get Level */
    if(levelType != 255){
        if((err = grib_get_long(handle,"level",&level))!= GRIB_SUCCESS)
        {
            fprintf(stderr,"Cannot get Level %s\n",grib_get_error_message(err));
            return err;
        }

        intv[0] = level;
        if ((err = int2_intin("level",intv,realv,text)))
        {
            fprintf(stderr,"Level setup INTIN failed %d\n",err);
        }
    }
    /* } */

    /* LSM */
    if( levelType == 100 || levelType == 108 || levelType == 109 || levelType == 255){
        text = "no";
    }
    else
        text = "yes";
    if ((err = int2_intin("lsmset",intv,realv,text)))
    {
        fprintf(stderr,"LSM setup INTIN failed %d\n",err);
    }

    /* get Parameter Number */
    /* if(err = grib_get_long(handle,"param",&parameter)) */
    if ((err = grib_get_long(handle,"paramId",&parameter)))
    {
        if(getenv("INTF2_IGNORE_PARAM") != 0)
        {
            parameter = 1;
            table = 1;
        }
        else
            fprintf(stderr,"Cannot Parameter %s\n",grib_get_error_message(err));
    }
    if(intf2_debug)
    {
        printf("DESCRIBE_INPUT_FIELD: Parameter - %ld\n",parameter);
    }

    if(parameter > 1000)
    {
        table     = parameter / 1000;
        parameter = parameter % 1000;
    }
    else
        table = 128;

    intv[0] = table;
    if ((err = int2_intin("table",intv,realv,text)))
    {
        fprintf(stderr,"Table Number setup INTIN failed %d\n",err);
    }

    intv[0] = parameter;
    if ((err = int2_intin("parameter",intv,realv,text)))
    {
        fprintf(stderr,"Parameter Number setup INTIN failed %d\n",err);
    }

    /* If missing values present  */
    if ((err = grib_get_long(handle,"missingValuesPresent",&missingValuesPresent))!= GRIB_SUCCESS)
    {
        fprintf(stderr,"Cannot get missingValuesPresent %s\n",grib_get_error_message(err));
        return err;
    }
    /*  There are exeptions for a few centres  */
    if ((err = grib_get_long(handle,"centre",&centre)))
    {
        fprintf(stderr,"Cannot get centre %s\n",grib_get_error_message(err));
    }
    if(levelType == 100 && parameter == 129)
        missingValuesPresent = 0;

    /* if(table == 128 && (parameter == 31 || parameter == 130)){ */
    /* Live emoslib to use its own values
     * Can Not set allways external missing value in case bitmap is Present because of some inconsistensy in LIBEMOS
     */
    if(missingValuesPresent) {
        /* Try to play in a same style as Libemos
            missingValue = -2147483647.0;
            err = grib_set_double(handle,"missingValue",missingValue);
            */

        err = grib_get_double(handle,"missingValue",&missingValue);

        if (err != GRIB_SUCCESS && err != GRIB_NOT_FOUND) {
            fprintf(stderr,"Missing value %s\n",grib_get_error_message(err));
        }
        if(intf2_debug)
            printf("DESCRIBE_INPUT_FIELD: missingValuesPresent Missing Value %f \n",missingValue);
        if(err == GRIB_SUCCESS) {
            realv[0] = missingValue;
            text = "yes";
            if ((err = int2_intin("missingval",intv,realv,text))) {
                fprintf(stderr,"Missing Value setup INTIN failed %d\n",err);
            }
        }
    }


    /* Default Accuracy */
    if((err = grib_get_long(handle,"bitsPerValue",&accuracy))!= GRIB_SUCCESS)
    {
        fprintf(stderr,"Cannot get accuracy %s\n",grib_get_error_message(err));
        return err;
    }
    intv[0] = accuracy;
    if ((err = int2_intin("accuracy",intv,realv,text)))
    {
        fprintf(stderr,"Accuracy setup INTIN failed %d\n",err);
    }
    if(intf2_debug)
    {
        printf("DESCRIBE_INPUT_FIELD: Input Accuracy  %ld\n",accuracy);
    }


    /* Particular Input fields */

    /* Spherical harmonics */
    if(strcmp(grid_type,"sh") == 0)
    {
        long truncation;
        /* get Truncation */
        if((err = grib_get_long(handle,"pentagonalResolutionParameterJ",&truncation))!= GRIB_SUCCESS)
        {
            fprintf(stderr,"Cannot get Truncation %s\n",grib_get_error_message(err));
            return err;
        }
        intv[0] = truncation;
        if ((err = int2_intin("truncation",intv,realv,text)))
        {
            fprintf(stderr,"Truncation  setup INTIN failed %d\n",err);
        }
        if(outputRepresentation)
        {
            if ((err = int2_intout("truncation",intv,realv,text)))
            {
                fprintf(stderr,"Truncation  setup INTOUT failed %d\n",err);
            }
        }
    }


    /* Gaussian grid */
    if (!strcmp(grid_type,"regular_gg") || !strcmp(grid_type,"reduced_gg")) {

        /* Gaussian grid number */
        long N = 0;
        if ((err=grib_get_long(handle,"numberOfParallelsBetweenAPoleAndTheEquator",&N))!=GRIB_SUCCESS) {
            fprintf(stderr,"regular_gg/reduced_gg setup: cannot get Gaussian grid number %s\n",grib_get_error_message(err));
            return err;
        }

        /* Gaussian grid type (regular, reduced_gg/octahedral, reduced_gg/classical, ...) */
        long isOctahedral = 0;
        long isRegularGG  = (!strcmp(grid_type,"regular_gg"));
        if (!isRegularGG) {
            if ((err=grib_get_long(handle,"isOctahedral",&isOctahedral))!=GRIB_SUCCESS) {
                fprintf(stderr,"regular_gg/reduced_gg setup: cannot get isOctahedral %s\n",grib_get_error_message(err));
                return err;
            }
        }

        /* pl array (static) */
        size_t pl_len = 0;
        long   pl[4000] = {0,};

        /* Gaussian grid name (gridName), comparing to internal pl arrays */
        char gridName[6] = {0,};
        if (isRegularGG) {
            snprintf(gridName,6,"F%li",N);
        }
        else if (isOctahedral) {
            snprintf(gridName,6,"O%li",N);
        }
        else {
            while (1) {

                fortint kret = 0;
                fortint knum = (fortint) N;
                char    htype = 'R';
                int     kpts[4000] = {0,};
                jnumgg_(&knum,&htype,kpts,&kret);
                if (kret!=0)
                    break;

                pl_len = 0;
                if ((err=grib_get_size(handle,"pl",&pl_len))!=GRIB_SUCCESS)
                    break;
                if (pl_len<=0 || pl_len!=2*N)
                    break;

#if 0
                // commented because staticly allocated pl might behave better
                long *pl = (long*)malloc(pl_len * sizeof(long));
                assert(pl);
                if ((err=grib_get_long_array(handle,"pl",pl,&pl_len))!=GRIB_SUCCESS)
                    break;
                for (i=0; i<pl_len && kret==0; ++i)
                    kret = (kpts[i]!=pl[i]);
                free(pl);
#else
                if ((err=grib_get_long_array(handle,"pl",pl,&pl_len))!=GRIB_SUCCESS)
                    break;
                for (i=0; i<pl_len && kret==0; ++i)
                    kret = (kpts[i]!=pl[i]);
#endif

                if (kret==0)
                    snprintf(gridName,6,"N%li",N);

                break;
            }
        }

        if (strlen(gridName)) {

            /* setup using gridname */
            if ((err=int2_intin("gridname",intv,realv,&gridName[0]))) {
                fprintf(stderr,"regular_gg/reduced_gg setup: INTIN(\"gridname\") failed %d\n",err);
                return err;
            }
            if (outputRepresentation && (err=int2_intout("gridname",intv,realv,&gridName[0])) ) {
                fprintf(stderr,"regular_gg/reduced_gg setup: INTOUT(\"gridname\") failed %d\n",err);
                return err;
            }

        }
        else {

            /* setup by manually specifying pl array */
            intv[0] = (fortint) N;
            if ((err=int2_intin("reduced",intv,realv,text))) {
                fprintf(stderr,"reduced_gg setup: INTIN(\"reduced\") failed %d\n",err);
                return err;
            }
            if (outputRepresentation && (err=int2_intout("reduced",intv,realv,text))) {
                fprintf(stderr,"reduced_gg setup: INTOUT(\"reduced\") failed %d\n",err);
                return err;
            }
            pl_len = 0;
            if ((err=grib_get_size(handle,"pl",&pl_len))!=GRIB_SUCCESS) {
                fprintf(stderr,"reduced_gg setup: cannot get size of pl %s\n",grib_get_error_message(err));
                return err;
            }
            if ((err=grib_get_long_array(handle,"pl",pl,&pl_len))!=GRIB_SUCCESS) {
                fprintf(stderr,"reduced_gg setup: cannot get pl %s\n",grib_get_error_message(err));
                return err;
            }

            if (intf2_debug)
                printf("DESCRIBE_INPUT_FIELD: Reduced Gaussian north-south number of points %lu\n",pl_len);
            for (i=0; i<pl_len; ++i)
                intv[i] = pl[i];

            if ((err=int2_intin("ga_pnts",intv,realv,text))) {
                fprintf(stderr,"reduced_gg setup: INTIN(\"ga_pnts\") failed %d\n",err);
                return err;
            }
        }

    }


    /* Shall missing value to be used */
    /* missingval because can not be applied in Emos lib everywhere... */

    /* Pick up area if the field is lat-lon or gaussian, scanning mode and number of points ns we */
    if(strcmp(grid_type,"regular_ll") == 0 || strcmp(grid_type,"regular_gg") == 0  || strcmp(grid_type,"reduced_gg") == 0 ||  strcmp(grid_type,"reduced_ll") == 0)
    {
        double latitudeOfFirstGridPoint, longitudeOfFirstGridPoint, latitudeOfLastGridPoint, longitudeOfLastGridPoint;
        if((err = grib_get_double(handle,"latitudeOfFirstGridPointInDegrees",&latitudeOfFirstGridPoint))!= GRIB_SUCCESS)
        {
            fprintf(stderr,"Cannot get latitudeOfFirstGridPointInDegrees  %s\n",grib_get_error_message(err));
            return err;
        }
        if((err = grib_get_double(handle,"longitudeOfFirstGridPointInDegrees",&longitudeOfFirstGridPoint))!= GRIB_SUCCESS)
        {
            fprintf(stderr,"Cannot get longitudeOfFirstGridPointInDegrees  %s\n",grib_get_error_message(err));
            return err;
        }
        if((err = grib_get_double(handle,"latitudeOfLastGridPointInDegrees",&latitudeOfLastGridPoint))!= GRIB_SUCCESS)
        {
            fprintf(stderr,"Cannot get latitudeOfLastGridPointInDegrees  %s\n",grib_get_error_message(err));
            return err;
        }
        if((err = grib_get_double(handle,"longitudeOfLastGridPointInDegrees",&longitudeOfLastGridPoint))!= GRIB_SUCCESS)
        {
            fprintf(stderr,"Cannot get longitudeOfLastGridPointInDegrees  %s\n",grib_get_error_message(err));
            return err;
        }

        realv[0] = latitudeOfFirstGridPoint;
        realv[1] = longitudeOfFirstGridPoint;
        realv[2] = latitudeOfLastGridPoint;
        realv[3] = longitudeOfLastGridPoint;
        if(intf2_debug)
        {
            printf("DESCRIBE_INPUT_FIELD: Input AREA    %f  %f  %f  %f  \n",latitudeOfFirstGridPoint,longitudeOfFirstGridPoint,latitudeOfLastGridPoint,longitudeOfLastGridPoint);
        }

        if ((err = int2_intin("area",intv,realv,text)))
        {
            fprintf(stderr,"Area setup INTIN failed %d\n",err);
        }
        /* Scanning mode */
        if((err = grib_get_long(handle,"jScansPositively",&scanningMode))!= GRIB_SUCCESS)
        {
            fprintf(stderr,"Cannot get scanning mode %s\n",grib_get_error_message(err));
            return err;
        }
        if(scanningMode)
            scanningMode = 64;
        if(intf2_debug)
        {
            printf("DESCRIBE_INPUT_FIELD: Input Scanning Mode -  %ld\n",scanningMode);
        }
        intv[0] = scanningMode;
        if ((err = int2_intin("scan",intv,realv,text)))
        {
            fprintf(stderr,"Scanning mode setup INTIN failed %d\n",err);
        }
        /* number Of Points Along A Parallel */
        if((err = grib_get_long(handle,"numberOfPointsAlongAParallel",&niwe))!= GRIB_SUCCESS)
        {
            fprintf(stderr,"Cannot get numberOfPointsAlongAParallel %s\n",grib_get_error_message(err));
            return err;
        }
        intv[0] = niwe;
        /* number Of Points Along A Meridian */
        if((err = grib_get_long(handle,"numberOfPointsAlongAMeridian",&nins))!= GRIB_SUCCESS)
        {
            fprintf(stderr,"Cannot get numberOfPointsAlongAMeridian %s\n",grib_get_error_message(err));
            return err;
        }
        intv[1] = nins;
        if ((err = int2_intin("npts",intv,realv,text)))
        {
            fprintf(stderr,"Number of points INTIN failed %d\n",err);
        }

    }

    /* Lat-Lon grid */
    if(strcmp(grid_type,"regular_ll") == 0  || strcmp(grid_type,"reduced_ll") == 0)
    {
        double iDirectionIncrement = 0, jDirectionIncrement = 0;
        size_t rll_length = 0;

        if(intf2_debug) {
            printf("DESCRIBE_INPUT_FIELD: Input Field -  %s \n",grid_type);
        }

        /* I/J Direction Increment */
        if (((err = grib_get_double(handle,"iDirectionIncrementInDegrees",&iDirectionIncrement)) != GRIB_SUCCESS) ||
            ((err = grib_get_double(handle,"jDirectionIncrementInDegrees",&jDirectionIncrement)) != GRIB_SUCCESS)) {
            fprintf(stderr,"Cannot get i/jDirectionIncrementInDegrees %s\n",grib_get_error_message(err));
            return err;
        }

        /* if regular_ll */
        if (strcmp(grid_type,"regular_ll") == 0) {

            realv[0] = iDirectionIncrement;
            realv[1] = jDirectionIncrement;
            if ((err = int2_intin("grid",intv,realv,text))) {
                fprintf(stderr,"regular_ll setup: INTIN(\"grid\") failed %d\n",err);
            }
            /* Set output representation if it is not set */
            if (outputRepresentation) {
                if ((err = int2_intout("grid",intv,realv,text))) {
                    fprintf(stderr,"regular_ll setup: INTOUT(\"grid\") failed %d\n",err);
                }
            }

        }

        /* if reduced_ll */
        else if (strcmp(grid_type,"reduced_ll") == 0) {

            realv[0] = 0.;
            realv[1] = jDirectionIncrement;
            if (outputRepresentation) {
                if ((err = int2_intout("red_latlon",intv,realv,text))) {
                    fprintf(stderr,"reduced_ll setup: INTOUT(\"red_latlon\") failed %d\n",err);
                }
            }

            if ((err = grib_get_size(handle,"pl",&rll_length))) {
                fprintf(stderr,"Cannot get size of pl %s\n",grib_get_error_message(err));
            }
            if(intf2_debug) {
                printf("DESCRIBE_INPUT_FIELD: Number of points NS from reduced_ll definition %lu\n",rll_length);
            }

            if ((err = grib_get_long_array(handle,"pl",rll_def,&rll_length))) {
                fprintf(stderr,"Cannot get pl %s\n",grib_get_error_message(err));
            }
            intv[0] = rll_length;
            if ((err = int2_intin("redu_ll",intv,realv,text))) {
                fprintf(stderr,"reduced_ll setup: INTIN(\"redu_ll\") failed %d\n",err);
            }
            for (i=0; i<rll_length; i++) {
                intv[i] = rll_def[i];
            }
            if ((err = int2_intin("l_npts",intv,realv,text))) {
                fprintf(stderr,"reduced_ll setup: INTIN(\"l_pnts\") failed %d\n",err);
            }

        }

        /* Force wave processing (certain parameters, or if reduced_ll) */
        if ( (strcmp(grid_type,"reduced_ll") == 0)
          || (table == 140)
          || (table == 131 && (parameter == 229))
          || (table == 131 && (parameter == 232)) ) {

            err = grib_get_long(handle,"matrixOfValues",&matrixOfValues);
            if (err == GRIB_SUCCESS) {
                intv[0] = matrixOfValues;
                if ((err = int2_intin("matrix",intv,realv,text))) {
                    fprintf(stderr,"regular_ll/reduced_ll setup: INTIN(\"matrix\") failed %d\n",err);
                }
            }

            err = grib_get_double(handle,"missingValue",&missingValue);
            if (err != GRIB_SUCCESS && err != GRIB_NOT_FOUND) {
                fprintf(stderr,"Missing value %s\n",grib_get_error_message(err));
            }
            if(intf2_debug)
                printf("DESCRIBE_INPUT_FIELD: Wave field Missing Value %f \n",missingValue);
            if(err == GRIB_SUCCESS) {
                realv[0] = missingValue;
                text = "yes";
                if ((err = int2_intin("missingvalue",intv,realv,text))) {
                    fprintf(stderr,"regular_ll/reduced_ll setup: INTIN(\"missingvalue\") failed %d\n",err);
                }
            }

        }

    }

    /************************************************************/
    /* Describe output unpacked field for INTF */


    /* Set INTOUT to get unpack field from INTF */
    if ((err = int2_intout("form",intv,realv,"unpacked")))
    {
        fprintf(stderr,"Form setup INTOUT failed %d\n",err);
    }


    /* Level Type */
    intv[0] = levelType;
    if ((err = int2_intout("levtype",intv,realv,text)))
    {
        fprintf(stderr,"Level Type setup INTOUT failed %d\n",err);
    }
    /*
    */
    /* if(levelType != 1){ */
    /* Level */
    intv[0] = level;
    if ((err = int2_intout("level",intv,realv,text)))
    {
        fprintf(stderr,"Level setup INTOUT failed %d\n",err);
    }
    /* } */


    /* Table */
    /* Parameter */
    intv[0] = table;
    if ((err = int2_intout("table",intv,realv,text)))
    {
        fprintf(stderr,"Table Number setup INTOUT failed %d\n",err);
    }

    intv[0] = parameter;
    if ((err = int2_intout("parameter",intv,realv,text)))
    {
        fprintf(stderr,"Parameter Number setup INTOUT failed %d\n",err);
    }

    /* Default Accuracy */
    /*
    intv[0] = accuracy;
    if(intf2_debug)
    {
        printf("DESCRIBE_INPUT_FIELD: Input Accuracy -  %d \n",accuracy);
    }
    if(err = int2_intout("accuracy",intv,realv,text))
    {
        fprintf(stderr,"Accuracy setup INTOUT failed %d\n",err);
    }
    */

    return err;

}
