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


fortint int2_chkout();
fortint int2_estima();
fortint int2_intf(fortfloat in_array[], fortint in_array_length, fortfloat *out_array, fortint *out_array_length);
fortint int2_outrep();
fortint int2_setrep(fortint output_flag);
int copy_spec_from_ksec(grib_util_grid_spec* spec,grib_util_packing_spec* packing_spec);
long describe_input_field(grib_handle* handle, long outputRepresentation);


fortint intf2_(char* grib_in, fortint* length_in, char* grib_out, fortint* length_out)
{
    fortint isec1[ISECTION_1] = {0,};
    fortint isec2[ISECTION_2] = {0,};
    fortint isec3[ISECTION_3] = {0,};
    fortint isec4[ISECTION_4] = {0,};

    static double *values_in = NULL;
    static double *values_out = NULL;
    static size_t values_in_len = 0;
    static size_t values_out_len = 0;
    double *values =NULL;

    static float *values_in1 = NULL;
    static float *values_out1 = NULL;
    static size_t values_in_len1 = 0;
    static size_t values_out_len1 = 0;

    size_t ii = 0;
    size_t inlen;
    size_t len=0;
    size_t outlen = *length_out;
    size_t outlen_grib_message = 0;

    fortint out_length = 0;

    long long_length_in = *length_in;
    fortint fortint_inlen = *length_in;
    fortint fortint_outlen = outlen;

    grib_handle *handle = 0;
    grib_handle *outh   = 0;
    grib_handle *finalh = 0;

    fortint err1 = 0, mars_interpolation = 0;
    int err = 0 , what = 0;
    int  resetOutput = 0;
    long nins, niwe;
    long acc = 0;
    long missingValuesPresent = 0;

    size_t size;

    const void* temp;
    char *intf2_debug = getenv("INTF2_DEBUG");
    char *intf2_write = getenv("INTF2_WRITE_TO_FILE");
    int jpeg = 0;
    int set_spec_flags=0;
    grib_util_grid_spec     spec={0,};
    grib_util_packing_spec packing_spec={0,};

    mars_interpolation = int2_chkout();

    if(!mars_interpolation)
    {
        *length_out = 0;
        if(intf2_debug){
            printf("INTF2: There is NOT users settings \n");
            printf("INTF2: No Interpolation carried out \n");
        }
        return err;
    }

    fortint outputRepresentation = int2_outrep();

    if(intf2_debug)
    {
        if(outputRepresentation)
            printf("INTF2:  Output Representation is NOT set by user\n");
    }

    handle = grib_handle_new_from_message(0,grib_in,long_length_in);
    if(handle == 0) {
        err = -1;
        fprintf(stdout,"INTF2: Cannot create Handle %d\n",err);
        /* intlog2("INTF2: Cannot create Handle"); */
        goto cleanup;
    }

    if ((err = grib_get_size(handle,"values",&inlen)))
    {
        fprintf(stdout,"INTF2: Cannot get size %s\n",grib_get_error_message(err));
        return err;
    }

    if(inlen > values_in_len)
    {
        if(values_in) free(values_in);
        values_in = (double*)malloc(sizeof(double)*inlen);
        values_in_len = inlen;

        if(!values_in)
        {
            err = -1;
            fprintf(stdout,"INTF2: Cannot allocate %ld\n",inlen);
            goto cleanup;
        }
    }

   /* If missing values present  */
    if((err = grib_get_long(handle,"missingValuesPresent",&missingValuesPresent))!= GRIB_SUCCESS)
     {
        fprintf(stderr,"Cannot get missingValuesPresent %s\n",grib_get_error_message(err));
        return err;
     }

    /* if bitmap present Set missing value which correspond WAVE in Emos  */
    if(missingValuesPresent)
    {
        /* if(err = grib_set_double(handle,"missingValue",12345.0)) */
        if ((err = grib_set_double(handle,"missingValue",-9999999.0)))
        {
            fprintf(stdout,"INTF2: Cannot set missingValue %s\n",grib_get_error_message(err));
            goto cleanup;
        }
    }

    /* Get decoded values */
    if ((err = grib_get_double_array(handle,"values",values_in,&inlen)))
    {
        fprintf(stdout,"INTF2: Cannot get decoded values %s\n",grib_get_error_message(err));
        goto cleanup;
    }


    /* Descibe input field and initial settings for output field */
    if ((err = describe_input_field(handle,outputRepresentation)))
    {
        fprintf(stdout,"INTF2 describe_input_field failed : %d\n",err);
        goto cleanup;
    }


    /*=============  INTF ====================================*/

    if(outputRepresentation){
        out_length = inlen;
    }
    else{
        out_length = int2_estima();
        if(!out_length){
            fprintf(stdout,"INTF2: Estimate for length of output array is 0 \n");
            err = -1;
            goto cleanup;
        }
    }
    if(out_length > values_out_len)
    {
        if(values_out) free(values_out);
        values_out = (double*)malloc(sizeof(double)*out_length);
        values_out_len = out_length;

        if(!values_out)
        {
            fprintf(stdout,"INTF2: Cannot allocate values_out %d\n",out_length);
            err = -1;
            goto cleanup;
        }
    }

    fortint_inlen = inlen;
    fortint_outlen = outlen;


#ifdef REAL_8
    if ((err = int2_intf(values_in,fortint_inlen,values_out,&fortint_outlen)))
    {
        fprintf(stdout,"INTF failed %d\n",err);
        goto cleanup;
    }
#else
    if(inlen > values_in_len1)
    {
        if(values_in1) free(values_in1);
        values_in1 = (float*)malloc(sizeof(float)*inlen);
        values_in_len1 = inlen;

        if(!values_in1)
        {
            err = -1;
            fprintf(stdout,"INTF2: Cannot allocate values_in1 %ld\n",inlen);
            goto cleanup;
        }
    }
    if(out_length > values_out_len1)
    {
        if(values_out1) free(values_out1);
        values_out1 = (float*)malloc(sizeof(float)*out_length);
        values_out_len1 = out_length;

        if(!values_out1)
        {
            fprintf(stdout,"INTF2: Cannot allocate values_out1 %ld\n",out_length);
            err = -1;
            goto cleanup;
        }
    }
    for( ii = 0 ; ii < fortint_inlen; ii++){
        values_in1[ii] = values_in[ii];
    }
    if ((err = int2_intf(values_in1,fortint_inlen,values_out1,&fortint_outlen)))
    {
        fprintf(stdout,"INTF failed %d\n",err);
        goto cleanup;
    }
    for( ii = 0 ; ii < fortint_outlen; ii++){
        values_out[ii] = values_out1[ii];
    }
#endif
    outlen = fortint_outlen;

    if(intf2_debug) {
        printf("INTF2: Outlen: %lu\n",outlen);
    }

    err=copy_spec_from_ksec(&spec,&packing_spec);
    if (err) {
        fprintf(stdout,"INTF2: ERROR - copy_grid_spec_from_ksec unable to copy spec: %d\n", err);
        goto cleanup;
    }

    packing_spec.accuracy=GRIB_UTIL_ACCURACY_USE_PROVIDED_BITS_PER_VALUES;
    packing_spec.packing=GRIB_UTIL_PACKING_USE_PROVIDED;

    if(outlen == 0) {
        set_spec_flags |= GRIB_UTIL_SET_SPEC_FLAGS_ONLY_PACKING;
        /* Sinisa and Enrico 7/3/11 comments this line */
        /* packing_spec.packing=0; */
        len=inlen;
        values=values_in;
        if(intf2_debug )
            printf("INTF2: Outlen = 0 No Interpolation carried out \n");
    } else {
        len=outlen;
        values=values_out;
    }
#ifdef DUMPING_GRIB_HANDLE
{
    const int dump_flags = GRIB_DUMP_FLAG_VALUES
                |  GRIB_DUMP_FLAG_READ_ONLY
                |  GRIB_DUMP_FLAG_ALIASES
                |  GRIB_DUMP_FLAG_TYPE;
    printf("intf2_ start dump...................\n");
    grib_dump_content(handle, stdout,"debug", dump_flags, NULL);
    printf("intf2_ end dump.....................\n");
}
#endif
    finalh = grib_util_set_spec(handle,&spec, &packing_spec, set_spec_flags,values, len, &err);
    if(!finalh)  {
        fprintf(stdout,"INTF2: ERROR - grib_util_set_spec: %d\n", err);
        if(!err) err = 1;
        goto cleanup;
    }

    /* deallocate pl array, if it has been passed into spec */
    if (spec.pl!=NULL) {
        free((long*) spec.pl);
        spec.pl = NULL;
    }

    if (outlen == 0){
        if(err == -1) {
            /* -1 coming from grib_util when there is not packing */
            err = 0;
            *length_out = 0;
            if(intf2_debug)
                printf("INTF2: No Interpolation carried out, nothing repacked \n");
            goto cleanup;
        }
    }

    err = grib_get_message(finalh,&temp,&outlen_grib_message);
    if(err!=0) {
        fprintf(stdout,"INTF2: ERROR - grib_get_message unable to generate message: %d\n", err);
        goto cleanup;
    }

    if(outlen_grib_message > *length_out) {
        fprintf(stdout,"INTF2: ERROR - INTF2  OUTLEN is too large: %ld > %d\n", outlen_grib_message, *length_out);
        err = 1;
        goto cleanup;
    }

    memcpy(grib_out,temp,outlen_grib_message);

    *length_out = outlen_grib_message;

    /* goto cleanup; */
    if(intf2_debug && (set_spec_flags & GRIB_UTIL_SET_SPEC_FLAGS_ONLY_PACKING)){
        printf("INTF2: No Interpolation carried out but data repacked \n");
    }


cleanup:
    if((resetOutput = int2_setrep(outputRepresentation))) printf("INTF2: Output Representation reset failed:  %d \n",resetOutput);
    if(handle)     {grib_handle_delete(handle);handle=NULL;}
    if(outh)       {grib_handle_delete(outh);outh=NULL;}
    if(outlen!=0 && finalh)     grib_handle_delete(finalh);

    return err;
}

fortint intf2(char* grib_in, fortint* length_in, char* grib_out, fortint* length_out)
{
    return intf2_(grib_in,length_in,grib_out,length_out);
}
