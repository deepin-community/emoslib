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


fortint int2_estima();
fortint int2_outrep();
int copy_spec_from_ksec(grib_util_grid_spec* spec,grib_util_packing_spec* packing_spec);
long describe_input_field(grib_handle* handle, long outputRepresentation);


fortint intvect2_(char* u_grib_in, char* v_grib_in, fortint* length_in, char* u_grib_out, char* v_grib_out, fortint* length_out)
{

    static double *u_values_in = NULL;
    static double *v_values_in = NULL;
    static double *u_values_out = NULL;
    static double *v_values_out = NULL;
    static size_t values_in_len = 0;
    static size_t values_out_len = 0;

    fortint out_length = 0;

    static float *u_values_in1 = NULL;
    static float *v_values_in1 = NULL;
    static float *u_values_out1 = NULL;
    static float *v_values_out1 = NULL;
    static size_t values_in_len1 = 0;
    static size_t values_out_len1 = 0;

    grib_handle* hu = NULL;
    grib_handle* hv = NULL;

    long long_inlen  = *length_in;
    long ii = 0;


    size_t inlen;
    size_t outlen = *length_out;
    size_t outlen1 = *length_out;

    fortint fortint_inlen  = *length_in;
    fortint fortint_outlen = outlen;

    grib_handle *handle1 = NULL, *handle2 = NULL;
    grib_handle *outh_u = NULL, *outh_v = NULL;
    int err = 0;
    int grib_err = 0;

    long accuracy = 0;

    fortint   intv[4];
    fortfloat realv[4];
    char *text = "";

    const void* temp1;
    const void* temp2;

    char *intf2_debug;
    char *intf2_write ;

    fortint outputRepresentation;
    int gribex_compatibility;
    grib_util_grid_spec spec={0,};
    grib_util_packing_spec packing_spec={0,};

    gribex_compatibility=grib_get_gribex_mode(0);

    intf2_debug = getenv("INTF2_DEBUG");
    intf2_write = getenv("INTF2_WRITE_TO_FILE");

    outputRepresentation = int2_outrep();

    if(outputRepresentation)
        if(intf2_debug) {
            printf("INTVECT2: Output Representation is Not set by user\n");
        }


    handle1 = grib_handle_new_from_message_copy(0,u_grib_in,long_inlen);
    if(!handle1) {
        err = -1;
        goto cleanup;
    }

    handle2 = grib_handle_new_from_message_copy(0,v_grib_in,long_inlen);
    if(!handle2) {
        err = -1;
        goto cleanup;
    }
    /* -------------------------------------------- */

    if ((err = grib_get_size(handle1,"values",&inlen)))
    {
        fprintf(stderr,"INTVECT2: Cannot get size for U %s\n",grib_get_error_message(err));
        return err;
    }
    if(intf2_debug) { printf("INTVECT2: inlen for U: %lu\n",inlen); }


    if ((err = grib_get_size(handle2,"values",&inlen)))
    {
        fprintf(stderr,"INTVECT2: Cannot get size for V  %s\n",grib_get_error_message(err));
        return err;
    }

    if(intf2_debug) { printf("INTVECT2: inlen for V: %lu\n",inlen); }

    if(inlen > values_in_len)
    {
        if(u_values_in) free(u_values_in);
        u_values_in = (double*)malloc(sizeof(double)*inlen);
        values_in_len = inlen;
        if(!u_values_in)
        {
            err = -1;
            fprintf(stderr,"INTVECT2: Cannot allocate  input array for U %lu\n",inlen);
            goto cleanup;
        }
        if(v_values_in) free(v_values_in);
        v_values_in = (double*)malloc(sizeof(double)*inlen);
        if(!v_values_in)
        {
            err = -1;
            fprintf(stderr,"INTVECT2: Cannot allocate input array for V %lu\n",inlen);
            goto cleanup;
        }
    }
    /* -------------------------------------------- */


    /* Default Accuracy */
    /*
    if ((err = grib_get_long(handle1,"bitsPerValue",&accuracy))!= GRIB_SUCCESS)
    {
        fprintf(stderr,"INTVECT2: Cannot get accuracy %s\n",grib_get_error_message(err));
        return err;
    }
    intv[0] = accuracy;
    if(err = int2_intin("accuracy",intv,realv,text))
    {
        fprintf(stderr,"INTVECT2: Accuracy setup INTIN failed %d\n",err);
    }
    if(intf2_debug)
    {
        printf("INTVECT2: Input Accuracy  %d  \n",accuracy);
    }

*/
    /* -------------------------------------------- */
    /* Get decoded U values */
    if ((err = grib_get_double_array(handle1,"values",u_values_in,&inlen)))
    {
        fprintf(stderr,"INTVECT2: Cannot get decoded values %s\n",grib_get_error_message(err));
        goto cleanup;
    }

    if ((err = describe_input_field(handle1,outputRepresentation)))
    {
        fprintf(stderr,"INTVECT2: describe_input_field failed : %d\n",err);
        goto cleanup;
    }

    /* Get decoded V values */
    if ((err = grib_get_double_array(handle2,"values",v_values_in,&inlen)))
    {
        fprintf(stderr,"INTVECT2: Cannot get decoded values %s\n",grib_get_error_message(err));
        goto cleanup;
    }

    if ((err = describe_input_field(handle2,outputRepresentation)))
    {
        fprintf(stderr,"INTVECT2 describe_input_field failed : %d\n",err);
        goto cleanup;
    }


    /*=============  INTVECY ====================================*/
    if(outputRepresentation){
        out_length = inlen;
    }
    else{
        out_length = int2_estima();
        if(!out_length){
            fprintf(stdout,"INTVECT2: Estimate for length of output array is 0 \n");
            err = -1;
            goto cleanup;
        }
    }

    if(out_length > values_out_len)
    {

        if(intf2_debug)
        {
            printf("INTVECT2:----------------- length for malloc %d\n",out_length);
        }
        if(u_values_out) free(u_values_out);
        u_values_out = (double*)malloc(sizeof(double)*out_length);
        values_out_len = out_length;

        if(!u_values_out)
        {
            fprintf(stderr,"INTVECT2: Cannot allocate u_values_out %d\n",out_length);
            err = -1;
            goto cleanup;
        }

        if(v_values_out) free(v_values_out);
        v_values_out = (double*)malloc(sizeof(double)*out_length);

        if(!v_values_out)
        {
            fprintf(stderr,"INTVECT2: Cannot allocate v_values_out %d\n",out_length);
            err = -1;
            goto cleanup;
        }
    }

    fortint_inlen = inlen;
    fortint_outlen = outlen;
#ifdef REAL_8
    if ((err = int2_intvecy(u_values_in,v_values_in,fortint_inlen,u_values_out,v_values_out,&fortint_outlen)))
    {
        fprintf(stderr,"INTVECT2 failed %d\n",err);
        goto cleanup;
    }
#else
    if(inlen > values_in_len1)
    {
        if(u_values_in1) free(u_values_in1);
        u_values_in1 = (float*)malloc(sizeof(float)*inlen);
        values_in_len1 = inlen;

        if(!u_values_in1)
        {
            err = -1;
            fprintf(stdout,"INTVECT2: Cannot allocate u_values_in1 %lu\n",inlen);
            goto cleanup;
        }

        if(v_values_in1) free(v_values_in1);
        v_values_in1 = (float*)malloc(sizeof(float)*inlen);
        if(!v_values_in1)
        {
            err = -1;
            fprintf(stderr,"INTVECT2: Cannot allocate input array for v_values_in1 %lu\n",inlen);
            goto cleanup;
        }
    }
    if(out_length > values_out_len1)
    {
        if(u_values_out1) free(u_values_out1);
        u_values_out1 = (float*)malloc(sizeof(float)*out_length);
        values_out_len1 = out_length;

        if(!u_values_out1)
        {
            fprintf(stdout,"INTVECT2: Cannot allocate u_values_out1 %lu\n",out_length);
            err = -1;
            goto cleanup;
        }
        if(v_values_out1) free(v_values_out1);
        v_values_out1 = (float*)malloc(sizeof(float)*out_length);

        if(!v_values_out1)
        {
            fprintf(stdout,"INTVECT2: Cannot allocate v_values_out1 %lu\n",out_length);
            err = -1;
            goto cleanup;
        }
    }

    for( ii = 0 ; ii < fortint_inlen; ii++){
        u_values_in1[ii] = u_values_in[ii];
    }
    for( ii = 0 ; ii < fortint_inlen; ii++){
        v_values_in1[ii] = v_values_in[ii];
    }

    if ((err = int2_intvecy(u_values_in1,v_values_in1,fortint_inlen,u_values_out1,v_values_out1,&fortint_outlen)))
    {
        fprintf(stderr,"INTVECT2 failed %d\n",err);
        goto cleanup;
    }
    for( ii = 0 ; ii < fortint_outlen; ii++){
        u_values_out[ii] = u_values_out1[ii];
    }
    for( ii = 0 ; ii < fortint_outlen; ii++){
        v_values_out[ii] = v_values_out1[ii];
    }
#endif
    *length_out = fortint_outlen;
    outlen = fortint_outlen;

    if(intf2_debug)
    {
        printf("INTVECT2: outlen: %lu\n",outlen);
    }

    if(outlen)
    {

        if(intf2_debug) {
            int i;
            for(i = 0; i < 10 ; i++){
                printf("INTVECT2: output data values -  %d -  %f \n",i,u_values_out[i]);
                printf("INTVECT2: output data values -  %d -  %f \n",i,v_values_out[i]);
            }
        }
        err=copy_spec_from_ksec(&spec,&packing_spec);
        if (err) {
            fprintf(stdout,"INTVECT2: ERROR - copy_grid_spec_from_ksec unable to copy spec: %d\n", err);
            goto cleanup;
        }

        packing_spec.accuracy=GRIB_UTIL_ACCURACY_USE_PROVIDED_BITS_PER_VALUES;
        packing_spec.packing=GRIB_UTIL_PACKING_USE_PROVIDED;
        /*
        if(err = set_grid_data_description(handle1,&jpeg,edition))
        {
            fprintf(stderr,"INTVECT2 set_grid_data_description failed : %d\n",err);
            goto cleanup;
        }
        if(err = grib_set_long(handle1,"uvRelativeToGrid",1))
        {
            fprintf(stderr,"INTVECT2: Cannot Set uvRelativeToGrid %s\n",grib_get_error_message(err));
            goto cleanup;
        }

        if(err = set_grid_data_description(handle2,&jpeg,edition))
        {
            fprintf(stderr,"INTVECT2 set_grid_data_description failed : %d\n",err);
            goto cleanup;
        }
        if(err = grib_set_long(handle2,"uvRelativeToGrid",1))
        {
            fprintf(stderr,"INTVECT2: Cannot Set uvRelativeToGrid %s\n",grib_get_error_message(err));
            goto cleanup;
        }
*/
        hu = grib_util_set_spec(handle1,&spec, &packing_spec, 0, u_values_out, outlen, &err);
        if(!hu)  {
            fprintf(stdout,"INTVECT2: ERROR - grib_util_set_spec: %d\n", err);
            if(!err) err = 1;
            goto cleanup;
        }
        if ((err = grib_set_long(hu,"uvRelativeToGrid",1)))
        {
            fprintf(stderr,"INTVECT2: Cannot Set uvRelativeToGrid %s\n",grib_get_error_message(err));
            goto cleanup;
        }

        hv = grib_util_set_spec(handle2,&spec, &packing_spec, 0, v_values_out, outlen, &err);
        if(!hv)  {
            fprintf(stdout,"INTVECT2: ERROR - grib_util_set_spec: %d\n", err);
            if(!err) err = 1;
            goto cleanup;
        }
        if ((err = grib_set_long(hv,"uvRelativeToGrid",1)))
        {
            fprintf(stderr,"INTVECT2: Cannot Set uvRelativeToGrid %s\n",grib_get_error_message(err));
            goto cleanup;
        }

        grib_get_message(hu,&temp1,&outlen);
        if(temp1) {
            if(intf2_debug) {
                printf("INTVECT2: outlen u-comp -> %lu\n", outlen);
            }
            memcpy(u_grib_out,temp1,outlen);
        }
        else
            fprintf(stderr,"INTUVP2: Error u-comp \n");
        outlen = *length_out;

        grib_get_message(hv,&temp2,&outlen);
        if(temp2) {
            if(intf2_debug) {
                printf("INTUVP2: outlen v-comp -> %lu\n", outlen);
            }
            memcpy(v_grib_out,temp2,outlen);
        }
        else
            fprintf(stderr,"INTUVP2: Error v-comp \n");
        *length_out = outlen;

        goto cleanup;



    }
    else {
        fprintf(stderr,"INTVECT2: ERROR - INTVECT2  OUTLEN is: %lu\n", outlen);
        err = 1;
    }

cleanup:

    if(hu && (hu != handle1))     grib_handle_delete(hu);
    if(hv && (hv != handle2))     grib_handle_delete(hv);
    if(handle1)     grib_handle_delete(handle1);
    if(handle2)     grib_handle_delete(handle2);

    return err;
}

fortint intvect2(char* u_grib_in, char* v_grib_in, fortint* length_in, char* u_grib_out, char* v_grib_out, fortint* length_out )
{
    return intvect2_(u_grib_in,v_grib_in,length_in,u_grib_out,v_grib_out,length_out);
}
