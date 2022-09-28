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


fortint int2_gettru();
fortint int2_intin (const char* param, fortint iv[], fortfloat dv[], const char* cv);
fortint int2_intout(const char* param, fortint iv[], fortfloat dv[], const char* cv);
fortint int2_intuvy(fortfloat vort_in[],fortfloat div_in[] , fortint in_array_length, fortfloat *vort_out, fortfloat *div_out, fortint *out_array_length);
fortint int2_outrep();
fortint int2_setrep(fortint output_flag);


fortint intuvs2_(char* vort_grib_in, char* div_grib_in, fortint* length_in, char* vort_grib_out, char* div_grib_out, fortint* length_out)
{

    static double *vort_values_in = NULL;
    static double *div_values_in  = NULL;
    static double *vort_values_out = NULL;
    static double *div_values_out  = NULL;
    static size_t values_in_len = 0;
    static size_t values_out_len = 0;

    static float *vort_values_in1 = NULL;
    static float *div_values_in1  = NULL;
    static float *vort_values_out1 = NULL;
    static float *div_values_out1  = NULL;
    static size_t values_in_len1 = 0;
    static size_t values_out_len1 = 0;

    long ii = 0;
    long long_inlen  = *length_in;
    size_t inlen;
    size_t outlen  = *length_out;
    fortint out_length  = 0;

    fortint fortint_inlen  = *length_in;
    fortint fortint_outlen = outlen;

    int jpeg = 0, resetOutput = 0;;

    char *text = "";
    char *complex_switch = getenv("COMPLIANT_UV_SPECTRAL_COMPLEX");
    fortint   intv[4];
    fortfloat realv[4];
    long truncation, inputTrunc;
    grib_handle *handle1, *handle2;
    int err = 0;
    long edition = 1;

    const void* temp1;
    const void* temp2;
    char *intf2_debug = getenv("INTF2_DEBUG");

    char packing_type[]="spectral_complex";
    size_t  size_pack=sizeof(packing_type);

    fortint outputRepresentation = int2_outrep();

    if(outputRepresentation)
        if(intf2_debug) {
            printf("INTUVS2: Output Representation is Not set by user\n");
        }

    handle1 = grib_handle_new_from_message_copy(0,vort_grib_in,long_inlen);
    if(!handle1) {
        err = -1;
        goto cleanup;
    }

    handle2 = grib_handle_new_from_message_copy(0,div_grib_in,long_inlen);
    if(!handle2) {
        err = -1;
        goto cleanup;
    }

    if((err =grib_get_long(handle1,"pentagonalResolutionParameterJ",&inputTrunc)))
    {
        fprintf(stderr,"INTUVS2: Cannot get Truncation %s\n",grib_get_error_message(err));
        goto cleanup;
    }
    /*----------------------------------*/
    if ((err = grib_get_size(handle1,"values",&inlen)))
    {
        fprintf(stderr,"INTUVS2: Cannot get size for vorticity %s\n",grib_get_error_message(err));
        goto cleanup;
    }

    if(intf2_debug) {
        printf("INTUVS2: inlen for vorticity: %lu\n",inlen);
    }

    if(inlen > values_in_len)
    {
        if(vort_values_in) free(vort_values_in);
        vort_values_in = (double*)malloc(sizeof(double)*inlen);
        values_in_len = inlen;

        if(!vort_values_in)
        {
            err = -1;
            fprintf(stdout,"INTUVS2: Cannot allocate vort_values_in %ld\n",inlen);
            goto cleanup;
        }

        if(div_values_in) free(div_values_in);
        div_values_in = (double*)malloc(sizeof(double)*inlen);
        if(!div_values_in)
        {
            err = -1;
            fprintf(stderr,"INTUVS2: Cannot allocate input array for divergency %lu\n",inlen);
            goto cleanup;
        }
    }

    /*
    if( err = grib_get_size(handle2,"values",&inlen))
    {
        fprintf(stderr,"INTUVS2: Cannot get size for divergency %s\n",grib_get_error_message(err));
        goto cleanup;
    }
*/
    /* get edition */
    if ((err = grib_get_long(handle1,"edition", &edition)))
    {
        fprintf(stderr,"INTUVS2: Cannot get Edition %s\n",grib_get_error_message(err));
        goto cleanup;
    }

    if(intf2_debug) {
        printf("INTUVS2: inlen for divergency: %lu\n",inlen);
    }

    /*----------------------------------*/

    /* Get decoded values vorticity */
    if ((err = grib_get_double_array(handle1,"values",vort_values_in,&inlen)))
    {
        fprintf(stderr,"INTUVS2: Cannot get decoded values %s\n",grib_get_error_message(err));
        goto cleanup;
    }

    /* Get decoded values divergency */
    if ((err = grib_get_double_array(handle2,"values",div_values_in,&inlen)))
    {
        fprintf(stderr,"INTUVS2: Cannot get decoded values %s\n",grib_get_error_message(err));
        goto cleanup;
    }
    /*----------------------------------*/

    /* get Truncation */
    if ((err = grib_get_long(handle1,"pentagonalResolutionParameterJ", &truncation)))
    {
        fprintf(stderr,"INTUVS2: Cannot get Truncation %s\n",grib_get_error_message(err));
        goto cleanup;
    }
    intv[0] = truncation;
    if ((err = int2_intin("truncation",intv,realv,text)))
    {
        fprintf(stderr,"INTUVS2: Truncation  setup INTIN failed %d\n",err);
        goto cleanup;
    }
    if(outputRepresentation)
    {
        if ((err = int2_intout("truncation",intv,realv,text)))
        {
            fprintf(stderr,"INTUVS2: Truncation  setup INTOUT failed %d\n",err);
            goto cleanup;
        }
    }

    /*=============  INTUVY ====================================*/
    out_length = inlen;

    if(out_length > values_out_len)
    {
        if(vort_values_out) free(vort_values_out);
        vort_values_out = (double*)malloc(sizeof(double)*out_length);
        values_out_len = out_length;

        if(!vort_values_out)
        {
            fprintf(stdout,"INTUVS2: Cannot allocate vort_values_out %d\n",out_length);
            err = -1;
            goto cleanup;
        }
        if(div_values_out) free(div_values_out);
        div_values_out = (double*)malloc(sizeof(double)*out_length);

        if(!div_values_out)
        {
            fprintf(stdout,"INTUVS2: Cannot allocate div_values_out %d\n",out_length);
            err = -1;
            goto cleanup;
        }
    }

    fortint_inlen = inlen;
    fortint_outlen = outlen;

#ifdef REAL_8
    if ((err = int2_intuvy(vort_values_in, div_values_in, fortint_inlen, vort_values_out, div_values_out, &fortint_outlen)))
    {
        fprintf(stderr,"INTUVS failed %d\n",err);
        goto cleanup;
    }
#else
    if(inlen > values_in_len1)
    {
        if(vort_values_in1) free(vort_values_in1);
        vort_values_in1 = (float*)malloc(sizeof(float)*inlen);
        values_in_len1 = inlen;

        if(!vort_values_in1)
        {
            err = -1;
            fprintf(stdout,"INTUVS2: Cannot allocate vort_values_in %lu\n",inlen);
            goto cleanup;
        }

        if(div_values_in1) free(div_values_in1);
        div_values_in1 = (float*)malloc(sizeof(float)*inlen);
        if(!div_values_in1)
        {
            err = -1;
            fprintf(stderr,"INTUVS2: Cannot allocate input array for divergency %lu\n",inlen);
            goto cleanup;
        }
    }
    if(out_length > values_out_len1)
    {
        if(vort_values_out1) free(vort_values_out1);
        vort_values_out1 = (float*)malloc(sizeof(float)*out_length);
        values_out_len1 = out_length;

        if(!vort_values_out1)
        {
            fprintf(stdout,"INTUVS2: Cannot allocate vort_values_out %lu\n",out_length);
            err = -1;
            goto cleanup;
        }
        if(div_values_out1) free(div_values_out1);
        div_values_out1 = (float*)malloc(sizeof(float)*out_length);

        if(!div_values_out1)
        {
            fprintf(stdout,"INTUVS2: Cannot allocate div_values_out %lu\n",out_length);
            err = -1;
            goto cleanup;
        }
    }
    for( ii = 0 ; ii < fortint_inlen; ii++){
        vort_values_in1[ii] = vort_values_in[ii];
    }
    for( ii = 0 ; ii < fortint_inlen; ii++){
        div_values_in1[ii] = div_values_in[ii];
    }
    if ((err = int2_intuvu(vort_values_in1, div_values_in1, fortint_inlen, vort_values_out1, div_values_out1, &fortint_outlen)))
    {
        fprintf(stderr,"INTUVS2 failed %d\n",err);
        goto cleanup;
    }
    for( ii = 0 ; ii < fortint_outlen; ii++){
        vort_values_in[ii] = vort_values_in1[ii];
    }
    for( ii = 0 ; ii < fortint_outlen; ii++){
        div_values_in[ii] = div_values_in1[ii];
    }

#endif

    *length_out = fortint_outlen;
    outlen     = fortint_outlen;

    if(intf2_debug)
    {
        int i;
        printf("INTUVS2: outlen: %lu\n",outlen);
        for(i=0; i<10 ; i++)
        {
            printf("INTUVS2: output data values U -  %d -  %f \n",i,vort_values_out[i]);
            printf("INTUVS2: output data values V -  %d -  %f \n",i,div_values_out[i]);
        }
    }

    if(outlen)
    {
        int trunc = 0;

        /* U velocity*/
        if ((err =grib_set_string(handle1,"packingType",packing_type,&size_pack)))
        {
            fprintf(stderr,"INTUVS2: Cannot Set packing  %s\n",grib_get_error_message(err));
            goto cleanup;
        }
        if(!outputRepresentation)
        {
            trunc = int2_gettru();
            if(trunc > inputTrunc){
                printf("INTUVS2: Automatic resolution too high: %d \n Resolution reset to input resolution: %ld\n",trunc,inputTrunc);
                trunc = inputTrunc;
            }
            else
                if(intf2_debug)
                    printf("INTUVS2:  trunc -  %d \n",trunc);

            /* Set Truncation */
            if ((err =grib_set_long(handle1,"pentagonalResolutionParameterJ",trunc)))
            {
                fprintf(stderr,"INTUVS2: Cannot Set pentagonalResolutionParameterJ %s\n",grib_get_error_message(err));
                goto cleanup;
            }
            if ((err =grib_set_long(handle1,"pentagonalResolutionParameterK",trunc)))
            {
                fprintf(stderr,"INTUVS2: Cannot Set pentagonalResolutionParameterK %s\n",grib_get_error_message(err));
                goto cleanup;
            }
            if ((err =grib_set_long(handle1,"pentagonalResolutionParameterM",trunc)))
            {
                fprintf(stderr,"INTUVS2: Cannot Set pentagonalResolutionParameterM %s\n",grib_get_error_message(err));
                goto cleanup;
            }
        }
        if(complex_switch){
            if ((err =grib_set_long(handle1,"JS",20)))
            {
                fprintf(stderr,"INTUVS2: Cannot Set pentagonalResolutionParameterJ %s\n",grib_get_error_message(err));
                goto cleanup;
            }
            if ((err =grib_set_long(handle1,"KS",20)))
            {
                fprintf(stderr,"INTUVS2: Cannot Set pentagonalResolutionParameterK %s\n",grib_get_error_message(err));
                goto cleanup;
            }
            if ((err =grib_set_long(handle1,"MS",20)))
            {
                fprintf(stderr,"INTUVS2: Cannot Set pentagonalResolutionParameterM %s\n",grib_get_error_message(err));
                goto cleanup;
            }
        }

        if ((err = grib_set_long(handle1,"paramId",131)))
        {
            fprintf(stderr,"INTUVS2: Cannot Set U parameter %s\n",grib_get_error_message(err));
            goto cleanup;
        }
        if ((err =grib_set_long(handle1,"computeLaplacianOperator",1)))
        {
            fprintf(stderr,"INTUVS2: Cannot Set computeLaplacianOperator %s\n",grib_get_error_message(err));
            goto cleanup;
        }
        if ((err =grib_set_long(handle1,"truncateLaplacian",1)))
        {
            fprintf(stderr,"INTUVS2: Cannot Set computeLaplacianOperator %s\n",grib_get_error_message(err));
            goto cleanup;
        }

        if(edition == 1){
            if ((err =grib_set_long(handle1,"representationMode",2)))
            {
                fprintf(stderr,"INTUVS2: Cannot Set representationMode %s\n",grib_get_error_message(err));
                goto cleanup;
            }
        }

        if ((err = grib_set_double_array(handle1,"values",vort_values_out,outlen)))
        {
            fprintf(stderr,"INTUVS2: Error seting the double array vorticity : %s\n",grib_get_error_message(err));
            goto cleanup;
        }
        grib_get_message(handle1,&temp1,&outlen);
        if(temp1) {
            if(intf2_debug) {
                printf("INTUVS2: outlen vorticity -> %lu\n", outlen);
            }
            memcpy(vort_grib_out,temp1,outlen);
        }
        else
            fprintf(stderr,"INTUVS2: Error memcpy  divergency \n");
        *length_out = outlen;


        /* V velocity*/
        if ((err =grib_set_string(handle2,"packingType",packing_type,&size_pack)))
        {
            fprintf(stderr,"INTUVS2: Cannot Set packing  %s\n",grib_get_error_message(err));
            goto cleanup;
        }
        if(!outputRepresentation)
        {
            if ((err =grib_set_long(handle2,"pentagonalResolutionParameterJ",trunc)))
            {
                fprintf(stderr,"INTUVS2: Cannot Set pentagonalResolutionParameterJ %s\n",grib_get_error_message(err));
                goto cleanup;
            }
            if ((err =grib_set_long(handle2,"pentagonalResolutionParameterK",trunc)))
            {
                fprintf(stderr,"INTUVS2: Cannot Set pentagonalResolutionParameterK %s\n",grib_get_error_message(err));
                goto cleanup;
            }
            if ((err =grib_set_long(handle2,"pentagonalResolutionParameterM",trunc)))
            {
                fprintf(stderr,"INTUVS2: Cannot Set pentagonalResolutionParameterM %s\n",grib_get_error_message(err));
                goto cleanup;
            }
        }
        if(complex_switch){
            if ((err =grib_set_long(handle2,"JS",20)))
            {
                fprintf(stderr,"INTUVS2: Cannot Set pentagonalResolutionParameterJ %s\n",grib_get_error_message(err));
                goto cleanup;
            }
            if ((err =grib_set_long(handle2,"KS",20)))
            {
                fprintf(stderr,"INTUVS2: Cannot Set pentagonalResolutionParameterK %s\n",grib_get_error_message(err));
                goto cleanup;
            }
            if ((err =grib_set_long(handle2,"MS",20)))
            {
                fprintf(stderr,"INTUVS2: Cannot Set pentagonalResolutionParameterM %s\n",grib_get_error_message(err));
                goto cleanup;
            }
        }

        if ((err = grib_set_long(handle2,"paramId",132)))
        {
            fprintf(stderr,"INTUVS2: Cannot Set V %s\n",grib_get_error_message(err));
            goto cleanup;
        }
        if ((err =grib_set_long(handle2,"computeLaplacianOperator",1)))
        {
            fprintf(stderr,"INTUVS2: Cannot Set computeLaplacianOperator  %s\n",grib_get_error_message(err));
            goto cleanup;
        }
        if ((err =grib_set_long(handle2,"truncateLaplacian",1)))
        {
            fprintf(stderr,"INTUVS2: Cannot Set computeLaplacianOperator %s\n",grib_get_error_message(err));
            goto cleanup;
        }
        if(edition == 1){
            if ((err =grib_set_long(handle2,"representationMode",2)))
            {
                fprintf(stderr,"INTUVS2: Cannot Set representationMode %s\n",grib_get_error_message(err));
                goto cleanup;
            }
        }

        outlen  = fortint_outlen;
        if ((err = grib_set_double_array(handle2,"values",div_values_out,outlen)))
        {
            fprintf(stderr,"INTUVS2: Error seting the double array divergency : %s\n",grib_get_error_message(err));
            goto cleanup;
        }

        grib_get_message(handle2,&temp2,&outlen);
        if(temp2) {
            if(intf2_debug) {
                printf("INTUVS2: outlen divergency -> %lu\n", outlen);
            }
            memcpy(div_grib_out,temp2,*length_out);
        }
        else
            fprintf(stderr,"INTUVS2: Error memcpy  divergency \n");
        *length_out = outlen;

        goto cleanup;

    }
    else {
        fprintf(stderr,"INTUVS2: ERROR - INTUVS2  OUTLEN is: %lu\n", outlen);
        err = 1;
    }

cleanup:

    if((resetOutput = int2_setrep(outputRepresentation))) printf("INTUVS2: Output Representation reset failed:  %d \n",resetOutput);
    if(handle1)     grib_handle_delete(handle1);
    if(handle2)     grib_handle_delete(handle2);

    return err;
}

fortint intuvs2(char* vort_grib_in, char* div_grib_in, fortint* length_in, char* vort_grib_out, char* div_grib_out, fortint* length_out)
{
    return intuvs2_(vort_grib_in,div_grib_in,length_in,vort_grib_out,div_grib_out,length_out);
}
