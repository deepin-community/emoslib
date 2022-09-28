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


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "emos.h"

#ifdef FORTRAN_UPPERCASE
#define outrep_  OUTREP
#define gettru_  GETTRU
#define setrep_  SETREP
#define chkout_  CHKOUT
#define global_  GLOBAL
#define intf_    INTF
#define intout_  INTOUT
#define intin_   INTIN
#define gasetup_ GASETUP
#define intuvs_  INTUVS
#define intvecy_ INTVECY
#define estima_  ESTIMA
#endif

#ifdef FORTRAN_NO_UNDERSCORE
#define outrep_  outrep
#define gettru_  gettru
#define setrep_  setrep
#define chkout_  chkout
#define global_  global
#define intf_    intf
#define intout_  intout
#define intin_   intin
#define gasetup_ gasetup
#define intuvs_  intuvs
#define intvecy_ intvecy
#define estima_  estima
#endif



fortint int2_estima()
{
    return estima_();
}

fortint int2_global()
{
    return global_();
}

fortint int2_chkout()
{
    return chkout_();
}

fortint int2_setrep(fortint output_flag)
{
    return setrep_(&output_flag);
}

fortint int2_gettru()
{
    return gettru_();
}

fortint int2_outrep()
{
    return outrep_();
}

fortint int2_intf( fortfloat in_array[], fortint in_array_length, fortfloat *out_array, fortint *out_array_length)
{
    char  in_grib[1];
    char  out_grib[500000];

    return intf_(in_grib, &in_array_length, in_array, out_grib, out_array_length, out_array);
}

fortint int2_intout(const char* param,fortint iv[],fortfloat dv[],const char* cv)
{
    char *intf2_debug = getenv("INTF2_DEBUG");

    if(!cv) cv = "";
    if(intf2_debug)
    {
        printf("int2_intout: %s\n",param);
    }
    return intout_(param,iv,dv,cv,strlen(param),strlen(cv));
}

fortint int2_intin(const char* param,fortint iv[],fortfloat dv[],const char* cv)
{
    if(!cv) cv = "";
    return intin_(param,iv,dv,cv,strlen(param),strlen(cv));
}

fortint int2_gasetup( fortint isec1[], fortint isec2[], fortint isec3[], fortint isec4[], fortfloat rsec2[], fortfloat zsec3[])
{
    return gasetup_( isec1, isec2, isec3, isec4, rsec2, zsec3 );
}

fortint int2_intuvy(fortfloat vort_in[],fortfloat div_in[] , fortint in_array_length, fortfloat *vort_out, fortfloat *div_out, fortint *out_array_length)
{
    return intuvy_( vort_in, div_in , &in_array_length, vort_out, div_out, out_array_length);
}

fortint int2_intuvu(fortfloat vort_in[],fortfloat div_in[] , fortint in_array_length, fortfloat *vort_out, fortfloat *div_out, fortint *out_array_length)
{
    return intuvu_( vort_in, div_in , &in_array_length, vort_out, div_out, out_array_length);
}

fortint int2_intvecy(fortfloat u_in[],fortfloat v_in[] , fortint in_array_length, fortfloat *u_out, fortfloat *v_out, fortint *out_array_length)
{
    return intvecy_( u_in, v_in , &in_array_length, u_out, v_out, out_array_length);
}

