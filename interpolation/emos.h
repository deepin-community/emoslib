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
#include "common/fortint.h"
#include "common/fortreal.h"
#define     ISECTION_0  2
#define     ISECTION_1  1024  /* WARN: (in parim.h this is different??) */  /* beware of  for ocean data */
#define     ISECTION_2  5000
#define     ISECTION_3  3     /* WARN: (in parim.h this is different??) */
#define     ISECTION_4  512   /* WARN: (in parim.h this is different??) */

#define     RSECTION_2  512   /* WARN: (in parim.h this is different??) */
#define     RSECTION_3  2
#define     RSECTION_4  1


fortint int2_global();
fortint outrep_();
fortint estima_();
fortint gettru_();
fortint setrep_(fortint*);
fortint chkout_();
fortint global_();
fortint intf_(const char*, fortint*, fortfloat[], const char*, fortint*, fortfloat[]);
fortint intout_(const char*, fortint[], fortfloat[], const char*, fortint, fortint);
fortint intin_(const char*, fortint[], fortfloat[], const char*, fortint, fortint);
fortint gasetup_( fortint isec1[], fortint isec2[], fortint isec3[], fortint isec4[], fortfloat rsec2[], fortfloat zsec3[]);
fortint intuvs_(fortfloat[], fortfloat[], fortint*, fortfloat[], fortfloat[], fortint*);
fortint intuvy_(fortfloat[], fortfloat[], fortint*, fortfloat[], fortfloat[], fortint*);
fortint intuvu_(fortfloat[], fortfloat[], fortint*, fortfloat[], fortfloat[], fortint*);
fortint intvecy_(fortfloat[], fortfloat[], fortint*, fortfloat[], fortfloat[], fortint*);
fortint int2_intvecy(fortfloat[],fortfloat[] , fortint, fortfloat*, fortfloat*, fortint*);


