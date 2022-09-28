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
#ifndef fortran_H
#define fortran_H

#include "common/fortint.h"


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
fortint intuvy_(fortfloat[], fortfloat[], fortint*, fortfloat[], fortfloat[], fortint*);
fortint intuvu_(fortfloat[], fortfloat[], fortint*, fortfloat[], fortfloat[], fortint*);
fortint intvecy_(fortfloat[], fortfloat[], fortint*, fortfloat[], fortfloat[], fortint*);

fortint csect4_(fortfloat*,fortint*,fortint*,fortint*,fortint*,fortint*, fortint*,fortint*,fortint*);
fortint iainit_(fortint*, fortint*);
fortint igglat_(fortint*,fortfloat*,fortint*,fortint*);
fortint ref2grb_(fortfloat*,fortint*,fortint*,fortint*);
void clear_c_();
void gbyte_(void*,void*,fortint*,fortint*);
void grsmkp_(fortint*);
void inscal_(fortfloat*,fortint*,fortint*,fortfloat*,fortfloat*,fortint*);
void inxbit_(fortint*,fortint*,fortint*,fortint*,fortint*,fortint*,fortint*, char*,fortint*,long);
void jgetgg_(fortint*,unsigned char*,fortfloat*,fortint*,fortint*,long);
void jmakgg_(fortint*, fortint*, fortint*, fortfloat*, fortfloat*, fortint*);
void jmakll_(fortint*, fortint*, fortfloat*, fortfloat*, fortfloat*, fortint*);

#endif
