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

#ifdef ENABLE_FLOATING_POINT_EXCEPTIONS
#define _GNU_SOURCE
#include <fenv.h>
int feenableexcept(int excepts);
#endif

void except_()
{
#ifdef ENABLE_FLOATING_POINT_EXCEPTIONS
    feenableexcept(FE_ALL_EXCEPT & ~FE_INEXACT & ~FE_UNDERFLOW);
#endif
}

