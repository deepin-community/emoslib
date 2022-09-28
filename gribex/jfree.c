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

#include <stdlib.h>

void jfree( int ** n)
{
    /*printf(" *** JFREE called"); */
    /*printf(" - pointer %p\n",n); */
    if (*n) free(*n);
    /*
    else printf(" **** JFREE - try to deallocate NULL pointer!!!\n");
    printf(" *** JFREE - pointer %p freed\n",n);
    */
}

void jfree_( int ** n)
{
    /* printf(" *** JFREE called via jfree_\n"); */
    jfree(n);
}
