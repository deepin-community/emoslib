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
#include <stdlib.h>
#include "common/fortint.h"
#include "common/JPointer.h"

#if (defined hpR64) || (defined hpiaR64) || (defined CRAY) || (defined rs6000)
#define XALLOC malloc
#else
#define XALLOC malloc
#endif

#if (defined hpR64) || (defined hpiaR64)
JPointer jmalloc_( fortint * n)
{
int number = *n;
long * alloc;
JPointer falloc;

    alloc = (long *) XALLOC(number);
    falloc = (JPointer) alloc;
    return falloc;
}

JPointer jmalloc( fortint * n) {
  return jmalloc_(n);
}
#elif (defined __alpha)
JPointer jmalloc_( int * n)
{
static JPointer ret;
    ret = (JPointer) XALLOC(*n);
    return ret;
}

JPointer jmalloc( int * n) {
  return jmalloc_(n);
}
#elif (defined POINTER_64)
JPointer jmalloc_( JPointer n)
{
/*size_t number_of_bytes = (size_t) (*n);*/

/*printf("======= jmalloc_ number of bytes %ld \n", *n);*/
   return  XALLOC(*n);
}

JPointer jmalloc( int * n) {
  return jmalloc_(n);
}


#else
int * jmalloc_( int * n)
{
    return (int *) XALLOC(*n);
}

int * jmalloc( int * n) {
  return jmalloc_(n);
}
#endif

size_t* jmalloc2_( size_t * n)
{
/*   printf("======= jmalloc2_ number of bytes %ld \n", *n);*/

   return  (size_t *)XALLOC(*n);
}

size_t* jmalloc2( size_t * n)
{
  return (size_t *)jmalloc2_(n);
}
