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
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "common/fortint.h"

/*
// FORTRAN callable chmod:
//
//   INTEGER FUNCTION JCHMOD (NAME, MODE)
//   CHARACTER*(*) NAME, MODE
//
//   The normal returned value is 0.  Any other value will be a system error
//   number.
//
*/

fortint jchmod_(char * filename, char * char_mode, long l1, long l2)
{
mode_t mode = 0;
int i, status = 0;
char * name;

    if( (*char_mode) == '0' )
    {
      for( i = 0; i < l2; i++)
        mode = (mode*8) + (*(char_mode+i)-'0');
    }
    else
    {
      for( i = 0; i < l2; i++)
        mode = (mode*10) + (*(char_mode+i)-'0');
    }

    name = (char*) malloc(l1+1);
    if( name == NULL ) {
      perror("JCHMOD: malloc error (name)");
      exit(1);
    }

    for( i = 0; i < l1; i++)
      name[i] = filename[i];
    name[l1] = '\0';

    status = chmod(name, mode);
    if( status ) {
      perror("JCHMOD error");
      exit(1);
    }

    free(name);

    return (fortint) status;
}

fortint jchmod(char * filename, char * char_mode, long l1, long l2) {
  return jchmod_(filename,char_mode,l1,l2);
}

