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
#include <unistd.h>

#include "common/fortint.h"

/*
// NB. Don't need the versions without underscore, they exist in
//     standard libraries (?!).
*/
#define RENAME rename_
#define UNLINK unlink_
#define GETPID getpid_

fortint GETPID(void) {
    return (fortint) getpid();
}

fortint RENAME( char * oldname, char * newname, int lold, int lnew) {
char * old, * new;
fortint status;

    old = (char *) malloc(lold+1);
    if( old == NULL ) {
      perror("RENAME: malloc failed for old");
      return (fortint) -1;
    }
    new = (char *) malloc(lnew+1);
    if( new == NULL ) {
      perror("RENAME: malloc failed for new");
      free(old);
      return (fortint) -1;
    }

    strncpy(old,oldname,lold);
    strncpy(new,newname,lnew);

    old[lold] = '\0';
    new[lold] = '\0';

    status = (fortint) rename(old,new);

    free(old);
    free(new);

    return status;
}

fortint UNLINK( char * oldname, int lold) {
char * old;
fortint status;

    old = (char *) malloc(lold+1);
    if( old == NULL ) {
      perror("UNLINK: malloc failed for old");
      return (fortint) -1;
    }

    strncpy(old,oldname,lold);
    old[lold] = '\0';

    status = (fortint) unlink(old);

    free(old);

    return status;
}

