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

#ifndef CRAY
#ifdef FORTRAN_NO_UNDERSCORE
#define LOCAL2C local2c
#define LOCAL2KC local2kc
#define EMOSNUM emosnum
#else
#define LOCAL2C local2c_
#define LOCAL2KC local2kc_
#define EMOSNUM emosnum_
#endif
#endif

#ifdef TABLE_PATH
#define USER 1
#else
#define USER 0
/* #define TABLE_PATH "/home/ma/emos/tables/gribex/nnnnnn/local_table_2_version_" */
#define TABLE_PATH "/usr/local/apps/libemos/tables/gribex/nnnnnn/local_table_2_version_"
#endif

#define JPROUTINE 20000
#define BUFFLEN 256

int LOCAL2C(int , int , char * , char * , char * , char * );
int LOCAL2KC(int * , char * , char * , char * , char * );
int EMOSNUM(int *);

int LOCAL2KC(int * ksec1,
            char * hfirst, char * hsecond, char * hthird, char * hfourth)
{
    return ( LOCAL2C( ksec1[0], ksec1[5], hfirst, hsecond, hthird, hfourth) );
}

int LOCAL2C(int ktable, int kparam,
            char * hfirst, char * hsecond, char * hthird, char * hfourth)
{
char yfile[256], * pyfile;
FILE * in;
char buffer[BUFFLEN];
int iparam;
int nnnnnn, noprint = 1;
char NNNNNN[7];

    strcpy( hfirst,  "NONE");
    strcpy( hsecond, "Undefined parameter");
    strcpy( hthird , "Undefined parameter");
    strcpy( hfourth, "Undefined parameter");

/* Build filename */

    pyfile = getenv("ECMWF_LOCAL_TABLE_PATH");

    if( pyfile == NULL ) {
        strcpy( yfile, TABLE_PATH);
       if(USER)
           strcat(yfile,"/gribtables");
    }
    else {
        strcpy( yfile, pyfile);
    }

    nnnnnn = EMOSNUM(&noprint);
    sprintf(NNNNNN,"%6.6d",nnnnnn);
    memcpy((yfile+28),NNNNNN,6);
    sprintf((yfile+strlen(yfile)),"%3.3d",ktable);

/* Open file */

    in = fopen(yfile, "r");
    if ( in == NULL )  return (JPROUTINE + 2);

/*  Loop through file */

    while(1)
    {
        fgets(buffer, BUFFLEN-1, in);
        if (feof(in) ) return (JPROUTINE + 1);

        fgets(buffer, BUFFLEN-1, in);
        if (feof(in) ) return (JPROUTINE + 1);
        sscanf(buffer, "%3d", &iparam);

/*  Match found in table */

        if( kparam == iparam )
        {
            fgets(buffer, BUFFLEN-1, in);
            if (feof(in) ) return (JPROUTINE + 1);
            buffer[strlen(buffer)-1] = '\0';
            strcpy(hfirst,buffer);
            fgets(buffer, BUFFLEN-1, in);
            if (feof(in) ) return (JPROUTINE + 1);
            buffer[strlen(buffer)-1] = '\0';
            strcpy(hsecond,buffer);
            fgets(buffer, BUFFLEN-1, in);
            if (feof(in) ) return (JPROUTINE + 1);
            buffer[strlen(buffer)-1] = '\0';
            strcpy(hthird,buffer);
            fgets(buffer, BUFFLEN-1, in);
            if (feof(in) ) return (JPROUTINE + 1);
            buffer[strlen(buffer)-1] = '\0';
            strcpy(hfourth,buffer);
            return 0;
        }
        else
        {
            fgets(buffer, BUFFLEN-1, in);
            if (feof(in) ) return (JPROUTINE + 1);
            fgets(buffer, BUFFLEN-1, in);
            if (feof(in) ) return (JPROUTINE + 1);
            fgets(buffer, BUFFLEN-1, in);
            if (feof(in) ) return (JPROUTINE + 1);
            fgets(buffer, BUFFLEN-1, in);
            if (feof(in) ) return (JPROUTINE + 1);
        }

    }
}
