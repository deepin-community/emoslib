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
/* ****************************************************************
** Name: *func.c*
** ----
** Products Generation c functions used by Fortran.
**
** D. Jokic, ECMWF, Oct-2009.
**
** Modifications: -
**
** Initial AIX version Oct-2009.
** ****************************************************************/
#include <stdio.h>
#include <stdarg.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/resource.h>

#    define  cstr2fort(a)  a
#    define _fcd char *
#    define _fcdtocp(a) a
#    define _fcdlen(a) strlen(a)

#    define ITIMER   itimer_
#    define FTIMER   ftimer_
#    define RUSAGE   rusage_


/* ================================================================
 * Name: *ITIMER*
 * ----
 * - double ftimer_(double *time)
 * Real*8 FTIMER,time in F90
 * double timer = 0.0; timer=FTIMER(&timer); ... timer=FTIMER(&timer);
** ===============================================================*/
double FTIMER(double *time)
{
    double diff;
    struct timeval now;


    gettimeofday( &now, NULL );
    diff = (double)now.tv_sec + ((double)now.tv_usec / 1000000.);
    /* printf("%g %g\n",*time,diff); */

    if (*time != 0.0) diff -= *time;
/*
    {
        time_t      tv_sec;      * seconds * 
        suseconds_t tv_usec;     * microseconds * 

        tv_usec = (*time%1)*1000000.;
        tv_sec = (time_t) ((long)(*time/1))

        tv_sec -= now.tv_sec;
        tv_usec -= now.tv_usec; 
        if (tv_usec < 0)
        {
            tv_sec--;
            tv_usec += 1000000;
        }

        diff = (double)tv_sec + ((double)tv_usec / 1000000.);
    }
 */
    return diff;
}

int ITIMER(int *seconds)
{
   time_t timval1;
   time(&timval1);

   if (*seconds == 0) return (int)timval1;

   return (int)(timval1 - (time_t)*seconds);
}

int RUSAGE()
{
   struct rusage usage;
   int who = RUSAGE_SELF;
   int istat;

   if (getrusage(who, &usage) != 0) return 1;


   printf("RUSAGE user time used %d\n",usage.ru_utime.tv_sec);
   printf("RUSAGE system time used %d\n",usage.ru_stime.tv_sec);

   printf("RUSAGE maximum resident set size %d\n",usage.ru_maxrss);
   printf("RUSAGE integral shared memory size %d\n",usage.ru_ixrss);
   printf("RUSAGE integral unshared data size %d\n",usage.ru_idrss);
   printf("RUSAGE integral unshared stack size %d\n",usage.ru_isrss);
   printf("RUSAGE page reclaims %d\n",usage.ru_minflt);
   printf("RUSAGE page faults %d\n",usage.ru_majflt);
   printf("RUSAGE swaps %d\n",usage.ru_nswap);
   printf("RUSAGE block input operations %d\n",usage.ru_inblock);
   printf("RUSAGE block output operations %d\n",usage.ru_oublock);
   printf("RUSAGE messages sent %d\n",usage.ru_msgsnd);
   printf("RUSAGE messages received %d\n",usage.ru_msgrcv);
   printf("RUSAGE signals received %d\n",usage.ru_nsignals);
   printf("RUSAGE voluntary context switches %d\n",usage.ru_nvcsw);
   printf("RUSAGE involuntary context switches %d\n",usage.ru_nivcsw);

   return 0;
}

