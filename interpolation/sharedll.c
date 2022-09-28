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

#define SHAREDLL sharedll_

#ifdef linux
#ifndef __USE_LARGEFILE64
#define __USE_LARGEFILE64
#endif
#include <sys/stat.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/mman.h>
#include <string.h>
#include "sharedlib.h"
#include "common/fortint.h"
#include "common/JPointer.h"

#include <sys/ipc.h>
#include <sys/shm.h>

/* extern int sharedlib_dbg = 0; */

#ifdef FORTRAN_NO_UNDERSCORE
#define SHAREDLL sharedll
#define PBOPEN pbopen
#define PBCLOSE pbclose
#else
#define SHAREDLL sharedll_
#define PBOPEN pbopen_
#define PBCLOSE pbclose_
#endif



fortint SHAREDLL(
  JPointer* ipdum,
  fortint* iktrunc,
  double* platinc)

/*
C
C**** SHAREDLL
C
C     IPDUM   - Dummy array for mapping legendre function file
C     PLATINC - Grid interval in degrees
C     KTRUNC  - Spherical truncation
C     KRET    - Return status, 0 = OK.
C
*/
{

#if (defined hpR64) || (defined hpiaR64)
long l1, l2;
#else
fortint l1, l2;
#endif

char filedum[128], filename[128];
#ifdef REAL_8
char ypfn[21] =        "CF_Txxxx_Raabbbb";
#else
char ypfn[21] =        "cf_txxxx_raabbbb";
#endif
static char yold[21] = "xxxxxxxxxxxxxxxxxxxx";
char defaultDirectory[] = "./";
int kbuild ;
char * fn;
static fortint fpindex;
static FILE * fp;
static void * result = 0;
fortint kret = 0;

int exist = 0;
int status;
pid_t process_id;
int ktrunc = (int) (*iktrunc);

/*
//  Setup the file name
*/
  sprintf( ypfn+4, "%04d", ktrunc);
  kbuild = (int) (((*platinc)*100000.0) + 0.5);
#ifdef REAL_8
  sprintf( ypfn+8, "_R%07d", kbuild);
#else
  sprintf( ypfn+8, "_r%07d", kbuild);
#endif

/*
//  See if the  file has already been created.
*/
  fn = getenv("PPDIR");
  if(fn == NULL) fn = defaultDirectory;

  if( (fn != NULL) && (strlen(fn) != 0) ) {
    strcpy( filename, fn );
    strcat( filename, "/");
    strcat( filename, ypfn);
    l1 = strlen(filename);
    l2 = 1;
  }
  /* printf("ll share_file filename=%s\n",filename); */

  result = (void *) share_file(filename);
  if (result == NULL) kret = 1;

  *ipdum = (JPointer) result;

  return kret;
}
