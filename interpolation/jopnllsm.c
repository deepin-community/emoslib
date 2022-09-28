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
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include "common/fortint.h"
#include "common/fortreal.h"
#include "common/JPointer.h"

#ifdef FORTRAN_NO_UNDERSCORE
#define JOPNLLSM jopnllsm
#define JMAKLL jmakll
#define INTLOGT intlogt
#define PBOPEN pbopen
#define PBCLOSE pbclose
#else
#define JOPNLLSM jopnllsm_
#define JMAKLL jmakll_
#define INTLOGT intlogt_
#define PBOPEN pbopen_
#define PBCLOSE pbclose_
#endif

#if defined(linux) && !defined(darwin)
# if !defined __off64_t_defined
typedef __off64_t off64_t;
#define __off64_t_defined
#ifdef FOPEN64
void *mmap64(void *__addr, size_t __len, int __prot, int __flags, int __fd, __off64_t __offset);
#endif
#endif
#endif

#ifdef FOPEN64
#define OFF_T off64_t
extern OFF_T ftello64(FILE *);
#else
#define OFF_T off_t
#endif

int iitimer(int *seconds)
{
    time_t timval1;
    time(&timval1);

    if (*seconds == 0) return (int)timval1;

    return (int)(timval1 - (time_t)*seconds);
}

#if defined(CRAY)
typedef void* _fcd;
#else
#define _fcd char *
#endif

void PBOPEN(fortint* unit, _fcd name, _fcd mode, fortint* iret, fortint l1, fortint l2);
void PBCLOSE(fortint* unit, fortint* iret);
void JMAKLL(fortint* kunit, fortint* ktrunc, fortfloat* pintvl, fortfloat* plat, fortfloat* pleg, fortint* kret);
void INTLOGT(char * message, long length);
FILE * pbfp(long);

void JOPNLLSM(
  JPointer* ipdum,
  fortfloat* platinc,
  fortint* iktrunc,
  fortint* kunit,
  fortfloat* zbuild,
  fortint* kret)
/*
C
C**** JOPNLLSM
C
C     Purpose
C     _______
C
C     This routine finds a file of legendre polynomials corresponding
C     to a given grid interval and truncation and maps the file to an
C     array and returns a file descriptor.
C
C     Interface
C     _________
C
C     CALL JOPNLLSM(IPDUM,PLATINC,KTRUNC,KUNIT,ZBUILD,KRET)
C
C     Input parameters
C     ________________
C
C     IPDUM   - Dummy array for mapping legendre function file
C     PLATINC - Grid interval in degrees
C     KTRUNC  - Spherical truncation
C
C     Output parameters
C     _________________
C
C     KUNIT   - file descriptor from PBOPEN
C               NULL , open failed
C     ZBUILD  - Grid interval of the lat/long grid
C               underlying the legendre function file
C     KRET    - Return status, 0 = OK.
C
C     Common block usage
C     __________________
C
C     None
C
C     Method
C     ______
C
C     Builds a file name from the truncation and grid interval and
C     tries to open a file of that name.
C
C     If the file is already open (from a previous call) the
C     previous unit number is returned.
C     If a different file is already open (from a previous call), the
C     existing file is closed first.
C
C     If no file can be located, a file is created.
C
C     Externals
C     _________
C
C     GETENV     - Get value of an environment variable
C     JMAKLL     - Makes a file of legendre coefficients
C     PBOPEN     - Opens a file
C     PBCLOSE    - Closes a file
C
C     Reference
C     _________
C
C     NONE
C
C     Comments
C     ________
C
C     The filename for the legendre polynomials has the form:
C         cf_txxxx_raabbbb    Truncation xxxx, aa.bbbb degrees
C     For example,
C         cf_t0213_r0050000   T213             0.5 degrees
C         cf_t0106_r0250000   T106             2.5 degrees
C
C     On the C90, the file of polynomials may be cached in /work/marsint
C
C     Otherwise the file is located in (or will be created in) the first
C     directory given by one of the following (in the order listed, if
C     they exist):
C         environment variable PPDIR
C     or
C         the current working directory.
C
C     Author
C     ______
C
C     J.D.Chambers      *ECMWF*      Feb 1996
C
C     Modifications
C     _____________
C
C     J.D.Chambers      *ECMWF*      Mar 1996
C     Standardise the search order for the environment variables.
C
C     J.D.Chambers      *ECMWF*      Sept 1999
C     Use PB routines for OPEN and CLOSE (use index into table of FILE *)
C
*/
{
  int time1, time2;
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
static char yarray[12][21];
static int ncnt = 0;
static int nunit = -1;
int i;
OFF_T len, len_pleg;
static OFF_T oldlen;
char * fn;
static fortint fpindex;
static FILE * fp;
OFF_T off = 0;
static void * result = 0;
static void * oldresult = 0;
static void * resultarray[12];
static int unitarray[12];
fortfloat plat = 0.0;
int kbuild ;
OFF_T knum;
fortfloat * pleg;
int exist = 0;
char message[] = "JOPNLLSM: creating coefficients file";
int status;
pid_t process_id;
int ktrunc = (int) (*iktrunc);
char *dbg;

/*
//  Setup the file name
*/
  len_pleg =          sizeof(fortfloat) * (ktrunc+1) * (ktrunc+4) /2;
  *zbuild = *platinc;
  kbuild = (int) (((*zbuild)*100000.0) + 0.5);
  knum = (OFF_T) ((90.0/(*zbuild)) + 0.5) + 1;
  len  = (OFF_T) (knum * sizeof(fortfloat) * (ktrunc+1) * (ktrunc+4) /2);

  sprintf( ypfn+4, "%04d", ktrunc);
#ifdef REAL_8
  sprintf( ypfn+8, "_R%07d", kbuild);
#else
  sprintf( ypfn+8, "_r%07d", kbuild);
#endif

  dbg = getenv("JDCNDBG");
  if (dbg==0 || (strncmp(dbg,"1",1) && strncmp(dbg,"2",1) && strncmp(dbg,"3",1))) {
    dbg = 0;
  }
  if (dbg) {
    fprintf(stdout,"INTLOG DEBUG: JOPNLLSM: Coefficients file to open is:\n");
    fprintf(stdout,"INTLOG DEBUG: %s\n",ypfn);
  }

/*
//  If file already open, return the existing unit number
*/
  for(i=0;i<ncnt;i++) {
    strncpy(yold,yarray[i],20);
    strcat(yold,"\0");
    if( !strcmp( ypfn, yold ) ) {
      *kunit = (fortint) unitarray[i];
      *ipdum = (JPointer) resultarray[i];
      return;
    }
  }

/*
//  Otherwise, unmap and close the existing file
*/
  result = 0;
/*
  if( nunit != -1 ) {
    if( munmap((caddr_t) oldresult, oldlen) ) {
      perror("JOPNLLSM: munmap error");
      *kret = (fortint) 999;
      return;
    }
    PBCLOSE(&fpindex,kret);
    if( *kret != 0 ) {
      perror("JOPNLLSM: PBCLOSE error");
      *kret = (fortint) 999;
      return;
    }
  }
*/

/*
  If more than 12 files are memory mapped , error
*/
  if(ncnt>=12) {
    fprintf(stderr,"JOPNLLSM: attempt to memory map more than 12 files\n");
    *kret = (fortint) 999;
    return;
  }

/*
//  See if the  file has already been created.
*/
  fn = getenv("PPDIR");
  if( (fn != NULL) && (strlen(fn) != 0) ) {
    strcpy( filename, fn );
    strcat( filename, "/");
    strcat( filename, ypfn);
    l1 = strlen(filename);
    l2 = 1;
    PBOPEN(&fpindex,filename,"r",kret,l1,l2);
    if( *kret == 0 ) {
      fp = pbfp((long)fpindex);
      if( fp != NULL ) exist = 1;
    }
  }

  if( !exist ) {
    strcpy( filename, "./" );
    strcat( filename, ypfn);
    l1 = strlen(filename);
    l2 = 1;
    PBOPEN(&fpindex,filename,"r",kret,l1,l2);
    if( *kret == 0 ) {
      fp = pbfp((long)fpindex);
      if( fp != NULL ) exist = 1;
    }
  }

/*
//  If file doesn't exist, find a suitable directory for it.
*/
  if( !exist ) {
    fn = getenv( "PPDIR" );
    if( (fn != NULL) && (strlen(fn) != 0) ) {
      strcpy( filename, fn );
      strcat( filename, "/");
    }
    else
      strcpy( filename, "./" );

    strcat( filename, ypfn);

/*
//  Open it with write access, change mode to 'read only', and make it
*/
    INTLOGT(message,strlen(message));
    INTLOGT(filename,strlen(filename));

    process_id = getpid();

    strcpy(filedum, filename);
    sprintf(&filedum[strlen(filename)],"_%07d",process_id);

    l1 = strlen(filedum);
    l2 = 1;
    PBOPEN(&fpindex,filedum,"w",kret,l1,l2);
    if( (*kret) || ( fp = pbfp((long)fpindex) ) == NULL ) {
      strcpy( filename, "./" );
      strcat( filename, ypfn);
      strcpy(filedum, filename);
      sprintf(&filedum[strlen(filename)],"_%07d",process_id);
      l1 = strlen(filedum);
      l2 = 1;
      PBOPEN(&fpindex,filedum,"w",kret,l1,l2);
      if( *kret ) {
        perror("JOPNLLSM: PBOPEN error");
        return;
      }
      fp = pbfp((long)fpindex);
      if( fp == NULL ) {
        perror("JOPNLLSM: file pointer after PBOPEN is NULL");
        *kret = (fortint) 998;
        return;
      }
    }

    status = chmod( filedum, (mode_t) 0444 );
    if( status ) {
      perror("JOPNLLSM: chmod error");
      *kret = (fortint) 997;
      return;
    }

/*
//  Setup scratch legendre file.
*/
    pleg = (fortfloat *) malloc(len_pleg*2);
    if( pleg == NULL ) {
      perror("JOPNLLSM: malloc error.");
      *kret = (fortint) 996;
      return;
    }
    JMAKLL( &fpindex, iktrunc, zbuild, &plat, pleg, kret);
    if( *kret != 0 ) {
      free(pleg);
      return;
    }

/*
//  Close it and rename it
*/
    free(pleg);
    PBCLOSE(&fpindex,kret);
    status = rename(filedum, filename);
    if( status ) {
      perror("JOPNLLSM: rename error");
      *kret = (fortint) 995;
      return;
    }
  }

/*
//  Check the file size and reopen it with read access
*/
  struct stat st;
  if( -1==stat(filename,&st) ) {
    perror(filename);
    *kret = (fortint) 994;
    return;
  }
  if( len != st.st_size ) {
    fprintf(stderr,"JOPNLLSM: coefficients file size should be %lld bytes, but it is %lld bytes.\n", len, st.st_size);
    *kret = (fortint) 993;
    return;
  }

  l1 = strlen(filename);
  l2 = 1;
  PBOPEN(&fpindex,filename,"r",kret,l1,l2);
  if( *kret ) {
    perror("JOPNLLSM: PBOPEN error");
    *kret = (fortint) 992;
    return;
  }
  fp = pbfp((long)fpindex);
  if( fp == NULL ) {
    perror("JOPNLLSM: file pointer after PBOPEN is NULL");
    *kret = (fortint) 992;
    return;
  }

/*
//  Map the file
*/
  nunit = fileno(fp);
  if( nunit >= 0 ) {
    char *p      = getenv("PPMAPADDR");
    caddr_t addr = (caddr_t) (p?atol(p):0);

    /* time1 = 0; */
    /* time2 = iitimer(&time1); */

#ifdef FOPEN64
    result =  (void *) mmap64(addr,(size_t) len, (PROT_READ),
                            MAP_SHARED, (int) nunit, (OFF_T) off);
#else
    result =  (void *) mmap(addr,(size_t) len, (PROT_READ),
                            MAP_SHARED, (int) nunit, (OFF_T) off);
#endif

    if( (caddr_t) result == (caddr_t) -1 ) {
      perror("JOPNLLSM: mmap error");
      *kret = (fortint) 992;
      return;
    }
    oldresult = result;
    oldlen = len;
  }

  *ipdum = (JPointer) result;
  *kunit = (fortint) nunit;
  strcpy( yarray[ncnt], ypfn);
  resultarray[ncnt] = result;
  unitarray[ncnt] = nunit;
  ncnt+=1;

  /* time2 = iitimer(&time1); */
  /* printf("MMAP-timer %d\n",iiii_time); */

  return;
}

