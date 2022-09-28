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

#define _fcd char *
#define _fcdtocp(a) a
#define _fcdlen(a) strlen(a)

char *fcd2char();  /* fortran to c string convertion (alloc memory) */

/* defines for FORTRAN subroutine */
#ifdef FORTRAN_NO_UNDERSCORE
#define INTLOGT intlogt
#else
#define INTLOGT intlogt_
#endif

/* Pointer to display function */
void (*g_pp_err_fn)(char * buffer) = 0;

void intlogs(void (*pp_error)(char *)) {

  g_pp_err_fn = pp_error;
  return;
}

void INTLOGT(char * message, long length) {
char * buffer;
int messageLen;

#define MAXMESSAGE 120
  messageLen = (length > MAXMESSAGE) ? MAXMESSAGE : length;
#undef MAXMESSAGE

  if( g_pp_err_fn != 0 ) {

    buffer = (char*) malloc(messageLen+1);
    if( buffer == NULL) {
      perror("INTLOGT: malloc error");
      return;
    }

    strncpy(buffer,message,messageLen);
    buffer[messageLen] = '\0';
    {
     char * p;
      p  = buffer + messageLen - 1;
      while(*p == ' ') {
        *p = '\0';
        p--;
      }
    }
    g_pp_err_fn(buffer);
    free(buffer);

  }
  return;
}


void intlog2(char *msg) {
  INTLOGT(msg,strlen(msg));
}

void intlog2ga(char *msg1, char *msg2) {
  INTLOGT(msg1,strlen(msg1));
  INTLOGT(msg2,strlen(msg2));
}
