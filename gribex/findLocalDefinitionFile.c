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
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include "common/fortint.h"

#ifdef TABLE_PATH
#define USER 1
#else
#define USER 0
#define TABLE_PATH "/usr/local/apps/libemos/tables/gribex"
#endif


char* findLocalDefinitionFile(fortint);

int fileExists(char* fileName) {
struct stat buf;
int status;

  status = stat(fileName,&buf);
  if( (status == 0) && S_ISREG(buf.st_mode) ) return 1;

  return 0;
}

char* findLocalDefinitionFile(fortint definitionNumber) {
char * directoryPath;
char * directoryName;
static char fullFileName[256];
char fileName[] = "localDefinitionTemplate_ccc_sss_nnn";
char defaultDirectory[] = "/usr/local/apps/libemos/tables/gribex";
char * endString;
char * startString;
fortint n;
int DefinitionNumber = (int) definitionNumber;
int centre    = DefinitionNumber/1000000;
int subcentre = (DefinitionNumber/1000)%1000;
int number    = DefinitionNumber%1000;
int defaultDirectoryChosen = 0;

  sprintf((fileName+24),"%03d",centre);
  sprintf((fileName+27),"_%03d",subcentre);
  sprintf((fileName+31),"_%03d",number);
  directoryPath = (char*) getenv("LOCAL_DEFINITION_TEMPLATES");
  if( directoryPath == NULL ) {
    defaultDirectoryChosen = 1;
    if(USER){
      char temp[256] = TABLE_PATH;
      strcat(temp,"/gribtemplates");
      directoryPath = temp;
    }
    else {
      directoryPath = TABLE_PATH;
    }
  }
/*
// The directoryPath can have a list of directories separated by ':'.
// Check each possible directory to find a suitable file.
*/

  startString = directoryPath;

  while( (endString = strchr(startString,':')) != NULL ) {
    n = (int) (endString - startString);

    strncpy(fullFileName,startString,n);
    fullFileName[n] = '/';
    strcpy(&fullFileName[n+1],fileName);

    if( fileExists(fullFileName) ) return fullFileName;
    startString += (n+1);
  }
  if( startString != directoryPath )
    n = strlen(directoryPath) - (int) (startString - directoryPath);
  else
    n = strlen(directoryPath);

  strncpy(fullFileName,startString,n);
  fullFileName[n] = '/';
  strcpy(&fullFileName[n+1],fileName);

  if( fileExists(fullFileName) )
    return fullFileName;
  else {
       /*try with the center = ECMWF and subcenter = 0
       printf("didnt found file name = %s\n", fullFileName);*/
       sprintf((fileName+24),"%03d",subcentre);
       sprintf((fileName+27),"_%03d",0);
       sprintf((fileName+31),"_%03d",number);
       n = strlen(directoryPath);
       strncpy(fullFileName,directoryPath,n);
       fullFileName[n] = '/';
       strcpy(&fullFileName[n+1],fileName);
       if( fileExists(fullFileName) ) return fullFileName;
     return NULL;
  }
}
