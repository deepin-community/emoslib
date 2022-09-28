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

#ifndef BITMAP_H
#define BITMAP_H

#ifdef FORTRAN_NO_UNDERSCORE
#define MAKEMAP makemap
#define SHOWMAP showmap
#define GMAPBIT gmapbit
#else
#define MAKEMAP makemap_
#define SHOWMAP showmap_
#define GMAPBIT gmapbit_
#endif

int MAKEMAP(char * , int * , int * , char ** , int );
void SHOWMAP(char ** , int * , int * );
int GMAPBIT( char ** , int * , int * , int * );
int findNumber(char * , int , int , int * , char );
char findDelimiter(char * , int , int );
int findCharacter(char * , int , int , char );
void setBit(char * , int , int , int , int );
void copyRow(char * , int , int , int , int );

#endif /* end of BITMAP_H */
