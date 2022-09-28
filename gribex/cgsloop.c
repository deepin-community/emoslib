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

/**************************************************************************
.TITLE    ECMWF Utility
.NAME     GRIBEX
.SECTION  L
.AUTHOR   Otto Pesonen
.DATE     05-JUN-1996 / 05-JUN-1996 / OP
.VERSION  1.0
.LANGUAGE ANSI-C
.FILE     cgsloop.c
.OS       FUJITSU
*
*  Vector loops of gsbite.f coded in C
*
*  C has unsigned datatypes
*
************************************o*************************************/

#include <stdio.h>

#pragma global noalias
#pragma global novrec

gsdecode_(
  unsigned int *in,
  unsigned int *out,
  int *ist_, int *ost_, int *len_, unsigned int *mask_, int *ish_)
{
  int oidx = 0;
  int iidx = 0;

           int ist  = *ist_;
           int ost  = *ost_; 
           int len  = *len_;
  unsigned int mask = *mask_;
           int ish  = *ish_;

  int factor;

  if( ish < 0 )
  { 
    ish = (-ish);

    factor = 1<<ish;

#pragma loop novrec
    for( ; len ; len-- )
    {
      out[oidx] = out[oidx] | (in[iidx] & mask) / factor;
      oidx +=  ost;
      iidx +=  ist;
    }
  }
  else
  {
    factor = 1<<ish;

#pragma loop novrec
    for( ; len ; len-- )
    {
      out[oidx] = out[oidx] | (in[iidx] & mask) * factor;
      oidx +=  ost;
      iidx +=  ist;
    }
  }
}

gsencode_(
  unsigned int *in,
  unsigned int *out,
  int *ist_, int *ost_, int *len_, unsigned int *mask_, int *ish_)
{
  int oidx = 0;
  int iidx = 0;

           int ist  = *ist_;
           int ost  = *ost_; 
           int len  = *len_;
  unsigned int mask = *mask_;
           int ish  = *ish_;

  int factor;

  ish = (-ish);

  if( ish < 0 )
  { 
    ish = (-ish);

    factor = 1<<ish;
#pragma loop novrec
    for( ; len ; len-- )
    {
      out[oidx] = ((in[iidx] / factor) & mask) |
                  (out[oidx] & (~mask)) ;
      oidx +=  ost;
      iidx +=  ist;
    }
  }
  else
  {
    factor = 1<<ish;

#pragma loop novrec
    for( ; len ; len-- )
    {
      out[oidx] = ((in[iidx] * factor) & mask) |
                  (out[oidx] & (~mask)) ;
      oidx +=  ost;
      iidx +=  ist;
    }
  }
}

