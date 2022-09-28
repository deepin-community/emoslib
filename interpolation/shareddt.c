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

#include "sharedlib.h"
#include "common/fortint.h"
#include "common/JPointer.h"

#ifdef FORTRAN_NO_UNDERSCORE
#define SHAREDDT shareddt
#else
#define SHAREDDT shareddt_
#endif

fortint SHAREDDT(
  JPointer* ipdum)
/*
C
C**** SHAREDDT
C     Provides Fortran wrapper to the shmdt call via release_shared_file
C
C     IPDUM   - Dummy array for mapping legendre function file
C     KRET    - Return status, 0 = OK.
t
*/
{
  fortint kret =0;
  int ret = release_shared_file(*ipdum) ;
  kret = (fortint) ret ;
  return kret;
}
