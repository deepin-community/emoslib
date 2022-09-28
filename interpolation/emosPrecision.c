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

int emosPrecision() {
/*
// Returns:
//   64 if double-precision (64-bit) option has been used for REALs,
//   64 if compiled on CRAY, (REALs are at least 64-bit precision),
//   32 otherwise.
*/
#ifdef REAL_8
  return (64);
#else
  return (32);
#endif
}
