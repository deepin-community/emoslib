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

#include "common/fortint.h"

void dsgnbt_(
  fortint * kout,
  fortint * kin,
  fortint * kpos,
  fortint * kret) {

fortint value = *kin;
unsigned fortint sign = value & (1 << ((*kpos)-1));
unsigned fortint mask = ~(-1 << ((*kpos)-1));
fortint new = value & mask;

  if( sign )
    *kout = -new;
  else
    *kout = new;

  *kret = 0;
  return;
}

void dsgnbt(
  fortint * kout,
  fortint * kin,
  fortint * kpos,
  fortint * kret) {

  dsgnbt_(kout,kin,kpos,kret);
}
