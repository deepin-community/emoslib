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

void csgnbt_(
  fortint * kout,
  fortint * kin,
  fortint * kpos,
  fortint * kret) {

fortint value = *kin;
unsigned fortint sign = (value < 0 ) ? (1 << ((*kpos)-1)) : 0;
unsigned fortint mask = ~(-1 << ((*kpos)-1));

  if( value < 0 )
    *kout = ((-value) & mask) | sign;
  else
    *kout = (value & mask);

  *kret = 0;
  return;
}

void csgnbt(
  fortint * kout,
  fortint * kin,
  fortint * kpos,
  fortint * kret) {

  csgnbt_(kout,kin,kpos,kret);
}
