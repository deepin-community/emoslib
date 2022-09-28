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

#ifndef JPOINTER_H
#define JPOINTER_H

#ifdef INTEGER_IS_INT
#define JPointer int *
#else
#if defined hpR64 || defined hpiaR64
#define JPointer long long *
#else
#define JPointer long *
#endif
#endif

#endif /* end of JPOINTER_H */
