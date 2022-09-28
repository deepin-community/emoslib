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
#include "common/JPointer.h"
#include "common/fortint.h"
#include "common/fortreal.h"


/*
 * memory containers and handlers (not to be used directly)
 */

static struct generic_userspace_t {
  JPointer array;
  int      nbytes;
  int      isstatic;
}
generic_userspace[11] = {0,};


int generic_userspace_static(struct generic_userspace_t *curr, fortfloat *array_, fortint *nbytes_)
{
    if (curr->array)
        free(curr->array);

    curr->array    = (JPointer) array_;
    curr->nbytes   = *nbytes_;
    curr->isstatic = 1;

    return 0;
}


JPointer generic_userspace_get(struct generic_userspace_t *curr, fortint *nbytes_)
{
    JPointer temp_array = NULL;

    /* if current user-space memory can contain request, return  */
    if ( ((curr->array!=NULL) && (curr->nbytes>=*nbytes_))
         || (nbytes_<=0) )
        return curr->array;

    if (curr->isstatic==1) {

        /* static memory management (ProdGen-style) */
        fprintf(stderr,
            "ERROR: userspace static allocation too small "
            "(current: %db, required: %db)\n", curr->nbytes, *nbytes_);
        return 0;

    }
    else if (curr->nbytes<*nbytes_) {

        /* dynamic memory management: reallocate as necessary */
        if (curr->array!=NULL)
            free(curr->array);

        /*
        fprintf(stdout,
            "INFO: userspace dynamic allocation requested: "
            "%ldb\n", *nbytes_);
        */
        temp_array = (JPointer) malloc(*nbytes_);
        if (temp_array==NULL) {
            fprintf(stderr,
                "ERROR: userspace dynamic allocation failure "
                "(requested: %db)\n", *nbytes_);
            return 0;
        }
        curr->nbytes = *nbytes_;
        curr->array  = temp_array;

    }

    return curr->array;
}


int generic_userspace_free(struct generic_userspace_t *curr)
{
    if (curr->array)
        free(curr->array);
    curr->nbytes   = 0;
    curr->isstatic = 0;
    return 0;
}


/*
 * HIRLAM/HIRLAMW/HIRLSM memory management interfaces
 */

int hirlam_userspace_1_static_ (fortfloat *array_, fortint *nbytes_) { return generic_userspace_static(&generic_userspace[ 0],array_,nbytes_); }
int hirlam_userspace_2_static_ (fortfloat *array_, fortint *nbytes_) { return generic_userspace_static(&generic_userspace[ 1],array_,nbytes_); }
int hirlam_userspace_3_static_ (fortfloat *array_, fortint *nbytes_) { return generic_userspace_static(&generic_userspace[ 2],array_,nbytes_); }
int hirlamw_userspace_1_static_(fortfloat *array_, fortint *nbytes_) { return generic_userspace_static(&generic_userspace[ 3],array_,nbytes_); }
int hirlamw_userspace_2_static_(fortfloat *array_, fortint *nbytes_) { return generic_userspace_static(&generic_userspace[ 4],array_,nbytes_); }
int hirlamw_userspace_3_static_(fortfloat *array_, fortint *nbytes_) { return generic_userspace_static(&generic_userspace[ 5],array_,nbytes_); }
int hirlsm_userspace_1_static_ (fortfloat *array_, fortint *nbytes_) { return generic_userspace_static(&generic_userspace[ 6],array_,nbytes_); }
int hirlsm_userspace_2_static_ (fortfloat *array_, fortint *nbytes_) { return generic_userspace_static(&generic_userspace[ 7],array_,nbytes_); }
int hirlsm_userspace_3_static_ (fortfloat *array_, fortint *nbytes_) { return generic_userspace_static(&generic_userspace[ 8],array_,nbytes_); }
int hirlsm_userspace_4_static_ (fortfloat *array_, fortint *nbytes_) { return generic_userspace_static(&generic_userspace[ 9],array_,nbytes_); }
int hirlsm_userspace_5_static_ (fortfloat *array_, fortint *nbytes_) { return generic_userspace_static(&generic_userspace[10],array_,nbytes_); }

JPointer hirlam_userspace_1_get_ (fortint *nbytes_) { return generic_userspace_get(&generic_userspace[ 0],nbytes_); }
JPointer hirlam_userspace_2_get_ (fortint *nbytes_) { return generic_userspace_get(&generic_userspace[ 1],nbytes_); }
JPointer hirlam_userspace_3_get_ (fortint *nbytes_) { return generic_userspace_get(&generic_userspace[ 2],nbytes_); }
JPointer hirlamw_userspace_1_get_(fortint *nbytes_) { return generic_userspace_get(&generic_userspace[ 3],nbytes_); }
JPointer hirlamw_userspace_2_get_(fortint *nbytes_) { return generic_userspace_get(&generic_userspace[ 4],nbytes_); }
JPointer hirlamw_userspace_3_get_(fortint *nbytes_) { return generic_userspace_get(&generic_userspace[ 5],nbytes_); }
JPointer hirlsm_userspace_1_get_ (fortint *nbytes_) { return generic_userspace_get(&generic_userspace[ 6],nbytes_); }
JPointer hirlsm_userspace_2_get_ (fortint *nbytes_) { return generic_userspace_get(&generic_userspace[ 7],nbytes_); }
JPointer hirlsm_userspace_3_get_ (fortint *nbytes_) { return generic_userspace_get(&generic_userspace[ 8],nbytes_); }
JPointer hirlsm_userspace_4_get_ (fortint *nbytes_) { return generic_userspace_get(&generic_userspace[ 9],nbytes_); }
JPointer hirlsm_userspace_5_get_ (fortint *nbytes_) { return generic_userspace_get(&generic_userspace[10],nbytes_); }

int hirlam_userspace_free_ () {
    generic_userspace_free(&generic_userspace[ 0]);
    generic_userspace_free(&generic_userspace[ 1]);
    generic_userspace_free(&generic_userspace[ 2]);
    return 0;
}
int hirlamw_userspace_free_() {
    generic_userspace_free(&generic_userspace[ 3]);
    generic_userspace_free(&generic_userspace[ 4]);
    generic_userspace_free(&generic_userspace[ 5]);
    return 0;
}
int hirlsm_userspace_free_ () {
    generic_userspace_free(&generic_userspace[ 6]);
    generic_userspace_free(&generic_userspace[ 7]);
    generic_userspace_free(&generic_userspace[ 8]);
    generic_userspace_free(&generic_userspace[ 9]);
    generic_userspace_free(&generic_userspace[10]);
    return 0;
}

