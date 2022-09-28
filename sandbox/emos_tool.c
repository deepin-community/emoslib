/*
 * (C) Copyright 1981-2016 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Baudouin Raoult
/// @date Apr 2015

#include <stdio.h>
#include <grib_api.h>

typedef int fortint;
typedef double fortfloat;
typedef void (*emos_cb_proc)(char *);

extern fortint intout_(const char *name,
                       const fortint ints[],
                       const fortfloat reals[],
                       const char *value,
                       const fortint name_len,
                       const fortint value_len);

extern fortint intin_(const char *name,
                      const fortint ints[],
                      const fortfloat reals[],
                      const char *value,
                      const fortint name_len,
                      const fortint value_len);

extern fortint intf_(const void *grib_in,
                     const fortint *length_in,
                     const fortfloat values_in[],
                     void *grib_out,
                     fortint *length_out,
                     fortfloat values_out[]);

extern fortint intf2(const void *grib_in,
                     const fortint *length_in,
                     void *grib_out,
                     fortint *length_out);

extern fortint intuvs2_(char *vort_grib_in,
                        char *div_grib_in,
                        const fortint *length_in,
                        char *u_grib_out,
                        char *v_grib_out,
                        const fortint *length_out);

extern fortint intuvp2_(const void *vort_grib_in,
                        const void *div_grib_in,
                        const fortint *length_in,
                        void *u_grib_out,
                        void *v_grib_out,
                        fortint *length_out);

extern fortint intvect2_(const void *u_grib_in,
                         const void *v_grib_in,
                         const fortint *length_in,
                         void *u_grib_out,
                         void *v_grib_out,
                         fortint *length_out);

extern fortint intuvs_(const void *vort_grib_in,
                       const void *div_grib_in,
                       const fortint *length_in,
                       void *u_grib_out,
                       void *v_grib_out,
                       fortint *length_out);

extern fortint intuvp_(const void *vort_grib_in,
                       const void *div_grib_in,
                       const fortint *length_in,
                       void *u_grib_out,
                       void *v_grib_out,
                       fortint *length_out);

extern fortint intvect_(const void *u_grib_in,
                        const void *v_grib_in,
                        const fortint *length_in,
                        void *u_grib_out,
                        void *v_grib_out,
                        fortint *length_out);

extern fortint iscrsz_();

extern fortint ibasini_(const fortint *force);

extern void intlogm_(fortint (*)(char *, fortint));

extern void intlogs(emos_cb_proc proc);

extern fortint areachk_(const fortfloat *we,
                        const fortfloat *ns,
                        fortfloat *north,
                        fortfloat *west,
                        fortfloat *south,
                        fortfloat *east);

extern fortint emosnum_(fortint *value);

extern void freecf_(const fortint *flag);

extern void jvod2uv_(const fortfloat vor[],
                     const fortfloat div[],
                     const fortint *ktin,
                     fortfloat u[],
                     fortfloat v[],
                     const fortint *ktout);


extern fortint jgglat_(const fortint *KLAT, fortfloat PGAUSS[]);

extern void jnumgg_(const fortint *knum,
                    const char *htype,
                    fortint kpts[],
                    fortint *kret,
                    fortint htype_len);

extern fortint wvqlint_(const fortint *knum,
                        const fortint numpts[],
                        const fortint *ke_w,
                        const fortint *kn_s,
                        const fortfloat *reson,
                        const fortfloat oldwave[],
                        fortfloat newwave[],
                        const fortfloat *north,
                        const fortfloat *west,
                        const fortint *kparam,
                        const fortfloat *pmiss,
                        const fortfloat *rns);

extern void wv2dint_(const fortint *knum,
                     const fortint numpts[],
                     const fortint *ke_w,
                     const fortint *kn_s,
                     const fortfloat *reson,
                     const fortfloat oldwave[],
                     fortfloat newwave[],
                     const fortfloat *north,
                     const fortfloat *west,
                     const fortint *knspec, // <== What is that?
                     const fortfloat *pmiss,
                     const fortfloat *rns);

extern fortint hirlam_(const fortint *l12pnt,
                       const fortfloat oldfld[],
                       const fortint *kount,
                       const fortint *kgauss,
                       const fortfloat area[],
                       const fortfloat pole[],
                       const fortfloat grid[],
                       fortfloat newfld[],
                       const fortint *ksize,
                       fortint *nlon,
                       fortint *nlat);

extern fortint hirlsm_(const fortint *l12pnt,
                       const fortfloat oldfld[],
                       const fortint *kount,
                       const fortint *kgauss,
                       const fortfloat area[],
                       const fortfloat pole[],
                       const fortfloat grid[],
                       fortfloat newfld[],
                       const fortint *ksize,
                       fortint *nlon,
                       fortint *nlat);

extern fortint hirlamw_(const fortint *l12pnt,
                        const fortfloat oldfldu[],
                        const fortfloat oldfldv[],
                        const fortint *kount,
                        const fortint *kgauss,
                        const fortfloat area[],
                        const fortfloat pole[],
                        const fortfloat grid[],
                        fortfloat newfldu[],
                        fortfloat newfldv[],
                        const fortint *ksize,
                        fortint *nlon,
                        fortint *nlat);

#define MAX 20

#define OPTION_STR 0
#define OPTION_INT 1
#define OPTION_REAL 2
#define OPTION_BOOL 3

const char *types[] = { "string", "integer", "real", "boolean"};


typedef struct value {
    const char *name;
    const char *intout;
} value;

typedef struct option {
    const char *name;
    const char *intout;
    const char *description;

    int type;
    int count;

    value *values;

    int set;
    char *arg;
    fortint ints[MAX];
    fortfloat reals[MAX];


} option;

static value styles[] = {
    {"dissemination", "dissemination",},
    {0,}
};


static value interpolation[] = {
    {"bilinear", "bilinear",},
    {"nlsm", "nearest lsm",},
    {"nn", "nearest neighbour",},
    {"off", "off",},
    {0,}
};


static value packings[] = {
    {"so", "second",},
    {"simple", "simple",},
    {"av", "archive",},
    {"complex", "complex",},
    {0,}
};




static option options[] = {
    {"accuracy", "accuracy", "Accuracy", OPTION_INT, 1, },
    {"area", "area", "Area north/west/south/east", OPTION_REAL, 4, },
    {"autoresol", "autoresol", "Auto resolution", OPTION_BOOL, 1, },
    {"bitmap", "bitmap", "Bitmap", OPTION_STR, 1, },
    {"frame", "frame", "Frame", OPTION_INT, 1, },
    {"grid", "grid", "Grid west-east/south-north increments", OPTION_REAL, 2, },
    {"hirlam12", NULL, "Use 12 points hirlam routines", OPTION_BOOL, 1, },
    {"hirlam4", NULL, "Use 4 points hirlam routines", OPTION_BOOL, 1, },
    {"intermediate_gaussian", "intermediate_gaussian", "Intermediate gaussian", OPTION_INT, 1, },
    {"interpolation", "interpolation", "Interpolation method", OPTION_STR, 1, interpolation},
    {"packing", "packing", "Packing", OPTION_STR, 1, packings},
    {"reduced", "reduced", "Reduced gaussian", OPTION_INT, 1, },
    {"regular", "regular", "Regular gaussian", OPTION_INT, 1, },
    {"rotation", "rotation", "Pole of rotation", OPTION_REAL, 2, },
    {"style", "style", "Style", OPTION_STR, 1, styles},
    {"truncation", "truncation", "Truncation", OPTION_INT, 1, },
    {"vod2uv", NULL, "Interpolate winds", OPTION_BOOL, 1, },
    {"wind", NULL, "Input is wind", OPTION_BOOL, 1, },
    {0,}
};


option *find_option(const char *name) {
    int j = 0;
    while (options[j].name) {
        if (strcmp(name, options[j].name) == 0) {
            return &options[j];
        }
        j++;
    }
    return NULL;
}

option *is_set(const char *name) {
    int j = 0;
    while (options[j].name) {
        if (strcmp(name, options[j].name) == 0 && options[j].set) {
            printf("%s is set\n", name);
            return &options[j];
        }
        j++;
    }
    printf("%s is not set\n", name);
    return NULL;
}

#define GRIB_CALL(a) if(a) { fprintf(stderr, "%s %s\n", #a, grib_get_error_message(a)); err=-1; goto cleanup;}
fortint hirlam(const char *ingrib, fortint inlen, char *outgrib, fortint *outlen, int twelve) {

    int err = 0;
    double *values = 0;
    grib_handle *h = 0;
    size_t count, size;
    long N;
    double *result;
    option *area = 0;
    option *grid = 0;
    option *pole = 0;
    const void *message;
    grib_handle *g;

    fortint l12pnt;
    fortint kount;
    fortint kgauss;
    fortint ksize;
    fortint nlon;
    fortint nlat;

    grib_util_grid_spec grid_spec = {0, };
    grib_util_packing_spec packing_spec = {0, };

    h  = grib_handle_new_from_message(NULL, (void *)ingrib, inlen);

    GRIB_CALL(grib_get_size(h, "values", &count));

    values = malloc(count * sizeof(double));
    GRIB_CALL(grib_get_double_array(h, "values", &values[0], &size));

    GRIB_CALL(grib_get_long(h, "N", &N));

    grid = find_option("grid");
    if (!grid->set) {
        fprintf(stderr, "HIRLAM: grid not specified\n");
        err = -1;
        goto cleanup;
    }

    pole = find_option("rotation");
    if (!pole->set) {
        fprintf(stderr, "HIRLAM: rotation not specified\n");
        err = -1;
        goto cleanup;
    }

    area = find_option("area");
    if (!area->set) {
        fprintf(stderr, "HIRLAM: area not specified, using global\n");
        area->reals[0] = 90;
        area->reals[1] = 0;
        area->reals[2] = -90;
        area->reals[3] = 360;
    }


    // long missingValuesPresent;
    // GRIB_CALL(grib_get_long(grib_, "missingValuesPresent", &missingValuesPresent));

    // double missing;
    // GRIB_CALL(grib_get_double(grib_, "missingValue", &missing));
    l12pnt = twelve;
    kount = count;
    kgauss = N;
    ksize = count * 4;
    result = malloc(ksize * sizeof(double));

    err = hirlam_(&l12pnt,
                  values,
                  &kount,
                  &kgauss,
                  area->reals,
                  pole->reals,
                  grid->reals,
                  result,
                  &ksize,
                  &nlon,
                  &nlat);

    if (err) {
        fprintf(stderr, "hirlam_ returns %d\n", err);
        goto cleanup;
    }

    if (nlon *  nlat > ksize) {
        fprintf(stderr, "HIRLAM: array too small %d <  %d\n", ksize, nlon *  nlat);
        err = -1;
        goto cleanup;
    }


    packing_spec.packing = GRIB_UTIL_PACKING_SAME_AS_INPUT;
    packing_spec.accuracy = GRIB_UTIL_ACCURACY_SAME_BITS_PER_VALUES_AS_INPUT;

    grid_spec.grid_type = GRIB_UTIL_GRID_SPEC_ROTATED_LL;

    grid_spec.Ni = nlon;
    grid_spec.Nj = nlat;
    grid_spec.iDirectionIncrementInDegrees = grid->reals[0];
    grid_spec.jDirectionIncrementInDegrees = grid->reals[1];

    grid_spec.longitudeOfFirstGridPointInDegrees = area->reals[1];
    grid_spec.longitudeOfLastGridPointInDegrees = area->reals[3];

    grid_spec.latitudeOfFirstGridPointInDegrees = area->reals[0];
    grid_spec.latitudeOfLastGridPointInDegrees = area->reals[2];

    grid_spec.latitudeOfSouthernPoleInDegrees = pole->reals[0];
    grid_spec.longitudeOfSouthernPoleInDegrees = pole->reals[1];


    g = grib_util_set_spec(h, &grid_spec, &packing_spec, 0, result, nlon * nlat, &err);
    GRIB_CALL(err);

    GRIB_CALL(grib_get_message(g, &message, &size));
    if (*outlen < size) {
        fprintf(stderr, "HIRLAM: buffer too small %d <  %d\n", *outlen, (int)size);
        err = -1;
        goto cleanup;
    }

    memcpy(outgrib, message, size);
    *outlen = size;

cleanup:

    if (h) grib_handle_delete(h);
    if (g) grib_handle_delete(g);
    if (values) free(values);
    if (result) free(result);

    return err;
}

fortint hirlamw(const char *u_ingrib, const char *v_ingrib, fortint inlen, char *u_outgrib, char *v_outgrib, fortint *outlen, int twelve) {

    int err = 0;
    double *u_values = 0;
    grib_handle *u = 0;
    grib_handle *v = 0;
    grib_handle *g = 0;
    size_t u_count, u_size;
    long u_N;
    double *u_result;
    option *area = 0;
    option *grid = 0;
    option *pole = 0;
    const void *u_message;

    double *v_values = 0;
    size_t v_count, v_size;
    long v_N;
    double *v_result;
    const void *v_message;

    fortint l12pnt;
    fortint kount;
    fortint kgauss;
    fortint ksize;
    fortint nlon;
    fortint nlat;

    grib_util_grid_spec grid_spec = {0, };
    grib_util_packing_spec packing_spec = {0, };

    u  = grib_handle_new_from_message(NULL, (void *)u_ingrib, inlen);

    GRIB_CALL(grib_get_size(u, "values", &u_count));

    u_values = malloc(u_count * sizeof(double));
    GRIB_CALL(grib_get_double_array(u, "values", &u_values[0], &u_size));

    GRIB_CALL(grib_get_long(u, "N", &u_N));

    v  = grib_handle_new_from_message(NULL, (void *)v_ingrib, inlen);

    GRIB_CALL(grib_get_size(v, "values", &v_count));

    v_values = malloc(v_count * sizeof(double));
    GRIB_CALL(grib_get_double_array(v, "values", &v_values[0], &v_size));

    GRIB_CALL(grib_get_long(v, "N", &v_N));

    if (u_N != v_N) {
        fprintf(stderr, "HIRLAM: grid mismatch %ld %ld\n", u_N, v_N);
        err = -1;
        goto cleanup;
    }

    if (u_count != v_count) {
        fprintf(stderr, "HIRLAM: count mismatch %ld %ld\n", u_count, v_count);
        err = -1;
        goto cleanup;
    }


    grid = find_option("grid");
    if (!grid->set) {
        fprintf(stderr, "HIRLAM: grid not specified\n");
        err = -1;
        goto cleanup;
    }

    pole = find_option("rotation");
    if (!pole->set) {
        fprintf(stderr, "HIRLAM: rotation not specified\n");
        err = -1;
        goto cleanup;
    }

    area = find_option("area");
    if (!area->set) {
        fprintf(stderr, "HIRLAM: area not specified, using global\n");
        area->reals[0] = 90;
        area->reals[1] = 0;
        area->reals[2] = -90;
        area->reals[3] = 360;
    }


    // long missingValuesPresent;
    // GRIB_CALL(grib_get_long(grib_, "missingValuesPresent", &missingValuesPresent));

    // double missing;
    // GRIB_CALL(grib_get_double(grib_, "missingValue", &missing));
    l12pnt = twelve;
    kount = u_count;
    kgauss = u_N;
    ksize = u_count * 4;
    u_result = malloc(ksize * sizeof(double));
    v_result = malloc(ksize * sizeof(double));

    err = hirlamw_(&l12pnt,
                   u_values,
                   v_values,
                   &kount,
                   &kgauss,
                   area->reals,
                   pole->reals,
                   grid->reals,
                   u_result,
                   v_result,
                   &ksize,
                   &nlon,
                   &nlat);

    if (err) {
        fprintf(stderr, "hirlamw_ returns %d\n", err);
        goto cleanup;
    }

    if (nlon *  nlat > ksize) {
        fprintf(stderr, "HIRLAMW: array too small %d <  %d\n", ksize, nlon *  nlat);
        err = -1;
        goto cleanup;
    }


    packing_spec.packing = GRIB_UTIL_PACKING_SAME_AS_INPUT;
    packing_spec.accuracy = GRIB_UTIL_ACCURACY_SAME_BITS_PER_VALUES_AS_INPUT;

    grid_spec.grid_type = GRIB_UTIL_GRID_SPEC_ROTATED_LL;

    grid_spec.Ni = nlon;
    grid_spec.Nj = nlat;
    grid_spec.iDirectionIncrementInDegrees = grid->reals[0];
    grid_spec.jDirectionIncrementInDegrees = grid->reals[1];

    grid_spec.longitudeOfFirstGridPointInDegrees = area->reals[1];
    grid_spec.longitudeOfLastGridPointInDegrees = area->reals[3];

    grid_spec.latitudeOfFirstGridPointInDegrees = area->reals[0];
    grid_spec.latitudeOfLastGridPointInDegrees = area->reals[2];

    grid_spec.latitudeOfSouthernPoleInDegrees = pole->reals[0];
    grid_spec.longitudeOfSouthernPoleInDegrees = pole->reals[1];

    grid_spec.uvRelativeToGrid = 1; // Check if hirlam does it


    g = grib_util_set_spec(u, &grid_spec, &packing_spec, 0, u_result, nlon * nlat, &err);
    GRIB_CALL(err);

    GRIB_CALL(grib_get_message(g, &u_message, &u_size));
    if (*outlen < u_size) {
        fprintf(stderr, "HIRLAMW: buffer too small %d <  %d\n", *outlen, (int)u_size);
        err = -1;
        goto cleanup;
    }

    memcpy(u_outgrib, u_message, u_size);
    grib_handle_delete(g);
    g = 0;

    g = grib_util_set_spec(v, &grid_spec, &packing_spec, 0, v_result, nlon * nlat, &err);
    GRIB_CALL(err);

    GRIB_CALL(grib_get_message(g, &v_message, &v_size));
    if (*outlen < v_size) {
        fprintf(stderr, "HIRLAMW: buffer too small %d <  %d\n", *outlen, (int)v_size);
        err = -1;
        goto cleanup;
    }

    memcpy(v_outgrib, v_message, v_size);
    if (u_size != v_size) {
        fprintf(stderr, "HIRLAMW: output size mismatch %ld %ld\n", u_size, v_size);
        err = -1;
        goto cleanup;
    }



    *outlen = u_size;

cleanup:

    if (u) grib_handle_delete(u);
    if (v) grib_handle_delete(v);
    if (g)grib_handle_delete(g);
    if (u_values) free(u_values);
    if (v_values) free(v_values);
    if (u_result) free(u_result);
    if (v_result) free(v_result);

    return err;
}

void usage(const char *prog) {
    int i, j;
    fprintf(stderr, "\nUsage: %s [options] in.grib out.grib\n", prog);
    fprintf(stderr, "\nOptions are:\n\n");
    j = 0;
    while (options[j].name) {
        const char *sep = "=";
        int l = 0;
        l += fprintf(stderr, " --%s", options[j].name);
        for (i = 0; i < options[j].count; i++) {
            l += fprintf(stderr, "%s%s", sep, types[options[j].type]);
            sep = "/";
        }
        while (l < 40) {
            l += fprintf(stderr, " ");
        }
        fprintf(stderr, "%s\n", options[j].description);
        j++;
    }
    fprintf(stderr, "\n");
    exit(1);
}


#ifdef FORTRAN_LINKER_PGI
#define main_ MAIN_
#else
#define main_ main
#endif


int main_( int argc, const char **argv ) {
    fortint dummy = 0;
    printf("WARNING: this tool if for debugging purposes only!!!\n");
    printf("EMOSNUM is %d\n", emosnum_(&dummy));
    option *o = NULL;
    const char *paths[2];
    FILE *f;
    FILE *g;

    int err = 0;
    const char *func = "NONE";
    int n = 0;


    int i, j, k, e;
    char *p;
    char *q;


    for (i = 1; i < argc; i++) {

        if (argv[i][0] == '-' && argv[i][1] == '-') {
            const char *a = argv[i];
            a += 2;

            j = 0;
            o = NULL;
            while (options[j].name && !o) {
                int len = strlen(options[j].name);
                if (strncmp(a, options[j].name, len) == 0 && (a[len] == '=' || a[len] == 0)) {
                    o = &options[j];
                    if (o->arg) {
                        free(o->arg);
                    }

                    if (a[len] == '=') {
                        o->arg = strdup(a + len + 1);
                    } else {
                        o->arg = strdup("1");
                    }
                }
                j++;
            }

            if (!o) {
                fprintf(stderr, "%s: invalid option [%s]\n", argv[0], a);
                usage(argv[0]);
            }

            printf("SET %s\n", o->name);
            o->set = 1;

            switch (o->type) {
            case OPTION_STR:


                if (o->values) {
                    int v = 0;
                    int ok = 0;
                    while (o->values[v].name) {
                        if (strncmp(o->values[v].name, o->arg, strlen(o->arg)) == 0 ||
                                strncmp(o->values[v].intout, o->arg, strlen(o->arg)) == 0) {
                            free(o->arg);
                            o->arg = strdup(o->values[v].intout);
                            ok = 1;
                            break;
                        }
                        v++;
                    }

                    if (!ok) {
                        fprintf(stderr, "Values for '%s' are:\n", o->name);
                        v = 0;
                        while (o->values[v].name) {
                            fprintf(stderr, "   %s\n", o->values[v].name);
                            if (strcmp(o->values[v].name, o->values[v].intout) != 0) {
                                fprintf(stderr, "   %s\n", o->values[v].intout);
                            }
                            v++;
                        }
                        exit(1);
                    }
                }
                if (o->intout) {
                    printf("INTOUT %s => %s\n", o->intout, o->arg);
                }
                break;

            case OPTION_REAL:

                k = 0;
                q = p = o->arg;
                while (*p) {
                    if (*p == '/') {
                        *p = 0;
                        if (k < MAX) {
                            o->reals[k++] = atof(q);
                        }
                        *p = '/';
                        q = p + 1;
                    }
                    p++;
                }

                if (k < MAX && q != p) {
                    o->reals[k++] = atof(q);
                }

                if (k != o->count) {
                    usage(argv[0]);
                }
                if (o->intout) {
                    printf("INTOUT %s =>", o->intout);
                    for (k = 0; k < o->count; k++) {
                        printf(" %g", o->reals[k]);
                    }
                    printf("\n");
                }

                break;

            case OPTION_INT:
                k = 0;
                q = p = o->arg;
                while (*p) {
                    if (*p == '/') {
                        *p = 0;
                        if (k < MAX) {
                            o->ints[k++] = atol(q);
                        }
                        *p = '/';
                        q = p + 1;
                    }
                    p++;
                }

                if (k < MAX && q != p) {
                    o->ints[k++] = atol(q);
                }

                if (k != o->count) {
                    usage(argv[0]);
                }
                if (o->intout) {
                    printf("INTOUT %s =>", o->intout);
                    for (k = 0; k < o->count; k++) {
                        printf(" %d", o->ints[k]);
                    }
                    printf("\n");
                }

                break;

            case OPTION_BOOL:
                o->ints[0] = atof(o->arg) ? 1 : 0;
                if (o->intout) {
                    printf("INTOUT %s => %d\n", o->intout, o->ints[0]);
                }
                break;
            }

            if (o->intout) {
                e = intout_(o->intout, o->ints, o->reals, o->arg, strlen(o->intout), strlen(o->arg));
                if (e) {
                    fprintf(stderr, "INTOUT returns %d\n", e);
                    exit(1);
                }
            }

        } else {
            if ( n < 2) {
                paths[n++] = argv[i];
            } else {
                usage(argv[0]);
            }
        }

    }

    if (n != 2) {
        usage(argv[0]);
    }

    f = fopen(paths[0], "r");
    if (!f) {
        perror(paths[0]);
        exit(1);
    }

    g = fopen(paths[1], "w");
    if (!g) {
        perror(paths[1]);
        exit(1);
    }

    if (is_set("vod2uv")) {
        fprintf(stderr, "%s: wind not yet supported\n", argv[0]);
        exit(1);
    }

    if (is_set("wind")) {
        grib_handle *u, *v;
        int u_buflen = 0;
        char *u_buffer = NULL;
        int v_buflen = 0;
        char *v_buffer = NULL;
        n = 0;
        printf("%s: wind interpolation\n", argv[0]);
        while ( (u = grib_handle_new_from_file(0,  f, &err)) != 0) {

            v = grib_handle_new_from_file(0,  f, &err);
            if (err) {
                break;
            }

            if (!v) {
                fprintf(stderr, "%s: cannot read V field\n", argv[0]);
                exit(1);
            }

            int e;
            const void *u_message = 0;
            size_t u_message_length = 0;
            fortint u_inlen, u_outlen;

            if ( (err = grib_get_message(u , &u_message, &u_message_length) ) != 0) {
                fprintf(stderr, "%s: %s\n", paths[0], grib_get_error_message(err));
                exit(1);
            }

            const void *v_message = 0;
            size_t v_message_length = 0;
            fortint v_inlen, v_outlen;

            if ( (err = grib_get_message(v , &v_message, &v_message_length) ) != 0) {
                fprintf(stderr, "%s: %s\n", paths[0], grib_get_error_message(err));
                exit(1);
            }

            if (u_buflen < u_message_length * 4) {
                if (u_buffer) {
                    free(u_buffer);
                }
                u_buflen = u_message_length * 4;
                u_buffer = malloc(u_buflen);
                if (u_buffer == NULL) {
                    fprintf(stderr, "%s: failed to allocate %d bytes\n", argv[0], u_buflen);
                    exit(1);
                }
            }

            if (v_buflen < v_message_length * 4) {
                if (v_buffer) {
                    free(v_buffer);
                }
                v_buflen = v_message_length * 4;
                v_buffer = malloc(v_buflen);
                if (v_buffer == NULL) {
                    fprintf(stderr, "%s: failed to allocate %d bytes\n", argv[0], v_buflen);
                    exit(1);
                }
            }

            u_inlen = u_message_length;
            u_outlen = u_buflen;
            v_inlen = v_message_length;
            v_outlen = v_buflen;

            if (is_set("hirlam12")) {
                func = "HIRLAMW12";
                e = hirlamw(u_message, v_message, u_inlen, u_buffer, v_buffer, &u_outlen, 1);
            }  else if (is_set("hirlam4")) {
                func = "HIRLAM4W";
                e = hirlamw(u_message, v_message, u_inlen, u_buffer, v_buffer, &u_outlen, 0);
            } else  {
                func = "INTVECT2";
                e = intvect2_(u_message, v_message, &u_inlen, u_buffer, v_buffer, &u_outlen);
                v_outlen = u_outlen;
            }
            if (e != 0) {
                fprintf(stderr, "%s: %s returns %d\n", argv[0], func, e);
                exit(1);
            }

            if (u_outlen == 0) {
                /* No iterpolation */
                printf("%s: no interpolation\n", argv[0]);
                if (fwrite(u_message, 1, u_inlen, g) != u_inlen) {
                    perror(paths[1]);
                    exit(1);
                }
                if (fwrite(v_message, 1, v_inlen, g) != v_inlen) {
                    perror(paths[1]);
                    exit(1);
                }
            } else {
                if (fwrite(u_buffer, 1, u_outlen, g) != u_outlen) {
                    perror(paths[1]);
                    exit(1);
                }
                if (fwrite(v_buffer, 1, v_outlen, g) != v_outlen) {
                    perror(paths[1]);
                    exit(1);
                }
            }

            n++;
            n++;
            grib_handle_delete(u);
            grib_handle_delete(v);
        }

    } else {
        grib_handle *h;
        int buflen = 0;
        char *buffer = NULL;


        n = 0;
        while ( (h = grib_handle_new_from_file(0,  f, &err)) != 0) {

            int e;
            const void *message = 0;
            size_t message_length = 0;
            fortint inlen, outlen;

            if ( (err = grib_get_message(h , &message, &message_length) ) != 0) {
                fprintf(stderr, "%s: %s\n", paths[0], grib_get_error_message(err));
                exit(1);
            }


            if (buflen < message_length * 4) {
                if (buffer) {
                    free(buffer);
                }
                buflen = message_length * 4;
                buffer = malloc(buflen);
                if (buffer == NULL) {
                    fprintf(stderr, "%s: failed to allocate %d bytes\n", argv[0], buflen);
                    exit(1);
                }
            }

            inlen = message_length;
            outlen = buflen;

            if (is_set("hirlam12")) {
                func = "HIRLAM12";
                e = hirlam(message, inlen, buffer, &outlen, 1);
            }  else if (is_set("hirlam4")) {
                func = "HIRLAM4";
                e = hirlam(message, inlen, buffer, &outlen, 0);
            } else  {
                func = "INTF2";
                e = intf2(message, &inlen, buffer, &outlen);
            }
            if (e != 0) {
                fprintf(stderr, "%s: %s returns %d\n", argv[0], func, e);
                exit(1);
            }

            if (outlen == 0) {
                /* No iterpolation */
                printf("%s: no interpolation\n", argv[0]);
                if (fwrite(message, 1, inlen, g) != inlen) {
                    perror(paths[1]);
                    exit(1);
                }
            } else {
                if (fwrite(buffer, 1, outlen, g) != outlen) {
                    perror(paths[1]);
                    exit(1);
                }
            }

            n++;
            grib_handle_delete(h);
        }


    }

    if (err) {
        fprintf(stderr, "%s: %s\n", paths[0], grib_get_error_message(err));
        exit(1);
    }


    if (fclose(f)) {
        perror(paths[0]);
        exit(1);
    }

    if (fclose(g)) {
        perror(paths[1]);
        exit(1);
    }
    if (n == 0) {
        fprintf(stderr, "%s: No GRIB found\n", paths[0]);
        exit(1);
    }

    printf("%d GRIB(s) written to %s\n", n, paths[1]);

    return 0;
}

