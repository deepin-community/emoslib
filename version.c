
#include "stdio.h"

#include "libemos_version.h"

#ifdef FORTRAN_LINKER_PGI
#define main_ MAIN_
#else
#define main_ main
#endif


int main_()
{
    printf("libemos\n");
    printf("version : %s\n", libemos_version_str() );
    printf("cycle : %d\n", libemos_version_int() );
    printf("sha1 : %s\n", libemos_git_sha1() );
    return 0;
}

