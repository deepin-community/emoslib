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

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>

#include <sys/stat.h>
#include <fcntl.h>
#include <sys/time.h>

#include <sys/sem.h>

#include "sharedlib.h"

/* set the shared memory debug output according to env var */
int sharedlib_dbg()
{
    char* var = getenv("EMOSLIB_SHM_DEBUG");
    return (var != NULL) && (strlen(var) != 0) ? 1 : 0;
}

/* 10 MB */

#define BUFFER_SIZE 10485760
#define MAGIC       1234567890

struct sembuf _lock[] = {
    { 0, 0,  SEM_UNDO }, /* test */
    { 0, 1,  SEM_UNDO }, /* lock */
};

struct sembuf _unlock[] = {
    { 0, -1, SEM_UNDO }, /* ulck */
};

#define INFO_PATH 1024

struct info {
    int ready;
    int magic;
    char path[INFO_PATH];
};



void *share_file(const char* file) {
    char message[1024];
    char path[1024];

    struct  stat64 s;
    struct  stat64 s2;
    int fd = -1;
    int err = 0;
    void *ptr = NULL;
    int shmid = 0;
    key_t key;
    char *addr;
    struct timeval start, end, diff;
    double time;
    int sem;
    int loadfile = 1;
    int locked = 0;
    int page_size = getpagesize();
    struct info *nfo;
    int semcount = 0;

    if(page_size < 0)
    {
        fprintf(stderr,"ERR: sharedmem:get_page_size faile\n");
        return NULL;
    }

    if(strlen(file) + 1 > INFO_PATH)
    {
        fprintf(stderr,"ERR: sharedmem:path too long(%lu) max is %d\n",strlen(file), INFO_PATH);
        return NULL;
    }

    if(strlen(file) + 1 > sizeof(path))
    {
        fprintf(stderr,"ERR: sharedmem:path too long(%lu) max is %lu\n",strlen(file), sizeof(path));
        return NULL;
    }

    if(realpath(file,path) == 0) {
        sprintf(message,"ERR: sharedmem:realpath(%s)",file);
        err = -1;
        goto error;
    }

    if (sharedlib_dbg())
        fprintf(stdout,"sharedmem: sharing %s\n",path);

    if(sizeof(s.st_size) < 8) {
        fprintf(stderr,"ERR: sharedmem:stat.st_size(%lu) is too small for 64bits files\n",sizeof(s.st_size));
        return NULL;
    }


    key = ftok(path,1);

    if(key ==  (key_t)-1) {
        sprintf(message,"ERR: sharedmem:ftok(%s)",path);
        err = -1;
        goto error;
    }


    if((sem = semget(key,1,IPC_CREAT|0600)) < 0)
    {
        sprintf(message,"ERR: sharedmem:semget(%s)",path);
        err = -1;
        goto error;
    }

    if (sharedlib_dbg()) gettimeofday( &start, NULL );

    while (semcount < 30) {
    if(semop(sem,_lock, 2 ) < 0) {

        semcount++;
        sprintf(message,"ERR: sharedmem:semop:lock(%s)",path);
        if (semcount >= 30) {
        err = -1;
        goto error;
        }
        else
        {
           sprintf(message,"WARNING: %d sharedmem:semop:lock(%s)",semcount,path);
           perror(message);
           sleep(2);
           semcount++;
        }
    }
    else
    {
        break;
    }
    }
    locked = 1;

    if (sharedlib_dbg())
    {
        gettimeofday( &end, NULL );

        diff.tv_sec  = end.tv_sec  - start.tv_sec;
        diff.tv_usec = end.tv_usec - start.tv_usec;

        if (diff.tv_usec < 0)
        {
            diff.tv_sec--;
            diff.tv_sec--;
            diff.tv_usec += 1000000;
        }
        time = (double)diff.tv_sec + ((double)diff.tv_usec / 1000000.);

        fprintf( stdout, "sharedmem:semop:lock wait %g secs\n", time);
    }

#ifdef O_LARGEFILE
    if((fd = open(path,O_RDONLY | O_LARGEFILE))  < 0)
#else
    if((fd = open(path,O_RDONLY))  < 0)
#endif
    {

        sprintf(message,"ERR: sharedmem:open(%s)",path);
        err = -1;
        goto error;
    }

    if(stat64(path,&s))
    {
        sprintf(message,"ERR: sharedmem:stat64(%s)",path);
        err = -1;
        goto error;
    }

    size_t shmsize = ((s.st_size + page_size-1)/page_size)*page_size + sizeof(struct info) ;

    if (sharedlib_dbg())
        fprintf(stdout,"sharedmem: calling shmget for key %d of size %ld with page_size %d \n",key, shmsize, page_size);
    if (sharedlib_dbg())
        fprintf(stdout,"sharedmem: sizeof(struct info)=%ld \n", sizeof(struct info));


    if((shmid = shmget(key, shmsize ,IPC_CREAT|0600)) < 0)
    {
        sprintf(message,"ERR: sharedmem:shmget(%s) key = %d shmsize = %ld ",path, key, shmsize);
        err = -1;
        goto error;
    }
    if (sharedlib_dbg())
        fprintf(stdout,"sharedmem: shmget for key %d returns shmid=%d \n",key, shmid);


#ifdef SHM_PAGESIZE
    {

    /* Use 64K pages to back the shared memory region */
    size_t shm_size;
    struct shmid_ds shm_buf = { 0 };
    psize_t psize_64k;
    psize_64k = 64 * 1024;

    shm_buf.shm_pagesize = psize_64k;
    if (shmctl(shmid, SHM_PAGESIZE, &shm_buf))
    {
        /*perror("shmctl(SHM_PAGESIZE) failed");*/
    }
    }

#endif

    /* attach shared memory */

    ptr = shmat( shmid, NULL, 0 );
    if (ptr == (void*)-1) {
        sprintf(message,"sharedmem:shmat(%s)",path);
        err = -1;
        goto error;
    }

    addr = (char*)ptr;
    if (sharedlib_dbg())
        fprintf(stdout,"sharedmem: shmat for ptr %p\n",(void *) ptr);

    nfo  = (struct info*)(addr + (((s.st_size + page_size-1)/page_size)*page_size));

    if(nfo->ready) {
        loadfile = 0;
        if(nfo->magic != MAGIC)
        {
            sprintf(message,"ERR: sharedmem:check: bad magic %d\n",nfo->magic);
            err = -1;
            goto error;
        }

        if(strcmp(nfo->path,path) != 0)
        {
            sprintf(message,"ERR: sharedmem:check: invalid path [%s]\n",nfo->path);
            err = -1;
            goto error;
        }
    }


    if(loadfile) {

        s2.st_size =  s.st_size;

        if (sharedlib_dbg()) gettimeofday( &start, NULL );

        while(s.st_size > 0)
        {
            size_t len = s.st_size > BUFFER_SIZE ? BUFFER_SIZE : s.st_size;
            if(read(fd, addr, len) != len) {
                sprintf(message,"ERR: sharedmem:read(%s)",path);
                err = -1;
                goto error;
            }
            s.st_size -= len;
            addr      += len;
        }

        if (sharedlib_dbg())
        {
            gettimeofday( &end, NULL );

            diff.tv_sec  = end.tv_sec  - start.tv_sec;
            diff.tv_usec = end.tv_usec - start.tv_usec;

            if (diff.tv_usec < 0)
            {
                diff.tv_sec--;
                diff.tv_usec += 1000000;
            }
            time = (double)diff.tv_sec + ((double)diff.tv_usec / 1000000.);

            fprintf( stdout, "sharedmem:read %lld bytes in %g secs\n",s2.st_size, time);
        }

        nfo->magic = MAGIC;
        strcpy(nfo->path,path);
        nfo->ready = 1;
    }
    else
    {
        if (sharedlib_dbg())
        fprintf( stdout, "sharedmem:read file already loaded\n");
    }

    close(fd);

error:

    if(fd>0) {
        close(fd);
    }

    if(err) {
        perror(message);
        if(ptr) shmdt(ptr);
        ptr = NULL;

    }

    if(locked) {
    semcount = 0;
    while (semcount < 30) {
        if(semop(sem,_unlock,1) < 0)
        {
            if (semcount >= 30) {
                sprintf(message,"ERR: sharedmem:UNLOCK semop:unlock(%s)",path);
                perror(message);
                break;
            }
            else
            {
               sprintf(message,"WARNING: %d sharedmem:UNLOCK semop:unlock(%s)",semcount,path);
               perror(message);
               sleep(2);
               semcount++;
            }
       } else {
            break;
       }
   }

   }

    return ptr;
}

int release_shared_file(void *ptr) {
    int err = 0 ;
    err = shmdt(ptr);
    return err ;
}

int remove_shared_file(const char* file) {
    char message[1024];
    char path[1024];
    int err = 0;
    int shmid = 0;
    key_t key;
    int sem;

    if(strlen(file) + 1 > sizeof(path))
    {
        fprintf(stderr,"ERR: sharedmem:path too long(%lu) max is %lu\n",strlen(file), sizeof(path));
        return -1;
    }

    if(realpath(file,path) == 0) {
        sprintf(message,"ERR: sharedmem:realpath(%s)",file);
        err = -1;
        goto error;
    }

    key  = ftok(path,1);

    if(key ==  (key_t)-1) {
        sprintf(message,"ERR: sharedmem:ftok(%s)",path);
        err = -1;
        goto error;
    }

    if((shmid = shmget(key,0,0600)) < 0)
    {
        sprintf(message,"ERR: sharedmem:shmget(%s)",path);
        err = -1;
        goto error;
    }

    fprintf(stdout,"sharedmem: removing shared memory for %s\n",path);

    if(shmctl(shmid, IPC_RMID, NULL) < 0)
    {
        sprintf(message,"ERR: sharedmem:shmctl:IPC_RMID,(%s)",path);
        err = -1;
        goto error;
    }


    if((sem = semget(key,1,0600)) <0 )
    {
        sprintf(message,"ERR: sharedmem:semget(%s)",path);
        err = -1;
        goto error;
    }

    if(semctl(sem, 0, IPC_RMID, NULL) < 0)
    {
        sprintf(message,"ERR: sharedmem:semctl:IPC_RMID,(%s)",path);
        err = -1;
        goto error;
    }



error:



    if(err)
        perror(message);

    return err;

}
