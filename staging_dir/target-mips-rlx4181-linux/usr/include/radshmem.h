#ifndef INC_radshmemh
#define INC_radshmemh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radshmem.h
 
  PURPOSE:
        Provide the shared memory utility.
 
  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        3/23/01         M.S. Teel       0               Original
 
  NOTES:
        Assumes semProcessInit has been called for this process.
 
  LICENSE:
        Copyright 2001-2005 Mark S. Teel. All rights reserved.

        Redistribution and use in source and binary forms, with or without 
        modification, are permitted provided that the following conditions 
        are met:

        1. Redistributions of source code must retain the above copyright 
           notice, this list of conditions and the following disclaimer.
        2. Redistributions in binary form must reproduce the above copyright 
           notice, this list of conditions and the following disclaimer in the 
           documentation and/or other materials provided with the distribution.

        THIS SOFTWARE IS PROVIDED BY Mark Teel ``AS IS'' AND ANY EXPRESS OR 
        IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
        WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
        DISCLAIMED. IN NO EVENT SHALL MARK TEEL OR CONTRIBUTORS BE LIABLE FOR 
        ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
        DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS 
        OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
        HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, 
        STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING 
        IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
        POSSIBILITY OF SUCH DAMAGE.
  
----------------------------------------------------------------------------*/

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <radsemaphores.h>
#include <radmsgLog.h>



/*  ... HIDDEN, don't use
*/

typedef struct shmemWorkTag
{
    SEM_ID      semId;
    int         shmId;
    void        *memory;
}
SHMEM_BLK;


/*  ... END HIDDEN
*/

typedef SHMEM_BLK   *SHMEM_ID;


/*  ... External references
*/

/*  ... Called from process to test a shared memory block for existence
    ... returns TRUE if it exists or FALSE if it doesn't
*/
extern int radShmemIfExist
(
    int         key             /* well-known key for this memory block */
);

/*  ... Called from process to get a shared memory block
    ... returns SHMEM_ID or NULL
*/
extern SHMEM_ID radShmemInit
(
    int         key,            /* well-known key for this memory block */
    int         semIndex,       /* well-known semaphore index */
    int         size
);

/*  ... get the pointer to the beginning of the shared block
    ... returns the pointer or NULL
*/
extern void *radShmemGet
(
    SHMEM_ID    id
);

/*  ... lock the block for exclusive access
    ... may block the caller if shared mem is already locked
*/
extern void radShmemLock
(
    SHMEM_ID    id
);

/*  ... unlock the block
*/
extern void radShmemUnlock
(
    SHMEM_ID    id
);


/*  ... call one or the other of the next 2 functions - NOT BOTH!
*/
/*  ... dettach from a shared block
*/
extern void radShmemExit
(
    SHMEM_ID    id
);

/*  ... exit and destroy the shared segment
    ... CAUTION: this destroys the shared memory block!
*/
extern void radShmemExitAndDestroy
(
    SHMEM_ID   id
);


#ifdef __cplusplus
}
#endif
#endif

