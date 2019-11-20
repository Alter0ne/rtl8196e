#ifndef INC_radbuffersh
#define INC_radbuffersh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radbuffers.h
 
  PURPOSE:
        Provide the inter-task message buffers utility.
 
  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        9/26/99         M.S. Teel       0               Original
        3/21/01         M.S. Teel       1               Port to Linux
 
  NOTES:
        semProcessInit and msgLogInit have been called...
 
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

#include <sys/ipc.h>
#include <sys/shm.h>

#include <radsysdefs.h>
#include <radsemaphores.h>
#include <radshmem.h>
#include <radmsgLog.h>



/*  ... HIDDEN, don't use
*/
#define MAX_BFR_SIZES       10


typedef struct bufferHdrTag
{
    UINT    next;
    USHORT  sizeIndex;
    USHORT  allocated;
}__attribute__ ((packed)) BFR_HDR;

typedef struct bufferShareTag
{
    int     numSizes;
    ULONG   sizes[MAX_BFR_SIZES];
    int     count[MAX_BFR_SIZES];
    UINT    pool[MAX_BFR_SIZES];
    int     allocCount;
} BUFFER_SHARE;

typedef struct bufferWorkTag
{
    SHMEM_ID        shmId;
    BUFFER_SHARE    *share;
} BUFFER_WKTAG;



/*  ... END HIDDEN
*/


/*  ... External references
*/

/*  ... Called from process initialization
*/
extern int radBuffersInit
(
    int         minBufferSize,
    int         maxBufferSize,
    int         *numberOfEachSize
);

/*  ... request a message buffer of size "size"
    ... returns the pointer or NULL
*/
extern void *radBufferGet
(
    int         size
);

/*  ... release a message buffer
    ... returns OK or ERROR
*/
extern int radBufferRls
(
    void        *buffer
);

/*  ... call this to get a ptr based on a buffer offset
    ... used by queue.h
*/
extern void *radBufferGetPtr
(
    UINT        offset
);

/*  ... call this to get an offset based on a buffer
    ... used by queue.h
*/
extern UINT radBufferGetOffset
(
    void        *buffer
);

/*  ... dettach from shmem
*/
extern void radBuffersExit
(
    void
);

/*  ... dettach from shmem and mark to be destroyed
*/
extern void radBuffersExitAndDestroy
(
    void
);

extern ULONG radBuffersGetTotal
(
    void
);

extern ULONG radBuffersGetAvailable
(
    void
);

extern void radBuffersDebug (void);

#ifdef __cplusplus
}
#endif
#endif
