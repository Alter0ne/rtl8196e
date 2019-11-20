#ifndef INC_radsysdefsh
#define INC_radsysdefsh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radsysdefs.h
 
  PURPOSE:
        Define system definitions.
 
  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        04/01/03        M.S. Teel       0               Initial
        02/08/07        M.S. Teel       1               Add 64-bit support
 
  NOTES:
        
 
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/sem.h>


/*  ... API macros
*/

#define TRUE                    1
#define FALSE                   0

#ifndef NULL
#define NULL                    ((void *)0)
#endif

#define OK                      0
#define ERROR                   -1
#define ERROR_ABORT             -2
#define TIMEOUT                 -3
#define BUSY                    -4
#define DONE                    1


#ifndef MAX
#define MAX(x,y)                ((x >= y) ? x : y)
#endif

#ifndef MIN
#define MIN(x,y)                ((x <= y) ? x : y)
#endif



/*  ... typedefs
*/
typedef unsigned char       UCHAR;
typedef unsigned short      USHORT;
typedef unsigned int        UINT;
typedef unsigned long       ULONG;
typedef unsigned long long  ULONGLONG;
typedef unsigned char       BOOL;



/*  ... we must define this per the man page on semctl
*/
#if defined(_SEM_SEMUN_UNDEFINED)
/*  ... according to X/OPEN we must define it ourselves
*/
union semun
{
    int                 val;
    struct semid_ds     *buf;
    USHORT              *array;
    struct seminfo      *__buf;
};
#endif


/*  ... define max possible semaphores for the system
    ... BEWARE - BSDs won't be happy with more than 32 or so!!!
*/
#define MAX_SEMAPHORES              16



/*  ... define system buffer values -
    ... Note: buffer sizes increase by powers of 2, i.e.,
    ... 64,128,256,512,1024,2048,4096 for the defaults below
    ... Note: custom buffer count arrays should use 
    ... SYS_BUFFER_NUMBER_OF_SIZES for the array size
*/
#define SYS_BUFFER_SMALLEST_SIZE    64
#define SYS_BUFFER_LARGEST_SIZE     8192
#define SYS_BUFFER_NUMBER_OF_SIZES  8

extern int sysBufferCounts[];


/*  ... define an internal message to call callback functions
    ... in process context (used for timers and events)
*/
typedef struct
{
    USHORT          length;
    int             msgType;
    void            (*callback) (void *parm);
    void            *parm;
} SYS_CALLBACK_MSG;


/*  ... define generic state machine stimulus types here
*/
typedef enum
{
    STIM_DUMMY          = 1,
    STIM_QMSG,
    STIM_EVENT,
    STIM_TIMER,
    STIM_IO
} STIM_TYPES;


/*  ... define generic state machine stimulus structure here
*/
typedef struct
{
    int         type;
    char        *srcQueueName;
    UINT        msgType;
    void        *msg;
    UINT        length;
    UINT        eventsRx;
    UINT        eventData;
    int         timerNumber;
    int         iofd;
} STIM;


/*  ... define ALL semaphore indexes here (they are global)
    ... user defined semaphore indexes should start after SEM_INDEX_USER_START
*/
enum SemIndexes
{
    SEM_INDEX_SYSTEM        = 1,                /* system sems defined here */
    SEM_INDEX_BUFFERS       = 2,
    SEM_INDEX_PROCLIST      = 3,
    SEM_INDEX_MSGQ          = 4,
    SEM_INDEX_CONFIG        = 5,

    SEM_INDEX_USER_START    = 10,               /* user sems begin here */


    SEM_INDEX_USER_END      = MAX_SEMAPHORES-1,

    SEM_INDEX_MAX           = MAX_SEMAPHORES-1
};


/*  ... define the system shared memory keys here
*/
#define RADLIB_SYSTEM_SEM_KEY       0x0FF00001
#define RADLIB_SYSTEM_SHM_KEY       0x0FF00002



/*  ... define all queue groups here, regardless of their target
*/
enum QueueGroups
{
    QUEUE_GROUP_ALL         = 1                 /* don't dork with this one */
};

#ifdef __cplusplus
}
#endif
#endif

