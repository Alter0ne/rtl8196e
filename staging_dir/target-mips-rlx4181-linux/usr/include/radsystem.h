#ifndef INC_radsystemh
#define INC_radsystemh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radsystem.h
 
  PURPOSE:
        Provide system initialization and process bookkeeping.
 
  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        02/06/2004      M.S. Teel       0               Original
 
  NOTES:
        These routines initialize and destroy radlib system resources.
        A radlib "system" is defined uniquely by a system ID, a value
        in the range 0-255.  Each radlib "system" has its own set of
        buffers, semaphores, Queue Groups, and processes.  System IDs
        must be defined uniquely and globally for each group of processes
        which make up a radlib "system".
        
        Assumes 
 
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

/*  ... System include files
*/
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <time.h>


/*  ... Library include files
*/
#include <radsysdefs.h>
#include <radsemaphores.h>
#include <radshmem.h>
#include <radmsgLog.h>
#include <radtimeUtils.h>


/*  ...HIDDEN, don't use
*/

#define MAX_RADLIB_SYSTEMS      256
#define RADLIB_SYSTEM_NAME      "sysInit"


#define _SECONDS_IN_MINUTE              60
#define _SECONDS_IN_HOUR                3600
#define _SECONDS_IN_DAY                 86400
#define _SECONDS_IN_MONTH               (UINT)(30.4*_SECONDS_IN_DAY)
#define _SECONDS_IN_YEAR                (365*_SECONDS_IN_DAY)


extern UINT     KEY_MSGQ;
extern UINT     KEY_SEMAPHORES;
extern UINT     KEY_BUFFERS_SHMEM;
extern UINT     KEY_CONFIG_SHMEM;


typedef struct
{
    int             numprocs;
    UINT            keyBase;
    ULONG           startTime;
    ULONGLONG       startTimeMS;
}
SYSTEM_INST;

typedef struct
{
    int             numAttached;
    SYSTEM_INST     systems[MAX_RADLIB_SYSTEMS];
}
SYSTEM_SHARE;

typedef struct
{
    int             semId;
    int             shmId;
    SYSTEM_SHARE    *share;
}
SYSTEM_CB;


/*  ... END HIDDEN
*/

/*  ... API definitions
*/
extern UINT     KEY_USER_START;
extern UINT     KEY_USER_STOP;


/*  ... radSystemInit: initialize a unique radlib system and/or register
    ...     the calling process within an existing radlib system;
    ... ALL processes within a radlib system must call this method
    ...     BEFORE calling radProcessInit;
    ... if this is the first process in the system to call this, system
    ...     facilities will be created and initialized, otherwise this
    ...     will just register the process with existing system facilities;
    ... care must be taken that all processes within a given radlib system
    ...     use the same unique systemID;
    ... returns OK or ERROR
*/
extern int radSystemInit (UCHAR systemID);


/*  ... radSystemInitBuffers: initialize a unique radlib system and/or
    ... register the calling process within an existing radlib system;
    ... ALL processes within a radlib system must call this method
    ...     BEFORE calling radProcessInit;
    ... if this is the first process in the system to call this, system
    ...     facilities will be created and initialized, otherwise this
    ...     will just register the process with existing system facilities;
    ... care must be taken that all processes within a given radlib system
    ...     use the same unique systemID;
    ... if "bufferCounts" is non-NULL, it will be used as the initialization
    ...     array for the system buffers, if NULL, the defaults will be
    ...     used (see radsysdefs.c for the default array and as an example,
    ...     and radsysdefs.h for the min, max and number of sizes 
    ...     definitions);
    ... returns OK or ERROR
*/
extern int radSystemInitBuffers (UCHAR systemID, int *bufferCounts);


/*  ... radSystemExit: deregister the calling process from a unique radlib
    ...     system and destroy all facilities if it is the last process
    ...     left in the radlib system;
    ... care must be taken that all processes within a given radlib system
    ...     use the same unique systemID
*/
extern void radSystemExit (UCHAR systemID);



/*  ... Provide system up time utilities
*/

/*  ... radSystemGetUpTimeMS: return the number of milliseconds since
    ...     radSystemInit was first called with this "systemID";
    ... care must be taken that all processes within a given radlib system
    ...     use the same unique systemID;
    ... returns number of milliseconds
*/
extern ULONG radSystemGetUpTimeMS (UCHAR systemID);


/*  ... radSystemGetUpTimeSEC: return the number of seconds since
    ...     radSystemInit was first called with this "systemID";
    ... care must be taken that all processes within a given radlib system
    ...     use the same unique systemID;
    ... returns number of seconds
*/
extern ULONG radSystemGetUpTimeSEC (UCHAR systemID);


/*  ... radSystemGetUpTimeSTR: return a string of the form:
    ...     "Y years, M months, D days, H hours, m minutes, S seconds"
    ...     which represents the time since radSystemInit was first called 
    ...     with this "systemID";
    ... care must be taken that all processes within a given radlib system
    ...     use the same unique systemID;
    ... returns a pointer to a static buffer containing the time string;
    ... Note: this call is not reentrant and successive calls will
    ...     overwrite the static buffer contents
*/
extern char *radSystemGetUpTimeSTR (UCHAR systemID);


#ifdef __cplusplus
}
#endif
#endif

