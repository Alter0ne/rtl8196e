#ifndef INC_radproclisth
#define INC_radproclisth
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radproclist.h
 
  PURPOSE:
        Provide ordered process lists to start processes. 
 
  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        04/01/03        MS Teel         0               Original
 
  NOTES:
        semProcessInit must have been called...
 
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

/*  ... includes
*/
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

#include <radsysdefs.h>
#include <radsemaphores.h>
#include <radmsgLog.h>
#include <radlist.h>
#include <radprocutils.h>


/*  ... HIDDEN, don't use
*/

/*  ... macro definitions
*/
#define PROC_PARENT_NAME_LEN    64


/*  ... typedefs
*/
typedef struct
{
    NODE            node;
    int             priority;
    pid_t           pid;
    int             (*entry) (void *pargs);
    void            *args;
} PROC_DATA, *PROC_DATA_ID;


typedef struct
{
    char            pName [PROC_PARENT_NAME_LEN];
    int             hasStarted;
    SEM_ID          semId;
    RADLIST_ID      procList;
} PROC_LIST, *PROC_LIST_ID;


/*  ... END HIDDEN
*/


/*  ... methods
*/

/*  ... radPlistCreate
    ... create a new, empty process list;
    ... returns - PROC_LIST_ID or NULL
*/
extern PROC_LIST_ID radPlistCreate
(
    char            *parentName
);


/*  ... radPlistDestroy
    ... destroy an existing process list (will clean up nodes);
    ... plistId - the PROC_LIST_ID;
    ... returns - OK or ERROR
*/
extern int radPlistDestroy
(
    PROC_LIST_ID    plistId
);


/*  ... radPlistAdd
    ... add a new "entry" into an existing process list;
    ... plistId - the PROC_LIST_ID;
    ... entry - the entry point for the process;
    ... args - the process-specific args to pass to <entry>;
    ... priority - a value 1 - 100 indicating start priority
    ...            (1 is first, 100 is last);
    ... returns - OK or ERROR;
    ... Note: <entry> must call processInit and then must give the
    ...       synchronization semaphore by calling "radPlistProcessReady"
    ...       or the parent process will hang forever waiting on it!
*/
extern int radPlistAdd
(
    PROC_LIST_ID    plistId,
    int             (*entry) (void *pargs),
    void            *args,
    int             priority
);


/*  ... radPlistStart
    ... start all process entries in an existing process list, ordered
    ... by priority (1 -> 100);
    ... plistId - the PROC_LIST_ID;
    ... returns - OK or ERROR
*/
extern int radPlistStart
(
    PROC_LIST_ID    plistId
);


/*  ... radPlistGetNumberRunning
    ... get the number of running processes (after radPlistStart is called);
    ... plistId - the PROC_LIST_ID;
    ... returns - number of processes successfully started
*/
extern int radPlistGetNumberRunning
(
    PROC_LIST_ID    plistId
);


/*  ... radPlistExecByEntryPoint
    ... execute a user-supplied function for the process specified
    ... by <entry> (after radPlistStart is called);
    ... plistId - the PROC_LIST_ID;
    ... entry - the entry point of the process to target;
    ... execFunction - function to call with each process pid and
    ... the supplied void pointer (work area);
    ... data - void pointer to user-supplied data, passed to 
    ... "execFunction";
    ... returns - OK or ERROR
*/
extern int radPlistExecByEntryPoint
(
    PROC_LIST_ID    plistId,
    int             (*entry) (void *pargs),
    void            (*execFunction) (pid_t pid, void *data),
    void            *data
);


/*  ... radPlistExecAll
    ... execute a user-supplied function for each running process
    ... (after radPlistStart is called);
    ... plistId - the PROC_LIST_ID;
    ... execFunction - function to call with each process pid and
    ... the supplied void pointer (work area);
    ... data - void pointer to user-supplied data, passed to 
    ... "execFunction";
    ... returns - count of affected processes or ERROR
*/
extern int radPlistExecAll
(
    PROC_LIST_ID    plistId,
    void            (*execFunction) (pid_t pid, void *data),
    void            *data
);


/*  ... radPlistAddPid
    ... add a process entry to the list;
    ... this may be done when a process is started after
    ... "radPlistStart" has been called to keep the list up to date;
    ... plistId - the PROC_LIST_ID;
    ... pid - pid of process to add to list;
    ... returns - OK or ERROR
*/
extern int radPlistAddPid
(
    PROC_LIST_ID    plistId,
    pid_t           pid
);


/*  ... radPlistRemovePid
    ... remove a process entry from the list;
    ... this may be done when it exits to keep the list up to date;
    ... plistId - the PROC_LIST_ID;
    ... pid - pid of process to remove from list;
    ... returns - OK or ERROR if not found
*/
extern int radPlistRemovePid
(
    PROC_LIST_ID    plistId,
    pid_t           pid
);


/*  ... radPlistAddandStart
    ... add a process entry to the list and start it;
    ... this may be done when a process is started after
    ... "radPlistStart" has been called to keep the list up to date;
    ... plistId - the PROC_LIST_ID;
    ... entry - the entry point for the process;
    ... args - the process-specific args to pass to <entry>;
    ... returns - OK or ERROR;
*/
extern int radPlistAddandStart
(
    PROC_LIST_ID    plistId,
    int             (*entry) (void *pargs),
    void            *args
);


/*  ... radPlistProcessReady
    ... signal to the parent process that initialization is complete
    ... and it is safe to start the next process;
    ... Note: this cannot be called until "processInit" has been
    ...       successfully called by the new process.
    ... returns - OK or ERROR
*/
extern int radPlistProcessReady
(
    void
);

/*  ... radPlistFindByEntryPoint
    ... find an entry based on its entry point;
    ... Note: this cannot be called until "processInit" has been
    ...       successfully called by the new process.
    ... returns - the process pid if found or ERROR if not
*/
extern int radPlistFindByEntryPoint
(
    PROC_LIST_ID    plistId,
    int             (*entry) (void *pargs)
);


#ifdef __cplusplus
}
#endif
#endif

