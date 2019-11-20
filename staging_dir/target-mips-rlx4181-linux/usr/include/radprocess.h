#ifndef INC_radprocessh
#define INC_radprocessh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radprocess.h
 
  PURPOSE:
     Provide an asynchronous event-driven process control utility.
        Sets up the following utilities for a new process:
            Semaphores (semaphore.h)
            System Buffers (buffers.h)
            MsgLog (msgLog.h)
            Message Queue (queue.h)
            IO processing ("select")
            Timer List with 'numTimers' timers (timers.h)
            Event handler (events.h)
        Also provides wrappers for many "IPC" utilities.
 
  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        12/26/01        M.S. Teel       0               Original
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

/*  ... System include files
*/
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <string.h>
#include <signal.h>


/*  ... Library include files
*/
#include <radsysdefs.h>
#include <radsemaphores.h>
#include <radbuffers.h>
#include <radsystem.h>
#include <radqueue.h>
#include <radtimers.h>
#include <radevents.h>
#include <radtimeUtils.h>



/*  ... generic API definitions
*/
#define PROCESS_MAX_NAME_LEN    32



/*  !!!!!!!!!!!!!!!!!!!!!  HIDDEN, don't use  !!!!!!!!!!!!!!!!!!!!!!!!!
*/

#define PROC_TOTAL_IO_BLOCKS    16

enum ProcessFdTypes
{
    PROC_FD_PIPE_READ           = 0,        /* MUST be first two! */
    PROC_FD_PIPE_WRITE          = 1,
    PROC_FD_MSG_QUEUE           = 2,
    PROC_FD_USER_FIRST          = 3,
    PROC_FD_USER_LAST           = (PROC_TOTAL_IO_BLOCKS-1),
    PROC_FD_NUM_INDEXES         = PROC_TOTAL_IO_BLOCKS
};


typedef struct
{
    NODE            node;
    ULONG           id;
    void            (*msgHandler) (char *srcQueueName,
                                   UINT msgType,
                                   void *msg,
                                   UINT length,
                                   void *userData);
    void            *udata;
} PROC_MSGQ_HANDLER;

typedef struct processIoTag
{
    void            (*ioCallback) (int fd, void *userData);
    void            *userData;
} PROC_IO_BLK;


/*  ... define the process data; there will be one (static) instance
    ... of this structure for each process
*/
typedef struct
{
    char            name[PROCESS_MAX_NAME_LEN+1];
    pid_t           pid;
    fd_set          fdSet;
    int             fdMax;
    int             fds   [PROC_FD_NUM_INDEXES];
    PROC_IO_BLK     ioIDs [PROC_FD_NUM_INDEXES];
    T_QUEUE_ID      myQueue;
    long            defaultMsgQID;
    RADLIST         msgqHandlerList;

    // two flags for queue message handling
    int             keepMsgQBuffer;
    int             stopMsqQHandlerTraversal;

    EVENTS_ID       events;
    void            *userData;
    int             exitFlag;
} PROCESS_DATA;


/*  !!!!!!!!!!!!!!!!!!!!!!!!  END HIDDEN  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/

/*  ... define an ID for process I/O registration so user file descriptors
    ... can be added to the "select" fd_set used by "processWait"
*/
typedef int PROC_IO_ID;



/*  ... API calls
*/

/*  ... initialize process management; called once during process init;
    ... automatically sets up the following utilities for a new process:
    ...     Semaphores (semaphore.h)
    ...     System Buffers (buffers.h)
    ...     MsgLog (msgLog.h)
    ...     Message Queue (queue.h)
    ...     IO processing ("select")
    ...     Timer List with 'numTimers' timers (timers.h)
    ...     Event handler (events.h)
    ...
    ... 'processName' is used for logging;
    ... 'queueName' is used for the msg queue and events;
    ... 'queueDbKey' is used to attach to the proper shared queue groups
    ...     and queue database;
    ... 'queueDbSemIndex' is necessary for mutex protection on the
    ...     shared queue groups and queue database;
    ... 'numTimers' determines the max number of timers for this process;
    ... 'runAsDaemon' determines if process can have a controlling terminal
    ...     and if msgLog's go to stderr or not;
    ... 'messageHandler' is the callback for queue messages;
    ... 'eventHandler' is the callback for events;
    ... 'userData' is passed to the event and message callbacks;
    ...
    ... returns OK or ERROR
    ...
    ... NOTE: If a messageHandler wants to retain ownership of the system
    ...       buffer passed in as 'msg', it should call the new function
    ...       'radProcessQueueKeepBuffer' prior to returning; it is then the 
    ...       responsibility of the receiving process to release the buffer 
    ...       via radBufferRls when it is finished with the buffer.
*/
extern int radProcessInit
(
    char    *processName,
    char    *queueName,
    int     numTimers,
    int     runAsDaemon,
    void    (*messageHandler) (char *srcQueueName,
                               UINT msgType,
                               void *msg,
                               UINT length,
                               void *userData),
    void    (*eventHandler) (UINT eventsRx,
                             UINT rxData,
                             void *userData),
    void    *userData
);

extern void radProcessExit (void);


/*  ... use this to set the internal exit flag (i.e. inside SIG handler)
*/
extern void radProcessSetExitFlag (void);

/*  ... use this to get the internal exit flag (i.e. inside SIG handler)
    ... returns TRUE if set otherwise FALSE
*/
extern int radProcessGetExitFlag (void);


/*  ... wait for messages, timers, events, and user IO blocks in one call;
    ... should be the focal point of a process's main loop;
    ... 'timeout' (in milliseconds), if > 0, will cause this function to 
    ... return TIMEOUT even if no I/O triggered after 'timeout' milliseconds 
    ... (remember linux PC timers are accurate to 10 ms typically);
    ... returns OK, ERROR, or TIMEOUT if 'timeout' occurred
*/
extern int radProcessWait (int timeout);




/*  *** general process utilities ***
*/
/*  ... get the calling process's name;
    ... returns the pointer 'store', where the name is copied
*/
extern char *radProcessGetName (char *store);

/*  ... get the calling process's pid
*/
extern pid_t radProcessGetPid (void);

/*  ... get the calling process's notify fd
*/
extern int radProcessGetNotifyFD (void);



/*  *** process I/O utilities ***
*/
/*  ... register your file descriptor for "processWait" inclusion;
    ... 'ioCallback' will be executed if data or an error occurs on 'fd';
    ... 'userData' will be passed to 'ioCallback';
    ... returns PROC_IO_ID or ERROR
*/
extern PROC_IO_ID radProcessIORegisterDescriptor
(
    int         fd,
    void        (*ioCallback) (int fd, void *userData),
    void        *userData
);

/*  ... de-register your file descriptor for "processWait" inclusion;
    ... returns OK or ERROR
*/
extern int radProcessIODeRegisterDescriptor
(
    PROC_IO_ID  id
);

extern int radProcessIODeRegisterDescriptorByFd
(
    int     fd
);

/*  ... register STDIN for "processWait" inclusion;
    ... 'ioCallback' will be executed if data or an error occurs on STDIN;
    ... 'userData will be passed to 'ioCallback';
    ... returns PROC_IO_ID or NULL
*/
extern PROC_IO_ID radProcessIORegisterSTDIN
(
    void        (*ioCallback) (int fd, void *userData),
    void        *userData
);



/*  *** process signal utilities ***
*/
/*  ... initialize ALL signals (except SIGKILL and SIGSTOP) to be caught
    ... by 'defaultHandler';
    ... returns OK or ERROR
*/
extern int radProcessSignalCatchAll (void (*defaultHandler) (int signum));

/*  ... assign a specific 'handler' to a specific 'signum';
    ... returns OK or ERROR
*/
extern int radProcessSignalCatch (int signum, void (*handler) (int signum));

/*  ... set 'signum' back to the default system handler;
    ... returns OK or ERROR
*/
extern int radProcessSignalRelease (int signum);

/*  ... set 'signum' to be ignored;
    ... returns OK or ERROR
*/
extern int radProcessSignalIgnore (int signum);

/*  ... retrieve the current handler for 'signum';
    ... returns the function pointer or NULL
*/
extern void (*(radProcessSignalGetHandler (int signum))) (int);




/*  *** queue wrappers ***
*/
/*  ... return my queue's name
*/
extern char *radProcessQueueGetName
(
    char        *store
);

/*  ... return my queue's ID
*/
extern T_QUEUE_ID radProcessQueueGetID
(
    void
);

/*  ... attach to an individual queue based on queue name
    ... so that messages can be sent to it
    ... returns OK or ERROR
*/
extern int radProcessQueueAttach
(
    char        *newName,
    int         group
);

/*  ... dettach from an individual queue based on queue name
*/
extern int radProcessQueueDettach
(
    char        *oldName,
    int         group
);

/*  ... add my queue to a group and add the group to my address list
    ... returns OK or ERROR
*/
extern int radProcessQueueJoinGroup
(
    int         groupNumber
);

/*  ... remove my queue from a group and remove a group from my address list
    ... returns OK or ERROR
*/
extern int radProcessQueueQuitGroup
(
    int         groupNumber
);

/*  ... write to a queue;
    ... assumes sysBuffer is a valid pointer to a system buffer (unless length
    ... is zero, in which case a zero-length message is sent);
    ... system buffer ownership is transfered to the receiving queue;
    ... returns OK, ERROR, or ERROR_ABORT if the dest queue is gone;
    ... user should dettach from a dest on ERROR_ABORT!
*/
extern int radProcessQueueSend
(
    char        *destQueueName,
    UINT        msgType,
    void        *sysBuffer,
    UINT        length
);

/*  ... write to all queues in a group;
    ... checks to make sure the group hasn't changed - if it has
    ... it refreshes the address list;
    ... assumes sysBuffer is a valid pointer to a system buffer;
    ... system buffer is released if this call returns OK;
    ... returns OK or ERROR
*/
extern int radProcessQueueSendGroup
(
    int         destGroup,
    UINT        msgType,
    void        *sysBuffer,
    UINT        length
);

/*  ... determine if calling process is attached to the given queue name
    ... returns TRUE or FALSE
*/
extern int radProcessQueueIsAttached
(
    char        *queueName
);

/*  ... ONLY called from inside a message queue handler to indicate that 
    ... retention of the buffer is desired; if not called, the buffer will be
    ... released as usual after the last handler in the traversal list has 
    ... been invoked; this replaces the cheesy method used before to retain
    ... buffers by setting the first byte of the 'srcQueueName' to '0';
    ... It is the caller's responsibility to free the buffer after making this
    ... call to retain it
    ... Note: this will implicitly set the 'radProcessQueueStopHandlerList' 
    ...       condition as well
*/
extern void radProcessQueueKeepBuffer
(
    void
);

/*  ... if more than one message queue handler has been defined via the 
    ... 'radProcessQueuePrependHandler', then this function is used to indicate
    ... that the traversal of the handler list should stop when the calling 
    ... handler returns (i.e., the calling handler "matches" the message 
    ... received)
*/
extern int radProcessQueueStopHandlerList
(
    void
);

/*  ... prepend an additional message queue handler to the existing list of 
    ... message handlers; this allows other utilities/objects/etc. to insert
    ... a message handler to process specific utility messages without the 
    ... radlib application process having to have knowledge of those messages;
    ... this will not interrupt normal application message reception (although
    ... the utility/object doing the insertion should ensure that message types
    ... it is using do not conflict with the application message type 
    ... definitions); this is completely backward compatible with existing 
    ... radlib applications;
    ... 'userdata' will be passed to 'msgHandler';
    ... returns a unique identifier > 0 that can be used to remove the message 
    ... handler later (see 'radProcessQueueRemoveHandler') or ERROR
*/
extern long radProcessQueuePrependHandler
(
    void            (*msgHandler) (char *srcQueueName,
                                   UINT msgType,
                                   void *msg,
                                   UINT length,
                                   void *userData),
    void            *userdata
);

/*  ... remove a message queue handler from the existing list of message 
    ... handlers; 'handlerID must be a valid return value from a previous
    ... call to 'radProcessQueuePrependHandler' (this implies the default 
    ... handler provided in 'radProcessInit' CANNOT be removed)
    ... returns OK or ERROR
*/
extern long radProcessQueueRemoveHandler
(
    long            handlerID
);



/*  *** timer wrappers ***
*/
extern TIMER_ID radProcessTimerCreate
(
    TIMER_ID  timer,
    void      (*routine) (void *parm),
    void      *parm
);
extern void radProcessTimerDelete
(
    TIMER_ID  timer
);
extern void radProcessTimerStart
(
    TIMER_ID  timer,
    ULONG     time
);
extern void radProcessTimerStop
(
    TIMER_ID  timer
);
extern int radProcessTimerStatus   /* TRUE if timer is running */
(
    TIMER_ID  timer
);
extern void radProcessTimerSetUserParm
(
    TIMER_ID  timer,
    void      *newParm
);



/*  *** event wrappers ***
*/
/*  ... add new events to catch;
    ... returns OK or ERROR
*/
extern int radProcessEventsAdd
(
    UINT            newEvents
);

/*  ... remove events from event mask;
    ... returns OK or ERROR
*/
extern int radProcessEventsRemove
(
    UINT            removeEvents
);

/*  ... return the currently enabled events mask
*/
extern UINT radProcessEventsGetEnabled
(
    void
);

/*  ... send an event to another process by queue name;
    ... if radProcessInit was initialized and destName == NULL,
    ...     send the events to the calling process;
    ... returns OK or ERROR
*/
extern int radProcessEventsSend
(
    char            *destName,
    UINT            eventsToSend,
    UINT            data
);

#ifdef __cplusplus
}
#endif
#endif

