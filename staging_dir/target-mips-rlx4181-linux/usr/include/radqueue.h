#ifndef INC_radqueueh
#define INC_radqueueh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radqueue.h
 
  PURPOSE:
        Provide the process message queue utility.
 
  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        9/28/99         M.S. Teel       0               Original
        3/23/01         M.S. Teel       1               Port to Linux
 
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

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <errno.h>

#include <radsysdefs.h>
#include <radlist.h>
#include <radshmem.h>
#include <radbuffers.h>
#include <radmsgLog.h>


/*  ... HIDDEN, don,t use!!!
*/

#define MAX_QUEUE_RECORDS       64
#define QUEUE_NAME_LENGTH       128


/*  ... define the global queue "database"
*/
typedef struct msgQueueRecordTag
{
    char            name[QUEUE_NAME_LENGTH+1];
    int             group;
    int             updateFlag;
} MSGQ_RECORD;

typedef struct msgQueueTableTag
{
    int             numRecs;
    MSGQ_RECORD     recs[MAX_QUEUE_RECORDS];
} MSGQ_TABLE;

/*  ... define the send queue list node
*/
typedef struct sendQueueNodeTag
{
    NODE            node;
    char            name[QUEUE_NAME_LENGTH+1];
    int             pipeFD;
    int             group;
} QSEND_NODE;

typedef struct QueueWork
{
    SHMEM_ID        tableId;
    MSGQ_TABLE      *queueTable;
    char            name[QUEUE_NAME_LENGTH+1];
    char            refName[QUEUE_NAME_LENGTH+1];
    int             reflectFD;
    int             pipeFD;
    RADLIST         sendQueues;
    pid_t           dummyPid;
    int             msgsRecv;
} T_QUEUE;

/*  ... END HIDDEN
*/

typedef T_QUEUE     *T_QUEUE_ID;


/*  ... define a msg header based on the unix msgbuf structure
*/
typedef struct msgHdrTag
{
    UINT            mtype;

    UINT            length;
    UINT            bfrOffset;
    char            name[QUEUE_NAME_LENGTH+1];
} QMSG_HDR;



/*  ... initialize the process queue global constructs for this process;
    ... if initFlag is TRUE, the global table will be initialized too;
    ... returns OK or ERROR
*/
extern int radQueueSystemInit (int initFlag);


/*  ... exit the process queue global constructs for this process;
    ... if destroy is TRUE, the global table will be destroyed;
    ... returns OK or ERROR
*/
extern void radQueueSystemExit (int destroy);


/*  ... create a msg queue for the calling process;
    ... list the caller's new queue in the global queue list
    ... with GROUP_ALL group
    ... returns T_QUEUE_ID or NULL
*/
extern T_QUEUE_ID radQueueInit
(
    char    *myName,
    int     startDummyProc
);


/*  ... radQueueExit: dettach from shm
*/
extern void radQueueExit
(
    T_QUEUE_ID      id
);


/*  ... radQueueExit: dettach from shm and mark for destroy
*/
extern void radQueueExitAndDestroy
(
    T_QUEUE_ID      id
);


/*  ... get the FD to use in select or poll calls
*/
extern int radQueueGetFD
(
    T_QUEUE_ID  id
);

/*  ... return my queue's name
*/
extern char *radQueueGetName
(
    T_QUEUE_ID  tqid,
    char        *store
);

/*  ... attach to an individual queue based on queue name
    ... so that messages can be sent to it
    ... returns OK or ERROR
*/
extern int radQueueAttach
(
    T_QUEUE_ID  tqid,
    char        *newName,
    int         group
);

/*  ... dettach from an individual queue based on queue name
*/
extern int radQueueDettach
(
    T_QUEUE_ID  tqid,
    char        *oldName,
    int         group
);

/*  ... add my queue to a group and add the group to my address list
    ... returns OK or ERROR
*/
extern int radQueueJoinGroup
(
    T_QUEUE_ID  tqid,
    int         groupNumber
);

/*  ... remove my queue from a group and remove a group from my address list
    ... returns OK or ERROR
*/
extern int radQueueQuitGroup
(
    T_QUEUE_ID  tqid,
    int         groupNumber
);


/*  ... read from msg queue
    ... populates (srcQueueKey, msg, length, msgType) and
    ... NOTE: msg will point to the system buffer when this call
    ... returns.  User MUST call bufferRls when done with buffer!
    ... RETURNS: TRUE if msg received, FALSE if queue is empty, ERROR if error
*/
extern int radQueueRecv
(
    T_QUEUE_ID  tqid,
    char        *srcQueueName,
    UINT        *msgType,
    void        **msg,
    UINT        *length
);


/*  ... write to a queue
    ... assumes sysBuffer is a valid pointer to a system buffer (unless length
    ... is zero, in which case a zero-length message is sent)
    ... system buffer ownership is transfered to the receiving queue
    ... returns OK, ERROR, or ERROR_ABORT if the dest queue is gone
    ... user should dettach from a dest on ERROR_ABORT!
*/
extern int radQueueSend
(
    T_QUEUE_ID  tqid,
    char        *destQueueName,
    UINT        msgType,
    void        *sysBuffer,
    UINT        length
);


/*  ... write to all queues in a group
    ... checks to make sure the group hasn't changed - if it has
    ... it refreshes the address list
    ... assumes sysBuffer is a valid pointer to a system buffer
    ... system buffer is released if this call returns OK
    ... returns OK or ERROR
*/
extern int radQueueSendGroup
(
    T_QUEUE_ID  tqid,
    int         destGroup,
    UINT        msgType,
    void        *sysBuffer,
    UINT        length
);


/*  ... determine if calling process is attached to the given queue name
    ... returns TRUE or FALSE
*/
extern int radQueueIsAttached
(
    T_QUEUE_ID  tqid,
    char        *queueName
);


#ifdef __cplusplus
}
#endif
#endif

