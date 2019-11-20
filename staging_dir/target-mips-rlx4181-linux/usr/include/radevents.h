#ifndef INC_radeventsh
#define INC_radeventsh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radevents.h
 
  PURPOSE:
        Provide the events processing utilities.
 
  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        3/23/01         M.S. Teel       0               Original
        12/27/01        M.S. Teel       1               Port to linux
 
  NOTES:
        Assumes process queues are present as this utilizes message queues
        to deliver events.
 
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
#include <sys/wait.h>
#include <string.h>
#include <signal.h>


/*  ... Library include files
*/
#include <radsysdefs.h>
#include <radbuffers.h>
#include <radmsgLog.h>
#include <radqueue.h>


/*  ... HIDDEN, don't use!
*/
/*  ... define the event message
*/
typedef struct
{
    UINT            events;
    UINT            data;
}
EVENTS_MSG;


struct eventsWorkTag
{
    T_QUEUE_ID      qid;
    UINT            mask;
    void            (*evtProcessor) (UINT eventsRx, UINT data, void *parm);
    void            *userParm;
};

/*  ... END HIDDEN
*/

typedef struct eventsWorkTag EVENTS;
typedef struct eventsWorkTag *EVENTS_ID;


/*  ... initialize - called once by each process
    ... evtCallback will be called when an event(s) is signaled
    ... returns the valid EVENTS_ID or NULL
*/
extern EVENTS_ID radEventsInit
(
    T_QUEUE_ID      queueId,
    UINT            initialEvents,
    void            (*evtCallback) (UINT eventsRx, UINT data, void *parm),
    void            *userParm
);

/*  ... exit this subsystem
*/
extern void radEventsExit
(
    EVENTS_ID       id
);

/*  ... add new events to catch
    ... returns OK or ERROR
*/
extern int radEventsAdd
(
    EVENTS_ID       id,
    UINT            newEvents
);

/*  ... remove events from event mask
    ... returns OK or ERROR
*/
extern int radEventsRemove
(
    EVENTS_ID       id,
    UINT            removeEvents
);

/*  ... return the current event mask
*/
extern UINT radEventsGetMask
(
    EVENTS_ID       id
);

/*  ... send an event to another process by queue name;
    ... if radProcessInit was initialized and destName == NULL,
    ...     send the events to the calling process;
    ... returns OK or ERROR
*/
extern int radEventsSend
(
    EVENTS_ID       id,
    char            *destName,
    UINT            eventsToSend,
    UINT            data
);


/*  ... this routine should be called with the result mask of a
    ... received event "message"
*/
extern int radEventsProcess
(
    EVENTS_ID       id,
    UINT            eventsRx,
    UINT            data
);


#ifdef __cplusplus
}
#endif
#endif

