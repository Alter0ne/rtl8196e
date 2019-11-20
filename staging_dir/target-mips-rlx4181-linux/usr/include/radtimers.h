#ifndef INC_radTimersh
#define INC_radTimersh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------

  FILENAME:
        radtimers.h

  PURPOSE:
        Timer subsystem API definitions. Deliver notifications through
        an open file descriptor (typically a pipe).

  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        5/26/95         M.S. Teel       0               Original
        11/20/96        M.S. Teel       1               Port to pSos ARM
        9/28/99         M.S. Teel       2               Port to pSOS 2.50
        12/25/01        M.S. Teel       3               Port to linux
        04/04/2008      M.S. Teel       4               Rewrite using radlist

  NOTES:
        A valid process queue has been created.

  LICENSE:
        Copyright 2001-2008 Mark S. Teel. All rights reserved.

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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <errno.h>

#include <radsysdefs.h>
#include <radsysutils.h>
#include <radtimeUtils.h>
#include <radlist.h>
#include <radmsgLog.h>


/*  ... some typedefs that are used by HIDDEN and other users
*/
typedef struct timerTag     *TIMER_ID;
typedef struct timerListTag *TIMER_LIST_ID;


/*  ... HIDDEN, don't use
*/
struct timerTag
{
    NODE            node;
    ULONG           deltaTime;
    USHORT          pending;
    void            (*routine) (void *parm);
    void            *parm;
};

struct timerListTag
{
    int             notifyFD;
    int             noFreeTimers;
    RADLIST         freeList;
    RADLIST         pendingList;
    ULONGLONG       lastTick;
};

/*  ...END HIDDEN
*/

/*  ... calls to manage the LIST of timers (call only once during process init)
    ... returns OK or ERROR
*/
extern int radTimerListCreate
(
    int         noTimers,
    int         notifyDescriptor
);
extern void radTimerListDelete (void);


/*  ... these calls are for the use of a specific timer
*/
extern TIMER_ID radTimerCreate
(
    TIMER_ID    timer,
    void        (*routine) (void *parm),
    void        *parm
);
extern void radTimerDelete
(
    TIMER_ID    timer
);
extern void radTimerStart
(
    TIMER_ID    timer,
    ULONG       time
);
extern void radTimerStop
(
    TIMER_ID    timer
);
extern int radTimerStatus   /* TRUE if timer is running */
(
    TIMER_ID    timer
);
extern void radTimerSetUserParm
(
    TIMER_ID    timer,
    void        *newParm
);


extern int radTimerListDebug (void);


#ifdef __cplusplus
}
#endif
#endif

