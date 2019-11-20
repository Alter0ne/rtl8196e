#ifndef INC_radstatesh
#define INC_radstatesh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radstates.h
 
  PURPOSE:
        Provide the state machine utilities.
 
  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        11/15/99        M.S. Teel       0               Original
        3/22/01         M.S. Teel       1               Port to Linux
 
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

#include <radsysdefs.h>


#define STATE_MAX_STATES        32


/*  ... HIDDEN, don't use
*/

struct statesTag
{
    int         mstate;
    int         (*stateProc[STATE_MAX_STATES])
    (
        int     state,
        void    *stimulus,
        void    *userData
    );
    void        *userData;
};

/*  ... END HIDDEN
*/

typedef struct statesTag      STATES;
typedef struct statesTag      *STATES_ID;

/*  ... External references
*/

/*  ... initialize a state machine
    ... states MUST be 0-based
    ... returns the ID or NULL if error
*/
extern STATES_ID radStatesInit (void *saveData);

extern void radStatesExit (STATES_ID id);


/*  ... add a state processor with this call
    ... state MUST be < STATE_MAX_STATES
    ... NOTE:  handler must ALWAYS return the next state of the machine
    ... returns OK or ERROR
*/
extern int radStatesAddHandler
(
    STATES_ID       id,
    int             state,
    int             (*handler)
    (
        int     state,
        void    *stimulus,
        void    *userData
    )
);

/*  ... remove a processor (resets the handler to the stub handler)
*/
extern int radStatesRemHandler (STATES_ID id, int state);

/*  ... process a stimulus
*/
extern void radStatesProcess (STATES_ID id, void *stimulus);

/*  ... change the state of the machine
    ... returns OK or ERROR
*/
extern int radStatesSetState (STATES_ID id, int state);

/*  ... get the state of the machine
    ... returns the current state
*/
extern int radStatesGetState (STATES_ID id);

/*  ... reset all handlers to the stub
*/
extern void radStatesReset (STATES_ID id, void *saveData);

#ifdef __cplusplus
}
#endif
#endif

