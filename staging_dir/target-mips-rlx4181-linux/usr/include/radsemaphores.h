#ifndef INC_radsemaphoresh
#define INC_radsemaphoresh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radsemaphores.h
 
  PURPOSE:
        Provide the semaphore utilities.
 
  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        3/21/01         M.S. Teel       0               Original
 
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
#include <sys/sem.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#include <radsysdefs.h>



/*  ... HIDDEN, don't use!
*/

struct semWorkTag
{
    int             semId;
    int             status[MAX_SEMAPHORES];
};


/*  ... define the individual semaphore structure
*/
typedef struct semaphoreTag
{
    int             semId;
    int             semNumber;
}
SEMAPHORE;

/*  ... END HIDDEN
*/

typedef struct semaphoreTag     *SEM_ID;


/*  ... API calls
*/

/*  ... called globally once to destroy a semaphore set
    ... can be used at init to clean-up dangling semaphores
    ... CAUTION: this will destroy all semaphores!
*/
extern void radSemSetDestroy (void);


/*  ... called once during process init (returns OK or ERROR)
    ... create a new semaphore, or attach to an existing sem
*/
extern int radSemProcessInit (void);


/*  ... create (or attach a specific indexed semaphore)
    ... (returns SEM_ID or NULL)
    ... count indicates the initial value of the sem (1 for mutex)
    ... count < 0 will leave the semaphore's count value undisturbed
    ... (i.e. when it already exists)
*/
extern SEM_ID radSemCreate (int semIndex, int count);


/*  ... take (lock) a semaphore
*/
extern void radSemTake (SEM_ID id);


/*  ... give (unlock) a semaphore
*/
extern void radSemGive (SEM_ID id);


/*  ... bump the count on a semaphore
*/
extern void radSemGiveMultiple (SEM_ID id, int numToGive);


/*  ... test a semaphore to see if it is free (unlocked)
    ... returns TRUE if the semaphore was taken by this call (locked),
    ... FALSE if the semaphore is already locked
*/
extern int radSemTest (SEM_ID id);


/*  ... free a semaphore
*/
extern int radSemDelete (SEM_ID id);


/*  ... dump sem info to stdout
*/
extern void radSemDebug (void);

#ifdef __cplusplus
}
#endif
#endif

