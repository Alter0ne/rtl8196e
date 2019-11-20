#ifndef INC_radsysutilsh
#define INC_radsysutilsh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------

  FILENAME:
        radsysutils.h

  PURPOSE:
        Provide system utilities.

  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        12/24/01        M.S. Teel       0               Original

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
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <signal.h>
#include <time.h>


/*  ... Library include files
*/
#include <radsysdefs.h>


/*  ...HIDDEN, don't use
*/

/*  ... END HIDDEN
*/

/*	... start the real interval timer to deliver SIGALRM after msecs 
	... milliseconds (probably rounded to the nearest 10 ms)
    ... returns OK or ERROR
*/
extern int radUtilsSetIntervalTimer (unsigned long msecs);


/*	... get the value of the real interval timer in milliseconds
	... (probably rounded to the nearest 10 ms)
    ... returns milliseconds or ERROR
*/
extern int radUtilsGetIntervalTimer (void);


/*	... allow a signal to be received (see signal.h)
	... returns OK or ERROR
*/
extern int radUtilsEnableSignal (int signum);


/*	... disallow a signal to be received (see signal.h)
	... returns OK or ERROR
*/
extern int radUtilsDisableSignal (int signum);


/*  ... make calling process a daemon;
    ... the calling process forks a new daemon child then exits;
    ... new process does a "chdir" to 'workingDirectory' if not NULL;
    ... returns the new process pid or ERROR
*/
extern int radUtilsBecomeDaemon (const char *workingDirectory);


/*  ... provide a sleep function with ms resolution;
*/
extern void radUtilsSleep (int msDuration);

#ifdef __cplusplus
}
#endif
#endif

