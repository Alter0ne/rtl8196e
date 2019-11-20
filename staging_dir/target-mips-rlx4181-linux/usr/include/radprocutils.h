#ifndef INC_radprocutilsh
#define INC_radprocutilsh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radprocutils.h
 
  PURPOSE:
        Provide some general process utilities including process lists.
 
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

#include <radsemaphores.h>
#include <radmsgLog.h>
#include <radlist.h>


/*  ... macro definitions
*/

/*  ... typedefs
*/


/*  ... methods
*/

/*  ... startProcess
    ... start a new process by forking;
    ... entryPoint - the child process entry point after the fork;
    ... args - void pointer to child process-specific argument structure;
    ... returns - the new child pid or ERROR for the parent, and never
    ...           returns if it becomes the child
*/
extern int radStartProcess
(
    int     (*entryPoint) (void * pargs),
    void    *args
);


#ifdef __cplusplus
}
#endif
#endif

