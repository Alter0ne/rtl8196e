#ifndef INC_radmsgLogh
#define INC_radmsgLogh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radmsgLog.h
 
  PURPOSE:
        System message log utility defs.
 
  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        9/29/99         M.S. Teel       0               Original
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

#include <stdio.h>
#include <syslog.h>

#include <radsysdefs.h>


/*  ... for use in process initialization
*/
extern int radMsgLogInit
(
    char        *procName,
    int         useStderr,      /* T/F: also write to stderr */
    int         timeStamp       /* T/F: include millisecond timestamps */
);

extern int radMsgLogExit
(
    void
);

/*  ... define the priority levels
*/
enum MsgLogPriorities
{
    PRI_STATUS          = LOG_INFO,
    PRI_MEDIUM          = LOG_WARNING,
    PRI_HIGH            = LOG_CRIT,
    PRI_CATASTROPHIC    = LOG_ALERT
};

/*  ... log a message - allow variable length parameter list
*/
extern int radMsgLog
(
    int         priority,
    char        *format,
    ...
);

extern void radMsgLogData
(
    void        *data,
    int         length
);

#ifdef __cplusplus
}
#endif
#endif

