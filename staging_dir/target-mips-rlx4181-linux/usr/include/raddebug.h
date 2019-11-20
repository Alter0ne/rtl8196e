#ifndef INC_raddebugh
#define INC_raddebugh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------

  FILENAME:
        raddebug.h

  PURPOSE:
        Debug utility defs.

  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        4/10/01         M.S. Teel       0               Original

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

#include <syslog.h>
#include <stdio.h>
#include <stdarg.h>

#include <radsysdefs.h>
#include <radmsgLog.h>



/*  ... Print to stdout the format string like printf, 
    ... then wait for <ENTER> if waitForInput is non-zero
*/
extern void radDEBUGPrint (int waitForInput, char *format, ...);


/*  ... Based on the value of _RAD_DBG_ENABLED log to the system log or nothing;
    ...
    ... #define _RAD_DBG_ENABLED    1
    ... #include <raddebug.h>
    ... 
    ... Include the above lines in your source file to enable the following 
    ... function...
*/
#if _RAD_DBG_ENABLED
static void radDEBUGLog (char *format, ...)
{
    va_list     argList;
    char        temp1[256];

    /*  ... print the var arg stuff to the message
    */
    va_start (argList, format);
    vsprintf (temp1, format, argList);
    va_end   (argList);

    // log it
    radMsgLogInit ("::DEBUG::", FALSE, TRUE);
    radMsgLog(PRI_MEDIUM, "::DEBUG::>%s", temp1);
    radMsgLogExit ();
}
#else
static void radDEBUGLog (char *format, ...)
{
    return;
}
#endif

#ifdef __cplusplus
}
#endif
#endif

