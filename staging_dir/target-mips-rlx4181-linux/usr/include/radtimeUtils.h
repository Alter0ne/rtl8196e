#ifndef INC_radtimeUtilsh
#define INC_radtimeUtilsh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------

  FILENAME:
        radtimeUtils.h

  PURPOSE:
        This file contains definitions used by the time-of-day utilities.

  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        04/23/03        MS Teel         0               Original
        04/07/2008      M.S. Teel       1               Change return value of
                                                        radTimeGetMSSinceEpoch
                                                        to ULONGLONG

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

#include <sys/time.h>
#include <unistd.h>
#include <radsysdefs.h>


/*  ... Returns the time in milliseconds since midnight, January 1, 1970
*/
extern ULONGLONG radTimeGetMSSinceEpoch (void);


/*  ... Returns the time in seconds since midnight, January 1, 1970
*/
extern ULONG radTimeGetSECSinceEpoch (void);


#ifdef __cplusplus
}
#endif
#endif

