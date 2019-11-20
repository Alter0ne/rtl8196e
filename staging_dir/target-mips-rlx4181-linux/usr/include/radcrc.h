#ifndef INC_radcrch
#define INC_radcrch
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------

  FILENAME:
        radcrc.h

  PURPOSE:
        Provide CRC16 and CRC32 utilities.

  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        10/20/2005      M.S. Teel       0               Original

  NOTES:
        The 32-bit CRC uses the standard polynomial:
        x^32+x^26+x^23+x^22+x^16+x^12+x^11+x^10+x^8+x^7+x^5+x^4+x^2+x+1
        
        The 16-bit CRC uses the standard CCITT polynomial:
        x^16+x^12+x^5+1

        Both use "all bits set" as the seed value, i.e. 0xFFFFFFFF and 0xFFFF
        respectively.

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
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/time.h>

/*  ... Library include files
*/
#include <radsysdefs.h>
#include <radsysutils.h>


/*  ...HIDDEN, don't use
*/

#define CRC16_POLYNOMIAL            ((USHORT)0x8408)
#define CRC16_SEED                  ((USHORT)0xFFFF)
#define CRC32_POLYNOMIAL            ((ULONG)0xEDB88320)
#define CRC32_SEED                  ((ULONG)0xFFFFFFFF)

/*  ... END HIDDEN
*/

/*  ... API methods
*/

/*  ... calculate the 16-bit CRC-CCITT for the given memory block;
    ... returns the USHORT CRC value
*/
extern USHORT radCRC16Calculate (void *block, int byteLength);


/*  ... calculate the 32-bit CRC for the given memory block;
    ... returns the ULONG CRC value
*/
extern ULONG radCRC32Calculate (void *block, int byteLength);


#ifdef __cplusplus
}
#endif
#endif

