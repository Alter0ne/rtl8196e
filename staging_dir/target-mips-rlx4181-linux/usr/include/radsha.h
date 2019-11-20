#ifndef INC_radshah
#define INC_radshah
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------

  FILENAME:
        radsha.h

  PURPOSE:
        Provide SHA-1 and SHA-256 secure hashing algorithm utilities.

  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        10/20/2005      M.S. Teel       0               Original

  NOTES:
        These utilities generate unique secure hashes or "digests" for given
        memory blocks or files.

        SHA-1       20-byte digest
        SHA-256     32-byte digest

        The SHA algorithms were designed by the National Security Agency (NSA) 
        and published as US government standards.

        The SHA-256 implementation is derived from the FreeBSD implementation
        written by Aaron D. Gifford.

        Expects the macro "WORDS_BIGENDIAN" to be defined if the host is big 
        endian.

  LICENSE:
        Portions are Copyright 2000 Aaron D. Gifford. All rights reserved.
        Copyright 2005 Mark S. Teel. All rights reserved.

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
#include <sys/wait.h>
#include <sys/time.h>

/*  ... Library include files
*/
#include <radsysdefs.h>
#include <radsysutils.h>


/*  ...HIDDEN, don't use
*/

typedef union longbyte
{
    ULONG   W[80];                  /* Process 16 32-bit words at a time */
    char    B[320];                 /* But read them as bytes for counting */
} lbyte;

 
struct SHA1Context 
{
    ULONG   h0, h1, h2, h3, h4;     /* accumulators for message digest */
    ULONG   hi_length, lo_length;
    int     inbytes, padded;
    lbyte   in;                     /* serves double duty: if inbytes = 0 
                                       serves as scratchpad for SHA1 calculation 
                                       (all 320 bytes used); otherwise holds
                                       inbytes (< 64) left over bytes waiting 
                                       for more bytes to be added to the digest 
                                       calculation */
};

#define RADSHA1_DIGEST_LENGTH             20

#define RADSHA256_BLOCK_LENGTH	          64
#define RADSHA256_DIGEST_LENGTH           32


typedef struct _SHA256_CTX 
{
	ULONG        state[8];
	ULONGLONG    bitcount;
	UCHAR        buffer[RADSHA256_BLOCK_LENGTH];
} SHA256_CTX;

/*  ... END HIDDEN
*/


/*  ... API - for public use
*/

/*  ... use these macros to allocate the digest result string size
*/
#define RADSHA1_DIGEST_STR_LENGTH       ((RADSHA1_DIGEST_LENGTH * 2) + 1)
#define RADSHA256_DIGEST_STR_LENGTH     ((RADSHA256_DIGEST_LENGTH * 2) + 1)


/*	... Compute the SHA-1 digest for the memory block 'block' of byte length
	... 'byteLength' and store the result in 'digestStore'
    ... returns OK or ERROR
*/
extern int radSHA1ComputeBlock 
(
    void        *block, 
    int         byteLength, 
    char        digestStore[RADSHA1_DIGEST_STR_LENGTH]
);


/*	... Compute the SHA-1 digest for the file given by full path and filename 
    ... 'filename' and store the result in 'digestStore'
    ... returns OK or ERROR
*/
extern int radSHA1ComputeFile
(
    char        *filename, 
    char        digestStore[RADSHA1_DIGEST_STR_LENGTH]
);


/*	... Compute the SHA-256 digest for the memory block 'block' of byte length
	... 'byteLength' and store the result in 'digestStore'
    ... returns OK or ERROR
*/
extern int radSHA256ComputeBlock 
(
    void        *block, 
    int         byteLength, 
    char        digestStore[RADSHA256_DIGEST_STR_LENGTH]
);


/*	... Compute the SHA-256 digest for the file given by full path and filename 
    ... 'filename' and store the result in 'digestStore'
    ... returns OK or ERROR
*/
extern int radSHA256ComputeFile
(
    char        *filename, 
    char        digestStore[RADSHA256_DIGEST_STR_LENGTH]
);


#ifdef __cplusplus
}
#endif
#endif

