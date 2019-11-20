#ifndef INC_radsocketh
#define INC_radsocketh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------

  FILENAME:
        radsocket.h

  PURPOSE:
        Provide standard AF_INET TCP stream socket utilities.

  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        06/05/2005      M.S. Teel       0               Original

  NOTES:
        This utility makes connections and passes data. Byte order or 
        content of the data is not considered. The user is responsible for 
        data contents and byte ordering conversions (if required).        

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
#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>

/*  ... Library include files
*/
#include <radsysdefs.h>
#include <radsysutils.h>
#include <radbuffers.h>
#include <radlist.h>
#include <radmsgLog.h>


/*  ...HIDDEN, don't use
*/
#define RADSOCK_MAX_HOST_LENGTH         128

typedef struct
{
    int         sockfd;
    int         portno;
    int         debug;
    char        host[RADSOCK_MAX_HOST_LENGTH];
    int         remotePort;
    char        remoteHost[RADSOCK_MAX_HOST_LENGTH];
} RADSOCK, *RADSOCK_ID;

// Define the DNS result container:
typedef struct 
{
    NODE            Node;
    char            host[256];
} DNS_RECORD;


// define our work area:
typedef struct
{
    RADLIST         results;
    DNS_RECORD*     currentRecord;
} DNS_WORK;
typedef DNS_WORK*   DNS_ID;


/*  ... END HIDDEN
*/

/*	... Create a socket server to listen on port "port";
	... returns RADSOCK_ID or NULL if ERROR
*/
extern RADSOCK_ID radSocketServerCreate (int port);


/*	... Create a new socket instance for the incoming connection and prepare 
    ... it for read/write activity - should be called when there is a "wake up" 
    ... on the listen socket indicating an incoming connection attempt;
	... returns the new RADSOCK_ID for the client connection or NULL if there 
    ... is an error
*/
extern RADSOCK_ID radSocketServerAcceptConnection (RADSOCK_ID listenServerID);


/*	... Create a socket client that connects to "hostNameOrIP" on  port "port";
	... returns RADSOCK_ID or NULL if ERROR
*/
extern RADSOCK_ID radSocketClientCreate (char *hostNameOrIP, int port);


/*	... Create a socket client that connects to "hostNameOrIP" on  port "port";
	... if "hostNameOrIP" resolves to multiple IP addresses (via getaddrinfo),
	... each IP address will be tried until a connection is achieved in the
	... order received from getaddrinfo;
	... returns RADSOCK_ID or NULL if ERROR
*/
extern RADSOCK_ID radSocketClientCreateAny (char *hostNameOrIP, int port);


/*	... Close connection and cleanup resources;
	... returns OK or ERROR
*/
extern int radSocketDestroy (RADSOCK_ID id);


/*  ... Get the socket descriptor (for select calls, etc.)
*/
extern int radSocketGetDescriptor (RADSOCK_ID id);


/*  ... Get the socket hostname or IP
*/
extern char *radSocketGetHost (RADSOCK_ID id);


/*  ... Get the socket port number
*/
extern int radSocketGetPort (RADSOCK_ID id);


/*  ... Get the socket remote hostname or IP
*/
extern char *radSocketGetRemoteHost (RADSOCK_ID id);


/*  ... Get the socket remote port number
*/
extern int radSocketGetRemotePort (RADSOCK_ID id);


/*	... read/write an exact size (will block if a blocking socket);
    ... will return less than requested amount if a non-blocking socket and
    ... the read would block or it is interrupted by a received signal;
	... returns bytes read/written or ERROR if an error occurs
*/
extern int radSocketReadExact 
(
    RADSOCK_ID      id, 
    void            *buffer, 
    int             lengthToRead
);
    

extern int radSocketWriteExact 
(
    RADSOCK_ID      id,
    void            *buffer, 
    int             lengthToWrite
);


/*  ... Set the socket for blocking or non-blocking IO - 
    ... it is the user's responsibility to handle blocking/non-blocking IO
    ... properly (EAGAIN and EINTR errno's);
    ... Returns OK or ERROR
*/
extern int radSocketSetBlocking (RADSOCK_ID id, int isBlocking);


/*  ... turn on/off data debug - dump all read/write data to msgLog
*/
extern void radSocketSetDebug (RADSOCK_ID id, int enable);


#ifdef __cplusplus
}
#endif
#endif

