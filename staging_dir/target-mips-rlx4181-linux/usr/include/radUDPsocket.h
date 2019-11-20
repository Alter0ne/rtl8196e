#ifndef INC_radudpsocketh
#define INC_radudpsocketh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------

  FILENAME:
        radUDPsocket.h

  PURPOSE:
        Provide standard UDP socket utilities.

  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        10/07/2005      M.S. Teel       0               Original

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
#include <arpa/inet.h>
#include <netdb.h>

/*  ... Library include files
*/
#include <radsysdefs.h>
#include <radsysutils.h>
#include <radmsgLog.h>


/*  ...HIDDEN, don't use
*/
#ifndef SOL_IP
#define SOL_IP IPPROTO_IP
#endif

#define RADUDPSOCK_MAX_HOST_LENGTH      128

typedef struct
{
    int         sockfd;
    int         portno;
    int         debug;
    char        host[RADUDPSOCK_MAX_HOST_LENGTH];
} RADUDPSOCK, *RADUDPSOCK_ID;

/*  ... END HIDDEN
*/

/*	... Create a UDP socket (not bound for receive);
	... returns RADUDPSOCK_ID or NULL if ERROR
*/
extern RADUDPSOCK_ID radUDPSocketCreate (void);


/*	... Close connection and cleanup resources;
	... returns OK or ERROR
*/
extern int radUDPSocketDestroy (RADUDPSOCK_ID id);


/*  ... Get the socket descriptor (for select calls, etc.)
*/
extern int radUDPSocketGetDescriptor (RADUDPSOCK_ID id);


/*  ... Bind the UDP socket to a local port so you can 
    ... add the socket descriptor to your select list and receive data;
    ... returns OK or ERROR
*/
extern int radUDPSocketBind (RADUDPSOCK_ID id, USHORT port);


/*  ... Enable/Disable sending broadcasts (TRUE or FALSE)
    ... returns OK or ERROR
*/
extern int radUDPSocketSetBroadcast (RADUDPSOCK_ID id, int enable);


/*  ... Set multicast interface for outgoing datagrams
    ... returns OK or ERROR
*/
extern int radUDPSocketSetMulticastTXInterface (RADUDPSOCK_ID id, char *interfaceIP);


/*  ... Set unicast TTL for outgoing datagrams (1 by default)
*/
extern int radUDPSocketSetUnicastTTL (RADUDPSOCK_ID id, int ttl);


/*  ... Set multicast TTL for outgoing datagrams (0 - 255, 1 by default)
    ... returns OK or ERROR
*/
extern int radUDPSocketSetMulticastTTL (RADUDPSOCK_ID id, int ttl);


/*  ... Enable/Disable multicast loopback (TRUE or FALSE, FALSE by default)
    ... returns OK or ERROR
*/
extern int radUDPSocketSetMulticastLoopback (RADUDPSOCK_ID id, int enable);


/*  ... "Turn on" RX multicast datagrams on the "multicastGroupIP" group and
    ... "interfaceIP" interface
    ... returns OK or ERROR
*/
extern int radUDPSocketAddMulticastMembership
(
    RADUDPSOCK_ID   id, 
    char            *multicastGroupIP,
    char            *interfaceIP
);


/*  ... "Turn off" RX multicast datagrams on the "multicastGroupIP" group and
    ... "interfaceIP" interface
    ... returns OK or ERROR
*/
extern int radUDPSocketDropMulticastMembership
(
    RADUDPSOCK_ID   id, 
    char            *multicastGroupIP,
    char            *interfaceIP
);


/*  ... receive a datagram from a UDP socket;
    ... will return less than requested amount based on the size of the datagram;
    ... returns bytes read or ERROR if an error occurs
*/
extern int radUDPSocketRecvFrom
(
    RADUDPSOCK_ID   id,
    void            *buffer,
    int             maxToRead
);


/*  ... receive a datagram from a UDP socket (and retrieve the far end socket info);
    ... will return less than requested amount based on the size of the datagram;
    ... returns bytes read or ERROR if an error occurs
*/
extern int radUDPSocketReceiveFrom
(
    RADUDPSOCK_ID       id,
    void                *buffer,
    int                 maxToRead,
    struct sockaddr_in  *sourceAdrs
);

/*  ... Send a datagram to a host name or IP adrs and port (connectionless)
    ... returns OK or ERROR
*/
extern int radUDPSocketSendTo
(
    RADUDPSOCK_ID   id, 
    char            *hostOrIPAdrs,
    USHORT          port,
    void            *data,
    int             length
);

/*  ... Set the socket for blocking or non-blocking IO - 
    ... it is the user's responsibility to handle blocking/non-blocking IO
    ... properly (EAGAIN and EINTR errno's);
    ... Returns OK or ERROR
*/
extern int radUDPSocketSetBlocking (RADUDPSOCK_ID id, int isBlocking);

/*  ... turn on/off data debug - dump all read/write data to msgLog
*/
extern void radUDPSocketSetDebug (RADUDPSOCK_ID id, int enable);


#ifdef __cplusplus
}
#endif
#endif

