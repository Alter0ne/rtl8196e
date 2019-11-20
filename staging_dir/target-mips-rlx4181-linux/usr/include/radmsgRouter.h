#ifndef INC_radmsgrouterh
#define INC_radmsgrouterh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------

  FILENAME:
        radmsgRouter.h

  PURPOSE:
        Provide a standalone message routing process and API to support the 
        "route by message ID" paradigm.

  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        12/03/2005      M.S. Teel       0               Original

  NOTES:
        There will be one message router process per radlib system ID. The 
        message router process is built and installed when radlib is built and 
        installed and will be found at "$prefix/bin/radmrouted". radmrouted
        must be started before any other radlib processes which plan to use 
        the message routing utility. radmrouted can be started and stopped like 
        any other radlib process with one requirement: the radlib system ID and
        the working directory for the application must be passed as arguments 
        to radmrouted.
        
        USAGE: [prefix]/radmrouted radSystemID workingDirectory <listenPort> <remoteIP:remotePort>
        
            radSystemID           the same system ID used by other processes in 
                                  this group
            workingDirectory      where to store FIFO and pid files for radmrouted
            <listenPort>          optional local port to listen on for remote 
                                  router connections
            <remoteIP:remotePort> optional remote server IP address and port to 
                                  connect to

        "workingDirectory" should match the working directory passed to 
        'radMsgRouterInit' by your processes.


        radlib processes which want to be a message producer and/or consumer 
        will initialize the message router interface then register or deregister 
        for particular message types as required.

  LICENSE:
        Copyright 2001-2010 Mark S. Teel. All rights reserved.

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

//  System include files
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>

//  Library include files
#include <radsysdefs.h>
#include <radsysutils.h>
#include <radprocutils.h>
#include <radlist.h>
#include <radprocess.h>
#include <radsocket.h>
#include <radthread.h>


///*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*///
///*/*/*/*    H I D D E N - I N T E R N A L  U S E  O N L Y    */*/*/*/*/*/*///
///*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*///

#define MSGRTR_LOCK_FILE_NAME           "radmrouted.pid"
#define MSGRTR_QUEUE_NAME               "radmroutedfifo"
#define PROC_NAME_MSGRTR                "radmrouted"
#define MSGRTR_NUM_TIMERS               1

#define MSGRTR_REMOTE_RETRY_INTERVAL    5000            // 5 secs
#define MSGRTR_MAX_ACK_WAIT             1000

#define MSGRTR_INTERNAL_MSGID           0xFFFFFFFF

#define MSGRTR_MAX_CLIENTS              32


// Define PIB types:
typedef enum
{
    PIB_TYPE_LOCAL      = 1,
    PIB_TYPE_REMOTE
} MSGRTR_PIB_TYPE;

// define the process information block (PIB):
typedef struct
{
    NODE            node;
    MSGRTR_PIB_TYPE type;
    char            name[PROCESS_MAX_NAME_LEN+1];

    // Local clients:
    int             pid;
    char            queueName[QUEUE_NAME_LENGTH+1];

    // Remote clients:
    RADSOCK_ID      rxclient;
    RADSOCK_ID      txclient;
    ULONG           maxMsgSize;

    // Stats:
    ULONG           transmits;
    ULONG           receives;
    ULONG           rxErrors;
    ULONG           txErrors;
} MSGRTR_PIB;


// define the consumer information block (CIB):
typedef struct
{
    NODE            node;
    MSGRTR_PIB      *pib;               // points to members of master PIB list
} MSGRTR_CIB;


// define the message ID information block (MIIB)
// this contains the list of registered consumers:
typedef struct
{
    NODE            node;
    ULONG           msgID;
    RADLIST         consumers;          // list of MSGRTR_CIB
} MSGRTR_MIIB;


// define the message router work data:
typedef struct
{
    UCHAR           radSystemID;
    pid_t           myPid;
    char            pidFile[128];
    char            fifoFile[128];
    TIMER_ID        remoteConnectTimer;

    char            remoteIP[256];
    int             remotePort;
    RADSOCK_ID      remoteServer;

    RADLIST         pibList;            // list of registrants (MSGRTR_PIB)
    RADLIST         miibList;           // list of messages by msgID (MSGRTR_MIIB)

    RADSOCK_ID      server;
    int             listenPort;

    ULONG           transmits;
    ULONG           receives;
} MSGRTR_WORK;


// define the process-specific message router work data:
typedef struct
{
    char            rtrQueueName[QUEUE_NAME_LENGTH+1];
} MSGRTR_LOCAL_WORK;


// define the message router message header:
#define MSGRTR_MAGIC_NUMBER             0x59E723F3
typedef struct
{
    ULONG           magicNumber;        // guard against non-API sends to router
    int             srcpid;
    ULONG           msgID;
    ULONG           length;
    UCHAR           msg[0];             // placeholder for start of message
} MSGRTR_HDR;

// define the internal admin message subtypes:
enum
{
    MSGRTR_SUBTYPE_REGISTER         = 1,
    MSGRTR_SUBTYPE_DEREGISTER,
    MSGRTR_SUBTYPE_ACK,
    MSGRTR_SUBTYPE_ENABLE_MSGID,
    MSGRTR_SUBTYPE_MSGID_IS_REGISTERED,
    MSGRTR_SUBTYPE_DISABLE_MSGID,
    MSGRTR_SUBTYPE_DUMP_STATS
};

// define the internal admin message:
typedef struct
{
    ULONG           subMsgID;
    char            name[PROCESS_MAX_NAME_LEN+1];
    ULONG           targetMsgID;
    char            srcIP[128];
    int             srcPort;
    ULONG           socketID;
    ULONG           maxMsgSize;
    int             isRegistered;
} MSGRTR_INTERNAL_MSG;
    

///*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*///
///*/*/*/*                  E N D  H I D D E N                 */*/*/*/*/*/*///
///*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*///


///*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*///
///*/*/*/*                 A P I  M E T H O D S                */*/*/*/*/*/*///
///*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*///

//  Register the message router API for the running process
//  - 'workingDir' specifies where the pid file and FIFOs for the message 
//    router process are to be maintained
//  - returns OK or ERROR
//  Note: 'radSystemInit' and 'radProcessInit' must have been called prior to 
//        calling this function
extern int radMsgRouterInit (char *workingDir);


//  Deregister the message router API for the running process
extern void radMsgRouterExit (void);


//  Deregister process with pid "pid" from the msgRouter
extern void radMsgRouterProcessExit (int pid);


//  Request to receive 'msgID' messages
//  - returns OK or ERROR
//  Note: msgID "MSGRTR_INTERNAL_MSGID" is reserved for internal use
extern int radMsgRouterMessageRegister (ULONG msgID);


//  Request if there are any subscribers to 'msgID' messages
//  - returns FALSE or TRUE
//  Note: msgID "MSGRTR_INTERNAL_MSGID" is reserved for internal use
extern int radMsgRouterMessageIsRegistered (ULONG msgID);


//  Request to NOT receive 'msgID' messages
//  - returns OK or ERROR
//  Note: msgID "MSGRTR_INTERNAL_MSGID" is reserved for internal use
extern int radMsgRouterMessageDeregister (ULONG msgID);


//  Send a message through the message router - all processes which have
//  subscribed to 'msgID' will receive a copy of the message;
//  'msg' will be copied - ownership of 'msg' is NOT transferred but remains 
//  with the caller;
//  - returns OK or ERROR
//  Note: msgID "MSGRTR_INTERNAL_MSGID" is reserved for internal use
extern int radMsgRouterMessageSend (ULONG msgID, void *msg, ULONG byteLength);


//  instruct the message router to dump statistics to the log file
//  - returns OK or ERROR
extern int radMsgRouterStatsDump (void);


#ifdef __cplusplus
}
#endif
#endif

