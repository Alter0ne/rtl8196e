#ifndef INC_radhthreadh
#define INC_radhthreadh
#ifdef __cplusplus
extern "C" {
#endif
/*---------------------------------------------------------------------------
 
  FILENAME:
        radthread.h
 
  PURPOSE:
        Provide an easy to use pthread utility.
 
  REVISION HISTORY:
        Date            Engineer        Revision        Remarks
        03/26/2011      MST             0               Original
 
  NOTES:
        Provide a uniform way to create and destroy utility threads from radlib 
        processes. 

        Communication between the parent and thread can be accomplished in
        three ways:
        1) Using the radthread internal queues via radthreadSendToThread,
           radthreadReceiveFromThread, radthreadSendToParent, 
           raddthreadReceiveFromParent.
        2) If the parent is using radProcessWait to receive messages and wait on
           file descriptor IO, the producer threads can use radMsgRouterMessageSend
           to send data to the Consumer (parent). The parent thread must have 
           registered with radMsgRouterInit and for the message the thread sends
           via radMsgRouterMessageRegister. The thread can send messages not 
           intended for the parent process as well.
        3) Modifying shared data directly. Parents and threads should use the 
           radthreadLock and radthreadUnlock functions if modifying any shared 
           data. This should be done with great caution. In particular, 
           radMsgRouterMessageSend locks and unlocks the thread mutex directly
           so the mutex cannot be held when calling it.

        All threads should periodically test for an exit request from the 
        parent using the radthreadShouldExit function. Parents calling 
        radthreadWaitExit will block until the thread terminates. If the parent
        needs to know when the thread exits, the thread should send a message
        via radMsgRouterMessageSend to indicate it is exiting.

       *Parent Actions
        --------------

        To create and start a thread:
            RAD_THREAD_ID radthreadCreate(void (*ThreadEntry)(RAD_THREAD_ID threadId, 
                                                              void* threadData), 
                                          void* threadData)

        To send data to the thread:
            void radthreadSendToThread(RAD_THREAD_ID threadId, 
                                       int type, 
                                       void* data, 
                                       int length)

        To receive data from the thread:
            int radthreadReceiveFromThread(RAD_THREAD_ID threadId, 
                                           void** data, 
                                           int *length,
                                           int blocking)

        To set exit flag and wait for thread to exit:
            void radthreadWaitExit(RAD_THREAD_ID threadId)


       *Thread Actions
        --------------
        Threads may only use the following radlib functions:
            radMsgRouterMessageSend (thread mutex should not be held when called)
            radTimeGetMSSinceEpoch
            radTimeGetSECSinceEpoch
            radCRC16Calculate
            radCRC32Calculate
            radlist (with non-shared RADLIST_ID)
            radsha
            radsortlist (with non-shared SORTLIST_ID)
            radstack (with non-shared STACK_ID)
            radstates (with non-shared STATES_ID)
            radtextsearch (with non-shared TEXT_SEARCH_ID)

        To send data to the parent:
            void radthreadSendToParent(RAD_THREAD_ID threadId, 
                                       int type, 
                                       void* data, 
                                       int length)

        To receive data from the parent:
            int radthreadReceiveFromParent(RAD_THREAD_ID threadId, 
                                           void** data, 
                                           int *length,
                                           int blocking)

        To test for exit command from parent:
            void radthreadShouldExit(RAD_THREAD_ID threadId)

        
       *Parent or Thread Actions
        ------------------------

        To lock the thread mutex:
            void radthreadLock(RAD_THREAD_ID threadId)
 
        To unlock the thread mutex:
            void radthreadUnlock(RAD_THREAD_ID threadId)

  LICENSE:
        Copyright 2011 Mark S. Teel. All rights reserved.

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
#include <pthread.h>
#include <radlist.h>
#include <radbuffers.h>


// // Data Definitions.

// Define the thread ID:
typedef struct _radThreadIdTag
{
    pthread_t           thread;
    int                 exitFlag;

    RADLIST             ToThreadQueue;
    pthread_mutex_t     ToThreadMutex;
    pthread_cond_t      ToThreadCondition;

    RADLIST             ToParentQueue;
    pthread_mutex_t     ToParentMutex;
    pthread_cond_t      ToParentCondition;
} *RAD_THREAD_ID;

// Define the thread args container:
typedef struct
{
    void                (*Entry)(RAD_THREAD_ID threadId, void* threadData);
    RAD_THREAD_ID       id;
    void*               data;
} RAD_THREAD_ARGS;

// Define the generic data container for queues:
typedef struct
{
    NODE                node;
    int                 type;
    int                 length;
    UCHAR               data[0];
} RAD_THREAD_NODE;


// // API methods:

// Parent: To create and start a thread:
// Returns: the new RAD_THREAD_ID or NULL if not successful.
extern RAD_THREAD_ID radthreadCreate
(
    void    (*ThreadEntry)(RAD_THREAD_ID threadId, void* threadData), 
    void*   threadData
);

// Parent: To set exit flag and wait for thread to exit:
extern void radthreadWaitExit(RAD_THREAD_ID threadId);

// Parent: To send data to the thread:
// "type" is a user-defined value;
// "data" is copied to the thread queue (ownership is not transferred);
// Returns: OK or ERROR
extern int radthreadSendToThread
(
    RAD_THREAD_ID   threadId, 
    int             type, 
    void*           data, 
    int             length
);

// Parent: To receive data from the thread:
// Returns: user-defined msg type or ERROR_ABORT if non-blocking and no msg
// "*data" will point to a radsysBuffer which must be freed via radBufferRls 
// when the receiver is done with it;
extern int radthreadReceiveFromThread
(
    RAD_THREAD_ID   threadId, 
    void**          data,
    int*            length,
    int             blocking
);

// Thread: To send data to the parent:
// "type" is a user-defined value;
// "data" is copied to the thread queue (ownership is not transferred);
// Returns: OK or ERROR
extern int radthreadSendToParent
(
    RAD_THREAD_ID   threadId, 
    int             type, 
    void*           data, 
    int             length
);

// Thread: To receive data from the parent:
// Returns: user-defined msg type or ERROR_ABORT if non-blocking and no msg
// "*data" will point to a radsysBuffer which must be freed via radBufferRls 
// when the receiver is done with it;
extern int radthreadReceiveFromParent
(
    RAD_THREAD_ID   threadId, 
    void**          data, 
    int*            length,
    int             blocking
);

// Thread: To test for exit command from parent:
// Returns: TRUE or FALSE
extern int radthreadShouldExit(RAD_THREAD_ID threadId);

// Parent/Thread: To lock the thread mutex:
extern void radthreadLock(void);
 
// Parent/Thread: To unlock the thread mutex:
extern void radthreadUnlock(void);


#ifdef __cplusplus
}
#endif
#endif

