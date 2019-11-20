/* $Id: synce_socket.h 2698 2006-12-12 20:39:00Z oleavr $ */
#ifndef __synce_socket_h__
#define __synce_socket_h__

#include "synce.h"
#include <netinet/in.h> /* for sockaddr_in */

#ifdef __cplusplus
extern "C"
{
#endif

struct _SynceSocket;
typedef struct _SynceSocket SynceSocket;

/**
 * Create new client socket
 */
SynceSocket* synce_socket_new(/* TODO: some parameters here */);

/**
 * Release client socket
 */
void synce_socket_free(SynceSocket* socket);

/**
  Get file descriptor so we can select() on many sockets including this
*/
int synce_socket_get_descriptor(SynceSocket* socket);

#define SYNCE_SOCKET_INVALID_DESCRIPTOR  (-1)

/**
 * Take ownership of an existing descriptor
 */
void synce_socket_take_descriptor(SynceSocket* socket, int fd);

/**
 * Connect to remote service
 */
bool synce_socket_connect(SynceSocket* socket, const char* host, int port);

/**
 * Connect to proxy service (vdccm)
 */
bool synce_socket_connect_proxy(SynceSocket* syncesock, const char* remoteIpAddress);

/**
 * Open listening socket
 */
bool synce_socket_listen(SynceSocket*, const char* host, int port);

/**
 * Accept incoming connections
 */
SynceSocket* synce_socket_accept(SynceSocket* socket, struct sockaddr_in* address);

/**
 * Close connection
 */
bool synce_socket_close(SynceSocket* socket);

/**
 * Write a number of bytes of data to socket
 */
bool synce_socket_write(SynceSocket* socket, const void* data, unsigned size);

/**
 * Read a number of bytes of data from a socket
 */
bool synce_socket_read(SynceSocket* socket, void* data, unsigned size);

/**
 * This that can happen to a socket... :-)
 *
 * Expand as needed, just use event numbers 1,2,4,8,16,32,...
 */
enum _SocketEvents
{
	EVENT_TIMEOUT     = 1,
	EVENT_READ        = 2,
	EVENT_WRITE       = 4,
	EVENT_INTERRUPTED = 8,
  EVENT_ERROR       = 16,
};

typedef enum _SocketEvents SocketEvents;

/**
 * Wait for an event on a socket
 */
bool synce_socket_wait(SynceSocket* socket, int timeoutInSeconds, short* events);

/**
 * Get the number of bytes available on a socket
 */
bool synce_socket_available(SynceSocket* socket, unsigned* count);


/*
 * Functions from password.c
 */

bool synce_password_send(
		SynceSocket *socket,
		const char *asciiPassword,
		unsigned char key);

bool synce_password_recv_reply(
		SynceSocket* socket,
		size_t size,
		bool* passwordCorrect);

#ifdef __cplusplus
}
#endif

#endif

