/******************************************************************
*
*	CyberNet for C
*
*	Copyright (C) Satoshi Konno 2005
*
*	File: csocket.h
*
*	Revision:
*
*	01/17/05
*		- first revision
*
******************************************************************/

#ifndef _CG_NET_CSOCKET_H_
#define _CG_NET_CSOCKET_H_

#include <cybergarage/typedef.h>
#include <cybergarage/util/cstring.h>

#ifdef  __cplusplus
extern "C" {
#endif

/****************************************
* Define
****************************************/

#define CG_NET_SOCKET_NONE 0

#define CG_NET_SOCKET_STREAM 1
#define CG_NET_SOCKET_DGRAM 2

#define CG_NET_SOCKET_CLIENT 1
#define CG_NET_SOCKET_SERVER 2

#define CG_NET_SOCKET_MAXHOST 32
#define CG_NET_SOCKET_MAXSERV 32

#if defined(BTRON) || defined(TENGINE)
typedef W SOCKET;
#elif defined(ITRON)
typedef ER SOCKET;
#elif !defined(WIN32) && !defined(__CYGWIN__)
typedef int SOCKET;
#endif

#define CG_SOCKET_LF '\n'

#define CG_NET_SOCKET_DGRAM_RECV_BUFSIZE 512

#if defined(ITRON)
#define CG_NET_SOCKET_WINDOW_BUFSIZE 4096
#endif

/****************************************
* Define (SocketList)
****************************************/

#if defined(ITRON)
#define CG_NET_USE_SOCKET_LIST 1
#endif

/****************************************
* Data Type
****************************************/

#if defined(CG_NET_USE_SOCKET_LIST)
#include <cybergarage/util/clist.h>
#endif

typedef struct _CgSocket {
#if defined(CG_NET_USE_SOCKET_LIST)
	BOOL headFlag;
	struct _CgSocket *prev;
	struct _CgSocket *next;
#endif
	SOCKET id;
	int type;
	int direction;
	CgString *ipaddr;
	int port;
#if defined(ITRON)
	UH *sendWinBuf;
	UH *recvWinBuf;
#endif
} CgSocket, CgSocketList;

typedef struct _CgDatagramPacket {
	CgString *data;
	CgString *localAddress;
	int localPort;
	CgString *remoteAddress;
	int remotePort;
} CgDatagramPacket;

/****************************************
* Function (Socket)
****************************************/

void cg_socket_startup();
void cg_socket_cleanup();

CgSocket *cg_socket_new(int type);
#define cg_socket_stream_new() cg_socket_new(CG_NET_SOCKET_STREAM)
#define cg_socket_dgram_new() cg_socket_new(CG_NET_SOCKET_DGRAM)
int cg_socket_delete(CgSocket *socket);

#define cg_socket_setid(socket, value) (socket->id = value)
#define cg_socket_getid(socket) (socket->id)

#define cg_socket_settype(socket, value) (socket->type = value)
#define cg_socket_gettype(socket) (socket->type)
#define cg_socket_issocketstream(socket) ((socket->type == CG_NET_SOCKET_STREAM) ? TRUE : FALSE)
#define cg_socket_isdatagramstream(socket) ((socket->type == CG_NET_SOCKET_DGRAM) ? TRUE : FALSE)

#define cg_socket_setdirection(socket, value) (socket->direction = value)
#define cg_socket_getdirection(socket) (socket->direction)
#define cg_socket_isclient(socket) ((socket->direction == CG_NET_SOCKET_CLIENT) ? TRUE : FALSE)
#define cg_socket_isserver(socket) ((socket->direction == CG_NET_SOCKET_SERVER) ? TRUE : FALSE)

#define cg_socket_setaddress(socket, value) cg_string_setvalue(socket->ipaddr, value)
#define cg_socket_setport(socket, value) (socket->port = value)
#define cg_socket_getaddress(socket) cg_string_getvalue(socket->ipaddr)
#define cg_socket_getport(socket) (socket->port)

BOOL cg_socket_isbound(CgSocket *socket);
BOOL cg_socket_close(CgSocket *socket);

BOOL cg_socket_listen(CgSocket *socket);

BOOL cg_socket_bind(CgSocket *sock, int bindPort, char *bindAddr, BOOL bindFlag, BOOL reuseFlag);
BOOL cg_socket_accept(CgSocket *sock, CgSocket *clientSock);
BOOL cg_socket_connect(CgSocket *sock, char *addr, int port);
int cg_socket_read(CgSocket *sock, char *buffer, int bufferLen);
int cg_socket_write(CgSocket *sock, char *data, int dataLen);
int cg_socket_readline(CgSocket *sock, char *buffer, int bufferLen);
long cg_socket_skip(CgSocket *sock, long skipLen);

int cg_socket_sendto(CgSocket *sock, char *addr, int port, char *data, int dataeLen);
int cg_socket_recv(CgSocket *sock, CgDatagramPacket *dgmPkt);

/****************************************
* Function (Multicast)
****************************************/

BOOL cg_socket_joingroup(CgSocket *sock, char *mcastAddr, char *ifAddr);

/****************************************
* Function (Option)
****************************************/

BOOL cg_socket_setreuseaddress(CgSocket *socket, BOOL flag);

/****************************************
* Function (DatagramPacket)
****************************************/

CgDatagramPacket *cg_socket_datagram_packet_new();
void cg_socket_datagram_packet_delete(CgDatagramPacket *dgmPkt);

#define cg_socket_datagram_packet_setdata(dgmPkt, value) cg_string_setvalue(dgmPkt->data, value)
#define cg_socket_datagram_packet_getdata(dgmPkt) cg_string_getvalue(dgmPkt->data)

#define cg_socket_datagram_packet_setlocaladdress(dgmPkt, addr) cg_string_setvalue(dgmPkt->localAddress, addr)
#define cg_socket_datagram_packet_getlocaladdress(dgmPkt) cg_string_getvalue(dgmPkt->localAddress)
#define cg_socket_datagram_packet_setlocalport(dgmPkt, port) (dgmPkt->localPort = port)
#define cg_socket_datagram_packet_getlocalport(dgmPkt) (dgmPkt->localPort)
#define cg_socket_datagram_packet_setremoteaddress(dgmPkt, addr) cg_string_setvalue(dgmPkt->remoteAddress, addr)
#define cg_socket_datagram_packet_getremoteaddress(dgmPkt) cg_string_getvalue(dgmPkt->remoteAddress)
#define cg_socket_datagram_packet_setremoteport(dgmPkt, port) (dgmPkt->remotePort = port)
#define cg_socket_datagram_packet_getremoteport(dgmPkt) (dgmPkt->remotePort)

void cg_socket_datagram_packet_copy(CgDatagramPacket *dstDgmPkt, CgDatagramPacket *srcDgmPkt);

/****************************************
* Function (SocketList)
****************************************/

#if defined(CG_NET_USE_SOCKET_LIST)

#define cg_socket_next(sock) (CgSocket *)cg_list_next((CgList *)sock)

CgSocketList *cg_socketlist_new();
void cg_socketlist_delete(CgSocketList *sockList);

#define cg_socketlist_clear(sockList) cg_list_clear((CgList *)sockList, (CG_LIST_DESTRUCTORFUNC)cg_socket_delete)
#define cg_socketlist_size(sockList) cg_list_size((CgList *)sockList)
#define cg_socketlist_gets(sockList) (CgSocket *)cg_list_next((CgList *)sockList)
#define cg_socketlist_add(sockList, sock) cg_list_add((CgList *)sockList, (CgList *)sock)

#endif

#ifdef  __cplusplus
}
#endif

#endif
