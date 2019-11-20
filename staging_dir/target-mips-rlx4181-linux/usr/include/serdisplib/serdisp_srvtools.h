/*
 *************************************************************************
 *
 * serdispd_srvtools.h
 * include file for tcp/ip communications
 *
 *************************************************************************
 *
 * copyright (C) 2006       //MAF
 *
 * additional maintenance and enhancements:
 * copyright (C) 2006-2010  wolfgang astleitner
 * email     mrwastl@users.sourceforge.net
 *
 *************************************************************************
 * This program is free software; you can redistribute it and/or modify   
 * it under the terms of the GNU General Public License as published by   
 * the Free Software Foundation; either version 2 of the License, or (at  
 * your option) any later version.                                        
 *                                                                        
 * This program is distributed in the hope that it will be useful, but    
 * WITHOUT ANY WARRANTY; without even the implied warranty of             
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU      
 * General Public License for more details.                               
 *                                                                        
 * You should have received a copy of the GNU General Public License      
 * along with this program; if not, write to the Free Software            
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA              
 * 02111-1307, USA.  Or, point your browser to                            
 * http://www.gnu.org/copyleft/gpl.html                                   
 *************************************************************************
 */


#ifndef SERDISP_SRVTOOLS_H
#define SERDISP_SRVTOOLS_H

/*=========================================================================*\
	Includes needed by definitions from this file
\*=========================================================================*/
#include <stdlib.h>
#include <syslog.h>
#include <time.h>

#include "serdisplib/serdisp.h"

/*=========================================================================*\
	General constants
\*=========================================================================*/

#define SD_SRV_PROTOVERS_MAJOR    0
#define SD_SRV_PROTOVERS_MINOR    3

#define SD_SRV_DEFPORT            15243

#define SD_SRV_HDRMAGIC           0x7ED963A0


/*=========================================================================*\
	Type definitions and related constants
\*=========================================================================*/

/*------------------------------------------------------------------------*\
   Data buffers used for sending and receiving data
\*------------------------------------------------------------------------*/
#define SERDISP_SRV_MINBUFLEN 128
typedef enum {
  MBUF_VIRGIN = 0,
  MBUF_LOADED,
  MBUF_HEADER,
  MBUF_PAYLOAD,
  MBUF_COMPLETE
} mbuf_stat;

typedef enum {
  MBUF_CMD_UNKNOWN = -1
 ,MBUF_CMD_PROTOVERS = 1
 ,MBUF_CMD_SDLIBVERS
 ,MBUF_CMD_ATTACH
 ,MBUF_CMD_DDINFO
 ,MBUF_CMD_DDGETTABLES
 ,MBUF_CMD_DDINIT
 ,MBUF_CMD_DDCLOSE
 ,MBUF_CMD_DDUPDATE
 ,MBUF_CMD_DDSETOPTION
 ,MBUF_CMD_DDGETOPTION
 ,MBUF_CMD_DDSETSDPIXEL
 ,MBUF_CMD_DDGETSDPIXEL

 ,MBUF_CMD_OPTIONAMOUNT
 ,MBUF_CMD_OPTIONDESC

 ,MBUF_CMD_GPIOAMOUNT
 ,MBUF_CMD_GPODESC
 ,MBUF_CMD_GPIDESC

 ,MBUF_CMD_DDPUTBUFFS
 ,MBUF_CMD_DDGETBUFFS
} mbuf_cmd;

typedef struct {
  mbuf_stat  status;
  char      *buffer;
  size_t     bufsize;      /* Allocated memory           */
  size_t     msglen;       /* Message Length             */
  size_t     index;        /* Index of last byte handled */
  char      *buffer_;      /* for storing payload while handling header */
  size_t     msglen_;
  mbuf_cmd   cmd;
  int        msgcntr;
  size_t     bytecntr;
  time_t     timestamp;
} SERDISP_SRVMBUFFER;


/*------------------------------------------------------------------------*\
   Handle connections
\*------------------------------------------------------------------------*/
typedef enum {
  CON_TRANSPARENT = 0,
  CON_WRITETHROUGH,
  CON_BUFFERED
} srvcon_mode;

#define CON_SELRD        0x0001
#define CON_SELWRT       0x0002
#define CON_SELEXPT      0x0004

typedef struct {
  int                  fd;
  char                *name;
  srvcon_mode          mode;
  int                  status;
  SERDISP_SRVMBUFFER   readbuf;
  SERDISP_SRVMBUFFER   writebuf;
  time_t               timestamp;
} SERDISP_SRVCON;



/*------------------------------------------------------------------------*\
   Argument definitions for packing/unpacking buffers
\*------------------------------------------------------------------------*/
typedef enum {
  MBUF_ARGEND = 0,
  MBUF_ARGBYTE,
  MBUF_ARGINT,
  MBUF_ARGLONG,
  MBUF_ARGSTR,
  MBUF_ARGBYTES,
  MBUF_ARGINTS,
  MBUF_ARGLONGS,
} mbuf_type;

typedef struct {
  mbuf_type  type;
  void      *buffer;
  size_t     size;         /* for type==MBUF_BYTES           */
} SERDISP_SRVARG;


/*=========================================================================*\
	Some Macros
\*=========================================================================*/
#ifndef MIN
  #define MIN(a,b) ((a)<(b)?(a):(b))
#endif
#ifndef MAX
  #define MAX(a,b) ((a)>(b)?(a):(b))
#endif
#define Sfree(p) ((p)?free(p),(p)=NULL:NULL)


/*=========================================================================*\
	Prototypes
\*=========================================================================*/
#ifdef __cplusplus
extern "C" {
#endif

SERDISP_SRVCON* serdisp_srv_connect     (const char *hostname, int port);
int             serdisp_srv_close       (SERDISP_SRVCON *con);

int             serdisp_srv_doreadbuf   (SERDISP_SRVMBUFFER *buf, int fd);
int             serdisp_srv_dowritebuf  (SERDISP_SRVMBUFFER *buf, int fd);

int             serdisp_srv_fillbuf     (SERDISP_SRVMBUFFER *buf, SERDISP_SRVARG *args);
int             serdisp_srv_unpackbuf   (SERDISP_SRVMBUFFER *buf, SERDISP_SRVARG *args);

int             serdisp_srv_setblocking (SERDISP_SRVCON *cd, int value);
int             serdisp_srv_sendcmd     (SERDISP_SRVCON *cd, mbuf_cmd cmd, SERDISP_SRVARG *args, SERDISP_SRVARG *retvals);

#ifdef __cplusplus
    }
#endif


#endif /* SERDISP_SRVTOOLS_H */
