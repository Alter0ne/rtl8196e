/** \file    serdisp_gpevents.h
  *
  * \brief   Definitions and functions for general purpose items and event handling
  * \date    (C) 2006-2013
  * \author  wolfgang astleitner (mrwastl@users.sourceforge.net)
  */

/*
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

/** \addtogroup SERDISP_GPEVENTS

  \section SDGPEV_INTRODUCTION Introduction
  serdisp_gpevents.h offers functions and definition for
  \li <b>general purpose items</b>
  \li <b>event handling</b>.

  \section FEATURES Features

  \subsection GPIO_TYPES General Purpose Items
    serdisplib supports both incoming and outgoing general purpose items
    \li <b>general purpose input (GPI)</b>: some event or value sent \em to the library \em from a device
    \li <b>general purpose output (GPO)</b>: some event or value sent \em from the library \em to a device

  \subsection EVENTHANDLING Event-Handling

  * @{
  */

#ifndef SERDISP_GPEVENTS_H
#define SERDISP_GPEVENTS_H

#include "serdisplib/serdisp_connect.h"
#include "serdisplib/serdisp_control.h"
#include <sys/time.h>
#include <stdint.h>

#include "../../config.h"

#ifdef HAVE_LIBPTHREAD
  #include <pthread.h>

  /* req. for input-device based touch device support */
  #ifdef __linux__
    #include <linux/input.h>
    #include <linux/fd.h>
  #endif
#endif


/* define 'byte' if not available yet */
#ifndef byte
  #define byte unsigned char
#endif

/* define 'ushort' if not available yet */
#ifndef ushort
  #define ushort unsigned short
#endif

#ifndef BOOL
  #define BOOL int
#endif


/* status of event loop thread */
#define SDEVLP_STOPPED   0         /* event loop thread is stopped */
#define SDEVLP_RUNNING   1         /* event loop thread is running */



/*****************/
/* GPI/GPO types, events (data exchange events, ...) */
/*****************/

/* numeric types */              /* 0000 xxxx */
#define SDGPT_BOOL         0x00  /* 0000 0000 */  /* boolean value, 0 = false, 1 = true */
#define SDGPT_INVBOOL      0x01  /* 0000 0001 */  /* inverted boolean, 0 = true, 1 = false */
#define SDGPT_VALUE        0x02  /* 0000 0010 */  /* numeric value,        min:           0, max: 4294967295 */
#define SDGPT_SGNVALUE     0x03  /* 0000 0011 */  /* signed numeric value, min: -2147483648, max: 2147483647 */

/* data-package types */         /* 0001 xxxx */
#define SDGPT_SIMPLETOUCH  0x10  /* 0001 0000 */  /* simple touch screen event, type: SDGP_evpkt_simpletouch_t */
#define SDGPT_GENERICTOUCH 0x11  /* 0001 0001 */  /* generic touch screen event, type: SDGP_evpkt_generictouch_t */

/* streaming types */            /* 0010 xxxx */
#define SDGPT_BYTESTREAM   0x20  /* 0010 0000 */  /* byte stream */
#define SDGPT_NORMRCSTREAM 0x24  /* 0010 0100 */  /* stream of normalised remote control sequences */


#define SDGPT_ERROR        0xFF  /* 1111 1111 */  

/* category = ((SDGPT_<type> & 0x30) >> 4) */
#define SDGPT_GETCATEGORY(_type)   (((_type) & 0x30) >> 4)

#define SDGPT_CATEGORYVALUE    0x0  /* numeric types */
#define SDGPT_CATEGORYPACKAGE  0x1  /* data-package types */
#define SDGPT_CATEGORYSTREAM   0x2  /* streaming types */


/* some touch defines */
#define SDGPT_TOUCHDOWN        0x0
#define SDGPT_TOUCHUP          0x1
#define SDGPT_TOUCHMOVE        0x2
#define SDGPT_TOUCHSCROLL      0x3

/*****************/
/* command types */
/*****************/
/*
 _GET_   ... ask for an item (eg. GET_APIVERSION tells the client to send its api-version)
 _SEND_  ... send an item
 
 _SET_   ... remotely set an item
 
 c2s     ... client (driver remote) to server (serdispd)
 s2c     ... server to client
 bidi    ... both: server to client and client to server
*/

/* commands for sending and receiving library-specific and display content */
#define SD_CMD_GET_SERDISPVERSION   0x00  /* 0000 0000 */ /* bidi: ask for serdisplib-version */
#define SD_CMD_SEND_SERDISPVERSION  0x08  /* 0000 1000 */ /* bidi: return serdisplib-version */

#define SD_CMD_GET_APIVERSION       0x01  /* 0000 0001 */ /* bidi: ask for api-version */
#define SD_CMD_SEND_APIVERSION      0x09  /* 0000 1001 */ /* bidi: return api-version */

#define SD_CMD_ACK_EVENT            0x0F  /* 0000 1111 */ /* bidi: acknowledge an event (lib/disp, gpi/gpo, exchange) */

/* commands for sending and receiving gpis and gpos */
#define SD_CMD_GET_GPO_AMOUNT       0x10  /* 0001 0000 */ /* c2s: get amount of available GPOs */
#define SD_CMD_SEND_GPO_AMOUNT      0x18  /* 0001 1000 */ /* s2c: return amount of available GPOs */

#define SD_CMD_GET_GPO_DESC         0x11  /* 0001 0001 */ /* c2s: get GPO description (identified by its ID) */
#define SD_CMD_SEND_GPO_DESC        0x19  /* 0001 1001 */ /* s2c: return GPO descroption */

#define SD_CMD_SET_GPO              0x12  /* 0001 0010 */ /* c2s: set GPO value or send package or stream to it */


#define SD_CMD_GET_GPI_AMOUNT       0x14  /* 0001 0100 */ /* c2s: get amount of available GPIs */
#define SD_CMD_SEND_GPI_AMOUNT      0x1C  /* 0001 1100 */ /* s2c: return amount of available GPIs */

#define SD_CMD_GET_GPI_DESC         0x15  /* 0001 0101 */ /* c2s: get GPI description (identified by its ID) */
#define SD_CMD_SEND_GPI_DESC        0x1D  /* 0001 1101 */ /* s2c: return GPI descroption */

#define SD_CMD_SEND_GPI             0x1E  /* 0001 1110 */ /* s2c: send GPI to client (value, package, or stream) */

/* commands for exchanging options */
#define SD_CMD_GET_OPTION_AMOUNT    0x40  /* 0100 0000 */ /* c2s: get amount of defined options */
#define SD_CMD_SEND_OPTION_AMOUNT   0x48  /* 0100 1000 */ /* s2c: return amount of def'd options */

#define SD_CMD_GET_OPTION_DESC      0x41  /* 0100 0001 */ /* c2s: get option description (identified by its ID) */
#define SD_CMD_SEND_OPTION_DESC     0x49  /* 0100 1001 */ /* s2c: return option desc */

#define SD_CMD_SET_OPTION           0x42  /* 0100 0010 */ /* c2s: set option value */


/* event-type for GPIs, GPOs, and data exchange messages. min. size: 16 byte, max size: 12 + 64) */
typedef struct SDGP_event_s { /* 16 to 78 bytes */
  /* byte  0 */
  byte           type;         /* one of SDGPT_* */
  byte           cmdid;        /* command-ID (one of SD_CMD_*) */
  byte           devid;        /* device ID, 0 == local */
  byte           subid;        /* gp-ID or page-ID */
  /* byte  4 */
  struct timeval timestamp;    /* timestamp (8 bytes) */
  /* byte 12 */
  union {
    int32_t      value;        /* if single value event: value  */
    struct {                   /* if streaming event or package: */
      uint16_t   length;       /*   length of stream if known or 0 if some stop tag is used */
      uint8_t    word_size;    /*   stream elements are bytes/chars (0 or 1), shorts (2), or longs (4) */
      byte       _reserved;    /*   reserved for later use */
    };
    byte         data[64];     /* if data-package type: max. 64 byte payload */
  };
} SDGP_event_t;


/* GPO */
typedef struct SDGPO_s {
  byte       id;                 /* unique id */
  char*      name;               /* name of output item */
  char*      aliasnames;         /* alias names for this item (short name, ...) */
  byte       type;               /* one of SDGPT_* */
  char       mode;               /* 'S' .. single, 'C' .. compound, 'P' .. only makes sense as a part of a compound */
  int32_t    minval, maxval;     /* mininum and maximum bandwidth for value (digital: min=0, max=1) */
  char*      defines;            /* defines for option values (eg: 1=YES), separated by commas */
} SDGPO_t;


/* GPI */
typedef struct SDGPI_s {  
  byte       id;                 /* unique id */
  char*      name;               /* name of option */
  char*      aliasnames;         /* alias names for this option (short name, ...) */
  byte       type;               /* one of SDGPT_* */
  int        enabled;            /* when initialising device: 1 == autostart. when runtime: is GPI enabled? */
  union {
    int32_t  value;               /* single value gpi: last known value */
    int      fd[2];               /* streaming gpi: file descriptor */
  };
} SDGPI_t;


/* event-payload-type for simple touchscreen events (no multitouch or similar) */
/* might be replaced by SDGP_evpkt_generictouch_t in the future */
typedef struct SDGP_evpkt_simpletouch_s {
  /* 12 bytes */
  int16_t    raw_x;               /* raw coordinate X */
  int16_t    raw_y;               /* raw coordinate Y */
  int16_t    raw_touch;           /* raw touch value */
  int16_t    norm_x;              /* normalised coordinate X (norm_x <= dd->width) */
  int16_t    norm_y;              /* normalised coordinate Y (norm_y <= dd->height) */
  int16_t    norm_touch;          /* normalised touch value */
} SDGP_evpkt_simpletouch_t;


/* event-payload-type for generic touchscreen events (no multitouch or similar) */
/* might replace SDGP_evpkt_simpletouch_t in the future */
typedef struct SDGP_evpkt_generictouch_s {
  /* 8 standard bytes, rest: filled with variable union-data */
  byte       type;                /* event type: one of SDGPT_TOUCH*  */
  byte       flags;               /* 0000 000x ... 0: binary touch, 1: touch with pressure information */
                                  /* xxxx 0000 ... 0000: union not used */
                                  /*               0001: raw touch information and unscaled x/y values included */
                                  /*               0010: -reserved- */
                                  /*               0011: -reserved- */
  int16_t    norm_x;              /* normalised coordinate X (norm_x <= dd->width) */
  int16_t    norm_y;              /* normalised coordinate Y (norm_y <= dd->height) */
  uint16_t   norm_touch;          /* normalised touch value */
                                  /* if binary touch: lower 8 bits contain bitmask of mouse button(s) pressed:
                                   * 0000 0000 ---S -xxx
                                   *                   ^--- touch event or left mouse button
                                   *                  ^---- right mouse button
                                   *                 ^----- middle mouse button
                                   *                ^------ (reserved)
                                   *              ^-------- if type == SDGPT_TOUCHSCROLL: S=0: down, S=1: up
                                   */
  union {
    byte       _reserved[64-8];   /* fill struct up to 64 bytes */
    struct {                      /* struct with touch-only raw information  */
      int32_t  raw_x;             /* raw unscaled x value */
      int32_t  raw_y;             /* raw unscaled y value */
      uint16_t raw_touch;         /* raw touch value (only if binary touch) */
      uint16_t raw_mintouch;      /* max raw touch value (only if binary touch) */
      uint16_t raw_maxtouch;      /* min raw touch value (only if binary touch) */
      byte     raw_actlow;        /* touchdown has higher value than touchlow (only if binary touch) */
    };
  };
} SDGP_evpkt_generictouch_t;


/* function pointer type for event listeners */
typedef void (*fp_eventlistener_t)(struct serdisp_s* dd, SDGP_event_t* recycle);

/* chain type for event listener (single chained) */
typedef struct SDGP_eventlistener_chain_s {
  fp_eventlistener_t                 eventlistener;
  byte                               gpid;
  struct SDGP_eventlistener_chain_s* next;
} SDGP_eventlistener_chain_t;


/* set for GPI/GPO and event handling */
typedef struct SDGP_gpevset_s {
  /* GPI/GPO */
#ifdef HAVE_LIBPTHREAD
  SDGPI_t*      gpis;                  /* GPI set */
#else
  void*         __dummy1;
#endif
  SDGPO_t*      gpos;                  /* GPO set */
  byte          amountgpis;            /* amount of GPIs in GPI set (max. 255 GPIs (id 255 ... reserved) */
  byte          amountgpos;            /* amount of GPOs in GPO set (max. 255 GPOs (id 255 ... reserved) */

  /* command processor */
#ifdef HAVE_LIBPTHREAD
  int           cmdproc_port;          /* port for command processor. 0: no processor */
  int           cmdproc_sock;          /* socket for command processor: -1: invalid */
#endif

  /* event loop */
#ifdef HAVE_LIBPTHREAD
  int           evlp_noautostart;      /* the very first call of SDEVLP_start() (called in serdisp_control.c) will NOT start loop.
                                          default: 0 -> auto start loop */
  int           evlp_status;           /* event loop thread status (SDEVLP_RUNNING or SDEVLP_STOPPED) */
  pthread_t     evlp_thread;           /* event loop thread */
  SDGP_event_t* (*fp_evlp_receiver)    (struct serdisp_s* dd, SDGP_event_t* recycle);  /* receiver routine for receiving all GPI-events */
  int           (*fp_evlp_trigevents)  (struct serdisp_s* dd, SDGP_event_t* currevent);  /* triggered events (GPOs, dependend on GPIs) */
  int           (*fp_evlp_schedevents) (struct serdisp_s* dd);  /* timed events (GPOs, independent on GPIs) */

  SDGP_eventlistener_chain_t* eventlistener_chain;    /* eventlistener chain */

#endif

  int           (*fp_hnd_gpo_value)    (struct serdisp_s* dd, byte gpid, int32_t value);  /* handling function for single value gpos */
  int           (*fp_hnd_gpo_package)  (struct serdisp_s* dd, byte gpid, byte* data, int32_t length); /* -"- for data package gpos */  

#ifdef HAVE_LIBPTHREAD
  int           (*fp_hnd_gpi_enable)   (struct serdisp_s* dd, byte gpid, int enable);  /* en/disable GPI */
#endif
} SDGP_gpevset_t;


typedef struct SDTOUCH_idev_touchdevice_s {
  int       fd;                        /* ts_*: data needed for touchscreen event processing */
  byte      currtype;                  /* down, up, moving, ... */
  int       swapx;                     /* swap x-axis of touchscreen */
  int       swapy;                     /* swap y-axis of touchscreen */
  /* all ___raw[x|y]: [ ABS_[X|Y].minimum - ABS_[X|Y].maximum ] */
  int32_t   currrawx;                  /* current raw x value */
  int32_t   currrawy;                  /* current raw y value */
  int32_t   minrawx;                   /* minimum raw y value ( >= 0 ) */
  int32_t   minrawy;                   /* minimum raw y value ( >= 0 ) */
  int32_t   maxrawx;                   /* maximum raw y value ( <= ABS_X.maximum ) */
  int32_t   maxrawy;                   /* maximum raw y value ( <= ABS_Y.maximum ) */
  /* minrawx, minrawy, maxrawx, maxrawy, swapx, swapy may be changed for calibration */
} SDTOUCH_idev_touchdevice_t;


#ifdef __cplusplus
extern "C" {
#endif

#ifdef HAVE_LIBPTHREAD
/** \name Event-loop functions
  */
/*!@{*/
  int       SDEVLP_start              (struct serdisp_s* dd);
  int       SDEVLP_stop               (struct serdisp_s* dd);
  int       SDEVLP_getstatus          (struct serdisp_s* dd);

  int       SDEVLP_add_listener       (struct serdisp_s* dd, byte gpid, fp_eventlistener_t eventlistener);
  int       SDEVLP_del_listener       (struct serdisp_s* dd, byte gpid, fp_eventlistener_t eventlistener);
  int       SDEVLP_purge_listeners    (struct serdisp_s* dd, byte gpid);
  int       SDEVLP_count_listeners    (struct serdisp_s* dd, byte gpid);
/*!@}*/
#endif

/** \name GPO specific functions
  */
/*!@{*/
  byte      SDGPO_getamount           (struct serdisp_s* dd);
  byte      SDGPO_gettype             (struct serdisp_s* dd, byte gpid);

  byte      SDGPO_search              (struct serdisp_s* dd, const char* gpname);

  SDGPO_t*  SDGPO_getdescriptor       (struct serdisp_s* dd, byte gpid);

  /* type specific */
  int       SDGPO_invert              (struct serdisp_s* dd, byte gpid);  /* only supported by numeric values */
  int       SDGPO_setvalue            (struct serdisp_s* dd, byte gpid, int32_t value);
  int       SDGPO_setpackage          (struct serdisp_s* dd, byte gpid, byte* data, int32_t length);

/*!@}*/


#ifdef HAVE_LIBPTHREAD
/** \name GPI specific functions
  */
/*!@{*/
  byte      SDGPI_getamount           (struct serdisp_s* dd);
  byte      SDGPI_gettype             (struct serdisp_s* dd, byte gpid);

  byte      SDGPI_search              (struct serdisp_s* dd, const char* gpname);

  SDGPI_t*  SDGPI_getdescriptor       (struct serdisp_s* dd, byte gpid);

  int       SDGPI_isenabled           (struct serdisp_s* dd, byte gpid);
  int       SDGPI_enable              (struct serdisp_s* dd, byte gpid, int enable);

  /* type specific */
  int       SDGPI_getstreamfd         (struct serdisp_s* dd, byte gpid);

  /*int       SDGPI_receivevalue        (struct serdisp_s* dd, byte gpid, long* value);*/
  /*int       SDGPI_receivepackage      (struct serdisp_s* dd, byte gpid, byte** data, short* length);*/

/*!@}*/
#endif


#ifdef HAVE_LIBPTHREAD
/** \name Host byte order vs. network byte order conversion functions
  */
/*!@{*/
  void      SDGPT_event_header_hton   (struct SDGP_event_s* event);
  void      SDGPT_event_header_ntoh   (struct SDGP_event_s* event);
  void      SDGPT_event_payload_hton  (void* payload, int bytes, byte word_length);
  void      SDGPT_event_payload_ntoh  (void* payload, int bytes, byte word_length);
/*!@}*/
#endif


#if defined(__linux__) && defined(HAVE_LIBPTHREAD)
/** \name Input-device based touch devices
  */
/*!@{*/
  int           SDTOUCH_idev_open          (const char* idev_name, SDTOUCH_idev_touchdevice_t* touchdev);
  void          SDTOUCH_idev_close         (SDTOUCH_idev_touchdevice_t* touchdev);
  int           SDTOUCH_idev_enable        (SDTOUCH_idev_touchdevice_t* touchdev, int enable);
  SDGP_event_t* SDTOUCH_idev_evlp_receiver (serdisp_t* dd, SDTOUCH_idev_touchdevice_t* touchdev, SDGP_event_t* recycle);
/*!@}*/
#endif

#if 0
/** \name hide this (subject to change)
  * \cond 0
  */
  byte      SDGPO_getstaticamount     (const char* devicename);
  SDGPO_t*  SDGPO_getstaticdesc       (const char* devicename, byte gpid);
  byte      SDGPI_getstaticamount     (const char* devicename);
  SDGPI_t*  SDGPI_getstaticdesc       (const char* devicename, byte gpid);
/** \endcond
*/
#endif

#ifdef __cplusplus
    }
#endif
#endif /* SERDISP_GPEVENTS_H */

/*! @} */
