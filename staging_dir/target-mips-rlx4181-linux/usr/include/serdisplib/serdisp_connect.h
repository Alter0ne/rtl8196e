/** \file    serdisp_connect.h
  *
  * \brief   Functions for accessing supported output devices (parport, serial device, ..)
  * \date    (C) 2003-2014
  * \author  wolfgang astleitner (mrwastl@users.sourceforge.net)
  */

/*
 *************************************************************************
 *
 * parport part initially based on:
 *   http://www.thiemo.net/projects/orpheus/optrex/
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

/** \addtogroup SERDISP_CONNECT

  \section SDC_INTRODUCTION Introduction
  serdisp_connect.h offers functions for opening and closing output devices using a descriptor ('sdcd').

  \section FEATURES Features

  \subsection OUTPUTDEVS Output devices
    \li <b>parallel port</b>
    \li <b>serial port</b> (I2C, RS232)
    \li <b>USB-to-serial adaptors</b> (I2C-devices only)
    \li <b>USB-modules</b>
    \li other output devices like <b>SDL</b>, <b>framebuffer</b>


  \subsection ACCESSMETHODS Supported access methods
  <b>Nota Bene:</b> ioctl vs. direct-io is only valid for displays connected to parallel or serial ports.
    \li <b>ioctl-calls</b>: output devices are accessed using devices
    \li <b>direct I/O</b>: output devices are accessed using port addresses (direct I/O is only supported with linux and i386/x86_64 architectures)

  The default output-method uses \em 'ioctl'-calls using device names (eg: \p /dev/parport0).

  serdisplib also supports using \em direct-IO (using port-addresses and inline-assembler).

  \subsubsection PROSCONS Pros and contras
    \li <b>ioctl</b>: \n
          <tt>+ </tt> not restricted to root-only (users qualified for eg. \p /dev/parport0 may control a display connected to it) \n
          <tt>+ </tt> universal (usable with more unix-derivats) \n
          <tt>- </tt> slower than direct I/O\n\n
    \li <b>direct I/O</b>: \n
          <tt>+ </tt> faster (at least in theory) \n
          <tt>- </tt> root-only \n
          <tt>- </tt> i386-compliant architectures only

  \subsection TESTEDCOMPILERS Tested compilers
    \li gcc 2.96
    \li gcc 3.x
    \li gcc 4.0 (tested using Fedora Core 4)
    \li gcc 4.5 - 4.7.2
    \li clang 2.9 - 3.2

  \subsection OPSYS Operating systems (tested)
    \li <b>Linux</b> (x86_32, x86_64, arm; direct I/O only with x86 architectures)
    \li <b>Solaris 10</b> (only x86 tested. devices: \p ecpp and \p cua )
    \li <b>FreeBSD</b> (only x86 and parport tested)

  \subsection OPSYSNOTTESTED Operating systems (untested)
    \li <b>Linux / non-x86</b>
    \li <b>FreeBSD/ non-86</b>
    \li <b>OpenBSD</b>

  * @{
  */


#ifndef SERDISP_CONNECT_H
#define SERDISP_CONNECT_H

#include <termios.h>
#include <signal.h>
#include <stdint.h>

/* define 'byte' if not available yet */
#ifndef byte
  #define byte unsigned char
#endif


/* enable multi-packet frames for cypress */
#ifndef HARDWARE_RPi
  #define SDCONN_ENABLE_CYPRESS_MPFRAMES 1
#endif


/* connection types (for bit-field) */
#define SERDISPCONNTYPE_PARPORT           0x0001
#define SERDISPCONNTYPE_SERRAW            0x0002
#define SERDISPCONNTYPE_I2C               0x0004
#define SERDISPCONNTYPE_IOW24             0x0008
#define SERDISPCONNTYPE_SERVER            0x0010
#define SERDISPCONNTYPE_HIDDEV            0x0020
#define SERDISPCONNTYPE_RS232             0x0080
#define SERDISPCONNTYPE_OUT               0x8000

/* short forms for connection types */
#define SDCT_PP                           SERDISPCONNTYPE_PARPORT
#define SDCT_SRAW                         SERDISPCONNTYPE_SERRAW
#define SDCT_I2C                          SERDISPCONNTYPE_I2C
#define SDCT_IOW24                        SERDISPCONNTYPE_IOW24
#define SDCT_HIDDEV                       SERDISPCONNTYPE_HIDDEV
#define SDCT_RS232                        SERDISPCONNTYPE_RS232
#define SDCT_OUT                          SERDISPCONNTYPE_OUT

/* protocols */
#define SDPROTO_DONTCARE                  0x0000  /* default */
#define SDPROTO_EMULATION                 0x0001
#define SDPROTO_I2C                       0x0002
#define SDPROTO_RS232                     0x0004
#define SDPROTO_SPI                       0x0010

/* hardware types */
#define SDHWT_ALL                         0xFFFF  /* valid for all hw-types */

#define SDHWT_PARSER                      0x0001  /* parallel or serial port (or usb->serial adapter) */

#define SDHWT_REMOTE                      0x0002  /* remote protocol */

#define SDHWT_OUT                         0x0080  /* dummy protocol for file / direct / ... output */

/* 0xFF00 reserved for usb-devices */
#define SDHWT_USB                         0xFF00  /* mask that hw needs usb routines */
#define SDHWT_USBFTDI2232                 0x0100  /* FTDI: 01 - 0F */
#define SDHWT_USBFTDI245                  0x0200
#define SDHWT_USBIOW24                    0x1000  /* c't maeusekino usb-display */
#define SDHWT_USBALPHACOOL                0x2000  /* alphacool 240x128 or 200x64 usb display */
#define SDHWT_USBL4ME5I                   0x3000  /* linux4media E-5i internal board + 128x64 LCD */
#define SDHWT_USB13700                    0x4000  /* USB13700 module by henri (lcdinfo.com) */
#define SDHWT_USBSDCMEGTRON               0x5000  /* smart-display-company/megtron 240x128 LCD */
#define SDHWT_USBL4M320T                  0x6000  /* linux4media 240x320 USB display module w/ touch screen */
#define SDHWT_USBGLCD2USB                 0x7000  /* glcd2usb based devices */

/* *************************** *
 * constants for parallel port *
 * *************************** */

/* base+0 ... data bits (in/out) */
/* usb/ftdi: channel A: AD0 - AD7*/
#define SD_PP_D0    0x00000001
#define SD_PP_D1    0x00000002
#define SD_PP_D2    0x00000004
#define SD_PP_D3    0x00000008
#define SD_PP_D4    0x00000010
#define SD_PP_D5    0x00000020
#define SD_PP_D6    0x00000040
#define SD_PP_D7    0x00000080

/* base+1 ... status bits (in) */
#define SD_PP_S0    0x00000100
#define SD_PP_S1    0x00000200
#define SD_PP_S2    0x00000400
#define SD_PP_S3    0x00000800  /* pin 15: ERROR */
#define SD_PP_S4    0x00001000  /* pin 13: SLCT */
#define SD_PP_S5    0x00002000  /* pin 12: PE */
#define SD_PP_S6    0x00004000  /* pin 10: ACK */
#define SD_PP_S7    0x00008000  /* pin 11: /BUSY */

/* base+2 ... control bits (out) */
/* usb/ftdi: channel B: BD0 - BD7*/
#define SD_PP_C0    0x00010000  /* pin  1: /STROBE */
#define SD_PP_C1    0x00020000  /* pin 14: /LINEFD */
#define SD_PP_C2    0x00040000  /* pin 16: INIT */
#define SD_PP_C3    0x00080000  /* pin 17: /SELECT */
#define SD_PP_C4    0x00100000
#define SD_PP_C5    0x00200000  /* enable bi-directional port */
#define SD_PP_C6    0x00400000
#define SD_PP_C7    0x00800000

#define SD_PP_BIDI  0x20        /* bi-di bit 5 ( ==  SD_PP_C5 >> 16 ) */
/* active low signals on a centronics parallel port */
#define SD_PP_ACTIVELOW  0x000B8000

/* input/output flags  (io_flags) */
#define SD_PP_WRITEDB        1  /* parport base+0 (data byte) needs to be written */
#define SD_PP_WRITECB        2  /* parport base+2 (control byte) -"-              */
#define SD_PP_READDB         4  /* parport base+0 (data byte) needs to be read */
#define SD_PP_READSB         8  /* parport base+1 (status byte) -"- */
#define SD_PP_READCB        16  /* parport base+2 (control byte) -"- */

#define SD_SP_TXDON          1  /* serport: keep TxD high for gaining power */

/* no constants for serial port (only one byte used) */


#define SD_MAX_SUPP_SIGNALS 32   /* maximum supported signals */


/* device health */
#define SD_STATUS_OK         0   /* device is ok */
#define SD_STATUS_ERROR      1   /* runtime error - device not functioning */
#define SD_STATUS_RESETTING  2   /* device currently being reset */


/* needed by iowarrior, copied from iowarrior.h ((C) codemercenaris) */
#define USB_VENDOR_ID_CODEMERCS	1984
/* low speed iowarrior */
#define USB_DEVICE_ID_CODEMERCS_IOW40	0x1500
#define USB_DEVICE_ID_CODEMERCS_IOW24	0x1501

#define CODEMERCS_MAGIC_NUMBER	0xC0	// like COde Mercenaries

/* Define the ioctl commands for reading and writing data */
#define IOW_WRITE	_IOW(CODEMERCS_MAGIC_NUMBER, 1, long)
#define IOW_READ	_IOW(CODEMERCS_MAGIC_NUMBER, 2, long)
#define IOW_GETINFO _IOR(CODEMERCS_MAGIC_NUMBER, 3, struct iowarrior_info_s)

#define IOW_REQ_GET_REPORT     0x01
#define IOW_REQ_SET_REPORT     0x09

#define IOW_I2C_ENABLE_REPORT  0x01
#define IOW_I2C_WRITE_REPORT   0x02
#define IOW_I2C_READ_REPORT    0x03
#define IOW_LCD_ENABLE_REPORT  0x04
#define IOW_LCD_WRITE_REPORT   0x05
#define IOW_LCD_READ_REPORT    0x06
#define IOW_RC5_ENABLE_REPORT  0x0c


#define IOW_FILLSTREAM(_stream, _rid, _val, _n0, _n1, _n2, _n3, _n4, _n5) \
        (_stream)[0] = (_rid); \
        (_stream)[1] = (_val); \
        (_stream)[2] = (_n0); \
        (_stream)[3] = (_n1); \
        (_stream)[4] = (_n2); \
        (_stream)[5] = (_n3); \
        (_stream)[6] = (_n4); \
        (_stream)[7] = (_n5);

/* generic stream device (eg. usb) */
typedef struct gen_stream_device_s {
  byte*                    stream;
  int                      streamsize;
  int                      streampos;
  int                      packetsize;
  int                      laststatus;   /* freely usable status info */
  int32_t                  store;        /* 4 bytes for freely usable storage */  
} gen_stream_device_t;


/* struct containing all infos and other things concerning a display */
typedef struct serdisp_CONN_s {
  uint16_t conntype;                  /* one of SERDISPCONNTYPE_* */
  uint16_t hardwaretype;              /* one of SDHWT_*. default: SDHWT_SERPAR */
  uint16_t protocol;                  /* one of SDPROTP_*. default: SDPROTO_DONTCARE */

#ifndef __cplusplus  /* qt/moc is having problems with the label 'signals' (name clash) */
  uint32_t signals[SD_MAX_SUPP_SIGNALS];  /* lookup vector for signals */
#else
  uint32_t _unusable[SD_MAX_SUPP_SIGNALS];/* not accessible for c++ programs (library is in c anyways) */
#endif
  uint32_t signals_permon;            /* signals forced to be permanently high */
  uint32_t signals_invert;            /* signals to be inverted (because they're inverted by some hardware or somethink like that
                                         (default: 0L -> no inversions) */


  byte io_flags_readstatus;
  byte io_flags_writedata;
  byte io_flags_writecmd;

  byte io_flags_default;              /* use this as default if io_flags_readstatus/writedata/writecmd not set */


  /* parport and serport variables */
  int  pp_bidi_supported;             /* parport: 1: bidirectional access supported, 0: not supported */
  byte pp_ctrlbits_saved;             /* parport: control bits saved (state after initialising the parport) */

  int                directIO;        /* 1: direct IO, 0: ioctl-calls */
  unsigned short int port;            /* port if access using direct io */
  int                fd;              /* file-descriptor if access using ioctl-calls */

  /* serport only */
  struct termios     termstate_bkp;   /* backuped state of serport */
  struct termios     termstate;       /* state of serport */

  char*              sdcdev;          /* unprocessed device name (including connection type) */

  int                debug_count;     /* counter for debugging purposes */

  void*              extra;           /* extra device data */
  
  int                device_status;   /* one of SD_STATUS_OK, SD_STATUS_ERROR, SD_STATUS_RESETTING */
  uint32_t           timestamp;       /* if resetting device: timestamp (in seconds) of last test. else: 0 */

  int                needs_confinit;  /* non-default configuration needed before 1st commit or read/write operation */
  union {
    struct {
      byte           framelen    :4;  /* SPI frame length intern = SPI frame length - 4 ([0 - 15] -> [4 - 19]) */
      byte           cpol        :1;  /* SPI clock polarity (0 or 1) */
      byte           cpha        :1;  /* SPI clock phase (0 or 1) */
      byte           data_high   :1;  /* 1: data = active high, 0: command = active high */
      byte           dc_extsig   :1;  /* 0: D/C bit part of SPI stream, 1: separate D/C signal wire */
      byte           prescaler;       /* SPI prescaler [2 - 254] */
      unsigned short divider;         /* SPI divider */
    } spi;
    struct {
      unsigned int   baudrate;        /* define according to cfsetspeed(), eg B500000 */

      byte           c_cs8_decr  :2;  /* databits: 8 - cs8_decr (default: 0 == 8 databits, 1 == 7, 2 == 6, 3 == 5) */
      byte           c_cstopb    :1;  /* 0: 1 stop bits (default), 1: 2 stop bits */
      byte           c_parenb    :1;  /* 0: don't enable parent bit; 1: enable parent bit */
      byte           c_parodd    :1;  /* 0: even parity; 1: odd parity */
      byte           c_cread     :1;  /* 0: don't enable receiver; 1: enable receiver */
      byte           c_local     :1;  /* 0: no local line; 1: local line */
      byte           c_rtscts    :1;  /* 0: no hw flow control; 1: enable hw flow control (if supp. by plattform) */

      byte           c_set_vmin  :1;  /* 0: don't change, 1: change c_cc[VMIN] to value in c_cc_vmin */
      byte           c_set_vtime :1;  /* 0: don't change, 1: change c_cc[VTIME] to value in c_cc_vtime */

      byte           c_cc_vmin;       /* value for c_cc[VMIN] (only changed if c_set_VMIN == 1) */
      byte           c_cc_vtime;      /* value for c_cc[VTIME] (only changed if c_set_vtime == 1) */
    } rs232;
  };
} serdisp_CONN_t;



typedef struct serdisp_conntype_s {
  char*    connname;
  uint16_t conntype;
  char*    description;
} serdisp_conntype_t;



typedef struct serdisp_wiresignal_s {
  uint16_t conntype;
  char*    signalname;
  int      activelow;
  char     cord;           /* command 'C' or data 'D' signal */
  int      index;
} serdisp_wiresignal_t;



typedef struct serdisp_wiredef_s {
  int      id;
  uint16_t conntype;
  char*    name;
  char*    definition;
  char*    description;
} serdisp_wiredef_t;


#ifdef __cplusplus
extern "C" {
#endif
  int             SDCONN_isavailable       (const char sdcdev[]);

/** \name Accessing an output device
  */
/*!@{*/
  serdisp_CONN_t* SDCONN_open              (const char sdcdev[]);
  void            SDCONN_close             (serdisp_CONN_t* sdcd);
/*!@}*/
/** \name Deprecated functions
  */
/*!@{*/  
  serdisp_CONN_t* SDCONN_import_PP         (int directIO, int hport);
/*!@}*/

  int             SDCONN_getstatus         (serdisp_CONN_t* sdcd);

  void            SDCONN_write             (serdisp_CONN_t* sdcd, uint32_t data, byte flags);
  void            SDCONN_writedelay        (serdisp_CONN_t* sdcd, uint32_t data, byte flags, long ns);
  uint32_t        SDCONN_read              (serdisp_CONN_t* sdcd, byte flags);
  int             SDCONN_readstream        (serdisp_CONN_t* sdcd, byte* buf, int count);
  void            SDCONN_commit            (serdisp_CONN_t* sdcd);
  void            SDCONN_usleep            (serdisp_CONN_t* sdcd, long usec);

  int             SDCONN_getsignalindex    (const char str[], uint16_t conntype, uint16_t hardwaretype);
  uint32_t        SDCONN_getsignalvalue    (int idx);
  int             SDCONN_issignalacticelow (int idx);
  char*           SDCONN_getsignalname     (int idx);
  int             SDCONN_isactivelow       (uint32_t signal, uint16_t conntype, uint16_t hardwaretype);

#ifdef __cplusplus
    }
#endif

#endif /* SERDISP_CONNECT_H */

/*! @} */
