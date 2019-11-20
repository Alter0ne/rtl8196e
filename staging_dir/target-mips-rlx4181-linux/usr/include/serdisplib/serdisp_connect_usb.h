/*
 *************************************************************************
 *
 * serdisp_connect_usb.h
 * routines for accessing usb-devices
 *
 *************************************************************************
 *
 * copyright (C) 2003-2012  wolfgang astleitner
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


#ifndef SERDISP_CONNECT_USB_H
#define SERDISP_CONNECT_USB_H

#include <usb.h>

/* define 'byte' if not available yet */
#ifndef byte
  #define byte unsigned char
#endif


typedef struct serdisp_usbdev_s {
  struct usb_device*       dev;
  struct usb_dev_handle*   usb_dev;
  int                      devID;        /* id in serdisp_usbdevices[] */
  char*                    stream;
  int                      streamsize;
  int                      streampos;
  int                      packetsize;
  int                      laststatus;   /* freely usable status info */
  int32_t                  store;        /* 4 bytes for freely usable storage */
  unsigned char            claimed;      /* bitmask storing claiming-status for max. 8 channels */
  int                      out_ep;       /* endpoint address for sending data to the interface */
  int                      in_ep;        /* endpoint address for receiving data from the interface */
  int                      read_timeout;
  int                      write_timeout;
} serdisp_usbdev_t;


#ifdef __cplusplus
extern "C" {
#endif
  serdisp_CONN_t* SDCONNusb_open           (const char sdcdev[]);
  void            SDCONNusb_close          (serdisp_CONN_t* sdcd);
  void            SDCONNusb_write          (serdisp_CONN_t* sdcd, uint32_t data, byte flags);
  void            SDCONNusb_writedelay     (serdisp_CONN_t* sdcd, uint32_t data, byte flags, long ns);
  uint32_t        SDCONNusb_read           (serdisp_CONN_t* sdcd, byte flags);
  int             SDCONNusb_readstream     (serdisp_CONN_t* sdcd, byte* buf, int count);
  void            SDCONNusb_commit         (serdisp_CONN_t* sdcd);  
  void            SDCONNusb_usleep         (serdisp_CONN_t* sdcd, long usec);
  /* protected functions */
  int             SDCONNusb_confinit       (serdisp_CONN_t* sdcd);
#ifdef __cplusplus
    }
#endif


#endif /* SERDISP_CONNECT_H */
