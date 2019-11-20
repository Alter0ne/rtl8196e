/** \file    serdisp_fctptr.h
  *
  * \brief   Initialisation of additional libraries and function pointers
  * \date    (C) 2010-2017
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

/** \addtogroup SERDISP_FCTPTR

  \section SDFP_INTRODUCTION Introduction
  serdisp_fctptr.h offers functions for initialising additional libraries and function pointers
  and requesting information about the additional libraries.


  * @{
  */


#ifndef SERDISP_FCTPTR_H
#define SERDISP_FCTPTR_H

/* define 'byte' if not available yet */
#ifndef byte
  #define byte unsigned char
#endif

#define SDFCTPTR_LIBUSB      1
#define SDFCTPTR_PTHREAD     2
#define SDFCTPTR_NETSOCK     3
#define SDFCTPTR_LIBSDL      4
#define SDFCTPTR_LIBDLO      5


#ifdef __cplusplus
extern "C" {
#endif
   void SDFCTPTR_init        (void);
   int  SDFCTPTR_checkavail  (int libID);
   void SDFCTPTR_cleanup     (void);
#ifdef __cplusplus
    }
#endif


#ifdef HAVE_LIBPTHREAD
 #include <pthread.h>

extern int             (*fp_pthread_create)       (pthread_t* thread, const pthread_attr_t* attr, 
                                                   void* (*start_routine)(void *), void* arg); 
extern int             (*fp_pthread_join)         (pthread_t thread, void** value_ptr);
extern int             (*fp_pthread_cancel)       (pthread_t thread);
extern void            (*fp_pthread_exit)         (void* value_ptr);
extern int             (*fp_pthread_mutex_lock)   (pthread_mutex_t* mutex);
extern int             (*fp_pthread_mutex_trylock)(pthread_mutex_t* mutex);
extern int             (*fp_pthread_mutex_unlock) (pthread_mutex_t* mutex);
#endif /* HAVE_LIBPTHREAD */


#ifdef HAVE_LIBUSB
 #include <usb.h>

extern void            (*fp_usb_init)             (void);
extern usb_dev_handle* (*fp_usb_open)             (struct usb_device* dev);
extern int             (*fp_usb_close)            (usb_dev_handle* dev);
extern int             (*fp_usb_reset)            (usb_dev_handle* dev);
extern int             (*fp_usb_interrupt_read)   (usb_dev_handle* dev, int ep, char* bytes, int size, int timeout);
extern int             (*fp_usb_release_interface)(usb_dev_handle* dev, int interface);
extern int             (*fp_usb_find_busses)      (void);
extern int             (*fp_usb_find_devices)     (void);
extern struct usb_bus* (*fp_usb_get_busses)       (void);
extern int             (*fp_usb_claim_interface)  (usb_dev_handle* dev, int interface);

extern int             (*fp_usb_bulk_read)        (usb_dev_handle* dev, int ep, char* bytes, int size, int timeout);
extern int             (*fp_usb_bulk_write)       (usb_dev_handle* dev, int ep, const char* bytes, int size, int timeout);
extern int             (*fp_usb_control_msg)      (usb_dev_handle* dev, int requesttype, int request,
                                                   int value, int index, char *bytes, int size, int timeout);
extern int             (*fp_usb_clear_halt)       (usb_dev_handle *dev, unsigned int ep);

extern int             (*fp_usb_set_altinterface) (usb_dev_handle *dev, int alternate);
extern int             (*fp_usb_set_configuration)(usb_dev_handle *dev, int configuration);
extern int             (*fp_usb_get_string_simple)(usb_dev_handle *dev, int index, char *buf, size_t buflen);

extern int             (*fp_usb_detach_kernel_driver_np) (usb_dev_handle* dev, int interface);
#endif /* HAVE_LIBUSB */


#ifdef HAVE_LIBSDL
 #ifdef HAVE_SDL_SDL_H
  #include "SDL/SDL.h"
 #else
  #include "SDL.h"
 #endif

 #include <inttypes.h>

 #define dfn_SDL_MUSTLOCK                  SDL_MUSTLOCK

extern  int            (*fp_SDL_Init)             (uint32_t);
extern  SDL_Surface*   (*fp_SDL_SetVideoMode)     (int, int, int, uint32_t);
extern  char*          (*fp_SDL_GetError)         (void);
extern  void           (*fp_SDL_WM_SetCaption)    (const char*, const char*);
extern  int            (*fp_SDL_LockSurface)      (SDL_Surface*);
extern  void           (*fp_SDL_UnlockSurface)    (SDL_Surface*);
extern  int            (*fp_SDL_Flip)             (SDL_Surface*);
extern  void           (*fp_SDL_FreeSurface)      (SDL_Surface*);
extern  uint32_t       (*fp_SDL_MapRGB)           (SDL_PixelFormat*, uint8_t, uint8_t, uint8_t);
extern  void           (*fp_SDL_PumpEvents)       (void);
extern  uint8_t        (*fp_SDL_GetMouseState)    (int* x, int* y);
extern  int            (*fp_SDL_PollEvent)        (SDL_Event*);
extern  int            (*fp_SDL_WaitEvent)        (SDL_Event*);
extern  void           (*fp_SDL_Quit)             (void);
#endif /* HAVE_LIBSDL */


#ifdef HAVE_LIBDLO
 #include "libdlo.h"

 #include <inttypes.h>
extern  const char*    (*fp_dlo_strerror)         (const dlo_retcode_t);
extern  dlo_retcode_t  (*fp_dlo_init)             (const dlo_init_t);
extern  dlo_retcode_t  (*fp_dlo_final)            (const dlo_final_t);
#if 0
extern  dlo_dev_t      (*fp_dlo_lookup_device)    (struct usb_device*);
#endif
extern  dlo_devlist_t* (*fp_dlo_enumerate_devices)(void);
extern  dlo_dev_t      (*fp_dlo_claim_first_device)(const dlo_claim_t, const uint32_t);
extern  dlo_dev_t      (*fp_dlo_claim_device)     (const dlo_dev_t, const dlo_claim_t, const uint32_t);
extern  dlo_retcode_t  (*fp_dlo_release_device)   (const dlo_dev_t);
extern  dlo_devinfo_t* (*fp_dlo_device_info)      (const dlo_dev_t);
extern  dlo_mode_t*    (*fp_dlo_get_mode)         (const dlo_dev_t);
extern  dlo_retcode_t  (*fp_dlo_set_mode)         (const dlo_dev_t, const dlo_mode_t*);
extern  dlo_retcode_t  (*fp_dlo_fill_rect)        (const dlo_dev_t, const dlo_view_t*, const dlo_rect_t*, const dlo_col32_t);
extern  dlo_retcode_t  (*fp_dlo_copy_host_bmp)    (const dlo_dev_t, const dlo_bmpflags_t,
                                                   const dlo_fbuf_t*, const dlo_view_t*, const dlo_dot_t*);
#endif /* HAVE_LIBDLO */


#ifdef HAVE_NETSOCK_LIBS
 #include <sys/types.h>
 #include <sys/socket.h>

 #include <netdb.h>

 #include <arpa/inet.h>
 #include <netinet/in.h>
 #include <sys/un.h>

extern int             (*fp_accept)               (int sockfd, struct sockaddr *addr, socklen_t *addrlen);
extern int             (*fp_bind)                 (int sockfd, const struct sockaddr *my_addr, socklen_t addrlen);
extern int             (*fp_connect)              (int sockfd, const struct sockaddr *serv_addr, socklen_t addrlen);
extern int             (*fp_listen)               (int sockfd, int backlog);
extern ssize_t         (*fp_recv)                 (int s, void *buf, size_t len, int flags);
extern ssize_t         (*fp_send)                 (int s, const void *buf, size_t len, int flags);
extern int             (*fp_setsockopt)           (int s, int level, int optname, const void *optval, socklen_t optlen);
extern int             (*fp_shutdown)             (int s, int how);
extern int             (*fp_socket)               (int domain, int type, int protocol);

extern void            (*fp_endhostent)           (void);
extern struct hostent* (*fp_gethostbyaddr)        (const void *addr, int len, int type);
extern struct hostent* (*fp_gethostbyname)        (const char *name);
extern int             (*fp_getnameinfo)          (const struct sockaddr *sa, socklen_t salen,
                                                   char *host, size_t hostlen,
                                                   char *serv, size_t servlen, int flags);

extern uint32_t        (*fp_htonl)                (uint32_t hostlong);
extern uint16_t        (*fp_htons)                (uint16_t hostshort);
extern uint32_t        (*fp_ntohl)                (uint32_t netlong);
extern uint16_t        (*fp_ntohs)                (uint16_t netshort);
extern in_addr_t       (*fp_inet_addr)            (const char *cp);
#endif /* HAVE_NETSOCK_LIBS */


#endif /* SERDISP_FCTPTR_H */

/*! @} */
