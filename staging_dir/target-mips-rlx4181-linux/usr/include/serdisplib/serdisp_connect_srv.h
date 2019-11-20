/*
 *************************************************************************
 *
 * serdisp_connect_srv.h
 * routines for accessing remote devices
 *
 *************************************************************************
 *
 * copyright (C) 2006       //MAF
 *
 * additional maintenance and enhancements:
 * copyright (C) 2006-2012  wolfgang astleitner
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


#ifndef SERDISP_CONNECT_SRV_H
#define SERDISP_CONNECT_SRV_H

#include "serdisplib/serdisp_srvtools.h"




typedef struct {
  SERDISP_SRVCON  *con;
} serdisp_srvdev_t;



#ifdef __cplusplus
extern "C" {
#endif
  serdisp_CONN_t* SDCONNsrv_open           (const char sdcdev[]);
  void            SDCONNsrv_close          (serdisp_CONN_t* sdcd);
  void            SDCONNsrv_write          (serdisp_CONN_t* sdcd, uint32_t data, byte flags);
  void            SDCONNsrv_writedelay     (serdisp_CONN_t* sdcd, uint32_t data, byte flags, long ns);
  uint32_t        SDCONNsrv_read           (serdisp_CONN_t* sdcd, byte flags);
  int             SDCONNsrv_readstream     (serdisp_CONN_t* sdcd, byte* buf, int count);
  void            SDCONNsrv_commit         (serdisp_CONN_t* sdcd);  
#ifdef __cplusplus
    }
#endif




#endif /* SERDISP_CONNECT_SRV_H */
