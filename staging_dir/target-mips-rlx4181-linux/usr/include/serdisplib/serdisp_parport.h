/*
 *************************************************************************
 *
 * serdisp_parport.h
 * routines for accessing a parport device
 *
 *************************************************************************
 *
 * copyright (C) 2003-2006  wolfgang astleitner
 * email     mrwastl@users.sourceforge.net
 *
 *************************************************************************
 *
 * initially based on:
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


#ifndef SERDISP_PARPORT_H
#define SERDISP_PARPORT_H

#warning serdisp_parport.h, PP_open, PP_close and PP_write are deprecated
#warning please use serdisp_connection.h, SDCONN_open, SDCONN_close and SDCONN_write instead!

#include "serdisp_connect.h"

#define ENABLE_DEFINES

#ifdef ENABLE_DEFINES

#ifndef serdisp_PP_s
#define serdisp_PP_s serdisp_CONN_s
#endif

#ifndef serdisp_PP_t
#define serdisp_PP_t serdisp_CONN_t
#endif

#ifndef fp_PP_write
#define fp_PP_write fp_SDCONN_write
#endif

#ifndef PP_open
#define PP_open SDCONN_open
#endif

#ifndef PP_close
#define PP_close SDCONN_close
#endif

#ifndef PP_write
#define PP_write SDCONN_write
#endif

#ifndef PP_import
#define PP_import SDCONN_import_PP
#endif

#endif # ENABLE_DEFINES

#endif /* SERDISP_PARPORT_H */
