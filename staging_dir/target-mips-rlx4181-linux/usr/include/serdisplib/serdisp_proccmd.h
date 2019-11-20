/** \file    serdisp_proccmd.h
  *
  * \brief   Command processor
  * \date    (C) 2007-2008
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

/** \addtogroup SERDISP_PROCCMD

  \section SDPROCCMD_INTRODUCTION Introduction
  serdisp_proccmd.h offers a command processor which can be used in two ways:
  \li <b>generic command processor</b> inside programs that link serdisplib
  \li <b>echo daemon</b> provided by the library at runtime
  * @{
  */

#ifndef SERDISP_PROCCMD_H
#define SERDISP_PROCCMD_H

#include "serdisplib/serdisp_connect.h"
#include "serdisplib/serdisp_control.h"

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


/** \name Version information for command processor
  */
/*!@{*/
#define SDCMD_VERSION_MAJOR     0
#define SDCMD_VERSION_MINOR     9
/*!@}*/

/** \name Return codes
  */
/*!@{*/
#define SDCMD_RCCODE_VERSION    210
#define SDCMD_RCCODE_INFO       212
#define SDCMD_RCCODE_HELP       214
#define SDCMD_RCCODE_SUCCESS    250
#define SDCMD_RCCODE_ALREADY    251
#define SDCMD_RCCODE_ERRCMD     500
#define SDCMD_RCCODE_ERRARGS    501
#define SDCMD_RCCODE_ERRRUNT    504
#define SDCMD_RCCODE_ERRNAVAIL  505
#define SDCMD_RCCODE_ERRSIZE    509
/*!@}*/


#ifdef __cplusplus
extern "C" {
#endif
/** \name Main function
  */
/*!@{*/
  /* command processor */
  int SDCMD_proccmd          (serdisp_t* dd, char* cmdline, char* msgbuf, int msgbuf_size, int show_rccode);
/*!@}*/
#ifdef __cplusplus
    }
#endif

#endif /* SERDISP_PROCCMD_H */

/*! @} */
