/** \file    serdisp_messages.h
  *
  * \brief   Debugging, logging, and error messages and funtions
  * \date    (C) 2003-2008
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

/** \addtogroup SERDISP_MESSAGES

  \section INTRODUCTION Introduction
  serdisplib uses \b syslog for reporting debugging information, errors and warnings.\n
  Thus, \p /var/log/messages (or \p /var/adm/messages depending on operating system used) 
  may contain useful informations.

  \attention
  Always have a look into \p /var/log/messages (or \p /var/adm/messages ) if there are problems.\n
  Warnings and errors (depending on the debugging level used) are logged there by serdisplib!

  * @{
  */

#ifndef SERDISP_MESSAGES_H
#define SERDISP_MESSAGES_H

#include <syslog.h>
#include <stdio.h>

/* debugging, logging, error variables and defines */
extern int  sd_debuglevel;        /* debug level. -1: no, 0: little debugging. 2: verbose debugging */
extern int  sd_errorcode;         /* error code */
extern char sd_errormsg[255];     /* extra error message */
extern int  sd_runtimeerror;      /* runtime error occured */

extern FILE* sd_logmedium;        /* log to syslog (sd_logmedium == 0), stderr or stdout */

#define SERDISP_NOERROR             0   /* no error */
#define SERDISP_EACCES              1   /* access not allowed */
#define SERDISP_ENXIO               2   /* no such device or address */
#define SERDISP_EBUSY               3   /* resource busy */
#define SERDISP_ENOTSUP             4   /* display / functionality not supported */
#define SERDISP_EDEVNOTSUP          5   /* device / ioport not supported by display */
#define SERDISP_EINVAL              6   /* invalid argument */
#define SERDISP_ENOPROTOOPT         7   /* protocol not available */
#define SERDISP_ENOMEM             12   /* out of memory */
#define SERDISP_EPIPE              32   /* broken pipe */
#define SERDISP_ERANGE             34   /* math result not representable */
#define SERDISP_EMALLOC            98   /* error allocating memory */
#define SERDISP_ERUNTIME           99   /* error while runtime */


#define SD_LOG_SYSLOG               0   /* debug messages -> syslog */
#define SD_LOG_STDERR               1   /* debug messages -> stderr */
#define SD_LOG_STDOUT               2   /* debug messages -> stdout */

#define SD_LVL_WARN                 0   /* only warning infos */
#define SD_LVL_INFO                 1   /* more verbose debugging */
#define SD_LVL_VERBOSE              2   /* verbose debugging */

#define sd_error(_errcode, args...)  { sd_errorcode=(_errcode); snprintf(sd_errormsg, 254, args); syslog(LOG_ERR,  args); }
#define sd_debug(_lvl, args...)      { if (sd_debuglevel >= (_lvl)) {\
                                         if (!sd_logmedium) syslog(LOG_INFO, args); \
                                         else {\
                                           fprintf(sd_logmedium, args); fprintf(sd_logmedium, "\n");\
                                         }\
                                       }\
                                     }

/* //MAF's former srvmsg() */
#define sd_srvmsg(_prio, args...)    { syslog(_prio, args); \
                                       fprintf(( (_prio) < LOG_INFO) ? stderr : stdout, args);\
                                       fprintf(( (_prio) < LOG_INFO) ? stderr : stdout, "\n");\
                                     }

/** \hideinitializer
  * \brief   tests if a runtime error has occured
  *
  * Tests if a runtime error has occured (and thus display drawing has been stopped by the library)
  *
  * \retval  0     no runtime error
  * \retval  1     a runtime error occured
  *
  * \attention  
  *    Display drawing is stopped after a runtime error and may be reset using serdisp_reset() or serdisp_fullreset().
  */
#define sd_runtime_error()           (sd_runtimeerror)

/** \hideinitializer
  * \brief   gets error message of last unsuccessful action
  *
  * Gets error message of last unsuccessful action
  *
  * \return  error message
  */
#define sd_geterrormsg()             (sd_errormsg)

/** \hideinitializer
  * \brief   gets the current debug level
  *
  * Gets the current debug level
  *
  * \return  debug level currently used
  */
#define sd_getdebuglevel()           (sd_debuglevel)

/** \hideinitializer
  * \brief   sets a new debug level
  *
  * Sets a new debug level
  *
  * \param  _level   constant which sets the debug level:<pre>
  SD_LVL_WARN    = 0  .. only write warning informations
  SD_LVL_INFO    = 1  .. more verbose debugging
  SD_LVL_VERBOSE = 2  .. verbose debugging </pre>
  *
  * \b Example:
  * \code
  * sd_setdebuglevel(SD_LVL_VERBOSE);
  * \endcode
  */
#define sd_setdebuglevel(_level)     (sd_debuglevel = ( (_level) > 2) ? SD_LVL_VERBOSE : ( (_level) < 0) ? SD_LVL_WARN : (_level))

/** \hideinitializer
  * \brief   sets the log medium for debug messages
  *
  * Sets the log medium for debug messages
  *
  * \param  _medium   constant which sets the log medium used for debugging information:<pre>
  SD_LOG_SYSLOG .. syslog
  SD_LOG_STDERR .. stderr
  SD_LOG_STDOUT .. stdout </pre>
  *
  * \b Example: \n
  * enables info debugging and writes debugging information to \p stderr instead of using syslog:
  * \code
  * sd_setdebuglevel(SD_LVL_INFO);
  * sd_setlogmedium(SD_LOG_STDERR);
  * \endcode
  */
#define sd_setlogmedium(_medium)     { if ((_medium) == 0) sd_logmedium = (FILE*)0; \
                                       else sd_logmedium = ( (_medium) >= SD_LOG_STDOUT) ? stdout : stderr; }


/*#ifdef __cplusplus
extern "C" {
#endif
#ifdef __cplusplus
    }
#endif
*/

#endif /* SERDISP_MESSAGES_H */

/*! @} */
