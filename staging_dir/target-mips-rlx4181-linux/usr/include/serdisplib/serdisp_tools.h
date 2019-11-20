/** \file    serdisp_tools.h
  *
  * \brief   Common functions
  * \date    (C) 2003-2014
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

/** \addtogroup SERDISP_TOOLS

  \section SDTOOLS_INTRODUCTION Introduction
  serdisp_tools.h provides common routines that are used in different parts of serdisplib.

  \attention These functions are \em NOT part of the API of serdisplib. \n
  Programs should use functions defined in \ref SERDISP_CONTROL  "serdisp_control.h".
  * @{
  */

#ifndef SERDISP_TOOLS_H
#define SERDISP_TOOLS_H

#include "serdisp_control.h"
#include <sys/time.h>

/* define 'byte' if not available yet */
#ifndef byte
  #define byte unsigned char
#endif

#ifdef __cplusplus
extern "C" {
#endif

/** \name Drawing functions
  */
/*!@{*/
  void     sdtools_generic_rotate      (serdisp_t* dd);
  void     sdtools_generic_setsdpixel  (serdisp_t* dd, int x, int y, SDCol sdcol);
  SDCol    sdtools_generic_getsdpixel  (serdisp_t* dd, int x, int y);
  void     sdtools_generic_setsdpixel_greyhoriz  (serdisp_t* dd, int x, int y, SDCol sdcol);
  SDCol    sdtools_generic_getsdpixel_greyhoriz  (serdisp_t* dd, int x, int y);
/*!@}*/


/** \name System functions
  */
/*!@{*/
  void     sdtools_nsleep              (long ns);
  void*    sdtools_malloc              (size_t size);
  byte     sdtools_reversebits         (byte b);
/*!@}*/


/** \name Conversion functions
  */
/*!@{*/
  int      sdtools_contrast_norm2hw    (serdisp_t* dd, int normval);
  int      sdtools_contrast_hw2norm    (serdisp_t* dd, int hwval);
  int      sdtools_rotate_deg2intern   (serdisp_t* dd, int degval);
  int      sdtools_rotate_intern2deg   (serdisp_t* dd, int irepval);

  byte     sdtools_dec2bcd             (byte num);
/*!@}*/


/** \name String processing
  */
/*!@{*/
  char*    sdtools_strncpy             (char *dest, const char *src, size_t n);
  char*    sdtools_strlefttrim         (const char* str);
  int      sdtools_isinelemlist        (const char* elemlist, const char* str, int len);
  char*    sdtools_nextpattern         (const char* str, char delim, int* len, int* border);
  int      sdtools_ismatching          (const char* str1, int len1, const char* str2, int len2);
  int      sdtools_strtrimmedlen       (const char* str, int len);
  int      sdtools_strtol              (const char* str, char delim, int base, long* value);
  int      sdtools_strtosd             (const char* str, char delim, double* value);
/*!@}*/


/** \name Calculation functions
  */
/*!@{*/
  void     sdtools_init_bbox           (serdisp_t* dd, int value);
  int      sdtools_calc_bbox           (serdisp_t* dd, int* xt, int* yt, int* xb, int* yb);
/*!@}*/


/** \name Math function replacements which do not require math.h nor -lm
  */
/*!@{*/
  double   sdtools_exp                 (double x);
  double   sdtools_log                 (double x);
  double   sdtools_logN                (double x, double base);
  double   sdtools_pow                 (double x, double y);
/*!@}*/


#ifdef SD_SUPP_ARCHDEP_COLOUR_FUNCTIONS
/** \name Backward compatibility functions
  */
/*!@{*/
  void     sdtools_generic_setpixel    (serdisp_t* dd, int x, int y, long colour);
  long     sdtools_generic_getpixel    (serdisp_t* dd, int x, int y);
  void     sdtools_generic_setpixel_greyhoriz  (serdisp_t* dd, int x, int y, long colour);
  long     sdtools_generic_getpixel_greyhoriz  (serdisp_t* dd, int x, int y);
/*!@}*/
#endif

#ifdef __cplusplus
    }
#endif

#endif /* SERDISP_TOOLS_H */

/*! @} */
