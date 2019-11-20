/** \file    serdisp_colour.h
  *
  * \brief   Colour and colour space specific functions
  * \date    (C) 2003-2012
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

/** \addtogroup SERDISP_COLOUR

  \section INTRODUCTION Introduction
  All hardware-independend pixel manipulating functions are defined here.

  \attention
  serdisp_getpixel() / serdisp_setpixel() are <b>hardware dependend</b> functions,
  and serdisp_getcolour() / serdisp_setcolour() are <b>architecture dependend</b> functions.\n\n
  To obtain <b>hardware <i>and</i> architecture independend</b> code, serdisp_getsdcol() / serdisp_setsdcol(), 
  which are defined in here, should be used! \n\n
  \b No descriptor fields or internal functions should be accessed directly as these are <em>subject to change</em>.

  * @{
  */

#ifndef SERDISP_COLOUR_H
#define SERDISP_COLOUR_H

#include "serdisp_control.h"

/* define 'byte' if not available yet */
#ifndef byte
  #define byte unsigned char
#endif


/* colour spaces */

/* no flag set: auto-detect */
#define SD_CS_AUTO                0x00000000

/* bits 0-3 reserved for flags */
#define SD_CS_SCRBUFCUSTOM        0x00000001  /* custom screen buffer (>=1 of the following conditions occur): */
                                              /*  * pixels are stored horizontally, not vertically (== pages)  */
                                              /*  * fontsizes != 8                                             */
#define SD_CS_SELFEMITTING        0x00000002  /* self emitting displays (eg.: OLEDs) */

/* bits 4-7: greyscale colour spaces */
#define SD_CS_GREYSCALE           0x00000010

/* bits 8-15 reserved for indexed colours */

/* bits 16-27: packed colour spaces */
/* if bit 7 (0x00800000) is set: BGR instead of RGB */
#define SD_CS_RGB332              0x00010000
#define SD_CS_RGB333              0x00020000
#define SD_CS_RGB444              0x00040000
#define SD_CS_RGB565              0x00080000
#define SD_CS_RGB666              0x00100000
#define SD_CS_BGR                 0x00800000

/* bits 28-31: true colour spaces */
#define SD_CS_TRUECOLOUR          0x40000000
#define SD_CS_ATRUECOLOUR         0x80000000

/* mask of all indexed colour spaces */
#define SD_CS_INDEXED_SPACE       SD_CS_GREYSCALE

#define SD_CS_ISGREY(_dd)         ( ( (((_dd)->colour_spaces) & 0x000000F0L) && (((_dd)->depth) <=8) ) ? 1 : 0 )
#define SD_CS_ISMONOCHROME(_dd)   ( ( ((_dd)->depth) == 1 ) ? 1 : 0 )

/* is packed colour space RGB (0) or BGR (1) ? */
#define SD_CS_ISBGR(_dd)          ( ( ((_dd)->colour_spaces) & 0x00800000L) ? 1 : 0 )

/* is display an OLED? if so: default background colour is black instead of white */
#define SD_CS_ISSELFEMITTING(_dd) ( ( ((_dd)->colour_spaces) & SD_CS_SELFEMITTING) ? 1 : 0 )

/* true colour or packed colour space */
#define SD_CS_ISDIRECTCOLOUR(_dd) ( ( (((_dd)->colour_spaces) & 0xFFFF0000L) && (((_dd)->depth) >= 8) ) ? 1 : 0 )

/* true colour space */
#define SD_CS_ISTRUECOLOUR(_dd)   ( ( (((_dd)->colour_spaces) & 0xF0000000L) && (((_dd)->depth) >= 24) ) ? 1 : 0 )

/* some colour defines */
#define SD_COL_BLACK              0xFF000000
#define SD_COL_WHITE              0xFFFFFFFF
#define SD_COL_RED                0xFFFF0000
#define SD_COL_GREEN              0xFF00FF00
#define SD_COL_BLUE               0xFF0000FF
#define SD_COL_MAGENTA            0xFFFF00FF
#define SD_COL_CYAN               0xFF00FFFF
#define SD_COL_YELLOW             0xFFFFFF00


#ifdef __cplusplus
extern "C" {
#endif
/** \name Colour set/get functions
  */
/*!@{*/
  void     serdisp_setsdcol            (serdisp_t* dd, int x, int y, SDCol sdcol);
  void     serdisp_setsdgrey           (serdisp_t* dd, int x, int y, byte grey);

  SDCol    serdisp_getsdcol            (serdisp_t* dd, int x, int y);
  byte     serdisp_getsdgrey           (serdisp_t* dd, int x, int y);

  int      serdisp_setarea             (serdisp_t* dd, int x, int y, int w, int h, int inpmode, byte* content);
  int      serdisp_cliparea            (serdisp_t* dd, int x, int y, int w, int h, int sx, int sy, int cw, int ch,
                                        int inpmode, byte* content);
/*!@}*/

/** \name Colour conversion functions
  */
/*!@{*/
  int      serdisp_sdcol_init          (serdisp_t* dd);

  SDCol    serdisp_transsdcol          (serdisp_t* dd, SDCol sdcol);
  SDCol    serdisp_transsdgrey         (serdisp_t* dd, byte greyvalue);

  SDCol    serdisp_lookupsdcol         (serdisp_t* dd, SDCol sdcol);
  byte     serdisp_lookupsdgrey        (serdisp_t* dd, SDCol sdcol);

  void     serdisp_setsdcoltabentry    (serdisp_t* dd, int idx, SDCol sdcol);
  SDCol    serdisp_getsdcoltabentry    (serdisp_t* dd, int idx);
/*!@}*/

#ifdef SD_SUPP_ARCHDEP_COLOUR_FUNCTIONS
/** \name Backward Compatibility functions
  */
/*!@{*/
  void     serdisp_setcolour           (serdisp_t* dd, int x, int y, long colour);
  void     serdisp_setgrey             (serdisp_t* dd, int x, int y, byte grey);
  long     serdisp_getcolour           (serdisp_t* dd, int x, int y);
  byte     serdisp_getgrey             (serdisp_t* dd, int x, int y);
  long     serdisp_transcolour         (serdisp_t* dd, long colour);
  long     serdisp_transgrey           (serdisp_t* dd, byte greyvalue);
  long     serdisp_lookupcolour        (serdisp_t* dd, long colour);
  byte     serdisp_lookupgrey          (serdisp_t* dd, long colour);
  void     serdisp_setcoltabentry      (serdisp_t* dd, int idx, long colour);
  long     serdisp_getcoltabentry      (serdisp_t* dd, int idx);
/*!@}*/
#endif

#ifdef __cplusplus
    }
#endif


/* some macros to make life easier */

/** \hideinitializer
  * \brief   packs alpha/red/green/blue values to a \p 0xAARRGGBB colour value
  *
  * Packs an alpha/red/green/blue-representation to a colour representation suitable for serdisplib.
  *
  * \param   _a            alpha-channel
  * \param   _r            red-channel
  * \param   _g            green-channel
  * \param   _b            blue-channel
  *
  * \return  hardware independend colour value,\n
  *          format: <tt>0xAARRGGBB</tt>, \p AA .. alpha, \p RR .. red, \p GG .. green, \p BB .. blue
  *
  * \b Examples: \n
  * set a red pixel in the display buffer at position \p (10/20)
  * \code
  * int r = 0xFF;
  * int g = 0x00;
  * int b = 0x00;
  * serdisp_setsdcol(dd, 10, 20, serdisp_pack2ARGB("0xFF", r, g, b));
  * \endcode
  */
#define serdisp_pack2ARGB(_a,_r,_g,_b) ( (((SDCol)(_a))<<24) | (((SDCol)(_r))<<16) | (((SDCol)(_g))<<8) | (SDCol)(_b) )

/** \hideinitializer
  * \brief   converts a colour value to a grey value
  *
  * Converts an \p 0xAARRGGBB colour value to a grey value
  *
  * \param   _col          colour value, format: <tt>0xAARRGGBB</tt>, \p AA .. alpha, \p RR .. red, \p GG .. green, \p BB .. blue
  *
  * \return  grey value, format: <tt>[0 .. 255]</tt>
  */
#define serdisp_ARGB2GREY(_col)        ((byte)((((0x00FF0000 & (_col)) >> 16) * 77 + ((0x0000FF00 & (_col)) >> 8) * 150 + (0x000000FF & (_col)) * 28) / 255))

/** \hideinitializer
  * \brief   converts a grey value to a \p 0xAARRGGBB colour value
  *
  * Converts a grey value to a \p 0xAARRGGBB colour value
  *
  * \param   _grey         colour value, format: <tt>[0 .. 255]</tt>
  *
  * \return  colour value, format: <tt>0xAARRGGBB</tt>, \p AA .. alpha, \p RR .. red, \p GG .. green, \p BB .. blue
  */
#define serdisp_GREY2ARGB(_grey)       ( 0xFF000000 | (((SDCol)(_grey))<<16) | (((SDCol)(_grey))<<8) | ((SDCol)(_grey)) )

#define serdisp_COLCROSSTOTAL(_col)    (((_col) & 0xFF) + (((_col) & 0xFF00) >> 8) + (((_col) & 0xFF0000) >> 16))

#endif /* SERDISP_COLOUR_H */

/*! @} */
