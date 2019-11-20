/** \file    serdisp_control.h
  *
  * \brief   Functions for accessing and controlling a display
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

/** \addtogroup SERDISP_CONTROL

  \section INTRODUCTION Introduction
  All elementary functions for controlling a display are defined here (except serdisp_setcolour() and serdisp_getcolour()).

  \attention
  Only these functions should be used. \n\n
  \b No descriptor fields or internal functions should be accessed directly as these are <em>subject to change</em>.

  * @{
  */

#ifndef SERDISP_CONTROL_H
#define SERDISP_CONTROL_H

#define SERDISP_VERSION_MAJOR 2    /**< \brief    major version of serdisplib       \hideinitializer */
#define SERDISP_VERSION_MINOR 01    /**< \brief    minor version of serdisplib       \hideinitializer */
#define SERDISP_VERSION_EXTRA ""    /**< \brief    extra version postfix (ignored)   \hideinitializer */

/** \hideinitializer
  * \brief   calculates a serdisplib version code
  * returns the version code which is calculated using major and minor version information
  * \param  _major   major version
  * \param  _minor   minor version
  *
  * \b Example: \n
    <tt>
    <tt>/</tt>* only include code if the header files for serdisplib are at least of version 1.95 *<tt>/</tt> \n
    <tt>#</tt>if (SERDISP_VERSION_CODE >= SERDISP_VERSION(1,95)) \n
    ... \n
    <tt>#</tt>endif \n
    </tt>
  */
#define SERDISP_VERSION(_major,_minor) (((_major) << 8) + (_minor))
#define SERDISP_VERSION_CODE (SERDISP_VERSION((SERDISP_VERSION_MAJOR),(SERDISP_VERSION_MINOR)))

/** \hideinitializer
  * returns the <em>major</em> version information out of a serdisplib version code
  * \param  _code   serdisplib version code
  */
#define SERDISP_VERSION_GET_MAJOR(_code)  ((int)( (_code) >> 8 ))

/** \hideinitializer
  * returns the <em>minor</em> version information out of a serdisplib version code
  * \param  _code   serdisplib version code
  */
#define SERDISP_VERSION_GET_MINOR(_code)  ((int)( (_code) & 0xFF ))



#include "serdisp_connect.h"
/*#include "serdisp_gpevents.h"*/  /* SDGP_gpevset_t declared as forward struct: struct SDGP_gpevset_s */
#include <inttypes.h>


/*****************************/
/* type defines and typedefs */
/*****************************/

/* define 'byte' if not available yet */
#ifndef byte
  #define byte unsigned char
#endif

#ifndef BOOL
  #define BOOL int
#endif

/* hardware independend unsigned type for colours (always 4 bytes) */
typedef uint32_t           SDCol;


/*****************************************************************************/
/* defines for discovering if a certain feature / functionality is supported */
/*****************************************************************************/

/* support for architecture independent colour functions (colour: always 4 bytes) */
#define SD_SUPP_ARCHINDEP_SDCOL_FUNCTIONS
/* support for architecture dependent 'long'-based colour functions (colour: 4 or 8 bytes) */
#define SD_SUPP_ARCHDEP_COLOUR_FUNCTIONS




/* max. amount of contrast steps */
#define MAX_CONTRASTSTEP    10


#define SD_OPTION_NO        0
#define SD_OPTION_YES       1
#define SD_OPTION_TOGGLE    2

/* bit field:
   bit 0: read/write: 1, read-only: 0
   bit 1: standard option (valid for all drivers): 1, else 0 
   bit 2: local option only (will not be propagated to remote clients): 1, else 0
*/
#define SD_OPTIONFLAG_RO    0x0
#define SD_OPTIONFLAG_RW    0x1
#define SD_OPTIONFLAG_STD   0x2
#define SD_OPTIONFLAG_LOC   0x4


/*
 * features for serdisp_feature()
 * if a feature is not supported by a certain display:
 * it will be silently ignored
 */
/* note that serdisp_feature() and the following defines are deprecated. use serdisp_setoption() instead */
/* old (deprecated) */
#define FEATURE_NO          SD_OPTION_NO
#define FEATURE_YES         SD_OPTION_YES
#define FEATURE_TOGGLE      SD_OPTION_TOGGLE

#define FEATURE_CONTRAST    0x01
#define FEATURE_INVERT      0x02
#define FEATURE_BACKLIGHT   0x03
#define FEATURE_ROTATE      0x04
#define FEATURE_REVERSE     FEATURE_INVERT


/**
  * struct describing a display/device option 
  */
typedef struct serdisp_options_s {
  char* name;               /**< name of option */
  char* aliasnames;         /**< alias names for this option (short name, ...) */
  long  minval;             /**< mininum bandwidth for value */
  long  maxval;             /**< maximum bandwidth for value */
  long  modulo;             /**< option value MODULO modulo => 0 */
  byte  flag;               /**< option flags (bit field)\n\n
                                 \p SD_OPTIONFLAG_STD (bit 1):\n
                                       set ... valid for all drivers\n
                                   not set ... driver specific\n\n
                                 <b>bit 0:</b> \n
                                 \p SD_OPTIONFLAG_RW  ... option can be changed interactively\n
                                 \p SD_OPTIONFLAG_RO  ... option can only be changed during setup */
  char* defines;            /**< defines for option values (eg: 1=YES), separated by commas */
} serdisp_options_t;


/**
 * struct containing all infos and other things concerning a display 
 */
typedef struct serdisp_s {
  /* set on init. */
  char* dsp_name;           /**< display name */
  char* dsp_optionstring;   /**< option string */
  int   dsp_id;             /**< display-ID to differ display models within a driver */

  int   width;              /**< width of display in pixels */
  int   height;             /**< height of display in pixels */
  int   depth;              /**< colour depth (1: monochrome) */
  int   startxcol;          /**< x-offset, if display-area is smaller than controller supported area
                                 and display area doesn't start at (0,0) (eg: nokia 7110)   */
  int   startycol;          /**< y-offset, if display-area is smaller than controller supported area
                                 and display area doesn't start at (0,0) (eg: nokia 7110)   */
  int*  xreloctab;          /**< relocation-table containing logical x-position to physical x-position\n
                                \b eg: \n logical x-position = 0 but needs to be passed to the controller as 8 => \n
                                 xreloctab[0] = 8;    setpixel(x,y, ...) {  x_i = xreloctab[x]; ... } */
  int*  yreloctab;          /**< relocation-table containing logical y-position to physical y-position\n
                                \b eg: \n logical y-position = 0 but needs to be passed to the controller as 2 => \n
                                 yreloctab[0] = 2;    setpixel(x,y, ...) {  y_i = yreloctab[y]; ... } */
  int   xcolgaps;           /**< extra elements needed in xreloctab because of gaps (eg: optrex 323 display) */
  int   ycolgaps;           /**< extra elements needed in yreloctab because of gaps */

  long  dsparea_width;      /**< width of physical display area in micrometres. 0 if unknown */
  long  dsparea_height;     /**< height of physical display area in micrometres. 0 if unknown */

  BOOL  feature_contrast;   /**< is contrast adjustable? */
  BOOL  feature_backlight;  /**< is backlight available? */
  BOOL  feature_invert;     /**< is hardware inversion of display supported? */
  int   min_contrast;       /**< if feature_contrast = true =>
                                 min_contrast contains the lower border at which the display is just about to be readable */
  int   max_contrast;       /**< if feature_contrast = true =>
                                 max_contrast contains the upper border at which the display is still readable */
  int   mid_contrast;       /**< if feature_contrast = true =>
                                 if mid_contrast is not set (==0): linear contrast from min_contrast to max_contrast
                                 if mid_contrast is set: the default linear function for the contrast values is changed 
                                 to a power function with fixed point for CONTRAST=5 => mid_contrast */

  long  delay;              /**< delay after each write-operation (in nanoseconds).\n
                                 ns = 0: ignore (no delay)\n
                                 ns = 1: delay = one call of gettimeofday()\n
                                 ns > 1: delay (at least) ns nanoseconds
                              */

  int   optalgo_maxdelta;   /**< used for optimised transfer algorithm */

  void* specific_data;      /**< place for a specific display-dependend struct (eg: i2c-ids, ...) */

  SDCol* ctable;            /**< colour table for indexed colour table */

  long  colour_spaces;      /**< colour spaces supported by this display (bit field). 0 == auto-detect */

  SDCol default_bgcolour;   /**< default background colour for displays with depth > 1 (will be used for ex. by serdisp_clear() */

  /* runtime settings */
  serdisp_CONN_t* sdcd;     /**< device handle */
  int   connection_types;   /**< connection types supported by this display (bit field) */

  int   curr_rotate;        /**< display normal (0) or rotated 180 degrees (1) */
  int   curr_contrast;      /**< current value for contrast */
  int   curr_backlight;     /**< status of backlight */
  int   curr_invert;        /**< display inverted (1) or not (0) */
  int   curr_dimming;       /**< current value for dimming (in percent) - between 0 and 100, default: 0\n
                                 dimming = 100 - BRIGHTNESS        (if dimming = 0%, brightness = 100%)
                                 driver needs to support option 'BRIGHTNESS' for this value to take effect
                              */

  int   supp_protocols;     /**< protocols supported by this display / module (bit field) */

  int   dbg_cnt;            /**< counter. may be used for debugging issues */

  /* function pointers to display-specific functions
     (or: what to do if you think c++ is a horrible, unusable language but you need some features of OO-programming) */
  /* we're using 'struct serdisp_s*' here instead of 'serdisp_t*' because of forward definition */
  void  (*fp_init)          (struct serdisp_s* dd);
  void  (*fp_update)        (struct serdisp_s* dd);
  void  (*fp_clear)         (struct serdisp_s* dd);
  int   (*fp_setoption)     (struct serdisp_s* dd, const char* option, long value);
  long  (*fp_getoption)     (struct serdisp_s* dd, const char* option, int* typesize);
  void  (*fp_close)         (struct serdisp_s* dd);

  void  (*fp_setsdpixel)    (struct serdisp_s* dd, int x, int y, SDCol sdcol);
  SDCol (*fp_getsdpixel)    (struct serdisp_s* dd, int x, int y);
  SDCol (*fp_transsdcol)    (struct serdisp_s* dd, SDCol sdcol);
  SDCol (*fp_transsdgrey)   (struct serdisp_s* dd, byte greyvalue);
  SDCol (*fp_lookupsdcol)   (struct serdisp_s* dd, SDCol sdcol);
  byte  (*fp_lookupsdgrey)  (struct serdisp_s* dd, SDCol sdcol);
  int   (*fp_cliparea)      (struct serdisp_s* dd, int x, int y, int w, int h, int sx, int sy, int cw, int ch,
                             int inpmode, byte* content);

  /* get pointer to a value specific for a driver (stored in specific_data). *typesize then contains the size in byte */
  /* 0 if no such values */
  void* (*fp_getvalueptr)   (struct serdisp_s* dd, const char* option, int* typesize);

  /* specific freeresources routine for memory allocations which can not be handled by serdisp_freeresources() */
  int   (*fp_freeresources) (struct serdisp_s* dd);


  byte* scrbuf;             /**< internal display buffer */
  byte* scrbuf_chg;         /**< display change buffer: contains change tags for internal display buffer */

  int   scrbuf_size;        /**< size of display buffer */
  int   scrbuf_chg_size;    /**< size of display change buffer */
  byte  scrbuf_bits_used;   /**< defaults to 8; if less: only n out of 8 bits are used (eg: t6963: fontwidth=6) */
  byte  bbox_dirty;         /**< bounding box state. 1: bounding box contains changes, 0: no changes */
  int   bbox[4];            /**< bounding box: [x1, y1, x2, y2] */

  /* wiring stuff */
  serdisp_wiresignal_t*     wiresignals;
  serdisp_wiredef_t*        wiredefs;
  int                       amountwiresignals;
  int                       amountwiredefs;
  /* options supported by driver */
  serdisp_options_t*        options;
  int                       amountoptions;

  byte                      remote_devid;  /**< if remote device: unique device id (or 0 if local / undefined */

  /* struct containing all gpi/gpo/event loop - specific items */
  struct SDGP_gpevset_s*    gpevset;  /* NULL if no support for gpi/gpo/event loop */
} serdisp_t;


/** serdisp_display_s is a reduced version of serdisp_setup_s (defined in serdisp_control.c) and is
    needed for querying supported displays from outside
*/
typedef struct serdisp_display_s {
  char* dispname;           /**< main display name */
  char* aliasnames;         /**< alias display names, separated by \p ',' (eg.: \p "SOMENAME,SM" ) */
  char* optionstring;       /**< default options used for initalisation or 
                                 if describing an already instanciated display: options currently used */
  char* description;        /**< description text */
} serdisp_display_t;



#ifdef __cplusplus
extern "C" {
#endif

/** \name Device handling functions
  */
/*!@{*/
  serdisp_t*      serdisp_init                   (serdisp_CONN_t*, const char dispname[], const char optionstring[]);
  void            serdisp_close                  (serdisp_t* dd);
  void            serdisp_quit                   (serdisp_t* dd);
  int             serdisp_reset                  (serdisp_t* dd);
  serdisp_t*      serdisp_fullreset              (serdisp_t* dd);
  serdisp_CONN_t* serdisp_getSDCONN              (serdisp_t* dd);
  const char*     serdisp_defaultdevice          (const char* dispname);
/*!@}*/

/** \name Library version
  */
/*!@{*/
  long            serdisp_getversioncode         (void);
/*!@}*/

/** \name Drawing functions
  */
/*!@{*/
  void            serdisp_setsdpixel             (serdisp_t* dd, int x, int y, SDCol sdcol);
  SDCol           serdisp_getsdpixel             (serdisp_t* dd, int x, int y);
  void            serdisp_clearbuffer            (serdisp_t* dd);
  void            serdisp_clear                  (serdisp_t* dd);
  void            serdisp_rewrite                (serdisp_t* dd);
  void            serdisp_update                 (serdisp_t* dd);
  void            serdisp_blink                  (serdisp_t* dd, int what, int cnt, int delta);
/*!@}*/

/** \name Get/Set display settings and informations
  */
/*!@{*/
  int             serdisp_getwidth               (serdisp_t* dd);
  int             serdisp_getheight              (serdisp_t* dd);
  int             serdisp_getcolours             (serdisp_t* dd);
  int             serdisp_getdepth               (serdisp_t* dd);
  int             serdisp_getpixelaspect         (serdisp_t* dd);
  const char*     serdisp_getdisplayname         (serdisp_t* dd);
  void            serdisp_currdisplaydescription (serdisp_t* dd, serdisp_display_t* displaydesc);
  int             serdisp_nextdisplaydescription (serdisp_display_t* displaydesc);
  int             serdisp_isdisplay              (const char* displayname);
  int             serdisp_getdisplaydescription  (const char* displayname, serdisp_display_t* displaydesc);
  int             serdisp_nextwiringdescription  (const char* displayname, serdisp_wiredef_t* wiredesc);
/*!@}*/

/** \name Display options
  */
/*!@{*/
  void            serdisp_setoption              (serdisp_t* dd, const char* optionname, long value);
  long            serdisp_getoption              (serdisp_t* dd, const char* optionname, int* typesize);
  int             serdisp_isoption               (serdisp_t* dd, const char* optionname);
  int             serdisp_getoptiondescription   (serdisp_t* dd, const char* optionname, serdisp_options_t* optiondesc);
  int             serdisp_nextoptiondescription  (serdisp_t* dd, serdisp_options_t* optiondesc);

  int             serdisp_nextstaticoptiondesc   (const char* displayname, serdisp_options_t* optiondesc);
/*!@}*/

#ifdef SD_SUPP_ARCHDEP_COLOUR_FUNCTIONS
/** \name Backward Compatibility functions
  */
/*!@{*/
  void            serdisp_setpixel               (serdisp_t* dd, int x, int y, long colour);
  long            serdisp_getpixel               (serdisp_t* dd, int x, int y);
/*!@}*/
#endif

/** \name Deprecated functions
  */
/*!@{*/
  void            serdisp_feature                (serdisp_t* dd, int feature, int value);
  void            serdisp_setpixels              (serdisp_t* dd, int x, int y, int w, int h, byte* data);
/*!@}*/

  /* internal use only */
  void            serdisp_freeresources          (serdisp_t* dd);
  int             serdisp_setupoptions           (serdisp_t* dd, const char* dispname, const char* optionstring);

  int             serdisp_getdispindex           (const char* dispname);
  int             serdisp_comparedispnames       (const char* dispname1, const char* dispname2);
  int             serdisp_compareoptionnames     (serdisp_t* dd, const char* optionname1, const char* optionname2);
  int             serdisp_getstandardoptionindex (const char* optionname);
  int             serdisp_getoptionindex         (serdisp_t* dd, const char* optionname);
  int             serdisp_scanoptvalue           (serdisp_t* dd, const char* optionname, const char* rawvalue, long* optvalue);


/* driver-specific setup functions */
  serdisp_t*      serdisp_i2c_setup              (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_ks0108_setup           (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_lh155_setup            (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_nokcol_setup           (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_pcd8544_setup          (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_sed133x_setup          (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_sed153x_setup          (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_sed156x_setup          (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_ssdoled_setup          (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_t6963_setup            (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_uc1608_setup           (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_s6b1713_setup          (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_acoolsdcm_setup        (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_l4m_setup              (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_goldelox_setup         (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_stv8105_setup          (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_remote_setup           (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_directgfx_setup        (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_rs232_setup            (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_lc7981_setup           (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_displaylink_setup      (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_ddusbt_setup           (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_framebuffer_setup      (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
  serdisp_t*      serdisp_glcd2usb_setup         (const serdisp_CONN_t* sdcd, const char* dispname, const char* paramstring);
#ifdef __cplusplus
    }
#endif


/*
 * some macros
 */


/* assign pointers to temp vars and use'em for comparations so that gcc will stop whining */
#define serdisp_setupstructinfos(_dd,_signals,_defs,_options) \
            { unsigned long tempsignals = (unsigned long)(_signals); \
              unsigned long tempdefs = (unsigned long)(_defs); \
             (_dd)->wiresignals = (_signals); \
             (_dd)->amountwiresignals = ( ( tempsignals ) ? (sizeof((_signals))/sizeof(serdisp_wiresignal_t)) : 0);\
             (_dd)->wiredefs = (_defs); \
             (_dd)->amountwiredefs = ( ( tempdefs ) ? (sizeof((_defs))/sizeof(serdisp_wiredef_t)) : 0) ;\
             (_dd)->options = (_options); \
             (_dd)->amountoptions = (sizeof((_options))/sizeof(serdisp_options_t));\
            }


/* calculate distance between two pointers -> returns distance in bytes */
#define serdisp_ptrdistance(_endptr, _startptr)      ((int)((long)(_endptr) - (long)(_startptr)))

/* calculate length of a string starting at _startptr and ending at _endptr -> returns length in characters */
#define serdisp_ptrstrlen(_endptr, _startptr)      ( serdisp_ptrdistance( (_endptr), (_startptr) ) / sizeof(char))



#endif /* SERDISP_CONTROL_H */

/*! @} */
