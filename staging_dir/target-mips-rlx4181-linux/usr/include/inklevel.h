/* inklevel.h
 *
 * (c) 2003, 2004, 2005, 2006, 2007, 2009 Markus Heinz
 *
 * This software is licensed under the terms of the GPL.
 * For details see file COPYING.
 */

#ifndef INKLEVEL_H
#define INKLEVEL_H

#ifdef DEBUG
#include <stdio.h> /* for the debug printfs */
#endif

/* public interface */

/* Values for port */

#define PARPORT 1
#define USB 2
#define CUSTOM_PARPORT 3
#define CUSTOM_USB 4
#define BJNP 5
#define CUSTOM_BJNP 6

/* Possible return values for get_ink_level() */

#define OK 0
#define ERROR -1
#define DEV_PARPORT_INACCESSIBLE -2
#define DEV_LP_INACCESSIBLE -3
#define COULD_NOT_GET_DEVICE_ID -4
#define DEV_USB_LP_INACCESSIBLE -5
#define UNKNOWN_PORT_SPECIFIED -6
#define NO_PRINTER_FOUND -7
#define NO_DEVICE_CLASS_FOUND -8
#define NO_CMD_TAG_FOUND -9
#define PRINTER_NOT_SUPPORTED -10
#define NO_INK_LEVEL_FOUND -11
#define COULD_NOT_WRITE_TO_PRINTER -12
#define COULD_NOT_READ_FROM_PRINTER -13
#define COULD_NOT_PARSE_RESPONSE_FROM_PRINTER -14
#define COULD_NOT_GET_CREDIT -15
#define DEV_CUSTOM_USB_INACCESSIBLE -16
#define BJNP_URI_INVALID -17
#define BJNP_INVALID_HOSTNAME -18

#define MODEL_NAME_LENGTH 100
#define MAX_CARTRIDGE_TYPES 40

/* Values for ink_level.status */

#define RESPONSE_INVALID 0
#define RESPONSE_VALID 1

/* Values for array index 0 defining the cartridge type */

#define CARTRIDGE_NOT_PRESENT 0
#define CARTRIDGE_BLACK 1
#define CARTRIDGE_COLOR 2
#define CARTRIDGE_PHOTO 3
#define CARTRIDGE_CYAN 4
#define CARTRIDGE_MAGENTA 5
#define CARTRIDGE_YELLOW 6
#define CARTRIDGE_PHOTOBLACK 7
#define CARTRIDGE_PHOTOCYAN 8
#define CARTRIDGE_PHOTOMAGENTA 9
#define CARTRIDGE_PHOTOYELLOW 10
#define CARTRIDGE_RED 11
#define CARTRIDGE_GREEN 12
#define CARTRIDGE_BLUE 13
#define CARTRIDGE_LIGHTBLACK 14
#define CARTRIDGE_LIGHTCYAN 15
#define CARTRIDGE_LIGHTMAGENTA 16
#define CARTRIDGE_LIGHTLIGHTBLACK 17
#define CARTRIDGE_MATTEBLACK 18
#define CARTRIDGE_GLOSSOPTIMIZER 19
#define CARTRIDGE_UNKNOWN 20
#define CARTRIDGE_KCM 21
#define CARTRIDGE_GGK 22
#define CARTRIDGE_KCMY 23
#define CARTRIDGE_LCLM 24
#define CARTRIDGE_YM 25
#define CARTRIDGE_CK 26
#define CARTRIDGE_LGPK 27
#define CARTRIDGE_LG 28
#define CARTRIDGE_G 29
#define CARTRIDGE_PG 30
#define CARTRIDGE_WHITE 31

/* Array indices for ink_level.levels */

#define INDEX_TYPE 0
#define INDEX_LEVEL 1

struct ink_level {
  char model[MODEL_NAME_LENGTH];
  unsigned short status;
  unsigned short levels[MAX_CARTRIDGE_TYPES][2];
};

int get_ink_level(const int port, const char*device_file, 
                  const int portnumber, struct ink_level *level);
char *get_version_string(void);

#endif
