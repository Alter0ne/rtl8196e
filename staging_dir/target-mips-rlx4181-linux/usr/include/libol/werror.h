/***************************************************************************
 *
 * Copyright (c) 1998-1999 Niels Möller
 * Copyright (c) 1999 BalaBit Computing
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * $Id: werror.h,v 1.4 1999/07/10 13:23:09 bazsi Exp $
 *
 ***************************************************************************/

#ifndef __ERROR_H_INCLUDED
#define __ERROR_H_INCLUDED

#include "objects.h"
#include <stdarg.h>

/* Global variables */
extern int debug_flag;
extern int quiet_flag;
extern int verbose_flag;

void set_error_stream(int fd, int with_poll);
void set_error_ignore(void);

#ifdef HAVE_SYSLOG
void set_error_syslog(const char *progname);
#endif

/* Format specifiers:
 *
 * %%  %-charqacter
 * %i  UINT32
 * %c  int, interpreted as a single character to output
 * %z  NUL-terminated string
 * %s  UINT32 length, UINT8 *data
 * %S  lsh_string *s
 *
 * Modifiers:
 *
 * x  hexadecimal output
 * f  Consume (and free) the input string
 * p  Filter out dangerous control characters
 */

/* message levels */

#define MSG_DEBUG           0
#define MSG_VERBOSE         1
#define MSG_NOTICE          2
#define MSG_ERROR          3
#define MSG_FATAL           4

extern int (*error_write)(int level, UINT32 length, UINT8 *data);

void notice(const char *format, ...);
void werror(const char *format, ...);
void debug(const char *format, ...);
void verbose(const char *format, ...);

void fatal(const char *format, ...) NORETURN;

#endif 
