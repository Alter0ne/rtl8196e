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
 * $Id: format.h,v 1.6 2001/10/21 11:03:08 bazsi Exp $
 *
 ***************************************************************************/

#ifndef __FORMAT_H_INCLUDED
#define __FORMAT_H_INCLUDED

#include "objects.h"

#include <stdarg.h>

#ifndef va_copy
#  ifdef __va_copy
#    define va_copy __va_copy
#  endif
#endif

#ifndef va_copy
#define va_copy(a, b)
#endif

struct ol_string *c_format(const char *format, ...);
UINT32 c_format_length(const char *format, ...);
UINT32 c_format_write(const char *format, UINT32 length, UINT8 *buffer, ...);

UINT32 c_vformat_length(const char *format, va_list args);
UINT32 c_vformat_write(const char *format, UINT32 length, UINT8 *buffer, va_list args);

/* Short cut */
#define ol_string_dup(s) (c_format("%S", (s)))

unsigned format_size_in_decimal(UINT32 n);
UINT32 format_size_in_hex(UINT32 n);


struct ol_string *format_cstring(const char *s);
struct ol_string *make_cstring(struct ol_string *s, int free);
struct ol_string *c_format_cstring(const char *format, ...);

#endif /* LSH_FORMAT_H_INCLUDED */
