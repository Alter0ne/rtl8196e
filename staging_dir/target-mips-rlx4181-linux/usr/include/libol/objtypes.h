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
 * $Id: objtypes.h,v 1.6 2004/08/05 11:13:45 bazsi Exp $
 *
 ***************************************************************************/

#ifndef __OBJ_TYPES_H_INCLUDED
#define __OBJ_TYPES_H_INCLUDED

#include "olconfig.h"

#if SIZEOF_SHORT >= 4
# define UINT32 unsigned short
#elif SIZEOF_INT >= 4
# define UINT32 unsigned int
#elif SIZEOF_LONG >= 4
# define UINT32 unsigned long
#else
# error No suitable type found to use for UINT32
#endif /* UINT32 */
 
#if SIZEOF_SHORT >= 2
# define UINT16 unsigned short
#elif SIZEOF_INT >= 2
# define UINT16 unsigned int
#else
# error No suitable type found to use for UINT16
#endif  /* UINT16 */
 
#define UINT8 unsigned char

#if __GNUC__ && HAVE_ATTRIBUTE
#define NORETURN __attribute__ ((noreturn))
#define PRINTF_STYLE(f, a) __attribute__ ((format(printf, f, a)))
#define UNUSED __attribute__ ((unused))
#else
#define NORETURN
#define PRINTF_STYLE(f, a)
#define UNUSED
#endif

#if !HAVE_SOCKLEN_T
typedef unsigned int socklen_t;
#endif

/* Some macros */

/* Useful macros. */
#define LIBOL_MIN(a, b) (((a)>(b)) ? (b) : (a))
#define LIBOL_MAX(a, b) (((a)>(b)) ? (a) : (b))

     
#endif
