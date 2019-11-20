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
 * $Id: alist.h,v 1.2 1999/07/10 13:23:09 bazsi Exp $
 *
 ***************************************************************************/

#ifndef __ALIST_H_INCLUDED
#define __ALIST_H_INCLUDED

#include "objects.h"
#include <stdarg.h>

/* Forward declaration */
struct alist;

#define CLASS_DECLARE
#include "alist.h.x"
#undef CLASS_DECLARE

/* Abstract interface allows for multiple implementations ("real"
 * alists, linear tables, hash tables */

/* CLASS:
   (meta
     (name alist)
     (methods
       "void * (*get)(struct alist *self, int atom)"
       "void (*set)(struct alist *self, int atom, void *value)"))
*/

/* CLASS:
   (class
     (name alist)
     (meta alist)
     (vars
       (size simple unsigned))
     ; Only subclasses has methods 
     (methods NULL NULL))
*/

#define ALIST_CLASS(l) ((struct alist_meta *) ((l)->super.isa))

#define ALIST_GET(alist, atom) \
     (ALIST_CLASS(alist)->get((alist), (atom)))

#define ALIST_SET(alist, atom, value) \
     (ALIST_CLASS(alist)->set((alist), (atom), (value)))

#if 0
#define ALIST_KEYS(alist) ((alist)->keys((alist)))
#endif

struct alist *alist_addv(struct alist *a, unsigned n, va_list args);
struct alist *alist_add(struct alist *a, unsigned n, ...);

/* n is the number of pairs. The argument list should be terminated
 * with -1, for sanity checks. */
     
struct alist *make_linear_alist(unsigned n, ...);
struct alist *make_linked_alist(unsigned n, ...);

#define make_alist make_linear_alist

#define MAX_LINEAR_ALIST_SIZE	100

#endif /* LSH_ALIST_H_INCLUDED */
