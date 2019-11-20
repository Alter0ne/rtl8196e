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
 * $Id: list.h,v 1.4 1999/07/10 13:23:09 bazsi Exp $
 *
 ***************************************************************************/

#ifndef __LIST_H_INCLUDED
#define __LIST_H_INCLUDED

#include "objects.h"

#include <stdarg.h>

typedef void *(*ol_keyof_fn)(struct ol_object *o);
typedef int (*ol_compare_fn)(void *k1, void *k2);

#define CLASS_DECLARE
#include "list.h.x"
#undef CLASS_DECLARE

/* CLASS:
   (class
     (name list_header)
     (vars
       (length simple unsigned)
       (allocated simple unsigned)
       (add method int "struct ol_object *s")))
*/

/* CLASS:
   (class
     (name int_list)
     (super list_header)
     (vars
       ; This is really of variable size
       (elements var-array int "super.length")))
*/

/* CLASS:
   (class
     (name object_list)
     (super list_header)
     (vars
       ; This is really of variable size
       (elements var-array (object ol_object) "super.length")))
*/

/* CLASS:
   (class
     (name string_list)
     (super list_header)
     (vars
       (elements var-array (object ol_string) "super.length")))
*/

/* CLASS:
   (class
     (name sorted_list)
     (super list_header)
     (vars
       (flags simple int)
       (keyof simple ol_keyof_fn)
       (compare simple ol_compare_fn)
       (search method int "void *" "unsigned *")
       (elements var-array (object ol_object) "super.length")))
*/

#define LIST(x) ((x)->elements)
#define LIST_LENGTH(x) (((struct list_header *) (x))->length)
#define LIST_MAX(x) (((struct list_header *) (x))->allocated)
#define LIST_KEYOF(l, x) ((l)->keyof ? (l)->keyof(x) : (x))
#define LIST_COMPARE(l, x1, x2) ((l)->compare(x1, x2))
#define LIST_ADD(l, x) (((struct list_header *) l)->add((struct list_header *) l, (struct ol_object *) x))
#define LIST_SEARCH(l, k, p) ((l)->search(l, k, p))

struct list_header *ol_list_alloc(struct ol_class *class,
				   unsigned length, size_t element_size);

#define alloc_int_list(n) \
  ((struct int_list *) ol_list_alloc(&CLASS(int_list), (n), sizeof(int)))
     
struct int_list *make_int_listv(unsigned length, va_list args);
struct int_list *make_int_list(unsigned length, ...);

#define alloc_string_list(n) \
  ((struct string_list *) \
   ol_list_alloc(&CLASS(string_list), (n), sizeof(struct ol_string *)))

struct string_list *make_string_listv(unsigned length, va_list args);
struct string_list *make_string_list(unsigned length, ...);

#define alloc_object_list(n) \
  ((struct object_list *) \
   ol_list_alloc(&CLASS(object_list), (n), sizeof(struct ol_object *)))

struct object_list *make_object_listv(unsigned length, va_list args);
struct object_list *make_object_list(unsigned length, ...);

#define alloc_sorted_list(n) \
  ((struct sorted_list *) \
   ol_list_alloc(&CLASS(sorted_list), (n), sizeof(struct ol_object *)))

#define LIST_ADD_SORTED		0x0001	/* sort list as items are added */
#define LIST_ADD_DUPS		0x0002	/* allow duplicates in sorted list */

struct sorted_list *make_sorted_list(unsigned n, int flags, ol_compare_fn cmp, ol_keyof_fn keyof);

#endif /* LSH_LIST_H_INCLUDED */
