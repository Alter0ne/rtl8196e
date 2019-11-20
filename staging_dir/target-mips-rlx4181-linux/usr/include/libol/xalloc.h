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
 * $Id: xalloc.h,v 1.3 1999/07/10 13:23:09 bazsi Exp $
 *
 ***************************************************************************/

#ifndef __XALLOC_H_INCLUDED
#define __XALLOC_H_INCLUDED

#define OL_ALLOC_HEAP	0
#define OL_ALLOC_STATIC	1
#define OL_ALLOC_STACK	2

#define STATIC_HEADER { NULL, NULL, OL_ALLOC_STATIC, 0, 0 }
#define STACK_HEADER  { NULL, NULL, OL_ALLOC_STACK, 0, 0 }

struct ol_object *ol_object_alloc(struct ol_class *class);

void ol_object_free(struct ol_object *o);

/* NOTE: This won't work for if there are strings or other instance
 * variables that can't be shared. */
struct ol_object *ol_object_clone(struct ol_object *o);

void *ol_space_alloc(size_t size);
void ol_space_free(void *p);

#ifdef DEBUG_ALLOC

#define ol_free debug_free
#define ol_malloc debug_malloc

struct malloc_entry {
	void *ptr;
	int size;
	struct malloc_entry *next;
};

extern struct malloc_entry *first_malloc;

void debug_free(void *p);

struct ol_object *ol_object_check(struct ol_class *class,
				    struct ol_object *instance);
struct ol_object *ol_object_check_subtype(struct ol_class *class,
					    struct ol_object *instance);

#define CHECK_TYPE(c, i) \
  ol_object_check(&CLASS(c), (struct ol_object *) (i))
#define CHECK_SUBTYPE(c, i) \
  ol_object_check_subtype(&CLASS(c), (struct ol_object *) (i))

#define CAST(class, var, o) \
  struct class *(var) = (struct class *) CHECK_TYPE(class, o)

#define CAST_SUBTYPE(class, var, o) \
  struct class *(var) = (struct class *) CHECK_SUBTYPE(class, o)
   

#else   /* !DEBUG_ALLOC */

#define ol_free free
#define ol_malloc malloc


#define CHECK_TYPE(c, o)
#define CHECK_SUBTYPE(c, o)
     
#define CAST(class, var, o) \
   struct class *(var) = (struct class *) (o)

#define CAST_SUBTYPE(class, var, o) CAST(class, var, o)

#endif  /* !DEBUG_ALLOC */

#define NEW(class, var) \
  struct class *(var) = (struct class *) ol_object_alloc(&CLASS(class))
#define NEW_SPACE(x) ((x) = ol_space_alloc(sizeof(*(x))))

#define CLONE(class, i) \
  ((struct class *) ol_object_clone(CHECK_TYPE(class, (i))))

#define CLONED(class, var, i) \
  struct class *(var) = CLONE(class, i)
     
#define KILL(x) gc_kill((struct ol_object *) (x))

void *xalloc(size_t size);


#endif
