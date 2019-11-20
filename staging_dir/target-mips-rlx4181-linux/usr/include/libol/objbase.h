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
 * $Id: objbase.h,v 1.4 2003/04/30 21:58:12 bazsi Exp $
 *
 ***************************************************************************/

#ifndef __OBJBASE_H_INCLUDED
#define __OBJBASE_H_INCLUDED

struct ol_class;

struct ol_object {
	struct ol_object *next;
	struct ol_class *isa;
	
	char alloc_method;
	char marked;
	char dead;
};

struct ol_class
{
  struct ol_object super;
  struct ol_class *super_class;
  char *name;  /* For debugging */

  size_t size;

  void (*mark_instance)(struct ol_object *instance,
                        void (*mark)(struct ol_object *o));
  void (*free_instance)(struct ol_object *instance);

  /* Particular classes may add their own methods here */
};

#define MARK_INSTANCE(c, i, f) ((c)->mark_instance((i), (f)))
#define FREE_INSTANCE(c, i) ((c)->free_instance((i)))

#define CLASS(c) (c##_class)

#ifdef DEBUG_ALLOC

struct ol_string
{
  int magic;
  UINT32 use_cnt;
  /* NOTE: The allocated size may be larger than the string length. */
  UINT32 length;
  UINT8 data[1];
};


#else   /* !DEBUG_ALLOC */

struct ol_string
{
  UINT32 use_cnt;
  /* NOTE: The allocated size may be larger than the string length. */
  UINT32 length;
  UINT8 data[1];
};

#endif  /* !DEBUG_ALLOC */

struct ol_string *ol_string_alloc(UINT32 length);
void ol_string_free(struct ol_string *s);
struct ol_string *ol_string_use(struct ol_string *str);

#endif
