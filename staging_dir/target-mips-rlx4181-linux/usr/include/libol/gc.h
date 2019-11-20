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
 * $Id: gc.h,v 1.4 2000/02/04 19:02:04 bazsi Exp $
 *
 ***************************************************************************/

#ifndef __GC_H_INCLUDED
#define __GC_H_INCLUDED

extern unsigned gc_idle_threshold;
extern unsigned gc_busy_threshold;

void gc_register(struct ol_object *o);
void gc_kill(struct ol_object *o);

void gc(struct ol_object *root);
void gc_maybe(struct ol_object *root, int busy);

#endif /* __GC_H_INCLUDED */
