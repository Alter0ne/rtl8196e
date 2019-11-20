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
 * $Id: queue.h,v 1.3 1999/11/22 18:26:24 bazsi Exp $
 *
 ***************************************************************************/

#ifndef __QUEUE_H_INCLUDED
#define __QUEUE_H_INCLUDED

#include "objects.h"

/* Layout taken from AmigaOS lists... The first node uses a prev
 * pointer that points to the queue's HEAD. The last node uses a next
 * pointer that points to the queue's TAIL field. The TAIL field is
 * always NULL; TAILPREV points to the last node in the queue. */
struct ol_queue_node
{
  struct ol_queue_node *np_links[2];
};
#define LSH_QUEUE_NEXT 0
#define LSH_QUEUE_PREV 1

struct ol_queue
{
  struct ol_queue_node *ht_links[3];
};
#define LSH_QUEUE_HEAD 0
#define LSH_QUEUE_TAIL 1
#define LSH_QUEUE_TAILPREV 2

/* This macro must be used at the start of a block, to make the
 * declarations legal. It is allowed to free n inside the loop. */

#define FOR_QUEUE(q, type, n)					\
  struct ol_queue_node *n##_this, *n##_next;			\
  type n;							\
  for ( n##_this = (q)->ht_links[LSH_QUEUE_HEAD];		\
	( n = (type) n##_this,					\
	  (n##_next = n##_this->np_links[LSH_QUEUE_NEXT]));	\
	n##_this = n##_next)

void ol_queue_init(struct ol_queue *q);
int ol_queue_is_empty(struct ol_queue *q);
void ol_queue_add_head(struct ol_queue *q, struct ol_queue_node *n);
void ol_queue_add_tail(struct ol_queue *q, struct ol_queue_node *n);
void ol_queue_remove(struct ol_queue_node *n);
struct ol_queue_node *ol_queue_remove_head(struct ol_queue *q);
struct ol_queue_node *ol_queue_remove_tail(struct ol_queue *q);

#define CLASS_DECLARE
#include "queue.h.x"
#undef CLASS_DECLARE

/* Object queue */
struct object_queue_node
{
  struct ol_queue_node header;
  struct ol_object *o;
};

/* CLASS:
   (class
     (name object_queue)
     (vars
       (q indirect-special "struct ol_queue"
          do_queue_mark do_queue_free)))
*/

struct object_queue *make_object_queue(void);

int object_queue_is_empty(struct object_queue *q);

struct object_queue_node *object_queue_add_head(struct object_queue *q, struct ol_object *o);
struct object_queue_node *object_queue_add_tail(struct object_queue *q, struct ol_object *o);
struct ol_object *object_queue_remove_head(struct object_queue *q);
struct ol_object *object_queue_remove_tail(struct object_queue *q);

struct ol_object *object_queue_peek_head(struct object_queue *q);
struct ol_object *object_queue_peek_tail(struct object_queue *q);

void object_queue_remove(struct object_queue_node *n);

/* For explicitly allocated object queues, which are not included in a
 * garbage collected object. */
void object_queue_kill(struct object_queue *q);

#define KILL_OBJECT_QUEUE(q) object_queue_kill((q))

#define FOR_OBJECT_QUEUE(oq, n)                                 \
  struct ol_queue_node *n##_this, *n##_next;                    \
  struct ol_object *n;                                          \
  for ( n##_this = (oq)->q.ht_links[LSH_QUEUE_HEAD];            \
        ( n = ((struct object_queue_node *) n##_this)->o,       \
          (n##_next = n##_this->np_links[LSH_QUEUE_NEXT]));     \
        n##_this = n##_next)                                    \

#endif /* OL_QUEUE_H_INCLUDED */
