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
 * $Id: resource.h,v 1.3 1999/07/10 13:23:09 bazsi Exp $
 *
 ***************************************************************************/

#ifndef __RESOURCE_H_INCLUDED
#define __RESOURCE_H_INCLUDED

#include "objects.h"
#include "queue.h"

#define CLASS_DECLARE
#include "resource.h.x"
#undef CLASS_DECLARE

/* CLASS:
   (class
     (name resource)
     (vars
       ; Hack to check liveness before the resource gets gc:ed.
       ; Live resources should never be forgotten.
       (alive special int #f dont_free_live_resource)
       
       (kill method void)))
*/

#define KILL_RESOURCE(r) ((r)->kill((r)))

/* For the resource list. It is doubly linked to make removing
 * elements easy. */

struct resource_node
{
  struct ol_queue_node header;
  struct resource *resource;
};

/* FIXME: Non-virtual methods would make sense for this class. Or
 * perhaps we should use a struct rather than a class? */
/* CLASS:
   (class
     (name resource_list)
     (vars
       (q special-struct "struct ol_queue"
                         do_mark_resources do_free_resources)

       ; Returns the node.
       ; NOTE: This pointer should only be stored together with
       ; the resource list object pointer, as the nodes are not gc:ed
       ; individually.  
       (remember method "struct resource_node *" "struct resource *r")

       ; Kills the resource and unlinks and deallocates the node.
       (kill_resource method void "struct resource_node *n")
       
       (kill_all method void)))
*/

/* For now, don't use the value returned from remember. */
#define REMEMBER_RESOURCE(l, r) (((l)->remember((l), (r))))

#define KILL_RESOURCE_NODE(l, n) ((l)->kill_resource((l), (n)))
#define KILL_RESOURCE_LIST(l) ((l)->kill_all((l)))

/* Allocates an empty list. */
struct resource_list *empty_resource_list(void);

#endif 
