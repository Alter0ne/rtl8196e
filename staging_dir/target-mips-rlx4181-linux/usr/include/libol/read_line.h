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
 * $Id: read_line.h,v 1.2 1999/07/10 13:23:09 bazsi Exp $
 *
 ***************************************************************************/

#ifndef  LSH_READ_HANDLER_H_INCLUDED
#define  LSH_READ_HANDLER_H_INCLUDED

#include "abstract_io.h"

#define CLASS_DECLARE
#include "read_line.h.x"
#undef CLASS_DECLARE

#define MAX_LINE 1024

/* FIXME: The handler should be able to return an error code. So
 * we should use a pointer argument instead of the return value to
 * install a new read handler. */

/* May store a new handler into *h. */
/* CLASS:
   (class
     (name line_handler)
     (vars
       (handler indirect-method int "struct read_handler **r"
		"UINT32 length" "UINT8 *line")))
*/

#define PROCESS_LINE(h, r, length, line) \
((h)->handler(&(h), (r), (length), (line)))

struct read_handler *make_read_line(struct line_handler *handler);

#endif /* LSH_READ_HANDLER_H_INCLUDED */
