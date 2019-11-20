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
 * $Id: abstract_io.h,v 1.7 2003/04/30 08:08:50 bazsi Exp $
 *
 ***************************************************************************/

#ifndef __ABSTRACT_IO_H_INCLUDED
#define __ABSTRACT_IO_H_INCLUDED

#include "objects.h"
#include "objtypes.h"

typedef void abstract_addr;

#define CLASS_DECLARE
#include "abstract_io.h.x"
#undef CLASS_DECLARE

/* A read-function returning n means:
 *
 * n > 0: n bytes were read successfully.
 * n = 0: No more data available, without blocking.
 * n = -1: Read failed.
 * n = -2: EOF.
 */
#define A_FAIL -1
#define A_EOF -2

/* CLASS:
   (class
     (name abstract_read)
     (vars
       (read indirect-method int "UINT32 length" "UINT8 *buffer")
       (recv indirect-method int "UINT32 length" "UINT8 *buffer" "abstract_addr *addr" "size_t *addrsize")))
*/

#define A_READ(f, length, buffer) (f)->read(&(f), (length), (buffer))
#define A_RECV(f, length, buf, addr, addrlen) ((f)->recv(&(f), length, buf, addr, addrlen))

/* May store a new handler into *h. */

/* CLASS:
   (class
     (name read_handler)
     (vars
       (handler indirect-method int "struct abstract_read *read")))
*/

#define READ_HANDLER(h, read) ((h)->handler(&(h), (read)))

/* CLASS:
   (class
     (name abstract_write)
     (vars
       (write method int "UINT32 length" "UINT8 *data")
       (writestr method int "struct ol_string *str")))
*/

#define A_WRITE(f, l, d) ((f)->write((f), l, d))
#define A_WRITE_CSTR(f, s) ((f)->write(f, strlen(s), s))
#define A_WRITE_STRING(f, p) ((f)->writestr(f, p))

/* CLASS:
     (class
       (name abstract_buffer)
       (super abstract_write)
       (vars
         (writable pointer int)
       	 (closed simple int)
         (flush method int "struct abstract_write *")
         (prepare method int)
         (close method void)))
*/

#define BUF_FLUSH(buf, writer) ((buf)->flush((buf), (writer)))
#define BUF_CLOSE(buf) ((buf)->close(buf))
#define BUF_PREPARE(buf) ((buf)->prepare(buf))


/* A handler that passes packets on to another handler */
/* CLASS:
   (class
     (name abstract_write_pipe)
     (super abstract_write)
     (vars
       (next object abstract_write)))
*/

#endif
