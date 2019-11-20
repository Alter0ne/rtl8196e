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
 * $Id: io_commands.h,v 1.1 1999/07/10 13:23:09 bazsi Exp $
 *
 ***************************************************************************/

#ifndef __IO_COMMANDS_H_INCLUDED
#define __IO_COMMANDS_H_INCLUDED

#include "command.h"
#include "io.h"

#define CLASS_DECLARE
#include "io_commands.h.x"
#undef CLASS_DECLARE

/* Returned by listen */
/* CLASS:
   (class
     (name listen_value)
     (vars
       (fd object io_fd)
       (peer object address_info)))
*/

struct command *make_listen_command(struct command *callback,
				    struct io_backend *backend);

extern struct collect_info_1 listen_command;
#define LISTEN_COMMAND (&listen_command.super.super.super)

struct command *make_simple_connect(struct io_backend *backend,
				    struct resource_list *resources);
struct command *
make_simple_listen(struct io_backend *backend,
		   struct resource_list *resources);

extern struct command_simple io_log_peer_command;

#endif /* __IO_COMMANDS_H_INCLUDED */
