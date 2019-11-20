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
 * $Id: stream_buffer.h,v 1.2 1999/07/10 13:23:09 bazsi Exp $
 *
 ***************************************************************************/

#ifndef __WRITE_BUFFER_H_INCLUDED
#define __WRITE_BUFFER_H_INCLUDED

#include "abstract_io.h"

struct abstract_buffer *make_stream_buffer(UINT32 size);

#endif 
