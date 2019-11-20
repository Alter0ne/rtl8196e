/***************************************************************************
 *
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
 * Inspired by nsyslog, originally written by Darren Reed.
 *
 * $Id: pkt_buffer.h,v 1.4 2003/01/31 09:27:02 bazsi Exp $
 *
 ***************************************************************************/

#ifndef __LINE_BUFFER_H_INCLUDED
#define __LINE_BUFFER_H_INCLUDED

#include "abstract_io.h"

extern int pktbuf_dropped_pkts;

struct abstract_buffer *make_pkt_buffer(int fifo_size);
struct abstract_buffer *make_pkt_buffer_ext(int queue_min, int queue_max, int pkt_flush);

#endif
