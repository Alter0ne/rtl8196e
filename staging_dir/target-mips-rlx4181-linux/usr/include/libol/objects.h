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
 * $Id: objects.h,v 1.4 2000/02/04 19:02:04 bazsi Exp $
 *
 ***************************************************************************/

#ifndef __OBJECTS_H_INCLUDED
#define __OBJECTS_H_INCLUDED

#include <stdlib.h>
#include <olconfig.h>

#define ST_OK           0
#define ST_FAIL         1

#define ST_GOON         0
#define ST_CLOSE        2
#define ST_DIE          4
#define ST_HOLD         8

#define ST_CLOSEDP(x) ((x) & (ST_CLOSE | ST_DIE | ST_FAIL))
#define ST_FAILUREP(x) ((x) & ST_FAIL)

#include "objtypes.h"
#include "objbase.h"
#include "xalloc.h"
#include "gc.h"
#include "werror.h"

#endif
