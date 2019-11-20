/*  This file is part of GNU bc.

    Copyright (C) 1991-1994, 1997, 2006 Free Software Foundation, Inc.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License , or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; see the file COPYING.  If not, write to:
      The Free Software Foundation, Inc.
      Foundation, Inc.  51 Franklin Street, Fifth Floor,
      Boston, MA 02110-1301  USA

    You may contact the author by:
       e-mail:  philnelson@acm.org
      us-mail:  Philip A. Nelson
                Computer Science Department, 9062
                Western Washington University
                Bellingham, WA 98226-9062
       
*************************************************************************/

/* global.c:  This defines the global variables. */

/* We are global.c ... so define and initialize variables here. */

#define _GLOBAL_C

#include "bcdefs.h"

CONST char *libmath[] = 
#include "libmath.h"
;
