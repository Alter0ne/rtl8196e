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

/* const.h: Constants for bc. */

/* Define INT_MAX and LONG_MAX if not defined.  Assuming 32 bits... */

#ifndef INT_MAX
#define INT_MAX 0x7FFFFFFF
#endif
#ifndef LONG_MAX
#define LONG_MAX 0x7FFFFFFF
#endif


/* Define constants in some reasonable size.  The next 4 constants are
   POSIX constants. */

#ifdef BC_BASE_MAX
  /* <limits.h> on a POSIX.2 system may have defined these.  Override. */
# undef BC_BASE_MAX
# undef BC_SCALE_MAX
# undef BC_STRING_MAX
# undef BC_DIM_MAX
#endif

#define BC_BASE_MAX   INT_MAX
#define BC_SCALE_MAX  INT_MAX
#define BC_STRING_MAX INT_MAX


/* Definitions for arrays. */

#define BC_DIM_MAX   16777215     /* this should be NODE_SIZE^NODE_DEPTH-1 */

#define   NODE_SIZE        64     /* Must be a power of 2. */
#define   NODE_MASK      0x3f     /* Must be NODE_SIZE-1. */
#define   NODE_SHIFT        6     /* Number of 1 bits in NODE_MASK. */
#define   NODE_DEPTH        4


/* Other BC limits defined but not part of POSIX. */

#define BC_LABEL_GROUP 64
#define BC_LABEL_LOG    6
#define BC_START_SIZE  1024	/* Initial code body size. */

/* Maximum number of variables, arrays and functions and the
   allocation increment for the dynamic arrays. */

#define MAX_STORE   32767
#define STORE_INCR     32

/* Other interesting constants. */

#define FALSE 0
#define TRUE  1

/* for use with lookup (). */
#define SIMPLE   0
#define ARRAY    1
#define FUNCT    2
#define FUNCTDEF 3

#ifdef __STDC__
#define CONST const
#define VOID  void
#else
#define CONST
#define VOID
#endif
