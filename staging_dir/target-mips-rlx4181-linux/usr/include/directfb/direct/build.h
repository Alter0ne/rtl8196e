/*
   (c) Copyright 2000-2002  convergence integrated media GmbH.
   (c) Copyright 2002-2004  convergence GmbH.

   All rights reserved.

   Written by Denis Oliver Kropp <dok@directfb.org>,
              Andreas Hundt <andi@fischlustig.de>,
              Sven Neumann <neo@directfb.org> and
              Ville Syrj�l� <syrjala@sci.fi>.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.
*/

#ifndef __DIRECT__BUILD_H__
#define __DIRECT__BUILD_H__

#define DIRECT_BUILD_DEBUG   (0)
#define DIRECT_BUILD_DEBUGS  (1)
#define DIRECT_BUILD_TRACE   (0)
#define DIRECT_BUILD_TEXT    (1)
#define DIRECT_BUILD_GETTID  (1)
#define DIRECT_BUILD_NETWORK (0)
#define DIRECT_BUILD_STDBOOL (1)


#if !DIRECT_BUILD_DEBUGS
#if defined(DIRECT_ENABLE_DEBUG) || defined(DIRECT_FORCE_DEBUG)
#define DIRECT_MINI_DEBUG
#endif
#undef DIRECT_ENABLE_DEBUG
#ifdef DIRECT_FORCE_DEBUG
#warning DIRECT_FORCE_DEBUG used with 'pure release' library headers.
#undef DIRECT_FORCE_DEBUG
#endif
#endif
 
#endif
