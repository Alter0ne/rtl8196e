/*
   (c) Copyright 2001-2009  The world wide DirectFB Open Source Community (directfb.org)
   (c) Copyright 2000-2004  Convergence (integrated media) GmbH

   All rights reserved.

   Written by Denis Oliver Kropp <dok@directfb.org>,
              Andreas Hundt <andi@fischlustig.de>,
              Sven Neumann <neo@directfb.org>,
              Ville Syrjälä <syrjala@sci.fi> and
              Claudio Ciccani <klan@users.sf.net>.

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

#ifndef __FUSION__LOCK_H__
#define __FUSION__LOCK_H__

#include <pthread.h>

#include <fusion/types.h>

#include <direct/messages.h>
#include <direct/util.h>


typedef union {
     /* multi app */
     struct {
          int                      id;
          const FusionWorldShared *shared;
          /* builtin impl */
          struct {
               unsigned int        locked;
               pid_t               owner;
               DirectLink         *waiting;
               bool                requested;
               bool                destroyed;
          } builtin;
     } multi;
     
     /* single app */
     struct {
          pthread_mutex_t          lock;
          pthread_cond_t           cond;
          int                      count;
     } single;
} FusionSkirmish;

/*
 * Initialize.
 */
DirectResult fusion_skirmish_init   ( FusionSkirmish    *skirmish,
                                      const char        *name,
                                      const FusionWorld *world );

/*
 * Lock.
 */
DirectResult fusion_skirmish_prevail( FusionSkirmish    *skirmish );

/*
 * Try lock.
 */
DirectResult fusion_skirmish_swoop  ( FusionSkirmish    *skirmish );

/*
 * Find out how many times current thread has acquired lock. 
 */
DirectResult fusion_skirmish_lock_count( FusionSkirmish *skirmish, int *lock_count );

/*
 * Unlock.
 */
DirectResult fusion_skirmish_dismiss( FusionSkirmish    *skirmish );

/*
 * Deinitialize.
 */
DirectResult fusion_skirmish_destroy( FusionSkirmish    *skirmish );

/*
 * Wait & Notify.
 *
 * Must be locked!
 */
DirectResult fusion_skirmish_wait   ( FusionSkirmish    *skirmish,
                                      unsigned int       timeout );
DirectResult fusion_skirmish_notify ( FusionSkirmish    *skirmish );


#if D_DEBUG_ENABLED
#define FUSION_SKIRMISH_ASSERT(skirmish)                                                  \
     do {                                                                                 \
          int lock_count;                                                                 \
                                                                                          \
          D_ASSERT( skirmish != NULL );                                                   \
                                                                                          \
          D_ASSERT( fusion_skirmish_lock_count( skirmish, &lock_count ) == DR_OK );      \
          D_ASSERT( lock_count > 0 );                                                     \
     } while (0)
#else
#define FUSION_SKIRMISH_ASSERT(skirmish)                                                  \
     do {                                                                                 \
     } while (0)
#endif

#endif

