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

#ifndef __INPUT_DRIVER_H__
#define __INPUT_DRIVER_H__

#include <core/input.h>


static int
driver_get_available( void );

static void
driver_get_info( InputDriverInfo *info );

static DFBResult
driver_open_device( CoreInputDevice  *device,
                    unsigned int      number,
                    InputDeviceInfo  *info,
                    void            **driver_data );

static DFBResult
driver_get_keymap_entry( CoreInputDevice           *device,
                         void                      *driver_data,
                         DFBInputDeviceKeymapEntry *entry );

#ifdef DFB_INPUTDRIVER_HAS_AXIS_INFO
static DFBResult
driver_get_axis_info( CoreInputDevice              *device,
                      void                         *driver_data,
                      DFBInputDeviceAxisIdentifier  axis,
                      DFBInputDeviceAxisInfo       *ret_info );
#endif

static void
driver_close_device( void *driver_data );

static const InputDriverFuncs driver_funcs = {
     .GetAvailable       = driver_get_available,
     .GetDriverInfo      = driver_get_info,
     .OpenDevice         = driver_open_device,
     .GetKeymapEntry     = driver_get_keymap_entry,
     .CloseDevice        = driver_close_device,

#ifdef DFB_INPUTDRIVER_HAS_AXIS_INFO
     .GetAxisInfo        = driver_get_axis_info
#endif
};

#define DFB_INPUT_DRIVER(shortname)                                             \
__attribute__((constructor)) void directfb_##shortname##_ctor( void );          \
__attribute__((destructor))  void directfb_##shortname##_dtor( void );          \
                                                                                \
void                                                                            \
directfb_##shortname##_ctor( void )                                             \
{                                                                               \
     direct_modules_register( &dfb_input_modules, DFB_INPUT_DRIVER_ABI_VERSION, \
                              #shortname, &driver_funcs );                      \
}                                                                               \
                                                                                \
void                                                                            \
directfb_##shortname##_dtor( void )                                             \
{                                                                               \
     direct_modules_unregister( &dfb_input_modules, #shortname );               \
}

#endif
