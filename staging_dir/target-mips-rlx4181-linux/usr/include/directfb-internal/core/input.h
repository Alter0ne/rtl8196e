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

#ifndef __INPUT_H__
#define __INPUT_H__

#include <pthread.h>
#include <directfb.h>

#include <direct/modules.h>

#include <fusion/reactor.h>

#include <core/coretypes.h>



DECLARE_MODULE_DIRECTORY( dfb_input_modules );


/*
 * Increase this number when changes result in binary incompatibility!
 */
#define DFB_INPUT_DRIVER_ABI_VERSION         7

#define DFB_INPUT_DRIVER_INFO_NAME_LENGTH   48
#define DFB_INPUT_DRIVER_INFO_VENDOR_LENGTH 64


typedef struct {
     int          major;              /* major version */
     int          minor;              /* minor version */
} InputDriverVersion;                 /* major.minor, e.g. 0.1 */

typedef struct {
     InputDriverVersion version;

     char               name[DFB_INPUT_DRIVER_INFO_NAME_LENGTH];
                                      /* Name of driver,
                                         e.g. 'Serial Mouse Driver' */

     char               vendor[DFB_INPUT_DRIVER_INFO_VENDOR_LENGTH];
                                      /* Vendor (or author) of the driver,
                                         e.g. 'directfb.org' or 'Sven Neumann' */
} InputDriverInfo;

typedef struct {
     unsigned int       prefered_id;  /* Prefered predefined input device id,
                                         e.g. DIDID_MOUSE */

     DFBInputDeviceDescription desc;  /* Capabilities, type, etc. */
} InputDeviceInfo;

typedef struct {
     int       (*GetAvailable)   (void);
     void      (*GetDriverInfo)  (InputDriverInfo              *driver_info);
     DFBResult (*OpenDevice)     (CoreInputDevice              *device,
                                  unsigned int                  number,
                                  InputDeviceInfo              *device_info,
                                  void                        **driver_data);
     DFBResult (*GetKeymapEntry) (CoreInputDevice              *device,
                                  void                         *driver_data,
                                  DFBInputDeviceKeymapEntry    *entry);
     void      (*CloseDevice)    (void                         *driver_data);

     DFBResult (*GetAxisInfo)    (CoreInputDevice              *device,
                                  void                         *driver_data,
                                  DFBInputDeviceAxisIdentifier  axis,
                                  DFBInputDeviceAxisInfo       *ret_info);
} InputDriverFuncs;


typedef DFBEnumerationResult (*InputDeviceCallback) (CoreInputDevice *device,
                                                     void            *ctx);

void dfb_input_enumerate_devices( InputDeviceCallback         callback,
                                  void                       *ctx,
                                  DFBInputDeviceCapabilities  caps );


DirectResult dfb_input_attach       ( CoreInputDevice *device,
                                      ReactionFunc     func,
                                      void            *ctx,
                                      Reaction        *reaction );

DirectResult dfb_input_detach       ( CoreInputDevice *device,
                                      Reaction        *reaction );

DirectResult dfb_input_attach_global( CoreInputDevice *device,
                                      int              index,
                                      void            *ctx,
                                      GlobalReaction  *reaction );

DirectResult dfb_input_detach_global( CoreInputDevice *device,
                                      GlobalReaction  *reaction );


DFBResult    dfb_input_add_global   ( ReactionFunc     func,
                                      int             *ret_index );

DFBResult    dfb_input_set_global   ( ReactionFunc     func,
                                      int              index );


void         dfb_input_dispatch     ( CoreInputDevice *device,
                                      DFBInputEvent   *event );



void              dfb_input_device_description( const CoreInputDevice     *device,
                                                DFBInputDeviceDescription *desc );

DFBInputDeviceID  dfb_input_device_id         ( const CoreInputDevice     *device );

CoreInputDevice  *dfb_input_device_at         ( DFBInputDeviceID           id );



DFBResult         dfb_input_device_get_keymap_entry( CoreInputDevice           *device,
                                                     int                        keycode,
                                                     DFBInputDeviceKeymapEntry *entry );

DFBResult         dfb_input_device_set_keymap_entry( CoreInputDevice           *device,
                                                     int                        keycode,
                                                     DFBInputDeviceKeymapEntry *entry );

DFBResult         dfb_input_device_load_keymap   ( CoreInputDevice           *device,
                                                   char                      *filename );

DFBResult         dfb_input_device_reload_keymap   ( CoreInputDevice           *device );

/* global reactions */

typedef enum {
     DFB_WINDOWSTACK_INPUTDEVICE_LISTENER
} DFB_INPUT_GLOBALS;

#endif
