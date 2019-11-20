/* glib-private.h - GLib-internal private API, shared between glib, gobject, gio
 * Copyright (C) 2011 Red Hat, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#ifndef __GLIB_PRIVATE_H__
#define __GLIB_PRIVATE_H__

#include <glib.h>
#include "gwakeup.h"

G_GNUC_INTERNAL
GMainContext *          g_get_worker_context            (void);

#define GLIB_PRIVATE_CALL(symbol) (glib__private__()->symbol)

typedef struct {
  /* See gwakeup.c */
  GWakeup *             (* g_wakeup_new)                (void);
  void                  (* g_wakeup_free)               (GWakeup *wakeup);
  void                  (* g_wakeup_get_pollfd)         (GWakeup *wakeup,
                                                        GPollFD *poll_fd);
  void                  (* g_wakeup_signal)             (GWakeup *wakeup);
  void                  (* g_wakeup_acknowledge)        (GWakeup *wakeup);

  /* See gmain.c */
  GMainContext *        (* g_get_worker_context)        (void);
  /* Add other private functions here, initialize them in glib-private.c */
} GLibPrivateVTable;

GLibPrivateVTable *glib__private__ (void);

#endif /* __G_MAIN_H__ */
