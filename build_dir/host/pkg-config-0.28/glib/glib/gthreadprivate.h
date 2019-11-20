/* GLIB - Library of useful routines for C programming
 *
 * gthreadprivate.h - GLib internal thread system related declarations.
 *
 *  Copyright (C) 2003 Sebastian Wilhelmi
 *
 * The Gnome Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * The Gnome Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with the Gnome Library; see the file COPYING.LIB.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 *   Boston, MA 02111-1307, USA.
 */

#ifndef __G_THREADPRIVATE_H__
#define __G_THREADPRIVATE_H__

#include "deprecated/gthread.h"

typedef struct _GRealThread GRealThread;
struct  _GRealThread
{
  GThread thread;

  gint ref_count;
  gboolean ours;
  gchar *name;
  gpointer retval;
};

/* system thread implementation (gthread-posix.c, gthread-win32.c) */
G_GNUC_INTERNAL
void            g_system_thread_wait            (GRealThread  *thread);

G_GNUC_INTERNAL
GRealThread *   g_system_thread_new             (GThreadFunc   func,
                                                 gulong        stack_size,
                                                 GError      **error);
G_GNUC_INTERNAL
void            g_system_thread_free            (GRealThread  *thread);

G_GNUC_INTERNAL
void            g_system_thread_exit            (void);
G_GNUC_INTERNAL
void            g_system_thread_set_name        (const gchar  *name);


/* gthread.c */
G_GNUC_INTERNAL
GThread *       g_thread_new_internal           (const gchar  *name,
                                                 GThreadFunc   proxy,
                                                 GThreadFunc   func,
                                                 gpointer      data,
                                                 gsize         stack_size,
                                                 GError      **error);

G_GNUC_INTERNAL
gpointer        g_thread_proxy                  (gpointer      thread);

#endif /* __G_THREADPRIVATE_H__ */
