/*
 * Linux joystick driver for Twin
 *
 * Copyright 2007 Jeremy Kerr <jk@ozlabs.org>
 *
 * This Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with the Twin Library; see the file COPYING.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */
#ifndef _TWIN_LINUX_JS_H_
#define _TWIN_LINUX_JS_H_

#include <libtwin/twin.h>

/**
 * twin_linux_js_create - create the linux joystick driver
 * @file: device file to open, pass NULL for default
 */
int twin_linux_js_create(twin_screen_t *screen);

/**
 * twin_linux_js_destroy - destroy the linux js driver
 */
void twin_linux_js_destroy(void);

#endif /* _TWIN_LINUX_JS_H_ */
