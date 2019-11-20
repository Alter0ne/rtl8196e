/*
 * Linux mouse driver for Twin
 *
 * Copyright 2006 Benjamin Herrenschmidt <benh@kernel.crashing.org>
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
#ifndef _TWIN_LINUX_MOUSE_H_
#define _TWIN_LINUX_MOUSE_H_

#include <libtwin/twin.h>

typedef struct _twin_linux_mouse {
	twin_screen_t   *screen;

	/* acceleration settings */
	int		acc_num;
	int		acc_den;
	int		acc_threshold;

	/* internals */
	int		fd;
	char		residual[2];
	int		res_cnt;
	int		btns;
	int		x,y;
} twin_linux_mouse_t;

/**
 * twin_linux_mouse_create - create the linux mouse driver
 * @file: device file to open, pass NULL for default
 */
twin_linux_mouse_t *twin_linux_mouse_create(const char *file,
					    twin_screen_t *screen);

/**
 * twin_linux_mouse_destroy - destroy the linux mouse driver
 */
void twin_linux_mouse_destroy(twin_linux_mouse_t *tm);

/**
 * twin_linux_mouse_set_bounds - set mouse boundaries
 */
void twin_linux_mouse_screen_changed(twin_linux_mouse_t *tm);


/**
 * twin_linux_mouse_set_accel - set mouse acceleration data
 */

void twin_linux_mouse_set_accel(twin_linux_mouse_t *tm,
				int num, int den, int threshold);

#endif /* _TWIN_LINUX_MOUSE_H_ */
