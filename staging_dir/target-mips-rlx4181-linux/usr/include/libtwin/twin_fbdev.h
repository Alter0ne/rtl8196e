/*
 * Linux fbdev driver for Twin
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
#ifndef _TWIN_FBDEV_H_
#define _TWIN_FBDEV_H_

#include <libtwin/twin.h>

#include <termios.h>
#include <linux/fb.h>

typedef struct _twin_fbdev {
	twin_screen_t   *screen;	/* twin screen */

	int		vt_no;		/* my VT */
	int		vt_prev;	/* previous VT */
	int		vt_fd;		/* my tty fd */
	int		vt_active;	/* vt is currently active */
	int		vt_swsig;

	struct termios	old_tio;	/* old termios */
	int		old_kbmode;

	int		active;		/* screen is active and useable */
	int		fb_fd;		/* my fbdev fd */
	int		last_btn;	/* last button state */

	/* fbdev setup */
	struct fb_var_screeninfo	fb_var;	
	struct fb_fix_screeninfo	fb_fix;
	unsigned short	cmap[3][256];
	char		*fb_base;
	size_t		fb_len;
	char		*fb_ptr;

} twin_fbdev_t;

/**
 * twin_fbdev_create - create the fbdev backend
 * @wanted_vt:	which VT do you want ? pass -1 for auto-choose
 * @switch_sig: signal for use internally for vt switch
 *
 * The new VT is not activated automatically. You can call
 * twin_fbdev_activate() to do that. That way, you can setup your
 * environment completely before you do the VT switch for smoother
 * transitions.
 *
 * The fbdev is left to it's default settings. You can call functions
 * to change them though they will only be applied if the fbdev is
 * frontmost or when it is activated.
 *
 * Note that this implementation only supports 32bpp argb though it
 * shouldn't be too hard to change that.
 *
 * Regarding the signal passed in switch_sig, it's the responsibility
 * of the caller to make sure it's not blocked.
 */

twin_fbdev_t *twin_fbdev_create(int wanted_vt, int switch_sig);

/**
 * twin_fbdev_destroy - distroy the fbdev backend
 * @tf: backend pointed returned by twin_fbdev_create
 */
void twin_fbdev_destroy(twin_fbdev_t *tf);

/**
 * twin_fbdev_activate - activate the fbdev
 * @tf: backemd pointer
 *
 * This triggers a console switch to the VT allocated to the
 * twin screen. It returns false if the activation failed
 * due to unsupported framebuffer settings or the VT switch
 * was refused.
 */
twin_bool_t twin_fbdev_activate(twin_fbdev_t *tf);


#endif /* _TWIN_FBDEV_H_ */
