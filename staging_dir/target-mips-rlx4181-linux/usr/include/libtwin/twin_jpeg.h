/*
 * Libjpeg interface to twin
 *
 * Copyright 2007 Benjamin Herrenschmidt <benh@kernel.crashing.org>
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
#ifndef _TWIN_JPEG_H_
#define _TWIN_JPEG_H_

#include <libtwin/twin.h>

/* This matches the libjpeg colorspace definitions so you don't have
 * to use libjpeg headers in your application
 */
typedef enum {
	TWIN_JCS_UNKNOWN,	/* error/unspecified */
	TWIN_JCS_GRAYSCALE,	/* monochrome */
	TWIN_JCS_RGB,		/* red/green/blue */
	TWIN_JCS_YCbCr,		/* Y/Cb/Cr (also known as YUV) */
	TWIN_JCS_CMYK,		/* C/M/Y/K */
	TWIN_JCS_YCCK		/* Y/Cb/Cr/K */
} twin_jpeg_cspace_t;

twin_bool_t twin_jpeg_query(const char		*filepath,
			    twin_coord_t	*out_width,
			    twin_coord_t	*out_height,
			    int			*out_components,
			    twin_jpeg_cspace_t	*out_colorspace);

twin_pixmap_t *twin_jpeg_to_pixmap(const char *filepath, twin_format_t fmt);

#endif /* _TWIN_JPEG_H_ */
