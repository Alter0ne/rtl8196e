/**
 *   zstream - Micro URL I/O stream library
 *   Copyright (C) 2010 Steven Barth <steven@midlink.org>
 *   Copyright (C) 2010 John Crispin <blogic@openwrt.org>
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
 *
 */

#ifndef ZSTREAM_H_
#define ZSTREAM_H_

#include <time.h>
#include <errno.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/types.h>

enum zstream_action {
	ZSTREAM_GET,
	ZSTREAM_PUT,
	ZSTREAM_POST,
	ZSTREAM_READ = ZSTREAM_GET,
	ZSTREAM_WRITE = ZSTREAM_PUT,
	ZSTREAM_CALL = ZSTREAM_POST,
};

#define ZSTREAM_DEBUG		0x0100
#define ZSTREAM_NONBLOCK	0x0200

typedef struct zstream zstream_t;

typedef struct zstream_req {
	void *data;
	size_t len;
	uint64_t offset;
	uint64_t size;
	const char *name;
	const char *type;
	time_t time;
} zstream_req_t;


zstream_t* zstream_open(const char *url, int action);
int zstream_reopen(zstream_t *stream, const char *url, int action);
ssize_t zstream_read(zstream_t *stream, void *buf, size_t len);
ssize_t zstream_write(zstream_t *stream, const void *buf, size_t len);
int zstream_close(zstream_t *stream);

ssize_t zstream_recvmsg(zstream_t *stream, zstream_req_t *req);
ssize_t zstream_sendmsg(zstream_t *stream, zstream_req_t *req);

char* zstream_urlencode(const char *in, int encode_plus);
char* zstream_urldecode(const char *in, int decode_plus);
char* zstream_b64encode(const void *in, size_t *len);
void* zstream_b64decode(const char *in, size_t *len);

#endif /* ZSTREAM_H_ */
