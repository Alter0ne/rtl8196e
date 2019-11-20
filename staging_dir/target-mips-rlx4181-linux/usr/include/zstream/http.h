/*
 * http.h
 *
 *  Created on: 18.12.2010
 *      Author: steven
 */

#ifndef ZSTREAM_HTTP_H_
#define ZSTREAM_HTTP_H_

#include "../zstream.h"
#include <openssl/ssl.h>

enum zstream_http_conf {
	ZSTREAM_HTTP_TIMEOUT,
	ZSTREAM_HTTP_LINKDEPTH,
	ZSTREAM_HTTP_COOKIES,
};

int zstream_http_addcookie(zstream_t *stream, const char *cookie);
int zstream_http_getcookies(zstream_t *stream, char **vals, size_t len);

int zstream_http_addheader(zstream_t *stream, const char *key, const char *val);
int zstream_http_getheader(zstream_t *stream, const char *key, char **vals, size_t len);

int zstream_http_set_ssl(zstream_t *stream, SSL_CTX *ctx);
int zstream_http_configure(zstream_t *stream, enum zstream_http_conf key, unsigned value);

#endif /* ZSTREAM_HTTP_H_ */
