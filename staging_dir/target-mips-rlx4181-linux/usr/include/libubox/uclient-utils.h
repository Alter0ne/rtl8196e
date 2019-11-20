/*
 * uclient - ustream based protocol client library
 *
 * Copyright (C) 2014 Felix Fietkau <nbd@openwrt.org>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#ifndef __UCLIENT_UTILS_H
#define __UCLIENT_UTILS_H

#include <stdbool.h>

struct http_digest_data {
	const char *uri;
	const char *method;

	const char *auth_hash; /* H(A1) */
	const char *qop;
	const char *nc;
	const char *nonce;
	const char *cnonce;
};

static inline int base64_len(int len)
{
	return ((len + 2) / 3) * 4;
}

void base64_encode(const void *inbuf, unsigned int len, void *out);
void bin_to_hex(char *dest, const void *buf, int len);

int uclient_urldecode(const char *in, char *out, bool decode_plus);

void http_digest_calculate_auth_hash(char *dest, const char *user, const char *realm, const char *password);
void http_digest_calculate_response(char *dest, const struct http_digest_data *data);

char *uclient_get_url_filename(const char *url, const char *default_name);

#endif
