/*
 * SPDX-License-Identifier: ISC
 *
 * Copyright (c) 2013-2019 Todd C. Miller <Todd.Miller@sudo.ws>
 *
 * Permission to use, copy, modify, and distribute this software for any
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

/*
 * This is an open source non-commercial project. Dear PVS-Studio, please check it.
 * PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
 */

#include <config.h>

#include <sys/types.h>

#include <stdio.h>
#include <stdlib.h>
#if defined(HAVE_STDINT_H)
# include <stdint.h>
#elif defined(HAVE_INTTYPES_H)
# include <inttypes.h>
#endif
#ifdef HAVE_STDBOOL_H
# include <stdbool.h>
#else
# include "compat/stdbool.h"
#endif
#include <ctype.h>
#include <errno.h>
#include <limits.h>

#define DEFAULT_TEXT_DOMAIN	"sudo"
#include "sudo_gettext.h"	/* must be included before sudo_compat.h */

#include "sudo_compat.h"
#include "sudo_debug.h"
#include "sudo_util.h"

/*
 * Make sure that the ID ends with a valid separator char.
 */
static bool
valid_separator(const char *p, const char *ep, const char *sep)
{
    bool valid = false;
    debug_decl(valid_separator, SUDO_DEBUG_UTIL)

    if (ep != p) {
	/* check for valid separator (including '\0') */
	if (sep == NULL)
	    sep = "";
	do {
	    if (*ep == *sep)
		valid = true;
	} while (*sep++ != '\0');
    }
    debug_return_bool(valid);
}

/*
 * Parse a uid/gid in string form.
 * If sep is non-NULL, it contains valid separator characters (e.g. comma, space)
 * If endp is non-NULL it is set to the next char after the ID.
 * On success, returns the parsed ID and clears errstr.
 * On error, returns 0 and sets errstr.
 */
#if SIZEOF_ID_T == SIZEOF_LONG_LONG
id_t
sudo_strtoid_v1(const char *p, const char *sep, char **endp, const char **errstr)
{
    char *ep;
    id_t ret = 0;
    long long llval;
    debug_decl(sudo_strtoid, SUDO_DEBUG_UTIL)

    /* skip leading space so we can pick up the sign, if any */
    while (isspace((unsigned char)*p))
	p++;

    /* While id_t may be 64-bit signed, uid_t and gid_t are 32-bit unsigned. */
    errno = 0;
    llval = strtoll(p, &ep, 10);
    if ((errno == ERANGE && llval == LLONG_MAX) || llval > (id_t)UINT_MAX) {
	errno = ERANGE;
	if (errstr != NULL)
	    *errstr = N_("value too large");
	goto done;
    }
    if ((errno == ERANGE && llval == LLONG_MIN) || llval < INT_MIN) {
	errno = ERANGE;
	if (errstr != NULL)
	    *errstr = N_("value too small");
	goto done;
    }

    /* Disallow id -1, which means "no change". */
    if (!valid_separator(p, ep, sep) || llval == -1 || llval == (id_t)UINT_MAX) {
	if (errstr != NULL)
	    *errstr = N_("invalid value");
	errno = EINVAL;
	goto done;
    }
    ret = (id_t)llval;
    if (errstr != NULL)
	*errstr = NULL;
    if (endp != NULL)
	*endp = ep;
done:
    debug_return_id_t(ret);
}
#else
id_t
sudo_strtoid_v1(const char *p, const char *sep, char **endp, const char **errstr)
{
    char *ep;
    id_t ret = 0;
    debug_decl(sudo_strtoid, SUDO_DEBUG_UTIL)

    /* skip leading space so we can pick up the sign, if any */
    while (isspace((unsigned char)*p))
	p++;

    errno = 0;
    if (*p == '-') {
	long lval = strtol(p, &ep, 10);
	if ((errno == ERANGE && lval == LONG_MAX) || lval > INT_MAX) {
	    errno = ERANGE;
	    if (errstr != NULL)
		*errstr = N_("value too large");
	    goto done;
	}
	if ((errno == ERANGE && lval == LONG_MIN) || lval < INT_MIN) {
	    errno = ERANGE;
	    if (errstr != NULL)
		*errstr = N_("value too small");
	    goto done;
	}

	/* Disallow id -1, which means "no change". */
	if (!valid_separator(p, ep, sep) || lval == -1) {
	    if (errstr != NULL)
		*errstr = N_("invalid value");
	    errno = EINVAL;
	    goto done;
	}
	ret = (id_t)lval;
    } else {
	unsigned long ulval = strtoul(p, &ep, 10);
	if ((errno == ERANGE && ulval == ULONG_MAX) || ulval > UINT_MAX) {
	    errno = ERANGE;
	    if (errstr != NULL)
		*errstr = N_("value too large");
	    goto done;
	}

	/* Disallow id -1, which means "no change". */
	if (!valid_separator(p, ep, sep) || ulval == UINT_MAX) {
	    if (errstr != NULL)
		*errstr = N_("invalid value");
	    errno = EINVAL;
	    goto done;
	}
	ret = (id_t)ulval;
    }
    if (errstr != NULL)
	*errstr = NULL;
    if (endp != NULL)
	*endp = ep;
done:
    debug_return_id_t(ret);
}
#endif /* SIZEOF_ID_T == 8 */
