/* -*- knf -*- */
/* $AMSEL: compat.h,v 1.7 2004/04/11 14:00:25 reyk Exp $ */

#ifndef _LIBAMSEL_COMPAT_HEADER
#define _LIBAMSEL_COMPAT_HEADER

#include <string.h>
#include <sys/types.h>
#include "compat/queue.h"

#ifndef __GNUC__
#define __GNUC__
#endif

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#ifndef strlcpy
extern size_t
strlcpy(char *, const char *, size_t);
#endif

#ifndef strlcat
extern size_t
strlcat(char *, const char *, size_t);
#endif

#ifndef arc4random
extern u_int32_t
arc4random(void);
#endif

#ifndef atoul
int atoul(char *, u_long *);
#endif

#endif /* _LIBAMSEL_COMPAT_HEADER */
