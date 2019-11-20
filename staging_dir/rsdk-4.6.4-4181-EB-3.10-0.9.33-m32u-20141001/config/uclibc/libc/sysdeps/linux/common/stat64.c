/* vi: set sw=4 ts=4: */
/*
 * stat64() for uClibc
 *
 * Copyright (C) 2000-2006 Erik Andersen <andersen@uclibc.org>
 *
 * Licensed under the LGPL v2.1, see the file COPYING.LIB in this tarball.
 */

#include <_lfs_64.h>
#include <sys/syscall.h>

#ifdef __NR_stat64
# include <sys/stat.h>
# include "xstatconv.h"
# define __NR___syscall_stat64 __NR_stat64
static __always_inline _syscall2(int, __syscall_stat64,
				 const char *, file_name, struct kernel_stat64 *, buf)

int stat64(const char *file_name, struct stat64 *buf)
{
	int result;
	struct kernel_stat64 kbuf;

	result = __syscall_stat64(file_name, &kbuf);
	if (result == 0) {
		__xstat64_conv(&kbuf, buf);
	}
	return result;
}
libc_hidden_def(stat64)
#endif
