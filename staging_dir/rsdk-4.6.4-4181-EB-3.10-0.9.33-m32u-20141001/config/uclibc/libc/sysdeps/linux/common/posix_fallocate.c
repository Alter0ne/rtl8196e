/* vi: set sw=4 ts=4: */
/*
 * posix_fallocate() for uClibc
 * http://www.opengroup.org/onlinepubs/9699919799/functions/posix_fallocate.html
 *
 * Copyright (C) 2000-2006 Erik Andersen <andersen@uclibc.org>
 *
 * Licensed under the LGPL v2.1 or later, see the file COPYING.LIB in this tarball.
 */

#include <sys/syscall.h>
#include <fcntl.h>
#include <bits/kernel-features.h>
#include <stdint.h>

#if defined __NR_fallocate
int posix_fallocate(int fd, __off_t offset, __off_t len)
{
	int ret;

# if __WORDSIZE == 32
	INTERNAL_SYSCALL_DECL(err);
	ret = (int) (INTERNAL_SYSCALL(fallocate, err, 6, fd, 0,
		__LONG_LONG_PAIR (0, offset),
		__LONG_LONG_PAIR (0, len)));
# elif __WORDSIZE == 64
	INTERNAL_SYSCALL_DECL(err);
	ret = (int) (INTERNAL_SYSCALL(fallocate, err, 4, fd, 0, offset, len));
# else
# error your machine is neither 32 bit or 64 bit ... it must be magical
#endif
	if (unlikely(INTERNAL_SYSCALL_ERROR_P (ret, err)))
		return INTERNAL_SYSCALL_ERRNO (ret, err);
	return 0;
}
# if defined __UCLIBC_HAS_LFS__ && __WORDSIZE == 64
strong_alias(posix_fallocate,posix_fallocate64)
# endif
#endif
