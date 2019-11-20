/* vi: set sw=4 ts=4: */
/*
 * sync_file_range() for uClibc
 *
 * Copyright (C) 2008 Bernhard Reutner-Fischer <uclibc@uclibc.org>
 *
 * Licensed under the LGPL v2.1, see the file COPYING.LIB in this tarball.
 */

#include <sys/syscall.h>

#if defined __NR_sync_file_range && defined __UCLIBC_HAS_LFS__ && defined __USE_GNU
# include <fcntl.h>

# define __NR___syscall_sync_file_range __NR_sync_file_range
# ifdef __mips__
__attribute__((nomips16))
static __inline__ _syscall7(int, __syscall_sync_file_range, int, fd, int, dummy,
			    off_t, offset_hi, off_t, offset_lo,
			    off_t, nbytes_hi, off_t, nbytes_lo, unsigned int, flags)

int __attribute__((nomips16)) sync_file_range(int fd, off64_t offset, off64_t nbytes, unsigned int flags)
{
	return __syscall_sync_file_range(fd, 0,
		__LONG_LONG_PAIR((long)(offset >> 32), (long)(offset & 0xffffffff)),
		__LONG_LONG_PAIR((long)(nbytes >> 32), (long)(nbytes & 0xffffffff)),
		flags);
}
# else
static __inline__ _syscall6(int, __syscall_sync_file_range, int, fd,
			    off_t, offset_hi, off_t, offset_lo,
			    off_t, nbytes_hi, off_t, nbytes_lo, unsigned int, flags)

int sync_file_range(int fd, off64_t offset, off64_t nbytes, unsigned int flags)
{
	return __syscall_sync_file_range(fd,
		__LONG_LONG_PAIR((long)(offset >> 32), (long)(offset & 0xffffffff)),
		__LONG_LONG_PAIR((long)(nbytes >> 32), (long)(nbytes & 0xffffffff)),
		flags);
}
# endif
#endif
