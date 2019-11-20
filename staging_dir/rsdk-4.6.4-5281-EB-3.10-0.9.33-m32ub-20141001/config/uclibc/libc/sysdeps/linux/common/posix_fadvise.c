/* vi: set sw=4 ts=4: */
/*
 * posix_fadvise() for uClibc
 * http://www.opengroup.org/onlinepubs/009695399/functions/posix_fadvise.html
 *
 * Copyright (C) 2000-2006 Erik Andersen <andersen@uclibc.org>
 *
 * Licensed under the LGPL v2.1, see the file COPYING.LIB in this tarball.
 */

#include <sys/syscall.h>

#ifdef __NR_arm_fadvise64_64
/* We handle the 64bit alignment issue which is why the arm guys renamed their
 * syscall in the first place.  So rename it back.
 */
# define __NR_fadvise64_64 __NR_arm_fadvise64_64
#endif

#if defined(__NR_fadvise64) || defined(__NR_fadvise64_64)
# include <fcntl.h>
# include <endian.h>
# include <bits/wordsize.h>

# if defined(__NR_fadvise64_64) && defined(__UCLIBC_HAS_LFS__)
#include <_lfs_64.h>

int posix_fadvise64(int fd, off64_t offset, off64_t len, int advice);
int posix_fadvise(int fd, off_t offset, off_t len, int advice)
{
	return posix_fadvise64(fd, offset, len, advice);
}
#else

int posix_fadvise(int fd, off_t offset, off_t len, int advice)
{
	int ret;
	INTERNAL_SYSCALL_DECL(err);

# ifdef __NR_fadvise64_64
#  if __WORDSIZE == 64
	ret = INTERNAL_SYSCALL(fadvise64_64, err, 4, fd, offset, len, advice);
#  else
#   if defined(__UCLIBC_SYSCALL_ALIGN_64BIT__) || defined(__arm__)
	ret = INTERNAL_SYSCALL(fadvise64_64, err, 6, fd, advice,
			OFF_HI_LO (offset), OFF_HI_LO (len));
#   else
	ret = INTERNAL_SYSCALL(fadvise64_64, err, 6, fd,
			OFF_HI_LO (offset), OFF_HI_LO (len), advice);
#   endif
#  endif
# else  /* __NR_fadvise64 */
#  if __WORDSIZE == 64
	ret = INTERNAL_SYSCALL(fadvise64, err, 4, fd, offset, len, advice);
#  else
#   if defined(__UCLIBC_SYSCALL_ALIGN_64BIT__)
	ret = INTERNAL_SYSCALL(fadvise64, err, 6, fd, /*unused*/0,
#   else
	ret = INTERNAL_SYSCALL(fadvise64, err, 5, fd,
#   endif
			OFF_HI_LO (offset), len, advice);
#  endif
#  endif
	if (INTERNAL_SYSCALL_ERROR_P (ret, err))
		return INTERNAL_SYSCALL_ERRNO (ret, err);
	return 0;
}
# if defined __UCLIBC_HAS_LFS__ && (!defined __NR_fadvise64_64 || __WORDSIZE == 64)
strong_alias(posix_fadvise,posix_fadvise64)
# endif
#endif
#endif
