/* vi: set sw=4 ts=4: */
/*
 * fork() for uClibc
 *
 * Copyright (C) 2000-2006 Erik Andersen <andersen@uclibc.org>
 *
 * Licensed under the LGPL v2.1, see the file COPYING.LIB in this tarball.
 */

#include <sys/syscall.h>

#if defined __NR_fork && defined __ARCH_USE_MMU__
# include <unistd.h>
# include <cancel.h>
#  define __NR___libc_fork __NR_fork
_syscall0(pid_t, __libc_fork)
# ifdef __UCLIBC_HAS_THREADS__
#	ifndef SHARED
libc_hidden_proto(fork)
weak_alias(__libc_fork, fork)
libc_hidden_weak(fork)
# endif
# else
weak_alias(__libc_fork, fork)
libc_hidden_def(fork)
# endif
#endif
