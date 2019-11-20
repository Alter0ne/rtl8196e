/* Copyright (C) 1996, 1999, 2001 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef _SYS_SYSINFO_H
#define _SYS_SYSINFO_H	1

#include <features.h>

/* Get sysinfo structure from kernel header.  */
#include <linux/kernel.h>

__BEGIN_DECLS

/* Returns information on overall system statistics.  */
extern int sysinfo (struct sysinfo *__info) __THROW;

/* Return number of configured processors.  */
#define get_nprocs_conf() (sysconf(_SC_NPROCESSORS_CONF))

/* Return number of available processors.  */
#define get_nprocs() (sysconf(_SC_NPROCESSORS_ONLN))


#if 0
/* Return number of physical pages of memory in the system.  */
extern long int get_phys_pages (void) __THROW;

/* Return number of available physical pages of memory in the system.  */
extern long int get_avphys_pages (void) __THROW;
#endif

__END_DECLS

#endif	/* sys/sysinfo.h */
