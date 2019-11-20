/* Code to enable profiling at program startup.
   Copyright (C) 1995,1996,1997,2000,2001,2002,2012 Free Software Foundation, Inc.
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

#include <sys/types.h>
#include <sys-gmon.h>
#include <stdlib.h>
#include <unistd.h>


/* definition from glibc/sysdeps/generic/entry.h */
#if defined(__mips__)
#define ENTRY_POINT __start
#elif defined(__arm__)
#define ENTRY_POINT _start
#endif

/* Beginning and end of our code segment. We cannot declare them
   as the external functions since we want the addresses of those
   labels. Taking the address of a function may have different
   meanings on different platforms. */
#ifdef ENTRY_POINT_DECL
ENTRY_POINT_DECL(extern)
#else
extern char ENTRY_POINT[];
#endif
extern char etext[];

#ifndef TEXT_START
# ifdef ENTRY_POINT_DECL
#  define TEXT_START ENTRY_POINT
# else
#  define TEXT_START &ENTRY_POINT
# endif
#endif

/* In ELF and COFF, we cannot use the normal constructor mechanism to call
   __gmon_start__ because gcrt1.o appears before crtbegin.o in the link.
   Instead crti.o calls it specially (see initfini.c).  */
extern void __gmon_start__ (void) __attribute__ ((constructor));

void
__gmon_start__ (void)
{
  /* Protect from being called more than once.  Since crti.o is linked
     into every shared library, each of their init functions will call us.  */
  static int called;

  if (called)
    return;

  called = 1;

  /* Start keeping profiling records.  */
  __monstartup ((u_long) TEXT_START, (u_long) &etext);

  /* Call _mcleanup before exiting; it will write out gmon.out from the
     collected data.  */
  atexit (&_mcleanup);
}
