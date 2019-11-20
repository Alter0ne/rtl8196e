/* Test of <dirent.h> substitute in C++ mode.
   Copyright (C) 2010-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* Written by Bruno Haible <bruno@clisp.org>, 2010.  */

#define GNULIB_NAMESPACE gnulib
#include <config.h>

#include <dirent.h>

#include "signature.h"


SIGNATURE_CHECK (GNULIB_NAMESPACE::closedir, int, (DIR *));

#if GNULIB_TEST_FDOPENDIR
SIGNATURE_CHECK (GNULIB_NAMESPACE::fdopendir, DIR *, (int));
#endif

SIGNATURE_CHECK (GNULIB_NAMESPACE::opendir, DIR *, (const char *));

#if GNULIB_TEST_SCANDIR
SIGNATURE_CHECK (GNULIB_NAMESPACE::scandir, int,
                 (const char *, struct dirent ***,
                  int (*) (const struct dirent *),
                  int (*) (const struct dirent **, const struct dirent **)));
#endif

#if GNULIB_TEST_ALPHASORT
SIGNATURE_CHECK (GNULIB_NAMESPACE::alphasort, int,
                 (const struct dirent **, const struct dirent **));
#endif


int
main ()
{
}
