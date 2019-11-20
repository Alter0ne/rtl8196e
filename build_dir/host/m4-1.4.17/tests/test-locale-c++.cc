/* Test of <locale.h> substitute in C++ mode.
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

#include <locale.h>

#include "signature.h"


#if GNULIB_TEST_SETLOCALE
SIGNATURE_CHECK (GNULIB_NAMESPACE::setlocale, char *, (int, const char *));
#endif

#if GNULIB_TEST_DUPLOCALE && HAVE_DUPLOCALE
SIGNATURE_CHECK (GNULIB_NAMESPACE::duplocale, locale_t, (locale_t));
#endif


int
main ()
{
}
