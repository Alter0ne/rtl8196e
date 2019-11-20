/*
 * Copyright © 2012-2015 Michael Heimpold <mhei@heimpold.de>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#ifndef UGPIO_VERSION_H
#define UGPIO_VERSION_H

/* The major version, (1, if %LIBUGPIO_VERSION is 1.2.3) */
#define LIBUGPIO_VERSION_MAJOR (0)

/* The minor version (2, if %LIBUGPIO_VERSION is 1.2.3) */
#define LIBUGPIO_VERSION_MINOR (0)

/* The micro version (3, if %LIBUGPIO_VERSION is 1.2.3) */
#define LIBUGPIO_VERSION_MICRO (6)

/* The full version, like 1.2.3 */
#define LIBUGPIO_VERSION        0.0.6

/* The full version, in string form (suited for string concatenation) */
#define LIBUGPIO_VERSION_STRING "0.0.6"

/* Numerically encoded version, like 0x010203 */
#define LIBUGPIO_VERSION_HEX ((LIBUGPIO_MAJOR_VERSION << 24) |   \
                              (LIBUGPIO_MINOR_VERSION << 16) |   \
                              (LIBUGPIO_MICRO_VERSION << 8))

/* Evaluates to True if the version is greater than @major, @minor and @micro */
#define LIBUGPIO_VERSION_CHECK(major,minor,micro)      \
    (LIBUGPIO_VERSION_MAJOR > (major) ||               \
     (LIBUGPIO_VERSION_MAJOR == (major) &&             \
      LIBUGPIO_VERSION_MINOR > (minor)) ||             \
     (LIBUGPIO_VERSION_MAJOR == (major) &&             \
      LIBUGPIO_VERSION_MINOR == (minor) &&             \
      LIBUGPIO_VERSION_MICRO >= (micro)))

#endif /* UGPIO_VERSION_H */
