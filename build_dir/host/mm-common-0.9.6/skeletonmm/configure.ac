## Copyright (c) 2010  Joe Hacker <joe@example.com>
##
## This file is part of skeletonmm.
##
## skeletonmm is free software: you can redistribute it and/or modify it
## under the terms of the GNU Lesser General Public License as published
## by the Free Software Foundation, either version 2.1 of the License,
## or (at your option) any later version.
##
## skeletonmm is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
## See the GNU Lesser General Public License for more details.
##
## You should have received a copy of the GNU Lesser General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

AC_INIT([skeletonmm], [1.2.3], [https://bugzilla.gnome.org/enter_bug.cgi?product=skeletonmm],
        [skeletonmm], [http://www.gtkmm.org/])
AC_PREREQ([2.59])

AC_CONFIG_SRCDIR([skeleton/skeletonmm.h])
AC_CONFIG_AUX_DIR([build])
AC_CONFIG_MACRO_DIR([build])
AC_CONFIG_HEADERS([build/config.h skeleton/skeletonmmconfig.h])

AM_INIT_AUTOMAKE([1.9 -Wno-portability check-news dist-bzip2 no-define nostdinc tar-pax])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES])
AM_MAINTAINER_MODE
AC_ARG_VAR([ACLOCAL_FLAGS], [aclocal flags, e.g. -I <macro dir>])

MM_PREREQ([0.9])
MM_INIT_MODULE([skeletonmm-1.0])

# Copy the mm-common .pl scripts into doc/, and use them from there,
# so we can dist them to avoid a tarball-build dependency.
MM_CONFIG_DOCTOOL_DIR([doc])

# http://www.gnu.org/software/libtool/manual/html_node/Updating-version-info.html
AC_SUBST([SKELETONMM_SO_VERSION], [0:0:0])

AC_PROG_CXX
AC_DISABLE_STATIC
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL

AC_SUBST([SKELETONMM_MODULES], ['skeleton-1.0 >= 1.2 glibmm-2.4 >= 2.16'])
PKG_CHECK_MODULES([SKELETONMM], [$SKELETONMM_MODULES])

MM_PKG_CONFIG_SUBST([GTHREAD_CFLAGS], [--cflags-only-other gthread-2.0])
MM_PKG_CONFIG_SUBST([GMMPROC_DIR], [--variable=gmmprocdir glibmm-2.4])

MM_ARG_ENABLE_DOCUMENTATION
MM_ARG_WITH_TAGFILE_DOC([libstdc++.tag], [mm-common-libstdc++])
MM_ARG_WITH_TAGFILE_DOC([libsigc++-2.0.tag], [sigc++-2.0])
MM_ARG_WITH_TAGFILE_DOC([glibmm-2.4.tag], [glibmm-2.4])

AC_LANG([C++])
MM_ARG_ENABLE_WARNINGS([SKELETONMM_WXXFLAGS],
                       [-Wall],
                       [-pedantic -Wall -Wextra],
                       [G SKELETON])

AC_CONFIG_FILES([Makefile
                 codegen/Makefile
                 skeleton/${SKELETONMM_MODULE_NAME}.pc:skeleton/skeletonmm.pc.in
                 skeleton/${SKELETONMM_MODULE_NAME}-uninstalled.pc:skeleton/skeletonmm-uninstalled.pc.in
                 skeleton/src/Makefile
                 skeleton/skeletonmm/Makefile
                 examples/Makefile
                 doc/Makefile
                 doc/reference/Doxyfile])
AC_OUTPUT
