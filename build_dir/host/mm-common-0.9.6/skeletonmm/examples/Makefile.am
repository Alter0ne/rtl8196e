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

AUTOMAKE_OPTIONS = subdir-objects

check_PROGRAMS = example/example

local_includes = -I$(top_builddir)/skeleton $(if $(srcdir:.=),-I$(top_srcdir)/skeleton)
local_libs = $(top_builddir)/skeleton/skeletonmm/libskeletonmm-$(SKELETONMM_API_VERSION).la

AM_CPPFLAGS = -I$(top_builddir) $(local_includes) $(GTHREAD_CFLAGS) $(SKELETONMM_CFLAGS)
AM_CXXFLAGS = $(SKELETONMM_WXXFLAGS)
LDADD = $(SKELETONMM_LIBS) $(local_libs)

example_example_SOURCES = example/example.cc
