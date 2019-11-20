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

include $(top_srcdir)/skeleton/src/filelist.am
include $(top_srcdir)/skeleton/skeletonmm/filelist.am

skeletonmm_files_h = $(files_built_h) $(filter-out wrap_init.h,$(files_extra_h))

book_name = $(SKELETONMM_MODULE_NAME)
doc_input = $(top_srcdir)/skeleton/skeletonmm.h	$(addprefix\
            $(top_srcdir)/skeleton/skeletonmm/,$(skeletonmm_files_h))

include $(top_srcdir)/build/doc-reference.am
