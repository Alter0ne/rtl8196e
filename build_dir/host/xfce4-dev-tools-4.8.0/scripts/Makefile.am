# $Id$
#
# Copyright (c) 2002-2005
#         The Xfce development team. All rights reserved.
#
# Written for Xfce by Benedikt Meurer <benny@xfce.org>.
#

bin_SCRIPTS =								\
	xdt-autogen							\
	xdt-commit							

xdt-autogen: Makefile $(srcdir)/xdt-autogen.in
	rm -f xdt-autogen xdt-autogen.gen
	sed -e 's,\@DATADIR\@,$(datadir),g' \
		xdt-autogen.in \
		> xdt-autogen.gen
	mv xdt-autogen.gen xdt-autogen

CLEANFILES =								\
	xdt-autogen

DISTCLEANFILES = 							\
	xdt-autogen.in

EXTRA_DIST =								\
	xdt-commit

# vi:set ts=8 sw=8 noet ai:
