## Copyright (c) 2009  Openismus GmbH  <http://www.openismus.com/>
##
## This file is part of mm-common.
##
## mm-common is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published
## by the Free Software Foundation, either version 2 of the License,
## or (at your option) any later version.
##
## mm-common is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with mm-common.  If not, see <http://www.gnu.org/licenses/>.

# Pick up aclocal flags from the environment.
ACLOCAL_AMFLAGS = ${ACLOCAL_FLAGS}

bin_SCRIPTS = util/mm-common-prepare
man1_MANS = util/mm-common-prepare.1

# These are installed so that mm-common-prepare can copy them into projects at autogen.sh time:
build_supportdir = $(pkgdatadir)/build
dist_build_support_DATA =		\
	build/compile-binding.am	\
	build/dist-changelog.am		\
	build/doc-reference.am		\
	build/generate-binding.am

# These are installed so that mm-common-prepare can copy them into projects at autogen.sh time:
aclocal_macrodir = $(datadir)/aclocal
aclocal_macro_DATA = macros/mm-common.m4
dist_aclocal_macro_DATA =		\
	macros/mm-dietlib.m4		\
	macros/mm-doc.m4		\
	macros/mm-module.m4		\
	macros/mm-pkg.m4		\
	macros/mm-warnings.m4

# These are installed so that mm-common-prepare can copy them into projects at autogen.sh time,
# if a directory path is given to MM_CONFIG_DOCTOOL_DIR(),
# or they can be found via pkg-config --variable=doctooldir mm-common,
# which is preferrable.
doctooldir = $(pkgdatadir)/doctool
dist_doctool_DATA =			\
	util/doc-install.pl		\
	util/doc-postprocess.pl		\
	util/doxygen.css		\
	util/tagfile-to-devhelp2.xsl

doctagsdir = $(pkgdatadir)/doctags
dist_doctags_DATA = doctags/libstdc++.tag

# Let other modules find the utils with pkg-config:
shared_pkgconfigdir = $(datadir)/pkgconfig
nodist_shared_pkgconfig_DATA =		\
	doctags/mm-common-libstdc++.pc	\
	util/mm-common-util.pc
noinst_DATA = doctags/mm-common-libstdc++-uninstalled.pc

dist_doc_DATA = README
doc_DATA = skeletonmm.tar.gz

skeletonmm_data_files =							\
	skeletonmm/.gitignore						\
	skeletonmm/AUTHORS						\
	skeletonmm/COPYING						\
	skeletonmm/ChangeLog						\
	skeletonmm/Makefile.am						\
	skeletonmm/README						\
	skeletonmm/configure.ac						\
	skeletonmm/skeletonmm.doap					\
	skeletonmm/build/.gitignore					\
	skeletonmm/codegen/Makefile.am					\
	skeletonmm/codegen/extradefs/generate_extra_defs_skeleton.cc	\
	skeletonmm/codegen/m4/convert.m4				\
	skeletonmm/codegen/m4/convert_skeleton.m4			\
	skeletonmm/codegen/m4/filelist.am				\
	skeletonmm/doc/Makefile.am					\
	skeletonmm/doc/reference/.gitignore				\
	skeletonmm/doc/reference/Doxyfile.in				\
	skeletonmm/examples/.gitignore					\
	skeletonmm/examples/Makefile.am					\
	skeletonmm/examples/example/example.cc				\
	skeletonmm/skeleton/.gitignore					\
	skeletonmm/skeleton/skeletonmm-uninstalled.pc.in		\
	skeletonmm/skeleton/skeletonmm.h				\
	skeletonmm/skeleton/skeletonmm.pc.in				\
	skeletonmm/skeleton/skeletonmmconfig.h.in			\
	skeletonmm/skeleton/skeletonmm/Makefile.am			\
	skeletonmm/skeleton/skeletonmm/filelist.am			\
	skeletonmm/skeleton/skeletonmm/wrap_init.h			\
	skeletonmm/skeleton/skeletonmm/private/.gitignore		\
	skeletonmm/skeleton/src/Makefile.am				\
	skeletonmm/skeleton/src/filelist.am				\
	skeletonmm/skeleton/src/skeleton.ccg				\
	skeletonmm/skeleton/src/skeleton.defs				\
	skeletonmm/skeleton/src/skeleton.hg				\
	skeletonmm/skeleton/src/skeleton_docs.xml			\
	skeletonmm/skeleton/src/skeleton_enum.defs			\
	skeletonmm/skeleton/src/skeleton_extra.defs			\
	skeletonmm/skeleton/src/skeleton_method.defs

skeletonmm_files = skeletonmm/autogen.sh $(skeletonmm_data_files)

dist_noinst_DATA = util/mm-common-prepare.1.in $(skeletonmm_data_files)
dist_noinst_SCRIPTS = autogen.sh skeletonmm/autogen.sh

CLEANFILES = $(man1_MANS) $(doc_DATA)
MAINTAINERCLEANFILES = $(dist_doctags_DATA)

# Remote location of the GNU libstdc++ Doxygen tag file.
libstdcxx_tag_url = http://gcc.gnu.org/onlinedocs/libstdc++/latest-doxygen/libstdc++.tag

# Commands for downloading a target from a remote location, taking care
# not to download a file again if its time stamp has not changed.
download_curl = $(CURL) --compressed --connect-timeout 300 -g -L -m 3600 -R --retry 5 \
		$(if $(wildcard $@),-z $@) -o $@
download_wget = $(WGET) -N -nd -T 300 -t 5 -P $(dir $@)

subst_manpage = $(SED) 's|[@]PACKAGE_STRING[@]|$(PACKAGE_STRING)|g;s|[@]docdir[@]|$(docdir)|g'
srctar_stdout = cd $(top_srcdir) >/dev/null && $(TAR) chof -

# If maintainer mode is enabled and network is enabled then always
# check the remote locations of the tag files for updates when
# building the default target.
if MAINTAINER_MODE
if NETWORK_ENABLED
check_updates = $(if $(filter all%,$(or $(MAKECMDGOALS),all)),$(dist_doctags_DATA))
else
check_updates =
endif
else
check_updates =
endif

.PHONY: $(check_updates) postinst-acdir-notice

# Avoid shipping partially downloaded files.
.DELETE_ON_ERROR:

# Download the libstdc++ tag file from the GCC website.
doctags/libstdc++.tag:
	$(AM_V_GEN)$(if $(filter no,$(USE_NETWORK)),\
	echo 'Error: $@ does not exist.';\
	echo 'Downloading it is not possible because network is disabled.';\
	echo 'Please run configure script with --enable-network flag.';\
	echo 'Bailing out.' >&2;\
	exit 1;,\
	$(if\
	$(CURL),$(download_curl) '$(libstdcxx_tag_url)',$(if\
	$(WGET),$(download_wget) '$(libstdcxx_tag_url)',test -f $@)))

# Build the mm-common-prepare(1) manual page.
util/mm-common-prepare.1: $(srcdir)/util/mm-common-prepare.1.in Makefile
	$(AM_V_GEN)$(subst_manpage) $(srcdir)/util/mm-common-prepare.1.in >$@

# Create tar archive of skeletonmm for installation.
skeletonmm.tar.gz: $(skeletonmm_files)
	$(AM_V_GEN)($(srctar_stdout) $(skeletonmm_files)) | gzip -c >$@

include $(top_srcdir)/build/dist-changelog.am

install-data-hook: $(if $(DESTDIR),,postinst-acdir-notice)

# Inform the installer that M4 macro files installed in a prefix
# not known to aclocal will not be picked up automatically.
postinst-acdir-notice:
	@$(POST_INSTALL)
	@-acdir=`$${ACLOCAL-aclocal} --print-ac-dir 2>/dev/null || :`; \
	if test "x$$acdir" = 'x$(aclocal_macrodir)'; then :; else \
	  case " $$ACLOCAL_FLAGS " in *' -I $(aclocal_macrodir) '*) ;; *) \
	    echo; echo 'NOTICE:'; echo; \
	    echo 'The $(PACKAGE_NAME) Autoconf macro files have been installed in a different'; \
	    echo 'prefix than the system aclocal directory.  In order for the installed'; \
	    echo 'macros to be found, it may be necessary to add the $(PACKAGE_NAME) include'; \
	    echo 'path to the ACLOCAL_FLAGS environment variable:'; echo; \
	    echo "  ACLOCAL_FLAGS='-I $(aclocal_macrodir)'; export ACLOCAL_FLAGS"; echo;; \
	  esac; \
	fi
