# PGXS: PostgreSQL extensions makefile

# $PostgreSQL: pgsql/src/makefiles/pgxs.mk,v 1.22 2010/07/05 23:40:13 tgl Exp $ 

# This file contains generic rules to build many kinds of simple
# extension modules.  You only need to set a few variables and include
# this file, the rest will be done here.
#
# Use the following layout for your Makefile:
#
#   [variable assignments, see below]
#   [custom rules, rarely necessary]
#
#   PG_CONFIG = pg_config
#   PGXS := $(shell $(PG_CONFIG) --pgxs)
#   include $(PGXS)
#
# Set one of these three variables to specify what is built:
#
#   MODULES -- list of shared objects to be built from source files with
#     same stem (do not include suffix in this list)
#   MODULE_big -- a shared object to build from multiple source files
#     (list object files in OBJS)
#   PROGRAM -- a binary program to build (list object files in OBJS)
#
# The following variables can also be set:
#
#   MODULEDIR -- subdirectory into which DATA and DOCS files should be
#     installed (if not set, default is "contrib")
#   DATA -- random files to install into $PREFIX/share/$MODULEDIR
#   DATA_built -- random files to install into $PREFIX/share/$MODULEDIR,
#     which need to be built first
#   DATA_TSEARCH -- random files to install into $PREFIX/share/tsearch_data
#   DOCS -- random files to install under $PREFIX/doc/$MODULEDIR
#   SCRIPTS -- script files (not binaries) to install into $PREFIX/bin
#   SCRIPTS_built -- script files (not binaries) to install into $PREFIX/bin,
#     which need to be built first
#   REGRESS -- list of regression test cases (without suffix)
#   EXTRA_CLEAN -- extra files to remove in 'make clean'
#   PG_CPPFLAGS -- will be added to CPPFLAGS
#   PG_LIBS -- will be added to PROGRAM link line
#   SHLIB_LINK -- will be added to MODULE_big link line
#   PG_CONFIG -- path to pg_config program for the PostgreSQL installation
#     to build against (typically just "pg_config" to use the first one in
#     your PATH)
#
# Better look at some of the existing uses for examples...

ifndef PGXS
ifndef NO_PGXS
$(error pgxs error: makefile variable PGXS or NO_PGXS must be set)
endif
endif


ifdef PGXS
# We assume that we are in src/makefiles/, so top is ...
top_builddir := $(dir $(PGXS))../..
include $(top_builddir)/src/Makefile.global

top_srcdir = $(top_builddir)
srcdir = .
VPATH =
endif


override CPPFLAGS := -I. -I$(srcdir) $(CPPFLAGS)

ifdef MODULES
override CFLAGS += $(CFLAGS_SL)
endif

ifdef MODULEDIR
datamoduledir = $(MODULEDIR)
docmoduledir = $(MODULEDIR)
else
datamoduledir = contrib
docmoduledir = contrib
endif

ifdef PG_CPPFLAGS
override CPPFLAGS := $(PG_CPPFLAGS) $(CPPFLAGS)
endif

all: $(PROGRAM) $(DATA_built) $(SCRIPTS_built) $(addsuffix $(DLSUFFIX), $(MODULES))

ifdef MODULE_big
# shared library parameters
NAME = $(MODULE_big)

include $(top_srcdir)/src/Makefile.shlib

all: all-lib
endif # MODULE_big


install: all installdirs
ifneq (,$(DATA)$(DATA_built))
	@for file in $(addprefix $(srcdir)/, $(DATA)) $(DATA_built); do \
	  echo "$(INSTALL_DATA) $$file '$(DESTDIR)$(datadir)/$(datamoduledir)'"; \
	  $(INSTALL_DATA) $$file '$(DESTDIR)$(datadir)/$(datamoduledir)' || exit; \
	done
endif # DATA
ifneq (,$(DATA_TSEARCH))
	@for file in $(addprefix $(srcdir)/, $(DATA_TSEARCH)); do \
	  echo "$(INSTALL_DATA) $$file '$(DESTDIR)$(datadir)/tsearch_data'"; \
	  $(INSTALL_DATA) $$file '$(DESTDIR)$(datadir)/tsearch_data' || exit; \
	done
endif # DATA_TSEARCH
ifdef MODULES
	@for file in $(addsuffix $(DLSUFFIX), $(MODULES)); do \
	  echo "$(INSTALL_SHLIB) $$file '$(DESTDIR)$(pkglibdir)'"; \
	  $(INSTALL_SHLIB) $$file '$(DESTDIR)$(pkglibdir)' || exit; \
	done
endif # MODULES
ifdef DOCS
ifdef docdir
	@for file in $(addprefix $(srcdir)/, $(DOCS)); do \
	  echo "$(INSTALL_DATA) $$file '$(DESTDIR)$(docdir)/$(docmoduledir)'"; \
	  $(INSTALL_DATA) $$file '$(DESTDIR)$(docdir)/$(docmoduledir)' || exit; \
	done
endif # docdir
endif # DOCS
ifdef PROGRAM
	$(INSTALL_PROGRAM) $(PROGRAM)$(X) '$(DESTDIR)$(bindir)'
endif # PROGRAM
ifdef SCRIPTS
	@for file in $(addprefix $(srcdir)/, $(SCRIPTS)); do \
	  echo "$(INSTALL_SCRIPT) $$file '$(DESTDIR)$(bindir)'"; \
	  $(INSTALL_SCRIPT) $$file '$(DESTDIR)$(bindir)' || exit; \
	done
endif # SCRIPTS
ifdef SCRIPTS_built
	@for file in $(SCRIPTS_built); do \
	  echo "$(INSTALL_SCRIPT) $$file '$(DESTDIR)$(bindir)'"; \
	  $(INSTALL_SCRIPT) $$file '$(DESTDIR)$(bindir)' || exit; \
	done
endif # SCRIPTS_built

ifdef MODULE_big
install: install-lib
endif # MODULE_big


installdirs:
ifneq (,$(DATA)$(DATA_built))
	$(MKDIR_P) '$(DESTDIR)$(datadir)/$(datamoduledir)'
endif
ifneq (,$(DATA_TSEARCH))
	$(MKDIR_P) '$(DESTDIR)$(datadir)/tsearch_data'
endif
ifneq (,$(MODULES))
	$(MKDIR_P) '$(DESTDIR)$(pkglibdir)'
endif
ifdef DOCS
ifdef docdir
	$(MKDIR_P) '$(DESTDIR)$(docdir)/$(docmoduledir)'
endif # docdir
endif # DOCS
ifneq (,$(PROGRAM)$(SCRIPTS)$(SCRIPTS_built))
	$(MKDIR_P) '$(DESTDIR)$(bindir)'
endif

ifdef MODULE_big
installdirs: installdirs-lib
endif # MODULE_big


uninstall:
ifneq (,$(DATA)$(DATA_built))
	rm -f $(addprefix '$(DESTDIR)$(datadir)/$(datamoduledir)'/, $(notdir $(DATA) $(DATA_built)))
endif
ifneq (,$(DATA_TSEARCH))
	rm -f $(addprefix '$(DESTDIR)$(datadir)/tsearch_data'/, $(notdir $(DATA_TSEARCH)))
endif
ifdef MODULES
	rm -f $(addprefix '$(DESTDIR)$(pkglibdir)'/, $(addsuffix $(DLSUFFIX), $(MODULES)))
endif
ifdef DOCS
	rm -f $(addprefix '$(DESTDIR)$(docdir)/$(docmoduledir)'/, $(DOCS))
endif
ifdef PROGRAM
	rm -f '$(DESTDIR)$(bindir)/$(PROGRAM)$(X)'
endif
ifdef SCRIPTS
	rm -f $(addprefix '$(DESTDIR)$(bindir)'/, $(SCRIPTS))
endif
ifdef SCRIPTS_built
	rm -f $(addprefix '$(DESTDIR)$(bindir)'/, $(SCRIPTS_built))
endif

ifdef MODULE_big
uninstall: uninstall-lib
endif # MODULE_big


clean:
ifdef MODULES
	rm -f $(addsuffix $(DLSUFFIX), $(MODULES)) $(addsuffix .o, $(MODULES))
endif
ifdef DATA_built
	rm -f $(DATA_built)
endif
ifdef SCRIPTS_built
	rm -f $(SCRIPTS_built)
endif
ifdef PROGRAM
	rm -f $(PROGRAM)$(X)
endif
ifdef OBJS
	rm -f $(OBJS)
endif
ifdef EXTRA_CLEAN
	rm -f $(EXTRA_CLEAN)
endif
ifdef REGRESS
# things created by various check targets
	rm -rf results tmp_check log
	rm -f regression.diffs regression.out regress.out run_check.out
ifeq ($(PORTNAME), win)
	rm -f regress.def
endif
endif # REGRESS

ifdef MODULE_big
clean: clean-lib
endif

distclean maintainer-clean: clean


ifdef REGRESS

# Calling makefile can set REGRESS_OPTS, but this is the default:
ifndef REGRESS_OPTS
ifneq ($(USE_MODULE_DB),)
  REGRESS_OPTS = --dbname=$(CONTRIB_TESTDB_MODULE)
else
  REGRESS_OPTS = --dbname=$(CONTRIB_TESTDB)
endif
endif

# where to find psql for running the tests
PSQLDIR = $(bindir)

# When doing a VPATH build, must copy over the data files so that the
# driver script can find them.  We have to use an absolute path for
# the targets, because otherwise make will try to locate the missing
# files using VPATH, and will find them in $(srcdir), but the point
# here is that we want to copy them from $(srcdir) to the build
# directory.

ifdef VPATH
abs_builddir := $(shell pwd)
test_files_src := $(wildcard $(srcdir)/data/*.data)
test_files_build := $(patsubst $(srcdir)/%, $(abs_builddir)/%, $(test_files_src))

all: $(test_files_build)
$(test_files_build): $(abs_builddir)/%: $(srcdir)/%
	ln -s $< $@
endif # VPATH

.PHONY: submake
submake:
ifndef PGXS
	$(MAKE) -C $(top_builddir)/src/test/regress pg_regress$(X)
endif

# against installed postmaster
installcheck: submake
	$(top_builddir)/src/test/regress/pg_regress --inputdir=$(srcdir) --psqldir="$(PSQLDIR)" $(REGRESS_OPTS) $(REGRESS)

# in-tree test doesn't work yet (no way to install my shared library)
#check: all submake
#	$(top_builddir)/src/test/regress/pg_regress --temp-install \
#	  --top-builddir=$(top_builddir) $(REGRESS_OPTS) $(REGRESS)
check:
	@echo "'make check' is not supported."
	@echo "Do 'make install', then 'make installcheck' instead."
	@exit 1
endif # REGRESS


# STANDARD RULES

ifneq (,$(MODULES)$(MODULE_big))
%.sql: %.sql.in
	sed 's,MODULE_PATHNAME,$$libdir/$*,g' $< >$@
endif

ifdef PROGRAM
$(PROGRAM): $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) $(PG_LIBS) $(LDFLAGS) $(LDFLAGS_EX) $(LIBS) -o $@$(X)
endif
