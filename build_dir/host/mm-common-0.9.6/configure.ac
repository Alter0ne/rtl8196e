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

AC_INIT([mm-common], [0.9.6], [https://bugzilla.gnome.org/enter_bug.cgi?product=mm-common],
        [mm-common], [http://www.gtkmm.org/])
AC_PREREQ([2.59])
AC_CONFIG_SRCDIR([util/mm-common-prepare.in])

AM_INIT_AUTOMAKE([1.9 -Wno-portability check-news dist-bzip2 no-define std-options])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES])
AM_MAINTAINER_MODE

AC_ARG_VAR([ACLOCAL_FLAGS], [aclocal flags, e.g. -I <macro dir>])
AC_ARG_VAR([CURL], [curl transfer command])
AC_ARG_VAR([WGET], [wget download command])

AC_PROG_SED
dnl list of tar execs is taken from the generated ./missing script.
AC_CHECK_PROGS([TAR], [tar gnutar gtar], [no])
AS_VAR_IF([TAR], [no], [AC_MSG_ERROR([tar utility not found])])

AC_MSG_CHECKING([whether to enable network])
AC_ARG_ENABLE([network],
              [AS_HELP_STRING([--enable-network],
                              [use network to download tags if needed @<:@default=same as maintainer mode@:>@])],
              [AS_VAR_IF([enableval], ['yes'],,
                         [AS_VAR_IF([enableval], ['no'],,
                                    [AC_MSG_ERROR([[Pass either `yes', `no' or nothing to --enable-network.]])]
                                   )
                         ]
                        )
               AS_VAR_SET([enable_network], ["$enableval"])
              ],
              [AS_VAR_SET([enable_network], ["$USE_MAINTAINER_MODE"])]
             )
AC_SUBST([USE_NETWORK], ["$enable_network"])
AM_CONDITIONAL(NETWORK_ENABLED, [test "x$enable_network" = 'xyes'])
AC_MSG_RESULT([$USE_NETWORK])

AS_VAR_IF([USE_NETWORK], ['yes'],
          [AC_CHECK_PROG([CURL], [curl], [curl])
           AC_CHECK_PROG([WGET], [wget], [wget])
           AS_VAR_IF([CURL], [],
                     [AS_VAR_IF([WGET], [],
                                [AC_MSG_FAILURE([[Network is enabled so either curl or wget is required for building mm-common.]])]
                               )
                     ]
                    )
          ]
         )

AC_CONFIG_FILES([Makefile
                 doctags/mm-common-libstdc++.pc
                 doctags/mm-common-libstdc++-uninstalled.pc
                 macros/mm-common.m4
                 util/mm-common-util.pc
                 util/mm-common-prepare])
AC_OUTPUT
