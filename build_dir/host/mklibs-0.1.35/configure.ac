AC_INIT([mklibs],m4_esyscmd([head -n1 debian/changelog | awk -F'[\\\\(\\\\)]' '{ print $2 }' | xargs -I{} echo -n {}]))
AM_INIT_AUTOMAKE([foreign no-define])
AC_CONFIG_HEADERS([config.h])
AM_MAINTAINER_MODE

AC_CANONICAL_TARGET

AC_PROG_CXX

AC_CONFIG_FILES([
Makefile
lib/Makefile
lib/mklibs/Makefile
lib/mklibs/utils/Makefile
src/Makefile
src/mklibs-readelf/Makefile
])

AC_OUTPUT
