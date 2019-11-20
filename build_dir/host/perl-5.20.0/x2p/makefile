# $RCSfile: Makefile.SH,v 29979Revision: 4.1 29979Date: 92/08/07 18:29:07 $
#
# $Log:	Makefile.SH,v $

CC = cc
BYACC = byacc
LDFLAGS =  -fstack-protector -L/usr/local/lib
shellflags = 

libs = -lnsl -ldl -lm -lcrypt -lutil -lc

#
# grrr
SHELL = /bin/sh

# These variables may need to be manually set for non-Unix systems.
AR = ar
EXE_EXT = 
LIB_EXT = .a
OBJ_EXT = .o
PATH_SEP = :

FIRSTMAKEFILE = makefile

# how to tr(anslate) newlines

TRNL = '\n'

OPTIMIZE = -O2

.SUFFIXES: .c $(OBJ_EXT)

RUN =
PERL = ../perl


REALPERL = ../perl
CCCMD = `sh $(shellflags) ../cflags "optimize='$(OPTIMIZE)'" $@`

pl = find2perl.PL s2p.PL
plextract = find2perl s2p

h = EXTERN.h INTERN.h ../config.h ../handy.h hash.h a2p.h str.h util.h

c = hash.c str.c util.c walk.c

obj = hash$(OBJ_EXT) str$(OBJ_EXT) util$(OBJ_EXT) walk$(OBJ_EXT)

lintflags = -phbvxac


.c$(OBJ_EXT):
	$(CCCMD) -DPERL_FOR_X2P $*.c

all: a2p$(EXE_EXT) s2p find2perl
	@echo " "

a2p$(EXE_EXT): $(obj) a2p$(OBJ_EXT)
	$(CC) -o a2p $(LDFLAGS) $(obj) a2p$(OBJ_EXT) $(libs)

# I now supply a2p.c with the kits, so the following section is
# used only if you force byacc to run by saying
#    make run_byacc
# byacc 1.8.2 or 1.9 are recommended.

run_byacc:	FORCE
	@ echo Expect many shift/reduce and reduce/reduce conflicts
	$(BYACC) a2p.y
	rm -f a2p.c
	sed -e 's/(yyn = yydefred\[yystate\])/((yyn = yydefred[yystate]))/' \
	    -e 's/(yys = getenv("YYDEBUG"))/((yys = getenv("YYDEBUG")))/' \
	    -e 's/^yyerrlab://' \
	    -e 's/^    goto yyerrlab;//' \
	    -e 's/^yynewerror://' \
	    -e 's/^    goto yynewerror;//' \
	    -e 's|^static char yysccsid\(.*\)|/* static char yysccsid\1 */|' \
	    -e 's/^\(char \*yyname\[\]\)/const \1/' \
	    -e 's/^\(char \*yyrule\[\]\)/const \1/' \
	    -e 's/^\(    register\) \(char \*yys;\)/\1 const \2/' \
	    < y.tab.c > a2p.c

FORCE:

# We don't want to regenerate a2p.c, but it might appear out-of-date
# after a patch is applied or a new distribution is made.
a2p.c: a2p.y
	-@sh -c true

a2p$(OBJ_EXT): a2p.c a2py.c a2p.h EXTERN.h util.h INTERN.h \
		../handy.h ../config.h str.h hash.h
	$(CCCMD) a2p.c

clean:
	rm -f a2p$(EXE_EXT) psed *$(OBJ_EXT) $(plexe) $(plc)

distclean: veryclean

realclean: clean
	-rmdir .depending
	rm -f core $(plextract) all
	rm -f $(FIRSTMAKEFILE) $(FIRSTMAKEFILE).old makefile.old

veryclean: realclean
	rm -f *~ *.orig

# The following lint has practically everything turned on.  Unfortunately,
# you have to wade through a lot of mumbo jumbo that can't be suppressed.
# If the source file has a /*NOSTRICT*/ somewhere, ignore the lint message
# for that spot.

lint:
	lint $(lintflags) $(defs) $(c) > a2p.fuzz

depend: ../makedepend
	sh ../makedepend MAKE=$(MAKE) ../cflags

clist:
	echo $(c) | tr ' ' $(TRNL) >.clist

hlist:
	echo $(h) | tr ' ' $(TRNL) >.hlist

$(plextract):
	$(RUN) $(PERL) -I../lib $@.PL

find2perl: find2perl.PL ../config.sh

s2p: s2p.PL ../config.sh

# AUTOMATICALLY GENERATED MAKE DEPENDENCIES--PUT NOTHING BELOW THIS LINE
# If this runs make out of memory, delete /usr/include lines.
hash$(OBJ_EXT): ../config.h
hash$(OBJ_EXT): ../handy.h
hash$(OBJ_EXT): /usr/include/alloca.h
hash$(OBJ_EXT): /usr/include/ctype.h
hash$(OBJ_EXT): /usr/include/endian.h
hash$(OBJ_EXT): /usr/include/features.h
hash$(OBJ_EXT): /usr/include/inttypes.h
hash$(OBJ_EXT): /usr/include/math.h
hash$(OBJ_EXT): /usr/include/stdc-predef.h
hash$(OBJ_EXT): /usr/include/stdint.h
hash$(OBJ_EXT): /usr/include/stdio.h
hash$(OBJ_EXT): /usr/include/stdlib.h
hash$(OBJ_EXT): /usr/include/string.h
hash$(OBJ_EXT): /usr/include/strings.h
hash$(OBJ_EXT): /usr/include/time.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/_G_config.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap-16.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/floatn-common.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/floatn.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fp-fast.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fp-logb.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/libio.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/long-double.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/math-vector.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdint-uintn.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sysmacros.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/FILE.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/struct_itimerspec.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/struct_tm.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/time_t.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
hash$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
hash$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/7/include/stdarg.h
hash$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/7/include/stdbool.h
hash$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/7/include/stddef.h
hash$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/7/include/stdint.h
hash$(OBJ_EXT): EXTERN.h
hash$(OBJ_EXT): a2p.h
hash$(OBJ_EXT): hash.c
hash$(OBJ_EXT): hash.h
hash$(OBJ_EXT): str.h
hash$(OBJ_EXT): util.h
hash$(OBJ_EXT): ../cflags
str$(OBJ_EXT): ../config.h
str$(OBJ_EXT): ../handy.h
str$(OBJ_EXT): /usr/include/alloca.h
str$(OBJ_EXT): /usr/include/ctype.h
str$(OBJ_EXT): /usr/include/endian.h
str$(OBJ_EXT): /usr/include/features.h
str$(OBJ_EXT): /usr/include/inttypes.h
str$(OBJ_EXT): /usr/include/math.h
str$(OBJ_EXT): /usr/include/stdc-predef.h
str$(OBJ_EXT): /usr/include/stdint.h
str$(OBJ_EXT): /usr/include/stdio.h
str$(OBJ_EXT): /usr/include/stdlib.h
str$(OBJ_EXT): /usr/include/string.h
str$(OBJ_EXT): /usr/include/strings.h
str$(OBJ_EXT): /usr/include/time.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/_G_config.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap-16.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/floatn-common.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/floatn.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fp-fast.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fp-logb.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/iscanonical.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/libio.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/long-double.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/math-vector.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdint-uintn.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sysmacros.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/FILE.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/struct_itimerspec.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/struct_tm.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/time_t.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
str$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
str$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/7/include/stdarg.h
str$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/7/include/stdbool.h
str$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/7/include/stddef.h
str$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/7/include/stdint.h
str$(OBJ_EXT): EXTERN.h
str$(OBJ_EXT): a2p.h
str$(OBJ_EXT): hash.h
str$(OBJ_EXT): str.c
str$(OBJ_EXT): str.h
str$(OBJ_EXT): util.h
str$(OBJ_EXT): ../cflags
util$(OBJ_EXT): ../config.h
util$(OBJ_EXT): ../handy.h
util$(OBJ_EXT): /usr/include/alloca.h
util$(OBJ_EXT): /usr/include/ctype.h
util$(OBJ_EXT): /usr/include/endian.h
util$(OBJ_EXT): /usr/include/features.h
util$(OBJ_EXT): /usr/include/inttypes.h
util$(OBJ_EXT): /usr/include/math.h
util$(OBJ_EXT): /usr/include/stdc-predef.h
util$(OBJ_EXT): /usr/include/stdint.h
util$(OBJ_EXT): /usr/include/stdio.h
util$(OBJ_EXT): /usr/include/stdlib.h
util$(OBJ_EXT): /usr/include/string.h
util$(OBJ_EXT): /usr/include/strings.h
util$(OBJ_EXT): /usr/include/time.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/_G_config.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap-16.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/floatn-common.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/floatn.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fp-fast.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fp-logb.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/iscanonical.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/libio.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/long-double.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/math-vector.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdint-uintn.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sysmacros.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/FILE.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/struct_itimerspec.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/struct_tm.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/time_t.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
util$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
util$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/7/include/stdarg.h
util$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/7/include/stdbool.h
util$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/7/include/stddef.h
util$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/7/include/stdint.h
util$(OBJ_EXT): EXTERN.h
util$(OBJ_EXT): INTERN.h
util$(OBJ_EXT): a2p.h
util$(OBJ_EXT): hash.h
util$(OBJ_EXT): str.h
util$(OBJ_EXT): util.c
util$(OBJ_EXT): util.h
util$(OBJ_EXT): ../cflags
walk$(OBJ_EXT): ../config.h
walk$(OBJ_EXT): ../handy.h
walk$(OBJ_EXT): /usr/include/alloca.h
walk$(OBJ_EXT): /usr/include/ctype.h
walk$(OBJ_EXT): /usr/include/endian.h
walk$(OBJ_EXT): /usr/include/features.h
walk$(OBJ_EXT): /usr/include/inttypes.h
walk$(OBJ_EXT): /usr/include/math.h
walk$(OBJ_EXT): /usr/include/stdc-predef.h
walk$(OBJ_EXT): /usr/include/stdint.h
walk$(OBJ_EXT): /usr/include/stdio.h
walk$(OBJ_EXT): /usr/include/stdlib.h
walk$(OBJ_EXT): /usr/include/string.h
walk$(OBJ_EXT): /usr/include/strings.h
walk$(OBJ_EXT): /usr/include/time.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/_G_config.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap-16.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/byteswap.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/endian.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/floatn-common.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/floatn.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/flt-eval-method.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fp-fast.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/fp-logb.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/iscanonical.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/libc-header-start.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/libio.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/libm-simd-decl-stubs.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/long-double.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/math-vector.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls-helper-functions.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/mathcalls.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes-arch.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/pthreadtypes.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/select.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdint-intn.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdint-uintn.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdio_lim.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/stdlib-float.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sys_errlist.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/sysmacros.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/thread-shared-types.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/time.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/timex.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/FILE.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/__FILE.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/__locale_t.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/__mbstate_t.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/__sigset_t.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/clock_t.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/clockid_t.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/locale_t.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/sigset_t.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/struct_itimerspec.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/struct_timespec.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/struct_timeval.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/struct_tm.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/time_t.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/types/timer_t.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/typesizes.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/uintn-identity.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitflags.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/waitstatus.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wchar.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/bits/wordsize.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs-64.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/gnu/stubs.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/cdefs.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/select.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/sysmacros.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/time.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/times.h
walk$(OBJ_EXT): /usr/include/x86_64-linux-gnu/sys/types.h
walk$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/7/include/stdarg.h
walk$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/7/include/stdbool.h
walk$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/7/include/stddef.h
walk$(OBJ_EXT): /usr/lib/gcc/x86_64-linux-gnu/7/include/stdint.h
walk$(OBJ_EXT): EXTERN.h
walk$(OBJ_EXT): a2p.h
walk$(OBJ_EXT): hash.h
walk$(OBJ_EXT): str.h
walk$(OBJ_EXT): util.h
walk$(OBJ_EXT): walk.c
walk$(OBJ_EXT): ../cflags
# WARNING: Put nothing here or make depend will gobble it up!
