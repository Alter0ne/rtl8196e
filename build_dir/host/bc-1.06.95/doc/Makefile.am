## Process this file with automake to produce Makefile.in

info_TEXINFOS = bc.texi dc.texi
MAKEINFO = makeinfo --no-split

MAINTAINERCLEANFILES = Makefile.in

dist_man_MANS = bc.1 dc.1

bc.dvi bc.pdf bc.html $(srcdir)/bc.info \
dc.dvi dc.pdf dc.html $(srcdir)/dc.info :  texi-ver.incl
