# Shorewall6 Lite Makefile to restart if firewall script is newer than last restart
VARDIR=$(shell /sbin/shorewall6-lite show vardir)
SHAREDIR=/usr/share/shorewall6-lite
RESTOREFILE?=.restore

all: $(VARDIR)/${RESTOREFILE}

$(VARDIR)/${RESTOREFILE}: $(VARDIR)/firewall
	@/sbin/shorewall6-lite -q save >/dev/null; \
	if \
	    /sbin/shorewall6-lite -q restart >/dev/null 2>&1; \
	then \
	    /sbin/shorewall6-lite -q save >/dev/null; \
	else \
	    /sbin/shorewall6-lite -q restart 2>&1 | tail >&2; \
	fi

# EOF
