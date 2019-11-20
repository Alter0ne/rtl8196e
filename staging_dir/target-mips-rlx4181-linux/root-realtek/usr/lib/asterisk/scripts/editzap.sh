#!/bin/bash

####################################################################
####	Author Brandon Kruse <bkruse@digium.com> 		####
####	Copyright (c) 2007-2008 Digium				####
####################################################################

# Quick script for applying zaptel settings from the GUI.

if [ -e /dev/zap ]; then
	ZAPCONF="/etc/zaptel.conf"
fi

if [ -e /dev/dahdi ]; then
	ZAPCONF="/etc/dahdi/system.conf"
fi

FILENAME="/etc/asterisk/applyzap.conf"
grep -v '\;' ${FILENAME} | sed 's/\[general\]//g' > ${ZAPCONF}

