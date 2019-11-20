#!/bin/bash

####################################################################
####	Author Brandon Kruse <bkruse@digium.com>		####
####	Copyright Digium					####
####################################################################

# Quick script for applying misdn settings from the GUI.

MISDNCONF="/etc/misdn-init.conf"
MISDNFILE="/etc/asterisk/applymisdn.conf"
grep -v "\[general\]" ${MISDNFILE} | grep -v "\;" | sed 's/ = /=/g' > ${MISDNCONF}
