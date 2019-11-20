#!/bin/sh

####################################################################
####    Author Brandon Kruse <bkruse@digium.com>                ####
####	Copyright 2008, Digium Inc                              ####
####################################################################

# script for generating regtool compatible tab delimited file from a gui config file.
INPUTFILE="/etc/asterisk/g729reginfo.conf"
OUTPUTFILE="/etc/asterisk/g729reginfo_tabs.conf"
rm ${OUTPUTFILE}

TABBEDVALUES=`grep -v '\;' ${INPUTFILE} | sed 's/\[general\]//g' | sed 's/ = /\\t/g'`
echo -e "${TABBEDVALUES}" > ${OUTPUTFILE}

register_tool --register --category 1 --product 16 --key $1  < ${OUTPUTFILE}

STATUS=$?
if [ "${STATUS}" = "0" ]; then
	echo "SUCCESS"
else
	echo "FAILED"
fi
 

