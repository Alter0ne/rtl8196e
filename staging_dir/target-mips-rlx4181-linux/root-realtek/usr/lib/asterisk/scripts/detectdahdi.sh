#!/bin/sh

# script to detect whether zaptel/dahdi was installed & generate an include file for GUI

OUTPUTFILE="/etc/asterisk/dahdi_guiread.conf"

echo "" > ${OUTPUTFILE}
echo "[general]" >> ${OUTPUTFILE}
echo "" >> ${OUTPUTFILE}

if [ -e /dev/zap ]; then
	echo "zaptel"
	echo "#include \"../zaptel.conf\"" >> ${OUTPUTFILE}
	echo "" >> ${OUTPUTFILE}
	exit 0
fi

if [ -e /dev/dahdi ]; then
        echo "dahdi"
	echo "#include \"../dahdi/system.conf\"" >> ${OUTPUTFILE}
	echo "" >> ${OUTPUTFILE}
	exit 0
fi

echo "None"
exit 0
