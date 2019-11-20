#!/bin/sh

if [ `id -u` -eq 0 ]; then
	# Since we're root, we can reboot directly (presumably).
	reboot || exit 1
else
	# Attempt to reboot using sudo, otherwise fail.
	sudo reboot 2>/dev/null || exit 1
fi
