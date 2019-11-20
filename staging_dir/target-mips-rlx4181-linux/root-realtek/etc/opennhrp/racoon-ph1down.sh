#!/bin/sh

# Purge opennhrp entries only if this was the last ISAKMP phase1
if [ -z "`racoonctl -ll show-sa isakmp | grep "$LOCAL_ADDR\.[0-9]* * $REMOTE_ADDR\.[0-9]* "`" ]; then
	opennhrpctl cache purge nbma $REMOTE_ADDR local-nbma $LOCAL_ADDR
fi
