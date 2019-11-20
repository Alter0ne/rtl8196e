#!/bin/sh
#.Distributed under the terms of the GNU General Public License (GPL) version 2.0
#.based on GoDaddy.sh v1.0 by Nazar78 @ TeaNazaR.com
#.2017-2018 Christian Schoenebeck <christian dot schoenebeck at gmail dot com>
[ -z "$CURL" ] && [ -z "$CURL_SSL" ] && write_log 14 "GoDaddy communication require cURL with SSL support. Please install"
[ -z "$username" ] && write_log 14 "Service section not configured correctly! Missing key as 'username'"
[ -z "$password" ] && write_log 14 "Service section not configured correctly! Missing secret as 'password'"
[ $use_https -eq 0 ] && use_https=1
local __HOST __DOMAIN __TYPE __URL __PRGBASE __RUNPROG __DATA __IPV6
__HOST=$(printf %s "$domain" | cut -d@ -f1)
__DOMAIN=$(printf %s "$domain" | cut -d@ -f2)
[ -z "$__HOST" -o "$__HOST" = "$__DOMAIN" ] && __HOST="%40"
[ $use_ipv6 -eq 0 ] && __TYPE="A" || __TYPE="AAAA"
__URL="https://api.godaddy.com/v1/domains/$__DOMAIN/records/$__TYPE/$__HOST"
godaddy_transfer() {
local __CNT=0
local __STATUS __ERR __DEVICE
while : ; do
write_log 7 "#> $__RUNPROG"
__STATUS=$(eval "$__RUNPROG")
__ERR=$?
[ $__ERR -eq 0 ] && break
write_log 4 "cURL error: '$__ERR'"
write_log 7 "$(cat $ERRFILE)"
[ $VERBOSE_MODE -gt 1 ] && {
write_log 4 "Transfer failed - Verbose Mode: $VERBOSE_MODE - NO retry on error"
break
}
__CNT=$(( $__CNT + 1 ))
[ $retry_count -gt 0 -a $__CNT -gt $retry_count ] && \
write_log 14 "Transfer failed after $retry_count retries"
write_log 4 "Transfer failed - retry $__CNT/$retry_count in $RETRY_SECONDS seconds"
sleep $RETRY_SECONDS &
PID_SLEEP=$!
wait $PID_SLEEP
PID_SLEEP=0
done
[ $__STATUS -ne 200 ] && {
write_log 4 "GoDaddy reported an error:"
write_log 7 "$(cat $DATFILE)"
return 1
}
return 0
}
__PRGBASE="$CURL -RsS -w '%{http_code}' -o $DATFILE --stderr $ERRFILE"
if [ -n "$bind_network" ]; then
local __DEVICE
network_get_physdev __DEVICE $bind_network || \
write_log 13 "Can not detect local device using 'network_get_physdev $bind_network' - Error: '$?'"
write_log 7 "Force communication via device '$__DEVICE'"
__PRGBASE="$__PRGBASE --interface $__DEVICE"
fi
if [ $force_ipversion -eq 1 ]; then
[ $use_ipv6 -eq 0 ] && __PRGBASE="$__PRGBASE -4" || __PRGBASE="$__PRGBASE -6"
fi
if [ "$cacert" = "IGNORE" ]; then
__PRGBASE="$__PRGBASE --insecure"
elif [ -f "$cacert" ]; then
__PRGBASE="$__PRGBASE --cacert $cacert"
elif [ -d "$cacert" ]; then
__PRGBASE="$__PRGBASE --capath $cacert"
elif [ -n "$cacert" ]; then
write_log 14 "No valid certificate(s) found at '$cacert' for HTTPS communication"
fi
if [ -z "$proxy" ]; then
__PRGBASE="$__PRGBASE --noproxy '*'"
elif [ -z "$CURL_PROXY" ]; then
write_log 13 "cURL: libcurl compiled without Proxy support"
fi
__PRGBASE="$__PRGBASE --header 'Authorization: sso-key $username:$password' "
__PRGBASE="$__PRGBASE --header 'Accept: application/json' "
__PRGBASE="$__PRGBASE --header 'Content-Type: application/json; charset=utf-8' "
__RUNPROG="$__PRGBASE --request GET $__URL"
godaddy_transfer || return 1
__DATA=$(sed -r 's/.+data":"(.+)","t.+/\1/g' $DATFILE)
[ $use_ipv6 -eq 0 ] \
&& __DATA=$(printf "%s" "$__DATA" | grep -m 1 -o "$IPV4_REGEX") \
|| __DATA=$(printf "%s" "$__DATA" | grep -m 1 -o "$IPV6_REGEX")
[ -n "$__DATA" ] && {
if [ $use_ipv6 -eq 1 ]; then
expand_ipv6 $__IP __IPV6
expand_ipv6 $__DATA __DATA
[ "$__DATA" = "$__IPV6" ] && {
write_log 7 "IPv6 at GoDaddy.com already up to date"
return 0
}
else
[ "$__DATA" = "$__IP" ] && {
write_log 7 "IPv4 at GoDaddy.com already up to date"
return 0
}
fi
}
cat > $DATFILE << EOF
[{"data":"$__IP"}]
EOF
__RUNPROG="$__PRGBASE --request PUT --data @$DATFILE $__URL"
godaddy_transfer || return 1
return 0
