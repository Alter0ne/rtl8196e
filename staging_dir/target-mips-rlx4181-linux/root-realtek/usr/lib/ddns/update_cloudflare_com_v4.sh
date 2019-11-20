#!/bin/sh
#.Distributed under the terms of the GNU General Public License (GPL) version 2.0
#.based on Ben Kulbertis cloudflare-update-record.sh found at http://gist.github.com/benkulbertis
#.and on George Johnson's cf-ddns.sh found at https://github.com/gstuartj/cf-ddns.sh
#.2016-2018 Christian Schoenebeck <christian dot schoenebeck at gmail dot com>
[ -z "$CURL" ] && [ -z "$CURL_SSL" ] && write_log 14 "Cloudflare communication require cURL with SSL support. Please install"
[ -z "$username" ] && write_log 14 "Service section not configured correctly! Missing key as 'username'"
[ -z "$password" ] && write_log 14 "Service section not configured correctly! Missing secret as 'password'"
[ $use_https -eq 0 ] && use_https=1
local __HOST __DOMAIN __TYPE __URLBASE __PRGBASE __RUNPROG __DATA __IPV6 __ZONEID __RECID __PROXIED
local __URLBASE="https://api.cloudflare.com/client/v4"
local __TTL=120
__HOST=$(printf %s "$domain" | cut -d@ -f1)
__DOMAIN=$(printf %s "$domain" | cut -d@ -f2)
[ -z "$__HOST" ] && __HOST=$__DOMAIN
[ "$__HOST" != "$__DOMAIN" ] && __HOST="${__HOST}.${__DOMAIN}"
[ $use_ipv6 -eq 0 ] && __TYPE="A" || __TYPE="AAAA"
cloudflare_transfer() {
local __CNT=0
local __ERR
while : ; do
write_log 7 "#> $__RUNPROG"
eval "$__RUNPROG"
__ERR=$?
[ $__ERR -eq 0 ] && break
write_log 3 "cURL Error: '$__ERR'"
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
grep -q '"success":true' $DATFILE || {
write_log 4 "CloudFlare reported an error:"
write_log 7 "$(cat $DATFILE)"
return 1
}
}
__PRGBASE="$CURL -RsS -o $DATFILE --stderr $ERRFILE"
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
__PRGBASE="$__PRGBASE --header 'X-Auth-Email: $username' "
__PRGBASE="$__PRGBASE --header 'X-Auth-Key: $password' "
__PRGBASE="$__PRGBASE --header 'Content-Type: application/json' "
__RUNPROG="$__PRGBASE --request GET '$__URLBASE/zones?name=$__DOMAIN'"
cloudflare_transfer || return 1
__ZONEID=$(grep -o '"id":"[^"]*' $DATFILE | grep -o '[^"]*$' | head -1)
[ -z "$__ZONEID" ] && {
write_log 4 "Could not detect 'zone id' for domain.tld: '$__DOMAIN'"
return 127
}
__RUNPROG="$__PRGBASE --request GET '$__URLBASE/zones/$__ZONEID/dns_records?name=$__HOST&type=$__TYPE'"
cloudflare_transfer || return 1
__RECID=$(grep -o '"id":"[^"]*' $DATFILE | grep -o '[^"]*$' | head -1)
[ -z "$__RECID" ] && {
write_log 4 "Could not detect 'record id' for host.domain.tld: '$__HOST'"
return 127
}
__DATA=$(grep -o '"content":"[^"]*' $DATFILE | grep -o '[^"]*$' | head -1)
[ $use_ipv6 -eq 0 ] \
&& __DATA=$(printf "%s" "$__DATA" | grep -m 1 -o "$IPV4_REGEX") \
|| __DATA=$(printf "%s" "$__DATA" | grep -m 1 -o "$IPV6_REGEX")
[ -n "$__DATA" ] && {
if [ $use_ipv6 -eq 1 ]; then
expand_ipv6 $__IP __IPV6
expand_ipv6 $__DATA __DATA
[ "$__DATA" = "$__IPV6" ] && {
write_log 7 "IPv6 at CloudFlare.com already up to date"
return 0
}
else
[ "$__DATA" = "$__IP" ] && {
write_log 7 "IPv4 at CloudFlare.com already up to date"
return 0
}
fi
}
__PROXIED=$(grep -o '"proxied":[^",]*' $DATFILE | grep -o '[^:]*$')
cat > $DATFILE << EOF
{"id":"$__ZONEID","type":"$__TYPE","name":"$__HOST","content":"$__IP","ttl":$__TTL,"proxied":$__PROXIED}
EOF
__RUNPROG="$__PRGBASE --request PUT --data @$DATFILE '$__URLBASE/zones/$__ZONEID/dns_records/$__RECID'"
cloudflare_transfer || return 1
return 0
