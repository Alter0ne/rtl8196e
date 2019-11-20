#.Distributed under the terms of the GNU General Public License (GPL) version 2.0
#.based on github request #957 by Jan Riechers <de at r-jan dot de>
#.2015 Christian Schoenebeck <christian dot schoenebeck at gmail dot com>
local __TTL=600		#.preset DNS TTL (in seconds)
local __RRTYPE __PW __TCP
local __PROG=$(which nsupdate)
[ -z "$__PROG" ] && __PROG=$(which knsupdate)
[ -z "$__PROG" ]     && write_log 14 "'nsupdate' or 'knsupdate' not installed !"
[ -z "$username" ]   && write_log 14 "Service section not configured correctly! Missing 'username'"
[ -z "$password" ]   && write_log 14 "Service section not configured correctly! Missing 'password'"
[ -z "$dns_server" ] && write_log 14 "Service section not configured correctly! Missing 'dns_server'"
[ $use_ipv6 -ne 0 ] && __RRTYPE="AAAA" || __RRTYPE="A"
[ $force_dnstcp -ne 0 ] && __TCP="-v" || __TCP=""
cat >$DATFILE <<-EOF
server $dns_server
key $username $password
update del $domain $__RRTYPE
update add $domain $__TTL $__RRTYPE $__IP
show
send
answer
quit
EOF
$__PROG -d $__TCP $DATFILE >$ERRFILE 2>&1
write_log 7 "(k)nsupdate reports:${N}$(cat $ERRFILE)"
return 0
