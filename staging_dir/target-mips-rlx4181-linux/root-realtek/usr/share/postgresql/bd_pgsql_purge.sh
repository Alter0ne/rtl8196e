echo "SET sort_mem TO 45000;"
echo -e "bd_rx_log \n bd_tx_log \n bd_rx_total_log \n bd_tx_total_log" | while read TABLE; 
do
cat << EOF
BEGIN;
insert into $TABLE (sensor_id, ip, timestamp, sample_duration, total, icmp, udp, tcp, ftp, http, p2p) 
select sensor_id, ip, 
date_trunc('day', timestamp) + (case when extract(hour from timestamp) >= 12 then interval '12 hours' 
else interval '0 hours' end) + interval '12 hours',
60*60*12, sum(total), sum(icmp), sum(udp), sum(tcp), sum(ftp), sum(http), sum(p2p)

from $TABLE
where sample_duration < 60*60*12
and timestamp < now() - interval '35 days'
group by sensor_id, ip, 
date_trunc('day', timestamp) + (case when extract(hour from timestamp) >= 12 then interval '12 hours'
else interval '0 hours' end);                                                                                                                             

delete from $TABLE where sample_duration < 60*60*12 and timestamp < now() - interval '35 days';
COMMIT;
BEGIN;
insert into $TABLE (sensor_id, ip, timestamp, sample_duration, total, icmp, udp, tcp, ftp, http, p2p) 
select sensor_id, ip, 
date_trunc('hour', timestamp)+interval '1 hour', 
60*60, sum(total), sum(icmp), sum(udp), sum(tcp), sum(ftp), sum(http), sum(p2p)

from $TABLE
where sample_duration < 60*60
and timestamp < now() - interval '7 days'
group by sensor_id, ip, 
date_trunc('hour', timestamp);

delete from $TABLE where sample_duration < 60*60 and timestamp < now() - interval '7 days';
COMMIT;
BEGIN;
insert into $TABLE (sensor_id, ip, timestamp, sample_duration, total, icmp, udp, tcp, ftp, http, p2p)
select sensor_id, ip, 
date_trunc('hour', timestamp) + (interval '1 minute' * trunc(EXTRACT(MINUTE FROM timestamp)::numeric,-1)) 
+ interval '10 minutes',
10*60, sum(total), sum(icmp), sum(udp), sum(tcp), sum(ftp), sum(http), sum(p2p)
                                                                                                                             
from $TABLE
where sample_duration < 10*60
and timestamp < now() - interval '2 days'
group by sensor_id, ip, 
date_trunc('hour', timestamp) + (interval '1 minute' * trunc(EXTRACT(MINUTE FROM timestamp)::numeric,-1));

delete from $TABLE where sample_duration < 10*60 and timestamp < now() - interval '2 days';
COMMIT;
EOF
done
echo "VACUUM ANALYZE"