BOOT:
  {
#ifdef dTHX
    dTHX;
#endif
    HV *symbol_table = get_hv("Socket::", GV_ADD);

    static const struct iv_s values_for_iv[] =
      {
#ifdef AF_802
        { "AF_802", 6, AF_802 },
#endif
#ifdef AF_AAL
        { "AF_AAL", 6, AF_AAL },
#endif
#ifdef AF_APPLETALK
        { "AF_APPLETALK", 12, AF_APPLETALK },
#endif
#ifdef AF_CCITT
        { "AF_CCITT", 8, AF_CCITT },
#endif
#ifdef AF_CHAOS
        { "AF_CHAOS", 8, AF_CHAOS },
#endif
#ifdef AF_CTF
        { "AF_CTF", 6, AF_CTF },
#endif
#ifdef AF_DATAKIT
        { "AF_DATAKIT", 10, AF_DATAKIT },
#endif
#ifdef AF_DECnet
        { "AF_DECnet", 9, AF_DECnet },
#endif
#ifdef AF_DLI
        { "AF_DLI", 6, AF_DLI },
#endif
#ifdef AF_ECMA
        { "AF_ECMA", 7, AF_ECMA },
#endif
#ifdef AF_GOSIP
        { "AF_GOSIP", 8, AF_GOSIP },
#endif
#ifdef AF_HYLINK
        { "AF_HYLINK", 9, AF_HYLINK },
#endif
#ifdef AF_IMPLINK
        { "AF_IMPLINK", 10, AF_IMPLINK },
#endif
#ifdef AF_INET
        { "AF_INET", 7, AF_INET },
#endif
#ifdef AF_INET6
        { "AF_INET6", 8, AF_INET6 },
#endif
#ifdef AF_ISO
        { "AF_ISO", 6, AF_ISO },
#endif
#ifdef AF_KEY
        { "AF_KEY", 6, AF_KEY },
#endif
#ifdef AF_LAST
        { "AF_LAST", 7, AF_LAST },
#endif
#ifdef AF_LAT
        { "AF_LAT", 6, AF_LAT },
#endif
#ifdef AF_LINK
        { "AF_LINK", 7, AF_LINK },
#endif
#ifdef AF_MAX
        { "AF_MAX", 6, AF_MAX },
#endif
#ifdef AF_NBS
        { "AF_NBS", 6, AF_NBS },
#endif
#ifdef AF_NIT
        { "AF_NIT", 6, AF_NIT },
#endif
#ifdef AF_NS
        { "AF_NS", 5, AF_NS },
#endif
#ifdef AF_OSI
        { "AF_OSI", 6, AF_OSI },
#endif
#ifdef AF_OSINET
        { "AF_OSINET", 9, AF_OSINET },
#endif
#ifdef AF_PUP
        { "AF_PUP", 6, AF_PUP },
#endif
#ifdef AF_ROUTE
        { "AF_ROUTE", 8, AF_ROUTE },
#endif
#ifdef AF_SNA
        { "AF_SNA", 6, AF_SNA },
#endif
#ifdef AF_UNIX
        { "AF_UNIX", 7, AF_UNIX },
#endif
#ifdef AF_UNSPEC
        { "AF_UNSPEC", 9, AF_UNSPEC },
#endif
#ifdef AF_USER
        { "AF_USER", 7, AF_USER },
#endif
#ifdef AF_WAN
        { "AF_WAN", 6, AF_WAN },
#endif
#ifdef AF_X25
        { "AF_X25", 6, AF_X25 },
#endif
#ifdef AI_ADDRCONFIG
        { "AI_ADDRCONFIG", 13, AI_ADDRCONFIG },
#endif
#ifdef AI_ALL
        { "AI_ALL", 6, AI_ALL },
#endif
#ifdef AI_CANONIDN
        { "AI_CANONIDN", 11, AI_CANONIDN },
#endif
#ifdef AI_CANONNAME
        { "AI_CANONNAME", 12, AI_CANONNAME },
#endif
#ifdef AI_IDN
        { "AI_IDN", 6, AI_IDN },
#endif
#ifdef AI_IDN_ALLOW_UNASSIGNED
        { "AI_IDN_ALLOW_UNASSIGNED", 23, AI_IDN_ALLOW_UNASSIGNED },
#endif
#ifdef AI_IDN_USE_STD3_ASCII_RULES
        { "AI_IDN_USE_STD3_ASCII_RULES", 27, AI_IDN_USE_STD3_ASCII_RULES },
#endif
#ifdef AI_NUMERICHOST
        { "AI_NUMERICHOST", 14, AI_NUMERICHOST },
#endif
#ifdef AI_NUMERICSERV
        { "AI_NUMERICSERV", 14, AI_NUMERICSERV },
#endif
#ifdef AI_PASSIVE
        { "AI_PASSIVE", 10, AI_PASSIVE },
#endif
#ifdef AI_V4MAPPED
        { "AI_V4MAPPED", 11, AI_V4MAPPED },
#endif
#ifdef EAI_ADDRFAMILY
        { "EAI_ADDRFAMILY", 14, EAI_ADDRFAMILY },
#endif
#ifdef EAI_AGAIN
        { "EAI_AGAIN", 9, EAI_AGAIN },
#endif
#ifdef EAI_BADFLAGS
        { "EAI_BADFLAGS", 12, EAI_BADFLAGS },
#endif
#ifdef EAI_BADHINTS
        { "EAI_BADHINTS", 12, EAI_BADHINTS },
#endif
#ifdef EAI_FAIL
        { "EAI_FAIL", 8, EAI_FAIL },
#endif
#ifdef EAI_FAMILY
        { "EAI_FAMILY", 10, EAI_FAMILY },
#endif
#ifdef EAI_NODATA
        { "EAI_NODATA", 10, EAI_NODATA },
#endif
#ifdef EAI_NONAME
        { "EAI_NONAME", 10, EAI_NONAME },
#endif
#ifdef EAI_PROTOCOL
        { "EAI_PROTOCOL", 12, EAI_PROTOCOL },
#endif
#ifdef EAI_SERVICE
        { "EAI_SERVICE", 11, EAI_SERVICE },
#endif
#ifdef EAI_SOCKTYPE
        { "EAI_SOCKTYPE", 12, EAI_SOCKTYPE },
#endif
#ifdef EAI_SYSTEM
        { "EAI_SYSTEM", 10, EAI_SYSTEM },
#endif
#ifdef IOV_MAX
        { "IOV_MAX", 7, IOV_MAX },
#endif
#ifdef IP_ADD_MEMBERSHIP
        { "IP_ADD_MEMBERSHIP", 17, IP_ADD_MEMBERSHIP },
#endif
#ifdef IP_ADD_SOURCE_MEMBERSHIP
        { "IP_ADD_SOURCE_MEMBERSHIP", 24, IP_ADD_SOURCE_MEMBERSHIP },
#endif
#ifdef IP_DROP_MEMBERSHIP
        { "IP_DROP_MEMBERSHIP", 18, IP_DROP_MEMBERSHIP },
#endif
#ifdef IP_DROP_SOURCE_MEMBERSHIP
        { "IP_DROP_SOURCE_MEMBERSHIP", 25, IP_DROP_SOURCE_MEMBERSHIP },
#endif
#ifdef IP_HDRINCL
        { "IP_HDRINCL", 10, IP_HDRINCL },
#endif
#ifdef IP_MULTICAST_IF
        { "IP_MULTICAST_IF", 15, IP_MULTICAST_IF },
#endif
#ifdef IP_MULTICAST_LOOP
        { "IP_MULTICAST_LOOP", 17, IP_MULTICAST_LOOP },
#endif
#ifdef IP_MULTICAST_TTL
        { "IP_MULTICAST_TTL", 16, IP_MULTICAST_TTL },
#endif
#ifdef IP_OPTIONS
        { "IP_OPTIONS", 10, IP_OPTIONS },
#endif
#ifdef IP_RECVOPTS
        { "IP_RECVOPTS", 11, IP_RECVOPTS },
#endif
#ifdef IP_RECVRETOPTS
        { "IP_RECVRETOPTS", 14, IP_RECVRETOPTS },
#endif
#ifdef IP_RETOPTS
        { "IP_RETOPTS", 10, IP_RETOPTS },
#endif
#ifdef IP_TOS
        { "IP_TOS", 6, IP_TOS },
#endif
#ifdef IP_TTL
        { "IP_TTL", 6, IP_TTL },
#endif
#ifdef IPTOS_LOWDELAY
        { "IPTOS_LOWDELAY", 14, IPTOS_LOWDELAY },
#endif
#ifdef IPTOS_THROUGHPUT
        { "IPTOS_THROUGHPUT", 16, IPTOS_THROUGHPUT },
#endif
#ifdef IPTOS_RELIABILITY
        { "IPTOS_RELIABILITY", 17, IPTOS_RELIABILITY },
#endif
#ifdef IPTOS_MINCOST
        { "IPTOS_MINCOST", 13, IPTOS_MINCOST },
#endif
#ifdef IPV6_ADD_MEMBERSHIP
        { "IPV6_ADD_MEMBERSHIP", 19, IPV6_ADD_MEMBERSHIP },
#endif
#ifdef IPV6_DROP_MEMBERSHIP
        { "IPV6_DROP_MEMBERSHIP", 20, IPV6_DROP_MEMBERSHIP },
#endif
#ifdef IPV6_JOIN_GROUP
        { "IPV6_JOIN_GROUP", 15, IPV6_JOIN_GROUP },
#endif
#ifdef IPV6_LEAVE_GROUP
        { "IPV6_LEAVE_GROUP", 16, IPV6_LEAVE_GROUP },
#endif
#ifdef IPV6_MTU
        { "IPV6_MTU", 8, IPV6_MTU },
#endif
#ifdef IPV6_MTU_DISCOVER
        { "IPV6_MTU_DISCOVER", 17, IPV6_MTU_DISCOVER },
#endif
#ifdef IPV6_MULTICAST_HOPS
        { "IPV6_MULTICAST_HOPS", 19, IPV6_MULTICAST_HOPS },
#endif
#ifdef IPV6_MULTICAST_IF
        { "IPV6_MULTICAST_IF", 17, IPV6_MULTICAST_IF },
#endif
#ifdef IPV6_MULTICAST_LOOP
        { "IPV6_MULTICAST_LOOP", 19, IPV6_MULTICAST_LOOP },
#endif
#ifdef IPV6_UNICAST_HOPS
        { "IPV6_UNICAST_HOPS", 17, IPV6_UNICAST_HOPS },
#endif
#ifdef IPV6_V6ONLY
        { "IPV6_V6ONLY", 11, IPV6_V6ONLY },
#endif
#ifdef MSG_BCAST
        { "MSG_BCAST", 9, MSG_BCAST },
#endif
#ifdef MSG_BTAG
        { "MSG_BTAG", 8, MSG_BTAG },
#endif
#ifdef MSG_CTLFLAGS
        { "MSG_CTLFLAGS", 12, MSG_CTLFLAGS },
#endif
#ifdef MSG_CTLIGNORE
        { "MSG_CTLIGNORE", 13, MSG_CTLIGNORE },
#endif
#ifdef MSG_DONTWAIT
        { "MSG_DONTWAIT", 12, MSG_DONTWAIT },
#endif
#ifdef MSG_EOF
        { "MSG_EOF", 7, MSG_EOF },
#endif
#ifdef MSG_EOR
        { "MSG_EOR", 7, MSG_EOR },
#endif
#ifdef MSG_ERRQUEUE
        { "MSG_ERRQUEUE", 12, MSG_ERRQUEUE },
#endif
#ifdef MSG_ETAG
        { "MSG_ETAG", 8, MSG_ETAG },
#endif
#ifdef MSG_FIN
        { "MSG_FIN", 7, MSG_FIN },
#endif
#ifdef MSG_MAXIOVLEN
        { "MSG_MAXIOVLEN", 13, MSG_MAXIOVLEN },
#endif
#ifdef MSG_MCAST
        { "MSG_MCAST", 9, MSG_MCAST },
#endif
#ifdef MSG_NOSIGNAL
        { "MSG_NOSIGNAL", 12, MSG_NOSIGNAL },
#endif
#ifdef MSG_RST
        { "MSG_RST", 7, MSG_RST },
#endif
#ifdef MSG_SYN
        { "MSG_SYN", 7, MSG_SYN },
#endif
#ifdef MSG_TRUNC
        { "MSG_TRUNC", 9, MSG_TRUNC },
#endif
#ifdef MSG_URG
        { "MSG_URG", 7, MSG_URG },
#endif
#ifdef MSG_WAITALL
        { "MSG_WAITALL", 11, MSG_WAITALL },
#endif
#ifdef MSG_WIRE
        { "MSG_WIRE", 8, MSG_WIRE },
#endif
#ifdef NI_DGRAM
        { "NI_DGRAM", 8, NI_DGRAM },
#endif
#ifdef NI_IDN
        { "NI_IDN", 6, NI_IDN },
#endif
#ifdef NI_IDN_ALLOW_UNASSIGNED
        { "NI_IDN_ALLOW_UNASSIGNED", 23, NI_IDN_ALLOW_UNASSIGNED },
#endif
#ifdef NI_IDN_USE_STD3_ASCII_RULES
        { "NI_IDN_USE_STD3_ASCII_RULES", 27, NI_IDN_USE_STD3_ASCII_RULES },
#endif
#ifdef NI_NAMEREQD
        { "NI_NAMEREQD", 11, NI_NAMEREQD },
#endif
#ifdef NI_NOFQDN
        { "NI_NOFQDN", 9, NI_NOFQDN },
#endif
#ifdef NI_NUMERICHOST
        { "NI_NUMERICHOST", 14, NI_NUMERICHOST },
#endif
#ifdef NI_NUMERICSERV
        { "NI_NUMERICSERV", 14, NI_NUMERICSERV },
#endif
#ifdef PF_802
        { "PF_802", 6, PF_802 },
#endif
#ifdef PF_AAL
        { "PF_AAL", 6, PF_AAL },
#endif
#ifdef PF_APPLETALK
        { "PF_APPLETALK", 12, PF_APPLETALK },
#endif
#ifdef PF_CCITT
        { "PF_CCITT", 8, PF_CCITT },
#endif
#ifdef PF_CHAOS
        { "PF_CHAOS", 8, PF_CHAOS },
#endif
#ifdef PF_CTF
        { "PF_CTF", 6, PF_CTF },
#endif
#ifdef PF_DATAKIT
        { "PF_DATAKIT", 10, PF_DATAKIT },
#endif
#ifdef PF_DECnet
        { "PF_DECnet", 9, PF_DECnet },
#endif
#ifdef PF_DLI
        { "PF_DLI", 6, PF_DLI },
#endif
#ifdef PF_ECMA
        { "PF_ECMA", 7, PF_ECMA },
#endif
#ifdef PF_GOSIP
        { "PF_GOSIP", 8, PF_GOSIP },
#endif
#ifdef PF_HYLINK
        { "PF_HYLINK", 9, PF_HYLINK },
#endif
#ifdef PF_IMPLINK
        { "PF_IMPLINK", 10, PF_IMPLINK },
#endif
#ifdef PF_INET
        { "PF_INET", 7, PF_INET },
#endif
#ifdef PF_INET6
        { "PF_INET6", 8, PF_INET6 },
#endif
#ifdef PF_ISO
        { "PF_ISO", 6, PF_ISO },
#endif
#ifdef PF_KEY
        { "PF_KEY", 6, PF_KEY },
#endif
#ifdef PF_LAST
        { "PF_LAST", 7, PF_LAST },
#endif
#ifdef PF_LAT
        { "PF_LAT", 6, PF_LAT },
#endif
#ifdef PF_LINK
        { "PF_LINK", 7, PF_LINK },
#endif
#ifdef PF_MAX
        { "PF_MAX", 6, PF_MAX },
#endif
#ifdef PF_NBS
        { "PF_NBS", 6, PF_NBS },
#endif
#ifdef PF_NIT
        { "PF_NIT", 6, PF_NIT },
#endif
#ifdef PF_NS
        { "PF_NS", 5, PF_NS },
#endif
#ifdef PF_OSI
        { "PF_OSI", 6, PF_OSI },
#endif
#ifdef PF_OSINET
        { "PF_OSINET", 9, PF_OSINET },
#endif
#ifdef PF_PUP
        { "PF_PUP", 6, PF_PUP },
#endif
#ifdef PF_ROUTE
        { "PF_ROUTE", 8, PF_ROUTE },
#endif
#ifdef PF_SNA
        { "PF_SNA", 6, PF_SNA },
#endif
#ifdef PF_UNIX
        { "PF_UNIX", 7, PF_UNIX },
#endif
#ifdef PF_UNSPEC
        { "PF_UNSPEC", 9, PF_UNSPEC },
#endif
#ifdef PF_USER
        { "PF_USER", 7, PF_USER },
#endif
#ifdef PF_WAN
        { "PF_WAN", 6, PF_WAN },
#endif
#ifdef PF_X25
        { "PF_X25", 6, PF_X25 },
#endif
#ifdef SCM_CONNECT
        { "SCM_CONNECT", 11, SCM_CONNECT },
#endif
#ifdef SCM_CREDENTIALS
        { "SCM_CREDENTIALS", 15, SCM_CREDENTIALS },
#endif
#ifdef SCM_CREDS
        { "SCM_CREDS", 9, SCM_CREDS },
#endif
#ifdef SCM_TIMESTAMP
        { "SCM_TIMESTAMP", 13, SCM_TIMESTAMP },
#endif
#ifdef SOCK_DGRAM
        { "SOCK_DGRAM", 10, SOCK_DGRAM },
#endif
#ifdef SOCK_RAW
        { "SOCK_RAW", 8, SOCK_RAW },
#endif
#ifdef SOCK_RDM
        { "SOCK_RDM", 8, SOCK_RDM },
#endif
#ifdef SOCK_SEQPACKET
        { "SOCK_SEQPACKET", 14, SOCK_SEQPACKET },
#endif
#ifdef SOCK_STREAM
        { "SOCK_STREAM", 11, SOCK_STREAM },
#endif
#ifdef SOCK_NONBLOCK
        { "SOCK_NONBLOCK", 13, SOCK_NONBLOCK },
#endif
#ifdef SOCK_CLOEXEC
        { "SOCK_CLOEXEC", 12, SOCK_CLOEXEC },
#endif
#ifdef SOL_SOCKET
        { "SOL_SOCKET", 10, SOL_SOCKET },
#endif
#ifdef SOMAXCONN
        { "SOMAXCONN", 9, SOMAXCONN },
#endif
#ifdef SO_ACCEPTCONN
        { "SO_ACCEPTCONN", 13, SO_ACCEPTCONN },
#endif
#ifdef SO_ATTACH_FILTER
        { "SO_ATTACH_FILTER", 16, SO_ATTACH_FILTER },
#endif
#ifdef SO_BACKLOG
        { "SO_BACKLOG", 10, SO_BACKLOG },
#endif
#ifdef SO_BROADCAST
        { "SO_BROADCAST", 12, SO_BROADCAST },
#endif
#ifdef SO_CHAMELEON
        { "SO_CHAMELEON", 12, SO_CHAMELEON },
#endif
#ifdef SO_DEBUG
        { "SO_DEBUG", 8, SO_DEBUG },
#endif
#ifdef SO_DETACH_FILTER
        { "SO_DETACH_FILTER", 16, SO_DETACH_FILTER },
#endif
#ifdef SO_DGRAM_ERRIND
        { "SO_DGRAM_ERRIND", 15, SO_DGRAM_ERRIND },
#endif
#ifdef SO_DOMAIN
        { "SO_DOMAIN", 9, SO_DOMAIN },
#endif
#ifdef SO_DONTLINGER
        { "SO_DONTLINGER", 13, SO_DONTLINGER },
#endif
#ifdef SO_DONTROUTE
        { "SO_DONTROUTE", 12, SO_DONTROUTE },
#endif
#ifdef SO_ERROR
        { "SO_ERROR", 8, SO_ERROR },
#endif
#ifdef SO_FAMILY
        { "SO_FAMILY", 9, SO_FAMILY },
#endif
#ifdef SO_KEEPALIVE
        { "SO_KEEPALIVE", 12, SO_KEEPALIVE },
#endif
#ifdef SO_LINGER
        { "SO_LINGER", 9, SO_LINGER },
#endif
#ifdef SO_OOBINLINE
        { "SO_OOBINLINE", 12, SO_OOBINLINE },
#endif
#ifdef SO_PASSCRED
        { "SO_PASSCRED", 11, SO_PASSCRED },
#endif
#ifdef SO_PASSIFNAME
        { "SO_PASSIFNAME", 13, SO_PASSIFNAME },
#endif
#ifdef SO_PEERCRED
        { "SO_PEERCRED", 11, SO_PEERCRED },
#endif
#ifdef SO_PROTOCOL
        { "SO_PROTOCOL", 11, SO_PROTOCOL },
#endif
#ifdef SO_PROTOTYPE
        { "SO_PROTOTYPE", 12, SO_PROTOTYPE },
#endif
#ifdef SO_RCVBUF
        { "SO_RCVBUF", 9, SO_RCVBUF },
#endif
#ifdef SO_RCVLOWAT
        { "SO_RCVLOWAT", 11, SO_RCVLOWAT },
#endif
#ifdef SO_RCVTIMEO
        { "SO_RCVTIMEO", 11, SO_RCVTIMEO },
#endif
#ifdef SO_REUSEADDR
        { "SO_REUSEADDR", 12, SO_REUSEADDR },
#endif
#ifdef SO_REUSEPORT
        { "SO_REUSEPORT", 12, SO_REUSEPORT },
#endif
#ifdef SO_SECURITY_AUTHENTICATION
        { "SO_SECURITY_AUTHENTICATION", 26, SO_SECURITY_AUTHENTICATION },
#endif
#ifdef SO_SECURITY_ENCRYPTION_NETWORK
        { "SO_SECURITY_ENCRYPTION_NETWORK", 30, SO_SECURITY_ENCRYPTION_NETWORK },
#endif
#ifdef SO_SECURITY_ENCRYPTION_TRANSPORT
        { "SO_SECURITY_ENCRYPTION_TRANSPORT", 32, SO_SECURITY_ENCRYPTION_TRANSPORT },
#endif
#ifdef SO_SNDBUF
        { "SO_SNDBUF", 9, SO_SNDBUF },
#endif
#ifdef SO_SNDLOWAT
        { "SO_SNDLOWAT", 11, SO_SNDLOWAT },
#endif
#ifdef SO_SNDTIMEO
        { "SO_SNDTIMEO", 11, SO_SNDTIMEO },
#endif
#ifdef SO_STATE
        { "SO_STATE", 8, SO_STATE },
#endif
#ifdef SO_TYPE
        { "SO_TYPE", 7, SO_TYPE },
#endif
#ifdef SO_USELOOPBACK
        { "SO_USELOOPBACK", 14, SO_USELOOPBACK },
#endif
#ifdef SO_XOPEN
        { "SO_XOPEN", 8, SO_XOPEN },
#endif
#ifdef SO_XSE
        { "SO_XSE", 6, SO_XSE },
#endif
#ifdef TCP_CONGESTION
        { "TCP_CONGESTION", 14, TCP_CONGESTION },
#endif
#ifdef TCP_CONNECTIONTIMEOUT
        { "TCP_CONNECTIONTIMEOUT", 21, TCP_CONNECTIONTIMEOUT },
#endif
#ifdef TCP_CORK
        { "TCP_CORK", 8, TCP_CORK },
#endif
#ifdef TCP_DEFER_ACCEPT
        { "TCP_DEFER_ACCEPT", 16, TCP_DEFER_ACCEPT },
#endif
#ifdef TCP_INFO
        { "TCP_INFO", 8, TCP_INFO },
#endif
#ifdef TCP_INIT_CWND
        { "TCP_INIT_CWND", 13, TCP_INIT_CWND },
#endif
#ifdef TCP_KEEPALIVE
        { "TCP_KEEPALIVE", 13, TCP_KEEPALIVE },
#endif
#ifdef TCP_KEEPCNT
        { "TCP_KEEPCNT", 11, TCP_KEEPCNT },
#endif
#ifdef TCP_KEEPIDLE
        { "TCP_KEEPIDLE", 12, TCP_KEEPIDLE },
#endif
#ifdef TCP_KEEPINTVL
        { "TCP_KEEPINTVL", 13, TCP_KEEPINTVL },
#endif
#ifdef TCP_LINGER2
        { "TCP_LINGER2", 11, TCP_LINGER2 },
#endif
#ifdef TCP_MAXRT
        { "TCP_MAXRT", 9, TCP_MAXRT },
#endif
#ifdef TCP_MAXSEG
        { "TCP_MAXSEG", 10, TCP_MAXSEG },
#endif
#ifdef TCP_MD5SIG
        { "TCP_MD5SIG", 10, TCP_MD5SIG },
#endif
#ifdef TCP_NODELAY
        { "TCP_NODELAY", 11, TCP_NODELAY },
#endif
#ifdef TCP_NOOPT
        { "TCP_NOOPT", 9, TCP_NOOPT },
#endif
#ifdef TCP_NOPUSH
        { "TCP_NOPUSH", 10, TCP_NOPUSH },
#endif
#ifdef TCP_QUICKACK
        { "TCP_QUICKACK", 12, TCP_QUICKACK },
#endif
#ifdef TCP_SACK_ENABLE
        { "TCP_SACK_ENABLE", 15, TCP_SACK_ENABLE },
#endif
#ifdef TCP_STDURG
        { "TCP_STDURG", 10, TCP_STDURG },
#endif
#ifdef TCP_SYNCNT
        { "TCP_SYNCNT", 10, TCP_SYNCNT },
#endif
#ifdef TCP_WINDOW_CLAMP
        { "TCP_WINDOW_CLAMP", 16, TCP_WINDOW_CLAMP },
#endif
#ifdef UIO_MAXIOV
        { "UIO_MAXIOV", 10, UIO_MAXIOV },
#endif
#ifdef IPPROTO_IP
        { "IPPROTO_IP", 10, IPPROTO_IP },
#endif
#ifdef IPPROTO_IPV6
        { "IPPROTO_IPV6", 12, IPPROTO_IPV6 },
#endif
#ifdef IPPROTO_RAW
        { "IPPROTO_RAW", 11, IPPROTO_RAW },
#endif
#ifdef IPPROTO_ICMP
        { "IPPROTO_ICMP", 12, IPPROTO_ICMP },
#endif
#ifdef IPPROTO_TCP
        { "IPPROTO_TCP", 11, IPPROTO_TCP },
#endif
#ifdef IPPROTO_UDP
        { "IPPROTO_UDP", 11, IPPROTO_UDP },
#endif
#ifdef SHUT_RD
        { "SHUT_RD", 7, SHUT_RD },
#endif
#ifdef SHUT_WR
        { "SHUT_WR", 7, SHUT_WR },
#endif
#ifdef SHUT_RDWR
        { "SHUT_RDWR", 9, SHUT_RDWR },
#endif
#if defined(MSG_CTRUNC) || defined(HAS_MSG_CTRUNC) /* might be an enum */
        { "MSG_CTRUNC", 10, MSG_CTRUNC },
#endif
#if defined(MSG_DONTROUTE) || defined(HAS_MSG_DONTROUTE) /* might be an enum */
        { "MSG_DONTROUTE", 13, MSG_DONTROUTE },
#endif
#if defined(MSG_OOB) || defined(HAS_MSG_OOB) /* might be an enum */
        { "MSG_OOB", 7, MSG_OOB },
#endif
#if defined(MSG_PEEK) || defined(HAS_MSG_PEEK) /* might be an enum */
        { "MSG_PEEK", 8, MSG_PEEK },
#endif
#if defined(MSG_PROXY) || defined(HAS_MSG_PROXY) /* might be an enum */
        { "MSG_PROXY", 9, MSG_PROXY },
#endif
#if defined(SCM_RIGHTS) || defined(HAS_SCM_RIGHTS) /* might be an enum */
        { "SCM_RIGHTS", 10, SCM_RIGHTS },
#endif
#ifndef IPPROTO_IP
        /* This is the default value: */
        { "IPPROTO_IP", 10, 0 },
#endif
#ifndef IPPROTO_IPV6
        /* This is the default value: */
        { "IPPROTO_IPV6", 12, 41 },
#endif
#ifndef IPPROTO_RAW
        /* This is the default value: */
        { "IPPROTO_RAW", 11, 255 },
#endif
#ifndef IPPROTO_ICMP
        /* This is the default value: */
        { "IPPROTO_ICMP", 12, 1 },
#endif
#ifndef IPPROTO_TCP
        /* This is the default value: */
        { "IPPROTO_TCP", 11, 6 },
#endif
#ifndef IPPROTO_UDP
        /* This is the default value: */
        { "IPPROTO_UDP", 11, 17 },
#endif
#ifndef SHUT_RD
        /* This is the default value: */
        { "SHUT_RD", 7, 0 },
#endif
#ifndef SHUT_WR
        /* This is the default value: */
        { "SHUT_WR", 7, 1 },
#endif
#ifndef SHUT_RDWR
        /* This is the default value: */
        { "SHUT_RDWR", 9, 2 },
#endif
        { NULL, 0, 0 } };
	const struct iv_s *value_for_iv = values_for_iv;
        while (value_for_iv->name) {
	    constant_add_symbol(aTHX_  symbol_table, value_for_iv->name,
				value_for_iv->namelen, newSViv(value_for_iv->value));
            ++value_for_iv;
	}
	if (C_ARRAY_LENGTH(values_for_notfound) > 1) {
#ifndef SYMBIAN
	    HV *const constant_missing = get_missing_hash(aTHX);
#endif
	    const struct notfound_s *value_for_notfound = values_for_notfound;
	    do {

		/* Need to add prototypes, else parsing will vary by platform.  */
		HE *he = (HE*) hv_common_key_len(symbol_table,
						 value_for_notfound->name,
						 value_for_notfound->namelen,
						 HV_FETCH_LVALUE, NULL, 0);
		SV *sv;
#ifndef SYMBIAN
		HEK *hek;
#endif
		if (!he) {
		    Perl_croak(aTHX_ 
			       "Couldn't add key '%s' to %%Socket::",
			       value_for_notfound->name);
		}
		sv = HeVAL(he);
		if (!SvOK(sv) && SvTYPE(sv) != SVt_PVGV) {
		    /* Nothing was here before, so mark a prototype of ""  */
		    sv_setpvn(sv, "", 0);
		} else if (SvPOK(sv) && SvCUR(sv) == 0) {
		    /* There is already a prototype of "" - do nothing  */
		} else {
		    /* Someone has been here before us - have to make a real
		       typeglob.  */
		    /* It turns out to be incredibly hard to deal with all the
		       corner cases of sub foo (); and reporting errors correctly,
		       so lets cheat a bit.  Start with a constant subroutine  */
		    CV *cv = newCONSTSUB(symbol_table,
					 value_for_notfound->name,
					 &PL_sv_yes);
		    /* and then turn it into a non constant declaration only.  */
		    SvREFCNT_dec(CvXSUBANY(cv).any_ptr);
		    CvCONST_off(cv);
		    CvXSUB(cv) = NULL;
		    CvXSUBANY(cv).any_ptr = NULL;
		}
#ifndef SYMBIAN
		hek = HeKEY_hek(he);
		if (!hv_common(constant_missing, NULL, HEK_KEY(hek),
 			       HEK_LEN(hek), HEK_FLAGS(hek), HV_FETCH_ISSTORE,
			       &PL_sv_yes, HEK_HASH(hek)))
		    Perl_croak(aTHX_  "Couldn't add key '%s' to missing_hash",
			       value_for_notfound->name);
#endif
	    } while ((++value_for_notfound)->name);
	}
#ifdef INADDR_ANY
        {
            SV * temp0;
            struct in_addr ip_address; ip_address.s_addr = htonl(INADDR_ANY);
            temp0 = newSVpvn_flags((char *)&ip_address,sizeof(ip_address), SVs_TEMP);
	    constant_add_symbol(aTHX_  symbol_table, "INADDR_ANY",
				    10, SvREFCNT_inc(temp0));
        }
#endif
#ifdef INADDR_LOOPBACK
        {
            SV * temp0;
            struct in_addr ip_address; ip_address.s_addr = htonl(INADDR_LOOPBACK);
            temp0 = newSVpvn_flags((char *)&ip_address,sizeof(ip_address), SVs_TEMP);
	    constant_add_symbol(aTHX_  symbol_table, "INADDR_LOOPBACK",
				    15, SvREFCNT_inc(temp0));
        }
#endif
#ifdef INADDR_NONE
        {
            SV * temp0;
            struct in_addr ip_address; ip_address.s_addr = htonl(INADDR_NONE);
            temp0 = newSVpvn_flags((char *)&ip_address,sizeof(ip_address), SVs_TEMP);
	    constant_add_symbol(aTHX_  symbol_table, "INADDR_NONE",
				    11, SvREFCNT_inc(temp0));
        }
#endif
#ifdef INADDR_BROADCAST
        {
            SV * temp0;
            struct in_addr ip_address; ip_address.s_addr = htonl(INADDR_BROADCAST);
            temp0 = newSVpvn_flags((char *)&ip_address,sizeof(ip_address), SVs_TEMP);
	    constant_add_symbol(aTHX_  symbol_table, "INADDR_BROADCAST",
				    16, SvREFCNT_inc(temp0));
        }
#endif
#ifdef IN6ADDR_ANY_INIT
        {
            SV * temp0;
            struct in6_addr ip6_address = IN6ADDR_ANY_INIT;
            temp0 = newSVpvn_flags((char *)&ip6_address,sizeof(ip6_address), SVs_TEMP);
	    constant_add_symbol(aTHX_  symbol_table, "IN6ADDR_ANY",
				    11, SvREFCNT_inc(temp0));
        }
#endif
#ifdef IN6ADDR_LOOPBACK_INIT
        {
            SV * temp0;
            struct in6_addr ip6_address = IN6ADDR_LOOPBACK_INIT;
            temp0 = newSVpvn_flags((char *)&ip6_address,sizeof(ip6_address), SVs_TEMP);
	    constant_add_symbol(aTHX_  symbol_table, "IN6ADDR_LOOPBACK",
				    16, SvREFCNT_inc(temp0));
        }
#endif
    /* As we've been creating subroutines, we better invalidate any cached
       methods  */
    mro_method_changed_in(symbol_table);
  }

void
AUTOLOAD()
    PROTOTYPE: DISABLE
    PREINIT:
	SV *sv = newSVpvn_flags(SvPVX(cv), SvCUR(cv), SVs_TEMP | SvUTF8(cv));
	const COP *cop = PL_curcop;
    PPCODE:
#ifndef SYMBIAN
	/* It's not obvious how to calculate this at C pre-processor time.
	   However, any compiler optimiser worth its salt should be able to
	   remove the dead code, and hopefully the now-obviously-unused static
	   function too.  */
	HV *constant_missing = (C_ARRAY_LENGTH(values_for_notfound) > 1)
	    ? get_missing_hash(aTHX) : NULL;
	if ((C_ARRAY_LENGTH(values_for_notfound) > 1)
	    ? hv_exists_ent(constant_missing, sv, 0) : 0) {
	    sv = newSVpvf("Your vendor has not defined Socket macro %" SVf
			  ", used at %" COP_FILE_F " line %d\n", sv,
			  COP_FILE(cop), CopLINE(cop));
	} else
#endif
	{
	    sv = newSVpvf("%"SVf" is not a valid Socket macro at %"
			  COP_FILE_F " line %d\n", sv, COP_FILE(cop), CopLINE(cop));
	}
	croak_sv(sv_2mortal(sv));
