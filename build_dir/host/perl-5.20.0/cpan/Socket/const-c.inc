#define PERL_constant_NOTFOUND	1
#define PERL_constant_NOTDEF	2
#define PERL_constant_ISIV	3
#define PERL_constant_ISNO	4
#define PERL_constant_ISNV	5
#define PERL_constant_ISPV	6
#define PERL_constant_ISPVN	7
#define PERL_constant_ISSV	8
#define PERL_constant_ISUNDEF	9
#define PERL_constant_ISUV	10
#define PERL_constant_ISYES	11

#ifndef NVTYPE
typedef double NV; /* 5.6 and later define NVTYPE, and typedef NV to it.  */
#endif
#ifndef aTHX_
#define aTHX_ /* 5.6 or later define this for threading support.  */
#endif
#ifndef pTHX_
#define pTHX_ /* 5.6 or later define this for threading support.  */
#endif

/* This allows slightly more efficient code on !USE_ITHREADS: */
#ifdef USE_ITHREADS
#  define COP_FILE(c)	CopFILE(c)
#  define COP_FILE_F	"s"
#else
#  define COP_FILE(c)	CopFILESV(c)
#  define COP_FILE_F	SVf
#endif

static void
constant_add_symbol(pTHX_  HV *hash, const char *name, I32 namelen, SV *value) {
    HE *he = (HE*) hv_common_key_len(hash, name, namelen, HV_FETCH_LVALUE, NULL,
				     0);
    SV *sv;

    if (!he) {
        Perl_croak(aTHX_  "Couldn't add key '%s' to %%Socket::",
		   name);
    }
    sv = HeVAL(he);
    if (SvOK(sv) || SvTYPE(sv) == SVt_PVGV) {
	/* Someone has been here before us - have to make a real sub.  */
	newCONSTSUB(hash, name, value);
    } else {
	SvUPGRADE(sv, SVt_RV);
	SvRV_set(sv, value);
	SvROK_on(sv);
	SvREADONLY_on(value);
    }
}



#ifndef SYMBIAN

/* Store a hash of all symbols missing from the package. To avoid trampling on
   the package namespace (uninvited) put each package's hash in our namespace.
   To avoid creating lots of typeblogs and symbol tables for sub-packages, put
   each package's hash into one hash in our namespace.  */

static HV *
get_missing_hash(pTHX) {
    HV *const parent
	= get_hv("ExtUtils::Constant::ProxySubs::Missing", GVf_MULTI);
    /* We could make a hash of hashes directly, but this would confuse anything
	at Perl space that looks at us, and as we're visible in Perl space,
	best to play nice. */
    SV *const *const ref
	= hv_fetch(parent, "Socket", 6, TRUE);
    HV *new_hv;

    if (!ref)
	return NULL;

    if (SvROK(*ref))
	return (HV*) SvRV(*ref);

    new_hv = newHV();
    SvUPGRADE(*ref, SVt_RV);
    SvRV_set(*ref, (SV *)new_hv);
    SvROK_on(*ref);
    return new_hv;
}

#endif

struct notfound_s {const char *name; I32 namelen;} ;

    static const struct notfound_s values_for_notfound[] =
      {
#ifndef AF_802
        { "AF_802", 6 },
#endif
#ifndef AF_AAL
        { "AF_AAL", 6 },
#endif
#ifndef AF_APPLETALK
        { "AF_APPLETALK", 12 },
#endif
#ifndef AF_CCITT
        { "AF_CCITT", 8 },
#endif
#ifndef AF_CHAOS
        { "AF_CHAOS", 8 },
#endif
#ifndef AF_CTF
        { "AF_CTF", 6 },
#endif
#ifndef AF_DATAKIT
        { "AF_DATAKIT", 10 },
#endif
#ifndef AF_DECnet
        { "AF_DECnet", 9 },
#endif
#ifndef AF_DLI
        { "AF_DLI", 6 },
#endif
#ifndef AF_ECMA
        { "AF_ECMA", 7 },
#endif
#ifndef AF_GOSIP
        { "AF_GOSIP", 8 },
#endif
#ifndef AF_HYLINK
        { "AF_HYLINK", 9 },
#endif
#ifndef AF_IMPLINK
        { "AF_IMPLINK", 10 },
#endif
#ifndef AF_INET
        { "AF_INET", 7 },
#endif
#ifndef AF_INET6
        { "AF_INET6", 8 },
#endif
#ifndef AF_ISO
        { "AF_ISO", 6 },
#endif
#ifndef AF_KEY
        { "AF_KEY", 6 },
#endif
#ifndef AF_LAST
        { "AF_LAST", 7 },
#endif
#ifndef AF_LAT
        { "AF_LAT", 6 },
#endif
#ifndef AF_LINK
        { "AF_LINK", 7 },
#endif
#ifndef AF_MAX
        { "AF_MAX", 6 },
#endif
#ifndef AF_NBS
        { "AF_NBS", 6 },
#endif
#ifndef AF_NIT
        { "AF_NIT", 6 },
#endif
#ifndef AF_NS
        { "AF_NS", 5 },
#endif
#ifndef AF_OSI
        { "AF_OSI", 6 },
#endif
#ifndef AF_OSINET
        { "AF_OSINET", 9 },
#endif
#ifndef AF_PUP
        { "AF_PUP", 6 },
#endif
#ifndef AF_ROUTE
        { "AF_ROUTE", 8 },
#endif
#ifndef AF_SNA
        { "AF_SNA", 6 },
#endif
#ifndef AF_UNIX
        { "AF_UNIX", 7 },
#endif
#ifndef AF_UNSPEC
        { "AF_UNSPEC", 9 },
#endif
#ifndef AF_USER
        { "AF_USER", 7 },
#endif
#ifndef AF_WAN
        { "AF_WAN", 6 },
#endif
#ifndef AF_X25
        { "AF_X25", 6 },
#endif
#ifndef AI_ADDRCONFIG
        { "AI_ADDRCONFIG", 13 },
#endif
#ifndef AI_ALL
        { "AI_ALL", 6 },
#endif
#ifndef AI_CANONIDN
        { "AI_CANONIDN", 11 },
#endif
#ifndef AI_CANONNAME
        { "AI_CANONNAME", 12 },
#endif
#ifndef AI_IDN
        { "AI_IDN", 6 },
#endif
#ifndef AI_IDN_ALLOW_UNASSIGNED
        { "AI_IDN_ALLOW_UNASSIGNED", 23 },
#endif
#ifndef AI_IDN_USE_STD3_ASCII_RULES
        { "AI_IDN_USE_STD3_ASCII_RULES", 27 },
#endif
#ifndef AI_NUMERICHOST
        { "AI_NUMERICHOST", 14 },
#endif
#ifndef AI_NUMERICSERV
        { "AI_NUMERICSERV", 14 },
#endif
#ifndef AI_PASSIVE
        { "AI_PASSIVE", 10 },
#endif
#ifndef AI_V4MAPPED
        { "AI_V4MAPPED", 11 },
#endif
#ifndef EAI_ADDRFAMILY
        { "EAI_ADDRFAMILY", 14 },
#endif
#ifndef EAI_AGAIN
        { "EAI_AGAIN", 9 },
#endif
#ifndef EAI_BADFLAGS
        { "EAI_BADFLAGS", 12 },
#endif
#ifndef EAI_BADHINTS
        { "EAI_BADHINTS", 12 },
#endif
#ifndef EAI_FAIL
        { "EAI_FAIL", 8 },
#endif
#ifndef EAI_FAMILY
        { "EAI_FAMILY", 10 },
#endif
#ifndef EAI_NODATA
        { "EAI_NODATA", 10 },
#endif
#ifndef EAI_NONAME
        { "EAI_NONAME", 10 },
#endif
#ifndef EAI_PROTOCOL
        { "EAI_PROTOCOL", 12 },
#endif
#ifndef EAI_SERVICE
        { "EAI_SERVICE", 11 },
#endif
#ifndef EAI_SOCKTYPE
        { "EAI_SOCKTYPE", 12 },
#endif
#ifndef EAI_SYSTEM
        { "EAI_SYSTEM", 10 },
#endif
#ifndef IOV_MAX
        { "IOV_MAX", 7 },
#endif
#ifndef IP_ADD_MEMBERSHIP
        { "IP_ADD_MEMBERSHIP", 17 },
#endif
#ifndef IP_ADD_SOURCE_MEMBERSHIP
        { "IP_ADD_SOURCE_MEMBERSHIP", 24 },
#endif
#ifndef IP_DROP_MEMBERSHIP
        { "IP_DROP_MEMBERSHIP", 18 },
#endif
#ifndef IP_DROP_SOURCE_MEMBERSHIP
        { "IP_DROP_SOURCE_MEMBERSHIP", 25 },
#endif
#ifndef IP_HDRINCL
        { "IP_HDRINCL", 10 },
#endif
#ifndef IP_MULTICAST_IF
        { "IP_MULTICAST_IF", 15 },
#endif
#ifndef IP_MULTICAST_LOOP
        { "IP_MULTICAST_LOOP", 17 },
#endif
#ifndef IP_MULTICAST_TTL
        { "IP_MULTICAST_TTL", 16 },
#endif
#ifndef IP_OPTIONS
        { "IP_OPTIONS", 10 },
#endif
#ifndef IP_RECVOPTS
        { "IP_RECVOPTS", 11 },
#endif
#ifndef IP_RECVRETOPTS
        { "IP_RECVRETOPTS", 14 },
#endif
#ifndef IP_RETOPTS
        { "IP_RETOPTS", 10 },
#endif
#ifndef IP_TOS
        { "IP_TOS", 6 },
#endif
#ifndef IP_TTL
        { "IP_TTL", 6 },
#endif
#ifndef IPTOS_LOWDELAY
        { "IPTOS_LOWDELAY", 14 },
#endif
#ifndef IPTOS_THROUGHPUT
        { "IPTOS_THROUGHPUT", 16 },
#endif
#ifndef IPTOS_RELIABILITY
        { "IPTOS_RELIABILITY", 17 },
#endif
#ifndef IPTOS_MINCOST
        { "IPTOS_MINCOST", 13 },
#endif
#ifndef IPV6_ADD_MEMBERSHIP
        { "IPV6_ADD_MEMBERSHIP", 19 },
#endif
#ifndef IPV6_DROP_MEMBERSHIP
        { "IPV6_DROP_MEMBERSHIP", 20 },
#endif
#ifndef IPV6_JOIN_GROUP
        { "IPV6_JOIN_GROUP", 15 },
#endif
#ifndef IPV6_LEAVE_GROUP
        { "IPV6_LEAVE_GROUP", 16 },
#endif
#ifndef IPV6_MTU
        { "IPV6_MTU", 8 },
#endif
#ifndef IPV6_MTU_DISCOVER
        { "IPV6_MTU_DISCOVER", 17 },
#endif
#ifndef IPV6_MULTICAST_HOPS
        { "IPV6_MULTICAST_HOPS", 19 },
#endif
#ifndef IPV6_MULTICAST_IF
        { "IPV6_MULTICAST_IF", 17 },
#endif
#ifndef IPV6_MULTICAST_LOOP
        { "IPV6_MULTICAST_LOOP", 19 },
#endif
#ifndef IPV6_UNICAST_HOPS
        { "IPV6_UNICAST_HOPS", 17 },
#endif
#ifndef IPV6_V6ONLY
        { "IPV6_V6ONLY", 11 },
#endif
#ifndef MSG_BCAST
        { "MSG_BCAST", 9 },
#endif
#ifndef MSG_BTAG
        { "MSG_BTAG", 8 },
#endif
#ifndef MSG_CTLFLAGS
        { "MSG_CTLFLAGS", 12 },
#endif
#ifndef MSG_CTLIGNORE
        { "MSG_CTLIGNORE", 13 },
#endif
#ifndef MSG_DONTWAIT
        { "MSG_DONTWAIT", 12 },
#endif
#ifndef MSG_EOF
        { "MSG_EOF", 7 },
#endif
#ifndef MSG_EOR
        { "MSG_EOR", 7 },
#endif
#ifndef MSG_ERRQUEUE
        { "MSG_ERRQUEUE", 12 },
#endif
#ifndef MSG_ETAG
        { "MSG_ETAG", 8 },
#endif
#ifndef MSG_FIN
        { "MSG_FIN", 7 },
#endif
#ifndef MSG_MAXIOVLEN
        { "MSG_MAXIOVLEN", 13 },
#endif
#ifndef MSG_MCAST
        { "MSG_MCAST", 9 },
#endif
#ifndef MSG_NOSIGNAL
        { "MSG_NOSIGNAL", 12 },
#endif
#ifndef MSG_RST
        { "MSG_RST", 7 },
#endif
#ifndef MSG_SYN
        { "MSG_SYN", 7 },
#endif
#ifndef MSG_TRUNC
        { "MSG_TRUNC", 9 },
#endif
#ifndef MSG_URG
        { "MSG_URG", 7 },
#endif
#ifndef MSG_WAITALL
        { "MSG_WAITALL", 11 },
#endif
#ifndef MSG_WIRE
        { "MSG_WIRE", 8 },
#endif
#ifndef NI_DGRAM
        { "NI_DGRAM", 8 },
#endif
#ifndef NI_IDN
        { "NI_IDN", 6 },
#endif
#ifndef NI_IDN_ALLOW_UNASSIGNED
        { "NI_IDN_ALLOW_UNASSIGNED", 23 },
#endif
#ifndef NI_IDN_USE_STD3_ASCII_RULES
        { "NI_IDN_USE_STD3_ASCII_RULES", 27 },
#endif
#ifndef NI_NAMEREQD
        { "NI_NAMEREQD", 11 },
#endif
#ifndef NI_NOFQDN
        { "NI_NOFQDN", 9 },
#endif
#ifndef NI_NUMERICHOST
        { "NI_NUMERICHOST", 14 },
#endif
#ifndef NI_NUMERICSERV
        { "NI_NUMERICSERV", 14 },
#endif
#ifndef PF_802
        { "PF_802", 6 },
#endif
#ifndef PF_AAL
        { "PF_AAL", 6 },
#endif
#ifndef PF_APPLETALK
        { "PF_APPLETALK", 12 },
#endif
#ifndef PF_CCITT
        { "PF_CCITT", 8 },
#endif
#ifndef PF_CHAOS
        { "PF_CHAOS", 8 },
#endif
#ifndef PF_CTF
        { "PF_CTF", 6 },
#endif
#ifndef PF_DATAKIT
        { "PF_DATAKIT", 10 },
#endif
#ifndef PF_DECnet
        { "PF_DECnet", 9 },
#endif
#ifndef PF_DLI
        { "PF_DLI", 6 },
#endif
#ifndef PF_ECMA
        { "PF_ECMA", 7 },
#endif
#ifndef PF_GOSIP
        { "PF_GOSIP", 8 },
#endif
#ifndef PF_HYLINK
        { "PF_HYLINK", 9 },
#endif
#ifndef PF_IMPLINK
        { "PF_IMPLINK", 10 },
#endif
#ifndef PF_INET
        { "PF_INET", 7 },
#endif
#ifndef PF_INET6
        { "PF_INET6", 8 },
#endif
#ifndef PF_ISO
        { "PF_ISO", 6 },
#endif
#ifndef PF_KEY
        { "PF_KEY", 6 },
#endif
#ifndef PF_LAST
        { "PF_LAST", 7 },
#endif
#ifndef PF_LAT
        { "PF_LAT", 6 },
#endif
#ifndef PF_LINK
        { "PF_LINK", 7 },
#endif
#ifndef PF_MAX
        { "PF_MAX", 6 },
#endif
#ifndef PF_NBS
        { "PF_NBS", 6 },
#endif
#ifndef PF_NIT
        { "PF_NIT", 6 },
#endif
#ifndef PF_NS
        { "PF_NS", 5 },
#endif
#ifndef PF_OSI
        { "PF_OSI", 6 },
#endif
#ifndef PF_OSINET
        { "PF_OSINET", 9 },
#endif
#ifndef PF_PUP
        { "PF_PUP", 6 },
#endif
#ifndef PF_ROUTE
        { "PF_ROUTE", 8 },
#endif
#ifndef PF_SNA
        { "PF_SNA", 6 },
#endif
#ifndef PF_UNIX
        { "PF_UNIX", 7 },
#endif
#ifndef PF_UNSPEC
        { "PF_UNSPEC", 9 },
#endif
#ifndef PF_USER
        { "PF_USER", 7 },
#endif
#ifndef PF_WAN
        { "PF_WAN", 6 },
#endif
#ifndef PF_X25
        { "PF_X25", 6 },
#endif
#ifndef SCM_CONNECT
        { "SCM_CONNECT", 11 },
#endif
#ifndef SCM_CREDENTIALS
        { "SCM_CREDENTIALS", 15 },
#endif
#ifndef SCM_CREDS
        { "SCM_CREDS", 9 },
#endif
#ifndef SCM_TIMESTAMP
        { "SCM_TIMESTAMP", 13 },
#endif
#ifndef SOCK_DGRAM
        { "SOCK_DGRAM", 10 },
#endif
#ifndef SOCK_RAW
        { "SOCK_RAW", 8 },
#endif
#ifndef SOCK_RDM
        { "SOCK_RDM", 8 },
#endif
#ifndef SOCK_SEQPACKET
        { "SOCK_SEQPACKET", 14 },
#endif
#ifndef SOCK_STREAM
        { "SOCK_STREAM", 11 },
#endif
#ifndef SOCK_NONBLOCK
        { "SOCK_NONBLOCK", 13 },
#endif
#ifndef SOCK_CLOEXEC
        { "SOCK_CLOEXEC", 12 },
#endif
#ifndef SOL_SOCKET
        { "SOL_SOCKET", 10 },
#endif
#ifndef SOMAXCONN
        { "SOMAXCONN", 9 },
#endif
#ifndef SO_ACCEPTCONN
        { "SO_ACCEPTCONN", 13 },
#endif
#ifndef SO_ATTACH_FILTER
        { "SO_ATTACH_FILTER", 16 },
#endif
#ifndef SO_BACKLOG
        { "SO_BACKLOG", 10 },
#endif
#ifndef SO_BROADCAST
        { "SO_BROADCAST", 12 },
#endif
#ifndef SO_CHAMELEON
        { "SO_CHAMELEON", 12 },
#endif
#ifndef SO_DEBUG
        { "SO_DEBUG", 8 },
#endif
#ifndef SO_DETACH_FILTER
        { "SO_DETACH_FILTER", 16 },
#endif
#ifndef SO_DGRAM_ERRIND
        { "SO_DGRAM_ERRIND", 15 },
#endif
#ifndef SO_DOMAIN
        { "SO_DOMAIN", 9 },
#endif
#ifndef SO_DONTLINGER
        { "SO_DONTLINGER", 13 },
#endif
#ifndef SO_DONTROUTE
        { "SO_DONTROUTE", 12 },
#endif
#ifndef SO_ERROR
        { "SO_ERROR", 8 },
#endif
#ifndef SO_FAMILY
        { "SO_FAMILY", 9 },
#endif
#ifndef SO_KEEPALIVE
        { "SO_KEEPALIVE", 12 },
#endif
#ifndef SO_LINGER
        { "SO_LINGER", 9 },
#endif
#ifndef SO_OOBINLINE
        { "SO_OOBINLINE", 12 },
#endif
#ifndef SO_PASSCRED
        { "SO_PASSCRED", 11 },
#endif
#ifndef SO_PASSIFNAME
        { "SO_PASSIFNAME", 13 },
#endif
#ifndef SO_PEERCRED
        { "SO_PEERCRED", 11 },
#endif
#ifndef SO_PROTOCOL
        { "SO_PROTOCOL", 11 },
#endif
#ifndef SO_PROTOTYPE
        { "SO_PROTOTYPE", 12 },
#endif
#ifndef SO_RCVBUF
        { "SO_RCVBUF", 9 },
#endif
#ifndef SO_RCVLOWAT
        { "SO_RCVLOWAT", 11 },
#endif
#ifndef SO_RCVTIMEO
        { "SO_RCVTIMEO", 11 },
#endif
#ifndef SO_REUSEADDR
        { "SO_REUSEADDR", 12 },
#endif
#ifndef SO_REUSEPORT
        { "SO_REUSEPORT", 12 },
#endif
#ifndef SO_SECURITY_AUTHENTICATION
        { "SO_SECURITY_AUTHENTICATION", 26 },
#endif
#ifndef SO_SECURITY_ENCRYPTION_NETWORK
        { "SO_SECURITY_ENCRYPTION_NETWORK", 30 },
#endif
#ifndef SO_SECURITY_ENCRYPTION_TRANSPORT
        { "SO_SECURITY_ENCRYPTION_TRANSPORT", 32 },
#endif
#ifndef SO_SNDBUF
        { "SO_SNDBUF", 9 },
#endif
#ifndef SO_SNDLOWAT
        { "SO_SNDLOWAT", 11 },
#endif
#ifndef SO_SNDTIMEO
        { "SO_SNDTIMEO", 11 },
#endif
#ifndef SO_STATE
        { "SO_STATE", 8 },
#endif
#ifndef SO_TYPE
        { "SO_TYPE", 7 },
#endif
#ifndef SO_USELOOPBACK
        { "SO_USELOOPBACK", 14 },
#endif
#ifndef SO_XOPEN
        { "SO_XOPEN", 8 },
#endif
#ifndef SO_XSE
        { "SO_XSE", 6 },
#endif
#ifndef TCP_CONGESTION
        { "TCP_CONGESTION", 14 },
#endif
#ifndef TCP_CONNECTIONTIMEOUT
        { "TCP_CONNECTIONTIMEOUT", 21 },
#endif
#ifndef TCP_CORK
        { "TCP_CORK", 8 },
#endif
#ifndef TCP_DEFER_ACCEPT
        { "TCP_DEFER_ACCEPT", 16 },
#endif
#ifndef TCP_INFO
        { "TCP_INFO", 8 },
#endif
#ifndef TCP_INIT_CWND
        { "TCP_INIT_CWND", 13 },
#endif
#ifndef TCP_KEEPALIVE
        { "TCP_KEEPALIVE", 13 },
#endif
#ifndef TCP_KEEPCNT
        { "TCP_KEEPCNT", 11 },
#endif
#ifndef TCP_KEEPIDLE
        { "TCP_KEEPIDLE", 12 },
#endif
#ifndef TCP_KEEPINTVL
        { "TCP_KEEPINTVL", 13 },
#endif
#ifndef TCP_LINGER2
        { "TCP_LINGER2", 11 },
#endif
#ifndef TCP_MAXRT
        { "TCP_MAXRT", 9 },
#endif
#ifndef TCP_MAXSEG
        { "TCP_MAXSEG", 10 },
#endif
#ifndef TCP_MD5SIG
        { "TCP_MD5SIG", 10 },
#endif
#ifndef TCP_NODELAY
        { "TCP_NODELAY", 11 },
#endif
#ifndef TCP_NOOPT
        { "TCP_NOOPT", 9 },
#endif
#ifndef TCP_NOPUSH
        { "TCP_NOPUSH", 10 },
#endif
#ifndef TCP_QUICKACK
        { "TCP_QUICKACK", 12 },
#endif
#ifndef TCP_SACK_ENABLE
        { "TCP_SACK_ENABLE", 15 },
#endif
#ifndef TCP_STDURG
        { "TCP_STDURG", 10 },
#endif
#ifndef TCP_SYNCNT
        { "TCP_SYNCNT", 10 },
#endif
#ifndef TCP_WINDOW_CLAMP
        { "TCP_WINDOW_CLAMP", 16 },
#endif
#ifndef UIO_MAXIOV
        { "UIO_MAXIOV", 10 },
#endif
#if defined(MSG_CTRUNC) || defined(HAS_MSG_CTRUNC) /* might be an enum */
#else
        { "MSG_CTRUNC", 10 },
#endif
#if defined(MSG_DONTROUTE) || defined(HAS_MSG_DONTROUTE) /* might be an enum */
#else
        { "MSG_DONTROUTE", 13 },
#endif
#if defined(MSG_OOB) || defined(HAS_MSG_OOB) /* might be an enum */
#else
        { "MSG_OOB", 7 },
#endif
#if defined(MSG_PEEK) || defined(HAS_MSG_PEEK) /* might be an enum */
#else
        { "MSG_PEEK", 8 },
#endif
#if defined(MSG_PROXY) || defined(HAS_MSG_PROXY) /* might be an enum */
#else
        { "MSG_PROXY", 9 },
#endif
#if defined(SCM_RIGHTS) || defined(HAS_SCM_RIGHTS) /* might be an enum */
#else
        { "SCM_RIGHTS", 10 },
#endif
#ifndef INADDR_ANY
        { "INADDR_ANY", 10 },
#endif
#ifndef INADDR_LOOPBACK
        { "INADDR_LOOPBACK", 15 },
#endif
#ifndef INADDR_NONE
        { "INADDR_NONE", 11 },
#endif
#ifndef INADDR_BROADCAST
        { "INADDR_BROADCAST", 16 },
#endif
#ifdef IN6ADDR_ANY_INIT
#else
        { "IN6ADDR_ANY", 11 },
#endif
#ifdef IN6ADDR_LOOPBACK_INIT
#else
        { "IN6ADDR_LOOPBACK", 16 },
#endif
        { NULL, 0 } };
struct iv_s {const char *name; I32 namelen; IV value;};
