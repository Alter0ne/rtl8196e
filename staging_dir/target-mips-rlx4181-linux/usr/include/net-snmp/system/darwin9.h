/*
 * While Darwin 9 (aka, Mac OS X 10.5 Leopard) is "BSD-like", it differs
 * substantially enough to not warrant pretending it is a BSD flavor.
 * This first section are the vestigal BSD remnants.
 */

/*
 * BSD systems use a different method of looking up sockaddr_in values 
 */
/* #define NEED_KLGETSA 1 */

/*
 * ARP_Scan_Next needs a 4th ifIndex argument 
 */
#define ARP_SCAN_FOUR_ARGUMENTS 1

#define CHECK_RT_FLAGS 1

/*
 * this is not good enough before freebsd3! 
 */
/* #undef HAVE_NET_IF_MIB_H */

/*
 * This section adds the relevant definitions from generic.h
 * (a file we don't include here)
 */

/*
 * udp_inpcb list symbol, e.g. for mibII/udpTable.c
 */
#define INP_NEXT_SYMBOL inp_next

/*
 * This section defines Mac OS X 10.5 (and later) specific additions.
 */
#define darwin 9

/*
 * Mac OS X should only use the modern API and definitions.
 */
#ifndef NETSNMP_NO_LEGACY_DEFINITIONS
#define NETSNMP_NO_LEGACY_DEFINITIONS 1
#endif

/*
 * looks like the IFTable stuff works better than the mibII versions
 */
 
#define NETSNMP_INCLUDE_IFTABLE_REWRITES

/*
 * Enabling this restricts the compiler to mostly public APIs.
 */
#ifndef __APPLE_API_STRICT_CONFORMANCE
#define __APPLE_API_STRICT_CONFORMANCE 1
#endif
#ifndef __APPLE_API_UNSTABLE
#define __APPLE_API_UNSTABLE 1
#endif

/*
 * Darwin's tools are capable of building multiple architectures in one pass.
 * As a result, platform definitions should be deferred until compile time.
 */
#ifdef BYTE_ORDER
# undef WORDS_BIGENDIAN
# if BYTE_ORDER == BIG_ENDIAN
#  define WORDS_BIGENDIAN 1
# endif
#endif

/*
 * Darwin's tools are capable of building multiple architectures in one pass.
 * As a result, platform definitions should be deferred until compile time.
 */
#ifdef BYTE_ORDER
# undef WORDS_BIGENDIAN
# if BYTE_ORDER == BIG_ENDIAN
#  define WORDS_BIGENDIAN 1
# endif
#endif

/*
 * Although Darwin does have a kvm.h file, kvm_openfiles etc. always
 * return null because /dev/kmem was removed completely in OS X 10.5.
 */
#undef HAVE_KVM_H
#undef HAVE_KVM_GETPROCS
#undef HAVE_KVM_OPENFILES

/*
 * Although Darwin does have an fstab.h file, getfsfile etc. always return null.
 * At least, as of 5.3.
 */
#undef HAVE_FSTAB_H

#define SWAPFILE_DIR "/private/var/vm"
#define SWAPFILE_PREFIX "swapfile"

/*
 * These apparently used to be in netinet/tcp_timers.h, but went away in
 * 10.4.2. Define them here til we find out a way to get the real values.
 */
#define TCPTV_MIN       (  1*PR_SLOWHZ)         /* minimum allowable value */
#define TCPTV_REXMTMAX  ( 64*PR_SLOWHZ)         /* max allowable REXMT value */

/*
 * Because Mac OS X is built on Mach, it does not provide a BSD-compatible
 * VM statistics API.
 */
#define USE_MACH_HOST_STATISTICS 1

/*
 * This tells code that manipulates IPv6 that the structures are unified,
 * i.e., IPv4 and IPv6 use the same structs.
 * This should eventually be replaced with a configure directive.
 */
/* #define USE_UNIFIED_IPV6_STRUCTS 1 */
#undef STRUCT_in6pcb_HAS_inp_vflag

/*
 * Mac OS X runs on both PPC and Intel hardware,
 *   which handle udpTable index values differently
 */
#include <TargetConditionals.h>
#ifdef TARGET_RT_LITTLE_ENDIAN
#define UDP_ADDRESSES_IN_HOST_ORDER 1
#endif
