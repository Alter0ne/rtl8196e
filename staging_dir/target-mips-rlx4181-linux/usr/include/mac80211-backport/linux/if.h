#ifndef _BACKPORT_LINUX_IF_H
#define _BACKPORT_LINUX_IF_H
#include_next <linux/if.h>
#include <linux/version.h>

/* mask IFF_DONT_BRIDGE as RHEL6 backports this */
#if !defined(IFF_DONT_BRIDGE)
#define IFF_DONT_BRIDGE 0x800		/* disallow bridging this ether dev */
#endif

#ifndef  IFF_TX_SKB_SHARING
#define IFF_TX_SKB_SHARING	0x10000
#endif

#ifndef IFF_LIVE_ADDR_CHANGE
#define IFF_LIVE_ADDR_CHANGE 0x100000
#endif

#ifndef IFF_SUPP_NOFCS
#define IFF_SUPP_NOFCS	0x80000		/* device supports sending custom FCS */
#endif

#ifndef IFF_UNICAST_FLT
#define IFF_UNICAST_FLT	0x20000		/* Supports unicast filtering	*/
#endif

#endif	/* _BACKPORT_LINUX_IF_H */
