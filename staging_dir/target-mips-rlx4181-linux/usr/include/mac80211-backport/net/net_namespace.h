#ifndef _COMPAT_NET_NET_NAMESPACE_H
#define _COMPAT_NET_NET_NAMESPACE_H 1

#include <linux/version.h>
#include <net/netns/ieee802154_6lowpan.h>
#include_next <net/net_namespace.h>

#if (LINUX_VERSION_CODE < KERNEL_VERSION(3,15,0))
#if (LINUX_VERSION_CODE >= KERNEL_VERSION(3,5,0))
/*
 * we provide backport for 6lowpan as per the dependencies file
 * down to 3.5 only.
 */
extern struct netns_ieee802154_lowpan ieee802154_lowpan;
struct netns_ieee802154_lowpan *net_ieee802154_lowpan(struct net *net);
#endif
#else /* < 3.5..3.14 */
/* This can be removed once and if this gets upstream */
static inline struct netns_ieee802154_lowpan *
net_ieee802154_lowpan(struct net *net)
{
	return &net->ieee802154_lowpan;
}
#endif /* LINUX_VERSION_CODE < KERNEL_VERSION(3,15,0) */

#endif	/* _COMPAT_NET_NET_NAMESPACE_H */
