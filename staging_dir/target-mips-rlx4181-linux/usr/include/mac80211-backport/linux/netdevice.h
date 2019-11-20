#ifndef __BACKPORT_NETDEVICE_H
#define __BACKPORT_NETDEVICE_H
#include_next <linux/netdevice.h>
#include <linux/netdev_features.h>
#include <linux/version.h>

/*
 * This is declared implicitly in newer kernels by netdevice.h using
 * this pointer in struct net_device, but declare it here anyway so
 * pointers to it are accepted as function arguments without warning.
 */
struct inet6_dev;

/* older kernels don't include this here, we need it */
#include <linux/ethtool.h>
#include <linux/rculist.h>
/*
 * new kernels include <net/netprio_cgroup.h> which
 * has this ... and some drivers rely on it :-(
 */
#include <linux/hardirq.h>

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,14,0)
/*
 * Backports note: if in-kernel support is provided we could then just
 * take the kernel's implementation of __dev_kfree_skb_irq() as it requires
 * raise_softirq_irqoff() which is not exported. For the backport case we
 * just use slightly less optimized version and we don't get the ability
 * to distinguish the two different reasons to free the skb -- whether it
 * was consumed or dropped.
 *
 * The upstream documentation for this:
 *
 * It is not allowed to call kfree_skb() or consume_skb() from hardware
 * interrupt context or with hardware interrupts being disabled.
 * (in_irq() || irqs_disabled())
 *
 * We provide four helpers that can be used in following contexts :
 *
 * dev_kfree_skb_irq(skb) when caller drops a packet from irq context,
 *  replacing kfree_skb(skb)
 *
 * dev_consume_skb_irq(skb) when caller consumes a packet from irq context.
 *  Typically used in place of consume_skb(skb) in TX completion path
 *
 * dev_kfree_skb_any(skb) when caller doesn't know its current irq context,
 *  replacing kfree_skb(skb)
 *
 * dev_consume_skb_any(skb) when caller doesn't know its current irq context,
 *  and consumed a packet. Used in place of consume_skb(skb)
 */
#define skb_free_reason LINUX_BACKPORT(skb_free_reason)
enum skb_free_reason {
	SKB_REASON_CONSUMED,
	SKB_REASON_DROPPED,
};

#define __dev_kfree_skb_irq LINUX_BACKPORT(__dev_kfree_skb_irq)
static inline void __dev_kfree_skb_irq(struct sk_buff *skb,
				       enum skb_free_reason reason)
{
	dev_kfree_skb_irq(skb);
}

#define __dev_kfree_skb_any LINUX_BACKPORT(__dev_kfree_skb_any)
static inline void __dev_kfree_skb_any(struct sk_buff *skb,
				       enum skb_free_reason reason)
{
	dev_kfree_skb_any(skb);
}

#define dev_consume_skb_irq LINUX_BACKPORT(dev_consume_skb_irq)
static inline void dev_consume_skb_irq(struct sk_buff *skb)
{
	dev_kfree_skb_irq(skb);
}

#define dev_consume_skb_any LINUX_BACKPORT(dev_consume_skb_any)
static inline void dev_consume_skb_any(struct sk_buff *skb)
{
	dev_kfree_skb_any(skb);
}
#endif /* LINUX_VERSION_CODE < KERNEL_VERSION(3,14,0) */

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,7,8)
#define netdev_set_default_ethtool_ops LINUX_BACKPORT(netdev_set_default_ethtool_ops)
extern void netdev_set_default_ethtool_ops(struct net_device *dev,
					   const struct ethtool_ops *ops);
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,3,0)
/*
 * BQL was added as of v3.3 but some Linux distributions
 * have backported BQL to their v3.2 kernels or older. To
 * address this we assume that they also enabled CONFIG_BQL
 * and test for that here and simply avoid adding the static
 * inlines if it was defined
 */
#ifndef CONFIG_BQL
#define netdev_tx_sent_queue LINUX_BACKPORT(netdev_tx_sent_queue)
static inline void netdev_tx_sent_queue(struct netdev_queue *dev_queue,
					unsigned int bytes)
{
}

#define netdev_sent_queue LINUX_BACKPORT(netdev_sent_queue)
static inline void netdev_sent_queue(struct net_device *dev, unsigned int bytes)
{
}

#define netdev_tx_completed_queue LINUX_BACKPORT(netdev_tx_completed_queue)
static inline void netdev_tx_completed_queue(struct netdev_queue *dev_queue,
					     unsigned pkts, unsigned bytes)
{
}

#define netdev_completed_queue LINUX_BACKPORT(netdev_completed_queue)
static inline void netdev_completed_queue(struct net_device *dev,
					  unsigned pkts, unsigned bytes)
{
}

#define netdev_tx_reset_queue LINUX_BACKPORT(netdev_tx_reset_queue)
static inline void netdev_tx_reset_queue(struct netdev_queue *q)
{
}

#define netdev_reset_queue LINUX_BACKPORT(netdev_reset_queue)
static inline void netdev_reset_queue(struct net_device *dev_queue)
{
}
#endif /* CONFIG_BQL */
#endif /* < 3.3 */

#ifndef NETDEV_PRE_UP
#define NETDEV_PRE_UP		0x000D
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,11,0)
#define netdev_notifier_info_to_dev(ndev) ndev
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,7,0)
#define netdev_notify_peers(dev) netif_notify_peers(dev)
#define napi_gro_flush(napi, old) napi_gro_flush(napi)
#endif

#endif /* __BACKPORT_NETDEVICE_H */
