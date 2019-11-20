#ifndef __BACKPORT_SKBUFF_H
#define __BACKPORT_SKBUFF_H
#include_next <linux/skbuff.h>
#include <linux/version.h>

#if (LINUX_VERSION_CODE < KERNEL_VERSION(3,4,0)) && \
      (RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(6,4)) && \
      !(defined(CONFIG_SUSE_KERNEL) && (LINUX_VERSION_CODE >= KERNEL_VERSION(3,0,0)))
#define skb_add_rx_frag(skb, i, page, off, size, truesize) \
	skb_add_rx_frag(skb, i, page, off, size)
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,3,0)
#define __pskb_copy LINUX_BACKPORT(__pskb_copy)
extern struct sk_buff *__pskb_copy(struct sk_buff *skb,
				   int headroom, gfp_t gfp_mask);

#define skb_complete_wifi_ack LINUX_BACKPORT(skb_complete_wifi_ack)
static inline void skb_complete_wifi_ack(struct sk_buff *skb, bool acked)
{
	WARN_ON(1);
}
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,2,0)
#include <linux/dma-mapping.h>

/* mask skb_frag_page as RHEL6 backports this */
#define skb_frag_page LINUX_BACKPORT(skb_frag_page)
static inline struct page *skb_frag_page(const skb_frag_t *frag)
{
	return frag->page;
}

#define skb_frag_size LINUX_BACKPORT(skb_frag_size)
static inline unsigned int skb_frag_size(const skb_frag_t *frag)
{
	return frag->size;
}

/* mask skb_frag_dma_map as RHEL6 backports this */
#define skb_frag_dma_map LINUX_BACKPORT(skb_frag_dma_map)
static inline dma_addr_t skb_frag_dma_map(struct device *dev,
					  const skb_frag_t *frag,
					  size_t offset, size_t size,
					  enum dma_data_direction dir)
{
	return dma_map_page(dev, skb_frag_page(frag),
			    frag->page_offset + offset, size, dir);
}
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,1,0)
/* mask __netdev_alloc_skb_ip_align as RHEL6 backports this */
#define __netdev_alloc_skb_ip_align(a,b,c) compat__netdev_alloc_skb_ip_align(a,b,c)
static inline struct sk_buff *__netdev_alloc_skb_ip_align(struct net_device *dev,
							  unsigned int length, gfp_t gfp)
{
	struct sk_buff *skb = __netdev_alloc_skb(dev, length + NET_IP_ALIGN, gfp);

	if (NET_IP_ALIGN && skb)
		skb_reserve(skb, NET_IP_ALIGN);
	return skb;
}
#endif

#ifndef skb_walk_frags
#define skb_walk_frags(skb, iter)	\
	for (iter = skb_shinfo(skb)->frag_list; iter; iter = iter->next)
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,2,0)
#define skb_frag_size_sub LINUX_BACKPORT(skb_frag_size_sub)
static inline void skb_frag_size_sub(skb_frag_t *frag, int delta)
{
	frag->size -= delta;
}

/**
 * skb_frag_address - gets the address of the data contained in a paged fragment
 * @frag: the paged fragment buffer
 *
 * Returns the address of the data within @frag. The page must already
 * be mapped.
 */
#define skb_frag_address LINUX_BACKPORT(skb_frag_address)
static inline void *skb_frag_address(const skb_frag_t *frag)
{
	return page_address(skb_frag_page(frag)) + frag->page_offset;
}
#endif /* LINUX_VERSION_CODE < KERNEL_VERSION(3,2,0) */

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,6,0)
/**
 *	__skb_alloc_pages - allocate pages for ps-rx on a skb and preserve pfmemalloc data
 *	@gfp_mask: alloc_pages_node mask. Set __GFP_NOMEMALLOC if not for network packet RX
 *	@skb: skb to set pfmemalloc on if __GFP_MEMALLOC is used
 *	@order: size of the allocation
 *
 * 	Allocate a new page.
 *
 * 	%NULL is returned if there is no free memory.
*/
static inline struct page *__skb_alloc_pages(gfp_t gfp_mask,
					      struct sk_buff *skb,
					      unsigned int order)
{
	struct page *page;

	gfp_mask |= __GFP_COLD;
#if 0
	if (!(gfp_mask & __GFP_NOMEMALLOC))
		gfp_mask |= __GFP_MEMALLOC;
#endif
	page = alloc_pages_node(NUMA_NO_NODE, gfp_mask, order);
#if 0
	if (skb && page && page->pfmemalloc)
		skb->pfmemalloc = true;
#endif
	return page;
}

/**
 *	__skb_alloc_page - allocate a page for ps-rx for a given skb and preserve pfmemalloc data
 *	@gfp_mask: alloc_pages_node mask. Set __GFP_NOMEMALLOC if not for network packet RX
 *	@skb: skb to set pfmemalloc on if __GFP_MEMALLOC is used
 *
 * 	Allocate a new page.
 *
 * 	%NULL is returned if there is no free memory.
 */
static inline struct page *__skb_alloc_page(gfp_t gfp_mask,
					     struct sk_buff *skb)
{
	return __skb_alloc_pages(gfp_mask, skb, 0);
}
#endif /* LINUX_VERSION_CODE < KERNEL_VERSION(3,6,0) */

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,9,0)
#ifndef NETDEV_FRAG_PAGE_MAX_ORDER
#define NETDEV_FRAG_PAGE_MAX_ORDER get_order(32768)
#endif
#ifndef NETDEV_FRAG_PAGE_MAX_SIZE
#define NETDEV_FRAG_PAGE_MAX_SIZE  (PAGE_SIZE << NETDEV_FRAG_PAGE_MAX_ORDER)
#endif
#endif /* LINUX_VERSION_CODE < KERNEL_VERSION(3,9,0) */

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,9,0)
#define skb_unclone LINUX_BACKPORT(skb_unclone)
static inline int skb_unclone(struct sk_buff *skb, gfp_t pri)
{
	might_sleep_if(pri & __GFP_WAIT);
	if (skb_cloned(skb))
		return pskb_expand_head(skb, 0, 0, pri);
       return 0;
}
#endif

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,2,0)

#define skb_frag_size_set LINUX_BACKPORT(skb_frag_size_set)
static inline void skb_frag_size_set(skb_frag_t *frag, unsigned int size)
{
	frag->size = size;
}

#define skb_frag_size_add LINUX_BACKPORT(skb_frag_size_add)
static inline void skb_frag_size_add(skb_frag_t *frag, int delta)
{
	frag->size += delta;
}

#define __skb_fill_page_desc LINUX_BACKPORT(__skb_fill_page_desc)
/**
 * __skb_fill_page_desc - initialise a paged fragment in an skb
 * @skb: buffer containing fragment to be initialised
 * @i: paged fragment index to initialise
 * @page: the page to use for this fragment
 * @off: the offset to the data with @page
 * @size: the length of the data
 *
 * Initialises the @i'th fragment of @skb to point to &size bytes at
 * offset @off within @page.
 *
 * Does not take any additional reference on the fragment.
 */
static inline void __skb_fill_page_desc(struct sk_buff *skb, int i,
					struct page *page, int off, int size)
{
	skb_frag_t *frag = &skb_shinfo(skb)->frags[i];

	/*
	 * Propagate page->pfmemalloc to the skb if we can. The problem is
	 * that not all callers have unique ownership of the page. If
	 * pfmemalloc is set, we check the mapping as a mapping implies
	 * page->index is set (index and pfmemalloc share space).
	 * If it's a valid mapping, we cannot use page->pfmemalloc but we
	 * do not lose pfmemalloc information as the pages would not be
	 * allocated using __GFP_MEMALLOC.
	 */
	frag->page		  = page;
	frag->page_offset	  = off;
	skb_frag_size_set(frag, size);

#if 0 /* we can't backport this for older kernels */
	page = compound_head(page);
	if (page->pfmemalloc && !page->mapping)
		skb->pfmemalloc	= true;
#endif
}

#define skb_fill_page_desc LINUX_BACKPORT(skb_fill_page_desc)
/**
 * skb_fill_page_desc - initialise a paged fragment in an skb
 * @skb: buffer containing fragment to be initialised
 * @i: paged fragment index to initialise
 * @page: the page to use for this fragment
 * @off: the offset to the data with @page
 * @size: the length of the data
 *
 * As per __skb_fill_page_desc() -- initialises the @i'th fragment of
 * @skb to point to @size bytes at offset @off within @page. In
 * addition updates @skb such that @i is the last fragment.
 *
 * Does not take any additional reference on the fragment.
 */
static inline void skb_fill_page_desc(struct sk_buff *skb, int i,
				      struct page *page, int off, int size)
{
	__skb_fill_page_desc(skb, i, page, off, size);
	skb_shinfo(skb)->nr_frags = i + 1;
}

#define __skb_frag_ref LINUX_BACKPORT(__skb_frag_ref)
/**
 * __skb_frag_ref - take an addition reference on kb_frag_page paged fragment.
 * @frag: the paged fragment
 *
 * Takes an additional reference on the paged fragment @frag.
 */
static inline void __skb_frag_ref(skb_frag_t *frag)
{
	get_page(skb_frag_page(frag));
}

#define skb_frag_ref LINUX_BACKPORT(skb_frag_ref)
/**
 * skb_frag_ref - take an addition reference on a paged fragment of an skb.
 * @skb: the buffer
 * @f: the fragment offset.
 *
 * Takes an additional reference on the @f'th paged fragment of @skb.
 */
static inline void skb_frag_ref(struct sk_buff *skb, int f)
{
	__skb_frag_ref(&skb_shinfo(skb)->frags[f]);
}

#define __skb_frag_unref LINUX_BACKPORT(__skb_frag_unref)
/**
 * __skb_frag_unref - release a reference on a paged fragment.
 * @frag: the paged fragment
 *
 * Releases a reference on the paged fragment @frag.
 */
static inline void __skb_frag_unref(skb_frag_t *frag)
{
	put_page(skb_frag_page(frag));
}

#define skb_frag_unref LINUX_BACKPORT(skb_frag_unref)
/**
 * skb_frag_unref - release a reference on a paged fragment of an skb.
 * @skb: the buffer
 * @f: the fragment offset
 *
 * Releases a reference on the @f'th paged fragment of @skb.
 */
static inline void skb_frag_unref(struct sk_buff *skb, int f)
{
	__skb_frag_unref(&skb_shinfo(skb)->frags[f]);
}

#define skb_frag_address_safe LINUX_BACKPORT(skb_frag_address_safe)
/**
 * skb_frag_address_safe - gets the address of the data contained in a paged fragment
 * @frag: the paged fragment buffer
 *
 * Returns the address of the data within @frag. Checks that the page
 * is mapped and returns %NULL otherwise.
 */
static inline void *skb_frag_address_safe(const skb_frag_t *frag)
{
	void *ptr = page_address(skb_frag_page(frag));
	if (unlikely(!ptr))
		return NULL;

	return ptr + frag->page_offset;
}

#define __skb_frag_set_page LINUX_BACKPORT(__skb_frag_set_page)
/**
 * __skb_frag_set_page - sets the page contained in a paged fragment
 * @frag: the paged fragment
 * @page: the page to set
 *
 * Sets the fragment @frag to contain @page.
 */
static inline void __skb_frag_set_page(skb_frag_t *frag, struct page *page)
{
	frag->page = page;
}

#define skb_frag_set_page LINUX_BACKPORT(skb_frag_set_page)
/**
 * skb_frag_set_page - sets the page contained in a paged fragment of an skb
 * @skb: the buffer
 * @f: the fragment offset
 * @page: the page to set
 *
 * Sets the @f'th fragment of @skb to contain @page.
 */
static inline void skb_frag_set_page(struct sk_buff *skb, int f,
				     struct page *page)
{
	__skb_frag_set_page(&skb_shinfo(skb)->frags[f], page);
}
#endif /* LINUX_VERSION_CODE < KERNEL_VERSION(3,2,0) */

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,14,0) && RHEL_RELEASE_CODE < RHEL_RELEASE_VERSION(7,0)
/*
 * Packet hash types specify the type of hash in skb_set_hash.
 *
 * Hash types refer to the protocol layer addresses which are used to
 * construct a packet's hash. The hashes are used to differentiate or identify
 * flows of the protocol layer for the hash type. Hash types are either
 * layer-2 (L2), layer-3 (L3), or layer-4 (L4).
 *
 * Properties of hashes:
 *
 * 1) Two packets in different flows have different hash values
 * 2) Two packets in the same flow should have the same hash value
 *
 * A hash at a higher layer is considered to be more specific. A driver should
 * set the most specific hash possible.
 *
 * A driver cannot indicate a more specific hash than the layer at which a hash
 * was computed. For instance an L3 hash cannot be set as an L4 hash.
 *
 * A driver may indicate a hash level which is less specific than the
 * actual layer the hash was computed on. For instance, a hash computed
 * at L4 may be considered an L3 hash. This should only be done if the
 * driver can't unambiguously determine that the HW computed the hash at
 * the higher layer. Note that the "should" in the second property above
 * permits this.
 */
enum pkt_hash_types {
	PKT_HASH_TYPE_NONE,	/* Undefined type */
	PKT_HASH_TYPE_L2,	/* Input: src_MAC, dest_MAC */
	PKT_HASH_TYPE_L3,	/* Input: src_IP, dst_IP */
	PKT_HASH_TYPE_L4,	/* Input: src_IP, dst_IP, src_port, dst_port */
};

static inline void
skb_set_hash(struct sk_buff *skb, __u32 hash, enum pkt_hash_types type)
{
#if LINUX_VERSION_CODE >= KERNEL_VERSION(3,2,0) /* 4031ae6edb */
	skb->l4_rxhash = (type == PKT_HASH_TYPE_L4);
#endif
#if LINUX_VERSION_CODE >= KERNEL_VERSION(3,4,0) /* bdeab99191 */
	skb->rxhash = hash;
#endif
}
#endif /* LINUX_VERSION_CODE < KERNEL_VERSION(3,14,0) */

#endif /* __BACKPORT_SKBUFF_H */
