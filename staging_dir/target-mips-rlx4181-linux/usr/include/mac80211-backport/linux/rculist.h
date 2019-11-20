#ifndef __BACKPORT_RCULIST_H
#define __BACKPORT_RCULIST_H
#include_next <linux/rculist.h>

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,9,0)
#include <backport/magic.h>
#define hlist_for_each_entry_rcu4(tpos, pos, head, member)		\
	for (pos = rcu_dereference_raw(hlist_first_rcu(head));		\
	     pos &&							\
		({ tpos = hlist_entry(pos, typeof(*tpos), member); 1; });\
	     pos = rcu_dereference_raw(hlist_next_rcu(pos)))

#define hlist_for_each_entry_rcu3(pos, head, member)				\
	for (pos = hlist_entry_safe (rcu_dereference_raw(hlist_first_rcu(head)),\
			typeof(*(pos)), member);				\
		pos;								\
		pos = hlist_entry_safe(rcu_dereference_raw(hlist_next_rcu(	\
			&(pos)->member)), typeof(*(pos)), member))

#undef hlist_for_each_entry_rcu
#define hlist_for_each_entry_rcu(...) \
	macro_dispatcher(hlist_for_each_entry_rcu, __VA_ARGS__)(__VA_ARGS__)
#endif /* < 3.9 */

#ifndef list_for_each_entry_continue_rcu
#define list_for_each_entry_continue_rcu(pos, head, member) 		\
	for (pos = list_entry_rcu(pos->member.next, typeof(*pos), member); \
	     prefetch(pos->member.next), &pos->member != (head);	\
	     pos = list_entry_rcu(pos->member.next, typeof(*pos), member))
#endif

#ifndef list_entry_rcu
#define list_entry_rcu(ptr, type, member) \
	container_of(rcu_dereference(ptr), type, member)
#endif

#endif /* __BACKPORT_RCULIST_H */
