#ifndef __BACKPORT_RANDOM_H
#define __BACKPORT_RANDOM_H
#include_next <linux/random.h>
#include <linux/version.h>

#if LINUX_VERSION_CODE < KERNEL_VERSION(3,8,0)
/* backports 496f2f9 */
#define prandom_seed(_seed)		srandom32(_seed)
#define prandom_u32()			random32()
#define prandom_u32_state(_state)	prandom32(_state)
/* backport 6582c665d6b882dad8329e05749fbcf119f1ab88 */
#define prandom_bytes LINUX_BACKPORT(prandom_bytes)
void prandom_bytes(void *buf, int bytes);
#endif

#endif /* __BACKPORT_RANDOM_H */
