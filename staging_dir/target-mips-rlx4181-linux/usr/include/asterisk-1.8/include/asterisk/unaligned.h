/*
 * Asterisk -- An open source telephony toolkit.
 *
 * Copyright (C) 1999 - 2009, Digium, Inc.
 *
 * Mark Spencer <markster@digium.com>
 *
 * See http://www.asterisk.org for more information about
 * the Asterisk project. Please do not directly contact
 * any of the maintainers of this project for assistance;
 * the project provides a web site, mailing lists and IRC
 * channels for your use.
 *
 * This program is free software, distributed under the terms of
 * the GNU General Public License Version 2. See the LICENSE file
 * at the top of the source tree.
 */

/*! \file
 * \brief Handle unaligned data access
 */

#ifndef _ASTERISK_UNALIGNED_H
#define _ASTERISK_UNALIGNED_H

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

#ifdef __GNUC__
/* If we just tell GCC what's going on, we can trust it to behave optimally */
static inline uint64_t get_unaligned_uint64(const void *p)
{
	const struct { uint64_t d; } __attribute__((packed)) *pp = p;
	return pp->d;
}

static inline unsigned int get_unaligned_uint32(const void *p)
{
	const struct { unsigned int d; } __attribute__((packed)) *pp = p;

	return pp->d;
}
static inline unsigned short get_unaligned_uint16(const void *p)
{
	const struct { unsigned short d; } __attribute__((packed)) *pp = p;

	return pp->d;
}

static inline void put_unaligned_uint64(void *p, uint64_t datum)
{
	struct { uint64_t d; } __attribute__((packed,may_alias)) *pp = p;

	pp->d = datum;
}

static inline void put_unaligned_uint32(void *p, unsigned int datum)
{
	struct { unsigned int d; } __attribute__((packed)) *pp = p;

	pp->d = datum;
}

static inline void put_unaligned_uint16(void *p, unsigned short datum)
{
	struct { unsigned short d; } __attribute__((packed)) *pp = p;

	pp->d = datum;
}
#elif defined(SOLARIS) && defined(__sparc__)
static inline uint64_t get_unaligned_uint64(const void *p)
{
	const unsigned char *cp = p;

	return
		(((uint64_t) cp[0]) << 56) |
		(((uint64_t) cp[1]) << 48) |
		(((uint64_t) cp[2]) << 40) |
		(((uint64_t) cp[3]) << 32) |
		(((uint64_t) cp[4]) << 24) |
		(((uint64_t) cp[5]) << 16) |
		(((uint64_t) cp[6]) <<  8) |
		(((uint64_t) cp[7]) <<  0);
}

static inline unsigned int get_unaligned_uint32(const void *p)
{
	const unsigned char *cp = p;

	return (cp[0] << 24) | (cp[1] << 16) | (cp[2] << 8) | cp[3];
}

static inline unsigned short get_unaligned_uint16(const void *p)
{
	const unsigned char *cp = p;

	return (cp[0] << 8) | cp[1] ;
}

static inline void put_unaligned_uint64(void *p, uint64_t datum)
{
	unsigned char *cp = p;

	cp[0] = (datum >> 56) & 0xff;
	cp[1] = (datum >> 48) & 0xff;
	cp[2] = (datum >> 40) & 0xff;
	cp[3] = (datum >> 32) & 0xff;
	cp[4] = (datum >> 24) & 0xff;
	cp[5] = (datum >> 16) & 0xff;
	cp[6] = (datum >>  8) & 0xff;
	cp[7] = (datum >>  0) & 0xff;
}

static inline void put_unaligned_uint32(void *p, unsigned int datum)
{
	unsigned char *cp = p;

	cp[0] = datum >> 24;
	cp[1] = datum >> 16;
	cp[2] = datum >> 8;
	cp[3] = datum;
}

static inline void put_unaligned_uint16(void *p, unsigned int datum)
{
	unsigned char *cp = p;

	cp[0] = datum >> 8;
	cp[1] = datum;
}
#else /* Not GCC, not Solaris/SPARC. Assume we can handle direct load/store. */
#define get_unaligned_uint64(p) (*((uint64_t *)(p)))
#define get_unaligned_uint32(p) (*((unsigned int *)(p)))
#define get_unaligned_uint16(p) (*((unsigned short *)(p)))
#define put_unaligned_uint64(p,d) do { uint64_t *__P = (p); *__P = d; } while(0)
#define put_unaligned_uint32(p,d) do { unsigned int *__P = (p); *__P = d; } while(0)
#define put_unaligned_uint16(p,d) do { unsigned short *__P = (p); *__P = d; } while(0)
#endif

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif


#endif /* _ASTERISK_UNALIGNED_H */
