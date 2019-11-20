/* Copyright (C) 1991,92,93,95,96,97,98,99,2001,02
	Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

/*
 *	ISO C99 Standard 7.4: Character handling	<ctype.h>
 */

#ifndef	_CTYPE_H
#define	_CTYPE_H	1

#include <features.h>
#include <bits/types.h>

__BEGIN_DECLS
__BEGIN_NAMESPACE_STD

/* The following names are all functions:
     int isCHARACTERISTIC(int c);
   which return nonzero iff C has CHARACTERISTIC.
   For the meaning of the characteristic names, see the `enum' above.  */
extern int isalnum(int __c) __THROW;
extern int isalpha(int __c) __THROW;
extern int iscntrl(int __c) __THROW;
extern int isdigit(int __c) __THROW;
extern int islower(int __c) __THROW;
extern int isgraph(int __c) __THROW;
extern int isprint(int __c) __THROW;
extern int ispunct(int __c) __THROW;
extern int isspace(int __c) __THROW;
extern int isupper(int __c) __THROW;
extern int isxdigit(int __c) __THROW;


/* Return the lowercase version of C.  */
extern int tolower(int __c) __THROW;

/* Return the uppercase version of C.  */
extern int toupper(int __c) __THROW;

#if (defined __USE_SVID || defined __USE_MISC || defined __USE_XOPEN) && \
	defined __UCLIBC_SUSV4_LEGACY__
/* Return nonzero iff C is in the ASCII set
   (i.e., is no more than 7 bits wide).  */
extern int isascii(int __c) __THROW;
/* Return the part of C that is in the ASCII set
   (i.e., the low-order 7 bits of C).  */
extern int toascii(int __c) __THROW;
#endif

__END_NAMESPACE_STD


/* ISO C99 introduced one new function.  */
#ifdef	__USE_ISOC99
__BEGIN_NAMESPACE_C99

extern int isblank(int __c) __THROW;

__END_NAMESPACE_C99
#endif
__END_DECLS

__BEGIN_DECLS

#ifndef _ISbit
/* These are all the characteristics of characters.
   If there get to be more than 16 distinct characteristics,
   __ctype_mask_t will need to be adjusted. */

/* libstdc++ from gcc toolchain needs this macro. */
# define _ISbit(bit)	(1 << (bit))

enum
{
  _ISupper = _ISbit (0),	/* UPPERCASE.  */
  _ISlower = _ISbit (1),	/* lowercase.  */
  _ISalpha = _ISbit (2),	/* Alphabetic.  */
  _ISdigit = _ISbit (3),	/* Numeric.  */
  _ISxdigit = _ISbit (4),	/* Hexadecimal numeric.  */
  _ISspace = _ISbit (5),	/* Whitespace.  */
  _ISprint = _ISbit (6),	/* Printing.  */
  _ISgraph = _ISbit (7),	/* Graphical.  */
  _ISblank = _ISbit (8),	/* Blank (usually SPC and TAB).  */
  _IScntrl = _ISbit (9),	/* Control character.  */
  _ISpunct = _ISbit (10),	/* Punctuation.  */
  _ISalnum = _ISbit (11)	/* Alphanumeric.  */
};
#else
#error _ISbit already defined!
#endif /* ! _ISbit  */

/* __ctype_XXX_t types and __UCLIBC_CTYPE_x_TBL_OFFSET constants */
#include <bits/uClibc_touplow.h>

# define __UCLIBC_CTYPE_IN_TO_DOMAIN(c) (((unsigned int)((c) + 128)) < 384)

/* In the thread-specific locale model (see `uselocale' in <locale.h>)
   we cannot use global variables for these as was done in the past.
   Instead, the following accessor functions return the address of
   each variable, which is local to the current thread if multithreaded.

   These point into arrays of 384, so they can be indexed by any `unsigned
   char' value [0,255]; by EOF (-1); or by any `signed char' value
   [-128,-1).  ISO C requires that the ctype functions work for `unsigned
   char' values and for EOF; we also support negative `signed char' values
   for broken old programs.  The case conversion arrays are of `int's
   rather than `unsigned char's because tolower (EOF) must be EOF, which
   doesn't fit into an `unsigned char'.  But today more important is that
   the arrays are also used for multi-byte character sets.  */

/* uClibc differences:
 *
 * When __UCLIBC_HAS_CTYPE_SIGNED is defined,
 *
 *    The upper and lower mapping arrays are type int16_t, so that
 *    they may store all char values plus EOF.  The glibc reasoning
 *    given above for these being type int is questionable, as the
 *    ctype mapping functions map from the set of (unsigned) char
 *    and EOF back into the set.  They have no awareness of multi-byte
 *    or wide characters.
 *
 * Otherwise,
 *
 *    The ctype array is defined for -1..255.
 *    The upper and lower mapping arrays are defined for 0..255.
 *    The upper and lower mapping arrays are type unsigned char.
 */

/* Pointers to the default C-locale data. */
extern const __ctype_mask_t *__C_ctype_b;
extern const __ctype_touplow_t *__C_ctype_toupper;
extern const __ctype_touplow_t *__C_ctype_tolower;

/* Pointers to the current global locale data in use. */
extern const __ctype_mask_t *__ctype_b;
extern const __ctype_touplow_t *__ctype_toupper;
extern const __ctype_touplow_t *__ctype_tolower;
#define __UCLIBC_CTYPE_B	(__ctype_b)
#define __UCLIBC_CTYPE_TOLOWER	(__ctype_tolower)
#define __UCLIBC_CTYPE_TOUPPER	(__ctype_toupper)

#define	__isascii(c)	(((c) & ~0x7f) == 0)	/* If C is a 7 bit value.  */
#define	__toascii(c)	((c) & 0x7f)		/* Mask off high bits.  */

#ifdef __USE_GNU
/* Test C for a set of character classes according to MASK.  */
extern int isctype(int __c, int __mask) __THROW;
#endif

#if (defined __USE_SVID || defined __USE_MISC || defined __USE_XOPEN) \
	&& defined __UCLIBC_SUSV4_LEGACY__
/* These are the same as `toupper' and `tolower' except that they do not
   check the argument for being in the range of a `char'.  */
extern int _toupper(int __c) __THROW;
extern int _tolower(int __c) __THROW;
#endif /* Use SVID or use misc.  */

/* This code is needed for the optimized mapping functions.  */
#define __tobody(c, f, table, args) \
(__extension__ ({ \
	int __res; \
	if (sizeof(c) > 1) { \
		if (__builtin_constant_p(c)) { \
			int __c = (c); \
			__res = __UCLIBC_CTYPE_IN_TO_DOMAIN(__c) ? (table)[__c] : __c; \
		} else \
			__res = f args; \
	} else \
		__res = (table)[(int) (c)]; \
	__res; \
}))

#define __isctype(c, type) ((__UCLIBC_CTYPE_B)[(int)(c)] & (__ctype_mask_t)type)
/* Do not combine in one #if - unifdef tool is not that clever */
#ifndef __cplusplus
# define isalnum(c)	__isctype((c), _ISalnum)
# define isalpha(c)	__isctype((c), _ISalpha)
# define iscntrl(c)	__isctype((c), _IScntrl)
# define isdigit(c)	__isctype((c), _ISdigit)
# define islower(c)	__isctype((c), _ISlower)
# define isgraph(c)	__isctype((c), _ISgraph)
# define isprint(c)	__isctype((c), _ISprint)
# define ispunct(c)	__isctype((c), _ISpunct)
# define isspace(c)	__isctype((c), _ISspace)
# define isupper(c)	__isctype((c), _ISupper)
# define isxdigit(c)	__isctype((c), _ISxdigit)
# ifdef __USE_ISOC99
#  define isblank(c)	__isctype((c), _ISblank)
# endif

# ifdef __USE_EXTERN_INLINES
__extern_inline int
__NTH (tolower (int __c))
{
  return __UCLIBC_CTYPE_IN_TO_DOMAIN(__c) ? (__UCLIBC_CTYPE_TOLOWER)[__c] : __c;
}
__extern_inline int
__NTH (toupper (int __c))
{
  return __UCLIBC_CTYPE_IN_TO_DOMAIN(__c) ? (__UCLIBC_CTYPE_TOUPPER)[__c] : __c;
}
# endif

# if __GNUC__ >= 2 && defined __OPTIMIZE__ && !defined __cplusplus
#  define tolower(c)	__tobody(c, tolower, __UCLIBC_CTYPE_TOLOWER, (c))
#  define toupper(c)	__tobody(c, toupper, __UCLIBC_CTYPE_TOUPPER, (c))
# endif /* Optimizing gcc */

# if (defined __USE_SVID || defined __USE_MISC || defined __USE_XOPEN) \
	&& defined __UCLIBC_SUSV4_LEGACY__
#  define isascii(c)	__isascii (c)
#  define toascii(c)	__toascii (c)
#  define _tolower(c)	((int) (__UCLIBC_CTYPE_TOLOWER)[(int) (c)])
#  define _toupper(c)	((int) (__UCLIBC_CTYPE_TOUPPER)[(int) (c)])
# endif

#endif /* not __cplusplus */


__END_DECLS

#endif /* ctype.h  */
