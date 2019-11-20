// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
// ulimits.h
//

#ifndef ULIMITS_H_1C2192EA3821E0811BBAF86B0F048364
#define ULIMITS_H_1C2192EA3821E0811BBAF86B0F048364

namespace ustl {

/// \class numeric_limits ulimits.h ustl.h
/// \brief Defines numeric limits for a type.
///
template <typename T> 
struct numeric_limits {
    /// Returns the minimum value for type T.
    static inline T min (void)		{ return (T(0)); }
    /// Returns the minimum value for type T.
    static inline T max (void)		{ return (T(0)); }
    static const bool is_signed = false;	///< True if the type is signed.
    static const bool is_integer = false;	///< True if stores an exact value.
    static const bool is_integral = false;	///< True if fixed size and cast-copyable.
};

#ifndef DOXYGEN_SHOULD_SKIP_THIS

#define _NUMERIC_LIMITS(type, minVal, maxVal, bSigned, bInteger, bIntegral)	\
template <>							\
struct numeric_limits<type> {					\
    static inline type min (void)	{ return (minVal); }	\
    static inline type max (void)	{ return (maxVal); }	\
    static const bool is_signed = bSigned;			\
    static const bool is_integer = bInteger;			\
    static const bool is_integral = bIntegral;			\
}

//--------------------------------------------------------------------------------------
//		type		min		max		signed	integer	integral
//--------------------------------------------------------------------------------------
_NUMERIC_LIMITS (bool,		false,		true,		false,	true,	true);
_NUMERIC_LIMITS (char,		SCHAR_MIN,	SCHAR_MAX,	true,	true,	true);
_NUMERIC_LIMITS (int,		INT_MIN,	INT_MAX,	true,	true,	true);
_NUMERIC_LIMITS (short,		SHRT_MIN,	SHRT_MAX,	true,	true,	true);
_NUMERIC_LIMITS (long,		LONG_MIN,	LONG_MAX,	true,	true,	true);
_NUMERIC_LIMITS (unsigned char,	0,		UCHAR_MAX,	false,	true,	true);
_NUMERIC_LIMITS (unsigned int,	0,		UINT_MAX,	false,	true,	true);
_NUMERIC_LIMITS (unsigned short,0,		USHRT_MAX,	false,	true,	true);
_NUMERIC_LIMITS (unsigned long,	0,		ULONG_MAX,	false,	true,	true);
_NUMERIC_LIMITS (wchar_t,	0,		WCHAR_MAX,	false,	true,	true);
_NUMERIC_LIMITS (float,		FLT_MIN,	FLT_MAX,	true,	false,	true);
_NUMERIC_LIMITS (double,	DBL_MIN,	DBL_MAX,	true,	false,	true);
_NUMERIC_LIMITS (long double,	LDBL_MIN,	LDBL_MAX,	true,	false,	true);
#ifdef HAVE_LONG_LONG
_NUMERIC_LIMITS (long long,	LLONG_MIN,	LLONG_MAX,	true,	true,	true);
_NUMERIC_LIMITS (unsigned long long,	0,	ULLONG_MAX,	false,	true,	true);
#endif
//--------------------------------------------------------------------------------------

#endif // DOXYGEN_SHOULD_SKIP_THIS

/// Macro for defining numeric_limits specializations
#define NUMERIC_LIMITS(type, minVal, maxVal, bSigned, bInteger, bIntegral)	\
namespace ustl { _NUMERIC_LIMITS (type, minVal, maxVal, bSigned, bInteger, bIntegral); }

/// Standard container NUMERIC_LIMITS
#define CONTAINER_NUMERIC_LIMITS(type)					\
namespace ustl {							\
    template <>								\
    struct numeric_limits<type> {					\
	typedef numeric_limits<type::value_type> value_limits;		\
	static inline type min (void)	{ type v; fill (v, value_limits::min()); return (v); }	\
	static inline type max (void)	{ type v; fill (v, value_limits::max()); return (v); }	\
	static const bool is_signed = value_limits::is_signed;		\
	static const bool is_integer = value_limits::is_integer;	\
	static const bool is_integral = value_limits::is_integral;	\
    };									\
}

/// Standard tuple NUMERIC_LIMITS
#define TUPLE_NUMERIC_LIMITS(type)	\
CONTAINER_NUMERIC_LIMITS(type)		\
namespace ustl {			\
    template <> inline size_t alignof (type) { return (alignof (type::value_type())); }; \
}

} // namespace ustl

#endif

