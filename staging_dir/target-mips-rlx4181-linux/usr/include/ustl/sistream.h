// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
// sistream.h
//

#ifndef SISTREAM_H_0CCA102229A49F5D65EE852E62B27CE2
#define SISTREAM_H_0CCA102229A49F5D65EE852E62B27CE2

#include "mistream.h"

namespace ustl {

class string;

/// \class istringstream sistream.h ustl.h
/// \ingroup TextStreams
///
/// \brief A stream that reads textual data from a memory block.
///
class istringstream : public istream {
public:
    static const size_type	c_MaxDelimiters = 16;	///< Maximum number of word delimiters.
public:
    				istringstream (void);
				istringstream (const void* p, size_type n);
    explicit			istringstream (const cmemlink& source);
    void			iread (int8_t& v);
    void			iread (int32_t& v);
    void			iread (double& v);
    void			iread (bool& v);
    void			iread (wchar_t& v);
    void			iread (string& v);
#ifdef HAVE_INT64_T
    void			iread (int64_t& v);
#endif
#if HAVE_LONG_LONG && (!HAVE_INT64_T || SIZE_OF_LONG_LONG > 8)
    void			iread (long long& v);
#endif
    void			set_delimiters (const char* delimiters);
    inline void			set_base (short base);
    inline void			set_decimal_separator (char s);
    inline void			set_thousand_separator (char s);
    void			read (void* buffer, size_type size);
    void			read (memlink& buf);
    inline void			read_strz (string& str);
protected:
    char			skip_delimiters (void);
private:
    inline bool			is_delimiter (char c) const;
private:
    char			m_Delimiters [c_MaxDelimiters];
    uint16_t			m_Base;
    char			m_DecimalSeparator;
    char			m_ThousandSeparator;
};

/// Sets the numeric base used to read numbers.
inline void istringstream::set_base (short base)
{
    m_Base = base;
}

/// Sets the decimal separator to recognize in numbers. Period by default.
inline void istringstream::set_decimal_separator (char s)
{
    m_DecimalSeparator = s;
}

/// Sets the thousand separator to recognize in numbers. Comma by default.
inline void istringstream::set_thousand_separator (char s)
{
    m_ThousandSeparator = s;
}

/// Reads a null-terminated character stream. This is not allowed in this class.
inline void istringstream::read_strz (string&)
{
    assert (!"Reading nul characters is not allowed from text streams");
}

/// Reads one type as another.
template <typename RealT, typename CastT>
void _cast_read (istringstream& is, RealT& v)
{
    CastT cv;
    is.iread (cv);
    v = RealT (cv);
}

inline istringstream& operator>> (istringstream& is, int8_t& v)	{ is.iread (v); return (is); }
inline istringstream& operator>> (istringstream& is, int32_t& v){ is.iread (v); return (is); }
inline istringstream& operator>> (istringstream& is, double& v)	{ is.iread (v); return (is); }
inline istringstream& operator>> (istringstream& is, bool& v)	{ is.iread (v); return (is); }
inline istringstream& operator>> (istringstream& is, wchar_t& v){ is.iread (v); return (is); }
inline istringstream& operator>> (istringstream& is, string& v)	{ is.iread (v); return (is); }
#if HAVE_INT64_T
inline istringstream& operator>> (istringstream& is, int64_t& v){ is.iread (v); return (is); }
#endif
#if HAVE_LONG_LONG && (!HAVE_INT64_T || SIZE_OF_LONG_LONG > 8)
inline istringstream& operator>> (istringstream& is, long long& v) { is.iread (v); return (is); }
#endif

#define ISTRSTREAM_CAST_OPERATOR(RealT, CastT)			\
inline istringstream& operator>> (istringstream& is, RealT& v)	\
{ _cast_read<RealT,CastT>(is, v); return (is); }

ISTRSTREAM_CAST_OPERATOR (uint8_t,	int8_t)
ISTRSTREAM_CAST_OPERATOR (int16_t,	int32_t)
ISTRSTREAM_CAST_OPERATOR (uint16_t,	int32_t)
ISTRSTREAM_CAST_OPERATOR (uint32_t,	int32_t)
ISTRSTREAM_CAST_OPERATOR (float,	double)
#if HAVE_THREE_CHAR_TYPES
ISTRSTREAM_CAST_OPERATOR (char,		int8_t)
#endif
#if HAVE_INT64_T
ISTRSTREAM_CAST_OPERATOR (uint64_t,	int64_t)
#endif
#if SIZE_OF_LONG == SIZE_OF_INT
ISTRSTREAM_CAST_OPERATOR (long,		int)
ISTRSTREAM_CAST_OPERATOR (unsigned long,int)
#endif
#if HAVE_LONG_LONG && (!HAVE_INT64_T || SIZE_OF_LONG_LONG > 8)
ISTRSTREAM_CAST_OPERATOR (unsigned long long, long long)
#endif
#undef ISTRSTREAM_CAST_OPERATOR

} // namespace ustl

#endif

