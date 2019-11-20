// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
// sostream.h
//

#ifndef SOSTREAM_H_5323DC8C26E181D43278F2F53FDCF19F
#define SOSTREAM_H_5323DC8C26E181D43278F2F53FDCF19F

#include "mostream.h"
#include "uios.h"
#include <stdarg.h>

namespace ustl {

class string;

/// \class ostringstream sostream.h ustl.h
/// \ingroup TextStreams
///
/// \brief This stream writes textual data into a memory block.
///
class ostringstream : public ostream {
public:
				ostringstream (void);
				ostringstream (void* p, size_type n);
    explicit			ostringstream (string& dest);
    explicit			ostringstream (memlink& dest);
    void			iwrite (uint8_t v);
    void			iwrite (wchar_t v);
    void			iwrite (int v);
    void			iwrite (unsigned int v);
    void			iwrite (long int v);
    void			iwrite (unsigned long int v);
    void			iwrite (float v);
    void			iwrite (double v);
    void			iwrite (bool v);
    void			iwrite (const char* s);
    void			iwrite (const string& v);
    void			iwrite (ios::fmtflags f);
#if HAVE_LONG_LONG
    void			iwrite (long long v);
    void			iwrite (unsigned long long v);
#endif
    int				vformat (const char* fmt, va_list args);
    int				format (const char* fmt, ...) __attribute__((__format__(__printf__, 2, 3)));
    inline void			set_base (uint16_t b)		{ m_Base = b; }
    inline void			set_width (uint16_t w)		{ m_Width = w; }
    inline void			set_decimal_separator (char c)	{ m_DecimalSeparator = c; }
    inline void			set_thousand_separator (char c)	{ m_ThousandSeparator = c; }
    inline void			set_precision (uint16_t v)	{ m_Precision = v; }
    inline void			link (void* p, size_type n)	{ ostream::link (p, n); }
    inline void			link (memlink& l)		{ ostream::link (l); }
    void			link (string& l);
    virtual void		unlink (void);
    void			write (const void* buffer, size_type size);
    void			write (const cmemlink& buf);
    inline void			write_strz (const char*)	{ assert (!"Writing nul characters into a text stream is not allowed"); }
    virtual size_type		overflow (size_type n = 1);
protected:
    void			write_buffer (const char* buf, size_type bufSize);
private:
    inline char*		encode_dec (char* fmt, uint32_t n) const;
    void			fmtstring (char* fmt, const char* typestr, bool bInteger) const;
    template <typename T>
    inline void			sprintf_iwrite (T v, const char* typestr);
private:
    string*			m_pResizable;		///< Pointer to the buffer, if resizable.
    uint32_t			m_Flags;		///< See ios::fmtflags.
    uint16_t			m_Base;			///< Numeric base for writing numbers.
    uint16_t			m_Precision;		///< Number of digits after the decimal separator.
    uint16_t			m_Width;		///< Field width.
    char			m_DecimalSeparator;	///< Period by default.
    char			m_ThousandSeparator;	///< Comma by default.
};

#define OSTRSTREAM_OPERATOR(RealT, CastT)			\
inline ostringstream& operator<< (ostringstream& os, RealT v)	\
{ os.iwrite ((CastT) v); return (os); }

template <typename T>
OSTRSTREAM_OPERATOR (T*,		unsigned long int)
OSTRSTREAM_OPERATOR (const void*,	unsigned long int)
OSTRSTREAM_OPERATOR (void*,		unsigned long int)
OSTRSTREAM_OPERATOR (const char*,	const char*)
OSTRSTREAM_OPERATOR (char*,		const char*)
OSTRSTREAM_OPERATOR (uint8_t*,		const char*)
OSTRSTREAM_OPERATOR (const uint8_t*,	const char*)
OSTRSTREAM_OPERATOR (const string&,	const string&)
OSTRSTREAM_OPERATOR (ios::fmtflags,	ios::fmtflags)
OSTRSTREAM_OPERATOR (int8_t,		uint8_t)
OSTRSTREAM_OPERATOR (uint8_t,		uint8_t)
OSTRSTREAM_OPERATOR (short int,		int)
OSTRSTREAM_OPERATOR (unsigned short,	unsigned int)
OSTRSTREAM_OPERATOR (int,		int)
OSTRSTREAM_OPERATOR (unsigned int,	unsigned int)
OSTRSTREAM_OPERATOR (long,		long)
OSTRSTREAM_OPERATOR (unsigned long,	unsigned long)
OSTRSTREAM_OPERATOR (float,		float)
OSTRSTREAM_OPERATOR (double,		double)
OSTRSTREAM_OPERATOR (bool,		bool)
OSTRSTREAM_OPERATOR (wchar_t,		wchar_t)
#if HAVE_THREE_CHAR_TYPES
OSTRSTREAM_OPERATOR (char,		uint8_t)
#endif
#if HAVE_LONG_LONG
OSTRSTREAM_OPERATOR (long long,		long long)
OSTRSTREAM_OPERATOR (unsigned long long, unsigned long long)
#endif

} // namespace ustl

#endif

