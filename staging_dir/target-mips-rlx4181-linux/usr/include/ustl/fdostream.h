// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
// fdostringstream.h
//

#ifndef FDOSTREAM_H_5E27FC3D530BF3CA04D6C73F5700EECC
#define FDOSTREAM_H_5E27FC3D530BF3CA04D6C73F5700EECC

#include "sistream.h"
#include "sostream.h"
#include "ustring.h"

#ifndef WITHOUT_CIN_COUT_CERR
namespace ustl {

class string;

/// \class fdostringstream fdostream.h ustl.h
/// \ingroup DeviceStreams
/// \brief A string stream that writes to an fd. Implements cout and cerr.
class fdostringstream : public ostringstream {
public:
    explicit			fdostringstream (int fd);
    virtual		       ~fdostringstream (void);
    void			flush (void);
    virtual size_type		overflow (size_type n = 1);
    inline virtual bool		eof (void) const	{ return (m_bEOF); }
private:
    string			m_Buffer;
    int				m_Fd;
    bool			m_bEOF;
};

/// \class fdistringstream fdostream.h ustl.h
/// \ingroup DeviceStreams
/// \brief A string stream that reads from an fd. Implements cin.
///
/// I would discourage the use of cin in general. As a quick-n-dirty
/// hack to read a configuration file it is ok, but if you want to
/// do real user input, it becomes much less acceptable. The problem
/// is not really the implementation, but the way it is used, expecting
/// the completely unverified information coming from the user to be
/// magically converted to numbers, strings, etc. Bounds checking and
/// filter adaptors like utf8 are damn hard to do directly over cin.
/// Furthermore, you are sitting there, blocking on user input, when
/// you could be doing something useful. Instead, implement a nonblocking
/// block reading routine (I cannot provide you with one, since it
/// depends on your event framework), which reads all available data
/// and sends it through a series of validation filters. UTF8 format
/// checking should be done this way for maximum simplicity. Then later
/// you can use istringstream directly on the buffer, which allows you
/// to restart reading in case of errors, for instance, whereas cin
/// will simply drop the offending characters (this implementation will
/// keep some, but you are not supposed to know that). Anyway, this
/// class is here only because it is really very very small and does
/// next to nothing.
///
class fdistringstream : public istringstream {
public:
    explicit			fdistringstream (int fd);
    virtual size_type		underflow (size_type n = 1);
    inline virtual bool		eof (void) const	{ return (m_bEOF); }
private:
    string			m_Buffer;
    int				m_Fd;
    bool			m_bEOF;
};

extern fdostringstream cout, cerr;
extern fdistringstream cin;

} // namespace ustl

#endif
#endif

