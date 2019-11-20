// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
// uexception.h
//
// This file contains stuff from \<exception\>.
// The standard C++ headers are duplicated because uSTL is intended
// to completely replace all C++ standard library functions.
//

#ifndef UEXCEPTION_H_18DE3EF55C4F00673268F0D66546AF5D
#define UEXCEPTION_H_18DE3EF55C4F00673268F0D66546AF5D

#include "utypes.h"
#ifndef WITHOUT_LIBSTDCPP
    #include <exception>
    #include <new>
#endif

#ifdef WITHOUT_LIBSTDCPP	// This code is copied from <exception>
namespace std {
/// If you write a replacement terminate handler, it must be of this type.
typedef void (*terminate_handler) (void);
/// If you write a replacement unexpected handler, it must be of this type.
typedef void (*unexpected_handler) (void);
/// Takes a new handler function as an argument, returns the old function.
terminate_handler set_terminate (terminate_handler pHandler) throw();
/// The runtime will call this function if exception handling must be
/// abandoned for any reason.  It can also be called by the user.
void terminate (void) __attribute__ ((__noreturn__));
/// Takes a new handler function as an argument, returns the old function.
unexpected_handler set_unexpected (unexpected_handler pHandler) throw();
/// The runtime will call this function if an exception is thrown which
/// violates the function's exception specification.
void unexpected (void) __attribute__ ((__noreturn__));
/// Returns true when the caught exception violates the throw specification.
bool uncaught_exception() throw();
} // namespace std
#endif

namespace ustl {

class string;
class istream;
class ostream;
class ostringstream;

typedef uint32_t	xfmt_t;

static const xfmt_t	xfmt_Exception			= 0;
static const xfmt_t	xfmt_BadAlloc			= 1;
static const xfmt_t	xfmt_LibcException		= 12;
static const xfmt_t	xfmt_FileException		= 13;
static const xfmt_t	xfmt_StreamBoundsException	= 14;

/// \class exception uexception.h ustl.h
/// \ingroup Exceptions
///
/// \brief Base class for exceptions, equivalent to std::exception.
///
#ifdef WITHOUT_LIBSTDCPP
class exception {
#else
class exception : public std::exception {
#endif
public:
    			exception (void) throw();
    virtual	       ~exception (void) throw();
    virtual const char*	what (void) const throw();
    virtual void	info (string& msgbuf, const char* fmt = NULL) const throw();
    virtual void	read (istream& is);
    virtual void	write (ostream& os) const;
    void		text_write (ostringstream& os) const;
    virtual size_t	stream_size (void) const;
    /// Format of the exception is used to lookup exception::info format string.
    /// Another common use is the instantiation of serialized exceptions, used
    /// by the error handler node chain to troubleshoot specific errors.
    inline xfmt_t	format (void) const { return (m_Format); }
protected:
    inline void		set_format (xfmt_t fmt) { m_Format = fmt; }
private:
    xfmt_t		m_Format;	///< Format of the exception's data.
};

/// \class bad_cast uexception.h ustl.h
/// \ingroup Exceptions
///
/// \brief Thrown to indicate a bad dynamic_cast usage.
///
class bad_cast : public exception {
public:
    explicit		bad_cast (void) throw();
    virtual const char*	what (void) const throw();
};

/// \class bad_alloc uexception.h ustl.h
/// \ingroup Exceptions
///
/// \brief Exception thrown on memory allocation failure by memblock::reserve.
///
#ifdef WITHOUT_LIBSTDCPP
class bad_alloc : public exception {
#else
class bad_alloc : public std::bad_alloc, public exception {
#endif
public:
    explicit		bad_alloc (size_t nBytes = 0) throw();
    virtual const char*	what (void) const throw();
    virtual void	info (string& msgbuf, const char* fmt = NULL) const throw();
    virtual void	read (istream& is);
    virtual void	write (ostream& os) const;
    virtual size_t	stream_size (void) const;
protected:
    size_t		m_nBytesRequested;	///< Number of bytes requested by the failed allocation.
};

/// \class libc_exception uexception.h ustl.h
/// \ingroup Exceptions
///
/// \brief Thrown when a libc function returns an error.
///
/// Contains an errno and description. This is a uSTL extension.
///
class libc_exception : public exception {
public:
    explicit		libc_exception (const char* operation) throw();
			libc_exception (const libc_exception& v) throw();
    const libc_exception& operator= (const libc_exception& v);
    virtual const char*	what (void) const throw();
    virtual void	info (string& msgbuf, const char* fmt = NULL) const throw();
    virtual void	read (istream& is);
    virtual void	write (ostream& os) const;
    virtual size_t	stream_size (void) const;
protected:
    int			m_Errno;		///< Error code returned by the failed operation.
    const char*		m_Operation;		///< Name of the failed operation.
};

/// \class file_exception uexception.h ustl.h
/// \ingroup Exceptions
///
/// \brief File-related exceptions.
///
/// Contains the file name. This is a uSTL extension.
///
class file_exception : public libc_exception {
public:
			file_exception (const char* operation, const char* filename) throw();
    virtual const char*	what (void) const throw();
    virtual void	info (string& msgbuf, const char* fmt = NULL) const throw();
    virtual void	read (istream& is);
    virtual void	write (ostream& os) const;
    virtual size_t	stream_size (void) const;
protected:
    char		m_Filename [PATH_MAX];	///< Name of the file causing the error.
};

/// \class stream_bounds_exception uexception.h ustl.h
/// \ingroup Exceptions
///
/// \brief Stream bounds checking.
///
/// Only thrown in debug builds unless you say otherwise in config.h
/// This is a uSTL extension.
///
class stream_bounds_exception : public libc_exception {
public:
			stream_bounds_exception (const char* operation, const char* type, uoff_t offset, size_t expected, size_t remaining) throw();
    virtual const char*	what (void) const throw();
    virtual void	info (string& msgbuf, const char* fmt = NULL) const throw();
    virtual void	read (istream& is);
    virtual void	write (ostream& os) const;
    virtual size_t	stream_size (void) const;
protected:
    const char*		m_TypeName;
    uoff_t		m_Offset;
    size_t		m_Expected;
    size_t		m_Remaining;
};

} // namespace ustl

#endif

