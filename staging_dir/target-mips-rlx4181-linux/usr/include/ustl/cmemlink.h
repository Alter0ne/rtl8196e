// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
// cmemlink.h
//

#ifndef CMEMLINK_H_7CFAB32C5C6732ED29B34EF00EA40A12
#define CMEMLINK_H_7CFAB32C5C6732ED29B34EF00EA40A12

#include "ualgobase.h"

/// The ustl namespace contains all ustl classes and algorithms.
namespace ustl {

class istream;
class ostream;

/// \class cmemlink cmemlink.h ustl.h
/// \ingroup MemoryManagement
///
/// \brief A read-only pointer to a sized block of memory.
///
/// Use this class the way you would a const pointer to an allocated unstructured block.
/// The pointer and block size are available through member functions and cast operator.
///
/// Example usage:
///
/// \code
///     void* p = malloc (46721);
///     cmemlink a, b;
///     a.link (p, 46721);
///     assert (a.size() == 46721));
///     b = a;
///     assert (b.size() == 46721));
///     assert (b.DataAt(34) == a.DataAt(34));
///     assert (0 == memcmp (a, b, 12));
/// \endcode
///
class cmemlink {
public:
    typedef char		value_type;
    typedef const value_type*	pointer;
    typedef const value_type*	const_pointer;
    typedef value_type		reference;
    typedef value_type		const_reference;
    typedef size_t		size_type;
    typedef ptrdiff_t		difference_type;
    typedef const_pointer	const_iterator;
    typedef const_iterator	iterator;
    typedef const cmemlink&	rcself_t;
public:
			cmemlink (void);
			cmemlink (const void* p, size_type n);
			cmemlink (const cmemlink& l);
    inline virtual     ~cmemlink (void) {}
    void		link (const void* p, size_type n);
    inline void		link (const void* first, const void* last);
			OVERLOAD_POINTER_AND_SIZE_T_V2(link, const void*)
    inline void		link (const cmemlink& l);
    inline void		relink (const void* p, size_type n);
    virtual void	unlink (void);
    inline rcself_t	operator= (const cmemlink& l)	{ link (l.m_CData, l.m_Size); return (*this); }
    bool		operator== (const cmemlink& l) const;
    void		swap (cmemlink& l);
    inline size_type	size (void) const		{ return (m_Size); }
    inline size_type	max_size (void) const		{ return (size()); }
    inline size_type	readable_size (void) const	{ return (size()); }
    inline bool		empty (void) const		{ return (!size()); }
   inline const_pointer	cdata (void) const		{ return (m_CData); }
    inline iterator	begin (void) const		{ return (iterator (m_CData)); }
    inline iterator	end (void) const		{ return (begin() + size()); }
    inline void		resize (size_type n)		{ m_Size = n; }
    inline void		read (istream&);
    void		write (ostream& os) const;
    size_type		stream_size (void) const;
    void		write_file (const char* filename, int mode = 0644) const;
private:
    const_pointer	m_CData;	///< Pointer to the data block (const)
    size_type		m_Size;		///< size of the data block
};

/// Links to \p l
inline void cmemlink::link (const cmemlink& l)
{
    link (l.begin(), l.size());
}

/// Links to iterator range \p first - \p last
inline void cmemlink::link (const void* first, const void* last)
{
    link (first, distance (first, last));
}

/// A fast alternative to link which can be used when relinking to the same block (i.e. when it is resized)
inline void cmemlink::relink (const void* p, size_type n)
{
    m_CData = reinterpret_cast<const_pointer>(p);
    m_Size = n;
}

/// Reads the object from stream \p os
inline void cmemlink::read (istream&)
{
    assert (!"ustl::cmemlink is a read-only object.");
}

// Specialization for stream alignment
template <> inline size_t alignof (cmemlink) { return (alignof (size_t())); }

/// Use with cmemlink-derived classes to link to a static array
#define static_link(v)	link (v, VectorSize(v))

} // namespace ustl

#endif

