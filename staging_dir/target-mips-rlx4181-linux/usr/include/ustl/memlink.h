// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
// memlink.h

#ifndef MEMLINK_H_798D25827C8E322D2D7E734B169FF5FC
#define MEMLINK_H_798D25827C8E322D2D7E734B169FF5FC

#include "cmemlink.h"

namespace ustl {

/// \class memlink memlink.h ustl.h
/// \ingroup MemoryManagement
///
/// \brief Wrapper for pointer to block with size.
///
/// Use this class the way you would a pointer to an allocated unstructured block.
/// The pointer and block size are available through member functions and cast operator.
/// The begin in the block may be modified, but the block is static in size.
/// \warning This class actually contains two pointers: a const pointer and a non-const
/// pointer. Both are the same when you have linked the object to a modifiable block.
/// But if you have for some reason linked the object to a const block, the non-const
/// pointer will be NULL. With proper usage, there should be no problem with this; just
/// be aware that such a thing may happen.
///
/// Example usage:
/// \code
///     void* p = malloc (46721);
///     memlink a, b;
///     a.link (p, 46721);
///     assert (a.size() == 46721));
///     b = a;
///     assert (b.size() == 46721));
///     assert (b.begin() + 34 == a.begin + 34);
///     assert (0 == memcmp (a, b, 12));
///     a.fill (673, b, 42, 67);
///     b.erase (87, 12);
/// \endcode
///
class memlink : public cmemlink {
public:
    typedef value_type*			pointer;
    typedef cmemlink::pointer		const_pointer;
    typedef cmemlink::const_iterator	const_iterator;
    typedef pointer			iterator;
public:
			memlink (void);
			memlink (void* p, size_type n);
			memlink (const void* p, size_type n);
			memlink (const memlink& l);
    explicit		memlink (const cmemlink& l);
    inline void		link (const void* p, size_type n)		{ cmemlink::link (p, n); }
    void		link (void* p, size_type n);
    inline void		link (const cmemlink& l)			{ cmemlink::link (l); }
    inline void		link (memlink& l);
			OVERLOAD_POINTER_AND_SIZE_T_V2(link, void*)
			OVERLOAD_POINTER_AND_SIZE_T_V2(link, const void*)
    inline void		link (const void* first, const void* last)	{ link (first, distance (first, last)); }
    inline void		link (void* first, void* last)			{ link (first, distance (first, last)); }
    inline void		relink (const void* p, size_type n)		{ cmemlink::relink (p, n); }
    inline void		relink (void* p, size_type n);
    virtual void	unlink (void);
    inline void		copy (const cmemlink& l);
    inline void		copy (const void* p, size_type n);
    void		copy (iterator offset, const void* p, size_type n);
    size_type		writable_size (void) const;
    const memlink&	operator= (const cmemlink& l);
    const memlink&	operator= (const memlink& l);
    void		swap (memlink& l);
    inline pointer	data (void)			{ return (m_Data); }
    inline iterator	begin (void)			{ assert ((data() || !cdata()) && "This container has no modifiable data. What you probably want is to first make a writable copy with copy_link."); return (iterator (data())); }
    inline iterator	end (void)			{ return (begin() + size()); }
    inline const_iterator	begin (void) const	{ return (cmemlink::begin()); }
    inline const_iterator	end (void) const	{ return (cmemlink::end()); }
    void		fill (iterator start, const void* p, size_type elsize, size_type elCount = 1);
    void		insert (iterator start, size_type size);
    void		erase (iterator start, size_type size);
    void		read (istream& is);
private:
    pointer		m_Data;	///< Pointer to the begin block (non-const)
};

/// Copies from \p l.
inline void memlink::copy (const cmemlink& l)
{
    assert ((begin() || !l.size()) && "Can't copy into a constant link.");
    copy (begin(), l.cdata(), l.size());
}

/// Copies begin from \p p, \p n to the linked block.
inline void memlink::copy (const void* p, size_type n)
{
    assert ((begin() || !n) && "Can't copy into a constant link.");
    copy (begin(), p, n);
}

/// Links to \p l
inline void memlink::link (memlink& l)
{
    cmemlink::link (l);
    m_Data = l.data();
}

inline void memlink::relink (void* p, size_type n)
{
    cmemlink::relink (p, n);
    m_Data = reinterpret_cast<pointer>(p);
}

/// Reads object \p l from stream \p is
inline istream& operator>> (istream& is, memlink& l)
{
    l.read (is);
    return (is);
}

/// Use with memlink-derived classes to allocate and link to stack space.
#define alloca_link(m,n)	(m).link (alloca (n), (n))

} // namespace ustl

#endif

