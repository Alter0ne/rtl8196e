// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
// uset.h
//

#ifndef USET_H_45543F516E02A87A3FCEA5024052A6F5
#define USET_H_45543F516E02A87A3FCEA5024052A6F5

#include "uvector.h"

namespace ustl {

/// \class set uset.h ustl.h
/// \ingroup Sequences
///
/// \brief Unique sorted container. Sorted vector with all values unique.
///
template <typename T>
class set : public vector<T> {
public:
    typedef typename vector<T>::value_type	key_type;
    typedef typename vector<T>::value_type	data_type;
    typedef typename vector<T>::value_type	value_type;
    typedef typename vector<T>::size_type	size_type;
    typedef typename vector<T>::pointer		pointer;
    typedef typename vector<T>::const_pointer	const_pointer;
    typedef typename vector<T>::reference	reference;
    typedef typename vector<T>::const_reference	const_reference;
    typedef typename vector<T>::const_iterator	const_iterator;
    typedef typename vector<T>::iterator	iterator;
    typedef typename vector<T>::reverse_iterator	reverse_iterator;
    typedef typename vector<T>::const_reverse_iterator	const_reverse_iterator;
public:
    inline			set (void);
    explicit inline		set (size_type n);
    inline			set (const set<T>& v);
    inline			set (const_iterator i1, const_iterator i2);
    inline const set<T>&	operator= (const set<T>& v);
    inline void			assign (const_iterator i1, const_iterator i2);
    inline void			push_back (const_reference v);
    const_iterator		find (const_reference v) const;
    iterator			find (const_reference v);
    iterator			insert (const_reference v);
    inline void			insert (const_iterator i1, const_iterator i2);
    void			erase (const_reference v);
    inline iterator		erase (iterator ep);
    inline iterator		erase (iterator ep1, iterator ep2);
    inline void			clear (void)		{ vector<T>::clear(); }
    inline size_type		size (void) const	{ return (vector<T>::size()); }
    inline iterator		begin (void)		{ return (vector<T>::begin()); }
    inline const_iterator	begin (void) const	{ return (vector<T>::begin()); }
    inline iterator		end (void)		{ return (vector<T>::end()); }
    inline const_iterator	end (void) const	{ return (vector<T>::end()); }
};

/// Default constructor.
template <typename T>
inline set<T>::set (void)
: vector<T> ()
{
}

/// Creates a container able to hold at least \p n elements.
template <typename T>
inline set<T>::set (size_type n)
: vector<T> (n)
{
}

/// Creates a copy of \p v.
template <typename T>
inline set<T>::set (const set<T>& v)
: vector<T> (v)
{
}

/// Inserts elements from range [i1,i2), which does not have to be sorted.
template <typename T>
inline set<T>::set (const_iterator i1, const_iterator i2)
: vector<T> ()
{
    insert (i1, i2);
}

/// Copies the contents of \p v.
template <typename T>
inline const set<T>& set<T>::operator= (const set<T>& v)
{
    vector<T>::operator= (v);
    return (*this);
}

/// Copies the contents of range [i1,i2).
template <typename T>
inline void set<T>::assign (const_iterator i1, const_iterator i2)
{
    clear();
    insert (i1, i2);
}

/// Returns the iterator to an element with value of \p v.
template <typename T>
typename set<T>::const_iterator set<T>::find (const_reference v) const
{
    return (binary_search (begin(), end(), v));
}

/// Returns the iterator to an element with value of \p v.
template <typename T>
typename set<T>::iterator set<T>::find (const_reference v)
{
    return (binary_search (begin(), end(), v));
}

/// Inserts \p v into the container, maintaining the sort order.
template <typename T>
inline void set<T>::push_back (const_reference v)
{
    insert (v);
}

/// Inserts \p v into the container, maintaining the sort order.
template <typename T>
typename set<T>::iterator set<T>::insert (const_reference v)
{
    iterator ip = lower_bound (begin(), end(), v);
    if (ip == end() || v < *ip)
	ip = vector<T>::insert (ip, v);
    else
	*ip = v;
    return (ip);
}

/// Inserts the contents of range [i1,i2)
template <typename T>
inline void set<T>::insert (const_iterator i1, const_iterator i2)
{
    assert (i1 <= i2);
    reserve (size() + distance (i1, i2));
    while (i1 < i2)
	push_back (*i1++);
}

/// Erases the element with value \p v.
template <typename T>
void set<T>::erase (const_reference v)
{
    iterator ip = binary_search (begin(), end(), v);
    if (ip != end())
	erase (ip);
}

/// Erases the element at \p ep.
template <typename T>
inline typename set<T>::iterator set<T>::erase (iterator ep)
{
    return (vector<T>::erase (ep));
}

/// Erases the range [ep1,ep2).
template <typename T>
inline typename set<T>::iterator set<T>::erase (iterator ep1, iterator ep2)
{
    return (vector<T>::erase (ep1, ep2));
}

} // namespace ustl

#endif

