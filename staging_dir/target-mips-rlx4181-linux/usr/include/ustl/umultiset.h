// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
// umultiset.h
//

#ifndef UMULTISET_H_446AEDBB7F61C6994DC228C25D5FA3A1
#define UMULTISET_H_446AEDBB7F61C6994DC228C25D5FA3A1

#include "uvector.h"
#include "ualgo.h"

namespace ustl {

/// \class multiset umultiset.h ustl.h
/// \ingroup AssociativeContainers
///
/// \brief Multiple sorted container.
/// Unlike set, it may contain multiple copies of each element.
///
template <typename T>
class multiset : public vector<T> {
public:
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
    inline			multiset (void);
    explicit inline		multiset (size_type n);
    inline			multiset (const multiset<T>& v);
    inline			multiset (const_iterator i1, const_iterator i2);
    inline const multiset<T>&	operator= (const multiset<T>& v);
    inline void			assign (const_iterator i1, const_iterator i2);
    size_type			count (const_reference v) const;
    inline void			push_back (const_reference v);
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
inline multiset<T>::multiset (void)
: vector<T> ()
{
}

/// Creates the container with space enough to hold \p n elements.
template <typename T>
inline multiset<T>::multiset (size_type n)
: vector<T> (n)
{
}

/// Creates a copy of \p v.
template <typename T>
inline multiset<T>::multiset (const multiset<T>& v)
: vector<T> (v)
{
}

/// Copies range [i1,i2)
template <typename T>
inline multiset<T>::multiset (const_iterator i1, const_iterator i2)
: vector<T> ()
{
    insert (i1, i2);
}

/// Copies contents of \p v.
template <typename T>
inline const multiset<T>& multiset<T>::operator= (const multiset<T>& v)
{
    vector<T>::operator= (v);
    return (*this);
}

/// Copies contents of range [i1,i2)
template <typename T>
inline void multiset<T>::assign (const_iterator i1, const_iterator i2)
{
    multiset<T>::clear();
    insert (i1, i2);
}

/// Returns the number of elements of value \p v.
template <typename T>
typename multiset<T>::size_type multiset<T>::count (const_reference v) const
{
    const pair<const_iterator,const_iterator> fr = equal_range (begin(), end(), v);
    return (distance (fr.first, fr.second));
}

/// Inserts \p v.
template <typename T>
inline void multiset<T>::push_back (const_reference v)
{
    insert (v);
}

/// Inserts \p v.
template <typename T>
typename multiset<T>::iterator multiset<T>::insert (const_reference v)
{
    iterator ip = upper_bound (begin(), end(), v);
    return (vector<T>::insert (ip, v));
}

/// Inserts all elements from range [i1,i2).
template <typename T>
inline void multiset<T>::insert (const_iterator i1, const_iterator i2)
{
    assert (i1 <= i2);
    reserve (size() + distance (i1, i2));
    while (i1 < i2)
	push_back (*i1++);
}

/// Erases all elements with value \p v.
template <typename T>
void multiset<T>::erase (const_reference v)
{
    pair<iterator,iterator> epr = equal_range (begin(), end(), v);
    erase (epr.first, epr.second);
}

/// Erases the element at \p ep.
template <typename T>
inline typename multiset<T>::iterator multiset<T>::erase (iterator ep)
{
    return (vector<T>::erase (ep));
}

/// Erases range [ep1,ep2).
template <typename T>
inline typename multiset<T>::iterator multiset<T>::erase (iterator ep1, iterator ep2)
{
    return (vector<T>::erase (ep1, ep2));
}

} // namespace ustl

#endif

