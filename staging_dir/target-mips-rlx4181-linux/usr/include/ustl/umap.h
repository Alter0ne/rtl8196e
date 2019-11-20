// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
// umap.h
//

#ifndef UMAP_H_45643F516E02A87A3DCEA5024052A6F5
#define UMAP_H_45643F516E02A87A3DCEA5024052A6F5

#include "uvector.h"
#include "ufunction.h"

namespace ustl {

/// \class map umap.h ustl.h
/// \ingroup AssociativeContainers
///
/// \brief A sorted associative container of pair<K,V>
///
template <typename K, typename V>
class map : public vector<pair<K,V> > {
public:
    typedef K						key_type;
    typedef V						data_type;
    typedef const K&					const_key_ref;
    typedef const V&					const_data_ref;
    typedef typename vector<pair<K,V> >::value_type	value_type;
    typedef typename vector<pair<K,V> >::size_type	size_type;
    typedef typename vector<pair<K,V> >::pointer	pointer;
    typedef typename vector<pair<K,V> >::const_pointer	const_pointer;
    typedef typename vector<pair<K,V> >::reference	reference;
    typedef typename vector<pair<K,V> >::const_reference	const_reference;
    typedef typename vector<pair<K,V> >::const_iterator		const_iterator;
    typedef typename vector<pair<K,V> >::iterator		iterator;
    typedef typename vector<pair<K,V> >::reverse_iterator	reverse_iterator;
    typedef typename vector<pair<K,V> >::const_reverse_iterator	const_reverse_iterator;
    typedef pair<const_iterator,const_iterator>		const_range_t;
    typedef pair<iterator,iterator>			range_t;
public:
    inline			map (void);
    explicit inline		map (size_type n);
    inline			map (const map<K,V>& v);
    inline			map (const_iterator i1, const_iterator i2);
    inline const map<K,V>&	operator= (const map<K,V>& v);
    inline const_data_ref	operator[] (const_key_ref i) const;
    data_type&			operator[] (const_key_ref i);
    inline void			assign (const_iterator i1, const_iterator i2);
    inline void			push_back (const_reference v);
    inline const_iterator	find (const_key_ref k) const;
    inline iterator		find (const_key_ref k);
    inline const_iterator	find_data (const_data_ref v, const_iterator first = NULL, const_iterator last = NULL) const;
    inline iterator		find_data (const_data_ref v, iterator first = NULL, iterator last = NULL);
    iterator			insert (const_reference v);
    inline void			insert (const_iterator i1, const_iterator i2);
    void			erase (const_key_ref k);
    inline iterator		erase (iterator ep);
    inline iterator		erase (iterator ep1, iterator ep2);
    inline void			clear (void)		{ vector<pair<K,V> >::clear(); }
    inline size_type		size (void) const	{ return (vector<pair<K,V> >::size()); }
    inline iterator		begin (void)		{ return (vector<pair<K,V> >::begin()); }
    inline const_iterator	begin (void) const	{ return (vector<pair<K,V> >::begin()); }
    inline iterator		end (void)		{ return (vector<pair<K,V> >::end()); }
    inline const_iterator	end (void) const	{ return (vector<pair<K,V> >::end()); }
};

/// Default constructor.
template <typename K, typename V>
inline map<K,V>::map (void)
: vector<pair<K,V> > ()
{
}

/// Constructs the container with space for \p n elements.
template <typename K, typename V>
inline map<K,V>::map (size_type n)
: vector<pair<K,V> > (n)
{
}

/// Copies contents of \p v.
template <typename K, typename V>
inline map<K,V>::map (const map<K,V>& v)
: vector<pair<K,V> > (v)
{
}

/// Inserts elements from range [i1,i2)
template <typename K, typename V>
inline map<K,V>::map (const_iterator i1, const_iterator i2)
: vector<pair<K,V> > ()
{
    insert (i1, i2);
}

/// Copies contents of \p v.
template <typename K, typename V>
inline const map<K,V>& map<K,V>::operator= (const map<K,V>& v)
{
    vector<pair<K,V> >::operator= (v);
    return (*this);
}

/// Inserts elements from range [i1,i2)
template <typename K, typename V>
inline void map<K,V>::assign (const_iterator i1, const_iterator i2)
{
    clear();
    insert (i1, i2);
}

/// Returns the pair<K,V> where K = \p k.
template <typename K, typename V>
inline typename map<K,V>::const_iterator map<K,V>::find (const_key_ref k) const
{
    return (binary_search (begin(), end(), make_pair(k,V()), mem_var_less(&value_type::first)));
}

/// Returns the pair<K,V> where K = \p k.
template <typename K, typename V>
inline typename map<K,V>::iterator map<K,V>::find (const_key_ref k)
{
    return (binary_search (begin(), end(), make_pair(k,V()), mem_var_less(&value_type::first)));
}

/// Returns the pair<K,V> where V = \p v, occuring in range [first,last).
template <typename K, typename V>
inline typename map<K,V>::const_iterator map<K,V>::find_data (const_data_ref v, const_iterator first, const_iterator last) const
{
    if (!first) first = begin();
    if (!last) last = end();
    return (find_if (first, last, mem_var_equal_to(&value_type::second, v)));
}

/// Returns the pair<K,V> where V = \p v, occuring in range [first,last).
template <typename K, typename V>
inline typename map<K,V>::iterator map<K,V>::find_data (const_data_ref v, iterator first, iterator last)
{
    if (!first) first = begin();
    if (!last) last = end();
    return (find_if (first, last, mem_var_equal_to(&value_type::second, v)));
}

/// Returns data associated with key \p k.
template <typename K, typename V>
inline const typename map<K,V>::data_type& map<K,V>::operator[] (const_key_ref k) const
{
    assert (find(k) && "operator[] const can not insert non-existent keys");
    return (find(k)->second);
}

/// Returns data associated with key \p k.
template <typename K, typename V>
typename map<K,V>::data_type& map<K,V>::operator[] (const_key_ref k)
{
    const value_type match (k, V());
    iterator ip = lower_bound (begin(), end(), match, mem_var_less(&value_type::first));
    if (ip == end() || match.first < ip->first)
	ip = vector<pair<K,V> >::insert (ip, match);
    return (ip->second);
}

/// Inserts the pair into the container.
template <typename K, typename V>
inline void map<K,V>::push_back (const_reference v)
{
    insert (v);
}

/// Inserts the pair into the container.
template <typename K, typename V>
typename map<K,V>::iterator map<K,V>::insert (const_reference v)
{
    iterator ip = lower_bound (begin(), end(), v, mem_var_less(&value_type::first));
    if (ip == end() || v.first < ip->first)
	ip = vector<pair<K,V> >::insert (ip, v);
    else
	*ip = v;
    return (ip);
}

/// Inserts elements from range [i1,i2) into the container.
template <typename K, typename V>
inline void map<K,V>::insert (const_iterator i1, const_iterator i2)
{
    assert (i1 <= i2);
    reserve (size() + distance (i1, i2));
    for_each (i1, i2, mem_fun (this, &map::push_back));
}

/// Erases the element with key value \p k.
template <typename K, typename V>
void map<K,V>::erase (const_key_ref k)
{
    iterator ip = binary_search (begin(), end(), make_pair(k,V()), mem_var_less(&value_type::first));
    if (ip != end())
	erase (ip);
}

/// Erases the element at iterator \p ep.
template <typename K, typename V>
inline typename map<K,V>::iterator map<K,V>::erase (iterator ep)
{
    return (vector<pair<K,V> >::erase (ep));
}

/// Erases range [ep1,ep2).
template <typename K, typename V>
inline typename map<K,V>::iterator map<K,V>::erase (iterator ep1, iterator ep2)
{
    return (vector<pair<K,V> >::erase (ep1, ep2));
}

} // namespace ustl

#endif

