// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
// uheap.h
//
// Implementation of STL heap algorithms.
//
// The function prototypes are copied
// exactly from the SGI version of STL documentation along with comments about
// their use. The code is NOT the same, though the functionality usually is.
//

#ifndef UHEAP_H_574B9EAF271A1C107190B4D575A356C5
#define UHEAP_H_574B9EAF271A1C107190B4D575A356C5

#include "ualgobase.h"

namespace ustl {

/// \brief Returns true if the given range is a heap under \p comp.
/// A heap is a sequentially encoded binary tree where for every node
/// comp(node,child1) is false and comp(node,child2) is false.
///
template <typename RandomAccessIterator, typename Compare>
bool is_heap (RandomAccessIterator first, RandomAccessIterator last, Compare comp)
{
    RandomAccessIterator iChild (first);
    for (; ++iChild < last; ++first)
	if (comp (*first, *iChild) || (++iChild < last && comp (*first, *iChild)))
	    return (false);
    return (true);
}

/// \brief make_heap turns the range [first, last) into a heap
/// At completion, is_heap (first, last, comp) is true.
/// The algorithm is adapted from "Classic Data Structures in C++" by Timothy Budd.
///
template <typename RandomAccessIterator, typename Compare>
void make_heap (RandomAccessIterator first, RandomAccessIterator last, Compare comp)
{
    typedef typename iterator_traits<RandomAccessIterator>::value_type value_type;
    const value_type v (*first);
    RandomAccessIterator i (first);
    while (i < last) {
	// replace position with the smaller of the two children, or the last element
	RandomAccessIterator iChild = i + 2 * distance (first, i) + 1;
	if (iChild < last) {
	    if (iChild + 1 < last && comp (*iChild, *(iChild + 1)))
		++ iChild;
	    if (comp (*iChild, v)) {
		*i = v;
		break;
	    } else {
		*i = *iChild;
		i = iChild;
	    }
	} else {
	    *i = v;
	    break;
	}
    }
}

/// \brief Inserts the *--last into the preceeding range assumed to be a heap.
template <typename RandomAccessIterator, typename Compare>
void push_heap (RandomAccessIterator first, RandomAccessIterator last, Compare comp)
{
    if (last <= first)
	return;
    typedef typename iterator_traits<RandomAccessIterator>::value_type value_type;
    const value_type v (*--last);
    while (first < last) {
	RandomAccessIterator iParent = first + (distance(first, last) - 1) / 2;
	if (comp (v, *iParent))
	    break;
	else {
	    *last = *iParent;
	    last = iParent;
	}
    }
    *last = v;
}

template <typename RandomAccessIterator>
inline bool is_heap (RandomAccessIterator first, RandomAccessIterator last)
{
    typedef typename iterator_traits<RandomAccessIterator>::value_type value_type;
    return (is_heap (first, last, less<value_type>()));
}

/// \brief Make_heap turns the range [first, last) into a heap
/// At completion, is_heap (first, last) is true.
template <typename RandomAccessIterator>
inline void make_heap (RandomAccessIterator first, RandomAccessIterator last)
{
    typedef typename iterator_traits<RandomAccessIterator>::value_type value_type;
    make_heap (first, last, less<value_type>());
}

/// \brief Inserts the *--last into the preceeding range assumed to be a heap.
template <typename RandomAccessIterator>
inline void push_heap (RandomAccessIterator first, RandomAccessIterator last)
{
    typedef typename iterator_traits<RandomAccessIterator>::value_type value_type;
    push_heap (first, last, less<value_type>());
}

} // namespace ustl

#endif

