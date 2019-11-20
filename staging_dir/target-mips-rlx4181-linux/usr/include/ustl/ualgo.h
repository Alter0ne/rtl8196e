// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
// ualgo.h
//
// Implementation of STL algorithms.
//
// The function prototypes are copied
// exactly from the SGI version of STL documentation along with comments about
// their use. The code is NOT the same, though the functionality usually is.
//

#ifndef UALGO_H
#define UALGO_H

#include "upair.h"
#include "ualgobase.h"
#include "ufunction.h"
#include "upredalgo.h"
#include "umemory.h"
#include <stdlib.h>	// for rand()

namespace ustl {

/// Swaps corresponding elements of [first, last) and [result,)
/// \ingroup SwapAlgorithms
///
template <typename ForwardIterator1, typename ForwardIterator2>
inline ForwardIterator2 swap_ranges (ForwardIterator1 first, ForwardIterator2 last, ForwardIterator2 result)
{
    for (; first != last; ++first, ++result)
	iterator_swap (first, result);
    return (result);
}

/// Returns the first iterator i in the range [first, last) such that
/// *i == value. Returns last if no such iterator exists. 
/// \ingroup SearchingAlgorithms
///
template <typename InputIterator, typename EqualityComparable>
inline InputIterator find (InputIterator first, InputIterator last, const EqualityComparable& value)
{
    while (first != last && !(*first == value))
	++ first;
    return (first);
}

/// Returns the first iterator such that *i == *(i + 1)
/// \ingroup SearchingAlgorithms
///
template <typename ForwardIterator>
inline ForwardIterator adjacent_find (ForwardIterator first, ForwardIterator last)
{
    if (first != last)
	for (ForwardIterator prev = first; ++first != last; ++ prev)
	    if (*prev == *first)
		return (prev);
    return (last);
}

/// Returns the pointer to the first pair of unequal elements.
/// \ingroup SearchingAlgorithms
///
template <typename InputIterator>
inline pair<InputIterator,InputIterator>
mismatch (InputIterator first1, InputIterator last1, InputIterator first2)
{
    while (first1 != last1 && *first1 == *first2)
	++ first1, ++ first2;
    return (make_pair (first1, first2));
}

/// \brief Returns true if two ranges are equal.
/// This is an extension, present in uSTL and SGI STL.
/// \ingroup SearchingAlgorithms
///
template <typename InputIterator>
inline bool equal (InputIterator first1, InputIterator last1, InputIterator first2)
{
    return (mismatch (first1, last1, first2).first == last1);
}

/// Count finds the number of elements in [first, last) that are equal
/// to value. More precisely, the first version of count returns the
/// number of iterators i in [first, last) such that *i == value.
/// \ingroup SearchingAlgorithms
///
template <typename InputIterator, typename EqualityComparable>
inline size_t count (InputIterator first, InputIterator last, const EqualityComparable& value)
{
    size_t total = 0;
    for (; first != last; ++first)
	if (*first == value)
	    ++ total;
    return (total);
}

///
/// The first version of transform performs the operation op(*i) for each
/// iterator i in the range [first, last), and assigns the result of that
/// operation to *o, where o is the corresponding output iterator. That is,
/// for each n such that 0 <= n < last - first, it performs the assignment
/// *(result + n) = op(*(first + n)).
/// The return value is result + (last - first).
/// \ingroup MutatingAlgorithms
///
template <typename InputIterator, typename OutputIterator, typename UnaryFunction>
inline OutputIterator transform (InputIterator first, InputIterator last, OutputIterator result, UnaryFunction op)
{
    for (; first != last; ++result, ++first)
	*result = op (*first);
    return (result);
}

///
/// The second version of transform is very similar, except that it uses a
/// Binary Function instead of a Unary Function: it performs the operation
/// op(*i1, *i2) for each iterator i1 in the range [first1, last1) and assigns
/// the result to *o, where i2 is the corresponding iterator in the second
/// input range and where o is the corresponding output iterator. That is,
/// for each n such that 0 <= n < last1 - first1, it performs the assignment
/// *(result + n) = op(*(first1 + n), *(first2 + n).
/// The return value is result + (last1 - first1).
/// \ingroup MutatingAlgorithms
///
template <typename InputIterator1, typename InputIterator2, typename OutputIterator, typename BinaryFunction>
inline OutputIterator transform (InputIterator1 first1, InputIterator1 last1, InputIterator2 first2, OutputIterator result, BinaryFunction op)
{
    for (; first1 != last1; ++result, ++first1, ++first2)
	*result = op (*first1, *first2);
    return (result);
}

/// Replace replaces every element in the range [first, last) equal to
/// old_value with new_value. That is: for every iterator i,
/// if *i == old_value then it performs the assignment *i = new_value.
/// \ingroup MutatingAlgorithms
///
template <typename ForwardIterator, typename T>
inline void replace (ForwardIterator first, ForwardIterator last, const T& old_value, const T& new_value)
{
    for (; first != last; ++first)
	if (*first == old_value)
	    *first = new_value;
}

/// Replace_copy copies elements from the range [first, last) to the range
/// [result, result + (last-first)), except that any element equal to old_value
/// is not copied; new_value is copied instead. More precisely, for every
/// integer n such that 0 <= n < last-first, replace_copy performs the
/// assignment *(result+n) = new_value if *(first+n) == old_value, and
/// *(result+n) = *(first+n) otherwise.
/// \ingroup MutatingAlgorithms
///
template <typename InputIterator, typename OutputIterator, typename T>
inline OutputIterator replace_copy (InputIterator first, InputIterator last, OutputIterator result, const T& old_value, const T& new_value)
{
    for (; first != last; ++result, ++first)
        *result = (*first == old_value) ? new_value : *first;
}

/// Generate assigns the result of invoking gen, a function object that
/// takes no arguments, to each element in the range [first, last).
/// \ingroup MutatingAlgorithms
///
template <typename ForwardIterator, typename Generator>
inline void generate (ForwardIterator first, ForwardIterator last, Generator gen)
{
    for (; first != last; ++first)
	*first = gen();
}

/// Generate_n assigns the result of invoking gen, a function object that
/// takes no arguments, to each element in the range [first, first+n).
/// The return value is first + n.
/// \ingroup MutatingAlgorithms
///
template <typename OutputIterator, typename Generator>
inline OutputIterator generate_n (OutputIterator first, size_t n, Generator gen)
{
    for (size_t i = 0; i != n; ++i, ++first)
	*first = gen();
    return (first);
}

/// Reverse reverses a range.
/// That is: for every i such that 0 <= i <= (last - first) / 2),
/// it exchanges *(first + i) and *(last - (i + 1)).
/// \ingroup MutatingAlgorithms
///
template <typename BidirectionalIterator>
inline void reverse (BidirectionalIterator first, BidirectionalIterator last)
{
    for (; first != last && first != --last; ++first)
	iterator_swap (first, last);
}

/// Exchanges ranges [first, middle) and [middle, last)
/// \ingroup MutatingAlgorithms
///
template <typename ForwardIterator>
ForwardIterator rotate (ForwardIterator first, ForwardIterator middle, ForwardIterator last)
{
    if (first == middle || middle == last)
	return (first);
#ifdef HAVE_ALLOCA_H
    void *buf, *vfirst(first), *vlast(last), *vresult(first + distance(middle, last));
    const void *cvfirst(first), *cvmiddle(middle), *cvlast(last);
    const size_t half1 (distance (cvfirst, cvmiddle));
    const size_t half2 (distance (cvmiddle, cvlast));
    if (half2 < half1 && (buf = alloca (half2))) {
	copy (cvmiddle, cvlast, buf);
	copy_backward (cvfirst, cvmiddle, vlast);
	copy_n ((const void*) buf, half2, vfirst);
    } else if (half1 <= half2 && (buf = alloca (half1))) {
	copy (cvfirst, cvmiddle, buf);
	copy (cvmiddle, cvlast, vfirst);
	copy_n ((const void*) buf, half1, vresult);
    } else
#endif
    {
	reverse (first, middle);
	reverse (middle, last);
	for (;first != middle && middle != last; ++first)
	    iterator_swap (first, --last);
	if (first == middle)
	    reverse (middle, last);
	else
	    reverse (first, middle);
    }
    return (first);
}

/// \brief Combines two sorted ranges.
/// \ingroup SortingAlgorithms
///
template <typename InputIterator1, typename InputIterator2, typename OutputIterator>
OutputIterator merge (InputIterator1 first1, InputIterator1 last1,
		      InputIterator2 first2, InputIterator2 last2, OutputIterator result)
{
    for (; first1 != last1 && first2 != last2; ++result) {
	if (*first1 < *first2)
	    *result = *first1++;
	else
	    *result = *first2++;
    }
    if (first1 < last1)
	return (copy (first1, last1, result));
    else
	return (copy (first2, last2, result));
}

/// Combines two sorted ranges from the same container.
/// \ingroup SortingAlgorithms
///
template <typename InputIterator>
void inplace_merge (InputIterator first, InputIterator middle, InputIterator last)
{
    for (; middle != last; ++first) {
	while (*first < *middle)
	    ++ first;
	reverse (first, middle);
	reverse (first, ++middle);
    }
}

/// Remove_copy copies elements that are not equal to value from the range
/// [first, last) to a range beginning at result. The return value is the
/// end of the resulting range. This operation is stable, meaning that the
/// relative order of the elements that are copied is the same as in the
/// range [first, last).
/// \ingroup MutatingAlgorithms
///
template <typename InputIterator, typename OutputIterator, typename T>
inline OutputIterator remove_copy (InputIterator first, InputIterator last, OutputIterator result, const T& value)
{
    for (; first != last; ++first) {
	if (!(*first == value)) {
	    *result = *first;
	    ++ result;
	}
    }
    return (result);
}

/// Remove_copy copies elements pointed to by iterators in [rfirst, rlast)
/// from the range [first, last) to a range beginning at result. The return
/// value is the end of the resulting range. This operation is stable, meaning
/// that the relative order of the elements that are copied is the same as in the
/// range [first, last). Range [rfirst, rlast) is assumed to be sorted.
/// This algorithm is a uSTL extension.
/// \ingroup MutatingAlgorithms
///
template <typename InputIterator, typename OutputIterator, typename RInputIterator>
OutputIterator remove_copy (InputIterator first, InputIterator last, OutputIterator result, RInputIterator rfirst, RInputIterator rlast)
{
    for (; first != last; ++first) {
	while (rfirst != rlast && *rfirst < first)
	    ++ rfirst;
	if (rfirst == rlast || first != *rfirst) {
	    *result = *first;
	    ++ result;
	}
    }
    return (result);
}

/// Remove removes from the range [first, last) all elements that are equal to
/// value. That is, remove returns an iterator new_last such that the range
/// [first, new_last) contains no elements equal to value. [1] The iterators
/// in the range [new_last, last) are all still dereferenceable, but the
/// elements that they point to are unspecified. Remove is stable, meaning
/// that the relative order of elements that are not equal to value is
/// unchanged.
/// \ingroup MutatingAlgorithms
///
template <typename ForwardIterator, typename T>
inline ForwardIterator remove (ForwardIterator first, ForwardIterator last, const T& value)
{
    return (remove_copy (first, last, first, value));
}

/// Unique_copy copies elements from the range [first, last) to a range
/// beginning with result, except that in a consecutive group of duplicate
/// elements only the first one is copied. The return value is the end of
/// the range to which the elements are copied. This behavior is similar
/// to the Unix filter uniq.
/// \ingroup MutatingAlgorithms
///
template <typename InputIterator, typename OutputIterator>
OutputIterator unique_copy (InputIterator first, InputIterator last, OutputIterator result)
{
    if (first != last) {
	*result = *first;
	while (++first != last)
	    if (!(*first == *result))
		*++result = *first;
	++ result;
    }
    return (result);
}

/// Every time a consecutive group of duplicate elements appears in the range
/// [first, last), the algorithm unique removes all but the first element.
/// That is, unique returns an iterator new_last such that the range [first,
/// new_last) contains no two consecutive elements that are duplicates.
/// The iterators in the range [new_last, last) are all still dereferenceable,
/// but the elements that they point to are unspecified. Unique is stable,
/// meaning that the relative order of elements that are not removed is
/// unchanged.
/// \ingroup MutatingAlgorithms
///
template <typename ForwardIterator>
inline ForwardIterator unique (ForwardIterator first, ForwardIterator last)
{
    return (unique_copy (first, last, first));
}

/// Returns the furthermost iterator i in [first, last) such that,
/// for every iterator j in [first, i), *j < value
/// Assumes the range is sorted.
/// \ingroup SearchingAlgorithms
///
template <typename ForwardIterator, typename LessThanComparable>
inline ForwardIterator lower_bound (ForwardIterator first, ForwardIterator last, const LessThanComparable& value)
{
    ForwardIterator mid;
    while (first != last) {
	mid = advance (first, distance (first,last) / 2);
	if (*mid < value)
	    first = mid + 1;
	else
	    last = mid;
    }
    return (first);
}

/// Performs a binary search inside the sorted range.
/// \ingroup SearchingAlgorithms
///
template <typename ForwardIterator, typename LessThanComparable>
inline ForwardIterator binary_search (ForwardIterator first, ForwardIterator last, const LessThanComparable& value)
{
    ForwardIterator found = lower_bound (first, last, value);
    return ((found == last || value < *found) ? last : found);
}

/// Returns the furthermost iterator i in [first,last) such that for
/// every iterator j in [first,i), value < *j is false.
/// \ingroup SearchingAlgorithms
///
template <typename ForwardIterator, typename LessThanComparable>
inline ForwardIterator upper_bound (ForwardIterator first, ForwardIterator last, const LessThanComparable& value)
{
    ForwardIterator mid;
    while (first != last) {
	mid = advance (first, distance (first,last) / 2);
	if (value < *mid)
	    last = mid;
	else
	    first = mid + 1;
    }
    return (last);
}

/// Returns pair<lower_bound,upper_bound>
/// \ingroup SearchingAlgorithms
///
template <typename ForwardIterator, typename LessThanComparable>
inline pair<ForwardIterator,ForwardIterator> equal_range (ForwardIterator first, ForwardIterator last, const LessThanComparable& value)
{
    pair<ForwardIterator,ForwardIterator> rv;
    rv.second = rv.first = lower_bound (first, last, value);
    while (rv.second != last && !(value < *(rv.second)))
	++ rv.second;
    return (rv);
}

/// Randomly permute the elements of the container.
/// \ingroup MutatingAlgorithms
///
template <typename RandomAccessIterator>
inline void random_shuffle (RandomAccessIterator first, RandomAccessIterator last)
{
    for (; first != last; ++ first)
	iterator_swap (first, first + (rand() % distance (first, last)));
}

template <typename ConstPointer, typename Compare>
int qsort_adapter (const void* p1, const void* p2)
{
    ConstPointer i1 = reinterpret_cast<ConstPointer>(p1);
    ConstPointer i2 = reinterpret_cast<ConstPointer>(p2);
    Compare comp;
    return (comp (*i1, *i2) ? -1 : (comp (*i2, *i1) ? 1 : 0));
}

/// Sorts the container
/// \ingroup SortingAlgorithms
///
template <typename RandomAccessIterator, typename Compare>
void sort (RandomAccessIterator first, RandomAccessIterator last, Compare)
{
    typedef typename iterator_traits<RandomAccessIterator>::value_type value_type;
    typedef typename iterator_traits<RandomAccessIterator>::const_pointer const_pointer;
    qsort (first, distance (first, last), sizeof(value_type),
	   &qsort_adapter<const_pointer, Compare>);
}

/// Sorts the container
/// \ingroup SortingAlgorithms
///
template <typename RandomAccessIterator>
inline void sort (RandomAccessIterator first, RandomAccessIterator last)
{
    typedef typename iterator_traits<RandomAccessIterator>::value_type value_type;
    sort (first, last, less<value_type>());
}

/// Sorts the container preserving order of equal elements.
/// \ingroup SortingAlgorithms
///
template <typename RandomAccessIterator, typename Compare>
void stable_sort (RandomAccessIterator first, RandomAccessIterator last, Compare comp)
{
    for (RandomAccessIterator ip (first); ip != last; ++ ip) {
	RandomAccessIterator minEl (ip), curEl (ip);
	while (++curEl != last)
	    if (comp(*curEl, *minEl))
		minEl = curEl;
	if (ip != minEl)
	    iterator_swap (ip, minEl);
    }
}

/// Sorts the container
/// \ingroup SortingAlgorithms
///
template <typename RandomAccessIterator>
inline void stable_sort (RandomAccessIterator first, RandomAccessIterator last)
{
    typedef typename iterator_traits<RandomAccessIterator>::value_type value_type;
    stable_sort (first, last, less<value_type>());
}

/// \brief Partially sort the range.
/// Postcondition is that \p middle has the nth element and [first, middle)
/// has elements smaller than those in (middle, last).
/// In this implementation, the entire array is sorted. I can't think of any
/// use for it where the time gained would be useful.
/// \ingroup SortingAlgorithms
///
template <typename RandomAccessIterator>
inline void partial_sort (RandomAccessIterator first, RandomAccessIterator, RandomAccessIterator last)
{
    sort (first, last);
}

/// \brief Puts \p nth element into its sorted position.
/// In this implementation, the entire array is sorted. I can't think of any
/// use for it where the time gained would be useful.
/// \ingroup SortingAlgorithms
///
template <typename RandomAccessIterator>
inline void nth_element (RandomAccessIterator first, RandomAccessIterator nth, RandomAccessIterator last)
{
    partial_sort (first, nth, last);
}

} // namespace ustl

#endif

