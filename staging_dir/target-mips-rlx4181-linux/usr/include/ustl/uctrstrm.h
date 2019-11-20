// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
/// \file uctrstrm.h
///
/// \brief Serialization templates for standard containers.
/// Because containers are templates, a single operator>> is impossible.
/// Making virtual read/write is also impossible because not all containers
/// contain serializable elements. Therefore, use the macros in this file.
///

#ifndef UCTRSTRM_H_75B2C3EA4980DDDC6B6DFFF767A3B7AC
#define UCTRSTRM_H_75B2C3EA4980DDDC6B6DFFF767A3B7AC

#include "mistream.h"
#include "sostream.h"

namespace ustl {

//----------------------------------------------------------------------
// Macros for easily declaring a container streamable.
//----------------------------------------------------------------------

/// \brief Declares container template \p type streamable.
///
/// Use TEMPLATE_TYPE and TEMPLATE_DECL macros to pass in templated
/// type with commas and the template declaration.
///
#define STD_TEMPLATE_CTR_STREAMABLE(type, template_decl)	\
    template_decl						\
    inline istream& operator>> (istream& is, type& v)		\
    { return (container_read (is, v)); } 			\
    template_decl						\
    inline ostream& operator<< (ostream& os, const type& v)	\
    { return (container_write (os, v)); } 			\
    template_decl						\
    inline ostringstream& operator<< (ostringstream& os, const type& v)	\
    { return (container_text_write (os, v)); }			\
    template_decl						\
    inline size_t stream_size_of (const type& v)		\
    { return (container_stream_size (v)); }

/// \brief Declares non-resizable container template \p type streamable.
#define STD_TEMPLATE_NR_CTR_STREAMABLE(type, template_decl)	\
    template_decl						\
    inline istream& operator>> (istream& is, type& v)		\
    { return (nr_container_read (is, v)); } 			\
    template_decl						\
    inline ostream& operator<< (ostream& os, const type& v)	\
    { return (nr_container_write (os, v)); } 			\
    template_decl						\
    inline ostringstream& operator<< (ostringstream& os, const type& v)	\
    { return (container_text_write (os, v)); }			\
    template_decl						\
    inline size_t stream_size_of (const type& v)		\
    { return (nr_container_stream_size (v)); }

//----------------------------------------------------------------------
// Fixed size container serialization.
//----------------------------------------------------------------------

/// Reads fixed size container \p v from stream \p is.
template <typename Container>
inline istream& nr_container_read (istream& is, Container& v)
{
    foreach (typename Container::iterator, i, v)
	is >> *i;
    return (is);
}

/// Writes fixed size container \p v into stream \p os.
template <typename Container>
inline ostream& nr_container_write (ostream& os, const Container& v)
{
    foreach (typename Container::const_iterator, i, v)
	os << *i;
    return (os);
}

/// Computes the stream size of a fixed size standard container.
template <typename Container>
size_t nr_container_stream_size (const Container& v)
{
    typedef typename Container::const_iterator vciter_t;
    typedef typename iterator_traits<vciter_t>::value_type value_type;
    size_t s = 0;
    if (numeric_limits<value_type>::is_integral)
	s += v.size() * stream_size_of(value_type());
    else {
	foreach (vciter_t, i, v)
	    s += stream_size_of(*i);
    }
    return (s);
}

//----------------------------------------------------------------------
// Resizable container serialization.
//----------------------------------------------------------------------

/// Reads container \p v from stream \p is.
template <typename Container>
istream& container_read (istream& is, Container& v)
{
    typedef typename Container::size_type size_type;
    typedef typename Container::value_type value_type;
    typedef typename Container::iterator iterator;
    size_type n;
    is >> n;
    const size_type expectedSize = n * stream_size_of(value_type());
#ifdef WANT_STREAM_BOUNDS_CHECKING
    if (expectedSize > is.remaining())
	throw stream_bounds_exception ("read", typeid(v).name(), is.pos(), expectedSize, is.remaining());
#else
    assert(expectedSize <= is.remaining());
#endif
    v.resize (n);
    nr_container_read (is, v);
    is.align();
    return (is);
}

/// Writes the vector to stream \p os.
template <typename Container>
ostream& container_write (ostream& os, const Container& v)
{
    os << v.size();
    nr_container_write (os, v);
    os.align();
    return (os);
}

/// Computes the stream size of a standard container.
template <typename Container>
size_t container_stream_size (const Container& v)
{
    return (Align (stream_size_of(v.size()) + nr_container_stream_size (v)));
}

/// \brief Writes element \p v into stream \p os as text.
/// Specialize to custom print elements.
template <typename T>
inline ostringstream& container_element_text_write (ostringstream& os, const T& v)
{ return (os << v); }

/// Writes container \p v into stream \p os as text.
template <typename Container>
ostringstream& container_text_write (ostringstream& os, const Container& v)
{
    typename Container::const_iterator i = v.begin();
    os << '(';
    while (i < v.end()) {
	container_element_text_write (os, *i);
	if (++i >= v.end()) break;
	os << ',';
    }
    os << ')';
    return (os);
}

//----------------------------------------------------------------------

} // namespace ustl

#endif

