// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
// ustack.h
//

#ifndef USTACK_H_5242F5635322B2EC44A9AEE73022C6E9
#define USTACK_H_5242F5635322B2EC44A9AEE73022C6E9

namespace ustl {

/// \class stack ustack.h ustl.h
/// \ingroup Sequences
///
/// \brief Stack adapter to uSTL containers.
///
template <class Sequence>
class stack {
public:
    typedef typename Sequence::value_type	value_type;
    typedef typename Sequence::size_type	size_type;
    typedef typename Sequence::difference_type	difference_type;
    typedef typename Sequence::reference	reference;
    typedef typename Sequence::const_reference	const_reference;
    typedef typename Sequence::pointer		pointer;
public:
    inline			stack (void);
    explicit inline		stack (const Sequence& s);
    inline bool			empty (void) const;
    inline size_type		size (void) const;
    inline reference		top (void);
    inline const_reference	top (void) const;
    inline void			push (const value_type& v);
    inline void			pop (void);
    inline bool			operator== (const stack& s);
    inline bool			operator< (const stack& s);
private:
    Sequence			m_Storage;
};

/// Default constructor.
template <class Sequence>
inline stack<Sequence>::stack (void)
: m_Storage ()
{
}

/// Copies contents of \p s.
template <class Sequence>
inline stack<Sequence>::stack (const Sequence& s)
: m_Storage (s)
{
}

/// Returns true if empty.
template <class Sequence>
inline bool stack<Sequence>::empty (void) const
{
    return (m_Storage.empty());
}

/// Returns the number of elements.
template <class Sequence>
inline typename stack<Sequence>::size_type stack<Sequence>::size (void) const
{
    return (m_Storage.size());
}

/// Returns the topmost element.
template <class Sequence>
inline typename stack<Sequence>::reference stack<Sequence>::top (void)
{
    return (m_Storage.back());
}

/// Returns the topmost element.
template <class Sequence>
inline typename stack<Sequence>::const_reference stack<Sequence>::top (void) const
{
    return (m_Storage.back());
}

/// Pushes \p v on the stack.
template <class Sequence>
inline void stack<Sequence>::push (const value_type& v)
{
    m_Storage.push_back (v);
}

/// Pops the topmost element from the stack.
template <class Sequence>
inline void stack<Sequence>::pop (void)
{
    m_Storage.pop_back();
}

/// Compares to \p s.
template <class Sequence>
inline bool stack<Sequence>::operator== (const stack& s)
{
    return (m_Storage == s.m_Storage);
}

/// Compares to \p s.
template <class Sequence>
inline bool stack<Sequence>::operator< (const stack& s)
{
    return (m_Storage.size() < s.m_Storage.size());
}

} // namespace ustl)

#endif

