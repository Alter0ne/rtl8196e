// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
/// \file unew.h
///
/// \brief Same as \<new\>, but throws ustl:: exceptions.
//

#ifndef UNEW_H_11D237512B324C9C05A55DAF1BF086F1
#define UNEW_H_11D237512B324C9C05A55DAF1BF086F1

#include "uexception.h"

#ifdef WITHOUT_LIBSTDCPP
//
// These are replaceable signatures:
//  - normal single new and delete (no arguments, throw @c bad_alloc on error)
//  - normal array new and delete (same)
//  - @c nothrow single new and delete (take a @c nothrow argument, return
//    @c NULL on error)
//  - @c nothrow array new and delete (same)
//
//  Placement new and delete signatures (take a memory address argument,
//  does nothing) may not be replaced by a user's program.
//
void* operator new (size_t) throw (ustl::bad_alloc);
void* operator new[] (size_t) throw (ustl::bad_alloc);
void  operator delete (void*) throw();
void  operator delete[] (void*) throw();

// Default placement versions of operator new.
inline void* operator new (size_t, void* p) throw() { return (p); }
inline void* operator new[] (size_t, void* p) throw() { return (p); }

// Default placement versions of operator delete.
inline void  operator delete  (void*, void*) throw() { }
inline void  operator delete[](void*, void*) throw() { }

#endif	// WITHOUT_LIBSTDCPP

#endif

