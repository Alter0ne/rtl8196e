/*============================================================================
  KWSys - Kitware System Library
  Copyright 2000-2009 Kitware, Inc., Insight Software Consortium

  Distributed under the OSI-approved BSD License (the "License");
  see accompanying file Copyright.txt for details.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the License for more information.
============================================================================*/
/*
 * Copyright (c) 1996
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 */
#ifndef cmsys_hash_fun_hxx
#define cmsys_hash_fun_hxx

#include <cmsys/Configure.hxx>
#include <cmsys/cstddef>        // size_t
#include <cmsys/stl/string>     // string

namespace cmsys
{

template <class _Key> struct hash { };

inline size_t _stl_hash_string(const char* __s)
{
  unsigned long __h = 0;
  for ( ; *__s; ++__s)
    __h = 5*__h + *__s;

  return size_t(__h);
}

cmsys_CXX_DEFINE_SPECIALIZATION
struct hash<char*> {
  size_t operator()(const char* __s) const { return _stl_hash_string(__s); }
};

cmsys_CXX_DEFINE_SPECIALIZATION
struct hash<const char*> {
  size_t operator()(const char* __s) const { return _stl_hash_string(__s); }
};

cmsys_CXX_DEFINE_SPECIALIZATION
  struct hash<cmsys_stl::string> {
  size_t operator()(const cmsys_stl::string & __s) const { return _stl_hash_string(__s.c_str()); }
};

#if !defined(__BORLANDC__)
cmsys_CXX_DEFINE_SPECIALIZATION
  struct hash<const cmsys_stl::string> {
  size_t operator()(const cmsys_stl::string & __s) const { return _stl_hash_string(__s.c_str()); }
};
#endif

cmsys_CXX_DEFINE_SPECIALIZATION
struct hash<char> {
  size_t operator()(char __x) const { return __x; }
};

cmsys_CXX_DEFINE_SPECIALIZATION
struct hash<unsigned char> {
  size_t operator()(unsigned char __x) const { return __x; }
};

cmsys_CXX_DEFINE_SPECIALIZATION
struct hash<signed char> {
  size_t operator()(unsigned char __x) const { return __x; }
};

cmsys_CXX_DEFINE_SPECIALIZATION
struct hash<short> {
  size_t operator()(short __x) const { return __x; }
};

cmsys_CXX_DEFINE_SPECIALIZATION
struct hash<unsigned short> {
  size_t operator()(unsigned short __x) const { return __x; }
};

cmsys_CXX_DEFINE_SPECIALIZATION
struct hash<int> {
  size_t operator()(int __x) const { return __x; }
};

cmsys_CXX_DEFINE_SPECIALIZATION
struct hash<unsigned int> {
  size_t operator()(unsigned int __x) const { return __x; }
};

cmsys_CXX_DEFINE_SPECIALIZATION
struct hash<long> {
  size_t operator()(long __x) const { return __x; }
};

cmsys_CXX_DEFINE_SPECIALIZATION
struct hash<unsigned long> {
  size_t operator()(unsigned long __x) const { return __x; }
};

// use long long or __int64
#if 1
cmsys_CXX_DEFINE_SPECIALIZATION
struct hash<long long> {
  size_t operator()(long long __x) const { return __x; }
};

cmsys_CXX_DEFINE_SPECIALIZATION
struct hash<unsigned long long> {
  size_t operator()(unsigned long long __x) const { return __x; }
};
#elif 0
cmsys_CXX_DEFINE_SPECIALIZATION
struct hash<__int64> {
  size_t operator()(__int64 __x) const { return __x; }
};
cmsys_CXX_DEFINE_SPECIALIZATION
struct hash<unsigned __int64> {
  size_t operator()(unsigned __int64 __x) const { return __x; }
};
#endif // use long long or __int64

} // namespace cmsys

#endif
