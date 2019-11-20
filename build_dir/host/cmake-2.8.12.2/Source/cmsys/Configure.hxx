/*============================================================================
  KWSys - Kitware System Library
  Copyright 2000-2009 Kitware, Inc., Insight Software Consortium

  Distributed under the OSI-approved BSD License (the "License");
  see accompanying file Copyright.txt for details.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the License for more information.
============================================================================*/
#ifndef cmsys_Configure_hxx
#define cmsys_Configure_hxx

/* Include C configuration.  */
#include <cmsys/Configure.h>

/* Whether ANSI C++ stream headers are to be used.  */
#define cmsys_IOS_USE_ANSI 1

/* Whether ANSI C++ streams are in std namespace.  */
#define cmsys_IOS_HAVE_STD 1

/* Whether ANSI C++ <sstream> header is to be used.  */
#define cmsys_IOS_USE_SSTREAM 1

/* Whether old C++ <strstream.h> header is to be used.  */
#define cmsys_IOS_USE_STRSTREAM_H 0

/* Whether old C++ <strstrea.h> header is to be used.  */
#define cmsys_IOS_USE_STRSTREA_H 0

/* Whether C++ streams support the ios::binary openmode.  */
#define cmsys_IOS_HAVE_BINARY 1

/* Whether STL is in std namespace.  */
#define cmsys_STL_HAVE_STD 1

/* Whether the STL string has operator<< for ostream.  */
#define cmsys_STL_STRING_HAVE_OSTREAM 1

/* Whether the STL string has operator>> for istream.  */
#define cmsys_STL_STRING_HAVE_ISTREAM 1

/* Whether the STL string has operator!= for char*.  */
#define cmsys_STL_STRING_HAVE_NEQ_CHAR 1

/* Define the stl namespace macro.  */
#if cmsys_STL_HAVE_STD
# define cmsys_stl std
#else
# define cmsys_stl
#endif

/* Define the ios namespace macro.  */
#if cmsys_IOS_HAVE_STD
# define cmsys_ios_namespace std
#else
# define cmsys_ios_namespace
#endif
#if cmsys_IOS_USE_SSTREAM
# define cmsys_ios cmsys_ios_namespace
#else
# define cmsys_ios cmsys_ios
#endif

/* Define the ios::binary openmode macro.  */
#if cmsys_IOS_HAVE_BINARY
# define cmsys_ios_binary cmsys_ios::ios::binary
#else
# define cmsys_ios_binary 0
#endif

/* Whether the cstddef header is available.  */
#define cmsys_CXX_HAS_CSTDDEF 1

/* Whether the compiler supports null template arguments.  */
#define cmsys_CXX_HAS_NULL_TEMPLATE_ARGS 1

/* Define the null template arguments macro.  */
#if cmsys_CXX_HAS_NULL_TEMPLATE_ARGS
# define cmsys_CXX_NULL_TEMPLATE_ARGS <>
#else
# define cmsys_CXX_NULL_TEMPLATE_ARGS
#endif

/* Whether the compiler supports member templates.  */
#define cmsys_CXX_HAS_MEMBER_TEMPLATES 1

/* Whether the compiler supports argument dependent lookup.  */
#define cmsys_CXX_HAS_ARGUMENT_DEPENDENT_LOOKUP 1

/* Whether the compiler supports standard full specialization syntax.  */
#define cmsys_CXX_HAS_FULL_SPECIALIZATION 1

/* Define the specialization definition macro.  */
#if cmsys_CXX_HAS_FULL_SPECIALIZATION
# define cmsys_CXX_DEFINE_SPECIALIZATION template <>
#else
# define cmsys_CXX_DEFINE_SPECIALIZATION
#endif

/* Define typename keyword macro for use in declarations.  */
#if defined(_MSC_VER) && _MSC_VER < 1300
# define cmsys_CXX_DECL_TYPENAME
#else
# define cmsys_CXX_DECL_TYPENAME typename
#endif

/* Whether the stl has iterator_traits.  */
#define cmsys_STL_HAS_ITERATOR_TRAITS 1

/* Whether the stl has iterator_category.  */
#define cmsys_STL_HAS_ITERATOR_CATEGORY 0

/* Whether the stl has __iterator_category.  */
#define cmsys_STL_HAS___ITERATOR_CATEGORY 0

/* Whether the stl allocator is the standard template.  */
#define cmsys_STL_HAS_ALLOCATOR_TEMPLATE 1

/* Whether the stl allocator is not a template.  */
#define cmsys_STL_HAS_ALLOCATOR_NONTEMPLATE 0

/* Whether the stl allocator has rebind.  */
#define cmsys_STL_HAS_ALLOCATOR_REBIND 1

/* Whether the stl allocator has a size argument for max_size.  */
#define cmsys_STL_HAS_ALLOCATOR_MAX_SIZE_ARGUMENT 0

/* Whether the stl containers support allocator objects.  */
#define cmsys_STL_HAS_ALLOCATOR_OBJECTS 1

/* Whether struct stat has the st_mtim member for high resolution times.  */
#define cmsys_STAT_HAS_ST_MTIM 1

/* If building a C++ file in kwsys itself, give the source file
   access to the macros without a configured namespace.  */
#if defined(KWSYS_NAMESPACE)
# if !cmsys_NAME_IS_KWSYS
#  define kwsys_stl cmsys_stl
#  define kwsys_ios cmsys_ios
#  define kwsys     cmsys
#  define kwsys_ios_binary cmsys_ios_binary
# endif
# define KWSYS_NAME_IS_KWSYS            cmsys_NAME_IS_KWSYS
# define KWSYS_STL_HAVE_STD             cmsys_STL_HAVE_STD
# define KWSYS_IOS_HAVE_STD             cmsys_IOS_HAVE_STD
# define KWSYS_IOS_USE_ANSI             cmsys_IOS_USE_ANSI
# define KWSYS_IOS_USE_SSTREAM          cmsys_IOS_USE_SSTREAM
# define KWSYS_IOS_USE_STRSTREAM_H      cmsys_IOS_USE_STRSTREAM_H
# define KWSYS_IOS_USE_STRSTREA_H       cmsys_IOS_USE_STRSTREA_H
# define KWSYS_IOS_HAVE_BINARY          cmsys_IOS_HAVE_BINARY
# define KWSYS_STAT_HAS_ST_MTIM         cmsys_STAT_HAS_ST_MTIM
# define KWSYS_CXX_HAS_CSTDDEF          cmsys_CXX_HAS_CSTDDEF
# define KWSYS_STL_STRING_HAVE_OSTREAM  cmsys_STL_STRING_HAVE_OSTREAM
# define KWSYS_STL_STRING_HAVE_ISTREAM  cmsys_STL_STRING_HAVE_ISTREAM
# define KWSYS_STL_STRING_HAVE_NEQ_CHAR cmsys_STL_STRING_HAVE_NEQ_CHAR
# define KWSYS_CXX_NULL_TEMPLATE_ARGS   cmsys_CXX_NULL_TEMPLATE_ARGS
# define KWSYS_CXX_HAS_MEMBER_TEMPLATES cmsys_CXX_HAS_MEMBER_TEMPLATES
# define KWSYS_CXX_HAS_FULL_SPECIALIZATION cmsys_CXX_HAS_FULL_SPECIALIZATION
# define KWSYS_CXX_DEFINE_SPECIALIZATION cmsys_CXX_DEFINE_SPECIALIZATION
# define KWSYS_CXX_DECL_TYPENAME        cmsys_CXX_DECL_TYPENAME
# define KWSYS_STL_HAS_ALLOCATOR_REBIND cmsys_STL_HAS_ALLOCATOR_REBIND
# define KWSYS_STL_HAS_ALLOCATOR_MAX_SIZE_ARGUMENT cmsys_STL_HAS_ALLOCATOR_MAX_SIZE_ARGUMENT
# define KWSYS_CXX_HAS_ARGUMENT_DEPENDENT_LOOKUP cmsys_CXX_HAS_ARGUMENT_DEPENDENT_LOOKUP
# define KWSYS_STL_HAS_ITERATOR_TRAITS cmsys_STL_HAS_ITERATOR_TRAITS
# define KWSYS_STL_HAS_ITERATOR_CATEGORY cmsys_STL_HAS_ITERATOR_CATEGORY
# define KWSYS_STL_HAS___ITERATOR_CATEGORY cmsys_STL_HAS___ITERATOR_CATEGORY
# define KWSYS_STL_HAS_ALLOCATOR_TEMPLATE cmsys_STL_HAS_ALLOCATOR_TEMPLATE
# define KWSYS_STL_HAS_ALLOCATOR_NONTEMPLATE cmsys_STL_HAS_ALLOCATOR_NONTEMPLATE
# define KWSYS_STL_HAS_ALLOCATOR_OBJECTS cmsys_STL_HAS_ALLOCATOR_OBJECTS
#endif

#endif
