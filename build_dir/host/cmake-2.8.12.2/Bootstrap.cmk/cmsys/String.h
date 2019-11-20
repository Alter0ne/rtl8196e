/*============================================================================
  KWSys - Kitware System Library
  Copyright 2000-2009 Kitware, Inc., Insight Software Consortium

  Distributed under the OSI-approved BSD License (the "License");
  see accompanying file Copyright.txt for details.

  This software is distributed WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the License for more information.
============================================================================*/
#ifndef cmsys_String_h
#define cmsys_String_h

#include <cmsys/Configure.h>

#include <stddef.h> /* size_t */

/* Redefine all public interface symbol names to be in the proper
   namespace.  These macros are used internally to kwsys only, and are
   not visible to user code.  Use kwsysHeaderDump.pl to reproduce
   these macros after making changes to the interface.  */
#if !defined(KWSYS_NAMESPACE)
# define kwsys_ns(x) cmsys##x
# define kwsysEXPORT cmsys_EXPORT
#endif
#if !cmsys_NAME_IS_KWSYS
# define kwsysString_strcasecmp                kwsys_ns(String_strcasecmp)
# define kwsysString_strncasecmp               kwsys_ns(String_strncasecmp)
#endif

#if defined(__cplusplus)
extern "C"
{
#endif

/**
 * Compare two strings ignoring the case of the characters.  The
 * integer returned is negative, zero, or positive if the first string
 * is found to be less than, equal to, or greater than the second
 * string, respectively.
 */
kwsysEXPORT int kwsysString_strcasecmp(const char* lhs, const char* rhs);

/**
 * Identical to String_strcasecmp except that only the first n
 * characters are considered.
 */
kwsysEXPORT int kwsysString_strncasecmp(const char* lhs, const char* rhs,
                                        size_t n);

#if defined(__cplusplus)
} /* extern "C" */
#endif

/* If we are building a kwsys .c or .cxx file, let it use these macros.
   Otherwise, undefine them to keep the namespace clean.  */
#if !defined(KWSYS_NAMESPACE)
# undef kwsys_ns
# undef kwsysEXPORT
# if !cmsys_NAME_IS_KWSYS
#  undef kwsysString_strcasecmp
#  undef kwsysString_strncasecmp
# endif
#endif

#endif
