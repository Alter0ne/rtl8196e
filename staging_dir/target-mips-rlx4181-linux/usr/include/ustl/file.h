// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
// file.h
//

#ifndef FILE_H_7F0F8DAF079E06CC00B2A00E0C289BBC
#define FILE_H_7F0F8DAF079E06CC00B2A00E0C289BBC

#include "utypes.h"

namespace ustl {

/// \class file file.h ustl/file.h
/// \brief Wraps the libc file descriptor API with exception throwing.
/// \warning Don't use this! It's a crutch until ifstream is implemented.
class file {
public:
    enum EOpenMode {
	for_Reading,
	for_Writing,
	for_Appending,
	for_Last
    };
public:
		file (void);
	       ~file (void) throw();
    void	open (const char* filename, EOpenMode mode = for_Reading, mode_t perms = 0644);
    void	attach (int fd, const char* filename = "");
    void	close (void);
    void	read (void* p, off_t n);
    void	write (const void* p, off_t n);
    off_t	size (void) const;
    off_t	pos (void) const;
private:
    const char*	m_Filename;	///< Currently open filename.
    int		m_fd;		///< Currently open file descriptor.
};

}

#endif

