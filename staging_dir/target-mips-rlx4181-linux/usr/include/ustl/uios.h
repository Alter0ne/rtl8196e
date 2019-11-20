// This file is part of the ustl library, an STL implementation.
//
// Copyright (C) 2005 by Mike Sharov <msharov@users.sourceforge.net>
// This file is free software, distributed under the MIT License.
//
// uios.h
//
// Types used by the streams for option setting.
//

#ifndef UIOS_H_630C16E316F7650E3A02E1C6611B789A
#define UIOS_H_630C16E316F7650E3A02E1C6611B789A

namespace ustl {

const char endl = '\n';		///< End of line character.
const char ends = '\0';		///< End of string character.

/// Defines types and constants used by all stream classes.
namespace ios {
    /// Used to set parameters for stringstreams
    enum fmtflags {
	boolalpha	= (1 << 0),
	dec		= (1 << 1),
	fixed		= (1 << 2),
	hex		= (1 << 3),
	internal	= (1 << 4),
	left		= (1 << 5),
	oct		= (1 << 6),
	right		= (1 << 7),
	scientific	= (1 << 8),
	showbase	= (1 << 9),
	showpoint	= (1 << 10),
	showpos		= (1 << 11),
	skipws		= (1 << 12),
	unitbuf		= (1 << 13),
	uppercase	= (1 << 14),
	adjustfield	= (1 << 15),
	basefield	= (1 << 16),
	floatfield	= (1 << 17)
    };
    /// For file-based streams, specifies fd mode.
    enum openmode {
	in	= (1 << 0),
	out	= (1 << 1),
	app	= (1 << 2),
	ate	= (1 << 3),
	binary	= (1 << 4),
	trunc	= (1 << 5)
    };
    /// Seek directions, equivalent to SEEK_SET, SEEK_CUR, and SEEK_END.
    enum seekdir {
	beg,
	cur,
	end
    };

    /// Default word delimiters for stringstreams.
    extern const char* c_DefaultDelimiters;
} // namespace ios
} // namespace ustl

#endif

