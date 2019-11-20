/*
 * elf_defs.hpp
 *
 * Copyright (C) 2007 Bastian Blank <waldi@debian.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef ELF_DEFS_HPP
#define ELF_DEFS_HPP

#include <stdint.h>

namespace Elf
{
  class file_class_32 { public: static const uint8_t id = 1; };
  class file_class_64 { public: static const uint8_t id = 2; };
  class file_data_2LSB { public: static const uint8_t id = 1; };
  class file_data_2MSB { public: static const uint8_t id = 2; };
  class section_type_UNDEFINED { };
  class section_type_STRTAB { public: static const uint32_t id = 3; };
  class section_type_DYNAMIC { public: static const uint32_t id = 6; };
  class section_type_DYNSYM { public: static const uint32_t id = 11; };
  class section_type_GNU_VERDEF { public: static const uint32_t id = 0x6ffffffd; };
  class section_type_GNU_VERNEED { public: static const uint32_t id = 0x6ffffffe; };
  class section_type_GNU_VERSYM { public: static const uint32_t id = 0x6fffffff; };
  class segment_type_UNDEFINED { };
  class segment_type_INTERP { public: static const uint8_t id = 3; };
}

#endif
