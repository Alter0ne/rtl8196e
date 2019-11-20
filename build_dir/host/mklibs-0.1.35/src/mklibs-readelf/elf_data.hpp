/*
 * elf_data.hpp
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

#ifndef ELF_DATA_HPP
#define ELF_DATA_HPP

#include "elf.hpp"
#include "elf_endian.hpp"

#include <stdexcept>
#include <string>
#include <vector>

#include <elf.h>
#include <stdint.h>

namespace Elf
{
  namespace
  {
    template <typename _class>
      struct _elfdef
      { };

    template <>
      struct _elfdef<file_class_32>
      {
        typedef Elf32_Dyn Dyn;
        typedef Elf32_Ehdr Ehdr;
        typedef Elf32_Phdr Phdr;
        typedef Elf32_Shdr Shdr;
        typedef Elf32_Sym Sym;
        typedef Elf32_Verdaux Verdaux;
        typedef Elf32_Verdef Verdef;
        typedef Elf32_Vernaux Vernaux;
        typedef Elf32_Verneed Verneed;
        typedef Elf32_Versym Versym;

        static inline uint8_t st_bind(uint8_t st_info) throw ()
        {
          return ELF32_ST_BIND(st_info);
        }

        static inline uint8_t st_type(uint8_t st_info) throw ()
        {
          return ELF32_ST_TYPE(st_info);
        }
      };

    template <>
      struct _elfdef<file_class_64>
      {
        typedef Elf64_Dyn Dyn;
        typedef Elf64_Ehdr Ehdr;
        typedef Elf64_Phdr Phdr;
        typedef Elf64_Shdr Shdr;
        typedef Elf64_Sym Sym;
        typedef Elf64_Verdaux Verdaux;
        typedef Elf64_Verdef Verdef;
        typedef Elf64_Vernaux Vernaux;
        typedef Elf64_Verneed Verneed;
        typedef Elf64_Versym Versym;

        static inline uint8_t st_bind(uint8_t st_info) throw ()
        {
          return ELF64_ST_BIND(st_info);
        }

        static inline uint8_t st_type(uint8_t st_info) throw ()
        {
          return ELF64_ST_TYPE(st_info);
        }
      };
  }

  template <typename _class, typename _data>
    class file_data : public file
    {
      public:
        file_data(uint8_t *, size_t len) throw (std::bad_alloc, std::runtime_error);

        const uint8_t get_class() const throw () { return _class::id; }
        const uint8_t get_data() const throw () { return _data::id; }
    };

  template <typename _class, typename _data>
    class section_data : public virtual section
    {
      private:
        typedef typename _elfdef<_class>::Shdr Shdr;

      public:
        section_data(Shdr *, uint8_t *) throw ();

        virtual void update(const file &) throw (std::bad_alloc);
    };

  template <typename _class, typename _data, typename _type>
    class section_real : public section_data<_class, _data>, public section_type<_type>
    {
      private:
        typedef typename _elfdef<_class>::Shdr Shdr;

      public:
        section_real(Shdr *a, uint8_t *b) throw ()
        : section_data<_class, _data>(a, b)
        { }
    };

  template <typename _class, typename _data>
    class section_real<_class, _data, section_type_DYNAMIC>
    : public section_data<_class, _data>,
      public section_type<section_type_DYNAMIC>
    {
      private:
        typedef typename _elfdef<_class>::Shdr Shdr;

      public:
        section_real(Shdr *, uint8_t *) throw (std::bad_alloc);

        void update(const file &) throw (std::bad_alloc);
    };

  template <typename _class, typename _data>
    class section_real<_class, _data, section_type_DYNSYM>
    : public section_data<_class, _data>,
      public section_type<section_type_DYNSYM>
    {
      private:
        typedef typename _elfdef<_class>::Shdr Shdr;

      public:
        section_real(Shdr *, uint8_t *) throw (std::bad_alloc);

        void update(const file &) throw (std::bad_alloc);
    };

  template <typename _class, typename _data>
    class section_real<_class, _data, section_type_GNU_VERDEF>
    : public section_data<_class, _data>,
      public section_type<section_type_GNU_VERDEF>
    {
      private:
        typedef typename _elfdef<_class>::Shdr Shdr;

      public:
        section_real(Shdr *, uint8_t *) throw (std::bad_alloc);

        void update(const file &) throw (std::bad_alloc);
    };

  template <typename _class, typename _data>
    class section_real<_class, _data, section_type_GNU_VERNEED>
    : public section_data<_class, _data>,
      public section_type<section_type_GNU_VERNEED>
    {
      private:
        typedef typename _elfdef<_class>::Shdr Shdr;

      public:
        section_real(Shdr *, uint8_t *) throw (std::bad_alloc);

        void update(const file &) throw (std::bad_alloc);
    };

  template <typename _class, typename _data>
    class section_real<_class, _data, section_type_GNU_VERSYM>
    : public section_data<_class, _data>,
      public section_type<section_type_GNU_VERSYM>
    {
      private:
        typedef typename _elfdef<_class>::Shdr Shdr;

      public:
        section_real(Shdr *, uint8_t *) throw (std::bad_alloc);
    };

  template <typename _class, typename _data>
    class segment_data : public virtual segment
    {
      private:
        typedef typename _elfdef<_class>::Phdr Phdr;

      public:
        segment_data (Phdr *, uint8_t *) throw ();
    };

  template <typename _class, typename _data, typename _type>
    class segment_real : public segment_data<_class, _data>, public segment_type<_type>
    {
      private:
        typedef typename _elfdef<_class>::Phdr Phdr;

      public:
        segment_real (Phdr *a, uint8_t *b) throw () : segment_data<_class, _data> (a, b) { }
    };

  template <typename _class, typename _data>
    class segment_real<_class, _data, segment_type_INTERP>
    : public segment_data<_class, _data>, public segment_type<segment_type_INTERP>
    {
      private:
        typedef typename _elfdef<_class>::Phdr Phdr;

      public:
        segment_real (Phdr *, uint8_t *) throw (std::bad_alloc);
    };

  template <typename _class, typename _data>
    class dynamic_data : public dynamic
    {
      private:
        typedef typename _elfdef<_class>::Dyn Dyn;

      public:
        dynamic_data (Dyn *) throw ();

        void update_string(const section_type<section_type_STRTAB> &) throw (std::bad_alloc);
    };

  template <typename _class, typename _data>
    class symbol_data : public symbol
    {
      private:
        typedef typename _elfdef<_class>::Sym Sym;

      public:
        symbol_data (Sym *) throw ();

        void update_string(const section_type<section_type_STRTAB> &) throw (std::bad_alloc);
        virtual void update_version (const file &, uint16_t) throw (std::bad_alloc);
    };

  template <typename _class, typename _data>
    class version_definition_data : public version_definition
    {
      public:
        typedef typename _elfdef<_class>::Verdaux Verdaux;
        typedef typename _elfdef<_class>::Verdef Verdef;

        version_definition_data (Verdef *) throw ();

        void update_string(const section_type<section_type_STRTAB> &) throw (std::bad_alloc);
    };

  template <typename _class, typename _data>
    class version_requirement_data : public version_requirement
    {
      public:
        typedef typename _elfdef<_class>::Vernaux Vernaux;
        typedef typename _elfdef<_class>::Verneed Verneed;

        version_requirement_data (Verneed *) throw ();

        void update_string(const section_type<section_type_STRTAB> &) throw (std::bad_alloc);
    };

  template <typename _class, typename _data>
    class version_requirement_entry_data : public version_requirement_entry
    {
      public:
        typedef typename _elfdef<_class>::Vernaux Vernaux;

        version_requirement_entry_data (Vernaux *, const version_requirement &) throw ();

        void update_string(const section_type<section_type_STRTAB> &) throw (std::bad_alloc);
    };
}

#endif
