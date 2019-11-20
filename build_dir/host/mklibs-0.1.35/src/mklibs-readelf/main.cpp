#include <config.h>

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <vector>

#include <elf.h>
#include <getopt.h>

#include "elf.hpp"

enum
{ 
  GETOPT_FIRST = 256,
  GETOPT_HELP,
  GETOPT_VERSION,
};

static struct option const long_opts[] =
{ 
  {"print-elf-header", no_argument, 0, 'e'},
  {"print-interp", no_argument, 0, 'i'},
  {"print-needed", no_argument, 0, 'n'},
  {"print-rpath", no_argument, 0, 'R'},
  {"print-soname", no_argument, 0, 's'},
  {"print-symbols-provided", no_argument, 0, 'p'},
  {"print-symbols-undefined", no_argument, 0, 'u'},
  {"help", no_argument, 0, GETOPT_HELP},
  {"version", no_argument, 0, GETOPT_VERSION},
  {0, 0, 0, 0}
};

char *program_name;

enum command
{
  COMMAND_PRINT_ELF_HEADER,
  COMMAND_PRINT_INTERP,
  COMMAND_PRINT_NEEDED,
  COMMAND_PRINT_RPATH,
  COMMAND_PRINT_SONAME,
  COMMAND_PRINT_SYMBOLS_PROVIDED,
  COMMAND_PRINT_SYMBOLS_UNDEFINED,
};

static void process_elf_header (Elf::file *file)
{
  std::cout
    << (unsigned int) file->get_class () << ' '
    << (unsigned int) file->get_data () << ' '
    << file->get_machine () << ' '
    << file->get_flags () << '\n';
}

static void process_dynamics (Elf::file *file, int64_t tag)
{
  const Elf::section_type<Elf::section_type_DYNAMIC> *section = file->get_section_DYNAMIC ();
  if (!section)
    return;

  for (std::vector<Elf::dynamic *>::const_iterator it = section->get_dynamics ().begin (); it != section->get_dynamics ().end (); ++it)
  {
    Elf::dynamic *dynamic = *it;
    if (dynamic->get_tag () == tag)
      std::cout << dynamic->get_val_string () << '\n';
  }
}

static void process_symbols_provided (const Elf::section_type<Elf::section_type_DYNSYM> *section)
{
  if (!section)
    return;

  for (std::vector<Elf::symbol *>::const_iterator it = section->get_symbols ().begin (); it != section->get_symbols ().end (); ++it)
  {
    const Elf::symbol *symbol = *it;
    uint8_t bind = symbol->get_bind ();
    uint16_t shndx = symbol->get_shndx ();
    uint8_t type = symbol->get_type ();
    const std::string &name = symbol->get_name_string ();

    if (bind != STB_GLOBAL && bind != STB_WEAK)
      continue;
    if (shndx == SHN_UNDEF || shndx == SHN_ABS)
      continue;
    if (type != STT_NOTYPE && type != STT_OBJECT && type != STT_FUNC && type != STT_COMMON && type != STT_TLS)
      continue;
    if (!name.size())
      continue;

    std::cout <<
      name <<
      ' ' << (bind == STB_WEAK ? "True" : "False") <<
      ' ' << symbol->get_version() <<
      ' ' << (symbol->get_version_data() & 0x8000 ? "False" : "True") <<
      '\n';
  }
}

static void process_symbols_undefined (const Elf::section_type<Elf::section_type_DYNSYM> *section)
{
  if (!section)
    return;

  for (std::vector<Elf::symbol *>::const_iterator it = section->get_symbols ().begin (); it != section->get_symbols ().end (); ++it)
  {
    const Elf::symbol *symbol = *it;
    uint8_t bind = symbol->get_bind ();
    uint16_t shndx = symbol->get_shndx ();
    uint8_t type = symbol->get_type ();
    const std::string &name = symbol->get_name_string ();

    if (bind != STB_GLOBAL && bind != STB_WEAK)
      continue;
    if (shndx != SHN_UNDEF)
      continue;
    if (type != STT_NOTYPE && type != STT_OBJECT && type != STT_FUNC && type != STT_COMMON && type != STT_TLS)
      continue;
    if (!name.size())
      continue;

    std::cout <<
      name << 
      ' ' << (bind == STB_WEAK ? "True" : "False") <<
      ' ' << symbol->get_version() <<
      ' ' << symbol->get_version_file() <<
      '\n';
  }
}

static void process (command cmd, const char *filename)
{
  Elf::file *file = Elf::file::open (filename);

  switch (cmd)
  {
    case COMMAND_PRINT_ELF_HEADER:
      process_elf_header (file);
      break;
    case COMMAND_PRINT_INTERP:
      if (file->get_segment_INTERP ())
        std::cout << file->get_segment_INTERP ()->get_interp () << '\n';
      break;
    case COMMAND_PRINT_NEEDED:
      process_dynamics (file, DT_NEEDED);
      break;
    case COMMAND_PRINT_RPATH:
      process_dynamics (file, DT_RPATH);
      break;
    case COMMAND_PRINT_SONAME:
      process_dynamics (file, DT_SONAME);
      break;
    case COMMAND_PRINT_SYMBOLS_PROVIDED:
      process_symbols_provided (file->get_section_DYNSYM ());
      break;
    case COMMAND_PRINT_SYMBOLS_UNDEFINED:
      process_symbols_undefined (file->get_section_DYNSYM ());
      break;
  }
}

static void usage (int status) __attribute__ ((noreturn));
static void usage (int status)
{
  if (status != 0)
    fprintf (stderr, "Try `%s --help' for more information.\n", program_name);
  else
  {
    fprintf (stdout, "\
Usage: %s [OPTION]... FILE...\n\
Prints informations about ELF shared objects and executables.\n\
\n\
", program_name);
    fputs ("\
Mandatory arguments to long options are mandatory for short options too.\n\
", stdout);
    fputs ("\
  -i, --print-interp                    Print interpreter of executable\n\
  -n, --print-needed                    Print needed shared libs\n\
  -R, --print-rpath                     Print rpath setting\n\
  -s, --print-soname                    Print soname of shared object\n\
  -p, --print-symbols-provided          Print provided symbols\n\
  -u, --print-symbols-undefined         Print undefined symbols\n\
      --help                            Display this help and exit\n\
      --version                         Output version information and exit\n\
", stdout);
  }
  exit (status);
}

int main (int argc, char *argv[])
{
  int c;
  command cmd = COMMAND_PRINT_SYMBOLS_PROVIDED;

  program_name = argv[0];

  while ((c = getopt_long (argc, argv, "einpRsu", long_opts, NULL)) != -1)
  {
    switch (c)
    { 
      case 0:
        break;
      case 'e':
        cmd = COMMAND_PRINT_ELF_HEADER;
        break;
      case 'i':
        cmd = COMMAND_PRINT_INTERP;
        break;
      case 'n':
        cmd = COMMAND_PRINT_NEEDED;
        break;
      case 'p':
        cmd = COMMAND_PRINT_SYMBOLS_PROVIDED;
        break;
      case 'R':
        cmd = COMMAND_PRINT_RPATH;
        break;
      case 's':
        cmd = COMMAND_PRINT_SONAME;
        break;
      case 'u':
        cmd = COMMAND_PRINT_SYMBOLS_UNDEFINED;
        break;
      case GETOPT_HELP:
        usage (EXIT_SUCCESS);
      case GETOPT_VERSION:
        fputs (PACKAGE_STRING, stdout);
        fputs ("\n\n\
This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\
", stdout);
        exit (EXIT_SUCCESS);
        break;
      default:
        usage (EXIT_FAILURE);
    }
  }

  for (int i = optind; i < argc; ++i)
    process (cmd, argv[i]);
}
