from __future__ import absolute_import

from . import logging
import optparse
import sys


class OptionParser(optparse.OptionParser):
    def get_prog_name(self):
        if self.prog is None:
            return sys.argv[0]
        else:
            return self.prog

    def error(self, msg):
        prog = self.get_prog_name()
        sys.stderr.write("%s: %s\nTry `%s --help' for more information.\n"% (
                prog, msg, prog,
            )
        )
        self.exit(2)


class MainBase(object):
    option_parser = OptionParser('%prog [OPTION]... -d DEST INPUT...', add_help_option=False)
    option_parser.add_option('-d', '--dest-dir',
        dest='dest_dir',
        help='create libraries in DIRECTORY',
        metavar='DIRECTORY',
    )
    option_parser.add_option('-D', '--no-default-lib',
        action='store_true',
        dest='omit_library_dir',
        help='omit default libpath',
    )
    def option_callback_library_dir(option, opt, value, parser):
        parser.values.library_dir.extend(value.split(':'))
    option_parser.add_option('-L',
        action='callback',
        callback=option_callback_library_dir,
        default=[],
        dest='library_dir',
        help='add DIRECTORY(s) to the library search path',
        metavar='DIRECTORY[:DIRECTORY]...',
        type='string',
    )
    option_parser.add_option('-l',
        metavar='LIBRARY'
    )
    option_parser.add_option('--ldlib',
        dest='ldlib',
        help='use LDLIB for the dynamic linker',
        metavar='LDLIB',
    )
    option_parser.add_option('--root',
        dest='root',
        help='search in DIRECTORY for library rpaths',
        metavar='DIRECTORY',
    )
    option_parser.add_option('-v', '--verbose',
        action='count',
        dest='verbose',
        help='explain what is being done',
    )
    option_parser.add_option('-h', '--help',
        action='help',
        help='display this help and exit',
    )
    option_parser.add_option('-V', '--version',
        action='version',
        help='output version information and exit',
    )

    def __init__(self):
        self.options, self.files = self.option_parser.parse_args()

        level = logging.WARNING
        if self.options.verbose == 1:
            level = logging.INFO
        elif self.options.verbose == 2:
            level = logging.VERBOSE
        elif self.options.verbose >= 3:
            level = logging.DEBUG
        logging.basicConfig(level=level, format="%(levelname)s: %(message)s")

