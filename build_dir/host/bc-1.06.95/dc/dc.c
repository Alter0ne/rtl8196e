/*
 * implement the "dc" Desk Calculator language.
 *
 * Copyright (C) 1994, 1997, 1998, 2000, 2003, 2006 Free Software Foundation, Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, you can either send email to this
 * program's author (see below) or write to:
 *   The Free Software Foundation, Inc.
 *   51 Franklin Street, Fifth Floor
 *   Boston, MA 02110-1301  USA
 */

/* Written with strong hiding of implementation details
 * in their own specialized modules.
 */
/* This module contains the argument processing/main functions.
 */

#include "config.h"

#include <stdio.h>
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
# include <string.h>
#else
# ifdef HAVE_STRINGS_H
#  include <strings.h>
# endif
#endif
#ifdef HAVE_FSTAT
# include <sys/types.h>
# include <sys/stat.h>
#endif
#include <getopt.h>
#include "dc.h"
#include "dc-proto.h"

#ifndef EXIT_SUCCESS	/* C89 <stdlib.h> */
# define EXIT_SUCCESS	0
#endif
#ifndef EXIT_FAILURE	/* C89 <stdlib.h> */
# define EXIT_FAILURE	1
#endif

const char *progname;	/* basename of program invocation */

static void
bug_report_info DC_DECLVOID()
{
	printf("Email bug reports to:  bug-dc@gnu.org .\n");
}

static void
show_version DC_DECLVOID()
{
	printf("dc (GNU %s %s) %s\n", PACKAGE, VERSION, DC_VERSION);
	printf("\n%s\n\
This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE,\n\
to the extent permitted by law.\n", DC_COPYRIGHT);
}

/* your generic usage function */
static void
usage DC_DECLARG((f))
	FILE *f DC_DECLEND
{
	fprintf(f, "\
Usage: %s [OPTION] [file ...]\n\
  -e, --expression=EXPR    evaluate expression\n\
  -f, --file=FILE          evaluate contents of file\n\
  -h, --help               display this help and exit\n\
  -V, --version            output version information and exit\n\
\n\
", progname);
	bug_report_info();
}

/* returns a pointer to one past the last occurance of c in s,
 * or s if c does not occur in s.
 */
static char *
r1bindex DC_DECLARG((s, c))
	char *s DC_DECLSEP
	int  c DC_DECLEND
{
	char *p = strrchr(s, c);

	if (p == NULL)
		return s;
	return p + 1;
}

static void
try_file(const char *filename)
{
	FILE *input;

	if (strcmp(filename, "-") == 0) {
		input = stdin;
	} else if ( (input=fopen(filename, "r")) == NULL ) {
		fprintf(stderr, "%s: Could not open file %s\n", progname, filename);
		return;
	}
	{
	    /* Several complaints have been filed about dc's silence
	     * when a "cd" typo is made.  I really wanted to avoid
	     * this mess, but I guess it really should be added...
	     */
#ifndef HAVE_FSTAT
	    /* non-POSIXish system; this code _might_ notice a directory */
	    int c = getc(input);
	    if (c == EOF && ferror(input)) {
			perror(filename);
			goto close;
	    }
	    ungetc(c, input);

#else /* HAVE_FSTAT */
  /* If HAVE_FSTAT and no S_IS*() macros, it must be a pre-POSIX
   * Unix-ish system?
   */
# ifndef S_ISREG
#  ifdef S_IFREG
#    define S_ISREG(m)	(((m)&S_IFMT)==S_IFREG)
#   else
#    define S_ISREG(m)	0
#  endif
# endif
# ifndef S_ISCHR
#  ifdef S_IFCHR
#   define S_ISCHR(m)	(((m)&S_IFMT)==S_IFCHR)
#  endif
# endif
# ifndef S_ISFIFO
#  ifdef S_IFIFO
#   define S_ISFIFO(m)	(((m)&S_IFMT)==S_IFIFO)
#  endif
# endif
# ifndef S_ISSOCK
#  ifdef S_IFSOCK
#   define S_ISSOCK(m)	(((m)&S_IFMT)==S_IFSOCK)
#  endif
# endif
# ifndef S_ISDIR
#  ifdef S_IFDIR
#   define S_ISDIR(m)	(((m)&S_IFMT)==S_IFDIR)
#  endif
# endif
# ifndef S_ISBLK
#  ifdef S_IFBLK
#   define S_ISBLK(m)	(((m)&S_IFMT)==S_IFBLK)
#  endif
# endif
	    struct stat s;
	    if (fstat(fileno(input), &s) == -1) {
			/* "can't happen" */
			fprintf(stderr, "%s: Could not fstat file ", progname);
			perror(filename);
			goto close;
	    }

#ifdef S_ISDIR
		if (S_ISDIR(s.st_mode)) {
			fprintf(stderr,
				"%s: Will not attempt to process directory %s\n",
				progname, filename);
			goto close;
		} else
#endif
#ifdef S_ISBLK
		if (S_ISBLK(s.st_mode)) {
			fprintf(stderr,
				"%s: Will not attempt to process block-special file %s\n",
				progname, filename);
			goto close;
		} else
#endif
	    if (!S_ISREG(s.st_mode)
# ifdef S_ISCHR
			/* typically will be /dev/null or some sort of tty */
			&& !S_ISCHR(s.st_mode)
# endif
# ifdef S_ISFIFO
			&& !S_ISFIFO(s.st_mode)
# endif
# ifdef S_ISSOCK
			&& !S_ISSOCK(s.st_mode)
# endif
				) {
			fprintf(stderr,
				"%s: Will not attempt to process file of unusual type: %s\n",
				progname, filename);
			goto close;
	    }
#endif /* HAVE_FSTAT */
	}
	if (dc_evalfile(input) != DC_SUCCESS)
		exit(EXIT_FAILURE);
close:
	if (input != stdin)
		fclose(input);
}



/* Check to see if there were any output errors; if so, then give
 * an error message (if stderr is not known to be unhappy), and
 * ensure that the program exits with an error indication.
 */
static int
flush_okay DC_DECLVOID()
{
	const char *errmsg = NULL;
	int r = EXIT_SUCCESS;

	if (ferror(stdout))
		errmsg = "error writing to stdout";
	else if (fflush(stdout))
		errmsg = "error flushing stdout";
	else if (fclose(stdout))
		errmsg = "error closing stdout";

	if (errmsg) {
		fprintf(stderr, "%s: ", progname);
		perror(errmsg);
		r = EXIT_FAILURE;
	}

	if (ferror(stderr) || fclose(stderr))
		r = EXIT_FAILURE;
	return r;
}


int
main DC_DECLARG((argc, argv))
	int  argc DC_DECLSEP
	char **argv DC_DECLEND
{
	static struct option const long_opts[] = {
		{"expression", required_argument, NULL, 'e'},
		{"file", required_argument, NULL, 'f'},
		{"help", no_argument, NULL, 'h'},
		{"version", no_argument, NULL, 'V'},
		{NULL, 0, NULL, 0}
	};
	int did_eval = 0;
	int c;

	progname = r1bindex(*argv, '/');
#ifdef HAVE_SETVBUF
	/* attempt to simplify interaction with applications such as emacs */
	(void) setvbuf(stdout, NULL, _IOLBF, 0);
#endif
	dc_math_init();
	dc_string_init();
	dc_register_init();
	dc_array_init();

	while ((c = getopt_long(argc, argv, "hVe:f:", long_opts, (int *)0)) != EOF) {
		switch (c) {
		case 'e':
			{	dc_data string = dc_makestring(optarg, strlen(optarg));
				if (dc_evalstr(&string) != DC_SUCCESS)
					return flush_okay();
				dc_free_str(&string.v.string);
				did_eval = 1;
			}
			break;
		case 'f':
			try_file(optarg);
			did_eval = 1;
			break;
		case 'h':
			usage(stdout);
			return flush_okay();
		case 'V':
			show_version();
			return flush_okay();
		default:
			usage(stderr);
			return EXIT_FAILURE;
		}
	}

	for (; optind < argc; ++optind) {
		try_file(argv[optind]);
		did_eval = 1;
	}
	if (did_eval == 0) {
		/* if no -e commands and no command files, then eval stdin */
		if (dc_evalfile(stdin) != DC_SUCCESS)
			return EXIT_FAILURE;
	}
	return flush_okay();
}


/*
 * Local Variables:
 * mode: C
 * tab-width: 4
 * End:
 * vi: set ts=4 :
 */
