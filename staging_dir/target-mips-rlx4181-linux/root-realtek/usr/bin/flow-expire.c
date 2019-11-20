/*
 * Copyright (c) 2001 Mark Fullmer and The Ohio State University
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *      $Id: flow-expire.c,v 1.16 2003/04/02 18:03:01 maf Exp $
 */

#include "ftconfig.h"
#include <ftlib.h>

#include <sys/types.h>
#include <sys/param.h>
#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <syslog.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>

#if HAVE_STRINGS_H
 #include <strings.h>
#endif

#if HAVE_STRING_H
  #include <string.h>
#endif

#include "ftbuild.h"

char *progname = "flow-expire";
int debug;

void usage(void);

int main(int argc, char **argv)
{
  struct ftfile_entries fte;
  int i, doit;
  char wpname[MAXPATHLEN+1];

  bzero(&fte, sizeof fte);

  /* init fterr */
  fterr_setid(argv[0]);

  fte.max_files = 0;
  fte.max_bytes = 0;
  wpname[0] = '.'; wpname[1] = 0;

  while ((i = getopt(argc, argv, "w:d:e:E:h?")) != -1)

    switch (i) {

    case 'd': /* debug */
      debug = atoi(optarg);
      break;

    case 'e': /* expire */
      fte.max_files = atoi(optarg);
      break;

    case 'E': /* size of flow files */
      if ((fte.max_bytes = scan_size(optarg)) == -1)
        fterr_errx(1, "scan_size(): failed");
      break;

    case 'w': /* output pathname */
      if (strlen(optarg) > (MAXPATHLEN))
        fterr_errx(1, "Pathname too long");
      strcpy(wpname, optarg);
      break;

    case '?':
    case 'h':
      usage();
      exit (0);

    default:
      usage();
      exit (1);

    } /* switch */

  if (argc - optind)
    fterr_errx(1, "Extra arguments starting with %s.", argv[optind]);

  if (debug) {
    doit = 0;
  }
  else {

    /* enable syslog */
    fterr_setsyslog(1, LOG_PID|LOG_NDELAY, LOG_LOCAL6);

    /* disable stderr */
    fterr_setfile(0, (void*)0L);

    doit = 1;
  }


  if (ftfile_loaddir(&fte, wpname,
    FT_FILE_SORT|FT_FILE_INIT|FT_FILE_CHECKNAMES)) {
    fterr_errx(1, "ftfile_scandir(): failed");
  }
  if (debug)
    ftfile_dump(&fte);
  if (ftfile_expire(&fte, doit, (u_int32)0)) {
    fterr_errx(1, "ftfile_expire(): failed");
  }
  if (debug)
    ftfile_dump(&fte);

  /* free storage allocated to fte */
  ftfile_free(&fte);

  return 0;

} /* main */

void usage(void)
{
  fprintf(stderr, "Usage: flow expire [-h] [-d debug_level] [-e expire_count]\n");
  fprintf(stderr, "       [-E expire_size[bKMG]] -w workdir\n");
  fprintf(stderr, "\n%s version %s: built by %s\n", PACKAGE, VERSION, FT_PROG_BUILD);

} /* usage */
