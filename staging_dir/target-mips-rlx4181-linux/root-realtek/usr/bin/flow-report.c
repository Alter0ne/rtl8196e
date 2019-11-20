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
 *      $Id: flow-report.c,v 1.9 2004/01/05 17:59:41 maf Exp $
 */

#include "ftconfig.h"
#include <ftlib.h>

#include <sys/types.h>
#include <sys/signal.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

#if HAVE_STRINGS_H
 #include <strings.h>
#endif

#if HAVE_STRING_H
  #include <string.h>
#endif

#include "ftbuild.h"

int debug;
void usage(void);

int sig_pipe_flag;
void sig_pipe(int);

int main(int argc, char **argv)
{
  struct ftio ftio;
  struct ftprof ftp;
  struct ftstat ftstat;
  struct ftstat_def *ftsd;
  struct ftver ftv;
  struct ftvar ftvar;
  struct ftset ftset;
  struct fts3rec_offsets fo;
  char *rec;
  char *fname, *dname;
  u_int32 total_flows;
  int i, split, done;
  int usage_call;

  /* init fterr */
  fterr_setid(argv[0]);

  bzero(&ftv, sizeof ftv);
  bzero(&ftvar, sizeof ftvar);
  total_flows = 0;
  usage_call = 0;

  /* init var binding */
  if (ftvar_new(&ftvar) < 0)
    fterr_errx(1, "ftvar_new(): failed");

  fname = FT_PATH_CFG_STAT;
  dname = "default";

  /* configure signal handler */
  if (mysignal(SIGPIPE, sig_pipe) == SIG_ERR)
    fterr_err(1, "signal(SIGPIPE)");

  /* defaults + no compression */
  ftset_init(&ftset, 0);

  while ((i = getopt(argc, argv, "b:C:d:h?s:S:kz:v:")) != -1)

    switch (i) {

    case 'd': /* debug */
      debug = atoi(optarg);
      break;

    case 's': /* stat file name */
      fname = optarg;
      break;

    case 'S': /* stat definition name */
      dname = optarg;
      break;

    case 'v': /* variable */
      if (ftvar_pset(&ftvar, optarg) < 0)
        fterr_errx(1, "ftvar_pset(%s): failed", optarg);
      break;

    case 'h': /* help */
    case '?': 
      usage();
      ++usage_call;
      break;

    default:
      usage();
      exit(1);
      break;

    } /* switch */

  if (argc - optind)
    fterr_errx(1, "Extra arguments starting with %s.", argv[optind]);

  if (usage_call)
    exit(0);

  /* initialize and load stats config */
  if (ftstat_load(&ftstat, &ftvar, fname))
    fterr_errx(1, "ftstat_load(): failed");

  if (!(ftsd = ftstat_def_find(&ftstat, dname))) 
    fterr_errx(1, "ftstat_find_def(%s): failed", dname);

  /* input is stdin */
  if (ftio_init(&ftio, 0, FT_IO_FLAG_READ) < 0)
    fterr_errx(1, "ftio_init(): failed");

  ftio_get_ver(&ftio, &ftv);

  if (ftstat_def_test_xfields(ftsd, ftrec_xfield(&ftv)))
    fterr_errx(1, "Report definition references a field not in flow.");

  fts3rec_compute_offsets(&fo, &ftv);

  /* profile */
  ftprof_start (&ftp);

  if (ftstat_def_new(ftsd)) {
    fterr_errx(1, "ftstat_new(%s): failed.",ftsd->name);
  }

  while ((rec = ftio_read(&ftio))) {

    ++total_flows;

    done = 0;

    if ((split = ftstat_def_accum(ftsd, rec, &fo)) < 0) {
      fterr_errx(1, "ftstat_eval(%s): failed.",ftsd->name);
    }

    if (split) {

      if (ftstat_def_calc(ftsd)) {
        fterr_errx(1, "ftstat_dump(%s): failed.",ftsd->name);
      }

      if (ftstat_def_dump(&ftio, ftsd)) {
        fterr_errx(1, "ftstat_dump(%s): failed.",ftsd->name);
      }

      if (ftstat_def_reset(ftsd)) {
        fterr_errx(1, "ftstat_def_reset(%s): failed.",ftsd->name);
      }

      if ((split = ftstat_def_accum(ftsd, rec, &fo)) < 0) {
        fterr_errx(1, "ftstat_eval(%s): failed.",ftsd->name);
      }

      if (split == 1)
        fterr_errx(1, "ftstat_def_accum(): looping on split");

    } /* split */

  } /* while more flows */

  if (ftstat_def_calc(ftsd)) {
    fterr_errx(1, "ftstat_dump(%s): failed.",ftsd->name);
  }

  if (ftstat_def_dump(&ftio, ftsd)) {
    fterr_errx(1, "ftstat_dump(%s): failed.",ftsd->name);
  }

  if (ftstat_def_free(ftsd)) {
    fterr_errx(1, "ftstat_def_free(%s): failed.",ftsd->name);
  }

  if (ftio_close(&ftio) < 0)
    fterr_errx(1, "ftio_close(): failed");

  if (debug > 0) {
    ftprof_end (&ftp, total_flows);
    ftprof_print(&ftp, argv[0], stderr);
  }

  ftstat_free(&ftstat);

  ftvar_free(&ftvar);

  return 0;

} /* main */

void usage(void) {

  static int first;

  if (!first) {

    fprintf(stderr, "Usage: flow-report [-h]\n");
    fprintf(stderr, "       [-d debug_level] [-s stat_fname] [-S stat_definition]\n");
    fprintf(stderr, "       [-v var=val]\n");
    fprintf(stderr, "\n%s version %s: built by %s\n", PACKAGE, VERSION, FT_PROG_BUILD);

    ++first;

  } else {

    fprintf(stderr, "\nAvailable reports:\n\n");
    ftstat_list_reports(stderr);

  }
}

void sig_pipe(int signo)
{
  sig_pipe_flag = 1;
}

