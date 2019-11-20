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
 *      $Id: flow-cat.c,v 1.35 2005/05/10 15:52:10 maf Exp $
 */

#include "ftconfig.h"
#include <ftlib.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <signal.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <fcntl.h>

#if HAVE_STRINGS_H
 #include <strings.h>
#endif

#if HAVE_STRING_H
  #include <string.h>
#endif

#include "ftbuild.h"

int debug;
int done;

void usage(void);
void sig_quit(int sig);

int main(int argc, char **argv)
{
  struct stat sb;
  struct ftio ftio_in, ftio_out;
  struct ftprof ftp;
  struct ftver ftv, ftv2;
  struct ftset ftset;
  struct ftfile_entries **fte;
  struct ftfile_entry *fty;
  int i, out_fd, out_fd_plain, in_fd, disable_mmap, in_fd_plain, sort;
  int fields;
  int x, n, fd, flags, fte_entries, preload, time_filter;
  char *fname, *out_fname;
  char *rec;
  u_long total_bytes;
  u_int32 total_flows, lost_flows, corrupt_flows, total_streams;
  u_int32 time_start, time_end, time_tmp1, time_tmp2, time_delta;
  u_int32 time_low, time_high;

  /* init fterr */
  fterr_setid(argv[0]);

  /* profile */
  ftprof_start (&ftp);

  bzero(&ftv, sizeof ftv);
  bzero(&ftv2, sizeof ftv2);

  /* defaults + no compression */
  ftset_init(&ftset, 0);

  preload = 0;
  corrupt_flows = 0;
  lost_flows = 0;
  total_bytes = 0;
  total_flows = 0;
  total_streams = 0;
  out_fd_plain = 0;
  out_fname = (char*)0L;
  out_fd = -1;
  time_start = -1; /* MAXINT */
  time_end = 0;
  disable_mmap = 0;
  sort = 0; /* global sorting disabled by default */
  fte_entries = 0;
  flags = FT_FILE_INIT | FT_FILE_SORT | FT_FILE_SKIPTMP;
  time_filter = 0;
  time_high = time_low = 0;
  fields = 0;

  while ((i = getopt(argc, argv, "ab:C:d:gh?mo:pt:T:z:")) != -1)

    switch (i) {

    case 'a': /* all files, even those with tmp */
      flags &= ~FT_FILE_SKIPTMP;
      break;

    case 'b': /* output byte order */
      if (!strcasecmp(optarg, "little"))
        ftset.byte_order = FT_HEADER_LITTLE_ENDIAN;
      else if (!strcasecmp(optarg, "big"))
        ftset.byte_order = FT_HEADER_BIG_ENDIAN;
      else
        fterr_errx(1, "expecting \"big\" or \"little\"");
      break;

    case 'C': /* comment field */
      ftset.comments = optarg;
      break;

    case 'd': /* debug */
      debug = atoi(optarg);
      break;

    case 'g': /* global sort */
      sort = 1;
      break;

    case 'h': /* help */
    case '?': 
    default:
      usage();
      exit (1);
      break;

    case 'm': /* disable mmap */
      disable_mmap = 1;
      break;

    case 'o': /* output filename */
      out_fname = optarg;
      break;

    case 'p': /* preload headers */
      preload = 1;
      break;

    case 't': /* start time */
      preload = 1;
      time_filter = 1;
      if ((time_low = get_date(optarg, (time_t*)0L)) == -1)
        fterr_errx(1, "Error parsing time: %s.", optarg);
      break;

    case 'T': /* end time */
      preload = 1;
      time_filter = 1;
      if ((time_high = get_date(optarg, (time_t*)0L)) == -1)
        fterr_errx(1, "Error parsing time: %s.", optarg);
      break;

    case 'z': /* compress level */
      ftset.z_level = atoi(optarg);
      if ((ftset.z_level < 0) || (ftset.z_level > 9))
        fterr_errx(1, "Compression level must be between 0 and 9");
      break;


    } /* switch */

  /* handle signals */

  if (mysignal(SIGQUIT, sig_quit) == SIG_ERR)
    fterr_err(1, "signal(SIGQUIT)");

  if (mysignal(SIGHUP, sig_quit) == SIG_ERR)
    fterr_err(1, "signal(SIGHUP)");

  if (mysignal(SIGINT, sig_quit) == SIG_ERR)
    fterr_err(1, "signal(SIGINT)");

  if (mysignal(SIGTERM, sig_quit) == SIG_ERR)
    fterr_err(1, "signal(SIGTERM)");

  /* if out_fname is not set, then use stdout */
  if (out_fname) {

    if ((out_fd = open(out_fname,  O_WRONLY|O_CREAT|O_TRUNC, 0644)) == -1)
      fterr_err(1, "open(%s)", out_fname);

    if (fstat(out_fd, &sb) == -1)
      fterr_err(1, "fstat(%s)", out_fname);

    /* is this a plain file? */
    if (S_ISREG(sb.st_mode))
      out_fd_plain = 1;

  } else 
    out_fd = 1;

  /* output to out_fd */    
  if (ftio_init(&ftio_out, out_fd, FT_IO_FLAG_WRITE |
    ((ftset.z_level) ? FT_IO_FLAG_ZINIT : 0) ) < 0)
    fterr_errx(1, "ftio_init(): failed");

  ftio_set_comment(&ftio_out, ftset.comments);
  ftio_set_byte_order(&ftio_out, ftset.byte_order);
  ftio_set_z_level(&ftio_out, ftset.z_level);
  ftio_set_streaming(&ftio_out, 1);
  ftio_set_debug(&ftio_out, debug);

  /* header must be full size on initial write */
  if (out_fd_plain) {
    ftio_set_cap_time(&ftio_out, time_start, time_end);
    ftio_set_flows_count(&ftio_out, total_flows);
    ftio_set_corrupt(&ftio_out, corrupt_flows);
    ftio_set_lost(&ftio_out, lost_flows);
  }

  /* number of file lists */
  fte_entries = argc - optind;

  /* always at least one entry */
  if (fte_entries <= 0)
    fte_entries = 1;

  if (sort)
    fte_entries = 1;

  /* allocate space for ftfile_entries lists */
  if (!(fte = (struct ftfile_entries**)malloc(
    sizeof (struct ftfile_entries*) * fte_entries))) {
    fterr_err(1, "malloc()");
  }

  /* allocate each entry */
  for (i = 0; i < fte_entries; ++i) {
    if (!(fte[i] = (struct ftfile_entries*)malloc(
      sizeof (struct ftfile_entries)))) {
      fterr_err(1, "malloc()");
    }
    bzero(fte[i], sizeof (struct ftfile_entries));
  }

  /* handle special case of stdin (no args) */
  if (optind >= argc) {
    /* "" will be treated as stdin below */
    if (ftfile_loadfile(fte[0], "", flags) < 0)
      fterr_errx(1, "ftfile_loadfile(%s)", "");
  }

  for (i = optind, x = 0; i < argc; ++i) {

    fname = argv[i];

    /* special case, stdin */
    if ((fname[0] == '-') && (fname[1] == 0)) {
      /* "" will be treated as stdin below */
      if (ftfile_loadfile(fte[x], "", flags) < 0)
        fterr_errx(1, "ftfile_loadfile(%s)", "");
      goto skip1;
    }

    /* open current file */
    if ((fd = open(fname, O_RDONLY, 0)) == -1) {
      fterr_warn("open(%s)", fname);
      continue;
    }

    /* stat current file */
    if (fstat(fd, &sb) == -1)
      fterr_err(1, "fstat(%s)", fname);

    /* done */
    close (fd);

    /* is this a plain file? */
    if (S_ISREG(sb.st_mode)) {
      if (ftfile_loadfile(fte[x], fname, flags) < 0)
        fterr_errx(1, "ftfile_loadfile(%s): failed", fname);
    /* else is this a directory? */
    } else if (S_ISDIR(sb.st_mode)) {
      if (ftfile_loaddir(fte[x], fname, flags) < 0)
        fterr_errx(1, "ftfile_loaddir(%s): failed", fname);
    } else {
      fterr_warnx("odd file, skipping: %s", fname);
      continue;
    }

skip1:

    /* if not sorting, use next list (sorting requires one list) */
    if (!sort)
      ++x;
    else /* else, the list is initialized, don't do it again */
      flags &= ~FT_FILE_INIT;

  } /* foreach file on the command line */

  /* dump list of files loaded */
  if (debug > 5) {
    for (i = 0; i < fte_entries; ++i) {
      fterr_info(" i=%d", i);
      ftfile_dump(fte[i]);
    }
  }

  /* preload headers */
  if (preload) {

    /* foreach list */
    for (i = 0; i < fte_entries; ++i) {

      /* foreach file in the list */
      FT_TAILQ_FOREACH(fty, &fte[i]->head, chain) {

        if (debug > 5)
          fterr_info("header load: file=%s", fty->name);

        /* stdin / real file? */
        if (fty->name[0]) {

          if ((in_fd = open(fty->name, O_RDONLY, 0)) == -1) {
            fterr_warn("open(%s)", fty->name);
            continue;
          }

        } else {

          continue;

        }

        /* initialize ftio stream */
        if (ftio_init(&ftio_in, in_fd, FT_IO_FLAG_READ |
          ((in_fd_plain && !disable_mmap) ? FT_IO_FLAG_MMAP : 0)) < 0)
          fterr_errx(1, "ftio_init(): failed");

        /* ensure required fields */
        if (ftio_check_xfield(&ftio_in,
          FT_XFIELD_UNIX_SECS|FT_XFIELD_UNIX_NSECS|FT_XFIELD_SYSUPTIME|
          FT_XFIELD_FIRST|FT_XFIELD_LAST))
          fterr_errx(1, "Flow record missing required field.");

        /* keep a record of all the possible fields */
        fields |= ftio_in.fth.fields;

        /* record smallest time */
        if (ftio_in.fth.fields & FT_FIELD_CAP_START) {
          time_tmp1 = ftio_get_cap_start(&ftio_in);
          if (time_tmp1 < time_start)
            time_start = time_tmp1;
        }

        /* record largest time */
        if (ftio_in.fth.fields & FT_FIELD_CAP_END) {
          time_tmp2 = ftio_get_cap_end(&ftio_in);
          if (time_tmp2 > time_end)
            time_end = time_tmp2;
        }

        /*
         * if coarse grained time filtering is enabled -- ie only filtering
         * files based on header time, ensure this file fits in the window
         */
        if (time_filter) {

          /* all flows between low and high */
          if (time_high && time_low) {

            time_delta = time_tmp2 - time_tmp1;

            if ((time_tmp1 < time_low) ||
                ((time_tmp2-time_delta) > time_high)) {
              fty->skip = 1;
              goto skip_file;
            }


          } else { /* between */

            /* all flows until low, ie -inf to low */
            if (time_low && (time_tmp1 > time_low)) {
              fty->skip = 1;
              goto skip_file;
            }

            /* all flows after high, ie high to +inf */
            if (time_high && (time_tmp2 < time_high)) {
              fty->skip = 1;
              goto skip_file;
            }

          } /* outside */

        } /* time_filter */

        /* total lost flows */
        lost_flows += ftio_get_lost(&ftio_in);

        /* total corrupt flows */
        corrupt_flows += ftio_get_corrupt(&ftio_in);

        /* total flows */
        total_flows += ftio_get_flows_count(&ftio_in);

skip_file:
        if (debug > 5)
          fterr_info("file=%s, status=%d", fty->name, fty->skip);

        ftio_close(&ftio_in);
      }
    }

    ftio_set_preloaded(&ftio_out, 1);
    if ((fields & (FT_FIELD_CAP_END|FT_FIELD_CAP_START)) == (
      FT_FIELD_CAP_START|FT_FIELD_CAP_END))
      ftio_set_cap_time(&ftio_out, time_start, time_end);
    ftio_set_flows_count(&ftio_out, total_flows);
    ftio_set_corrupt(&ftio_out, corrupt_flows);
    ftio_set_lost(&ftio_out, lost_flows);

    time_start = -1; /* MAXINT */
    time_end = 0;
    total_flows = 0;

  } /* preload */


  /* foreach list */
  for (i = 0; i < fte_entries; ++i) {

    /* foreach file in the list */
    FT_TAILQ_FOREACH(fty, &fte[i]->head, chain) {

      /* skip to next file? */
      if (fty->skip)
        continue;

      if (debug > 5)
        fterr_info("working file=%s", fty->name);

      /* stdin / real file? */
      if (fty->name[0]) {

        in_fd_plain = 1;

        if ((in_fd = open(fty->name, O_RDONLY, 0)) == -1) {
          fterr_warn("open(%s)", fty->name);
          continue;
        }

      } else {

        in_fd_plain = 0;
        in_fd = 0; /* stdin */

      }

      /* initialize ftio stream */
      if (ftio_init(&ftio_in, in_fd, FT_IO_FLAG_READ |
        ((in_fd_plain && !disable_mmap) ? FT_IO_FLAG_MMAP : 0)) < 0)
        fterr_errx(1, "ftio_init(): failed");

      /* get version from stream */
      ftio_get_ver(&ftio_in, &ftv2);

      /* successful process of a stream */
      ++ total_streams;

      /* record smallest time */
      time_tmp1 = ftio_get_cap_start(&ftio_in);
      if (time_tmp1 < time_start)
        time_start = time_tmp1;

      /* record largest time */
      time_tmp2 = ftio_get_cap_end(&ftio_in);
      if (time_tmp2 > time_end)
        time_end = time_tmp2;

      /* first time through loop? */
      if (!ftv.d_version) {

        /*
         * is this really the right thing to do here.  Reading a v1
         * stream gets handled by ftio_read(), but ftio_* leaves the
         * s_version at 1.
         */
        ftv2.s_version = FT_IO_SVERSION;

        /* set the version information in the io stream */
        if (ftio_set_ver(&ftio_out, &ftv2) < 0)
          fterr_errx(1, "ftio_set_ver(): failed");

        /* save for later compare */
        bcopy(&ftv2, &ftv, sizeof ftv);

        /* header first */
        if ((n = ftio_write_header(&ftio_out)) < 0)
          fterr_errx(1, "ftio_write_header(): failed");

        total_bytes += n;

        if (debug > 6)
          if (n)
            fterr_info("ftio_write_header()=%d", n);

      } else {

        /* ensure previous version == current version */
        if ((ftv.d_version != ftv2.d_version) ||
            (ftv.agg_method != ftv2.agg_method))
          fterr_errx(1, "data version or sub version changed!");

      }

      /* foreach flow record, copy it */
      while ((rec = ftio_read(&ftio_in))) {

        ++total_flows;

        if ((n = ftio_write(&ftio_out, rec)) < 0)
          fterr_errx(1, "ftio_write(): failed");

        total_bytes += n;

        if (debug > 6)
          if (n)
            fterr_info("ftio_write()=%d", n);

        if ((debug > 5) && ((total_flows & 0x3ffff) == 0))
           fterr_info("processed/total flows: %lu / %lu", total_flows, ftio_get_flows_count(&ftio_out));


        /* interrupted? */
        if (done)
          break;

      } /* while copying */

      /* done with input stream */
      if (ftio_close(&ftio_in) < 0)
        fterr_errx(1, "ftio_close(): failed");

      /* interrupted? */
      if (done)
        break;

    }  /* FOREACH filename in dir */

  } /* foreach dir bundle */

  /*
   * if the output file descriptor was a real file, re-write the
   * flow_header with the correct # of total flows
   */
  if (out_fd_plain) {

    ftio_set_cap_time(&ftio_out, time_start, time_end);
    ftio_set_flows_count(&ftio_out, total_flows);
    ftio_set_streaming(&ftio_out, 0);

    if ((n = ftio_write_header(&ftio_out)) < 0)
      fterr_errx(1, "ftio_write_header(): failed");

    total_bytes += n;

    if (debug > 6)
      if (n)
        fterr_info("ftio_write_header()=%d", n);

  } /* out_fd_plain */

  /* done with output stream */
  if ((n = ftio_close(&ftio_out)) < 0)
    fterr_errx(1, "ftio_close(): failed");

  total_bytes += n;

  if (debug > 6)
    if (n)
      fterr_info("ftio_close(ftio_out)=%d\n", n);

  if (debug > 0) {
    ftprof_end (&ftp, total_flows);
    ftprof_print(&ftp, argv[0], stderr);
  }

  if (debug > 1)
    fterr_info("Bytes written=%lu", total_bytes);

  /* free storage allocated to file list(s) */
  if (fte_entries) {
    for (i = 0; i < fte_entries; ++i) {
      ftfile_free(fte[i]);
      free(fte[i]);
    }
    free(fte);
  } /* fte_entries */

  /* no successful streams, then error */
  if (!total_streams) 
    return 1;
  else
    return 0;

} /* main */

void sig_quit(int sig)
{
  done = 1;
} /* sig_quit */

void usage(void) {

  fprintf(stderr, "Usage: flow-cat [-aghmp] [-b byte_order] [-C comment] [-d debug_level]\n");
  fprintf(stderr, "       [-o filename] [-t start_time] [-T end_time] [-z z_level]\n");
  fprintf(stderr, "       file|directory ...");
  fprintf(stderr, "\n");
  fprintf(stderr, "\n%s version %s: built by %s\n", PACKAGE, VERSION, FT_PROG_BUILD);


} /* usage */

