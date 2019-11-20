/*
 * Copyright (c) 2001 E. Larry Lidz
 * Portions Copyright (c) 2001 Mark Fullmer and The Ohio State University
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
 *      $Id: flow-merge.c,v 1.9 2003/04/02 18:03:02 maf Exp $
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

struct ftio_table {
  char *cur_entry;
  struct ftio ftio_data;
};

int debug;
int done;

void usage(void);
void sig_quit(int sig);
int find_earliest(struct ftio_table ftio_entry[], int num_entries);

int main(int argc, char **argv)
{
  struct stat sb;
  struct ftio ftio_out;
  struct ftprof ftp;
  struct ftver ftv, ftv2;
  struct ftset ftset;
  struct ftfile_entries **fte;
  struct ftfile_entry *fty;
  struct ftio_table *ftio_array;
  int i, out_fd, out_fd_plain, in_fd, disable_mmap, in_fd_plain, sort;
  int j, ftio_entries, entry;
  int x, fd, flags, fte_entries;
  char *fname, *out_fname;
  u_int32 total_flows;
  u_int32 time_start, time_end, time_tmp;

  /* init fterr */
  fterr_setid(argv[0]);

  /* profile */
  ftprof_start (&ftp);

  bzero(&ftv, sizeof ftv);
  bzero(&ftv2, sizeof ftv2);

  /* defaults + no compression */
  ftset_init(&ftset, 0);

  total_flows = 0;
  out_fd_plain = 0;
  out_fname = (char*)0L;
  out_fd = -1;
  time_start = -1; /* MAXINT */
  time_end = 0;
  disable_mmap = 0;
  sort = 0; /* global sorting disabled by default */
  fte_entries = 0;
  flags = FT_FILE_INIT | FT_FILE_SORT | FT_FILE_SKIPTMP;
  ftio_entries = 0;

  while ((i = getopt(argc, argv, "ab:C:d:gh?mo:i:z:")) != -1)

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

    case 'm': /* disable mmap */
      disable_mmap = 1;
      break;

    case 'o': /* output filename */
      out_fname = optarg;
      break;

    case 'z': /* compress level */
      ftset.z_level = atoi(optarg);
      if ((ftset.z_level < 0) || (ftset.z_level > 9))
        fterr_errx(1, "Compression level must be between 0 and 9");
      break;

    case 'h': /* help */
    case '?': 
    default:
      usage();
      exit (1);
      break;

    } /* switch */

  /* handle signals */

  if (mysignal(SIGQUIT, sig_quit) == SIG_ERR)
    fterr_err(1, "signal(SIGQUIT)");

  if (mysignal(SIGHUP, sig_quit) == SIG_ERR)
    fterr_err(1, "signal(SIGHUP)");

  if (mysignal(SIGINT, sig_quit) == SIG_ERR)
    fterr_err(1, "signal(SIGHUP)");

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
        fterr_errx(1, "ftfile_loadfile(%s)", fname);
    /* else is this a directory? */
    } else if (S_ISDIR(sb.st_mode)) {
      if (ftfile_loaddir(fte[x], fname, flags) < 0)
        fterr_errx(1, "ftfile_loaddir(%s), fname");
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

  /* count total entries */
  for (i = 0; i < fte_entries; ++i) {
    /* foreach file in the list */
    FT_TAILQ_FOREACH(fty, &fte[i]->head, chain) {
      ftio_entries++;
    }
  }

  ftio_array = malloc(ftio_entries * sizeof(struct ftio_table));
  bzero(ftio_array, ftio_entries * sizeof(struct ftio_table));

  /* foreach list */
  j = 0;
  for (i = 0; i < fte_entries; ++i) {

    /* foreach file in the list */
    FT_TAILQ_FOREACH(fty, &fte[i]->head, chain) {

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
      if (ftio_init(&ftio_array[j].ftio_data, in_fd, FT_IO_FLAG_READ |
        ((in_fd_plain && !disable_mmap) ? FT_IO_FLAG_MMAP : 0)) < 0)
        fterr_errx(1, "ftio_init(): failed");

      /* ensure required fields */
      if (ftio_check_xfield(&ftio_array[j].ftio_data,
        FT_XFIELD_UNIX_SECS|FT_XFIELD_UNIX_NSECS|FT_XFIELD_SYSUPTIME|
        FT_XFIELD_FIRST|FT_XFIELD_LAST))
        fterr_errx(1, "Flow record missing required field.");

      /* get version from stream */
      ftio_get_ver(&ftio_array[j].ftio_data, &ftv2);

      /* record smallest time */
      time_tmp = ftio_get_cap_start(&ftio_array[j].ftio_data);
      if (time_tmp < time_start)
        time_start = time_tmp;

      /* record largest time */
      time_tmp = ftio_get_cap_end(&ftio_array[j].ftio_data);
      if (time_tmp > time_end)
        time_end = time_tmp;

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
        if (ftio_write_header(&ftio_out) < 0)
          fterr_errx(1, "ftio_write_header(): failed");

      } else {

        /* ensure previous version == current version */
        if ((ftv.d_version != ftv2.d_version) ||
            (ftv.agg_method != ftv2.agg_method))
          fterr_errx(1, "data version or sub version changed!");

      }

      /* get the next element into the cur_entry field  */
      /* this is so it can be compared in find_earliest */
      ftio_array[j].cur_entry = ftio_read(&ftio_array[j].ftio_data);
      j++;

    }  /* FOREACH filename in dir */

  } /* foreach dir bundle */

  while ((entry = find_earliest(ftio_array, ftio_entries)) >= 0) {

    /* copy the earliest entry in ftio_array */
    if (ftio_write(&ftio_out, ftio_array[entry].cur_entry) < 0)
      fterr_errx(1, "ftio_write(): failed");
  
    /* get the next element into the cur_entry field */
    ftio_array[entry].cur_entry = ftio_read(&ftio_array[entry].ftio_data);

    ++total_flows;
  }

  for (i = 0; i < ftio_entries; i++) {

    /* done with input stream */
    if (ftio_close(&ftio_array[i].ftio_data) < 0)
      fterr_errx(1, "ftio_close(): failed");

  }

  /*
   * if the output file descriptor was a real file, re-write the
   * flow_header with the correct # of total flows
   */
  if (out_fd_plain) {

    ftio_set_cap_time(&ftio_out, time_start, time_end);
    ftio_set_flows_count(&ftio_out, total_flows);
    ftio_set_streaming(&ftio_out, 0);

    if (ftio_write_header(&ftio_out) < 0)
      fterr_errx(1, "ftio_write_header(): failed");

  } /* out_fd_plain */

  /* done with output stream */
  if (ftio_close(&ftio_out) < 0)
    fterr_errx(1, "ftio_close(): failed");

  if (debug > 0) {
    ftprof_end (&ftp, total_flows);
    ftprof_print(&ftp, argv[0], stderr);
  }

  /* free storage allocated to file list(s) */
  if (fte_entries) {
    for (i = 0; i < fte_entries; ++i) {
      ftfile_free(fte[i]);
      free(fte[i]);
    }
    free(fte);
  } /* fte_entries */
    
  return 0;

} /* main */

int find_earliest(struct ftio_table ftio_entry[], int num_entries)
{
  unsigned int i, earliest_offset;
  register struct timeval earliest_time;
  struct fts3rec_all cur;
  struct fttime ftt;
  int first;

  earliest_offset = -1;
  first = 0;

  for (i = 0; i < num_entries; i++) {

    /* already read all the entries in this file */
    if (ftio_entry[i].cur_entry == NULL) 
      continue;

    cur.unix_secs = ((u_int32*)(ftio_entry[i].cur_entry+
      ftio_entry[i].ftio_data.fo.unix_secs));
    cur.unix_nsecs = ((u_int32*)(ftio_entry[i].cur_entry+
      ftio_entry[i].ftio_data.fo.unix_nsecs));
    cur.sysUpTime = ((u_int32*)(ftio_entry[i].cur_entry+
      ftio_entry[i].ftio_data.fo.sysUpTime));
    cur.Last = ((u_int32*)(ftio_entry[i].cur_entry+
      ftio_entry[i].ftio_data.fo.Last));

    ftt = ftltime(*cur.sysUpTime, *cur.unix_secs, *cur.unix_nsecs, *cur.Last);

    if (first == 0) { /* first entry is the earliest by default */

      earliest_offset = i;
      earliest_time.tv_sec = ftt.secs;
      earliest_time.tv_usec = ftt.msecs;
      first = 1;

    } else {

      if ((ftt.secs < earliest_time.tv_sec) ||
          (ftt.secs == earliest_time.tv_sec &&
           (ftt.msecs < earliest_time.tv_usec))) {

        earliest_offset = i;
        earliest_time.tv_sec = ftt.secs;
        earliest_time.tv_usec = ftt.msecs;

      }

    }

  }

  return earliest_offset;

} /* find_earliest */


void sig_quit(int sig)
{
  done = 1;
} /* sig_quit */

void usage(void)
{
  fprintf(stderr, "Usage: flow-merge [-aghm] [-b big|little] [-C comment] [-d debug_level]\n");
  fprintf(stderr, "       [-o filename] [-z z_level] [file|directory ...]\n");
  fprintf(stderr, "\n%s version %s: built by %s\n", PACKAGE, VERSION, FT_PROG_BUILD);

} /* usage */

