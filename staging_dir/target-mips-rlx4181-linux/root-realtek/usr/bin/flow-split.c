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
 *      $Id: flow-split.c,v 1.12 2003/04/02 18:03:03 maf Exp $
 */

#include "ftconfig.h"
#include <ftlib.h>

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

#if HAVE_STRINGS_H
 #include <strings.h>
#endif

#if HAVE_STRING_H
  #include <string.h>
#endif

#include <fcntl.h>
#include "ftbuild.h"

int debug;
void usage(void);

#define MAXPATHNAME 256

enum split_tag {SPLIT_TAG_UNSET, SPLIT_TAG_SRC, SPLIT_TAG_DST};

int main(int argc, char **argv)
{
  struct ftio ftio_in;
  struct ftprof ftp;
  struct ftver ftv;
  struct ftset ftset;
  struct fts3rec_offsets fo;
  struct ftsym *sym_tag;
  char *rec;
  struct ftchash *ftch;
  struct ftchash_rec_split ftch_recsplit, *ftch_recsplitp;
  enum split_tag stag;
  int i, names;
  char *out_path, out_fname[MAXPATHNAME], fmt_buf[32];
  u_int32 max_flows, max_time, hash, unix_secs, total_flows;

  /* init fterr */
  fterr_setid(argv[0]);

  bzero(&ftv, sizeof ftv);
  bzero(&ftch_recsplit, sizeof ftch_recsplit);
  ftch_recsplit.fd = -1;
  ftch_recsplit.newfile = 1;
  ftch_recsplitp = &ftch_recsplit;
  out_path = "split";
  max_flows = max_time = 0;
  names = 0;
  sym_tag = (struct ftsym*)0L;
  ftch = (struct ftchash*)0L;
  stag = SPLIT_TAG_UNSET;
  total_flows = 0;

  /* profile */
  ftprof_start (&ftp);

  /* defaults */
  ftset_init(&ftset, -1);

  while ((i = getopt(argc, argv, "b:C:d:gGh?nN:o:T:z:")) != -1)

    switch (i) {

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

    case 'g': /* split src tag */
      if (stag == SPLIT_TAG_DST)
        fterr_errx(1, "Split on destination or source.");
      stag = SPLIT_TAG_SRC;
      break;

    case 'G': /* split dst tag */
      if (stag == SPLIT_TAG_SRC)
        fterr_errx(1, "Split on destination or source.");
      stag = SPLIT_TAG_DST;
      break;

    case 'n': /* names */
      names = 1;
      break;

    case 'N': /* max flows */
      max_flows = strtoul(optarg, (char **)0L, 10);
      break;

    case 'o': /* base output filename */
      out_path = optarg;
      if (strlen(out_path) > (MAXPATHNAME-25))
        fterr_errx(1, "base output too long");
      break;

    case 'T': /* max time */
      max_time = strtoul(optarg, (char **)0L, 10);
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

  if (argc - optind)
    fterr_errx(1, "Extra arguments starting with %s.", argv[optind]);

  /* input is stdin */
  if (ftio_init(&ftio_in, 0, FT_IO_FLAG_READ) < 0)
    fterr_errx(1, "ftio_init(): failed");

  ftio_get_ver(&ftio_in, &ftv);

  ftv.s_version = FT_IO_SVERSION;

  if (max_time)
    if (ftio_check_xfield(&ftio_in, FT_XFIELD_UNIX_SECS))
      fterr_errx(1, "Flow record missing unix_secs.");

  /* if splitting on tags is enabled, make sure the field is available */
  if (stag) {

    if (stag == SPLIT_TAG_SRC)
      if (ftio_check_xfield(&ftio_in, FT_XFIELD_SRC_TAG))
        fterr_errx(1, "Flow record missing source tag.");

    if (stag == SPLIT_TAG_DST)
      if (ftio_check_xfield(&ftio_in, FT_XFIELD_DST_TAG))
        fterr_errx(1, "Flow record missing destination tag.");

    if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_split), 4,
      65536)))
      fterr_errx(1, "ftchash_new(): failed");


  } /* stag */

  /* symbolic names? */
  if (stag && names) {
    sym_tag = ftsym_new(FT_PATH_SYM_TAG);
  }

  fts3rec_compute_offsets(&fo, &ftv);

  while ((rec = ftio_read(&ftio_in))) {

    ++total_flows;

    unix_secs = *((u_int32*)(rec+fo.unix_secs));

    /*
     * if tagging is enabled, grab the tag and look it up, possibly
     * trigger a new file creation
     *
     */
     if (stag) {

       if (stag == SPLIT_TAG_SRC)
         ftch_recsplit.tag = *((u_int32*)(rec+fo.src_tag));
       else if (stag == SPLIT_TAG_DST)
         ftch_recsplit.tag = *((u_int32*)(rec+fo.dst_tag));

       hash = (ftch_recsplit.tag>>16) ^ (ftch_recsplit.tag & 0xFFFF);

       if (!(ftch_recsplitp = ftchash_update(ftch, &ftch_recsplit, hash)))
         fterr_errx(1, "ftchash_update(): failed.");

       /* first flow for this hash entry? */
       if (!ftch_recsplitp->total_flows) {
         ftch_recsplitp->fd = -1;
         ftch_recsplitp->newfile = 1;
       }

    }

    /* create new output file */
    if (ftch_recsplitp->newfile) {

      /* close the previous file first? */
      if (ftch_recsplitp->fd != -1) {

        ftio_set_cap_time(&ftch_recsplitp->ftio, ftch_recsplitp->cap_start,
          ftch_recsplitp->cap_end);

        ftio_set_flows_count(&ftch_recsplitp->ftio,
          ftch_recsplitp->total_flows);

        ftio_set_streaming(&ftch_recsplitp->ftio, 0);

        if (ftio_write_header(&ftch_recsplitp->ftio) < 0)
          fterr_errx(1, "ftio_write_header(): failed");

        if (ftio_close(&ftch_recsplitp->ftio) < 0)
          fterr_errx(1, "ftio_close(): failed");

        ftch_recsplitp->total_flows = 0;
        ftch_recsplitp->cap_start = 0;
        ftch_recsplitp->cap_end = 0;

      } /* closing */

      /* if splitting on tags, include the tag name in the filename */
      if (stag) {

        fmt_uint32s(sym_tag, 32, fmt_buf, ftch_recsplitp->tag, FMT_JUST_LEFT);

        snprintf(out_fname, MAXPATHNAME, "%s.%s.%d", out_path, fmt_buf,
          ftch_recsplitp->id);


      } else {

        snprintf(out_fname, MAXPATHNAME, "%s.%d", out_path,
          ftch_recsplitp->id);

      }

      if ((ftch_recsplitp->fd = open(out_fname,
        O_WRONLY|O_CREAT|O_TRUNC, 0644)) == -1)
        fterr_err(1, "open(%s)", out_fname);

      /* output to out_fd */    
      if (ftio_init(&ftch_recsplitp->ftio, ftch_recsplitp->fd,
        FT_IO_FLAG_WRITE |
        ((ftset.z_level) ? FT_IO_FLAG_ZINIT : 0) ) < 0)
        fterr_errx(1, "ftio_init(): failed");

      /* set the version information in the io stream */
      if (ftio_set_ver(&ftch_recsplitp->ftio, &ftv) < 0)
        fterr_errx(1, "ftio_set_ver(): failed");

      ftio_set_comment(&ftch_recsplitp->ftio, ftset.comments);
      ftio_set_byte_order(&ftch_recsplitp->ftio, ftset.byte_order);
      ftio_set_z_level(&ftch_recsplitp->ftio, ftset.z_level);
      ftio_set_streaming(&ftch_recsplitp->ftio, 1);
      ftio_set_debug(&ftch_recsplitp->ftio, debug);
      ftio_set_cap_time(&ftch_recsplitp->ftio, 0, 0);
      ftio_set_flows_count(&ftch_recsplitp->ftio, 0);

      if (ftio_write_header(&ftch_recsplitp->ftio) < 0)
        fterr_errx(1, "ftio_write_header(): failed");

      /* LC's not synched very well */
      if (unix_secs > ftch_recsplitp->cap_start) {
        ftch_recsplitp->cap_start = unix_secs;
      }

      ftch_recsplitp->newfile = 0;
      ftch_recsplitp->id ++;

    } /* new_file */

    ftch_recsplitp->total_flows ++;

    /* signal new file if total_flows >= max_flows */
    if (max_flows && (ftch_recsplitp->total_flows >= max_flows))
      ftch_recsplitp->newfile = 1;

    /* signal new file if time elapsed > max_time */
    if ((max_time && (unix_secs > ftch_recsplitp->cap_start) &&
      (unix_secs - ftch_recsplitp->cap_start) > max_time)) {
      /* LC's not synch'd very well */
      ftch_recsplitp->cap_start = unix_secs;
      ftch_recsplitp->newfile = 1;
    }

    ftch_recsplitp->cap_end = unix_secs;

    if (ftio_write(&ftch_recsplitp->ftio, rec) < 0)
      fterr_errx(1, "ftio_write(): failed");

  }

  if (!stag) {

    if (ftch_recsplitp->fd != -1) {

      ftio_set_cap_time(&ftch_recsplitp->ftio, 
        ftch_recsplitp->cap_start, ftch_recsplitp->cap_end);

      ftio_set_flows_count(&ftch_recsplitp->ftio,
        ftch_recsplitp->total_flows);

      ftio_set_streaming(&ftch_recsplitp->ftio, 0);

      if (ftio_write_header(&ftch_recsplitp->ftio) < 0)
        fterr_errx(1, "ftio_write_header(): failed");

      if (ftio_close(&ftch_recsplitp->ftio) < 0)
        fterr_errx(1, "ftio_close(): failed");

    } /* if */

  } else { /* !stag */

    ftchash_first(ftch);

    while ((ftch_recsplitp = ftchash_foreach(ftch))) {

      if (ftch_recsplitp->fd != -1) {

        ftio_set_cap_time(&ftch_recsplitp->ftio,
          ftch_recsplitp->cap_start, ftch_recsplitp->cap_end);
 
        ftio_set_flows_count(&ftch_recsplitp->ftio,
          ftch_recsplitp->total_flows);

        ftio_set_streaming(&ftch_recsplitp->ftio, 0);

        if (ftio_write_header(&ftch_recsplitp->ftio) < 0)
          fterr_errx(1, "ftio_write_header(): failed");

        if (ftio_close(&ftch_recsplitp->ftio) < 0)
          fterr_errx(1, "ftio_close(): failed");

      } /* if */

    } /* while */

  } /* stag */

  if (ftio_close(&ftio_in) < 0)
    fterr_errx(1, "ftio_close(): failed");

  if (debug > 0) {
    ftprof_end (&ftp, total_flows);
    ftprof_print(&ftp, argv[0], stderr);
  }

  if (sym_tag)
    ftsym_free(sym_tag);

  if (ftch)
    ftchash_free(ftch);

  return 0;

} /* main */

void usage() {

  fprintf(stderr, "Usage: flow-split [-gGhn] [-b big|little] [-C comment] [-d debug_level]\n");
  fprintf(stderr, "       [-N nflows] [-o outfile_basename] [-T nseconds] [-z z_level]\n");
  fprintf(stderr, "\n%s version %s: built by %s\n", PACKAGE, VERSION, FT_PROG_BUILD);

}

