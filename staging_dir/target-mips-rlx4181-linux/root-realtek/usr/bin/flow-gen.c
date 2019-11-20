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
 *      $Id: flow-gen.c,v 1.23 2003/04/02 18:03:01 maf Exp $
 */

#include "ftconfig.h"
#include <ftlib.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#if HAVE_STRINGS_H
 #include <strings.h>
#endif

#if HAVE_STRING_H
  #include <string.h>
#endif

#include "ftbuild.h"

/*
 * generate flows for testing
 */

int gen_v1(struct ftio *ftio, int options);
int gen_v5(struct ftio *ftio, int options);
int gen_v6(struct ftio *ftio, int options);
int gen_v7(struct ftio *ftio, int options);
int gen_v1005(struct ftio *ftio, int options);
int gen_v8_1(struct ftio *ftio, int options);
int gen_v8_2(struct ftio *ftio, int options);
int gen_v8_3(struct ftio *ftio, int options);
int gen_v8_4(struct ftio *ftio, int options);
int gen_v8_5(struct ftio *ftio, int options);
int gen_v8_6(struct ftio *ftio, int options);
int gen_v8_7(struct ftio *ftio, int options);
int gen_v8_8(struct ftio *ftio, int options);
int gen_v8_9(struct ftio *ftio, int options);
int gen_v8_10(struct ftio *ftio, int options);
int gen_v8_11(struct ftio *ftio, int options);
int gen_v8_12(struct ftio *ftio, int options);
int gen_v8_13(struct ftio *ftio, int options);
int gen_v8_14(struct ftio *ftio, int options);

int debug;

void usage(void);

int main(int argc, char **argv)
{
  struct ftset ftset;
  struct ftio ftio;
  struct ftver ftv;
  int count, i, n;
  unsigned int v1, v2;

  /* init fterr */
  fterr_setid(argv[0]);

  /* defaults + no compression */
  ftset_init(&ftset, 0);

  bzero(&ftv, sizeof ftv);
  count = 1000;
  ftv.s_version = FT_IO_SVERSION;
  ftv.d_version = 5;
  ftv.agg_method = 1;
  ftv.agg_version = 2;

   while ((i = getopt(argc, argv, "b:d:hn:V:z:")) != -1)

    switch(i) {

    case 'b': /* output byte order */
      if (!strcasecmp(optarg, "little"))
        ftset.byte_order = FT_HEADER_LITTLE_ENDIAN;
      else if (!strcasecmp(optarg, "big"))
        ftset.byte_order = FT_HEADER_BIG_ENDIAN;
      else
        fterr_errx(1, "expecting \"big\" or \"little\"");
      break;

    case 'd': /* debug level */
      debug = atoi(optarg);
      break;

    case 'h': /* help */
      usage();
      exit(0);
      break;

    case 'n': /* count */
      count = atoi(optarg);
      break;

   case 'V': /* PDU version */
      n = sscanf(optarg, "%u.%u", &v1, &v2);
      if (n == 1) {
        ftv.s_version = FT_IO_SVERSION;
        ftv.d_version = v1;
        ftv.set = 1;
      } else if (n == 2) {
        ftv.s_version = FT_IO_SVERSION;
        ftv.d_version = v1;
        ftv.agg_method = v2;
        ftv.agg_version = 2;
        ftv.set = 1;
      } else
        fterr_errx(1, "Version scan failed");
      break;

    case 'z': /* compress level */
      ftset.z_level = atoi(optarg);
      if ((ftset.z_level < 0) || (ftset.z_level > 9))
        fterr_errx(1, "Compression level must be between 0 and 9");
      break;

    default:
      usage();
      exit (1);

    } /* switch */

  if (argc - optind)
    fterr_errx(1, "Extra arguments starting with %s.", argv[optind]);

  /* output to stdout */
  if (ftio_init(&ftio, 1, FT_IO_FLAG_WRITE |
    ((ftset.z_level) ? FT_IO_FLAG_ZINIT : 0) ) < 0)
    fterr_errx(1, "ftio_init(): failed");

  ftio_set_comment(&ftio, "flow-gen");
  ftio_set_cap_hostname(&ftio, "flow-gen");
  ftio_set_byte_order(&ftio, ftset.byte_order);
  ftio_set_z_level(&ftio, ftset.z_level);
  ftio_set_streaming(&ftio, 1);
  ftio_set_debug(&ftio, debug);

  if (ftio_set_ver(&ftio, &ftv) < 0)
    fterr_errx(1, "ftio_set_ver(): failed");

  /* header first */
  if (ftio_write_header(&ftio) < 0)
    fterr_errx(1, "ftio_write_header(): failed");

  switch (ftv.d_version) {

    case 1:
      gen_v1(&ftio, count);
      break;

    case 5:
      gen_v5(&ftio, count);
      break;

    case 6:
      gen_v6(&ftio, count);
      break;

    case 7:
      gen_v7(&ftio, count);
      break;

    case 8:
      switch (ftv.agg_method) {

        case 1:
          gen_v8_1(&ftio, count);
          break;

        case 2:
          gen_v8_2(&ftio, count);
          break;

        case 3:
          gen_v8_3(&ftio, count);
          break;

        case 4:
          gen_v8_4(&ftio, count);
          break;

        case 5:
          gen_v8_5(&ftio, count);
          break;

        case 6:
          gen_v8_6(&ftio, count);
          break;

        case 7:
          gen_v8_7(&ftio, count);
          break;

        case 8:
          gen_v8_8(&ftio, count);
          break;

        case 9:
          gen_v8_9(&ftio, count);
          break;

        case 10:
          gen_v8_10(&ftio, count);
          break;

        case 11:
          gen_v8_11(&ftio, count);
          break;

        case 12:
          gen_v8_12(&ftio, count);
          break;

        case 13:
          gen_v8_13(&ftio, count);
          break;

        case 14:
          gen_v8_14(&ftio, count);
          break;

        default:
          fterr_errx(1, "Unsupported agg_method %d", (int)ftv.agg_method);
          break;

      } /* switch */
      break;

    case 1005:
      gen_v1005(&ftio, count);
      break;

    default:
      fterr_errx(1, "Unsupported d_version %d", (int)ftv.d_version);
      break;

  } /* switch d_version */

  if (ftio_close(&ftio) < 0)
    fterr_errx(1, "ftio_close(): failed");

  return 0;

} /* main */

int gen_v1(struct ftio *ftio, int count)
{
  struct fts3rec_v1 rec_v1;
  int i;

  bzero(&rec_v1, sizeof rec_v1);

  rec_v1.dstaddr = 0xFFFF0000;
  rec_v1.dstport = 0xFF00;
  rec_v1.output = 0xFF00;
  rec_v1.Last = 0xFFFF0000;
  rec_v1.prot = 17;

  for (i = 0; i < count; ++i) {

    rec_v1.dPkts ++;
    rec_v1.dOctets ++;

    if (ftio_write(ftio, &rec_v1) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v1.srcaddr ++;
    rec_v1.dstaddr ++;

    rec_v1.srcport ++;
    rec_v1.dstport ++;

    rec_v1.input ++;
    rec_v1.output ++;

    rec_v1.First ++;
    rec_v1.Last ++;

  } /* for */

  return 0;

} /* gen_v1 */

int gen_v5(struct ftio *ftio, int count)
{
  struct fts3rec_v5 rec_v5;
  int i;

  bzero(&rec_v5, sizeof rec_v5);

  rec_v5.dstaddr = 0xFFFF0000;
  rec_v5.dstport = 0xFF00;
  rec_v5.output = 0xFF00;
  rec_v5.Last = 0xFFFF0000;
  rec_v5.dst_as = 0xFF00;
  rec_v5.prot = 17;

  for (i = 0; i < count; ++i) {

    rec_v5.dPkts ++;
    rec_v5.dOctets ++;

    if (ftio_write(ftio, &rec_v5) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v5.srcaddr ++;
    rec_v5.dstaddr ++;

    rec_v5.srcport ++;
    rec_v5.dstport ++;

    rec_v5.input ++;
    rec_v5.output ++;

    rec_v5.First ++;
    rec_v5.Last ++;

    rec_v5.src_as ++;
    rec_v5.dst_as ++;

    rec_v5.tos ++;

  } /* for */

  return 0;

} /* gen_v5 */

int gen_v6(struct ftio *ftio, int count)
{
  struct fts3rec_v6 rec_v6;
  int i;

  bzero(&rec_v6, sizeof rec_v6);

  rec_v6.dstaddr = 0xFFFF0000;
  rec_v6.dstport = 0xFF00;
  rec_v6.output = 0xFF00;
  rec_v6.Last = 0xFFFF0000;
  rec_v6.dst_as = 0xFF00;
  rec_v6.out_encaps = 0x80;
  rec_v6.prot = 17;

  for (i = 0; i < count; ++i) {

    rec_v6.dPkts ++;
    rec_v6.dOctets ++;

    if (ftio_write(ftio, &rec_v6) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v6.srcaddr ++;
    rec_v6.dstaddr ++;

    rec_v6.srcport ++;
    rec_v6.dstport ++;

    rec_v6.input ++;
    rec_v6.output ++;

    rec_v6.First ++;
    rec_v6.Last ++;

    rec_v6.src_as ++;
    rec_v6.dst_as ++;

    rec_v6.peer_nexthop ++;

    rec_v6.in_encaps ++;
    rec_v6.out_encaps ++;

  } /* for */

  return 0;

} /* gen_v6 */

int gen_v7(struct ftio *ftio, int count)
{
  struct fts3rec_v7 rec_v7;
  int i;

  bzero(&rec_v7, sizeof rec_v7);

  rec_v7.dstaddr = 0xFFFF0000;
  rec_v7.dstport = 0xFF00;
  rec_v7.output = 0xFF00;
  rec_v7.Last = 0xFFFF0000;
  rec_v7.dst_as = 0xFF00;
  rec_v7.prot = 17;

  for (i = 0; i < count; ++i) {

    rec_v7.dPkts ++;
    rec_v7.dOctets ++;

    if (ftio_write(ftio, &rec_v7) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v7.srcaddr ++;
    rec_v7.dstaddr ++;

    rec_v7.srcport ++;
    rec_v7.dstport ++;

    rec_v7.input ++;
    rec_v7.output ++;

    rec_v7.First ++;
    rec_v7.Last ++;

    rec_v7.src_as ++;
    rec_v7.dst_as ++;

    rec_v7.router_sc ++;

  } /* for */

  return 0;

} /* gen_v7 */

int gen_v8_1(struct ftio *ftio, int count)
{
  struct fts3rec_v8_1 rec_v8_1;
  int i;

  bzero(&rec_v8_1, sizeof rec_v8_1);

  rec_v8_1.output = 0xFF00;
  rec_v8_1.Last = 0xFFFF0000;
  rec_v8_1.dst_as = 0xFF00;

  for (i = 0; i < count; ++i) {

    rec_v8_1.dPkts ++;
    rec_v8_1.dOctets ++;
    rec_v8_1.dFlows += 2;

    if (ftio_write(ftio, &rec_v8_1) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v8_1.input ++;
    rec_v8_1.output ++;

    rec_v8_1.First ++;
    rec_v8_1.Last ++;

    rec_v8_1.src_as ++;
    rec_v8_1.dst_as ++;

  } /* for */

  return 0;

} /* gen_v8_1 */

int gen_v8_2(struct ftio *ftio, int count)
{
  struct fts3rec_v8_2 rec_v8_2;
  int i;

  bzero(&rec_v8_2, sizeof rec_v8_2);

  rec_v8_2.Last = 0xFFFF0000;
  rec_v8_2.dstport = 0xFF00;

  for (i = 0; i < count; ++i) {

    rec_v8_2.dPkts ++;
    rec_v8_2.dOctets ++;
    rec_v8_2.dFlows += 2;

    if (ftio_write(ftio, &rec_v8_2) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v8_2.First ++;
    rec_v8_2.Last ++;

    rec_v8_2.srcport ++;
    rec_v8_2.dstport ++;

  } /* for */

  return 0;

} /* gen_v8_2 */

int gen_v8_3(struct ftio *ftio, int count)
{
  struct fts3rec_v8_3 rec_v8_3;
  int i;

  bzero(&rec_v8_3, sizeof rec_v8_3);

  rec_v8_3.Last = 0xFFFF0000;

  for (i = 0; i < count; ++i) {

    rec_v8_3.dPkts ++;
    rec_v8_3.dOctets ++;
    rec_v8_3.dFlows += 2;

    if (ftio_write(ftio, &rec_v8_3) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v8_3.First ++;
    rec_v8_3.Last ++;

    rec_v8_3.srcaddr ++;
    rec_v8_3.src_mask ++;
    rec_v8_3.src_as ++;
    rec_v8_3.input ++;
   

  } /* for */

  return 0;

} /* gen_v8_3 */

int gen_v8_4(struct ftio *ftio, int count)
{
  struct fts3rec_v8_4 rec_v8_4;
  int i;

  bzero(&rec_v8_4, sizeof rec_v8_4);

  rec_v8_4.Last = 0xFFFF0000;

  for (i = 0; i < count; ++i) {

    rec_v8_4.dPkts ++;
    rec_v8_4.dOctets ++;
    rec_v8_4.dFlows += 2;

    if (ftio_write(ftio, &rec_v8_4) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v8_4.First ++;
    rec_v8_4.Last ++;

    rec_v8_4.dstaddr ++;
    rec_v8_4.dst_mask ++;
    rec_v8_4.dst_as ++;
    rec_v8_4.output ++;
   
  } /* for */

  return 0;

} /* gen_v8_4 */

int gen_v8_5(struct ftio *ftio, int count)
{
  struct fts3rec_v8_5 rec_v8_5;
  int i;

  bzero(&rec_v8_5, sizeof rec_v8_5);

  rec_v8_5.Last = 0xFFFF0000;
  rec_v8_5.dstaddr = 0xFFFF0000;
  rec_v8_5.dst_as = 0xFF00;
  rec_v8_5.output = 0xFF00;

  for (i = 0; i < count; ++i) {

    rec_v8_5.dPkts ++;
    rec_v8_5.dOctets ++;
    rec_v8_5.dFlows += 2;

    if (ftio_write(ftio, &rec_v8_5) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v8_5.First ++;
    rec_v8_5.Last ++;

    rec_v8_5.srcaddr ++;
    rec_v8_5.src_mask ++;
    rec_v8_5.src_as ++;
    rec_v8_5.input ++;

    rec_v8_5.dstaddr ++;
    rec_v8_5.dst_mask ++;
    rec_v8_5.dst_as ++;
    rec_v8_5.output ++;
   
  } /* for */

  return 0;

} /* gen_v8_5 */

int gen_v8_6(struct ftio *ftio, int count)
{
  struct fts3rec_v8_6 rec_v8_6;
  int i;

  bzero(&rec_v8_6, sizeof rec_v8_6);

  rec_v8_6.Last = 0xFFFF0000;
  rec_v8_6.dstaddr = 0xFFFF0000;
  rec_v8_6.output = 0xFF00;
  rec_v8_6.marked_tos = 0xFF;

  for (i = 0; i < count; ++i) {

    rec_v8_6.dPkts ++;
    rec_v8_6.dOctets ++;

    if (ftio_write(ftio, &rec_v8_6) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v8_6.First ++;
    rec_v8_6.Last ++;

    rec_v8_6.dstaddr ++;
    rec_v8_6.output ++;

    rec_v8_6.tos ++;
    rec_v8_6.marked_tos ++;
    rec_v8_6.router_sc ++;
    rec_v8_6.extra_pkts ++;
    rec_v8_6.router_sc ++;
   
  } /* for */

  return 0;

} /* gen_v8_6 */

int gen_v8_7(struct ftio *ftio, int count)
{
  struct fts3rec_v8_7 rec_v8_7;
  int i;

  bzero(&rec_v8_7, sizeof rec_v8_7);

  rec_v8_7.Last = 0xFFFF0000;
  rec_v8_7.dstaddr = 0xFFFF0000;
  rec_v8_7.output = 0xFF00;
  rec_v8_7.marked_tos = 0xFF;

  for (i = 0; i < count; ++i) {

    rec_v8_7.dPkts ++;
    rec_v8_7.dOctets ++;

    if (ftio_write(ftio, &rec_v8_7) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v8_7.First ++;
    rec_v8_7.Last ++;

    rec_v8_7.dstaddr ++;
    rec_v8_7.srcaddr ++;
    rec_v8_7.output ++;
    rec_v8_7.input ++;

    rec_v8_7.tos ++;
    rec_v8_7.marked_tos ++;
    rec_v8_7.router_sc ++;
    rec_v8_7.extra_pkts ++;
    rec_v8_7.router_sc ++;
   
  } /* for */

  return 0;

} /* gen_v8_7 */

int gen_v8_8(struct ftio *ftio, int count)
{
  struct fts3rec_v8_8 rec_v8_8;
  int i;

  bzero(&rec_v8_8, sizeof rec_v8_8);

  rec_v8_8.Last = 0xFFFF0000;
  rec_v8_8.dstaddr = 0xFFFF0000;
  rec_v8_8.output = 0xFF00;
  rec_v8_8.marked_tos = 0xFF;

  for (i = 0; i < count; ++i) {

    rec_v8_8.dPkts ++;
    rec_v8_8.dOctets ++;

    if (ftio_write(ftio, &rec_v8_8) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v8_8.First ++;
    rec_v8_8.Last ++;

    rec_v8_8.dstaddr ++;
    rec_v8_8.srcaddr ++;
    rec_v8_8.output ++;
    rec_v8_8.input ++;

    rec_v8_8.dstport ++;
    rec_v8_8.srcport ++;

    rec_v8_8.prot ++;
    rec_v8_8.tos ++;
    rec_v8_8.marked_tos ++;
    rec_v8_8.router_sc ++;
    rec_v8_8.extra_pkts ++;
    rec_v8_8.router_sc ++;
   
  } /* for */

  return 0;

} /* gen_v8_8 */

int gen_v8_9(struct ftio *ftio, int count)
{
  struct fts3rec_v8_9 rec_v8_9;
  int i;

  bzero(&rec_v8_9, sizeof rec_v8_9);

  rec_v8_9.output = 0xFF00;
  rec_v8_9.Last = 0xFFFF0000;
  rec_v8_9.dst_as = 0xFF00;

  for (i = 0; i < count; ++i) {

    rec_v8_9.dPkts ++;
    rec_v8_9.dOctets ++;
    rec_v8_9.dFlows += 2;

    if (ftio_write(ftio, &rec_v8_9) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v8_9.input ++;
    rec_v8_9.output ++;

    rec_v8_9.First ++;
    rec_v8_9.Last ++;

    rec_v8_9.src_as ++;
    rec_v8_9.dst_as ++;

    rec_v8_9.tos ++;

  } /* for */

  return 0;

} /* gen_v8_9 */

int gen_v8_10(struct ftio *ftio, int count)
{
  struct fts3rec_v8_10 rec_v8_10;
  int i;

  bzero(&rec_v8_10, sizeof rec_v8_10);

  rec_v8_10.Last = 0xFFFF0000;
  rec_v8_10.dstport = 0xFF00;

  for (i = 0; i < count; ++i) {

    rec_v8_10.dPkts ++;
    rec_v8_10.dOctets ++;
    rec_v8_10.dFlows += 2;

    if (ftio_write(ftio, &rec_v8_10) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v8_10.First ++;
    rec_v8_10.Last ++;

    rec_v8_10.srcport ++;
    rec_v8_10.dstport ++;

    rec_v8_10.tos ++;

  } /* for */

  return 0;

} /* gen_v8_10 */

int gen_v8_11(struct ftio *ftio, int count)
{
  struct fts3rec_v8_11 rec_v8_11;
  int i;

  bzero(&rec_v8_11, sizeof rec_v8_11);

  rec_v8_11.Last = 0xFFFF0000;

  for (i = 0; i < count; ++i) {

    rec_v8_11.dPkts ++;
    rec_v8_11.dOctets ++;
    rec_v8_11.dFlows += 2;

    if (ftio_write(ftio, &rec_v8_11) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v8_11.First ++;
    rec_v8_11.Last ++;

    rec_v8_11.srcaddr ++;
    rec_v8_11.src_mask ++;
    rec_v8_11.src_as ++;
    rec_v8_11.input ++;
   
    rec_v8_11.tos ++;

  } /* for */

  return 0;

} /* gen_v8_11 */

int gen_v8_12(struct ftio *ftio, int count)
{
  struct fts3rec_v8_12 rec_v8_12;
  int i;

  bzero(&rec_v8_12, sizeof rec_v8_12);

  rec_v8_12.Last = 0xFFFF0000;

  for (i = 0; i < count; ++i) {

    rec_v8_12.dPkts ++;
    rec_v8_12.dOctets ++;
    rec_v8_12.dFlows += 2;

    if (ftio_write(ftio, &rec_v8_12) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v8_12.First ++;
    rec_v8_12.Last ++;

    rec_v8_12.dstaddr ++;
    rec_v8_12.dst_mask ++;
    rec_v8_12.dst_as ++;
    rec_v8_12.output ++;

    rec_v8_12.tos ++;
   
  } /* for */

  return 0;

} /* gen_v8_12 */

int gen_v8_13(struct ftio *ftio, int count)
{
  struct fts3rec_v8_13 rec_v8_13;
  int i;

  bzero(&rec_v8_13, sizeof rec_v8_13);

  rec_v8_13.Last = 0xFFFF0000;
  rec_v8_13.dstaddr = 0xFFFF0000;
  rec_v8_13.dst_as = 0xFF00;
  rec_v8_13.output = 0xFF00;

  for (i = 0; i < count; ++i) {

    rec_v8_13.dPkts ++;
    rec_v8_13.dOctets ++;
    rec_v8_13.dFlows += 2;

    if (ftio_write(ftio, &rec_v8_13) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v8_13.First ++;
    rec_v8_13.Last ++;

    rec_v8_13.srcaddr ++;
    rec_v8_13.src_mask ++;
    rec_v8_13.src_as ++;
    rec_v8_13.input ++;

    rec_v8_13.dstaddr ++;
    rec_v8_13.dst_mask ++;
    rec_v8_13.dst_as ++;
    rec_v8_13.output ++;
   
    rec_v8_13.tos ++;

  } /* for */

  return 0;

} /* gen_v8_13 */

int gen_v8_14(struct ftio *ftio, int count)
{
  struct fts3rec_v8_14 rec_v8_14;
  int i;

  bzero(&rec_v8_14, sizeof rec_v8_14);

  rec_v8_14.Last = 0xFFFF0000;
  rec_v8_14.dstaddr = 0xFFFF0000;
  rec_v8_14.output = 0xFF00;
  rec_v8_14.dstport = 0xFF00;

  for (i = 0; i < count; ++i) {

    rec_v8_14.dPkts ++;
    rec_v8_14.dOctets ++;
    rec_v8_14.dFlows += 2;

    if (ftio_write(ftio, &rec_v8_14) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v8_14.First ++;
    rec_v8_14.Last ++;

    rec_v8_14.srcport ++;
    rec_v8_14.dstport ++;

    rec_v8_14.srcaddr ++;
    rec_v8_14.src_mask ++;
    rec_v8_14.input ++;

    rec_v8_14.dstaddr ++;
    rec_v8_14.dst_mask ++;
    rec_v8_14.output ++;

    rec_v8_14.tos ++;
    rec_v8_14.prot ++;

  } /* for */

  return 0;

} /* gen_v8_14 */

int gen_v1005(struct ftio *ftio, int count)
{
  struct fts3rec_v1005 rec_v1005;
  int i;

  bzero(&rec_v1005, sizeof rec_v1005);

  rec_v1005.dstaddr = 0x8092C500;
  rec_v1005.dstport = 0xFF00;
  rec_v1005.output = 0xFF00;
  rec_v1005.Last = 0xFFFF0000;
  rec_v1005.dst_as = 0xFF00;
  rec_v1005.prot = 17;
  rec_v1005.dst_tag = 0xFF;
  rec_v1005.nexthop = 0x8092C500;

  for (i = 0; i < count; ++i) {

    rec_v1005.dPkts ++;
    rec_v1005.dOctets ++;

    if (ftio_write(ftio, &rec_v1005) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    rec_v1005.srcaddr ++;
    rec_v1005.dstaddr ++;

    rec_v1005.srcport ++;
    rec_v1005.dstport ++;

    rec_v1005.input ++;
    rec_v1005.output ++;

    rec_v1005.First ++;
    rec_v1005.Last ++;

    rec_v1005.src_as ++;
    rec_v1005.dst_as ++;

    rec_v1005.src_tag ++;
    rec_v1005.dst_tag ++;

    rec_v1005.tos ++;

    rec_v1005.nexthop ++;

  } /* for */

  return 0;

} /* gen_v1005 */
void usage(void) {

  fprintf(stderr, "Usage: flow-gen [-h] [-b big|little] [-d debug_level] [-n count]\n");
  fprintf(stderr, "       [-V version] [-z z_level]\n");
  fprintf(stderr, "\n%s version %s: built by %s\n", PACKAGE, VERSION, FT_PROG_BUILD);

} /* usage */
