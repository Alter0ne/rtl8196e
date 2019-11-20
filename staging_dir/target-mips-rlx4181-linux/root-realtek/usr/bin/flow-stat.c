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
 *      $Id: flow-stat.c,v 1.46 2004/11/03 02:12:36 maf Exp $
 */

#include "ftconfig.h"
#include <ftlib.h>

#include <sys/types.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <netinet/in.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <time.h>
#include <ctype.h>
#include <fcntl.h>

#if HAVE_STRINGS_H
 #include <strings.h>
#endif

#if HAVE_STRING_H
  #include <string.h>
#endif

#include "ftbuild.h"

/*
 * XXX TODO
 *
 * wide format to include bandwidth
 * 
 * format groups
 *
 * tally for all reports
 *
 * html formatting
 *
 * possible memory leaks with new allocation
 *
 * x src,dst,src/dst formats also need just 'x' like IP
 *
 * bw summary in f0
 *
 * bw report
 *
 */

#define FORMATS 33  /* # of types of output */

#define CUR_GET\
    cur.duration = *((u_int32*)(rec+fo.Last)) - *((u_int32*)(rec+fo.First));\
    cur.octets = *((u_int32*)(rec+fo.dOctets));\
    cur.packets = *((u_int32*)(rec+fo.dPkts));\

#define CUR_GET_PLUS_FLOWS\
    if (args->ftio.xfield & FT_XFIELD_DFLOWS)\
      cur.flows = *((u_int32*)(rec+fo.dFlows));\
    cur.duration = *((u_int32*)(rec+fo.Last)) - *((u_int32*)(rec+fo.First));\
    cur.octets = *((u_int32*)(rec+fo.dOctets));\
    cur.packets = *((u_int32*)(rec+fo.dPkts));\

#define TOTAL_INC\
    total.flows += cur.flows;\
    total.octets += cur.octets;\
    total.packets += cur.packets;\
    total.duration += cur.duration;\

#define STAT_INCP(A)\
    A->nflows += cur.flows;\
    A->noctets += cur.octets;\
    A->npackets += cur.packets;\
    A->etime += cur.duration;\

#define STAT_INCA(A)\
    stat.flows[A] += cur.flows;\
    stat.octets[A] += cur.octets;\
    stat.packets[A] += cur.packets;\
    stat.duration[A] += cur.duration;\

int debug;

static u_int64 *sort_i64;

static int sort_cmp64(const void *a, const void *b);


struct fopdi {
  u_int64 *flows, *octets, *packets, *duration;
  u_int32 *index;
};

struct fopd32 {
  u_int32 flows, octets, packets, duration;
};

struct fopd32p {
  u_int32 flows, octets, packets, duration;
};

struct fopd {
  u_int64 flows, octets, packets, duration;
};

struct fmtargs {
  struct ftio ftio;
  int sort_order;
  int options;
  int tally;
  char cc;
};

struct jump {
    int (*where)(struct fmtargs *args);
};

int fopdi_alloc(struct fopdi *fopdi, int n);
void fopdi_free(struct fopdi *fopdi);

int tbl_out1(struct fmtargs *args, u_int nindex, struct fopdi *stat,
  struct fopd *total, char *title, char *symfile);

int chash_c32_dump (struct ftchash *ftch, char cc, int sort_order, int options,
  struct fopd *total, char *title, char *symfile);

int chash_c322_dump (struct ftchash *ftch, char cc, int sort_order, int options,
  struct fopd *total, char *title, char *title2, char *symfile);

int chash_ip_dump (struct ftchash *ftch, char cc, int sort_order, int options,
  struct fopd *total);

int chash_ip2_dump (struct ftchash *ftch, char cc, int sort_order, int options,
  struct fopd *total);

int chash_as2_dump (struct ftchash *ftch, char cc, int sort_order, int options,
  struct fopd *total, char *symfile);

int chash_if2_dump (struct ftchash *ftch, char cc, int sort_order, int options,
  struct fopd *total);

int chash_pre_dump (struct ftchash *ftch, char cc, int sort_order, int options,
  struct fopd *total);

int chash_pre2_dump (struct ftchash *ftch, char cc, int sort_order, int options,
  struct fopd *total);


int format0(struct fmtargs *args); int format1(struct fmtargs *args);
int format2(struct fmtargs *args); int format3(struct fmtargs *args);
int format4(struct fmtargs *args); int format5(struct fmtargs *args);
int format6(struct fmtargs *args); int format7(struct fmtargs *args);
int format8(struct fmtargs *args); int format9(struct fmtargs *args);
int format10(struct fmtargs *args); int format11(struct fmtargs *args);
int format12(struct fmtargs *args); int format13(struct fmtargs *args);
int format14(struct fmtargs *args); int format15(struct fmtargs *args);
int format16(struct fmtargs *args); int format17(struct fmtargs *args);
int format18(struct fmtargs *args); int format19(struct fmtargs *args);
int format20(struct fmtargs *args); int format21(struct fmtargs *args);
int format22(struct fmtargs *args); int format23(struct fmtargs *args);
int format24(struct fmtargs *args); int format25(struct fmtargs *args);
int format26(struct fmtargs *args); int format27(struct fmtargs *args);
int format28(struct fmtargs *args); int format29(struct fmtargs *args);
int format30(struct fmtargs *args); int format31(struct fmtargs *args);
int format32(struct fmtargs *args);

struct jump format[] = {
          {format0}, {format1}, {format2}, {format3}, {format4}, {format5},
          {format6}, {format7}, {format8}, {format9}, {format10}, {format11},
          {format12}, {format13}, {format14}, {format15}, {format16},
          {format17}, {format18}, {format19}, {format20}, {format21},
          {format22}, {format23}, {format24}, {format25}, {format26},
          {format27}, {format28}, {format29}, {format30}, {format31},
          {format32}, 
          };

char *format_name[] = {
  "Overall Summary",
  "Average packet size distribution",
  "Packets per flow distribution",
  "Octets per flow distribution",
  "Bandwidth per flow distribution",
  "UDP/TCP destination port",
  "UDP/TCP source port",
  "UDP/TCP port",
  "Destination IP",
  "Source IP",
  "Source/Destination IP",
  "Source or Destination IP",
  "IP protocol",
  "octets for flow duration plot data",
  "packets for flow duration plot data",
  "short summary",
  "IP Next Hop",
  "Input interface",
  "Output interface",
  "Source AS",
  "Destination AS",
  "Source/Destination AS",
  "IP ToS",
  "Input/Output Interface",
  "Source Prefix",
  "Destination Prefix",
  "Source/Destination Prefix",
  "Exporter IP",
  "Engine Id",
  "Engine Type",
  "Source Tag",
  "Destination Tag",
  "Source/Destination Tag",
  };

struct stat0 {

  u_int64 nflows;     /* total # flows */
  u_int64 noctets;    /* total # octets */
  u_int64 npackets;   /* total # packets */

  u_int64 time;       /* total time in 1/1000 of flows */

  double  aflowtime;  /* average time of flow */
  double  aps;        /* average packet size */
  double  afs;        /* average flow size */
  double  apf;        /* average packets per flow */
  double  fps;        /* average flows per second */
  double  fps_real;   /* average flows per second (realtime) */
  double  aos;        /* average octets per second */
  double  aos_real;   /* average octets per second (realtime) */

  u_int64 start;      /* earliest flow time */
  u_int64 end;        /* latest flow time */

  u_int32 time_start; /* earliest flow (realtime) */
  u_int32 time_end;   /* last flow (realtime) */

  u_int32 time_real;  /* realtime duration */

  /* average packet sizes */
  u_int64 psize32;    /* bytes/packet 1    <= p <= 32 */
  u_int64 psize64;    /* bytes/packet 32   < p <= 64  */
  u_int64 psize96; u_int64 psize128; u_int64 psize160; u_int64 psize192;
  u_int64 psize224; u_int64 psize256; u_int64 psize288; u_int64 psize320;
  u_int64 psize352; u_int64 psize384; u_int64 psize416; u_int64 psize448;
  u_int64 psize480; u_int64 psize512; u_int64 psize544; u_int64 psize576;
  u_int64 psize1024; u_int64 psize1536; u_int64 psize2048; u_int64 psize2560;
  u_int64 psize3072; u_int64 psize3584; u_int64 psize4096; u_int64 psize4608;

  /* packets per flow */
  u_int64 fpsize1;    /* packets/flow = 1 */
  u_int64 fpsize2;    /* packets/flow = 2 */
  u_int64 fpsize4;    /* packets/flow 2 < p <= 4 */
  u_int64 fpsize8;    /* packets/flow 4 < p <= 8 */
  u_int64 fpsize12; u_int64 fpsize16; u_int64 fpsize20; u_int64 fpsize24;
  u_int64 fpsize28; u_int64 fpsize32; u_int64 fpsize36; u_int64 fpsize40;
  u_int64 fpsize44; u_int64 fpsize48; u_int64 fpsize52; u_int64 fpsize60;
  u_int64 fpsize100; u_int64 fpsize200; u_int64 fpsize300; u_int64 fpsize400;
  u_int64 fpsize500; u_int64 fpsize600; u_int64 fpsize700; u_int64 fpsize800;
  u_int64 fpsize900;
  u_int64 fpsize_other; /* packets/flow 200 < p */

  /* octets per flow */
  u_int64 fosize32;     /* octets/flow 1    <= p <= 32 */
  u_int64 fosize64;     /* octets/flow 32   < p <= 64 */
  u_int64 fosize128;    /* octets/flow 64   < p <= 128 */
  u_int64 fosize256;    /* octets/flow 128   < p <= 256 */
  u_int64 fosize512; u_int64 fosize1280; u_int64 fosize2048;
  u_int64 fosize2816; u_int64 fosize3584; u_int64 fosize4352;
  u_int64 fosize5120; u_int64 fosize5888; u_int64 fosize6656;
  u_int64 fosize7424; u_int64 fosize8192; u_int64 fosize8960;
  u_int64 fosize9728; u_int64 fosize10496; u_int64 fosize11264;
  u_int64 fosize12032; u_int64 fosize12800; u_int64 fosize13568;
  u_int64 fosize14336; u_int64 fosize15104; u_int64 fosize15872;
  u_int64 fosize_other; /* octets/flow 15872   < p */

  /* time per flow */
  u_int64 ftime10;    /* time/flow 1 <= p <= 10 */
  u_int64 ftime50;    /* time/flow 10 < p <= 50 */
  u_int64 ftime100; u_int64 ftime200; u_int64 ftime500; u_int64 ftime1000;
  u_int64 ftime2000; u_int64 ftime3000; u_int64 ftime4000; u_int64 ftime5000;
  u_int64 ftime6000; u_int64 ftime7000; u_int64 ftime8000; u_int64 ftime9000;
  u_int64 ftime10000; u_int64 ftime12000; u_int64 ftime14000;
  u_int64 ftime16000; u_int64 ftime18000; u_int64 ftime20000;
  u_int64 ftime22000; u_int64 ftime24000; u_int64 ftime26000;
  u_int64 ftime28000; u_int64 ftime30000;
  u_int64 ftime_other;  /* time/flow 2000 < p */

};

void usage(void);

int main(int argc, char **argv)
{
  struct fmtargs args;
  struct ftprof ftp;
  int x, y, ret, i, format_index, print_header, usage_call;
  char *title, *c;

  title = (char*)0L;
  usage_call = 0;

  /* init fterr */
  fterr_setid(argv[0]);

  /* profile */
  ftprof_start (&ftp);

  args.options = 0;
  format_index = 0;
  print_header = 0;
  args.cc = '#';
  args.sort_order = 0;

  while ((i = getopt(argc, argv, "c:d:f:h?npPs:S:t:T:w")) != -1)

    switch (i) {

    case 'c': /* comment character */
      args.cc = optarg[0];
      break;

    case 'd': /* debug */
      debug = atoi(optarg);
      break;

    case 'f': /* format */
      format_index = atoi(optarg);
      break;

    case 'h': /* help */
    case '?': /* help */
      usage();
      ++usage_call;
      break;

    case 'n': /* use names */
      args.options |= FT_OPT_NAMES;
      break;

    case 'p': /* print header */
      print_header = 1;
      break;

    case 'P': /* percent's */
      args.options |= FT_OPT_PERCENT;
      break;

    case 's': /* sort low to high on field n */
      args.sort_order = (atoi(optarg)+1) * -1;
      break;

    case 'S': /* sort high to low on field n */
      args.sort_order = (atoi(optarg)+1);
      break;

    case 't': /* tallies */
      args.options |= FT_OPT_TALLY;
      args.tally = atoi(optarg);
      break;

    case 'T': /* title */
      title = optarg;
      break;

    case 'w': /* wide output */
      args.options |= FT_OPT_WIDE;
      break;

    default:
      usage();
      exit (1);
      break;

    } /* switch */

  if (argc - optind)
    fterr_errx(1, "Extra arguments starting with %s.", argv[optind]);

  if (usage_call)
    exit (0);

  if (format_index >= FORMATS)
    fterr_errx(1, "No such format, %d\n", format_index);

  /* read from stdin */
  if (ftio_init(&args.ftio, 0, FT_IO_FLAG_READ) < 0)
    fterr_errx(1, "ftio_init(): failed");

  printf("%c  --- ---- ---- Report Information --- --- ---\n", args.cc);
  printf("%c\n", args.cc);

  if (title)
    printf("%c Title:     %s\n", args.cc, title);

  printf("%c Fields:    %sTotal\n", args.cc,
    (args.options & FT_OPT_PERCENT) ?  "Percent " : "");

  printf("%c Symbols:   %s\n", args.cc,
    (args.options & FT_OPT_NAMES) ? "Enabled" :
    "Disabled");

  if (!args.sort_order)
    c = "None";
  else if (args.sort_order > 0)
    c = "Descending";
  else
    c = "Ascending";
  printf("%c Sorting:   %s", args.cc, c);

  if (args.sort_order)
    printf(" Field %d\n", abs(args.sort_order)-1);
  else
    printf("\n");

  printf("%c Name:      %s\n", args.cc, format_name[format_index]);

  /* print cmdl args */
  printf("%c\n%c Args:      ", args.cc, args.cc);
  for (x = 0; x < argc; ++x) {
    for (y = 0; y < strlen(argv[x]); ++y) {
      if (isprint((int)argv[x][y])) putc(argv[x][y], stdout);
    }
    putc (' ', stdout);
  }
  putc ('\n', stdout);

  printf("%c\n", args.cc);

  if (print_header) {
    ftio_header_print(&args.ftio, stdout, args.cc);
  }

  ret = format[format_index].where(&args);

  if ((!ret) && (debug > 0)) {
    ftprof_end(&ftp, ftio_get_rec_total(&args.ftio));
    ftprof_print(&ftp, argv[0], stderr);
  }

  ret = 0;

  return ret;

} /* main */

/*
 * function: format0
 *
 *  prints long summary
 *
 * returns 0 for success.
 */
int format0(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopd32 cur;
  struct ftver ftv;
  struct stat0 fs0;
  u_long p;
  char fmt_buf[256];
  u_int32 time_tmp;
  char *rec;
  u_int32 First, Last, unix_secs;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_UNIX_SECS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  bzero(&fs0, sizeof fs0);

  fs0.start = 0xFFFFFFFF;
  fs0.end = 0;
  fs0.time_start = 0xFFFFFFFF;
  fs0.time_end = 0;

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;

    First = *((u_int32*)(rec+fo.First));
    Last = *((u_int32*)(rec+fo.Last));
    unix_secs = *((u_int32*)(rec+fo.unix_secs));

    if (!cur.packets) {
      fprintf(stderr, "Ignoring bogus flow dPkts=0\n");
      continue;
    }

    fs0.nflows += cur.flows;
    fs0.noctets += cur.octets;
    fs0.npackets += cur.packets;

    time_tmp = unix_secs;

    if (time_tmp < fs0.time_start)
      fs0.time_start = time_tmp;

    if (time_tmp > fs0.time_end)
      fs0.time_end = time_tmp;

    if (First < fs0.start)
      fs0.start = First;

    if (Last > fs0.end)
      fs0.end = Last;

    p = cur.octets / cur.packets;

    if (p <= 32) ++ fs0.psize32;
    else if (p <= 64) ++ fs0.psize64; else if (p <= 96) ++ fs0.psize96;
    else if (p <= 128) ++ fs0.psize128; else if (p <= 160) ++ fs0.psize160;
    else if (p <= 192) ++ fs0.psize192; else if (p <= 224) ++ fs0.psize224;
    else if (p <= 256) ++ fs0.psize256; else if (p <= 288) ++ fs0.psize288;
    else if (p <= 320) ++ fs0.psize320; else if (p <= 352) ++ fs0.psize352;
    else if (p <= 384) ++ fs0.psize384; else if (p <= 416) ++ fs0.psize416;
    else if (p <= 448) ++ fs0.psize448; else if (p <= 480) ++ fs0.psize480;
    else if (p <= 512) ++ fs0.psize512; else if (p <= 544) ++ fs0.psize544;
    else if (p <= 576) ++ fs0.psize576; else if (p <= 1024) ++ fs0.psize1024;
    else if (p <= 1536) ++ fs0.psize1536; else if (p <= 2048) ++ fs0.psize2048;
    else if (p <= 2560) ++ fs0.psize2560; else if (p <= 3072) ++ fs0.psize3072;
    else if (p <= 3584) ++ fs0.psize3584; else if (p <= 4096) ++ fs0.psize4096;
    else if (p <= 4608) ++ fs0.psize4608;

    p = cur.packets;
    if (p <= 1) ++ fs0.fpsize1; else if (p <= 2) ++ fs0.fpsize2;
    else if (p <= 4) ++ fs0.fpsize4; else if (p <= 8) ++ fs0.fpsize8;
    else if (p <= 12) ++ fs0.fpsize12; else if (p <= 16) ++ fs0.fpsize16;
    else if (p <= 20) ++ fs0.fpsize20; else if (p <= 24) ++ fs0.fpsize24;
    else if (p <= 28) ++ fs0.fpsize28; else if (p <= 32) ++ fs0.fpsize32;
    else if (p <= 36) ++ fs0.fpsize36; else if (p <= 40) ++ fs0.fpsize40;
    else if (p <= 44) ++ fs0.fpsize44; else if (p <= 48) ++ fs0.fpsize48;
    else if (p <= 52) ++ fs0.fpsize52; else if (p <= 60) ++ fs0.fpsize60;
    else if (p <= 100) ++ fs0.fpsize100; else if (p <= 200) ++ fs0.fpsize200;
    else if (p <= 300) ++ fs0.fpsize300; else if (p <= 400) ++ fs0.fpsize400;
    else if (p <= 500) ++ fs0.fpsize500; else if (p <= 600) ++ fs0.fpsize600;
    else if (p <= 700) ++ fs0.fpsize700; else if (p <= 800) ++ fs0.fpsize800;
    else if (p <= 900) ++ fs0.fpsize900; else ++ fs0.fpsize_other;

    p = cur.octets;
    if (p <= 32) ++ fs0.fosize32;
    else if (p <= 64) ++ fs0.fosize64; else if (p <= 128) ++ fs0.fosize128;
    else if (p <= 256) ++ fs0.fosize256; else if (p <= 512) ++ fs0.fosize512;
    else if (p <= 1280) ++ fs0.fosize1280;
    else if (p <= 2048) ++ fs0.fosize2048;
    else if (p <= 2816) ++ fs0.fosize2816;
    else if (p <= 3584) ++ fs0.fosize3584;
    else if (p <= 4352) ++ fs0.fosize4352;
    else if (p <= 5120) ++ fs0.fosize5120;
    else if (p <= 5888) ++ fs0.fosize5888;
    else if (p <= 6656) ++ fs0.fosize6656;
    else if (p <= 7424) ++ fs0.fosize7424;
    else if (p <= 8192) ++ fs0.fosize8192;
    else if (p <= 8960) ++ fs0.fosize8960;
    else if (p <= 9728) ++ fs0.fosize9728;
    else if (p <= 10496) ++ fs0.fosize10496;
    else if (p <= 11264) ++ fs0.fosize11264;
    else if (p <= 12032) ++ fs0.fosize12032;
    else if (p <= 12800) ++ fs0.fosize12800;
    else if (p <= 13568) ++ fs0.fosize13568;
    else if (p <= 14336) ++ fs0.fosize14336;
    else if (p <= 15104) ++ fs0.fosize15104;
    else if (p <= 15872) ++ fs0.fosize15872;
    else ++ fs0.fosize_other;


    p = Last - First;
    fs0.time += p;

    if (p <= 10) ++ fs0.ftime10;
    else if (p <= 50) ++ fs0.ftime50; else if (p <= 100) ++ fs0.ftime100;
    else if (p <= 200) ++ fs0.ftime200; else if (p <= 500) ++ fs0.ftime500;
    else if (p <= 1000) ++ fs0.ftime1000; else if (p <= 2000) ++ fs0.ftime2000;
    else if (p <= 3000) ++ fs0.ftime3000; else if (p <= 4000) ++ fs0.ftime4000;
    else if (p <= 5000) ++ fs0.ftime5000; else if (p <= 6000) ++ fs0.ftime6000;
    else if (p <= 7000) ++ fs0.ftime7000; else if (p <= 8000) ++ fs0.ftime8000;
    else if (p <= 9000) ++ fs0.ftime9000;
    else if (p <= 10000) ++ fs0.ftime10000;
    else if (p <= 12000) ++ fs0.ftime12000;
    else if (p <= 14000) ++ fs0.ftime14000;
    else if (p <= 16000) ++ fs0.ftime16000;
    else if (p <= 18000) ++ fs0.ftime18000;
    else if (p <= 20000) ++ fs0.ftime20000;
    else if (p <= 22000) ++ fs0.ftime22000;
    else if (p <= 24000) ++ fs0.ftime24000;
    else if (p <= 26000) ++ fs0.ftime26000;
    else if (p <= 28000) ++ fs0.ftime28000;
    else if (p <= 30000) ++ fs0.ftime30000;
    else ++ fs0.ftime_other;

  }

  if (fs0.nflows) {

    fs0.aflowtime = (fs0.nflows != 0) ? fs0.time / (float)fs0.nflows : 0;
    fs0.aps = (fs0.npackets != 0) ? fs0.noctets / (float)fs0.npackets : 0;
    fs0.afs = (fs0.nflows != 0) ? fs0.noctets / (float)fs0.nflows : 0;
    fs0.apf = (fs0.nflows != 0) ?  fs0.npackets / (float)fs0.nflows : 0;
    fs0.fps = (float)fs0.nflows / ((fs0.end - fs0.start) / 1000);
    fs0.aos = ((float)(fs0.noctets*8) / 1000) / ((fs0.end - fs0.start) / 1000);
    fs0.time_real = (fs0.npackets != 0) ? fs0.time_end - fs0.time_start : 0;
 
    fs0.fps_real = (fs0.time_real != 0) ? ((float)fs0.nflows /
      (float)fs0.time_real) : 0; 
    fs0.aos_real = (fs0.time_real != 0) ? (((float)(fs0.noctets*8) / 1000) /
      (fs0.time_real)) : 0;
  
    strcpy(fmt_buf, "Total Flows                     : ");
    fmt_uint64(fmt_buf+34, fs0.nflows, FMT_JUST_LEFT);
    puts(fmt_buf);
  
    strcpy(fmt_buf, "Total Octets                    : ");
    fmt_uint64(fmt_buf+34, fs0.noctets, FMT_JUST_LEFT);
    puts(fmt_buf);
  
    strcpy(fmt_buf, "Total Packets                   : ");
    fmt_uint64(fmt_buf+34, fs0.npackets, FMT_JUST_LEFT);
    puts(fmt_buf);
  
    strcpy(fmt_buf, "Total Time (1/1000 secs) (flows): ");
    fmt_uint64(fmt_buf+34, fs0.time, FMT_JUST_LEFT);
    puts(fmt_buf);
  
    strcpy(fmt_buf, "Duration of data  (realtime)    : ");
    fmt_uint32(fmt_buf+34, fs0.time_real, FMT_JUST_LEFT);
    puts(fmt_buf);
  
    strcpy(fmt_buf, "Duration of data (1/1000 secs)  : ");
    fmt_uint64(fmt_buf+34, (fs0.npackets != 0) ? (fs0.end - fs0.start) : 0,
      FMT_JUST_LEFT);
    puts(fmt_buf);
  
    printf("Average flow time (1/1000 secs) : %4.4f\n", fs0.aflowtime);
    printf("Average packet size (octets)    : %4.4f\n", fs0.aps);
    printf("Average flow size (octets)      : %4.4f\n", fs0.afs);
    printf("Average packets per flow        : %4.4f\n", fs0.apf);
    printf("Average flows / second (flow)   : %4.4f\n", fs0.fps);
    printf("Average flows / second (real)   : %4.4f\n", fs0.fps_real);
    printf("Average Kbits / second (flow)   : %4.4f\n", fs0.aos);
    printf("Average Kbits / second (real)   : %4.4f\n", fs0.aos_real);
  
    printf("\n\n");
  
    printf("IP packet size distribution:\n");
    printf("   1-32   64   96  128  160  192  224  256  288  320  352  384  416  448  480\n   ");
    print_3float((float)fs0.psize32 / fs0.nflows);
    print_3float((float)fs0.psize64 / fs0.nflows);
    print_3float((float)fs0.psize96 / fs0.nflows);
    print_3float((float)fs0.psize128 / fs0.nflows);
    print_3float((float)fs0.psize160 / fs0.nflows);
    print_3float((float)fs0.psize192 / fs0.nflows);
    print_3float((float)fs0.psize224 / fs0.nflows);
    print_3float((float)fs0.psize256 / fs0.nflows);
    print_3float((float)fs0.psize288 / fs0.nflows);
    print_3float((float)fs0.psize320 / fs0.nflows);
    print_3float((float)fs0.psize352 / fs0.nflows);
    print_3float((float)fs0.psize384 / fs0.nflows);
    print_3float((float)fs0.psize416 / fs0.nflows);
    print_3float((float)fs0.psize448 / fs0.nflows);
    print_3float((float)fs0.psize480 / fs0.nflows);
    printf("\n\n");
  
    printf("    512  544  576 1024 1536 2048 2560 3072 3584 4096 4608\n   ");
    print_3float((float)fs0.psize512 / fs0.nflows);
    print_3float((float)fs0.psize544 / fs0.nflows);
    print_3float((float)fs0.psize576 / fs0.nflows);
    print_3float((float)fs0.psize1024 / fs0.nflows);
    print_3float((float)fs0.psize1536 / fs0.nflows);
    print_3float((float)fs0.psize2048 / fs0.nflows);
    print_3float((float)fs0.psize2560 / fs0.nflows);
    print_3float((float)fs0.psize3072 / fs0.nflows);
    print_3float((float)fs0.psize3584 / fs0.nflows);
    print_3float((float)fs0.psize4096 / fs0.nflows);
    print_3float((float)fs0.psize4608 / fs0.nflows);
    printf("\n\n");
  
    printf("Packets per flow distribution:\n");
    printf("      1    2    4    8   12   16   20   24   28   32   36   40   44   48   52\n   ");
  
    print_3float((float)fs0.fpsize1 / fs0.nflows);
    print_3float((float)fs0.fpsize2 / fs0.nflows);
    print_3float((float)fs0.fpsize4 / fs0.nflows);
    print_3float((float)fs0.fpsize8 / fs0.nflows);
    print_3float((float)fs0.fpsize12 / fs0.nflows);
    print_3float((float)fs0.fpsize16 / fs0.nflows);
    print_3float((float)fs0.fpsize20 / fs0.nflows);
    print_3float((float)fs0.fpsize24 / fs0.nflows);
    print_3float((float)fs0.fpsize28 / fs0.nflows);
    print_3float((float)fs0.fpsize32 / fs0.nflows);
    print_3float((float)fs0.fpsize36 / fs0.nflows);
    print_3float((float)fs0.fpsize40 / fs0.nflows);
    print_3float((float)fs0.fpsize44 / fs0.nflows);
    print_3float((float)fs0.fpsize48 / fs0.nflows);
    print_3float((float)fs0.fpsize52 / fs0.nflows);
    printf("\n\n     60  100  200  300  400  500  600  700  800  900 >900\n   ");
    print_3float((float)fs0.fpsize60 / fs0.nflows);
    print_3float((float)fs0.fpsize100 / fs0.nflows);
    print_3float((float)fs0.fpsize200 / fs0.nflows);
    print_3float((float)fs0.fpsize300 / fs0.nflows);
    print_3float((float)fs0.fpsize400 / fs0.nflows);
    print_3float((float)fs0.fpsize500 / fs0.nflows);
    print_3float((float)fs0.fpsize600 / fs0.nflows);
    print_3float((float)fs0.fpsize700 / fs0.nflows);
    print_3float((float)fs0.fpsize800 / fs0.nflows);
    print_3float((float)fs0.fpsize900 / fs0.nflows);
    print_3float((float)fs0.fpsize_other / fs0.nflows);
    printf("\n\n");
               
    printf("Octets per flow distribution:\n");
    printf("     32   64  128  256  512 1280 2048 2816 3584 4352 5120 5888 6656 7424 8192\n   ");
    print_3float((float)fs0.fosize32 / fs0.nflows);
    print_3float((float)fs0.fosize64 / fs0.nflows);
    print_3float((float)fs0.fosize128 / fs0.nflows);
    print_3float((float)fs0.fosize256 / fs0.nflows);
    print_3float((float)fs0.fosize512 / fs0.nflows);
    print_3float((float)fs0.fosize1280 / fs0.nflows);
    print_3float((float)fs0.fosize2048 / fs0.nflows);
    print_3float((float)fs0.fosize2816 / fs0.nflows);
    print_3float((float)fs0.fosize3584 / fs0.nflows);
    print_3float((float)fs0.fosize4352 / fs0.nflows);
    print_3float((float)fs0.fosize5120 / fs0.nflows);
    print_3float((float)fs0.fosize5888 / fs0.nflows);
    print_3float((float)fs0.fosize6656 / fs0.nflows);
    print_3float((float)fs0.fosize7424 / fs0.nflows);
    print_3float((float)fs0.fosize8192 / fs0.nflows);
    printf("\n\n   8960 9728 10496 11264 12032 12800 13568 14336 15104 15872 >15872\n   ");
    print_3float((float)fs0.fosize8960 / fs0.nflows);
    print_3float2((float)fs0.fosize9728 / fs0.nflows);
    print_3float2((float)fs0.fosize10496 / fs0.nflows);
    print_3float2((float)fs0.fosize11264 / fs0.nflows);
    print_3float2((float)fs0.fosize12032 / fs0.nflows);
    print_3float2((float)fs0.fosize12800 / fs0.nflows);
    print_3float2((float)fs0.fosize13568 / fs0.nflows);
    print_3float2((float)fs0.fosize14336 / fs0.nflows);
    print_3float2((float)fs0.fosize15104 / fs0.nflows);
    print_3float2((float)fs0.fosize15872 / fs0.nflows);
    print_3float2((float)fs0.fosize_other / fs0.nflows);
    printf("\n\n");
  
    printf("Flow time distribution:\n");
    printf("    10    50  100  200  500 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000\n   ");
    print_3float((float)fs0.ftime10 / fs0.nflows);
    print_3float((float)fs0.ftime50 / fs0.nflows);
    print_3float((float)fs0.ftime100 / fs0.nflows);
    print_3float((float)fs0.ftime200 / fs0.nflows);
    print_3float((float)fs0.ftime500 / fs0.nflows);
    print_3float((float)fs0.ftime1000 / fs0.nflows);
    print_3float((float)fs0.ftime2000 / fs0.nflows);
    print_3float((float)fs0.ftime3000 / fs0.nflows);
    print_3float((float)fs0.ftime4000 / fs0.nflows);
    print_3float((float)fs0.ftime5000 / fs0.nflows);
    print_3float((float)fs0.ftime6000 / fs0.nflows);
    print_3float((float)fs0.ftime7000 / fs0.nflows);
    print_3float((float)fs0.ftime8000 / fs0.nflows);
    print_3float2((float)fs0.ftime9000 / fs0.nflows);
    print_3float2((float)fs0.ftime10000 / fs0.nflows);
    printf("\n\n  12000 14000 16000 18000 20000 22000 24000 26000 28000 30000 >30000\n   ");
    print_3float2((float)fs0.ftime12000 / fs0.nflows);
    print_3float2((float)fs0.ftime14000 / fs0.nflows);
    print_3float2((float)fs0.ftime16000 / fs0.nflows);
    print_3float2((float)fs0.ftime18000 / fs0.nflows);
    print_3float2((float)fs0.ftime20000 / fs0.nflows);
    print_3float2((float)fs0.ftime22000 / fs0.nflows);
    print_3float2((float)fs0.ftime24000 / fs0.nflows);
    print_3float2((float)fs0.ftime26000 / fs0.nflows);
    print_3float2((float)fs0.ftime28000 / fs0.nflows);
    print_3float2((float)fs0.ftime30000 / fs0.nflows);
    print_3float2((float)fs0.ftime_other / fs0.nflows);
    printf("\n\n");

  }

  return 0;

} /* format0 */

/*
 * function: format1
 *
 *  Average packet size distribution histogram
 *
 * returns 0 for success.
 */
int format1(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct ftchash *ftch;
  struct ftchash_rec_c32 ftch_recc32, *ftch_recc32p;
  struct fopd32 cur;
  struct ftver ftv;
  struct fopd total;
  u_int32 hash;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  bzero(&ftch_recc32, sizeof ftch_recc32);

  bzero(&total, sizeof total);

  if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_c32), 4, 65536))) {
    fterr_warnx("ftchash_new(): failed");
    return -1;
  }

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;
  
    TOTAL_INC;

    if (!cur.packets) {
      fprintf(stderr, "Ignoring bogus flow dPkts=0\n");
      continue;
    }

    ftch_recc32.c32 = cur.octets / cur.packets;

    hash = (ftch_recc32.c32>>16) ^ (ftch_recc32.c32 & 0xFFFF);
    
    if (!(ftch_recc32p = ftchash_update(ftch, &ftch_recc32, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }

    STAT_INCP(ftch_recc32p);

  }

  chash_c32_dump(ftch, args->cc, args->sort_order, args->options, &total,
    "Pkt Size", (char*)0L);

  ftchash_free(ftch);

  return 0;

} /* format1 */


/*
 * function: format2
 *
 *  Packets per flow distribution histogram
 *
 * returns 0 for success.
 */
int format2(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct ftchash *ftch;
  struct ftchash_rec_c32 ftch_recc32, *ftch_recc32p;
  struct fopd32 cur;
  struct ftver ftv;
  struct fopd total;
  u_int32 hash;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);
  
  bzero(&ftch_recc32, sizeof ftch_recc32);

  bzero(&total, sizeof total);

  if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_ip), 4, 65536))) {
    fterr_warnx("ftchash_new(): failed");
    return -1;
  }

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;
       
    TOTAL_INC;

    ftch_recc32.c32 = cur.packets;

    hash = (ftch_recc32.c32>>16) ^ (ftch_recc32.c32 & 0xFFFF);
   
    if (!(ftch_recc32p = ftchash_update(ftch, &ftch_recc32, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }

    STAT_INCP(ftch_recc32p);
       
  }

  chash_c32_dump(ftch, args->cc, args->sort_order, args->options, &total,
    "Packets ", (char*)0L);

  ftchash_free(ftch);

  return 0;

} /* format2 */

/*
 * function: format3
 *
 *  Octets per flow flow distribution histogram
 *
 * returns 0 for success.
 */
int format3(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopd32 cur;
  struct ftchash *ftch;
  struct ftchash_rec_c32 ftch_recc32, *ftch_recc32p;
  struct ftver ftv;
  struct fopd total;
  char *rec;
  u_int32 hash;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);
  
  bzero(&ftch_recc32, sizeof ftch_recc32);
  
  bzero(&total, sizeof total);

  if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_c32), 4, 65536))) {
    fterr_warnx("ftchash_new(): failed");
    return -1;
  }
 
  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;
  
    TOTAL_INC;

    ftch_recc32.c32 = cur.octets;

    hash = (ftch_recc32.c32>>16) ^ (ftch_recc32.c32 & 0xFFFF);
   
    if (!(ftch_recc32p = ftchash_update(ftch, &ftch_recc32, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }

    STAT_INCP(ftch_recc32p);

  }

  chash_c32_dump(ftch, args->cc, args->sort_order, args->options, &total,
    "Octets  ", (char*)0L);
     
  ftchash_free(ftch);
    
  return 0;

} /* format3 */

/*
 * function: format4
 *
 *  ??
 *
 */
int format4(struct fmtargs *args)
{

  printf("Not implemented.\n");
  return 0;
}

/*
 * function: format5
 *
 *  UDP/TCP destination port flows,octets,packets,duration histogram
 *
 * returns 0 for success.
 */
int format5(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopd32 cur;
  struct ftver ftv;
  struct fopdi stat;
  struct fopd total;
  char *rec;
  u_int8 prot;
  u_int16 dstport;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_PROT | FT_XFIELD_DSTPORT)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  if (fopdi_alloc(&stat, 65536) < 0)
    return -1;
      
  bzero(&total, sizeof total);

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    prot = *((u_int8*)(rec+fo.prot));

    /* ports only make sense for TCP and UDP */
    if ((prot != IPPROTO_UDP) && (prot != IPPROTO_TCP))
      continue;

    CUR_GET_PLUS_FLOWS;

    TOTAL_INC;

    dstport = *((u_int16*)(rec+fo.dstport));

    STAT_INCA(dstport);

  } /* while */

  tbl_out1(args, 65536, &stat, &total, "port      ", FT_PATH_SYM_TCP_PORT);

  fopdi_free(&stat);

  return 0;

} /* format5 */

/*
 * function: format6
 *
 *  UDP/TCP source port flows,octets,packets,duration
 *
 * returns 0 for success.
 */
int format6(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopd32 cur;
  struct fopdi stat;
  struct fopd total;
  struct ftver ftv;
  char *rec;
  u_int8 prot;
  u_int16 srcport;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_PROT | FT_XFIELD_SRCPORT)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  if (fopdi_alloc(&stat, 65536) < 0)
    return -1;
      
  bzero(&total, sizeof total);

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    prot = *((u_int8*)(rec+fo.prot));

    /* ports only make sense for TCP and UDP */
    if ((prot != IPPROTO_UDP) && (prot != IPPROTO_TCP))
      continue;

    CUR_GET_PLUS_FLOWS;

    TOTAL_INC;

    srcport = *((u_int16*)(rec+fo.srcport));
    
    STAT_INCA(srcport);

  } /* while */

  tbl_out1(args, 65536, &stat, &total, "port      ", FT_PATH_SYM_TCP_PORT);

  fopdi_free(&stat);

  return 0;

} /* format6 */

/*
 * function: format7
 *
 *  UDP/TCP port flows,octets,packets,duration histogram
 *
 * returns 0 for success.
 */
int format7(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopd32 cur;
  struct ftver ftv;
  struct fopdi stat;
  struct fopd total;
  char *rec;
  u_int8 prot;
  u_int16 srcport, dstport;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_PROT | FT_XFIELD_SRCPORT | FT_XFIELD_DSTPORT)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  if (fopdi_alloc(&stat, 65536) < 0)
    return -1;

  bzero(&total, sizeof total);

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    prot = *((u_int8*)(rec+fo.prot));

    /* ports only make sense for TCP and UDP */
    if ((prot != IPPROTO_UDP) && (prot != IPPROTO_TCP))
      continue;

    CUR_GET_PLUS_FLOWS;

    TOTAL_INC;
    TOTAL_INC;

    srcport = *((u_int16*)(rec+fo.srcport));
    dstport = *((u_int16*)(rec+fo.dstport));

    STAT_INCA(srcport);
    STAT_INCA(dstport);

  }

  tbl_out1(args, 65536, &stat, &total, "port      ", FT_PATH_SYM_TCP_PORT);

  fopdi_free(&stat);

  return 0;

} /* format7 */

/*
 * function: format8
 *
 *  Destination IP flows,octets,packets,duration
 *
 * returns 0 for success.
 */
int format8(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopd32 cur;
  struct ftver ftv;
  struct ftchash *ftch;
  struct ftchash_rec_ip ftch_recip, *ftch_recipp;
  struct fopd total;
  u_int32 hash;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_DSTADDR)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }
 
  fts3rec_compute_offsets(&fo, &ftv);

  bzero(&total, sizeof total);

  bzero(&ftch_recip, sizeof ftch_recip);

  if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_ip), 4, 65536))) {
    fterr_warnx("ftchash_new(): failed");
    return -1;
  }

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;

    TOTAL_INC;

    ftch_recip.addr = *((u_int32*)(rec+fo.dstaddr));

    hash = (ftch_recip.addr>>16) ^ (ftch_recip.addr & 0xFFFF);

    if (!(ftch_recipp = ftchash_update(ftch, &ftch_recip, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }

    STAT_INCP(ftch_recipp);

  }

  chash_ip_dump(ftch, args->cc, args->sort_order, args->options, &total);

  ftchash_free(ftch);

  return 0;

} /* format8 */

/*
 * function: format9
 *
 *  Source IP flows,octets,packets,duration
 *
 * returns 0 for success.
 */
int format9(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopd32 cur;
  struct ftver ftv;
  struct ftchash *ftch;
  struct ftchash_rec_ip ftch_recip, *ftch_recipp;
  struct fopd total;
  u_int32 hash;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_SRCADDR)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }
 
  fts3rec_compute_offsets(&fo, &ftv);

  bzero(&total, sizeof total);

  bzero(&ftch_recip, sizeof ftch_recip);

  if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_ip), 4, 65536))) {
    fterr_warnx("ftchash_new(): failed");
    return -1;
  }

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;

    TOTAL_INC;

    ftch_recip.addr = *((u_int32*)(rec+fo.srcaddr));

    hash = (ftch_recip.addr>>16) ^ (ftch_recip.addr & 0xFFFF);

    if (!(ftch_recipp = ftchash_update(ftch, &ftch_recip, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }

    STAT_INCP(ftch_recipp);

  }

  chash_ip_dump(ftch, args->cc, args->sort_order, args->options, &total);

  ftchash_free(ftch);

  return 0;

} /* format9 */

/*
 * function: format10
 *
 *  Source/Destination IP flows,octets,packets,duration
 *
 * returns 0 for success.
 */
int format10(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopd32 cur;
  struct ftver ftv;
  struct ftchash *ftch;
  struct ftchash_rec_ip2 ftch_recip2, *ftch_recip2p;
  struct fopd total;
  u_int32 hash;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_SRCADDR | FT_XFIELD_DSTADDR)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  bzero(&total, sizeof total);

  bzero(&ftch_recip2, sizeof ftch_recip2);

  if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_ip2), 8, 65536))) {
    fterr_warnx("ftchash_new(): failed");  
    return -1;
  }

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;

    TOTAL_INC;

    ftch_recip2.src_addr = *((u_int32*)(rec+fo.srcaddr));
    ftch_recip2.dst_addr = *((u_int32*)(rec+fo.dstaddr));

    hash =  (ftch_recip2.src_addr>>16) ^ (ftch_recip2.src_addr & 0xFFFF) ^
      (ftch_recip2.dst_addr>>16) ^ (ftch_recip2.dst_addr & 0xFFFF);

    if (!(ftch_recip2p = ftchash_update(ftch, &ftch_recip2, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }

    STAT_INCP(ftch_recip2p);

  }

  chash_ip2_dump(ftch, args->cc, args->sort_order, args->options, &total);
      
  ftchash_free(ftch);

  return 0;

} /* format10 */

/*
 * function: format11
 *
 *  IP flows,octets,packets,duration
 *
 * returns 0 for success.
 */
int format11(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopd32 cur;
  struct ftchash *ftch;
  struct ftchash_rec_ip ftch_recip, *ftch_recipp;
  struct ftver ftv;
  struct fopd total;
  u_int32 hash;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_SRCADDR | FT_XFIELD_DSTADDR)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  bzero(&total, sizeof total);

  bzero(&ftch_recip, sizeof ftch_recip);
 
  if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_ip), 4, 65536))) {
    fterr_warnx("ftchash_new(): failed");
    return -1;
  }

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;

    TOTAL_INC;

    ftch_recip.addr = *((u_int32*)(rec+fo.srcaddr));

    hash = (ftch_recip.addr>>16) ^ (ftch_recip.addr & 0xFFFF);

    if (!(ftch_recipp = ftchash_update(ftch, &ftch_recip, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }

    STAT_INCP(ftch_recipp);

    ftch_recip.addr = *((u_int32*)(rec+fo.dstaddr));

    hash = (ftch_recip.addr>>16) ^ (ftch_recip.addr & 0xFFFF);

    if (!(ftch_recipp = ftchash_update(ftch, &ftch_recip, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }

    STAT_INCP(ftch_recipp);

  }

  chash_ip_dump(ftch, args->cc, args->sort_order, args->options, &total);
      
  ftchash_free(ftch);

  return 0;
}

/*
 * function: format12
 *
 *  flows,octets,packets,duration by IP protocol
 *
 *  returns 0 for success.
 */
int format12(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopd32 cur;
  struct ftver ftv;
  struct fopdi stat;
  struct fopd total;
  char *rec;
  u_int8 prot;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_PROT)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  if (fopdi_alloc(&stat, 256) < 0)
    return -1;

  bzero(&total, sizeof total);

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;

    TOTAL_INC;
  
    prot = *((u_int8*)(rec+fo.prot));

    STAT_INCA(prot);

  }

  tbl_out1(args, 256, &stat, &total, "protocol  ", FT_PATH_SYM_IP_PROT);

  fopdi_free(&stat);

  return 0;

} /* format12 */

/*
 * function: format13
 *
 *  octets for flow duration plot data
 *
 *  returns 0 for success.
 */
int format13(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopd32 cur;
  struct ftver ftv;
  u_long ymin, ymax;
  u_long xmin, xmax;
  u_int32 First, Last;
  u_int64 nflows;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  nflows = 0;

  xmin = ymin = 0xFFFFFFFF;
  xmax = ymax = 0;

  printf("%c\n%c start      octets\n%c end        octets\n%c\n",
    args->cc, args->cc, args->cc, args->cc);

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;

    Last = *((u_int32*)(rec+fo.Last));
    First = *((u_int32*)(rec+fo.First));

    ++nflows;

    if (Last > xmax)
      xmax = Last;

    if (First < xmin)
      xmin = First;

    if (cur.octets > ymax)
      ymax = cur.octets;

    if (cur.octets < ymin)
      ymin = cur.octets;

    printf("%-10lu   %-10lu\n", (u_long)First, (u_long)cur.octets);
    printf("%-10lu   %-10lu\n\n", (u_long)Last, (u_long)cur.octets);

  }

  printf("%c xmin=%-10lu   ymin=%-10lu   xmax=%-10lu   ymax=%-10lu\n",
    args->cc, xmin, ymin, xmax, ymax);

  return 0;

} /* format13 */

/*
 * function: format14
 *
 *  octets for flow duration plot data
 *
 *  returns 0 for success.
 */
int format14(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopd32 cur;
  struct ftver ftv;
  u_long ymin, ymax;
  u_long xmin, xmax;
  u_int64 nflows;
  u_int32 First, Last;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  nflows = 0;

  xmin = ymin = 0xFFFFFFFF;
  xmax = ymax = 0;

  printf("%c\n%c start     packets\n%c end       packets\n%c\n",
    args->cc, args->cc, args->cc, args->cc);

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {
   
    CUR_GET_PLUS_FLOWS;

    Last = *((u_int32*)(rec+fo.Last));
    First = *((u_int32*)(rec+fo.First));

    if (Last > xmax)
      xmax = Last;

    if (First < xmin)
      xmin = First;

    if (cur.octets > ymax)
      ymax = cur.packets;

    if (cur.octets < ymin)
      ymin = cur.packets;

    printf("%-10lu    %-10lu\n", (u_long)First,
      (u_long)cur.packets);
    printf("%-10lu    %-10lu\n\n", (u_long)Last,
      (u_long)cur.packets);

  }

  printf("%c xmin=%-10lu   ymin=%-10lu   xmax=%-10lu   ymax=%-10lu\n",
    args->cc, xmin, ymin, xmax, ymax);

  return 0;

} /* format14 */


/*
 * function: format15
 *
 *  short summary
 *
 * returns 0 for success.
 */
int format15(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopd total;
  struct fopd32 cur;
  struct ftver ftv;
  char *rec;
  double mbytes;
  char fmt_buf[256];
  u_int len;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  bzero(&total, sizeof total);

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;
  
    TOTAL_INC;

  }

  mbytes = (double)total.octets / (double)(1000000);

  printf("%c\n%c Octets            Packets             MBytes\n%c\n",
    args->cc, args->cc, args->cc);

  len = fmt_uint64(fmt_buf, total.octets, FMT_PAD_RIGHT);
  len += fmt_uint64(fmt_buf+len, total.packets, FMT_PAD_RIGHT);

  printf("%s%3.3f\n", fmt_buf, mbytes);

  return 0;

} /* format15 */

/*
 * function: format16
 *
 *  Next hop flows,octets,packets,duration
 *
 * returns 0 for success.
 */
int format16(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopd32 cur;
  struct ftver ftv;
  struct ftchash *ftch;
  struct ftchash_rec_ip ftch_recip, *ftch_recipp;
  struct fopd total;
  u_int32 hash;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST| FT_XFIELD_NEXTHOP)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  bzero(&total, sizeof total);

  bzero(&ftch_recip, sizeof ftch_recip);
 
  if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_ip), 4, 65536))) {
    fterr_warnx("ftchash_new(): failed");  
    return -1;
  }

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;
    
    TOTAL_INC;

    ftch_recip.addr = *((u_int32*)(rec+fo.nexthop));
  
    hash = (ftch_recip.addr>>16) ^ (ftch_recip.addr & 0xFFFF);
   
    if (!(ftch_recipp = ftchash_update(ftch, &ftch_recip, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }

    STAT_INCP(ftch_recipp);

  }

  chash_ip_dump(ftch, args->cc, args->sort_order, args->options, &total);
      
  ftchash_free(ftch);

  return 0;

} /* format16 */

/*
 * function: format17
 *
 *  input interface flows,octets,packets,duration
 *
 * returns 0 for success.
 */
int format17(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopdi stat;
  struct fopd total;
  struct fopd32 cur;
  struct ftver ftv;
  char *rec;
  u_int16 input;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_INPUT)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  if (fopdi_alloc(&stat, 65536) < 0)
    return -1;

  bzero(&total, sizeof total);

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;
  
    TOTAL_INC;
       
    input = *((u_int16*)(rec+fo.input));

    STAT_INCA(input);

  }

  /* doesn't make sense here */
  args->options &= ~FT_OPT_NAMES;

  tbl_out1(args, 65536, &stat, &total, "interface ", (void*)0L);
    
  fopdi_free(&stat);

  return 0;

} /* format17 */

/*
 * function: format18
 *
 *  output interface flows,octets,packets,duration 
 *
 * returns 0 for success.
 */
int format18(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopdi stat;
  struct fopd total;
  struct fopd32 cur;
  struct ftver ftv;
  char *rec;
  u_int16 output;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_OUTPUT)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  if (fopdi_alloc(&stat, 65536) < 0)
    return -1;

  bzero(&total, sizeof total);

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;
 
    TOTAL_INC;

    output = *((u_int16*)(rec+fo.output));

    STAT_INCA(output);

  }

  /* doesn't make sense here */
  args->options &= ~FT_OPT_NAMES;

  tbl_out1(args, 65536, &stat, &total, "interface ", (void*)0L);
    
  fopdi_free(&stat);

  return 0;

} /* format18 */

/*
 * function: format19
 *
 *  Source AS flows,octets,packets,duration
 *
 * returns 0 for success.
 */
int format19(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopdi stat;
  struct fopd total;
  struct fopd32 cur;
  struct ftver ftv;
  char *rec;
  u_int16 src_as;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_SRC_AS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  if (fopdi_alloc(&stat, 65536) < 0)
    return -1;

  bzero(&total, sizeof total);

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;

    TOTAL_INC;
       
    src_as = *((u_int16*)(rec+fo.src_as));

    STAT_INCA(src_as);

  }

  tbl_out1(args, 65536, &stat, &total, "src AS    ", FT_PATH_SYM_ASN);
    
  fopdi_free(&stat);

  return 0;

} /* format19 */

/*
 * function: format20
 *
 *  Destination AS flows,octets,packets,duration
 *
 * returns 0 for success.
 */
int format20(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopdi stat;
  struct fopd total;
  struct fopd32 cur;
  struct ftver ftv;
  char *rec;
  u_int16 dst_as;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_DST_AS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  if (fopdi_alloc(&stat, 65536) < 0)
    return -1;
    
  bzero(&total, sizeof total);

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {
   
    CUR_GET_PLUS_FLOWS;
 
    TOTAL_INC;
       
    dst_as = *((u_int16*)(rec+fo.dst_as));

    STAT_INCA(dst_as);

  }

  tbl_out1(args, 65536, &stat, &total, "dst AS    ", FT_PATH_SYM_ASN);
    
  fopdi_free(&stat);

  return 0;

} /* format20 */

/*
 * function: format21
 *
 * Source/Destination AS flows,octets,packets,duration
 *
 * returns 0 for success.
 */
int format21(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct ftchash_rec_as2 ftch_recas2, *ftch_recas2p;
  struct ftchash *ftch;
  struct ftver ftv;
  struct fopd total;
  struct fopd32 cur;
  u_long hash;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_DST_AS | FT_XFIELD_SRC_AS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  bzero(&total, sizeof total);

  bzero(&ftch_recas2, sizeof ftch_recas2);
 
  if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_as2), 4, 65536))) {
    fterr_warnx("ftchash_new(): failed");
    return -1;
  }

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;

    TOTAL_INC;

    ftch_recas2.src_as = *((u_int16*)(rec+fo.src_as));
    ftch_recas2.dst_as = *((u_int16*)(rec+fo.dst_as));

    hash = (ftch_recas2.src_as) ^ (ftch_recas2.dst_as);

    if (!(ftch_recas2p = ftchash_update(ftch, &ftch_recas2, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }
    
    STAT_INCP(ftch_recas2p);

  }

  chash_as2_dump(ftch, args->cc, args->sort_order, args->options,
    &total, FT_PATH_SYM_ASN);
    
  ftchash_free(ftch);

  return 0;

} /* format21 */

/*
 * function: format22
 *
 *  flows,octets,packets,duration by IP ToS
 *
 * returns 0 for success.
 */
int format22(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopdi stat;
  struct fopd total;
  struct fopd32 cur;
  struct ftver ftv;
  char *rec;
  u_int8 tos;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_TOS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  if (fopdi_alloc(&stat, 256) < 0)
    return -1;

  bzero(&total, sizeof total);

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    tos = *((u_int8*)(rec+fo.tos));

    CUR_GET_PLUS_FLOWS;

    TOTAL_INC;

    STAT_INCA(tos);

  }

  tbl_out1(args, 256, &stat, &total, "ToS       ", (void*)0L);
    
  fopdi_free(&stat);

  return 0;

} /* format22 */

/*
 * function: format23
 *
 * Input/Output Interface flows,octets,packets,duration
 *
 * returns 0 for success.
 */
int format23(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct ftchash_rec_if2 ftch_recif2, *ftch_recif2p;
  struct ftchash *ftch;
  struct ftver ftv;
  struct fopd total;
  struct fopd32 cur;
  u_long hash;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_OUTPUT | FT_XFIELD_INPUT)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  bzero(&total, sizeof total);

  bzero(&ftch_recif2, sizeof ftch_recif2);
 
  if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_if2), 4, 65536))) {
    fterr_warnx("ftchash_new(): failed");
    return -1;
  }

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    ftch_recif2.input = *((u_int16*)(rec+fo.input));
    ftch_recif2.output = *((u_int16*)(rec+fo.output));

    hash = (ftch_recif2.input) ^ (ftch_recif2.output);

    if (!(ftch_recif2p = ftchash_update(ftch, &ftch_recif2, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }

    CUR_GET_PLUS_FLOWS;
    
    TOTAL_INC;
    
    STAT_INCP(ftch_recif2p);

  }

  chash_if2_dump(ftch, args->cc, args->sort_order, args->options, &total);
    
  ftchash_free(ftch);

  return 0;

} /* format23 */

/*
 * function: format24
 *
 *  Source prefix flows,octets,packets,duration
 *
 * returns 0 for success.
 */
int format24(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct ftchash *ftch;
  struct ftchash_rec_prefix ftch_recpre, *ftch_recprep;
  struct fopd total;
  struct fopd32 cur;
  struct ftver ftv;
  u_int32 hash;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_SRCADDR | FT_XFIELD_SRC_MASK)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  bzero(&total, sizeof total);

  bzero(&ftch_recpre, sizeof ftch_recpre);

  if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_prefix), 5,
    65536))) {
    fterr_warnx("ftchash_new(): failed");
    return -1;
  }

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    ftch_recpre.prefix = *((u_int32*)(rec+fo.srcaddr));
    ftch_recpre.mask = *((u_char*)(rec+fo.src_mask));

    /* mask off host bits */
    ftch_recpre.prefix &= ipv4_len2mask(ftch_recpre.mask);

    hash = (ftch_recpre.prefix>>16) ^ (ftch_recpre.prefix & 0xFFFF);
    hash = hash ^ (ftch_recpre.mask);

    if (!(ftch_recprep = ftchash_update(ftch, &ftch_recpre, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }

    CUR_GET_PLUS_FLOWS;
    
    TOTAL_INC;

    STAT_INCP(ftch_recprep);

  }

  chash_pre_dump(ftch, args->cc, args->sort_order, args->options, &total);

  ftchash_free(ftch);

  return 0;

} /* format24 */

/*
 * function: format25
 *
 *  Destination prefix flows,octets,packets,duration
 *
 * returns 0 for success.
 */
int format25(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct ftchash *ftch;
  struct ftchash_rec_prefix ftch_recpre, *ftch_recprep;
  struct fopd total;
  struct fopd32 cur;
  struct ftver ftv;
  u_int32 hash;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_DSTADDR | FT_XFIELD_DST_MASK)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  bzero(&total, sizeof total);

  bzero(&ftch_recpre, sizeof ftch_recpre);

  if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_prefix), 5,
    65536))) {
    fterr_warnx("ftchash_new(): failed");
    return -1;
  }

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    ftch_recpre.prefix = *((u_int32*)(rec+fo.dstaddr));
    ftch_recpre.mask = *((u_char*)(rec+fo.dst_mask));

    /* mask off host bits */
    ftch_recpre.prefix &= ipv4_len2mask(ftch_recpre.mask);

    hash = (ftch_recpre.prefix>>16) ^ (ftch_recpre.prefix & 0xFFFF);
    hash = hash ^ (ftch_recpre.mask);

    if (!(ftch_recprep = ftchash_update(ftch, &ftch_recpre, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }

    CUR_GET_PLUS_FLOWS;

    TOTAL_INC;

    STAT_INCP(ftch_recprep);

  }

  chash_pre_dump(ftch, args->cc, args->sort_order, args->options, &total);

  ftchash_free(ftch);

  return 0;

} /* format25 */

/*
 * function: format26
 *
 *  Source/Destination prefix flows,octets,packets,duration
 *
 * returns 0 for success.
 */
int format26(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct ftchash *ftch;
  struct ftchash_rec_prefix2 ftch_recpre2, *ftch_recpre2p;
  struct fopd total;
  struct fopd32 cur;
  struct ftver ftv;
  u_int32 hash;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_SRCADDR | FT_XFIELD_DSTADDR | FT_XFIELD_SRC_MASK |
    FT_XFIELD_DST_MASK)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  bzero(&total, sizeof total);

  bzero(&ftch_recpre2, sizeof ftch_recpre2);

  fts3rec_compute_offsets(&fo, &ftv);

  if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_prefix2), 16,
    65536))) {
    fterr_warnx("ftchash_new(): failed");
    return -1;
  }

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    ftch_recpre2.src_prefix = *((u_int32*)(rec+fo.srcaddr));
    ftch_recpre2.src_mask = *((u_char*)(rec+fo.src_mask));

    ftch_recpre2.dst_prefix = *((u_int32*)(rec+fo.dstaddr));
    ftch_recpre2.dst_mask = *((u_char*)(rec+fo.dst_mask));

    /* mask off host bits */
    ftch_recpre2.src_prefix &= ipv4_len2mask(ftch_recpre2.src_mask);
    ftch_recpre2.dst_prefix &= ipv4_len2mask(ftch_recpre2.dst_mask);

    hash = (ftch_recpre2.src_prefix>>16)^
           (ftch_recpre2.src_prefix & 0xFFFF)^
           (ftch_recpre2.dst_prefix>>16)^
           (ftch_recpre2.dst_prefix & 0xFFFF)^
           (ftch_recpre2.src_mask)^
           (u_int32)(ftch_recpre2.dst_mask<<8);

    if (!(ftch_recpre2p = ftchash_update(ftch, &ftch_recpre2, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }

    CUR_GET_PLUS_FLOWS;

    TOTAL_INC;

    STAT_INCP(ftch_recpre2p);

  }

  chash_pre2_dump(ftch, args->cc, args->sort_order, args->options, &total);

  ftchash_free(ftch);

  return 0;

} /* format26 */

/*
 * function: format27
 *
 *  Exporter IP flows,octets,packets,duration
 *
 * returns 0 for success.
 */
int format27(struct fmtargs *args)
{
  struct ftchash *ftch;
  struct ftchash_rec_ip ftch_recip, *ftch_recipp;
  struct fts3rec_offsets fo;
  struct fopd total;
  struct fopd32 cur;
  struct ftver ftv;
  u_int32 hash;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_EXADDR)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  bzero(&total, sizeof total);

  bzero(&ftch_recip, sizeof ftch_recip);

  cur.flows = 1;

  if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_ip), 4, 65536))) {
    fterr_warnx("ftchash_new(): failed");
    return -1;
  }

  while ((rec = ftio_read(&args->ftio))) {

    ftch_recip.addr = *((u_int32*)(rec+fo.exaddr));

    hash = (ftch_recip.addr>>16) ^ (ftch_recip.addr & 0xFFFF);

    if (!(ftch_recipp = ftchash_update(ftch, &ftch_recip, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }

    CUR_GET_PLUS_FLOWS;

    TOTAL_INC;

    STAT_INCP(ftch_recipp);

  } /* while */

  chash_ip_dump(ftch, args->cc, args->sort_order, args->options, &total);

  ftchash_free(ftch);

  return 0;
}

/*
 * function: format28
 *
 *  flows,octets,packets,duration by engine_id
 *
 *  returns 0 for success.
 */
int format28(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopdi stat;
  struct fopd total;
  struct fopd32 cur;
  struct ftver ftv;
  char *rec;
  u_char engine_id;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_ENGINE_ID)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  if (fopdi_alloc(&stat, 256) < 0)
    return -1;

  bzero(&total, sizeof total);

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;

    TOTAL_INC;

    engine_id = *((u_char*)(rec+fo.engine_id));

    STAT_INCA(engine_id);

  }

  tbl_out1(args, 256, &stat, &total, "engine_id ", (void*)0L);

  fopdi_free(&stat);

  return 0;

} /* format28 */

/*
 * function: format29
 *
 *  flows,octets,packets,duration by engine_type
 *
 *  returns 0 for success.
 */
int format29(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct fopdi stat;
  struct fopd total;
  struct fopd32 cur;
  struct ftver ftv;
  char *rec;
  u_char engine_type;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_ENGINE_TYPE)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  if (fopdi_alloc(&stat, 256) < 0)
    return -1;

  bzero(&total, sizeof total);

  cur.flows = 1;

  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET_PLUS_FLOWS;

    TOTAL_INC;

    engine_type = *((u_char*)(rec+fo.engine_type));

    STAT_INCA(engine_type);

  }

  tbl_out1(args, 256, &stat, &total, "engine_type ", (void*)0L);

  fopdi_free(&stat);

  return 0;

} /* format29 */

/*
 * function: format30
 *
 *  flows,octets,packets,duration by src_tag
 *
 *  returns 0 for success.
 */
int format30(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct ftchash *ftch;
  struct ftchash_rec_c32 ftch_recc32, *ftch_recc32p;
  struct fopd32 cur;
  struct ftver ftv;
  struct fopd total;
  u_int32 hash;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_SRC_TAG)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  bzero(&ftch_recc32, sizeof ftch_recc32);
  
  bzero(&total, sizeof total);
  
  if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_c32), 4, 65536))) {
    fterr_warnx("ftchash_new(): failed");
    return -1;
  }

  cur.flows = 1;
       
  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET;

    TOTAL_INC;

    ftch_recc32.c32 = *((u_int32*)(rec+fo.src_tag));

    hash = (ftch_recc32.c32>>16) ^ (ftch_recc32.c32 & 0xFFFF);

    if (!(ftch_recc32p = ftchash_update(ftch, &ftch_recc32, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }

    STAT_INCP(ftch_recc32p);

  } /* while */

  chash_c32_dump(ftch, args->cc, args->sort_order, args->options, &total,
    "Src Tag ", FT_PATH_SYM_TAG);
  
  ftchash_free(ftch);
    
  return 0;

} /* format30 */

/*
 * function: format31
 *
 *  flows,octets,packets,duration by dst_tag
 *
 *  returns 0 for success.
 */
int format31(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct ftchash *ftch;
  struct ftchash_rec_c32 ftch_recc32, *ftch_recc32p;
  struct fopd32 cur;
  struct ftver ftv;
  struct fopd total;
  u_int32 hash;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_DST_TAG)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  bzero(&ftch_recc32, sizeof ftch_recc32);
  
  bzero(&total, sizeof total);
  
  if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_c32), 4, 65536))) {
    fterr_warnx("ftchash_new(): failed");
    return -1;
  }

  cur.flows = 1;
       
  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET;

    TOTAL_INC;

    ftch_recc32.c32 = *((u_int32*)(rec+fo.dst_tag));

    hash = (ftch_recc32.c32>>16) ^ (ftch_recc32.c32 & 0xFFFF);

    if (!(ftch_recc32p = ftchash_update(ftch, &ftch_recc32, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }

    STAT_INCP(ftch_recc32p);

  } /* while */

  chash_c32_dump(ftch, args->cc, args->sort_order, args->options, &total,
    "Dst Tag ", FT_PATH_SYM_TAG);
  
  ftchash_free(ftch);
    
  return 0;

} /* format31 */

/*
 * function: format32
 *
 *  flows,octets,packets,duration by src/dst_tag
 *
 *  returns 0 for success.
 */
int format32(struct fmtargs *args)
{
  struct fts3rec_offsets fo;
  struct ftchash *ftch;
  struct ftchash_rec_c322 ftch_recc322, *ftch_recc322p;
  struct fopd32 cur;
  struct ftver ftv;
  struct fopd total;
  u_int32 hash;
  char *rec;

  ftio_get_ver(&args->ftio, &ftv);

  if (ftio_check_xfield(&args->ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_SRC_TAG | FT_XFIELD_DST_TAG)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  fts3rec_compute_offsets(&fo, &ftv);

  bzero(&ftch_recc322, sizeof ftch_recc322);
  
  bzero(&total, sizeof total);
  
  if (!(ftch = ftchash_new(65536, sizeof (struct ftchash_rec_c322),
    8, 65536))) {
    fterr_warnx("ftchash_new(): failed");
    return -1;
  }

  cur.flows = 1;
       
  while ((rec = ftio_read(&args->ftio))) {

    CUR_GET;

    TOTAL_INC;

    ftch_recc322.c32a = *((u_int32*)(rec+fo.src_tag));
    ftch_recc322.c32b = *((u_int32*)(rec+fo.dst_tag));

    hash = (ftch_recc322.c32a>>16) ^ (ftch_recc322.c32a & 0xFFFF) ^
      (ftch_recc322.c32b>>16) ^ (ftch_recc322.c32b & 0xFFFF);

    if (!(ftch_recc322p = ftchash_update(ftch, &ftch_recc322, hash))) {
      fterr_warnx("ftch_update(): failed");
      ftchash_free(ftch);
      return -1;
    }

    STAT_INCP(ftch_recc322p);

  } /* while */

  chash_c322_dump(ftch, args->cc, args->sort_order, args->options, &total,
    "Src Tag ", "Dst Tag   ", FT_PATH_SYM_TAG);
  
  ftchash_free(ftch);
    
  return 0;

} /* format32 */


int chash_ip_dump (struct ftchash *ftch, char cc, int sort_order, int options,
  struct fopd *total)
{
  struct ftchash_rec_ip *ftch_recip;
  char fmt_buf[256];
  int len, sort_flags, sort_offset;

  if (sort_order) {

    if (sort_order < 0)
      sort_flags = 0;
    else
      sort_flags = FT_CHASH_SORT_ASCENDING;

    if ((sort_order == 1) || (sort_order == -1)) {
      sort_offset = offsetof(struct ftchash_rec_ip, addr);
      sort_flags |= FT_CHASH_SORT_32;
    } else if ((sort_order == 2) || (sort_order == -2)) {
      sort_offset = offsetof(struct ftchash_rec_ip, nflows);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 3) || (sort_order == -3)) {
      sort_offset = offsetof(struct ftchash_rec_ip, noctets);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 4) || (sort_order == -4)) {
      sort_offset = offsetof(struct ftchash_rec_ip, npackets);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 5) || (sort_order == -5)) {
      sort_offset = offsetof(struct ftchash_rec_ip, etime);
      sort_flags |= FT_CHASH_SORT_64;
    } else {
      fprintf(stderr, "%c can't sort on field %d.\n", cc, sort_order);
      return -1;
    }

    ftchash_sort(ftch, sort_offset, sort_flags);

  } /* sorted? */

  if (options & FT_OPT_PERCENT) {
    if (options & FT_OPT_WIDE)
      printf("%c\n%c IPaddr         flows    octets   packets  duration\n%c\n", cc, cc, cc);
    else
      printf("%c\n%c IPaddr         flows    octets   packets\n%c\n", cc, cc, cc);
  } else {
    if (options & FT_OPT_WIDE)
      printf("%c\n%c IPaddr         flows                 octets                packets               duration\n%c\n", cc, cc, cc);
    else
      printf("%c\n%c IPaddr         flows                 octets                packets\n%c\n", cc, cc, cc);
  }

  ftchash_first(ftch);

  while ((ftch_recip = ftchash_foreach(ftch))) {

    if (options & FT_OPT_PERCENT) {
  
      fmt_ipv4(fmt_buf, ftch_recip->addr, FMT_PAD_RIGHT);

      if (options & FT_OPT_WIDE)

        printf("%-15.15s  %-6.3f   %-6.3f   %-6.3f   %-6.3f\n", fmt_buf,
          ((double)ftch_recip->nflows/(double)total->flows)*100,
          ((double)ftch_recip->noctets/(double)total->octets)*100,
          ((double)ftch_recip->npackets/(double)total->packets)*100,
          ((double)ftch_recip->etime/(double)total->duration)*100);
      else

        printf("%-15.15s  %-6.3f   %-6.3f   %-6.3f\n", fmt_buf,
          ((double)ftch_recip->nflows/(double)total->flows)*100,
          ((double)ftch_recip->noctets/(double)total->octets)*100,
          ((double)ftch_recip->npackets/(double)total->packets)*100);

    } else {
  
      len = fmt_ipv4(fmt_buf, ftch_recip->addr, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      len += fmt_uint64(fmt_buf+len, ftch_recip->nflows, FMT_PAD_RIGHT);
       fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      len += fmt_uint64(fmt_buf+len, ftch_recip->noctets, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';


      if (options & FT_OPT_WIDE) {
        len += fmt_uint64(fmt_buf+len, ftch_recip->npackets, FMT_PAD_RIGHT);
        fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';
        len += fmt_uint64(fmt_buf+len, ftch_recip->etime, FMT_JUST_LEFT);
      } else
        fmt_uint64(fmt_buf+len, ftch_recip->npackets, FMT_JUST_LEFT);

      puts(fmt_buf);

    }
  }

  return 0;

} /* chash_ip_dump */

int chash_ip2_dump (struct ftchash *ftch, char cc, int sort_order, int options,
  struct fopd *total)
{
  struct ftchash_rec_ip2 *ftch_recip2;
  char fmt_buf[256];
  int len, sort_flags, sort_offset;

  if (sort_order) {

    if (sort_order < 0)
      sort_flags = 0;
    else
      sort_flags = FT_CHASH_SORT_ASCENDING;

    if ((sort_order == 1) || (sort_order == -1)) {
      sort_offset = offsetof(struct ftchash_rec_ip2, src_addr);
      sort_flags |= FT_CHASH_SORT_32;
    } else if ((sort_order == 2) || (sort_order == -2)) {
      sort_offset = offsetof(struct ftchash_rec_ip2, dst_addr);
      sort_flags |= FT_CHASH_SORT_32;
    } else if ((sort_order == 3) || (sort_order == -3)) {
      sort_offset = offsetof(struct ftchash_rec_ip2, nflows);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 4) || (sort_order == -4)) {
      sort_offset = offsetof(struct ftchash_rec_ip2, noctets);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 5) || (sort_order == -5)) {
      sort_offset = offsetof(struct ftchash_rec_ip2, npackets);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 6) || (sort_order == -6)) {
      sort_offset = offsetof(struct ftchash_rec_ip2, etime);
      sort_flags |= FT_CHASH_SORT_64;
    } else {
      fprintf(stderr, "%c can't sort on field %d.\n", cc, sort_order);
      return -1;
    }

    ftchash_sort(ftch, sort_offset, sort_flags);

  } /* sorted? */

  if (options & FT_OPT_PERCENT) {
    if (options & FT_OPT_WIDE)
      printf("%c\n%c src IPaddr     dst IPaddr       flows    octets   packets  duration\n%c\n", cc, cc, cc);
    else
      printf("%c\n%c src IPaddr     dst IPaddr       flows    octets   packets\n%c\n", cc, cc, cc);
  } else {
    if (options & FT_OPT_WIDE)
      printf("%c\n%c src IPaddr     dst IPaddr       flows                 octets                packets               duration\n%c\n", cc, cc, cc);
    else
      printf("%c\n%c src IPaddr     dst IPaddr       flows                 octets                packets\n%c\n", cc, cc, cc);
  }

  ftchash_first(ftch);

  while ((ftch_recip2 = ftchash_foreach(ftch))) {

    if (options & FT_OPT_PERCENT) {

      len = fmt_ipv4(fmt_buf, ftch_recip2->src_addr, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      fmt_ipv4(fmt_buf+len, ftch_recip2->dst_addr, FMT_PAD_RIGHT);

      if (options & FT_OPT_WIDE)
        printf("%-32.32s  %-6.3f   %-6.3f   %-6.3f   %-6.3f\n", fmt_buf,
          ((double)ftch_recip2->nflows/(double)total->flows)*100,
          ((double)ftch_recip2->noctets/(double)total->octets)*100,
          ((double)ftch_recip2->npackets/(double)total->packets)*100,
          ((double)ftch_recip2->etime/(double)total->duration)*100);
      else
        printf("%-32.32s  %-6.3f   %-6.3f   %-6.3f\n", fmt_buf,
          ((double)ftch_recip2->nflows/(double)total->flows)*100,
          ((double)ftch_recip2->noctets/(double)total->octets)*100,
          ((double)ftch_recip2->npackets/(double)total->packets)*100);

    } else {
      
      len = fmt_ipv4(fmt_buf, ftch_recip2->src_addr, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      len += fmt_ipv4(fmt_buf+len, ftch_recip2->dst_addr, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      len += fmt_uint64(fmt_buf+len, ftch_recip2->nflows, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';
    
      len += fmt_uint64(fmt_buf+len, ftch_recip2->noctets, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      len += fmt_uint64(fmt_buf+len, ftch_recip2->npackets, FMT_PAD_RIGHT);

      if (options & FT_OPT_WIDE) {
        fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';
        len += fmt_uint64(fmt_buf+len, ftch_recip2->etime, FMT_JUST_LEFT);
      }

      puts(fmt_buf);
    }
  }

  return 0;

} /* chash_ip2_dump */


int chash_as2_dump (struct ftchash *ftch, char cc, int sort_order, int options,
  struct fopd *total, char *symfile)
{
  struct ftchash_rec_as2 *ftch_recas2;
  struct ftsym *ftsym;
  char fmt_buf[256], fmt_buf1[64], fmt_buf2[64];
  int len, sort_flags, sort_offset;

  ftsym = (struct ftsym*)0L;

  if (sort_order) {

    if (sort_order < 0)
      sort_flags = 0;
    else
      sort_flags = FT_CHASH_SORT_ASCENDING;

    if ((sort_order == 1) || (sort_order == -1)) {
      sort_offset = offsetof(struct ftchash_rec_as2, src_as);
      sort_flags |= FT_CHASH_SORT_16;
    } else if ((sort_order == 2) || (sort_order == -2)) {
      sort_offset = offsetof(struct ftchash_rec_as2, dst_as);
      sort_flags |= FT_CHASH_SORT_16;
    } else if ((sort_order == 3) || (sort_order == -3)) {
      sort_offset = offsetof(struct ftchash_rec_as2, nflows);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 4) || (sort_order == -4)) {
      sort_offset = offsetof(struct ftchash_rec_as2, noctets);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 5) || (sort_order == -5)) {
      sort_offset = offsetof(struct ftchash_rec_as2, npackets);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 6) || (sort_order == -6)) {
      sort_offset = offsetof(struct ftchash_rec_as2, etime);
      sort_flags |= FT_CHASH_SORT_64;
    } else {
      fprintf(stderr, "%c can't sort on field %d.\n", cc, sort_order);
      return -1;
    }

    ftchash_sort(ftch, sort_offset, sort_flags);

  } /* sorted? */

  if (options & FT_OPT_NAMES)
    ftsym = ftsym_new(symfile);

  if (options & FT_OPT_PERCENT) {
    if (options & FT_OPT_WIDE)
      printf("%c\n%c src AS          dst AS            flows    octets   packets  duration\n%c\n", cc, cc, cc);
    else
      printf("%c\n%c src AS          dst AS            flows    octets   packets\n%c\n", cc, cc, cc);
  } else {
    if (options & FT_OPT_WIDE)
      printf("%c\n%c src AS          dst AS            flows                 octets                packets               duration\n%c\n", cc, cc, cc);
    else
      printf("%c\n%c src AS          dst AS            flows                 octets                packets\n%c\n", cc, cc, cc);
  }

  ftchash_first(ftch);

  while ((ftch_recas2 = ftchash_foreach(ftch))) {

    fmt_uint16s(ftsym, 18, fmt_buf1, (u_int16)ftch_recas2->src_as,
      FMT_PAD_RIGHT);
    fmt_uint16s(ftsym, 18, fmt_buf2, (u_int16)ftch_recas2->dst_as,
      FMT_PAD_RIGHT);

    if (options & FT_OPT_PERCENT) {

      if (options & FT_OPT_WIDE)
        printf("%-16.16s  %-16.16s  %-6.3f   %-6.3f   %-6.3f   %-6.3f\n",
          fmt_buf1, fmt_buf2,
          ((double)ftch_recas2->nflows/(double)total->flows)*100,
          ((double)ftch_recas2->noctets/(double)total->octets)*100,
          ((double)ftch_recas2->npackets/(double)total->packets)*100,
          ((double)ftch_recas2->etime/(double)total->duration)*100);
      else
        printf("%-16.16s  %-16.16s  %-6.3f   %-6.3f   %-6.3f\n",
          fmt_buf1, fmt_buf2, 
          ((double)ftch_recas2->nflows/(double)total->flows)*100,
          ((double)ftch_recas2->noctets/(double)total->octets)*100,
          ((double)ftch_recas2->npackets/(double)total->packets)*100);

    } else {
      
      len = fmt_uint64(fmt_buf, ftch_recas2->nflows, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';
    
      len += fmt_uint64(fmt_buf+len, ftch_recas2->noctets, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      len += fmt_uint64(fmt_buf+len, ftch_recas2->npackets, FMT_PAD_RIGHT);

      if (options & FT_OPT_WIDE) {
        fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';
        len += fmt_uint64(fmt_buf+len, ftch_recas2->etime, FMT_JUST_LEFT);
      }

      printf("%-16.16s  %-16.16s  %s\n", fmt_buf1, fmt_buf2, fmt_buf);


    }

  }

  ftsym_free(ftsym);

  return 0;

} /* chash_as2_dump */

int chash_if2_dump (struct ftchash *ftch, char cc, int sort_order, int options,
  struct fopd *total)
{
  struct ftchash_rec_if2 *ftch_recif2;
  char fmt_buf[256];
  int len, sort_flags, sort_offset;

  if (sort_order) {

    if (sort_order < 0)
      sort_flags = 0;
    else
      sort_flags = FT_CHASH_SORT_ASCENDING;

    if ((sort_order == 1) || (sort_order == -1)) {
      sort_offset = offsetof(struct ftchash_rec_if2, input);
      sort_flags |= FT_CHASH_SORT_16;
    } else if ((sort_order == 2) || (sort_order == -2)) {
      sort_offset = offsetof(struct ftchash_rec_if2, output);
      sort_flags |= FT_CHASH_SORT_16;
    } else if ((sort_order == 3) || (sort_order == -3)) {
      sort_offset = offsetof(struct ftchash_rec_if2, nflows);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 4) || (sort_order == -4)) {
      sort_offset = offsetof(struct ftchash_rec_if2, noctets);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 5) || (sort_order == -5)) {
      sort_offset = offsetof(struct ftchash_rec_if2, npackets);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 6) || (sort_order == -6)) {
      sort_offset = offsetof(struct ftchash_rec_if2, etime);
      sort_flags |= FT_CHASH_SORT_64;
    } else {
      fprintf(stderr, "%c can't sort on field %d.\n", cc, sort_order);
      return -1;
    }

    ftchash_sort(ftch, sort_offset, sort_flags);

  } /* sorted? */

  if (options & FT_OPT_PERCENT) {
    if (options & FT_OPT_WIDE)
      printf("%c\n%c in   out    flows    octets   packets  duration\n%c\n", cc, cc, cc);
    else
      printf("%c\n%c in   out    flows    octets   packets\n%c\n", cc, cc, cc);
  } else {
    if (options & FT_OPT_WIDE)
      printf("%c\n%c in   out    flows                 octets                packets               duration\n%c\n", cc, cc, cc);
    else
      printf("%c\n%c in   out    flows                 octets                packets\n%c\n", cc, cc, cc);
  }

  ftchash_first(ftch);

  while ((ftch_recif2 = ftchash_foreach(ftch))) {

    if (options & FT_OPT_PERCENT) {

      len = fmt_uint16(fmt_buf, ftch_recif2->input, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';
    
      len += fmt_uint16(fmt_buf+len, ftch_recif2->output, FMT_PAD_RIGHT);

      if (options & FT_OPT_WIDE)
        printf("%s  %-6.3f   %-6.3f   %-6.3f   %-6.3f\n", fmt_buf,
          ((double)ftch_recif2->nflows/(double)total->flows)*100,
          ((double)ftch_recif2->noctets/(double)total->octets)*100,
          ((double)ftch_recif2->npackets/(double)total->packets)*100,
          ((double)ftch_recif2->etime/(double)total->duration)*100);
      else
        printf("%s  %-6.3f   %-6.3f   %-6.3f\n", fmt_buf,
          ((double)ftch_recif2->nflows/(double)total->flows)*100,
          ((double)ftch_recif2->noctets/(double)total->octets)*100,
          ((double)ftch_recif2->npackets/(double)total->packets)*100);

    } else {
      
      len = fmt_uint16(fmt_buf, ftch_recif2->input, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';
    
      len += fmt_uint16(fmt_buf+len, ftch_recif2->output, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      len += fmt_uint64(fmt_buf+len, ftch_recif2->nflows, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';
    
      len += fmt_uint64(fmt_buf+len, ftch_recif2->noctets, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      len += fmt_uint64(fmt_buf+len, ftch_recif2->npackets, FMT_PAD_RIGHT);

      if (options & FT_OPT_WIDE) {
        fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';
        len += fmt_uint64(fmt_buf+len, ftch_recif2->etime, FMT_JUST_LEFT);
      }

      puts(fmt_buf);

    }

  }

  return 0;

} /* chash_if2_dump */

int chash_pre_dump (struct ftchash *ftch, char cc, int sort_order, int options,
  struct fopd *total)
{
  struct ftchash_rec_prefix *ftch_recpre;
  char fmt_buf[256];
  int len, sort_flags, sort_offset;

  if (sort_order) {

    if (sort_order < 0)
      sort_flags = 0;
    else
      sort_flags = FT_CHASH_SORT_ASCENDING;

    if ((sort_order == 1) || (sort_order == -1)) {
      sort_offset = offsetof(struct ftchash_rec_prefix, prefix);
      sort_flags |= FT_CHASH_SORT_32;
    } else if ((sort_order == 2) || (sort_order == -2)) {
      sort_offset = offsetof(struct ftchash_rec_prefix, nflows);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 3) || (sort_order == -3)) {
      sort_offset = offsetof(struct ftchash_rec_prefix, noctets);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 4) || (sort_order == -4)) {
      sort_offset = offsetof(struct ftchash_rec_prefix, npackets);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 5) || (sort_order == -5)) {
      sort_offset = offsetof(struct ftchash_rec_prefix, etime);
      sort_flags |= FT_CHASH_SORT_64;
    } else {
      fprintf(stderr, "%c can't sort on field %d.\n", cc, sort_order);
      return -1;
    }

    ftchash_sort(ftch, sort_offset, sort_flags);

  } /* sorted? */

  if (options & FT_OPT_PERCENT) {
    if (options & FT_OPT_WIDE)
      printf("%c\n%c Source Prefix     flows    octets   packets  duration\n%c\n", cc, cc, cc);
    else
      printf("%c\n%c Source Prefix     flows    octets   packets\n%c\n", cc, cc, cc);
  } else {
    if (options & FT_OPT_WIDE)
      printf("%c\n%c Source Prefix     flows                 octets                packets               duration\n%c\n", cc, cc, cc);
    else
      printf("%c\n%c Source Prefix     flows                 octets                packets\n%c\n", cc, cc, cc);
  }

  ftchash_first(ftch);

  while ((ftch_recpre = ftchash_foreach(ftch))) {

    if (options & FT_OPT_PERCENT) {
  
      fmt_ipv4prefix(fmt_buf, ftch_recpre->prefix, ftch_recpre->mask,
        FMT_PAD_RIGHT);

      if (options & FT_OPT_WIDE)

        printf("%-18.18s  %-6.3f   %-6.3f   %-6.3f   %-6.3f\n", fmt_buf,
          ((double)ftch_recpre->nflows/(double)total->flows)*100,
          ((double)ftch_recpre->noctets/(double)total->octets)*100,
          ((double)ftch_recpre->npackets/(double)total->packets)*100,
          ((double)ftch_recpre->etime/(double)total->duration)*100);
      else

        printf("%-18.18s  %-6.3f   %-6.3f   %-6.3f\n", fmt_buf,
          ((double)ftch_recpre->nflows/(double)total->flows)*100,
          ((double)ftch_recpre->noctets/(double)total->octets)*100,
          ((double)ftch_recpre->npackets/(double)total->packets)*100);

    } else {
  
      len = fmt_ipv4prefix(fmt_buf, ftch_recpre->prefix, ftch_recpre->mask,
        FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      len += fmt_uint64(fmt_buf+len, ftch_recpre->nflows, FMT_PAD_RIGHT);
       fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      len += fmt_uint64(fmt_buf+len, ftch_recpre->noctets, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      if (options & FT_OPT_WIDE) {
        len += fmt_uint64(fmt_buf+len, ftch_recpre->npackets, FMT_PAD_RIGHT);
        fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';
        len += fmt_uint64(fmt_buf+len, ftch_recpre->etime, FMT_JUST_LEFT);
      } else
        fmt_uint64(fmt_buf+len, ftch_recpre->npackets, FMT_JUST_LEFT);

      puts(fmt_buf);

    }
  }

  return 0;

} /* chash_pre_dump */

int chash_pre2_dump (struct ftchash *ftch, char cc, int sort_order, int options,
  struct fopd *total)
{
  struct ftchash_rec_prefix2 *ftch_recpre2;
  char fmt_buf[256];
  int len, sort_flags, sort_offset;

  if (sort_order) {

    if (sort_order < 0)
      sort_flags = 0;
    else
      sort_flags = FT_CHASH_SORT_ASCENDING;

    if ((sort_order == 1) || (sort_order == -1)) {
      sort_offset = offsetof(struct ftchash_rec_prefix2, src_prefix);
      sort_flags |= FT_CHASH_SORT_32;
    } else if ((sort_order == 2) || (sort_order == -2)) {
      sort_offset = offsetof(struct ftchash_rec_prefix2, dst_prefix);
      sort_flags |= FT_CHASH_SORT_32;
    } else if ((sort_order == 3) || (sort_order == -3)) {
      sort_offset = offsetof(struct ftchash_rec_prefix2, nflows);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 4) || (sort_order == -4)) {
      sort_offset = offsetof(struct ftchash_rec_prefix2, noctets);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 5) || (sort_order == -5)) {
      sort_offset = offsetof(struct ftchash_rec_prefix2, npackets);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 6) || (sort_order == -6)) {
      sort_offset = offsetof(struct ftchash_rec_prefix2, etime);
      sort_flags |= FT_CHASH_SORT_64;
    } else {
      fprintf(stderr, "%c can't sort on field %d.\n", cc, sort_order);
      return -1;
    }

    ftchash_sort(ftch, sort_offset, sort_flags);

  } /* sorted? */

  if (options & FT_OPT_PERCENT) {
    if (options & FT_OPT_WIDE)
      printf("%c\n%c Source Prefix     Destination Prefix  flows    octets   packets  duration\n%c\n", cc, cc, cc);
    else
      printf("%c\n%c Source Prefix     Destination Prefix  flows    octets   packets\n%c\n", cc, cc, cc);
  } else {
    if (options & FT_OPT_WIDE)
      printf("%c\n%c Source Prefix     Destination Prefix  flows                 octets                packets               duration\n%c\n", cc, cc, cc);
    else
      printf("%c\n%c Source Prefix     Destination Prefix  flows                 octets                packets\n%c\n", cc, cc, cc);
  }

  ftchash_first(ftch);

  while ((ftch_recpre2 = ftchash_foreach(ftch))) {

    if (options & FT_OPT_PERCENT) {
  
      len = fmt_ipv4prefix(fmt_buf, ftch_recpre2->src_prefix,
        ftch_recpre2->src_mask, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      fmt_ipv4prefix(fmt_buf+len, ftch_recpre2->dst_prefix,
        ftch_recpre2->dst_mask, FMT_PAD_RIGHT);

      if (options & FT_OPT_WIDE)

        printf("%-38.38s  %-6.3f   %-6.3f   %-6.3f   %-6.3f\n", fmt_buf,
          ((double)ftch_recpre2->nflows/(double)total->flows)*100,
          ((double)ftch_recpre2->noctets/(double)total->octets)*100,
          ((double)ftch_recpre2->npackets/(double)total->packets)*100,
          ((double)ftch_recpre2->etime/(double)total->duration)*100);
      else

        printf("%-38.38s  %-6.3f   %-6.3f   %-6.3f\n", fmt_buf,
          ((double)ftch_recpre2->nflows/(double)total->flows)*100,
          ((double)ftch_recpre2->noctets/(double)total->octets)*100,
          ((double)ftch_recpre2->npackets/(double)total->packets)*100);

    } else {
  
      len = fmt_ipv4prefix(fmt_buf, ftch_recpre2->src_prefix,
        ftch_recpre2->src_mask, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      len += fmt_ipv4prefix(fmt_buf+len, ftch_recpre2->dst_prefix,
        ftch_recpre2->dst_mask, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      len += fmt_uint64(fmt_buf+len, ftch_recpre2->nflows, FMT_PAD_RIGHT);
       fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      len += fmt_uint64(fmt_buf+len, ftch_recpre2->noctets, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      if (options & FT_OPT_WIDE) {
        len += fmt_uint64(fmt_buf+len, ftch_recpre2->npackets, FMT_PAD_RIGHT);
        fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';
        len += fmt_uint64(fmt_buf+len, ftch_recpre2->etime, FMT_JUST_LEFT);
      } else
        fmt_uint64(fmt_buf+len, ftch_recpre2->npackets, FMT_JUST_LEFT);

      puts(fmt_buf);

    }
  }

  return 0;

} /* chash_pre2_dump */

/*
 * function fopdi_free()
 *
 * free resources allocated to struct fopdi
 *
 */
void fopdi_free(struct fopdi *fopdi)
{
  if (fopdi->flows)
    free(fopdi->flows);
  if (fopdi->octets)
    free(fopdi->octets);
  if (fopdi->packets)
    free(fopdi->packets);
  if (fopdi->duration)
    free(fopdi->duration);
} /* fopdi_free */

/*
 * function fopdi_alloc
 *
 * alloc n entries for each member
 * 
 * returns 0 for success
*/
int fopdi_alloc(struct fopdi *fopdi, int n)
{
  register int i;

  if (!(fopdi->flows = (u_int64*)malloc(n*sizeof(u_int64)))) {
    fterr_warn("malloc(fopdi):");
    return -1;
  }

  if (!(fopdi->octets = (u_int64*)malloc(n*sizeof(u_int64)))) {
    fterr_warn("malloc(fopdi):");
    free(fopdi->flows);
    return -1;
  }

  if (!(fopdi->packets = (u_int64*)malloc(n*sizeof(u_int64)))) {
    fterr_warn("malloc(fopdi):");
    free(fopdi->flows);
    free(fopdi->octets);
    return -1;
  }

  if (!(fopdi->duration = (u_int64*)malloc(n*sizeof(u_int64)))) {
    fterr_warn("malloc(fopdi):");
    free(fopdi->flows);
    free(fopdi->octets);
    free(fopdi->packets);
    return -1;
  }

  if (!(fopdi->index = (u_int32*)malloc(n*sizeof(u_int32)))) {
    fterr_warn("malloc(fopdi):");
    free(fopdi->flows);
    free(fopdi->octets);
    free(fopdi->duration);
    return -1;
  }

  bzero(fopdi->flows, n*sizeof(u_int64));
  bzero(fopdi->octets, n*sizeof(u_int64));
  bzero(fopdi->packets, n*sizeof(u_int64));
  bzero(fopdi->duration, n*sizeof(u_int64));

  for (i = 0; i < n; ++i)
    fopdi->index[i] = i;

  return 0;

} /* fopdi_alloc */

int tbl_out1(struct fmtargs *args, u_int nindex, struct fopdi *stat,
  struct fopd *total, char *title, char *symfile)
{
  struct ftsym *ftsym;
  struct fopd total2;
  char fmt_buf[256], fmt_buf2[64];
  int32 i, start, end, increment, x;
  int s, len;
  u_int plines;
  u_int32 *index;

  s = abs(args->sort_order);
  x = 0;
  bzero(&total2, sizeof total2);
  index = stat->index;
  ftsym = (struct ftsym*)0L;

  if (s == 0) /* no sorting */
    ;
  else if (s == 1) /* port */
    ;
  else if (s == 2) { /* flows */
    sort_i64 = stat->flows;
    qsort(stat->index, nindex, sizeof (u_int32), sort_cmp64);
  } else if (s == 3) { /* octets */
    sort_i64 = stat->octets;
    qsort(stat->index, nindex, sizeof (u_int32), sort_cmp64);
  } else if (s == 4) { /* packets */
    sort_i64 = stat->packets;
    qsort(stat->index, nindex, sizeof (u_int32), sort_cmp64);
  } else if (s == 5) { /* duration */
    sort_i64 = stat->duration;
    qsort(stat->index, nindex, sizeof (u_int32), sort_cmp64);
  } else {
    fprintf(stderr, "%c can't sort on field %d.\n", args->cc, args->sort_order);
    return -1;
  }

  if (args->sort_order >= 0)
    start = nindex - 1, end = -1, increment = -1;
  else
    start = 0, end = nindex, increment = 1;

  /* load symbol table */
  if (args->options & FT_OPT_NAMES)
    ftsym = ftsym_new(symfile);

  /* header */
  if (args->options & FT_OPT_PERCENT) {
    if (args->options & FT_OPT_WIDE)
      printf("%c\n%c %-15.15sflows    octets   packets  duration\n%c\n",
        args->cc, args->cc, title, args->cc);
    else
      printf("%c\n%c %-10.10sflows    octets   packets\n%c\n",
        args->cc, args->cc, title, args->cc);
  } else {
    if (args->options & FT_OPT_WIDE)
      printf("%c\n%c %-15.15sflows                 octets                packets               duration\n%c\n", args->cc, args->cc, title, args->cc);
    else
      printf("%c\n%c %-10.10sflows                 octets                packets\n%c\n", args->cc, args->cc, title, args->cc);
  }

  /*
   * if FT_OPT_PERCENT and we're doing tallying, then the # of lines
   * that will be output needs to be known first
   */
  for (i = start; i != end; i += increment)
    if (stat->flows[index[i]])
      ++plines;

  for (i = start; i != end; i += increment) {

    if (stat->flows[index[i]]) {

      /* print a tally line ? */
      if ( (args->options & FT_OPT_TALLY) && x && (!(x % args->tally))) {

        if (args->options & FT_OPT_PERCENT) {

          printf("%cSUB %-6.3f  %-6.3f   %-6.3f   %-6.3f   %-6.3f\n", 
            args->cc, ((double)x/(double)plines)*100,
            ((double)total2.flows/(double)total->flows)*100,
            ((double)total2.octets/(double)total->octets)*100,
            ((double)total2.packets/(double)total->packets)*100,
            ((double)total2.duration/(double)total->duration)*100);
        } /* options & FT_OPT_PERCENT */
      } /* print tally */

      fmt_uint16s(ftsym, 16, fmt_buf2, (u_int16)stat->index[i], FMT_JUST_LEFT);

      if (args->options & FT_OPT_PERCENT) {


        if (args->options & FT_OPT_WIDE)
          printf("%-15.15s  %-6.3f   %-6.3f   %-6.3f   %-6.3f\n", 
            fmt_buf2,
            ((double)stat->flows[index[i]]/(double)total->flows)*100,
            ((double)stat->octets[index[i]]/(double)total->octets)*100,
            ((double)stat->packets[index[i]]/(double)total->packets)*100,
            ((double)stat->duration[index[i]]/(double)total->duration)*100);
        else
          printf("%-10.10s  %-6.3f   %-6.3f   %-6.3f\n", 
            fmt_buf2,
            ((double)stat->flows[index[i]]/(double)total->flows)*100,
            ((double)stat->octets[index[i]]/(double)total->octets)*100,
            ((double)stat->packets[index[i]]/(double)total->packets)*100);

      } else {

        len = fmt_uint64(fmt_buf, stat->flows[index[i]], FMT_PAD_RIGHT);
        fmt_buf[len++] = ' '; fmt_buf[len++] = ' '; 

        len += fmt_uint64(fmt_buf+len, stat->octets[index[i]], FMT_PAD_RIGHT);
        fmt_buf[len++] = ' '; fmt_buf[len++] = ' '; 

        len += fmt_uint64(fmt_buf+len, stat->packets[index[i]],
          FMT_PAD_RIGHT);

        if (args->options & FT_OPT_WIDE) {

          fmt_buf[len++] = ' '; fmt_buf[len++] = ' '; 
          len += fmt_uint64(fmt_buf+len, stat->duration[index[i]],
            FMT_JUST_LEFT);

          printf("%-15.15s  %s\n", fmt_buf2, fmt_buf);

        } else {

          printf("%-10.10s  %s\n", fmt_buf2, fmt_buf);

        }

      }

      total2.flows += stat->flows[index[i]];
      total2.octets += stat->octets[index[i]];
      total2.packets += stat->packets[index[i]];
      total2.duration += stat->duration[index[i]];
      ++x;

    } /* stat->flows[index[i]] */
  }

  ftsym_free(ftsym);

  return 0;

} /* tbl_out1 */

int chash_c32_dump (struct ftchash *ftch, char cc, int sort_order, int options,
  struct fopd *total, char *title, char *symfile)
{
  struct ftsym *ftsym;
  struct ftchash_rec_c32 *ftch_recc32;
  char fmt_buf[256], fmt_buf2[64], *fmt_bufp;
  int len, sort_flags, sort_offset;

  ftsym = (struct ftsym*)0L;

  if (sort_order) {

    if (sort_order < 0)
      sort_flags = 0;
    else
      sort_flags = FT_CHASH_SORT_ASCENDING;

    if ((sort_order == 1) || (sort_order == -1)) {
      sort_offset = offsetof(struct ftchash_rec_c32, c32);
      sort_flags |= FT_CHASH_SORT_32;
    } else if ((sort_order == 2) || (sort_order == -2)) {
      sort_offset = offsetof(struct ftchash_rec_c32, nflows);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 3) || (sort_order == -3)) {
      sort_offset = offsetof(struct ftchash_rec_c32, noctets);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 4) || (sort_order == -4)) {
      sort_offset = offsetof(struct ftchash_rec_c32, npackets);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 5) || (sort_order == -5)) {
      sort_offset = offsetof(struct ftchash_rec_c32, etime);
      sort_flags |= FT_CHASH_SORT_64;
    } else {
      fprintf(stderr, "%c can't sort on field %d.\n", cc, sort_order);
      return -1;
    }

    ftchash_sort(ftch, sort_offset, sort_flags);

  } /* sorted? */

  if (options & FT_OPT_PERCENT) {
    if (options & FT_OPT_WIDE)
      printf("%c\n%c %s  flows    octets   packets  duration\n%c\n", cc, cc, 
        title, cc);
    else
      printf("%c\n%c %s  flows    octets   packets\n%c\n", cc, cc, title, cc);
  } else {
    if (options & FT_OPT_WIDE)
      printf("%c\n%c %s  flows                 octets                packets               duration\n%c\n", cc, cc, title, cc);
    else
      printf("%c\n%c %s  flows                 octets                packets\n%c\n", cc, cc, title, cc);
  }

  ftchash_first(ftch);

  if (options & FT_OPT_NAMES) {
    ftsym = ftsym_new(FT_PATH_SYM_TAG);
  }

  while ((ftch_recc32 = ftchash_foreach(ftch))) {

    if (options & FT_OPT_PERCENT) {
 
      if (options & FT_OPT_NAMES) {
        if (ftsym_findbyval(ftsym, ftch_recc32->c32, &fmt_bufp) != 1) {
          fmt_uint32(fmt_buf, ftch_recc32->c32, FMT_PAD_RIGHT);
          fmt_bufp = fmt_buf;
        }
      } else {
        fmt_uint32(fmt_buf, ftch_recc32->c32, FMT_PAD_RIGHT);
        fmt_bufp = fmt_buf;
      }

      if (options & FT_OPT_WIDE)

        printf("%-10.10s  %-6.3f   %-6.3f   %-6.3f   %-6.3f\n", fmt_bufp,
          ((double)ftch_recc32->nflows/(double)total->flows)*100,
          ((double)ftch_recc32->noctets/(double)total->octets)*100,
          ((double)ftch_recc32->npackets/(double)total->packets)*100,
          ((double)ftch_recc32->etime/(double)total->duration)*100);
      else

        printf("%-10.10s  %-6.3f   %-6.3f   %-6.3f\n", fmt_bufp,
          ((double)ftch_recc32->nflows/(double)total->flows)*100,
          ((double)ftch_recc32->noctets/(double)total->octets)*100,
          ((double)ftch_recc32->npackets/(double)total->packets)*100);

    } else {

      if (options & FT_OPT_NAMES) {
        if (ftsym_findbyval(ftsym, ftch_recc32->c32, &fmt_bufp) != 1) {
          fmt_uint32(fmt_buf2, ftch_recc32->c32, FMT_PAD_RIGHT);
          fmt_bufp = fmt_buf2;
        }
      } else {
        fmt_uint32(fmt_buf2, ftch_recc32->c32, FMT_PAD_RIGHT);
        fmt_bufp = fmt_buf2;
      }

      len = fmt_uint64(fmt_buf, ftch_recc32->nflows, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      len += fmt_uint64(fmt_buf+len, ftch_recc32->noctets, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      if (options & FT_OPT_WIDE) {
        len += fmt_uint64(fmt_buf+len, ftch_recc32->npackets, FMT_PAD_RIGHT);
        fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';
        len += fmt_uint64(fmt_buf+len, ftch_recc32->etime, FMT_JUST_LEFT);
        printf("%-15.15s  %s\n", fmt_bufp, fmt_buf);
      } else {
        fmt_uint64(fmt_buf+len, ftch_recc32->npackets, FMT_JUST_LEFT);
        printf("%-10.10s  %s\n", fmt_bufp, fmt_buf);
      } 

    }
  }

  ftsym_free(ftsym);

  return 0;

} /* chash_c32_dump */


int chash_c322_dump (struct ftchash *ftch, char cc, int sort_order, int options,
  struct fopd *total, char *title, char *title2, char *symfile)
{
  struct ftchash_rec_c322 *ftch_recc322;
  struct ftsym *ftsym;
  char fmt_buf[256], fmt_buf2[256], *fmt_buf_a, *fmt_buf_b;
  int len, sort_flags, sort_offset;

  ftsym = (struct ftsym*)0L;

  if (sort_order) {

    if (sort_order < 0)
      sort_flags = 0;
    else
      sort_flags = FT_CHASH_SORT_ASCENDING;

    if ((sort_order == 1) || (sort_order == -1)) {
      sort_offset = offsetof(struct ftchash_rec_c322, c32a);
      sort_flags |= FT_CHASH_SORT_32;
    } else if ((sort_order == 2) || (sort_order == -2)) {
      sort_offset = offsetof(struct ftchash_rec_c322, c32b);
      sort_flags |= FT_CHASH_SORT_32;
    } else if ((sort_order == 3) || (sort_order == -3)) {
      sort_offset = offsetof(struct ftchash_rec_c322, nflows);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 4) || (sort_order == -4)) {
      sort_offset = offsetof(struct ftchash_rec_c322, noctets);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 5) || (sort_order == -5)) {
      sort_offset = offsetof(struct ftchash_rec_c322, npackets);
      sort_flags |= FT_CHASH_SORT_64;
    } else if ((sort_order == 6) || (sort_order == -6)) {
      sort_offset = offsetof(struct ftchash_rec_c322, etime);
      sort_flags |= FT_CHASH_SORT_64;
    } else {
      fprintf(stderr, "%c can't sort on field %d.\n", cc, sort_order);
      return -1;
    }

    ftchash_sort(ftch, sort_offset, sort_flags);

  } /* sorted? */

  if (options & FT_OPT_PERCENT) {
    if (options & FT_OPT_WIDE)
      printf("%c\n%c %s  %s  flows    octets   packets  duration\n%c\n",
        cc, cc, title, title2, cc);
    else
      printf("%c\n%c %s  %s  flows    octets   packets\n%c\n",
      cc, cc, title, title2, cc);
  } else {
    if (options & FT_OPT_WIDE)
      printf("%c\n%c %s  %s  flows                 octets                packets               duration\n%c\n", cc, cc, title, title2, cc);
    else
      printf("%c\n%c %s  %s  flows                 octets                packets\n%c\n", cc, cc, title, title2, cc);
  }

  ftchash_first(ftch);

  if (options & FT_OPT_NAMES) {
    ftsym = ftsym_new(FT_PATH_SYM_TAG);
  }

  while ((ftch_recc322 = ftchash_foreach(ftch))) {

    if (options & FT_OPT_NAMES) {

      if (ftsym_findbyval(ftsym, ftch_recc322->c32a, &fmt_buf_a) != 1) {
        fmt_uint32(fmt_buf, ftch_recc322->c32a, FMT_PAD_RIGHT);
        fmt_buf_a = fmt_buf;
      }

      if (ftsym_findbyval(ftsym, ftch_recc322->c32b, &fmt_buf_b) != 1) {
        fmt_uint32(fmt_buf2, ftch_recc322->c32b, FMT_PAD_RIGHT);
        fmt_buf_b = fmt_buf2;
      }

    } else {

      fmt_uint32(fmt_buf, ftch_recc322->c32a, FMT_PAD_RIGHT);
      fmt_buf_a = fmt_buf;

      fmt_uint32(fmt_buf2, ftch_recc322->c32b, FMT_PAD_RIGHT);
      fmt_buf_b = fmt_buf2;

    }

    printf("%-10.10s  %-10.10s  ", fmt_buf_a, fmt_buf_b);

    if (options & FT_OPT_PERCENT) {

      if (options & FT_OPT_WIDE)

        printf("%-6.3f   %-6.3f   %-6.3f   %-6.3f\n",
          ((double)ftch_recc322->nflows/(double)total->flows)*100,
          ((double)ftch_recc322->noctets/(double)total->octets)*100,
          ((double)ftch_recc322->npackets/(double)total->packets)*100,
          ((double)ftch_recc322->etime/(double)total->duration)*100);
      else

        printf("%-6.3f   %-6.3f   %-6.3f\n",
          ((double)ftch_recc322->nflows/(double)total->flows)*100,
          ((double)ftch_recc322->noctets/(double)total->octets)*100,
          ((double)ftch_recc322->npackets/(double)total->packets)*100);

    } else {
  
      len = fmt_uint64(fmt_buf, ftch_recc322->nflows, FMT_PAD_RIGHT);
       fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';

      len += fmt_uint64(fmt_buf+len, ftch_recc322->noctets, FMT_PAD_RIGHT);
      fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';


      if (options & FT_OPT_WIDE) {
        len += fmt_uint64(fmt_buf+len, ftch_recc322->npackets, FMT_PAD_RIGHT);
        fmt_buf[len++] = ' '; fmt_buf[len++] = ' ';
        len += fmt_uint64(fmt_buf+len, ftch_recc322->etime, FMT_JUST_LEFT);
      } else
        fmt_uint64(fmt_buf+len, ftch_recc322->npackets, FMT_JUST_LEFT);

      puts(fmt_buf);

    }
  }

  return 0;

} /* chash_c322_dump */

static int sort_cmp64(const void *a, const void *b)
{
  u_int32 l, r;

  l = *(u_int32*)a;
  r = *(u_int32*)b;

  if (sort_i64[l] < sort_i64[r])
    return -1;
  if (sort_i64[l] > sort_i64[r])
    return 1;
  return 0;

} /* sort_cmp64 */

void usage(void)
{
  static int first;
  int i;

  if (!first) {

    fprintf(stderr, "Usage: flow-stat [-hnpPw] [-d debug_level] [-f format] [-S sort_field]\n");
    fprintf(stderr, "       [-s sort_field] [-t tally_lines] [-T title]\n");
    fprintf(stderr, "\n%s version %s: built by %s\n", PACKAGE, VERSION, FT_PROG_BUILD);

    ++first;

  } else {

    fprintf(stderr, "Formats:\n");
    for (i = 0; i < FORMATS; ++i)
      if (format_name[i][0])
        fprintf(stderr, "  %-2d %s\n", i, format_name[i]);

   }

} /* usage */

