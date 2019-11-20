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
 *      $Id: flow-print.c,v 1.29 2003/04/02 18:03:02 maf Exp $
 */

#include "ftconfig.h"
#include <ftlib.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <netinet/in.h>
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

struct jump {
    int (*where)(struct ftio *ftio, int options);
};

int format0(struct ftio *ftio, int options);
int format1(struct ftio *ftio, int options);
int format2(struct ftio *ftio, int options);
int format3(struct ftio *ftio, int options);
int format4(struct ftio *ftio, int options);
int format5(struct ftio *ftio, int options);
int format6(struct ftio *ftio, int options);
int format7(struct ftio *ftio, int options);
int format8(struct ftio *ftio, int options);
int format9(struct ftio *ftio, int options);
int format10(struct ftio *ftio, int options);
int format11(struct ftio *ftio, int options);
int format12(struct ftio *ftio, int options);
int format13(struct ftio *ftio, int options);
int format14(struct ftio *ftio, int options);
int format15(struct ftio *ftio, int options);
int format16(struct ftio *ftio, int options);
int format17(struct ftio *ftio, int options);
int format18(struct ftio *ftio, int options);
int format19(struct ftio *ftio, int options);
int format20(struct ftio *ftio, int options);
int format21(struct ftio *ftio, int options);
int format22(struct ftio *ftio, int options);
int format23(struct ftio *ftio, int options);
int format24(struct ftio *ftio, int options);

struct jump format[] = {{format0}, {format1}, {format2},
          {format3}, {format4}, {format5}, {format6}, {format7},
          {format8}, {format9}, {format10}, {format11}, {format12},
          {format13}, {format14}, {format15}, {format16}, {format17},
          {format18}, {format19}, {format20}, {format21}, {format22},
          {format23}, {format24}};

#define NFORMATS 25

void usage(void);

int main(argc, argv)
int argc;
char **argv;
{
  struct ftio ftio;
  struct ftprof ftp;
  int i, format_index, set_format, ret;
  int print_header, options, debug;
  char cc; /* comment character */

  /* init fterr */
  fterr_setid(argv[0]);

  options = 0;
  debug = 0;

  /* profile */
  ftprof_start (&ftp);

  set_format = 0;
  print_header = 0;
  cc = '#';

  while ((i = getopt(argc, argv, "ph?d:f:c:lnw")) != -1)
    switch (i) {

    case 'c': /* comment character */
      cc = optarg[0];
      break;

    case 'd': /* debug */
      debug = atoi(optarg);
      break;

    case 'f': /* format */
      format_index = atoi(optarg);
      set_format = 1;
      break;

    case 'h': /* help */
    case '?':
      usage();
      exit (0);
      break;

    case 'l': /* turn off buffered output */
      options |= FT_OPT_NOBUF;
      break;

    case 'n': /* symbolic names */
      options |= FT_OPT_NAMES;
      break;

    case 'p': /* print header */
      print_header = 1;
      break;

    case 'w': /* wide */
      options |= FT_OPT_WIDE;
      break;

    default:
      usage();
      exit (1);
      break;

    } /* switch */

  if (argc - optind)
    fterr_errx(1, "Extra arguments starting with %s.", argv[optind]);

  if (set_format && (format_index >= NFORMATS)) 
    fterr_errx(1, "No such format, %d\n", format_index);

  /* read from stdin */
  if (ftio_init(&ftio, 0, FT_IO_FLAG_READ) < 0)
    fterr_errx(1, "ftio_init(): failed");

  /* if the format was not set on the command line use a reasonable default */
  if (!set_format) {
    if (ftio.fth.d_version == 8) {
      if (ftio.fth.agg_method == 1)
        format_index = 10;
      else if (ftio.fth.agg_method == 2)
        format_index = 11;
      else if (ftio.fth.agg_method == 3)
        format_index = 12;
      else if (ftio.fth.agg_method == 4)
        format_index = 13;
      else if (ftio.fth.agg_method == 5)
        format_index = 14;
      else if (ftio.fth.agg_method == 6)
        format_index = 15;
      else if (ftio.fth.agg_method == 7)
        format_index = 16;
      else if (ftio.fth.agg_method == 8)
        format_index = 17;
      else if (ftio.fth.agg_method == 9)
        format_index = 18;
      else if (ftio.fth.agg_method == 10)
        format_index = 19;
      else if (ftio.fth.agg_method == 11)
        format_index = 20;
      else if (ftio.fth.agg_method == 12)
        format_index = 21;
      else if (ftio.fth.agg_method == 13)
        format_index = 22;
      else if (ftio.fth.agg_method == 14)
        format_index = 23;
    } else if (ftio.fth.d_version == 7) {
      format_index = 7;
    } else if (ftio.fth.d_version == 6) {
      format_index = 8;
    } else if (ftio.fth.d_version == 1005) {
      format_index = 9;
    } else
      format_index = 3;
  } /* !set_format */

  if (print_header) {
    ftio_header_print(&ftio, stdout, cc);
  }

  ret = format[format_index].where(&ftio, options);

  if ((!ret) && (debug > 0)) {
    ftprof_end(&ftp, ftio_get_rec_total(&ftio));
    ftprof_print(&ftp, argv[0], stderr);
  }

  return ret;

} /* main */


/*
 * function: format0
 *
 * 1 line summary
 */
int format0(struct ftio *ftio, int options)
{
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[64], fmt_buf2[64];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_INPUT | FT_XFIELD_OUTPUT | FT_XFIELD_PROT |
    FT_XFIELD_SRCADDR | FT_XFIELD_DSTADDR | FT_XFIELD_INPUT |
    FT_XFIELD_SRCPORT | FT_XFIELD_DSTPORT | FT_XFIELD_OUTPUT)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }
 
  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  puts("Sif  SrcIPaddress     Dif  DstIPaddress      Pr SrcP DstP  Pkts       Octets");

  while ((rec = ftio_read(ftio))) {

    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.input = ((u_int16*)(rec+fo.input));
    cur.output = ((u_int16*)(rec+fo.output));
    cur.srcport = ((u_int16*)(rec+fo.srcport));
    cur.dstport = ((u_int16*)(rec+fo.dstport));
    cur.prot = ((u_int8*)(rec+fo.prot));

    fmt_ipv4(fmt_buf1, *cur.srcaddr, FMT_PAD_RIGHT);
    fmt_ipv4(fmt_buf2, *cur.dstaddr, FMT_PAD_RIGHT);

    printf("%4.4x %-15.15s  %4.4x %-15.15s   %2.2x %-4x %-4x  %-10lu %-10lu\n",
      (int)*cur.input, fmt_buf1, (int)*cur.output, fmt_buf2,
      (int)*cur.prot, (int)*cur.srcport, (int)*cur.dstport,
      (u_long)*cur.dPkts, (u_long)*cur.dOctets);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  return 0;

} /* format0 */


/*
 * function: format1
 *
 * 2 line summary
 */
int format1(struct ftio *ftio, int options)
{
  struct fttime ftt;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  struct tm *tm;
  u_long active_secs, active_msecs;
  u_long bpp;
  char fmt_buf1[64], fmt_buf2[64];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_INPUT |
    FT_XFIELD_OUTPUT | FT_XFIELD_SRCADDR | FT_XFIELD_DSTADDR |
    FT_XFIELD_SRCPORT | FT_XFIELD_DSTPORT | 
    FT_XFIELD_UNIX_SECS | FT_XFIELD_UNIX_NSECS | FT_XFIELD_SYSUPTIME |
    FT_XFIELD_TOS | FT_XFIELD_TCP_FLAGS | FT_XFIELD_PROT)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  puts(
    "Sif  SrcIPaddress     DIf  DstIPaddress      Pr SrcP DstP  Pkts  Octets");
  puts(
    " StartTime          EndTime             Active   B/Pk Ts Fl\n");

  while ((rec = ftio_read(ftio))) {

    cur.unix_secs = ((u_int32*)(rec+fo.unix_secs));
    cur.unix_nsecs = ((u_int32*)(rec+fo.unix_nsecs));
    cur.sysUpTime = ((u_int32*)(rec+fo.sysUpTime));
    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.First = ((u_int32*)(rec+fo.First));
    cur.Last = ((u_int32*)(rec+fo.Last));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.input = ((u_int16*)(rec+fo.input));
    cur.output = ((u_int16*)(rec+fo.output));
    cur.srcport = ((u_int16*)(rec+fo.srcport));
    cur.dstport = ((u_int16*)(rec+fo.dstport));
    cur.prot = ((u_int8*)(rec+fo.prot));
    cur.tcp_flags = ((u_int8*)(rec+fo.tcp_flags));
    cur.tos = ((u_int8*)(rec+fo.tos));

    if (!*cur.dPkts) {
      fprintf(stderr, "Ignoring bogus flow dPkts=0\n");
      continue;
    }

    fmt_ipv4(fmt_buf1, *cur.srcaddr, FMT_PAD_RIGHT);
    fmt_ipv4(fmt_buf2, *cur.dstaddr, FMT_PAD_RIGHT);

    printf("%4.4x %-15.15s  %4.4x %-15.15s   %2.2x %-4x %-4x  %-10lu %-10lu\n",
      (int)*cur.input, fmt_buf1, (int)*cur.output, fmt_buf2,
      (int)*cur.prot, (int)*cur.srcport, (int)*cur.dstport,
      (u_long)*cur.dPkts, (u_long)*cur.dOctets);

    ftt = ftltime(*cur.sysUpTime, *cur.unix_secs, *cur.unix_nsecs, *cur.First);
    tm = localtime((time_t*)&ftt.secs);

    printf(" %-2.2d%-2.2d.%-2.2d:%-2.2d:%-2.2d.%-3.3lu  ",
      (int)tm->tm_mon+1, (int)tm->tm_mday, (int)tm->tm_hour,
      (int)tm->tm_min, (int)tm->tm_sec, (u_long)ftt.msecs);

    ftt = ftltime(*cur.sysUpTime, *cur.unix_secs, *cur.unix_nsecs, *cur.Last);
    tm = localtime((time_t*)&ftt.secs);

    active_secs = (*cur.Last - *cur.First) / 1000;
    active_msecs = (*cur.Last - *cur.First) % 1000;

    bpp = *cur.dOctets / *cur.dPkts;

    printf("%-2.2d%-2.2d.%-2.2d:%-2.2d:%-2.2d.%-3.3lu  %5lu.%-3.3lu %-3lu %2.2x %2.2x\n\n",
      (int)tm->tm_mon+1, (int)tm->tm_mday, (int)tm->tm_hour,
      (int)tm->tm_min, (int)tm->tm_sec, (u_long)ftt.msecs,
      active_secs, active_msecs, bpp, (int)*cur.tos,
      (int)*cur.tcp_flags);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  return 0;

} /* format1 */


/*
 * function: format2
 *
 * only print flows that are TCP with only a SYN bit set and
 * a single packet
 */
int format2(struct ftio *ftio, int options)
{
  struct tm *tm;
  struct fttime ftt;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[64], fmt_buf2[64];
  u_long active_secs, active_msecs;
  u_long bpp;
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_INPUT |
    FT_XFIELD_OUTPUT | FT_XFIELD_SRCADDR | FT_XFIELD_DSTADDR |
    FT_XFIELD_SRCPORT | FT_XFIELD_DSTPORT | 
    FT_XFIELD_UNIX_SECS | FT_XFIELD_UNIX_NSECS | FT_XFIELD_SYSUPTIME |
    FT_XFIELD_TOS | FT_XFIELD_TCP_FLAGS | FT_XFIELD_PROT)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  puts("Sif SrcIPaddress     DIf DstIPaddress    Pr SrcP DstP Pkts       Octets");
  puts(" StartTime          EndTime             Active   B/Pk Ts Fl\n");

  while ((rec = ftio_read(ftio))) {

    cur.unix_secs = ((u_int32*)(rec+fo.unix_secs));
    cur.unix_nsecs = ((u_int32*)(rec+fo.unix_nsecs));
    cur.sysUpTime = ((u_int32*)(rec+fo.sysUpTime));
    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.First = ((u_int32*)(rec+fo.First));
    cur.Last = ((u_int32*)(rec+fo.Last));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.input = ((u_int16*)(rec+fo.input));
    cur.output = ((u_int16*)(rec+fo.output));
    cur.srcport = ((u_int16*)(rec+fo.srcport));
    cur.dstport = ((u_int16*)(rec+fo.dstport));
    cur.prot = ((u_int8*)(rec+fo.prot));
    cur.tcp_flags = ((u_int8*)(rec+fo.tcp_flags));
    cur.tos = ((u_int8*)(rec+fo.tos));

    if (!*cur.dPkts) {
      fprintf(stderr, "Ignoring bogus flow dPkts=0\n");
      continue;
    }

    /* If it's not TCP */
    if (*cur.prot != IPPROTO_TCP)
      continue;

    /* If more than the SYN bit is set */
    if (*cur.tcp_flags != 2)
      continue;

    /* many SYN bit only packets per flow are suspect */
    if (*cur.dPkts != 2)
      continue;

    /* 40 byte datagrams are the output of the current tool */
    if (*cur.dOctets != (*cur.dPkts * 40))
      continue;

    fmt_ipv4(fmt_buf1, *cur.srcaddr, FMT_PAD_RIGHT);
    fmt_ipv4(fmt_buf2, *cur.dstaddr, FMT_PAD_RIGHT);

    printf("%2.2x  %-15.15s  %2.2x  %-15.15s %2.2x %-4x %-4x %-10lu %-10lu\n",
      (int)*cur.input, fmt_buf1, (int)*cur.output, fmt_buf2,
      (int)*cur.prot, (int)*cur.srcport, (int)*cur.dstport,
      (u_long)*cur.dPkts, (u_long)*cur.dOctets);

    ftt = ftltime(*cur.sysUpTime, *cur.unix_secs, *cur.unix_nsecs, *cur.First);
    tm = localtime((time_t*)&ftt.secs);

    printf(" %-2.2d%-2.2d.%-2.2d:%-2.2d:%-2.2d.%-3.3lu  ",
      (int)tm->tm_mon+1, (int)tm->tm_mday, (int)tm->tm_hour, (int)tm->tm_min,
      (int)tm->tm_sec, (u_long)ftt.msecs);

    ftt = ftltime(*cur.sysUpTime, *cur.unix_secs, *cur.unix_nsecs, *cur.Last);
    tm = localtime((time_t*)&ftt.secs);

    active_secs = (*cur.Last - *cur.First) / 1000;
    active_msecs = (*cur.Last - *cur.First) % 1000;

    bpp = *cur.dOctets / *cur.dPkts;

    printf("%-2.2d%-2.2d.%-2.2d:%-2.2d:%-2.2d.%-3.3lu  %5lu.%-3.3lu %-3lu %2.2x %2.2x\n\n",
      (int)tm->tm_mon+1, (int)tm->tm_mday, (int)tm->tm_hour,
      (int)tm->tm_min, (int)tm->tm_sec, (u_long)ftt.msecs, active_secs,
      active_msecs, bpp, (int)*cur.tos, (int)*cur.tcp_flags);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  return 0;

} /* format2 */


/*
 * function: format3
 *
 * another 1 line format
 */
int format3(struct ftio *ftio, int options)
{
  struct ftsym *sym_tcp, *sym_prot;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[64], fmt_buf2[64], fmt_buf3[64], fmt_buf4[64], fmt_buf5[64];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_SRCADDR | FT_XFIELD_DSTADDR |
    FT_XFIELD_SRCPORT | FT_XFIELD_DSTPORT | FT_XFIELD_PROT)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;   
  }

  ftio_get_ver(ftio, &ftv);
  
  fts3rec_compute_offsets(&fo, &ftv);

  sym_tcp = sym_prot = (struct ftsym*)0L;

  if (options & FT_OPT_NAMES) {
    sym_tcp = ftsym_new(FT_PATH_SYM_TCP_PORT);
    sym_prot = ftsym_new(FT_PATH_SYM_IP_PROT);
  }

  if (options & FT_OPT_WIDE)
    puts("srcIP            dstIP            prot   srcPort           dstPort           octets      packets");
  else
    puts("srcIP            dstIP            prot  srcPort  dstPort  octets      packets");

  while ((rec = ftio_read(ftio))) {

    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));   
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.srcport = ((u_int16*)(rec+fo.srcport));
    cur.dstport = ((u_int16*)(rec+fo.dstport));
    cur.prot = ((u_int8*)(rec+fo.prot));

    fmt_ipv4(fmt_buf1, *cur.srcaddr, FMT_PAD_RIGHT);

    fmt_ipv4(fmt_buf2, *cur.dstaddr, FMT_PAD_RIGHT);

    fmt_uint16s(sym_prot, 5, fmt_buf3, (u_int16)*cur.prot, FMT_PAD_RIGHT);

    fmt_uint16s(sym_tcp, 16, fmt_buf4, (u_int16)*cur.srcport, FMT_PAD_RIGHT);

    fmt_uint16s(sym_tcp, 16, fmt_buf5, (u_int16)*cur.dstport, FMT_PAD_RIGHT);

    if (options & FT_OPT_WIDE)
      printf("%-15.15s  %-15.15s  %-5.5s  %-16.16s  %-16.16s  %-10lu  %-10lu\n",
        fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5,
        (u_long)*cur.dOctets, (u_long)*cur.dPkts);
    else
      printf("%-15.15s  %-15.15s  %-4.4s  %-7.7s  %-7.7s  %-10lu  %-10lu\n",
        fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5,
        (u_long)*cur.dOctets, (u_long)*cur.dPkts);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  ftsym_free(sym_tcp);
  ftsym_free(sym_prot);

  return 0;

} /* format3 */


/*
 * function: format4
 *
 */
int format4(struct ftio *ftio, int options)
{
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  struct ftsym *sym_prot, *sym_asn;
  char fmt_buf1[64], fmt_buf2[64], fmt_buf3[64], fmt_buf4[64], fmt_buf5[64];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_INPUT | FT_XFIELD_OUTPUT |
    FT_XFIELD_SRCADDR | FT_XFIELD_DSTADDR | FT_XFIELD_SRC_AS |
    FT_XFIELD_DST_AS | FT_XFIELD_PROT | FT_XFIELD_SRC_MASK |
    FT_XFIELD_DST_MASK)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;   
  }

  ftio_get_ver(ftio, &ftv);
  
  fts3rec_compute_offsets(&fo, &ftv);

  sym_prot = sym_asn = (struct ftsym*)0L;

  if (options & FT_OPT_NAMES) {
    sym_prot = ftsym_new(FT_PATH_SYM_IP_PROT);
    sym_asn = ftsym_new(FT_PATH_SYM_ASN);
  }


  if (options & FT_OPT_WIDE)
    puts("Sif  Dif  srcIP               dstIP               prot   srcAS             dstAS             octets      packets");
  else
    puts("srcIP              dstIP              prot  srcAS  dstAS  octets      packets");

  while ((rec = ftio_read(ftio))) {

    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.First = ((u_int32*)(rec+fo.First));
    cur.Last = ((u_int32*)(rec+fo.Last));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));   
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.input = ((u_int16*)(rec+fo.input));
    cur.output = ((u_int16*)(rec+fo.output));
    cur.src_as = ((u_int16*)(rec+fo.src_as));
    cur.dst_as = ((u_int16*)(rec+fo.dst_as));
    cur.prot = ((u_int8*)(rec+fo.prot));
    cur.src_mask = ((u_int8*)(rec+fo.src_mask));
    cur.dst_mask = ((u_int8*)(rec+fo.dst_mask));

    fmt_ipv4prefix(fmt_buf1, *cur.srcaddr, *cur.src_mask, FMT_JUST_LEFT);

    fmt_ipv4prefix(fmt_buf2, *cur.dstaddr, *cur.dst_mask, FMT_JUST_LEFT);

    fmt_uint16s(sym_prot, 5, fmt_buf3, (u_int16)*cur.prot, FMT_PAD_RIGHT);

    fmt_uint16s(sym_asn, 18, fmt_buf4, (u_int16)*cur.src_as, FMT_JUST_LEFT);

    fmt_uint16s(sym_asn, 18, fmt_buf5, (u_int16)*cur.dst_as, FMT_JUST_LEFT);

    if (options & FT_OPT_WIDE)
      printf("%4.4x %4.4x %-18.18s  %-18.18s  %-5.5s  %-16.16s  %-16.16s  %-10lu  %-10lu\n",
        (int)*cur.input, (int)*cur.output, fmt_buf1, fmt_buf2, fmt_buf3,
        fmt_buf4, fmt_buf5,
        (u_long)*cur.dOctets, (u_long)*cur.dPkts);
    else
      printf("%-18.18s %-18.18s %-4.4s  %-5.5s  %-5.5s  %-10lu  %-10lu\n",
        fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5,
        (u_long)*cur.dOctets, (u_long)*cur.dPkts);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  ftsym_free(sym_prot);
  ftsym_free(sym_asn);

  return 0;

} /* format4 */


/*
 * function: format5
 *
 * 1 line summary, steve's favorite
 */
int format5(struct ftio *ftio, int options)
{
  struct tm *tm;
  struct fttime ftt;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[64], fmt_buf2[64];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_INPUT |
    FT_XFIELD_OUTPUT | FT_XFIELD_SRCADDR | FT_XFIELD_DSTADDR |
    FT_XFIELD_SRCPORT | FT_XFIELD_DSTPORT | FT_XFIELD_UNIX_SECS |
    FT_XFIELD_UNIX_NSECS | FT_XFIELD_SYSUPTIME | FT_XFIELD_TCP_FLAGS |
    FT_XFIELD_PROT)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);
  
  fts3rec_compute_offsets(&fo, &ftv);

  puts("Start             End               Sif   SrcIPaddress    SrcP  DIf   DstIPaddress    DstP    P Fl Pkts       Octets\n");

  while ((rec = ftio_read(ftio))) {

    cur.unix_secs = ((u_int32*)(rec+fo.unix_secs));
    cur.unix_nsecs = ((u_int32*)(rec+fo.unix_nsecs));
    cur.sysUpTime = ((u_int32*)(rec+fo.sysUpTime));
    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.First = ((u_int32*)(rec+fo.First));
    cur.Last = ((u_int32*)(rec+fo.Last));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.input = ((u_int16*)(rec+fo.input));
    cur.output = ((u_int16*)(rec+fo.output));
    cur.srcport = ((u_int16*)(rec+fo.srcport));
    cur.dstport = ((u_int16*)(rec+fo.dstport));
    cur.prot = ((u_int8*)(rec+fo.prot));
    cur.tcp_flags = ((u_int8*)(rec+fo.tcp_flags));

    ftt = ftltime(*cur.sysUpTime, *cur.unix_secs, *cur.unix_nsecs, *cur.First);
    tm = localtime((time_t*)&ftt.secs);

    printf("%-2.2d%-2.2d.%-2.2d:%-2.2d:%-2.2d.%-3.3lu ",
      (int)tm->tm_mon+1, (int)tm->tm_mday, (int)tm->tm_hour,
      (int)tm->tm_min, (int)tm->tm_sec, (u_long)ftt.msecs);

    ftt = ftltime(*cur.sysUpTime, *cur.unix_secs, *cur.unix_nsecs, *cur.Last);
    tm = localtime((time_t*)&ftt.secs);

    printf("%-2.2d%-2.2d.%-2.2d:%-2.2d:%-2.2d.%-3.3lu ",
      (int)tm->tm_mon+1, (int)tm->tm_mday, (int)tm->tm_hour,
      (int)tm->tm_min, (int)tm->tm_sec, (u_long)ftt.msecs);

    /* other info */
    fmt_ipv4(fmt_buf1, *cur.srcaddr, FMT_PAD_RIGHT);
    fmt_ipv4(fmt_buf2, *cur.dstaddr, FMT_PAD_RIGHT);

    printf("%-5u %-15.15s %-5u %-5u %-15.15s %-5u %-3u %-2d %-10lu %-10lu\n",

           (u_int)*cur.input, fmt_buf1, (u_int)*cur.srcport, 
           (u_int)*cur.output, fmt_buf2, (u_int)*cur.dstport,
           (u_int)*cur.prot, 
           (u_int)*cur.tcp_flags & 0x7,
           (u_long)*cur.dPkts, 
           (u_long)*cur.dOctets);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  return 0;

} /* format5 */

/*
 * function: format6
 *
 * 1 line summary, similar to `show ip accounting`
 */
int format6(struct ftio *ftio, int options)
{
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;   
  struct ftver ftv;
  char fmt_buf1[64], fmt_buf2[64];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_SRCADDR | FT_XFIELD_DSTADDR)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);
  
  fts3rec_compute_offsets(&fo, &ftv);

  puts(
    "   Source           Destination              Packets               Bytes");

  while ((rec = ftio_read(ftio))) {

    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));

    fmt_ipv4(fmt_buf1, *cur.srcaddr, FMT_PAD_RIGHT);

    fmt_ipv4(fmt_buf2, *cur.dstaddr, FMT_PAD_RIGHT);

    printf(" %-15.15s  %-15.15s  %17lu  %18lu\n",
      fmt_buf1, fmt_buf2, (u_long)*cur.dPkts, (u_long)*cur.dOctets);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  return 0;

} /* format6 */

/*
 * function: format7
 *
 * 1 line, 132 column -- includes router_sc from v7 format
 */
int format7(struct ftio *ftio, int options)
{
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  struct ftsym *sym_tcp, *sym_prot;
  char fmt_buf1[64], fmt_buf2[64], fmt_buf3[64], fmt_buf4[64], fmt_buf5[64];
  char fmt_buf6[64];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_PROT | FT_XFIELD_SRCADDR |
    FT_XFIELD_DSTADDR | FT_XFIELD_SRC_MASK | FT_XFIELD_DST_MASK |
    FT_XFIELD_ROUTER_SC | FT_XFIELD_SRCPORT | FT_XFIELD_DSTPORT )) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  sym_tcp = sym_prot = (struct ftsym*)0L;
 
  if (options & FT_OPT_NAMES) {
    sym_tcp = ftsym_new(FT_PATH_SYM_TCP_PORT);
    sym_prot = ftsym_new(FT_PATH_SYM_IP_PROT);
  }

  puts("srcIP               dstIP               router_sc        prot   srcPort         dstPort         octets      packets");

  while ((rec = ftio_read(ftio))) {

    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.router_sc = ((u_int32*)(rec+fo.router_sc));
    cur.srcport = ((u_int16*)(rec+fo.srcport));
    cur.dstport = ((u_int16*)(rec+fo.dstport));
    cur.prot = ((u_int8*)(rec+fo.prot));
    cur.src_mask = ((u_int8*)(rec+fo.src_mask));
    cur.dst_mask = ((u_int8*)(rec+fo.dst_mask));

    fmt_ipv4prefix(fmt_buf1, *cur.srcaddr, *cur.src_mask, FMT_PAD_RIGHT);

    fmt_ipv4prefix(fmt_buf2, *cur.dstaddr, *cur.dst_mask, FMT_PAD_RIGHT);

    fmt_ipv4(fmt_buf3, *cur.router_sc, FMT_PAD_RIGHT);

    fmt_uint16s(sym_prot, 5, fmt_buf4, (u_int16)*cur.prot, FMT_PAD_RIGHT);

    fmt_uint16s(sym_tcp, 16, fmt_buf5, (u_int16)*cur.srcport, FMT_PAD_RIGHT);

    fmt_uint16s(sym_tcp, 16, fmt_buf6, (u_int16)*cur.dstport, FMT_PAD_RIGHT);

    printf("%-18.18s  %-18.18s  %-15.15s  %-5.5s  %-14.14s  %-14.14s  %-10lu  %-10lu\n",
      fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6,
      (u_long)*cur.dOctets, (u_long)*cur.dPkts);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  ftsym_free(sym_tcp);
  ftsym_free(sym_prot);

  return 0;

} /* format7 */

/*
 * function: format8
 *
 * 1 line, 132 column -- includes encapsulation size from v6 format
 */
int format8(struct ftio *ftio, int options)
{
  struct ftsym *sym_tcp, *sym_prot;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[64], fmt_buf2[64], fmt_buf3[64], fmt_buf4[64], fmt_buf5[64];
  char fmt_buf6[64];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_PROT | FT_XFIELD_SRCADDR |
    FT_XFIELD_DSTADDR | FT_XFIELD_SRC_MASK | FT_XFIELD_DST_MASK |
    FT_XFIELD_PEER_NEXTHOP | FT_XFIELD_SRCPORT | FT_XFIELD_DSTPORT |
    FT_XFIELD_IN_ENCAPS | FT_XFIELD_OUT_ENCAPS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);
 
  fts3rec_compute_offsets(&fo, &ftv);

  sym_tcp = sym_prot = (struct ftsym*)0L;
  
  if (options & FT_OPT_NAMES) {
    sym_tcp = ftsym_new(FT_PATH_SYM_TCP_PORT);
    sym_prot = ftsym_new(FT_PATH_SYM_IP_PROT);
  } 

  puts("srcIP               dstIP               peer_nexthop     encap i/o  prot   srcPort         dstPort         octets      packets");

  while ((rec = ftio_read(ftio))) {

    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.peer_nexthop = ((u_int32*)(rec+fo.peer_nexthop));
    cur.srcport = ((u_int16*)(rec+fo.srcport));
    cur.dstport = ((u_int16*)(rec+fo.dstport));
    cur.prot = ((u_int8*)(rec+fo.prot));
    cur.src_mask = ((u_int8*)(rec+fo.src_mask));
    cur.dst_mask = ((u_int8*)(rec+fo.dst_mask));
    cur.in_encaps = ((u_int8*)(rec+fo.in_encaps));
    cur.out_encaps = ((u_int8*)(rec+fo.out_encaps));

    fmt_ipv4prefix(fmt_buf1, *cur.srcaddr, *cur.src_mask, FMT_PAD_RIGHT);

    fmt_ipv4prefix(fmt_buf2, *cur.dstaddr, *cur.dst_mask, FMT_PAD_RIGHT);

    fmt_ipv4(fmt_buf3, *cur.peer_nexthop, FMT_PAD_RIGHT);

    fmt_uint16s(sym_prot, 5, fmt_buf4, (u_int16)*cur.prot, FMT_PAD_RIGHT);

    fmt_uint16s(sym_tcp, 16, fmt_buf5, (u_int16)*cur.srcport, FMT_PAD_RIGHT);

    fmt_uint16s(sym_tcp, 16, fmt_buf6, (u_int16)*cur.dstport, FMT_PAD_RIGHT);

    printf("%-18.18s  %-18.18s  %-15.15s  %-4d %-4d  %-5.5s  %-14.14s  %-14.14s  %-10lu  %-10lu\n",
      fmt_buf1, fmt_buf2, fmt_buf3, (int)*cur.in_encaps, (int)*cur.out_encaps,
      fmt_buf4, fmt_buf5, fmt_buf6, (u_long)*cur.dOctets, (u_long)*cur.dPkts);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  ftsym_free(sym_tcp);
  ftsym_free(sym_prot);

  return 0;

} /* format8 */

/*
 * 1 line, include src and dst tag
 */
int format9(struct ftio *ftio, int options)
{
  struct ftsym *sym_tag;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[64], fmt_buf2[64], fmt_buf3[64], fmt_buf4[64];
  char *fmt_src, *fmt_dst;
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_SRCADDR | FT_XFIELD_DSTADDR |
    FT_XFIELD_SRC_TAG | FT_XFIELD_DST_TAG | FT_XFIELD_PROT)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;   
  }

  sym_tag = (struct ftsym*)0L;
  
  if (options & FT_OPT_NAMES) {
    sym_tag = ftsym_new(FT_PATH_SYM_TAG);
  } 

  ftio_get_ver(ftio, &ftv);
  
  fts3rec_compute_offsets(&fo, &ftv);

  if (options & FT_OPT_WIDE)
    puts("srcTag             dstTag             srcIP            dstIP            octets      packets");
  else
    puts("srcTag      dstTag      srcIP            dstIP            octets      packets");

  while ((rec = ftio_read(ftio))) {

    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));   
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.src_tag = ((u_int32*)(rec+fo.src_tag));   
    cur.dst_tag = ((u_int32*)(rec+fo.dst_tag));

    fmt_ipv4(fmt_buf1, *cur.srcaddr, FMT_PAD_RIGHT);
    fmt_ipv4(fmt_buf2, *cur.dstaddr, FMT_PAD_RIGHT);

    sprintf(fmt_buf3, "0x%-8.8lx", (u_long)*cur.src_tag);
    sprintf(fmt_buf4, "0x%-8.8lx", (u_long)*cur.dst_tag);

    if (options & FT_OPT_NAMES) {

      if (ftsym_findbyval(sym_tag, (u_int32)*cur.src_tag, &fmt_src) != 1) {
        sprintf(fmt_buf3, "0x%-8.8lx", (u_long)*cur.src_tag);
        fmt_src = fmt_buf3;
      }

      if (ftsym_findbyval(sym_tag, (u_int32)*cur.dst_tag, &fmt_dst) != 1) {
        sprintf(fmt_buf4, "0x%-8.8lx", (u_long)*cur.dst_tag);
        fmt_dst = fmt_buf4;
      }

    } else {

      sprintf(fmt_buf3, "0x%-8.8lx", (u_long)*cur.src_tag);
      sprintf(fmt_buf4, "0x%-8.8lx", (u_long)*cur.dst_tag);

      fmt_src = fmt_buf3;
      fmt_dst = fmt_buf4;

    }


    if (options & FT_OPT_WIDE)
      printf("%-15.15s  %-15.15s  %-15.15s  %-15.15s  %-10lu  %-10lu\n",
        fmt_src, fmt_dst, fmt_buf1, fmt_buf2,
        (u_long)*cur.dOctets, (u_long)*cur.dPkts);
    else
      printf("%-10.10s  %-10.10s  %-15.15s  %-15.15s  %-10lu  %-10lu\n",
        fmt_src, fmt_dst, fmt_buf1, fmt_buf2,
        (u_long)*cur.dOctets, (u_long)*cur.dPkts);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  ftsym_free(sym_tag);

  return 0;

} /* format9 */

/*
 * 8.1 AS aggregation
 */
int format10(struct ftio *ftio, int options)
{
  struct ftsym *sym_asn;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[64], fmt_buf2[64], fmt_buf3[32], fmt_buf4[32];
  char fmt_buf5[32], fmt_buf6[32], fmt_buf7[32], fmt_buf8[32];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DFLOWS | FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_INPUT |
    FT_XFIELD_OUTPUT | FT_XFIELD_SRC_AS | FT_XFIELD_DST_AS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  sym_asn = (struct ftsym*)0L;
        
  if (options & FT_OPT_NAMES) {
    sym_asn = ftsym_new(FT_PATH_SYM_ASN);
  }

  if (options & FT_OPT_WIDE)
    puts("srcAS             dstAS             in     out    flows       octets      packets     duration");
  else
    puts(
"srcAS  dstAS  in     out    flows       octets      packets     duration");

  while ((rec = ftio_read(ftio))) {

    cur.dFlows = ((u_int32*)(rec+fo.dFlows));
    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.First = ((u_int32*)(rec+fo.First));
    cur.Last = ((u_int32*)(rec+fo.Last));
    cur.input = ((u_int16*)(rec+fo.input));
    cur.output = ((u_int16*)(rec+fo.output));
    cur.src_as = ((u_int16*)(rec+fo.src_as));
    cur.dst_as = ((u_int16*)(rec+fo.dst_as));

    fmt_uint16s(sym_asn, 18, fmt_buf1, (u_int16)*cur.src_as, FMT_JUST_LEFT);
    fmt_uint16s(sym_asn, 18, fmt_buf2, (u_int16)*cur.dst_as, FMT_JUST_LEFT);
    fmt_uint16(fmt_buf3, *cur.input, FMT_JUST_LEFT);
    fmt_uint16(fmt_buf4, *cur.output, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf5, *cur.dFlows, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf6, *cur.dOctets, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf7, *cur.dPkts, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf8, *cur.Last - *cur.First, FMT_JUST_LEFT);

    if (options & FT_OPT_WIDE)
      printf(
        "%-16.16s  %-16.16s  %-7.7s%-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
        fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6, fmt_buf7,
        fmt_buf8);
    else
      printf("%-5.5s  %-5.5s  %-7.7s%-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
        fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6, fmt_buf7,
        fmt_buf8);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  }

  ftsym_free(sym_asn);

  return 0;

} /* format10 */

/*
 * 8.2 Protocol Port aggregation
 */
int format11(struct ftio *ftio, int options)
{
  struct ftsym *sym_tcp, *sym_prot;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[32], fmt_buf2[32], fmt_buf3[32], fmt_buf4[32];
  char fmt_buf5[32], fmt_buf6[32], fmt_buf7[32];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DFLOWS | FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_SRCPORT | FT_XFIELD_DSTPORT | FT_XFIELD_PROT)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);
    
  fts3rec_compute_offsets(&fo, &ftv);

  sym_tcp = sym_prot = (struct ftsym*)0L;

  if (options & FT_OPT_NAMES) {
    sym_tcp = ftsym_new(FT_PATH_SYM_TCP_PORT);
    sym_prot = ftsym_new(FT_PATH_SYM_IP_PROT);
  }

  if (options & FT_OPT_WIDE)
    puts(
"srcPort           dstPort           prot   flows       octets      packets     duration");
  else
    puts(
"srcPort  dstPort  prot   flows       octets      packets     duration");

   while ((rec = ftio_read(ftio))) {

    cur.dFlows = ((u_int32*)(rec+fo.dFlows));
    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.First = ((u_int32*)(rec+fo.First));
    cur.Last = ((u_int32*)(rec+fo.Last));
    cur.srcport = ((u_int16*)(rec+fo.srcport));
    cur.dstport = ((u_int16*)(rec+fo.dstport));
    cur.prot = ((u_int8*)(rec+fo.prot));

    fmt_uint16s(sym_tcp, 16, fmt_buf1, (u_int16)*cur.srcport, FMT_PAD_RIGHT);
    fmt_uint16s(sym_tcp, 16, fmt_buf2, (u_int16)*cur.dstport, FMT_PAD_RIGHT);
    fmt_uint16s(sym_prot, 5, fmt_buf3, (u_int16)*cur.prot, FMT_PAD_RIGHT);
    fmt_uint32(fmt_buf4, *cur.dFlows, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf5, *cur.dOctets, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf6, *cur.dPkts, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf7, *cur.Last - *cur.First, FMT_JUST_LEFT);

    if (options & FT_OPT_WIDE)
      printf("%-16.16s  %-16.16s  %-5.5s  %-12.12s%-12.12s%-12.12s%-12.12s\n",
        fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6, fmt_buf7);
    else
      printf("%-7.7s  %-7.7s  %-5.5s  %-12.12s%-12.12s%-12.12s%-12.12s\n",
        fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6, fmt_buf7);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  }

  ftsym_free(sym_tcp);
  ftsym_free(sym_prot);

  return 0;

} /* format11 */

/*
 * 8.3 Source Prefix aggregation
 */
int format12(struct ftio *ftio, int options)
{
  struct ftsym *sym_asn;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[32], fmt_buf3[32], fmt_buf4[32], fmt_buf5[32];
  char fmt_buf6[32], fmt_buf7[32], fmt_buf8[32];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DFLOWS | FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_INPUT |
    FT_XFIELD_SRCADDR | FT_XFIELD_SRC_MASK | FT_XFIELD_SRC_AS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  sym_asn = (struct ftsym*)0L;

  if (options & FT_OPT_NAMES) {
    sym_asn = ftsym_new(FT_PATH_SYM_ASN);
  }

  if (options & FT_OPT_WIDE)
    puts(
"srcPrefix           srcAS             input  flows       octets      packets     duration");
  else
    puts(
"src/mask            srcAS  input  flows       octets      packets     duration");

   while ((rec = ftio_read(ftio))) {

    cur.dFlows = ((u_int32*)(rec+fo.dFlows));
    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.First = ((u_int32*)(rec+fo.First));
    cur.Last = ((u_int32*)(rec+fo.Last));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.input = ((u_int16*)(rec+fo.input));
    cur.src_as = ((u_int16*)(rec+fo.src_as));
    cur.src_mask = ((u_int8*)(rec+fo.src_mask));

    fmt_ipv4prefix(fmt_buf1, *cur.srcaddr, *cur.src_mask, FMT_JUST_LEFT);
    fmt_uint16s(sym_asn, 18, fmt_buf3, *cur.src_as, FMT_JUST_LEFT);
    fmt_uint16(fmt_buf4, *cur.input, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf5, *cur.dFlows, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf6, *cur.dOctets, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf7, *cur.dPkts, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf8, *cur.Last - *cur.First, FMT_JUST_LEFT);

    if (options & FT_OPT_WIDE)
      printf("%-18.18s  %-16.16s  %-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
        fmt_buf1, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6, fmt_buf7, fmt_buf8);
    else
      printf("%-18.18s  %-5.5s  %-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
        fmt_buf1, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6, fmt_buf7, fmt_buf8);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  ftsym_free(sym_asn);

  return 0;

} /* format12 */

/*
 * 8.4 Destination Prefix aggregation
 */
int format13(struct ftio *ftio, int options)
{
  struct ftsym *sym_asn;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[32], fmt_buf3[32], fmt_buf4[32], fmt_buf5[32];
  char fmt_buf6[32], fmt_buf7[32], fmt_buf8[32];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DFLOWS | FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_OUTPUT |
    FT_XFIELD_DSTADDR | FT_XFIELD_DST_MASK | FT_XFIELD_DST_AS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  sym_asn = (struct ftsym*)0L;

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  if (options & FT_OPT_NAMES) {
    sym_asn = ftsym_new(FT_PATH_SYM_ASN);
  }

  if (options & FT_OPT_WIDE)
    puts(
"dstPrefix           dstAS             input  flows       octets      packets     duration");
  else
    puts(
"dst/mask            dstAS  input  flows       octets      packets     duration");

  while ((rec = ftio_read(ftio))) {

    cur.dFlows = ((u_int32*)(rec+fo.dFlows));
    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.First = ((u_int32*)(rec+fo.First));
    cur.Last = ((u_int32*)(rec+fo.Last));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.output = ((u_int16*)(rec+fo.output));
    cur.dst_as = ((u_int16*)(rec+fo.dst_as));
    cur.dst_mask = ((u_int8*)(rec+fo.dst_mask));

    fmt_ipv4prefix(fmt_buf1, *cur.dstaddr, *cur.dst_mask, FMT_JUST_LEFT);
    fmt_uint16s(sym_asn, 18, fmt_buf3, *cur.dst_as, FMT_JUST_LEFT);
    fmt_uint16(fmt_buf4, *cur.output, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf5, *cur.dFlows, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf6, *cur.dOctets, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf7, *cur.dPkts, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf8, *cur.Last - *cur.First, FMT_JUST_LEFT);

    if (options & FT_OPT_WIDE)
      printf("%-18.18s  %-16.16s  %-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
        fmt_buf1, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6, fmt_buf7, fmt_buf8);
    else
      printf("%-18.18s  %-5.5s  %-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
        fmt_buf1, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6, fmt_buf7, fmt_buf8);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  ftsym_free(sym_asn);

  return 0;
} /* format13 */

/*
 * 8.5 Prefix aggregation
 */
int format14(struct ftio *ftio, int options)
{
  struct ftsym *sym_asn;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[32], fmt_buf2[32], fmt_buf3[32], fmt_buf4[32];
  char fmt_buf5[32], fmt_buf6[32], fmt_buf7[32], fmt_buf8[32];
  char fmt_buf9[32], fmt_buf10[32];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DFLOWS | FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_INPUT |
    FT_XFIELD_OUTPUT | FT_XFIELD_SRCADDR | FT_XFIELD_DSTADDR |
    FT_XFIELD_SRC_MASK | FT_XFIELD_DST_MASK | FT_XFIELD_SRC_AS |
    FT_XFIELD_DST_AS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  sym_asn = (struct ftsym*)0L;

  if (options & FT_OPT_NAMES) {
    sym_asn = ftsym_new(FT_PATH_SYM_ASN);
  }

  if (options & FT_OPT_WIDE)
    puts(
"srcPrefix           srcAS             dstPrefix           dstAS             input  output flows       octets      packets     duration");
  else
    puts(
"srcPrefix           srcAS  dstPrefix           dstAS  input  output flows       octets      packets     duration");

  while ((rec = ftio_read(ftio))) {

    cur.dFlows = ((u_int32*)(rec+fo.dFlows));
    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.First = ((u_int32*)(rec+fo.First));
    cur.Last = ((u_int32*)(rec+fo.Last));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.input = ((u_int16*)(rec+fo.input));
    cur.output = ((u_int16*)(rec+fo.output));
    cur.src_as = ((u_int16*)(rec+fo.src_as));
    cur.dst_as = ((u_int16*)(rec+fo.dst_as));
    cur.src_mask = ((u_int8*)(rec+fo.src_mask));
    cur.dst_mask = ((u_int8*)(rec+fo.dst_mask));

    fmt_ipv4prefix(fmt_buf1, *cur.srcaddr, *cur.src_mask, FMT_JUST_LEFT);
    fmt_ipv4prefix(fmt_buf3, *cur.dstaddr, *cur.dst_mask, FMT_JUST_LEFT);
    fmt_uint16s(sym_asn, 18, fmt_buf2, *cur.src_as, FMT_JUST_LEFT);
    fmt_uint16s(sym_asn, 18, fmt_buf4, *cur.dst_as, FMT_JUST_LEFT);
    fmt_uint16(fmt_buf5, *cur.input, FMT_JUST_LEFT);
    fmt_uint16(fmt_buf6, *cur.output, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf7, *cur.dFlows, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf8, *cur.dOctets, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf9, *cur.dPkts, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf10, *cur.Last - *cur.First, FMT_JUST_LEFT);

    if (options & FT_OPT_WIDE)
    printf("%-18.18s  %-16.16s  %-18.18s  %-16.16s  %-7.7s%-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
      fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6,
      fmt_buf7, fmt_buf8, fmt_buf9, fmt_buf10);
    else
    printf("%-18.18s  %-5.5s  %-18.18s  %-5.5s  %-7.7s%-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
      fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6,
      fmt_buf7, fmt_buf8, fmt_buf9, fmt_buf10);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  ftsym_free(sym_asn);

  return 0;

} /* format14 */

/*
 * function: format15
 *
 * 1 line, 132 column for v8.6
 */
int format15(struct ftio *ftio, int options)
{
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[32], fmt_buf2[32], fmt_buf3[32], fmt_buf4[32], fmt_buf5[32];
  char fmt_buf6[32], fmt_buf7[32], fmt_buf8[32];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_DSTADDR | FT_XFIELD_OUTPUT |
    FT_XFIELD_ROUTER_SC | FT_XFIELD_EXTRA_PKTS | FT_XFIELD_TOS |
    FT_XFIELD_MARKED_TOS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  puts("dstIP            router_sc        Dif    ToS  mToS xpackets    octets      packets");

  while ((rec = ftio_read(ftio))) {

    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.router_sc = ((u_int32*)(rec+fo.router_sc));
    cur.output = ((u_int16*)(rec+fo.output));
    cur.tos = ((u_int8*)(rec+fo.tos));
    cur.marked_tos = ((u_int8*)(rec+fo.marked_tos));
    cur.extra_pkts = ((u_int32*)(rec+fo.extra_pkts));

    fmt_ipv4(fmt_buf1, *cur.dstaddr, FMT_PAD_RIGHT);
    fmt_ipv4(fmt_buf2, *cur.router_sc, FMT_PAD_RIGHT);
    fmt_uint16(fmt_buf3, *cur.output, FMT_PAD_RIGHT);
    fmt_uint8(fmt_buf4, *cur.tos, FMT_PAD_RIGHT);
    fmt_uint8(fmt_buf5, *cur.marked_tos, FMT_PAD_RIGHT);
    fmt_uint32(fmt_buf6, *cur.extra_pkts, FMT_PAD_RIGHT);
    fmt_uint32(fmt_buf7, *cur.dOctets, FMT_PAD_RIGHT);
    fmt_uint32(fmt_buf8, *cur.dPkts, FMT_PAD_RIGHT);

    printf("%-15.15s  %-15.15s  %-5.5s  %-3.3s  %-3.3s  %-10.10s  %-10.10s  %-10.10s\n",
      fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6,
      fmt_buf7, fmt_buf8);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  return 0;

} /* format15 */

/*
 * function: format16
 *
 * 1 line, 132 column for v8.7
 */
int format16(struct ftio *ftio, int options)
{
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[32], fmt_buf2[32], fmt_buf3[32], fmt_buf4[32], fmt_buf5[32];
  char fmt_buf6[32], fmt_buf7[32], fmt_buf8[32], fmt_buf9[32], fmt_buf10[32];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DPKTS | FT_XFIELD_DOCTETS |
    FT_XFIELD_DSTADDR | FT_XFIELD_SRCADDR | FT_XFIELD_OUTPUT |
    FT_XFIELD_INPUT | FT_XFIELD_ROUTER_SC | FT_XFIELD_EXTRA_PKTS |
    FT_XFIELD_TOS | FT_XFIELD_MARKED_TOS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  puts("srcIP            dstIP            router_sc        Sif    Dif    ToS  mToS xpackets    octets      packets");

  while ((rec = ftio_read(ftio))) {

    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.router_sc = ((u_int32*)(rec+fo.router_sc));
    cur.output = ((u_int16*)(rec+fo.output));
    cur.input = ((u_int16*)(rec+fo.input));
    cur.tos = ((u_int8*)(rec+fo.tos));
    cur.marked_tos = ((u_int8*)(rec+fo.marked_tos));
    cur.extra_pkts = ((u_int32*)(rec+fo.extra_pkts));

    fmt_ipv4(fmt_buf1, *cur.srcaddr, FMT_PAD_RIGHT);
    fmt_ipv4(fmt_buf2, *cur.dstaddr, FMT_PAD_RIGHT);
    fmt_ipv4(fmt_buf3, *cur.router_sc, FMT_PAD_RIGHT);
    fmt_uint16(fmt_buf4, *cur.input, FMT_PAD_RIGHT);
    fmt_uint16(fmt_buf5, *cur.output, FMT_PAD_RIGHT);
    fmt_uint8(fmt_buf6, *cur.tos, FMT_PAD_RIGHT);
    fmt_uint8(fmt_buf7, *cur.marked_tos, FMT_PAD_RIGHT);
    fmt_uint32(fmt_buf8, *cur.extra_pkts, FMT_PAD_RIGHT);
    fmt_uint32(fmt_buf9, *cur.dOctets, FMT_PAD_RIGHT);
    fmt_uint32(fmt_buf10, *cur.dPkts, FMT_PAD_RIGHT);

    printf("%-15.15s  %-15.15s  %-15.15s  %-5.5s  %-5.5s  %-3.3s  %-3.3s  %-10.10s  %-10.10s  %-10.10s\n",
      fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6,
      fmt_buf7, fmt_buf8, fmt_buf9, fmt_buf10);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  return 0;

} /* format16 */

/*
 * function: format17
 *
 * 1 line, 132 column for v8.8
 */
int format17(struct ftio *ftio, int options)
{
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  struct ftsym *sym_tcp, *sym_prot;
  char fmt_buf1[32], fmt_buf2[32], fmt_buf3[32], fmt_buf4[32], fmt_buf5[32];
  char fmt_buf6[32], fmt_buf7[32], fmt_buf8[32], fmt_buf9[32], fmt_buf10[32];
  char fmt_buf11[32], fmt_buf12[32], fmt_buf13[32];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DPKTS | FT_XFIELD_DOCTETS |
    FT_XFIELD_DSTADDR | FT_XFIELD_SRCADDR | FT_XFIELD_OUTPUT |
    FT_XFIELD_INPUT | FT_XFIELD_ROUTER_SC | FT_XFIELD_EXTRA_PKTS |
    FT_XFIELD_SRCPORT | FT_XFIELD_DSTPORT | FT_XFIELD_PROT |
    FT_XFIELD_TOS | FT_XFIELD_MARKED_TOS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  sym_tcp = sym_prot = (struct ftsym*)0L;

  if (options & FT_OPT_NAMES) {
    sym_tcp = ftsym_new(FT_PATH_SYM_TCP_PORT);
    sym_prot = ftsym_new(FT_PATH_SYM_IP_PROT);
  }

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  puts("srcIP            dstIP            router_sc        Sif    Dif    SrcP   DstP   prot ToS  mToS xpackets    octets      packets");

  while ((rec = ftio_read(ftio))) {

    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.router_sc = ((u_int32*)(rec+fo.router_sc));
    cur.output = ((u_int16*)(rec+fo.output));
    cur.input = ((u_int16*)(rec+fo.input));
    cur.srcport = ((u_int16*)(rec+fo.srcport));
    cur.dstport = ((u_int16*)(rec+fo.dstport));
    cur.tos = ((u_int8*)(rec+fo.tos));
    cur.prot = ((u_int8*)(rec+fo.prot));
    cur.marked_tos = ((u_int8*)(rec+fo.marked_tos));
    cur.extra_pkts = ((u_int32*)(rec+fo.extra_pkts));

    fmt_uint16s(sym_tcp, 5, fmt_buf6, (u_int16)*cur.srcport, FMT_PAD_RIGHT);
  
    fmt_uint16s(sym_tcp, 5, fmt_buf7, (u_int16)*cur.dstport, FMT_PAD_RIGHT);

    fmt_uint16s(sym_prot, 5, fmt_buf8, (u_int16)*cur.prot, FMT_PAD_RIGHT);

    fmt_ipv4(fmt_buf1, *cur.srcaddr, FMT_PAD_RIGHT);
    fmt_ipv4(fmt_buf2, *cur.dstaddr, FMT_PAD_RIGHT);
    fmt_ipv4(fmt_buf3, *cur.router_sc, FMT_PAD_RIGHT);
    fmt_uint16(fmt_buf4, *cur.input, FMT_PAD_RIGHT);
    fmt_uint16(fmt_buf5, *cur.output, FMT_PAD_RIGHT);
    fmt_uint8(fmt_buf9, *cur.tos, FMT_PAD_RIGHT);
    fmt_uint8(fmt_buf10, *cur.marked_tos, FMT_PAD_RIGHT);
    fmt_uint32(fmt_buf11, *cur.extra_pkts, FMT_PAD_RIGHT);
    fmt_uint32(fmt_buf12, *cur.dOctets, FMT_PAD_RIGHT);
    fmt_uint32(fmt_buf13, *cur.dPkts, FMT_PAD_RIGHT);

    printf("%-15.15s  %-15.15s  %-15.15s  %-5.5s  %-5.5s  %-5.5s  %-5.5s  %-3.3s  %-3.3s  %-3.3s  %-10.10s  %-10.10s  %-10.10s\n",
      fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6,
      fmt_buf7, fmt_buf8, fmt_buf9, fmt_buf10, fmt_buf11, fmt_buf12,
      fmt_buf13);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  ftsym_free(sym_tcp);
  ftsym_free(sym_prot);

  return 0;

} /* format17 */

/*
 * 8.9 ToS AS aggregation
 */
int format18(struct ftio *ftio, int options)
{
  struct ftsym *sym_asn;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[64], fmt_buf2[64], fmt_buf3[32], fmt_buf4[32];
  char fmt_buf5[32], fmt_buf6[32], fmt_buf7[32], fmt_buf8[32], fmt_buf9[32];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DFLOWS | FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_INPUT |
    FT_XFIELD_OUTPUT | FT_XFIELD_SRC_AS | FT_XFIELD_DST_AS | FT_XFIELD_TOS )) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  sym_asn = (struct ftsym*)0L;
        
  if (options & FT_OPT_NAMES) {
    sym_asn = ftsym_new(FT_PATH_SYM_ASN);
  }

  if (options & FT_OPT_WIDE)
    puts("ToS  srcAS             dstAS             in     out    flows       octets      packets     duration");
  else
    puts(
"ToS  srcAS  dstAS  in     out    flows       octets      packets     duration");

  while ((rec = ftio_read(ftio))) {

    cur.dFlows = ((u_int32*)(rec+fo.dFlows));
    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.First = ((u_int32*)(rec+fo.First));
    cur.Last = ((u_int32*)(rec+fo.Last));
    cur.input = ((u_int16*)(rec+fo.input));
    cur.output = ((u_int16*)(rec+fo.output));
    cur.src_as = ((u_int16*)(rec+fo.src_as));
    cur.dst_as = ((u_int16*)(rec+fo.dst_as));
    cur.tos = ((u_int8*)(rec+fo.tos));

    fmt_uint8(fmt_buf1, *cur.tos, FMT_JUST_LEFT);
    fmt_uint16s(sym_asn, 18, fmt_buf2, (u_int16)*cur.src_as, FMT_JUST_LEFT);
    fmt_uint16s(sym_asn, 18, fmt_buf3, (u_int16)*cur.dst_as, FMT_JUST_LEFT);
    fmt_uint16(fmt_buf4, *cur.input, FMT_JUST_LEFT);
    fmt_uint16(fmt_buf5, *cur.output, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf6, *cur.dFlows, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf7, *cur.dOctets, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf8, *cur.dPkts, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf9, *cur.Last - *cur.First, FMT_JUST_LEFT);

    if (options & FT_OPT_WIDE)
      printf(
        "%-3.3s  %-16.16s  %-16.16s  %-7.7s%-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
        fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6, fmt_buf7,
        fmt_buf8, fmt_buf9);
    else
      printf("%-3.3s  %-5.5s  %-5.5s  %-7.7s%-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
        fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6, fmt_buf7,
        fmt_buf8, fmt_buf9);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  }

  ftsym_free(sym_asn);

  return 0;

} /* format18 */

/*
 * 8.10 ToS Protocol Port aggregation
 */
int format19(struct ftio *ftio, int options)
{
  struct ftsym *sym_tcp, *sym_prot;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[32], fmt_buf2[32], fmt_buf3[32], fmt_buf4[32];
  char fmt_buf5[32], fmt_buf6[32], fmt_buf7[32], fmt_buf8[32];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DFLOWS | FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST |
    FT_XFIELD_SRCPORT | FT_XFIELD_DSTPORT | FT_XFIELD_PROT | FT_XFIELD_TOS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);
    
  fts3rec_compute_offsets(&fo, &ftv);

  sym_tcp = sym_prot = (struct ftsym*)0L;

  if (options & FT_OPT_NAMES) {
    sym_tcp = ftsym_new(FT_PATH_SYM_TCP_PORT);
    sym_prot = ftsym_new(FT_PATH_SYM_IP_PROT);
  }

  if (options & FT_OPT_WIDE)
    puts(
"ToS  srcPort           dstPort           prot   flows       octets      packets     duration");
  else
    puts(
"ToS  srcPort  dstPort  prot   flows       octets      packets     duration");

   while ((rec = ftio_read(ftio))) {

    cur.dFlows = ((u_int32*)(rec+fo.dFlows));
    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.First = ((u_int32*)(rec+fo.First));
    cur.Last = ((u_int32*)(rec+fo.Last));
    cur.srcport = ((u_int16*)(rec+fo.srcport));
    cur.dstport = ((u_int16*)(rec+fo.dstport));
    cur.prot = ((u_int8*)(rec+fo.prot));
    cur.tos = ((u_int8*)(rec+fo.prot));

    fmt_uint8(fmt_buf1, *cur.tos, FMT_PAD_RIGHT);
    fmt_uint16s(sym_tcp, 16, fmt_buf2, (u_int16)*cur.srcport, FMT_PAD_RIGHT);
    fmt_uint16s(sym_tcp, 16, fmt_buf3, (u_int16)*cur.dstport, FMT_PAD_RIGHT);
    fmt_uint16s(sym_prot, 5, fmt_buf4, (u_int16)*cur.prot, FMT_PAD_RIGHT);
    fmt_uint32(fmt_buf5, *cur.dFlows, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf6, *cur.dOctets, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf7, *cur.dPkts, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf8, *cur.Last - *cur.First, FMT_JUST_LEFT);

    if (options & FT_OPT_WIDE)
      printf("%-3.3s  %-16.16s  %-16.16s  %-5.5s  %-12.12s%-12.12s%-12.12s%-12.12s\n",
        fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6, fmt_buf7,
        fmt_buf8);
    else
      printf("%-3.3s  %-7.7s  %-7.7s  %-5.5s  %-12.12s%-12.12s%-12.12s%-12.12s\n",
        fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6, fmt_buf7, 
        fmt_buf8);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  }

  ftsym_free(sym_tcp);
  ftsym_free(sym_prot);

  return 0;

} /* format19 */

/*
 * 8.11 ToS Source Prefix aggregation
 */
int format20(struct ftio *ftio, int options)
{
  struct ftsym *sym_asn;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[32], fmt_buf2[32], fmt_buf3[32], fmt_buf4[32];
  char fmt_buf5[32], fmt_buf6[32], fmt_buf7[32], fmt_buf8[32];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DFLOWS | FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_INPUT |
    FT_XFIELD_SRCADDR | FT_XFIELD_SRC_MASK | FT_XFIELD_SRC_AS |
    FT_XFIELD_TOS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  sym_asn = (struct ftsym*)0L;

  if (options & FT_OPT_NAMES) {
    sym_asn = ftsym_new(FT_PATH_SYM_ASN);
  }

  if (options & FT_OPT_WIDE)
    puts(
"ToS  srcPrefix           srcAS             input  flows       octets      packets     duration");
  else
    puts(
"ToS  srcPrefix           srcAS  input  flows       octets      packets     duration");

   while ((rec = ftio_read(ftio))) {

    cur.dFlows = ((u_int32*)(rec+fo.dFlows));
    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.First = ((u_int32*)(rec+fo.First));
    cur.Last = ((u_int32*)(rec+fo.Last));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.input = ((u_int16*)(rec+fo.input));
    cur.src_as = ((u_int16*)(rec+fo.src_as));
    cur.src_mask = ((u_int8*)(rec+fo.src_mask));
    cur.tos = ((u_int8*)(rec+fo.tos));

    fmt_uint8(fmt_buf1, *cur.tos, FMT_JUST_LEFT);
    fmt_ipv4prefix(fmt_buf2, *cur.srcaddr, *cur.src_mask, FMT_JUST_LEFT);
    fmt_uint16s(sym_asn, 18, fmt_buf3, *cur.src_as, FMT_JUST_LEFT);
    fmt_uint16(fmt_buf4, *cur.input, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf5, *cur.dFlows, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf6, *cur.dOctets, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf7, *cur.dPkts, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf8, *cur.Last - *cur.First, FMT_JUST_LEFT);

    if (options & FT_OPT_WIDE)
      printf("%-3.3s  %-18.18s  %-16.16s  %-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
        fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6, fmt_buf7,
        fmt_buf8);
    else
      printf("%-3.3s  %-18.18s  %-5.5s  %-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
        fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6, fmt_buf7,
        fmt_buf8);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  ftsym_free(sym_asn);

  return 0;

} /* format20 */

/*
 * 8.12 ToS Destination Prefix aggregation
 */
int format21(struct ftio *ftio, int options)
{
  struct ftsym *sym_asn;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[32], fmt_buf2[32], fmt_buf3[32], fmt_buf4[32];
  char fmt_buf5[32], fmt_buf6[32], fmt_buf7[32], fmt_buf8[32];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DFLOWS | FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_OUTPUT |
    FT_XFIELD_DSTADDR | FT_XFIELD_DST_MASK | FT_XFIELD_DST_AS |
    FT_XFIELD_TOS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  sym_asn = (struct ftsym*)0L;

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  if (options & FT_OPT_NAMES) {
    sym_asn = ftsym_new(FT_PATH_SYM_ASN);
  }

  if (options & FT_OPT_WIDE)
    puts(
"ToS  dstPrefix           dstAS             input  flows       octets      packets     duration");
  else
    puts(
"ToS  dst/mask            dstAS  input  flows       octets      packets     duration");

  while ((rec = ftio_read(ftio))) {

    cur.dFlows = ((u_int32*)(rec+fo.dFlows));
    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.First = ((u_int32*)(rec+fo.First));
    cur.Last = ((u_int32*)(rec+fo.Last));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.output = ((u_int16*)(rec+fo.output));
    cur.dst_as = ((u_int16*)(rec+fo.dst_as));
    cur.dst_mask = ((u_int8*)(rec+fo.dst_mask));
    cur.tos = ((u_int8*)(rec+fo.tos));

    fmt_uint8(fmt_buf1, *cur.tos, FMT_JUST_LEFT);
    fmt_ipv4prefix(fmt_buf2, *cur.dstaddr, *cur.dst_mask, FMT_JUST_LEFT);
    fmt_uint16s(sym_asn, 18, fmt_buf3, *cur.dst_as, FMT_JUST_LEFT);
    fmt_uint16(fmt_buf4, *cur.output, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf5, *cur.dFlows, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf6, *cur.dOctets, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf7, *cur.dPkts, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf8, *cur.Last - *cur.First, FMT_JUST_LEFT);

    if (options & FT_OPT_WIDE)
      printf("%-3.3s  %-18.18s  %-16.16s  %-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
        fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6,
        fmt_buf7, fmt_buf8);
    else
      printf("%-3.3s  %-18.18s  %-5.5s  %-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
        fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6,
        fmt_buf7, fmt_buf8);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  ftsym_free(sym_asn);

  return 0;
} /* format21 */

/*
 * 8.13 ToS Prefix aggregation
 */
int format22(struct ftio *ftio, int options)
{
  struct ftsym *sym_asn;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[32], fmt_buf2[32], fmt_buf3[32], fmt_buf4[32];
  char fmt_buf5[32], fmt_buf6[32], fmt_buf7[32], fmt_buf8[32];
  char fmt_buf9[32], fmt_buf10[32], fmt_buf11[32];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DFLOWS | FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_INPUT |
    FT_XFIELD_OUTPUT | FT_XFIELD_SRCADDR | FT_XFIELD_DSTADDR |
    FT_XFIELD_SRC_MASK | FT_XFIELD_DST_MASK | FT_XFIELD_SRC_AS |
    FT_XFIELD_DST_AS | FT_XFIELD_TOS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  sym_asn = (struct ftsym*)0L;

  if (options & FT_OPT_NAMES) {
    sym_asn = ftsym_new(FT_PATH_SYM_ASN);
  }

  if (options & FT_OPT_WIDE)
    puts(
"ToS  srcPrefix           srcAS             dstPrefix           dstAS             input  output flows       octets      packets     duration");
  else
    puts(
"ToS  srcPrefix           srcAS  dstPrefix           dstAS  input  output flows       octets      packets     duration");

  while ((rec = ftio_read(ftio))) {

    cur.dFlows = ((u_int32*)(rec+fo.dFlows));
    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.First = ((u_int32*)(rec+fo.First));
    cur.Last = ((u_int32*)(rec+fo.Last));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.input = ((u_int16*)(rec+fo.input));
    cur.output = ((u_int16*)(rec+fo.output));
    cur.src_as = ((u_int16*)(rec+fo.src_as));
    cur.dst_as = ((u_int16*)(rec+fo.dst_as));
    cur.src_mask = ((u_int8*)(rec+fo.src_mask));
    cur.dst_mask = ((u_int8*)(rec+fo.dst_mask));
    cur.tos = ((u_int8*)(rec+fo.tos));

    fmt_uint8(fmt_buf1, *cur.tos, FMT_JUST_LEFT);
    fmt_ipv4prefix(fmt_buf2, *cur.srcaddr, *cur.src_mask, FMT_JUST_LEFT);
    fmt_ipv4prefix(fmt_buf3, *cur.dstaddr, *cur.dst_mask, FMT_JUST_LEFT);
    fmt_uint16s(sym_asn, 18, fmt_buf4, *cur.src_as, FMT_JUST_LEFT);
    fmt_uint16s(sym_asn, 18, fmt_buf5, *cur.dst_as, FMT_JUST_LEFT);
    fmt_uint16(fmt_buf6, *cur.input, FMT_JUST_LEFT);
    fmt_uint16(fmt_buf7, *cur.output, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf8, *cur.dFlows, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf9, *cur.dOctets, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf10, *cur.dPkts, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf11, *cur.Last - *cur.First, FMT_JUST_LEFT);

    if (options & FT_OPT_WIDE)
    printf("%-3.3s  %-18.18s  %-16.16s  %-18.18s  %-16.16s  %-7.7s%-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
      fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6,
      fmt_buf7, fmt_buf8, fmt_buf9, fmt_buf10, fmt_buf11);
    else
    printf("%-3.3s  %-18.18s  %-5.5s  %-18.18s  %-5.5s  %-7.7s%-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
      fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6,
      fmt_buf7, fmt_buf8, fmt_buf9, fmt_buf10, fmt_buf11);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  ftsym_free(sym_asn);

  return 0;

} /* format22 */

/*
 * 8.14 ToS Port Prefix aggregation
 */
int format23(struct ftio *ftio, int options)
{
  struct ftsym *sym_tcp, *sym_prot;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf1[32], fmt_buf2[32], fmt_buf3[32], fmt_buf4[32];
  char fmt_buf5[32], fmt_buf6[32], fmt_buf7[32], fmt_buf8[32];
  char fmt_buf9[32], fmt_buf10[32], fmt_buf11[32], fmt_buf12[32];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DFLOWS | FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_INPUT |
    FT_XFIELD_OUTPUT | FT_XFIELD_SRCADDR | FT_XFIELD_DSTADDR |
    FT_XFIELD_SRC_MASK | FT_XFIELD_DST_MASK | FT_XFIELD_TOS |
    FT_XFIELD_SRCPORT | FT_XFIELD_DSTPORT | FT_XFIELD_PROT)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  sym_tcp = (struct ftsym*)0L;
  sym_prot = (struct ftsym*)0L;

  if (options & FT_OPT_NAMES) {
    sym_tcp = ftsym_new(FT_PATH_SYM_TCP_PORT);
    sym_prot = ftsym_new(FT_PATH_SYM_IP_PROT);
  }

  if (options & FT_OPT_WIDE)
    puts(
"ToS  srcPrefix           dstPrefix           Pr     SrcP              DstP              input  output flows       octets      packets     duration");
  else
    puts(
"ToS  srcPrefix           dstPrefix           Pr   SrcP   DstP    input  output flows       octets      packets     duration");

  while ((rec = ftio_read(ftio))) {

    cur.dFlows = ((u_int32*)(rec+fo.dFlows));
    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.First = ((u_int32*)(rec+fo.First));
    cur.Last = ((u_int32*)(rec+fo.Last));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.input = ((u_int16*)(rec+fo.input));
    cur.output = ((u_int16*)(rec+fo.output));
    cur.srcport = ((u_int16*)(rec+fo.srcport));
    cur.dstport = ((u_int16*)(rec+fo.dstport));
    cur.src_mask = ((u_int8*)(rec+fo.src_mask));
    cur.dst_mask = ((u_int8*)(rec+fo.dst_mask));
    cur.prot = ((u_int8*)(rec+fo.prot));
    cur.tos = ((u_int8*)(rec+fo.tos));


    fmt_uint8(fmt_buf1, *cur.tos, FMT_JUST_LEFT);
    fmt_ipv4prefix(fmt_buf2, *cur.srcaddr, *cur.src_mask, FMT_JUST_LEFT);
    fmt_ipv4prefix(fmt_buf3, *cur.dstaddr, *cur.dst_mask, FMT_JUST_LEFT);

    fmt_uint16s(sym_prot, 5, fmt_buf4, (u_int16)*cur.prot, FMT_PAD_RIGHT);
    fmt_uint16s(sym_tcp, 16, fmt_buf5, (u_int16)*cur.srcport, FMT_PAD_RIGHT);
    fmt_uint16s(sym_tcp, 16, fmt_buf6, (u_int16)*cur.dstport, FMT_PAD_RIGHT);

    fmt_uint16(fmt_buf7, *cur.input, FMT_JUST_LEFT);
    fmt_uint16(fmt_buf8, *cur.output, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf9, *cur.dFlows, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf10, *cur.dOctets, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf11, *cur.dPkts, FMT_JUST_LEFT);
    fmt_uint32(fmt_buf12, *cur.Last - *cur.First, FMT_JUST_LEFT);

    if (options & FT_OPT_WIDE)
    printf("%-3.3s  %-18.18s  %-18.18s  %-5.5s  %-16.16s  %-16.16s  %-7.7s%-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
      fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6,
      fmt_buf7, fmt_buf8, fmt_buf9, fmt_buf10, fmt_buf11, fmt_buf12);
    else
    printf("%-3.3s  %-18.18s  %-18.18s  %-3.3s  %-5.5s  %-5.5s  %-7.7s%-7.7s%-12.12s%-12.12s%-12.12s%-12.12s\n",
      fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4, fmt_buf5, fmt_buf6,
      fmt_buf7, fmt_buf8, fmt_buf9, fmt_buf10, fmt_buf11, fmt_buf12);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  ftsym_free(sym_tcp);
  ftsym_free(sym_prot);

  return 0;

} /* format23 */

/*
 * function: format24
 *
 * machine-parsable 1-line-format with nearly all data
 */
int format24(struct ftio *ftio, int options)
{
  struct ftsym *sym_asn;
  struct fttime ftt;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  struct tm *tm;
  u_long active_secs, active_msecs;
  u_long bpp;
  char fmt_buf1[64], fmt_buf2[64], fmt_buf3[64], fmt_buf4[64], fmt_buf5[64], fmt_buf6[64];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_DPKTS |
    FT_XFIELD_DOCTETS | FT_XFIELD_FIRST | FT_XFIELD_LAST | FT_XFIELD_INPUT |
    FT_XFIELD_OUTPUT | FT_XFIELD_SRCADDR | FT_XFIELD_DSTADDR |
    FT_XFIELD_SRCPORT | FT_XFIELD_DSTPORT | 
    FT_XFIELD_UNIX_SECS | FT_XFIELD_UNIX_NSECS | FT_XFIELD_SYSUPTIME |
    FT_XFIELD_EXADDR | FT_XFIELD_ROUTER_SC | FT_XFIELD_TOS | FT_XFIELD_TCP_FLAGS |
    FT_XFIELD_PROT | FT_XFIELD_SRC_AS | FT_XFIELD_DST_AS)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);
  sym_asn = (struct ftsym*)0L;

  if (options & FT_OPT_NAMES) {
    sym_asn = ftsym_new(FT_PATH_SYM_ASN);
  }
                  
  puts(
    "#Sif SrcIPaddress     DIf  DstIPaddress     Pr SrcP  DstP  Pkts       Octets       StartDate  StartTime     EndDate    EndTime       ExporterAddr    RouterSrc          Active B/Pk  Ts Fl  SrcAS DstAS\n");

  while ((rec = ftio_read(ftio))) {

    cur.unix_secs = ((u_int32*)(rec+fo.unix_secs));
    cur.unix_nsecs = ((u_int32*)(rec+fo.unix_nsecs));
    cur.sysUpTime = ((u_int32*)(rec+fo.sysUpTime));
    cur.dOctets = ((u_int32*)(rec+fo.dOctets));
    cur.dPkts = ((u_int32*)(rec+fo.dPkts));
    cur.First = ((u_int32*)(rec+fo.First));
    cur.Last = ((u_int32*)(rec+fo.Last));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.exaddr = ((u_int32*)(rec+fo.exaddr));
    cur.router_sc = ((u_int32*)(rec+fo.router_sc));
    cur.input = ((u_int16*)(rec+fo.input));
    cur.output = ((u_int16*)(rec+fo.output));
    cur.srcport = ((u_int16*)(rec+fo.srcport));
    cur.dstport = ((u_int16*)(rec+fo.dstport));
    cur.prot = ((u_int8*)(rec+fo.prot));
    cur.tcp_flags = ((u_int8*)(rec+fo.tcp_flags));
    cur.tos = ((u_int8*)(rec+fo.tos));
    cur.src_as = ((u_int16*)(rec+fo.src_as));
    cur.dst_as = ((u_int16*)(rec+fo.dst_as));
            

    if (!*cur.dPkts) {
      fprintf(stderr, "Ignoring bogus flow dPkts=0\n");
      continue;
    }

    fmt_ipv4(fmt_buf1, *cur.srcaddr, FMT_PAD_RIGHT);
    fmt_ipv4(fmt_buf2, *cur.dstaddr, FMT_PAD_RIGHT);
    fmt_ipv4(fmt_buf5, *cur.exaddr, FMT_PAD_RIGHT);
    fmt_ipv4(fmt_buf6, *cur.router_sc, FMT_PAD_RIGHT); 
    fmt_uint16s(sym_asn, 18, fmt_buf3, *cur.src_as, FMT_JUST_LEFT);
    fmt_uint16s(sym_asn, 18, fmt_buf4, *cur.dst_as, FMT_JUST_LEFT);
        
    printf("%4.4x %-15.15s  %4.4x %-15.15s  %2.2x %-5u %-5u %-10lu %-10lu  ",
      (int)*cur.input, fmt_buf1, (int)*cur.output, fmt_buf2,
      (int)*cur.prot, (int)*cur.srcport, (int)*cur.dstport,
      (u_long)*cur.dPkts, (u_long)*cur.dOctets);

    ftt = ftltime(*cur.sysUpTime, *cur.unix_secs, *cur.unix_nsecs, *cur.First);
    tm = localtime((time_t*)&ftt.secs);

    printf(" %-4.4d-%-2.2d-%-2.2d %-2.2d:%-2.2d:%-2.2d.%-3.3lu  ",
      (int)tm->tm_year+1900, (int)tm->tm_mon+1, (int)tm->tm_mday, (int)tm->tm_hour,
      (int)tm->tm_min, (int)tm->tm_sec, (u_long)ftt.msecs);

    ftt = ftltime(*cur.sysUpTime, *cur.unix_secs, *cur.unix_nsecs, *cur.Last);
    tm = localtime((time_t*)&ftt.secs);

    active_secs = (*cur.Last - *cur.First) / 1000;
    active_msecs = (*cur.Last - *cur.First) % 1000;

    bpp = *cur.dOctets / *cur.dPkts;

    printf("%-4.4d-%-2.2d-%-2.2d %-2.2d:%-2.2d:%-2.2d.%-3.3lu  %-15.15s %-15.15s %5lu.%-3.3lu %-4lu  %2.2x %2.2x",
      (int)tm->tm_year+1900, (int)tm->tm_mon+1, (int)tm->tm_mday, (int)tm->tm_hour,
      (int)tm->tm_min, (int)tm->tm_sec, (u_long)ftt.msecs, fmt_buf5, fmt_buf6,
      active_secs, active_msecs, bpp, (int)*cur.tos,
      (int)*cur.tcp_flags);
    printf("  %-5.5s %-5.5s\n",
      fmt_buf3, fmt_buf4);

    if (options & FT_OPT_NOBUF)
      fflush(stdout);

  } /* while */

  return 0;

} /* format24 */


void usage(void) {

  fprintf(stderr, "Usage: flow-print [-hlnpw] [-d debug_level] [-f format]\n");
  fprintf(stderr, "\n%s version %s: built by %s\n", PACKAGE, VERSION, FT_PROG_BUILD);

} /* usage */

