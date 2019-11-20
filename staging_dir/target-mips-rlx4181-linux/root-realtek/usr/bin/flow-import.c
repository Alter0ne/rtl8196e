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
 *      $Id: flow-import.c,v 1.14 2003/11/11 16:53:57 maf Exp $
 */

#include "ftconfig.h"
#include <ftlib.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/uio.h>
#include <ctype.h>
#include <unistd.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <time.h>
#include <fcntl.h>

#if HAVE_STRINGS_H
 #include <strings.h>
#endif

#if HAVE_STRING_H
  #include <string.h>
#endif

#if !HAVE_STRSEP
  char    *strsep (char **, const char *);
#endif

#include "ftbuild.h"
#include "cflowd.h"

struct options {
  struct ftver ftv;
  struct ftset ftset;
  u_int64 ft_mask;
  int set_format;
  u_long records;
};

struct jump {
    int (*where)(struct ftio *ftio, struct options *opt);
};

int format0(struct ftio *ftio, struct options *opt);
int format1(struct ftio *ftio, struct options *opt);
int format2(struct ftio *ftio, struct options *opt);
int format_NFCollector1(struct ftio *ftio, struct options *opt);
int format4(struct ftio *ftio, struct options *opt);

void usage(void);

struct jump format[] = {{format0}, {format1}, {format2},
			{ format_NFCollector1 }, {format4} };

static u_int64 vXmask[] = {
  0,
  FT_XFIELD_V1_MASK,
  0, 0, 0,
  FT_XFIELD_V5_MASK,
  FT_XFIELD_V6_MASK,
  FT_XFIELD_V7_MASK };

static u_int64 v8mask[] = {
  0,
  FT_XFIELD_V8_1_MASK,
  FT_XFIELD_V8_2_MASK,
  FT_XFIELD_V8_3_MASK,
  FT_XFIELD_V8_4_MASK,
  FT_XFIELD_V8_5_MASK,
  FT_XFIELD_V8_6_MASK,
  FT_XFIELD_V8_7_MASK,
  FT_XFIELD_V8_8_MASK,
  FT_XFIELD_V8_9_MASK,
  FT_XFIELD_V8_10_MASK,
  FT_XFIELD_V8_11_MASK,
  FT_XFIELD_V8_12_MASK,
  FT_XFIELD_V8_13_MASK,
  FT_XFIELD_V8_14_MASK };

#define NFORMATS 5 /* nformats + 1 */

#if HAVE_LL_STRTOUL
  #define strtoull strtoul
#endif /* HAVE_LL_STRTOULL */

int main(int argc, char **argv)
{
  struct ftio ftio;
  struct options opt;
  int i, n, format_index, ret;
  unsigned int v1, v2;
  int debug;

  /* init fterr */
  fterr_setid(argv[0]);

  debug = 0;
  format_index = 0;
  bzero(&opt, sizeof opt);

  /* defaults + no compression */
  ftset_init(&opt.ftset, 0);

  opt.ft_mask = 0xFFFFFFFFFFFFFFFFLL;

  while ((i = getopt(argc, argv, "b:d:f:h?m:V:z:")) != -1)

    switch (i) {

    case 'b': /* output byte order */
      if (!strcasecmp(optarg, "little"))
        opt.ftset.byte_order = FT_HEADER_LITTLE_ENDIAN;
      else if (!strcasecmp(optarg, "big"))
        opt.ftset.byte_order = FT_HEADER_BIG_ENDIAN;
      else
        fterr_errx(1, "expecting \"big\" or \"little\"");
      break;

    case 'd': /* debug */
      debug = atoi(optarg);
      break;

    case 'f': /* format */
      format_index = atoi(optarg);
      break;

    case 'h': /* help */
    case '?':
      usage();
      exit (0);
      break;

    case 'm': /* mask */
      if (isalpha((int)optarg[0])) {
        if (ftxfield_parse(optarg, &opt.ft_mask) < 0)
          fterr_errx(1, "ftxfield_parse(): failed");
      } else {
        opt.ft_mask = strtoull(optarg, (char **)0L, 0);
      }
      opt.set_format = 1;
      break;

   case 'V': /* PDU version */
      n = sscanf(optarg, "%u.%u", &v1, &v2);
      if (n == 1) {
        opt.ftv.s_version = FT_IO_SVERSION;
        opt.ftv.d_version = v1;
        opt.ftv.set = 1;
      } else if (n == 2) {
        opt.ftv.s_version = FT_IO_SVERSION;
        opt.ftv.d_version = v1;
        opt.ftv.agg_method = v2;
        opt.ftv.agg_version = 2;
        opt.ftv.set = 1;
      } else
        fterr_errx(1, "Version scan failed");
      break;

    case 'z': /* compress level */
      opt.ftset.z_level = atoi(optarg);
      if ((opt.ftset.z_level < 0) || (opt.ftset.z_level > 9))
        fterr_errx(1, "Compression level must be between 0 and 9");
      break;

    default:
      usage();
      exit (1);
      break;

    } /* switch */

  if (argc - optind)
    fterr_errx(1, "Extra arguments starting with %s.", argv[optind]);

  if (format_index >= NFORMATS)
    fterr_errx(1, "No such format, %d", format_index);

  if (!opt.ftv.set)
    fterr_errx(1, "Must specify export version to store flows");

  /* output to stdout */
  if (ftio_init(&ftio, 1, FT_IO_FLAG_WRITE |
    ((opt.ftset.z_level) ? FT_IO_FLAG_ZINIT : 0) ) < 0)
    fterr_errx(1, "ftio_init(): failed");
    
  ftio_set_comment(&ftio, "flow-import");
  ftio_set_cap_hostname(&ftio, "flow-import");
  ftio_set_byte_order(&ftio, opt.ftset.byte_order);
  ftio_set_z_level(&ftio, opt.ftset.z_level);
  ftio_set_streaming(&ftio, 1);
  ftio_set_debug(&ftio, debug);

  if (ftio_set_ver(&ftio, &opt.ftv) < 0)
    fterr_errx(1, "ftio_set_ver(): failed");

  /* header first */
  if (ftio_write_header(&ftio) < 0)
    fterr_errx(1, "ftio_write_header(): failed");

  ret = format[format_index].where(&ftio, &opt);

  if (ftio_close(&ftio) < 0)
    fterr_errx(1, "ftio_close(): failed");

  fprintf(stderr, "%s: Imported %lu records.\n", argv[0], opt.records);

  return ret;

} /* main */

/*
 * function: format0
 *
 * import from cflowd files
*/
int format0(struct ftio *ftio, struct options *opt)
{
  struct fts3rec_offsets fo;
  size_t rlen;
  u_char buf[FT_IO_MAXREC];
  char *rec;
  u_int32 ui32, Start;
  u_int16 ui16;
  u_int8 ui8;
  u_int32 cfdmask, index;
  int ret;

  rec = (char*)&buf;

  fts3rec_compute_offsets(&fo, &opt->ftv);

  switch (opt->ftv.d_version) {

    case 1:
      cfdmask = CF_INDEX_V1_MASK;
      break;

    case 5:
      cfdmask = CF_INDEX_V5_MASK;
      break;

    case 6:
      cfdmask = CF_INDEX_V6_MASK;
      break;

    case 7:
      cfdmask = CF_INDEX_V7_MASK;
      break;

    case 8:

      switch (opt->ftv.agg_method) {

        case 1:
          cfdmask = CF_INDEX_V8_1_MASK;
          break;

        case 2:
          cfdmask = CF_INDEX_V8_2_MASK;
          break;

        case 3:
          cfdmask = CF_INDEX_V8_3_MASK;
          break;

        case 4:
          cfdmask = CF_INDEX_V8_4_MASK;
          break;

        case 5:
          cfdmask = CF_INDEX_V8_5_MASK;
          break;

        case 6:
          cfdmask = CF_INDEX_V8_6_MASK;
          break;

        case 7:
          cfdmask = CF_INDEX_V8_7_MASK;
          break;

        case 8:
          cfdmask = CF_INDEX_V8_8_MASK;
          break;

        case 9:
          cfdmask = CF_INDEX_V8_9_MASK;
          break;

        case 10:
          cfdmask = CF_INDEX_V8_10_MASK;
          break;

        case 11:
          cfdmask = CF_INDEX_V8_11_MASK;
          break;

        case 12:
          cfdmask = CF_INDEX_V8_12_MASK;
          break;

        case 13:
          cfdmask = CF_INDEX_V8_13_MASK;
          break;

        case 14:
          cfdmask = CF_INDEX_V8_14_MASK;
          break;

        default:
          fterr_warnx("Unsupported export version");
          return -1;

       } /* switch */
       break;

    default:
      fterr_warnx("Unsupported export version");
      return -1;

  } /* switch */

  ret = -1;
  while (!(feof(stdin))) {

    ret = -1;

    bzero(rec, FT_IO_MAXREC);
    Start = 0;

    if ((rlen = fread(&index, sizeof (index), 1, stdin) != 1))
      goto done;
    index = ntohl(index);

    if (index & CF_ROUTERMASK) {
      if ((rlen = fread(&ui32, sizeof (ui32), 1, stdin) != 1))
        goto done;
      ui32 = ntohl(ui32);
      if (cfdmask & CF_ROUTERMASK)
        *((u_int32*)(rec+fo.exaddr)) = ui32;
    }

    if (index & CF_SRCIPADDRMASK) {
      if ((rlen = fread(&ui32, sizeof (ui32), 1, stdin) != 1))
        goto done;
      ui32 = ntohl(ui32);
      if (cfdmask & CF_SRCIPADDRMASK)
        *((u_int32*)(rec+fo.srcaddr)) = ui32;
    }

    if (index & CF_DSTIPADDRMASK) {
      if ((rlen = fread(&ui32, sizeof (ui32), 1, stdin) != 1))
        goto done;
      ui32 = ntohl(ui32);
      if (cfdmask & CF_DSTIPADDRMASK)
        *((u_int32*)(rec+fo.dstaddr)) = ui32;
    }

    if (index & CF_INPUTIFINDEXMASK) {
      if ((rlen = fread(&ui16, sizeof (ui16), 1, stdin) != 1))
        goto done;
      ui16 = ntohs(ui16);
      if (cfdmask & CF_DSTIPADDRMASK)
        *((u_int16*)(rec+fo.input)) = ui16;
    }

    if (index & CF_OUTPUTIFINDEXMASK) {
      if ((rlen = fread(&ui16, sizeof (ui16), 1, stdin) != 1))
        goto done;
      ui16 = ntohs(ui16);
      if (cfdmask & CF_OUTPUTIFINDEXMASK)
        *((u_int16*)(rec+fo.output)) = ui16;
    }

    if (index & CF_SRCPORTMASK) {
      if ((rlen = fread(&ui16, sizeof (ui16), 1, stdin) != 1))
        goto done;
      ui16 = ntohs(ui16);
      if (cfdmask & CF_SRCPORTMASK)
        *((u_int16*)(rec+fo.srcport)) = ui16;
    }

    if (index & CF_DSTPORTMASK) {
      if ((rlen = fread(&ui16, sizeof (ui16), 1, stdin) != 1))
        goto done;
      ui16 = ntohs(ui16);
      if (cfdmask & CF_DSTPORTMASK)
        *((u_int16*)(rec+fo.dstport)) = ui16;
    }

    if (index & CF_PKTSMASK) {
      if ((rlen = fread(&ui32, sizeof (ui32), 1, stdin) != 1))
        goto done;
      ui32 = ntohl(ui32);
      if (cfdmask & CF_PKTSMASK)
        *((u_int32*)(rec+fo.dPkts)) = ui32;
    }

    if (index & CF_BYTESMASK) {
      if ((rlen = fread(&ui32, sizeof (ui32), 1, stdin) != 1))
        goto done;
      ui32 = ntohl(ui32);
      if (cfdmask & CF_BYTESMASK)
        *((u_int32*)(rec+fo.dOctets)) = ui32;
    }

    if (index & CF_IPNEXTHOPMASK) {
      if ((rlen = fread(&ui32, sizeof (ui32), 1, stdin) != 1))
        goto done;
      ui32 = ntohl(ui32);
      if (cfdmask & CF_IPNEXTHOPMASK)
        *((u_int32*)(rec+fo.nexthop)) = ui32;
    }

    if (index & CF_STARTTIMEMASK) {
      if ((rlen = fread(&ui32, sizeof (ui32), 1, stdin) != 1))
        goto done;
      ui32 = ntohl(ui32);
      Start = ui32;
      if (cfdmask & CF_STARTTIMEMASK)
        *((u_int32*)(rec+fo.unix_secs)) = ui32;
    }

    if (index & CF_ENDTIMEMASK) {
      if ((rlen = fread(&ui32, sizeof (ui32), 1, stdin) != 1))
        goto done;
      ui32 = ntohl(ui32);
      if (cfdmask & CF_ENDTIMEMASK) {
        if (Start)
          *((u_int32*)(rec+fo.Last)) = (ui32 - Start) * 1000;
        else
          *((u_int32*)(rec+fo.unix_secs)) = ui32;
        
      }
    }

    if (index & CF_PROTOCOLMASK) {
      if ((rlen = fread(&ui8, sizeof (ui8), 1, stdin) != 1))
        goto done;
      if (cfdmask & CF_PROTOCOLMASK)
        *((u_int8*)(rec+fo.prot)) = ui8;
    }

    if (index & CF_TOSMASK) {
      if ((rlen = fread(&ui8, sizeof (ui8), 1, stdin) != 1))
        goto done;
      if (cfdmask & CF_TOSMASK)
        *((u_int8*)(rec+fo.tos)) = ui8;
    }

    if (index & CF_SRCASMASK) {
      if ((rlen = fread(&ui16, sizeof (ui16), 1, stdin) != 1))
        goto done;
      ui16 = ntohs(ui16);
      if (cfdmask & CF_SRCASMASK)
        *((u_int16*)(rec+fo.src_as)) = ui16;
    }

    if (index & CF_DSTASMASK) {
      if ((rlen = fread(&ui16, sizeof (ui16), 1, stdin) != 1))
        goto done;
      ui16 = ntohs(ui16);
      if (cfdmask & CF_DSTASMASK)
        *((u_int16*)(rec+fo.dst_as)) = ui16;
    }

    if (index & CF_SRCMASKLENMASK) {
      if ((rlen = fread(&ui8, sizeof (ui8), 1, stdin) != 1))
        goto done;
      if (cfdmask & CF_SRCMASKLENMASK)
        *((u_int8*)(rec+fo.src_mask)) = ui8;
    }

    if (index & CF_DSTMASKLENMASK) {
      if ((rlen = fread(&ui8, sizeof (ui8), 1, stdin) != 1))
        goto done;
      if (cfdmask & CF_DSTMASKLENMASK)
        *((u_int8*)(rec+fo.dst_mask)) = ui8;
    }

    if (index & CF_TCPFLAGSMASK) {
      if ((rlen = fread(&ui8, sizeof (ui8), 1, stdin) != 1))
        goto done;
      if (cfdmask & CF_TCPFLAGSMASK)
        *((u_int8*)(rec+fo.tcp_flags)) = ui8;
    }

    if (index & CF_INPUTENCAPMASK) {
      if ((rlen = fread(&ui8, sizeof (ui8), 1, stdin) != 1))
        goto done;
      if (cfdmask & CF_INPUTENCAPMASK)
        *((u_int8*)(rec+fo.in_encaps)) = ui8;
    }

    if (index & CF_OUTPUTENCAPMASK) {
      if ((rlen = fread(&ui8, sizeof (ui8), 1, stdin) != 1))
        goto done;
      if (cfdmask & CF_OUTPUTENCAPMASK)
        *((u_int8*)(rec+fo.out_encaps)) = ui8;
    }

    if (index & CF_PEERNEXTHOPMASK) {
      if ((rlen = fread(&ui32, sizeof (ui32), 1, stdin) != 1))
        goto done;
      ui32 = ntohl(ui32);
      if (cfdmask & CF_PEERNEXTHOPMASK)
        *((u_int32*)(rec+fo.peer_nexthop)) = ui32;
    }

    if (index & CF_ENGINETYPEMASK) {
      if ((rlen = fread(&ui8, sizeof (ui8), 1, stdin) != 1))
        goto done;
      if (cfdmask & CF_ENGINETYPEMASK)
        *((u_int8*)(rec+fo.engine_type)) = ui8;
    }

    if (index & CF_ENGINEIDMASK) {
      if ((rlen = fread(&ui8, sizeof (ui8), 1, stdin) != 1))
        goto done;
      if (cfdmask & CF_ENGINEIDMASK)
        *((u_int8*)(rec+fo.engine_id)) = ui8;
    }


    if (ftio_write(ftio, rec) < 0) {
      fterr_warnx("ftio_write(): failed");
      return -1;
    }

    ++opt->records;

    ret = 0;

  }

done:

  return ret;

} /* format0 */

int format1(struct ftio *ftio, struct options *opt)
{
  fterr_warnx("Not implemented");
  return -1;
} /* format1 */

/*
 * function: format2
 *
 * import from ASCII CSV format
*/
int format2(struct ftio *ftio, struct options *opt)
{
  struct fts3rec_offsets fo;
  u_char buf[FT_IO_MAXREC];
  char inbuf[1024], *inbufp, *field;
  char *rec, *c;
  u_int64 dmask, inmask;
  int ret;

  rec = (char*)&buf;

  fts3rec_compute_offsets(&fo, &opt->ftv);

  dmask = 0;
  dmask = ( opt->ftv.d_version == 8 ? 
	    v8mask[opt->ftv.agg_method] : vXmask[opt->ftv.d_version] );
  if(!dmask) {
    fterr_warnx("unsupported export version");
    return -1;
  }

  /* no user specified format then default to one appropriate for version */
  if (!opt->set_format)
    inmask = dmask;
  else
    inmask = opt->ft_mask;

  ret = -1;
  while (!(feof(stdin))) {

    ret = 0;

    if (!fgets(inbuf, 1024, stdin))
      goto done;

    for (field = inbuf; *field; ++field)
      if (*field == '\n') {
        *field = 0;
        break;
      }

    /* inline mask? */
    if (!opt->records && inbuf[0] == '#' && inbuf[1] == ':') {

      if (ftxfield_parse(inbuf+2, &inmask) < 0) {
        fterr_warnx("ftxfield_parse(): failed");
        return -1;
      }

    }

    for (c = inbuf; *c && isspace(*c); ++c);
    if (*c == '#')
      continue;

    ret = -1;

    bzero(rec, FT_IO_MAXREC);

    inbufp = inbuf;

    if (inmask & FT_XFIELD_UNIX_SECS) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_UNIX_SECS))
        *((u_int32*)(rec+fo.unix_secs)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_UNIX_NSECS) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_UNIX_NSECS))
        *((u_int32*)(rec+fo.unix_nsecs)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_SYSUPTIME) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_SYSUPTIME))
        *((u_int32*)(rec+fo.sysUpTime)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_EXADDR) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_EXADDR))
        *((u_int32*)(rec+fo.exaddr)) = scan_ip(field);
    }

    if (inmask & FT_XFIELD_DFLOWS) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_DFLOWS))
        *((u_int32*)(rec+fo.dFlows)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_DPKTS) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_DPKTS))
        *((u_int32*)(rec+fo.dPkts)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_DOCTETS) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_DOCTETS))
        *((u_int32*)(rec+fo.dOctets)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_FIRST) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_FIRST))
        *((u_int32*)(rec+fo.First)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_LAST) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_LAST))
        *((u_int32*)(rec+fo.Last)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_ENGINE_TYPE) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_ENGINE_TYPE))
        *((u_int8*)(rec+fo.engine_type)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_ENGINE_ID) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_ENGINE_ID))
        *((u_int8*)(rec+fo.engine_id)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_SRCADDR) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_SRCADDR))
        *((u_int32*)(rec+fo.srcaddr)) = scan_ip(field);
    }

    if (inmask & FT_XFIELD_DSTADDR) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_DSTADDR))
        *((u_int32*)(rec+fo.dstaddr)) = scan_ip(field);
    }

    if (inmask & FT_XFIELD_NEXTHOP) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_NEXTHOP))
        *((u_int32*)(rec+fo.nexthop)) = scan_ip(field);
    }

    if (inmask & FT_XFIELD_INPUT) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_INPUT))
        *((u_int16*)(rec+fo.input)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_OUTPUT) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_OUTPUT))
        *((u_int16*)(rec+fo.output)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_SRCPORT) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_SRCPORT))
        *((u_int16*)(rec+fo.srcport)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_DSTPORT) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_DSTPORT))
        *((u_int16*)(rec+fo.dstport)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_PROT) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_PROT))
        *((u_int8*)(rec+fo.prot)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_TOS) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_TOS))
        *((u_int8*)(rec+fo.tos)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_TCP_FLAGS) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_TCP_FLAGS))
        *((u_int8*)(rec+fo.tcp_flags)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_SRC_MASK) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_SRC_MASK))
        *((u_int8*)(rec+fo.src_mask)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_DST_MASK) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_DST_MASK))
        *((u_int8*)(rec+fo.dst_mask)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_SRC_AS) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_SRC_AS))
        *((u_int16*)(rec+fo.src_as)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_DST_AS) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_DST_AS))
        *((u_int16*)(rec+fo.dst_as)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_IN_ENCAPS) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_IN_ENCAPS))
        *((u_int8*)(rec+fo.in_encaps)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_OUT_ENCAPS) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_OUT_ENCAPS))
        *((u_int8*)(rec+fo.out_encaps)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_PEER_NEXTHOP) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_PEER_NEXTHOP))
        *((u_int32*)(rec+fo.peer_nexthop)) = scan_ip(field);
    }

    if (inmask & FT_XFIELD_ROUTER_SC) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_ROUTER_SC))
        *((u_int32*)(rec+fo.router_sc)) = scan_ip(field);
    }

    if (inmask & FT_XFIELD_MARKED_TOS) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_MARKED_TOS))
        *((u_int8*)(rec+fo.marked_tos)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_EXTRA_PKTS) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_EXTRA_PKTS))
        *((u_int32*)(rec+fo.extra_pkts)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_SRC_TAG) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_SRC_TAG))
        *((u_int32*)(rec+fo.src_tag)) = strtoul(field, (char **)0L, 0);
    }

    if (inmask & FT_XFIELD_DST_TAG) {
      field = strsep(&inbufp, ",");
      if (field && (dmask & FT_XFIELD_DST_TAG))
        *((u_int32*)(rec+fo.dst_tag)) = strtoul(field, (char **)0L, 0);
    }
      
    if (ftio_write(ftio, rec) < 0) {
      fterr_warnx("ftio_write(): failed");
      goto done;
    }

    ++opt->records;

  } /* while */

done:

  return ret;

} /* format2 */

/*
 * functio: format_NFCollector1
 * 
 * import from Cisco NFCollector v1 ascii files
 */

/* Break line into fields */
static int ascii_fields(char **fp,int maxfp,char *inbuf,const char *delim)
{
  int n=0;
  while((*fp = strsep(&inbuf,delim)) != NULL && n < maxfp) {
    n++;
    fp++;
  }
  return ( n < maxfp ? n : 0 );
}

/* Map NFCollector recordtypes to flow-tools datatypes */
typedef enum { TYPE_IPV4, TYPE_16B, TYPE_32B, TYPE_8B , TYPE_DISCARD,
	       TYPE_LAST } cvt_t;

struct for2nat_st {
  u_int64 mask;
  cvt_t type;
  int offset;
};
struct nfcollector2flowtools {
  char *name;
  struct for2nat_st fcv[24];
};

struct nfcollector2flowtools NFC2ft[] = {
  { "AGGREGATION CallRecord", 
    { { FT_XFIELD_SRCADDR, TYPE_IPV4, offsetof(struct fts3rec_offsets,srcaddr) },
      { FT_XFIELD_DSTADDR, TYPE_IPV4, offsetof(struct fts3rec_offsets,dstaddr) },
      { FT_XFIELD_SRCPORT, TYPE_16B,  offsetof(struct fts3rec_offsets,srcport) },
      { FT_XFIELD_DSTPORT, TYPE_16B,  offsetof(struct fts3rec_offsets,dstport) },
      { FT_XFIELD_PROT   , TYPE_8B ,  offsetof(struct fts3rec_offsets,prot) },
      { FT_XFIELD_TOS    , TYPE_16B,  offsetof(struct fts3rec_offsets,tos) },
      { FT_XFIELD_DPKTS,   TYPE_32B,  offsetof(struct fts3rec_offsets,dPkts) },
      { FT_XFIELD_DOCTETS, TYPE_32B,  offsetof(struct fts3rec_offsets,dOctets) },
      { FT_XFIELD_DFLOWS,  TYPE_32B,  offsetof(struct fts3rec_offsets,dFlows) },
      /* We have pretty darn good uptime */
      { FT_XFIELD_FIRST, TYPE_32B,offsetof(struct fts3rec_offsets,First) },
      { FT_XFIELD_LAST   , TYPE_32B,  offsetof(struct fts3rec_offsets,Last) },
      { 0,                 TYPE_DISCARD, 0 },
      { 0,                 TYPE_LAST, 0 } }
  },
  { "AGGREGATION HostMatrix",
    { { FT_XFIELD_SRCADDR, TYPE_IPV4, offsetof(struct fts3rec_offsets,srcaddr) },
      { FT_XFIELD_DSTADDR, TYPE_IPV4, offsetof(struct fts3rec_offsets,dstaddr) },
      { FT_XFIELD_DPKTS,   TYPE_32B,  offsetof(struct fts3rec_offsets,dPkts) },
      { FT_XFIELD_DOCTETS, TYPE_32B,  offsetof(struct fts3rec_offsets,dOctets) },
      { FT_XFIELD_DFLOWS,  TYPE_32B,  offsetof(struct fts3rec_offsets,dFlows) },
      { 0,                 TYPE_LAST, 0 } }
  },
  { "AGGREGATION SourceNode",
    { { FT_XFIELD_SRCADDR, TYPE_IPV4, offsetof(struct fts3rec_offsets,srcaddr) },
      { FT_XFIELD_DPKTS,   TYPE_32B,  offsetof(struct fts3rec_offsets,dPkts) },
      { FT_XFIELD_DOCTETS, TYPE_32B,  offsetof(struct fts3rec_offsets,dOctets) },
      { FT_XFIELD_DFLOWS,  TYPE_32B,  offsetof(struct fts3rec_offsets,dFlows) },
      { 0,                 TYPE_LAST } }
  },
  { "AGGREGATION DestNode",
    { { FT_XFIELD_DSTADDR, TYPE_IPV4, offsetof(struct fts3rec_offsets,dstaddr) },
      { FT_XFIELD_DPKTS,   TYPE_32B,  offsetof(struct fts3rec_offsets,dPkts) },
      { FT_XFIELD_DOCTETS, TYPE_32B,  offsetof(struct fts3rec_offsets,dOctets) },
      { FT_XFIELD_DFLOWS,  TYPE_32B,  offsetof(struct fts3rec_offsets,dFlows) },
      { 0,                 TYPE_LAST } }
  },
    
  { NULL }
};


int format_NFCollector1(struct ftio *ftio, struct options *opt)
{
  struct for2nat_st *fcv;
  struct fts3rec_offsets fo;
  u_char buf[FT_IO_MAXREC];
  char inbuf[1024];
  char *rec;
  char *fields[20];
  u_int64 dmask, imask;
  int ret;
  int debug=ftio_get_debug(ftio);

  rec = (char*)&buf;

  fts3rec_compute_offsets(&fo, &opt->ftv);

  dmask=0;
  if(opt->ftv.d_version == 8)
    dmask = ( opt->ftv.agg_method > 0 && opt->ftv.agg_method < sizeof(v8mask)-1 ?
	      v8mask[opt->ftv.agg_method] : 0);
  else
    dmask = ( opt->ftv.d_version > 0 && opt->ftv.d_version < sizeof(vXmask)-1 ?
	      vXmask[opt->ftv.d_version] : 0);
  if(!dmask) {
    fterr_warnx("unsupported export version");
    return -1;
  }

  /* First line has format descriptor */
  if(fgets(inbuf,sizeof(inbuf),stdin) == NULL) {
    fterr_warnx("No header");
    return -1;
  }
  if(ascii_fields(fields,sizeof(fields),inbuf,"|") != 9) {
    /* XXX complain about unregocnized format */
    fterr_warnx("Unregconized format line");
    return -1;
  }
  if(strcmp("FORMAT A",fields[1]) != 0) {
    fterr_warnx("Unregocnized format \"%s\"",fields[1]);
    return -1;
  }
  fcv=NULL;
  for(ret=0;NFC2ft[ret].name != NULL;ret++) {
    if(strcmp(NFC2ft[ret].name,fields[2]) == 0) {
      fcv=(NFC2ft[ret].fcv);
      if(debug)
	fprintf(stderr,"Input format %s\n",fields[2]);
      break;
    }
  } 
  if(fcv == NULL) {
    fterr_warnx("Unsupported aggrecation scheme \"%s\"",fields[2]);
    return -1;
  }
  /* Compute imask from conversion table */
  imask=0;
  for(ret = 0; (fcv+ret)->type != TYPE_LAST; ret++) {
    imask = imask | (fcv+ret)->mask;
  }
  /* check compatibility of imask & dmask XXX */
  if((imask & dmask) == 0) {
    fterr_warnx("Incompatible input and destination (no common fields)");
    return -1;
  }

  ret = 1;
  while (!(feof(stdin))) {
    char **fp;
    u_int32 val=0;
    int off=0,n,i;
    if(fgets(inbuf,sizeof(inbuf),stdin) == NULL)
      continue;
    ++ret;
    n=ascii_fields(fields,sizeof(fields),inbuf,"|");
    fp=fields;
    bzero(rec,sizeof(buf));
    for(i=0;i < n && *fp;i++) {
      if(dmask & fcv[i].mask) {
	off=*(short *)(((char *)&(fo))+fcv[i].offset);
	switch(fcv[i].type) {
	case TYPE_32B:
	case TYPE_16B:
	case TYPE_8B:
	  val = strtoul(*fp,(char **)0L,10);
	  break;
	case TYPE_IPV4:
	  val = scan_ip(*fp);
	  /* Fall thru */
	case TYPE_DISCARD:
	case TYPE_LAST:
	default:
	  break;
	}
	switch(fcv[i].type) {
	case TYPE_16B:
	  *((u_int16*)(rec+off)) = (u_int16)val;
	  break;
	case TYPE_IPV4:
	case TYPE_32B:
	  *((u_int32*)(rec+off)) = (u_int32)val;
	  break;
	case TYPE_8B:
	  *((u_int8*)(rec+off)) = (u_int8)val;
	case TYPE_DISCARD:
	case TYPE_LAST:
	  break; /* Make sun Cpro happy */
	}
      }
      fp++;
    }
    /* XXX Something sane here */
    if(fp && fcv[i].type == TYPE_LAST) {
      if(ftio_write(ftio,rec) < 0) {
	fterr_warnx("ftio_write(): failed");
	break;
      }
      ++opt->records;
    } else
      fterr_warnx("Broken record at line %d (not output)",ret);
  }
  return 1;
}

/*
 * function: format4
 *
 * raw packet format
*/
int format4(struct ftio *ftio, struct options *opt)
{
  struct ftpdu_header ftheader, ftheader2;
  struct ftpdu ftpdu;
  struct ftseq ftseq;
  size_t rlen, len;
  void (*xlate)(void *in_rec, void *out_rec);
  int ret, n, i, offset;
  char xl_rec[FT_IO_MAXREC], *out_rec;

  bzero (&ftpdu, sizeof ftpdu);
  bzero (&ftseq, sizeof ftseq);
  xlate = (void*)0L;

  while (!(feof(stdin))) {

    ret = -1;

    /* find out the PDU version and flow count */
    if ((rlen = fread(&ftheader, sizeof (ftheader), 1, stdin) != 1))
      goto done;

    /* copy to swap bytes */
    bcopy(&ftheader, &ftheader2, sizeof ftheader);

#if BYTE_ORDER == LITTLE_ENDIAN
  SWAPINT16(ftheader2.version);
  SWAPINT16(ftheader2.count);
#endif /* LITTLE_ENDIAN */

    switch (ftheader2.version) {

      case 1:
        len = ftheader2.count*sizeof(struct ftrec_v1);
        /* v1 does not have sequence# and engine* */
        len -= 8;
        break;

      case 5:
        len = ftheader2.count*sizeof(struct ftrec_v5);
        break;

      case 6:
        len = ftheader2.count*sizeof(struct ftrec_v6);
        break;

      case 7:
        len = ftheader2.count*sizeof(struct ftrec_v7);
        break;

      case 8:

        switch (ftheader2.aggregation) {

          case 1:
            len = ftheader2.count*sizeof(struct ftrec_v8_1);
            break;

          case 2:
            len = ftheader2.count*sizeof(struct ftrec_v8_2);
            break;

          case 3:
            len = ftheader2.count*sizeof(struct ftrec_v8_3);
            break;

          case 4:
            len = ftheader2.count*sizeof(struct ftrec_v8_4);
            break;

          case 5:
            len = ftheader2.count*sizeof(struct ftrec_v8_5);
            break;

          case 6:
            len = ftheader2.count*sizeof(struct ftrec_v8_6);
            break;

          case 7:
            len = ftheader2.count*sizeof(struct ftrec_v8_7);
            break;

          case 8:
            len = ftheader2.count*sizeof(struct ftrec_v8_8);
            break;

          case 9:
            len = ftheader2.count*sizeof(struct ftrec_v8_9);
            break;

          case 10:
            len = ftheader2.count*sizeof(struct ftrec_v8_10);
            break;

          case 11:
            len = ftheader2.count*sizeof(struct ftrec_v8_11);
            break;

          case 12:
            len = ftheader2.count*sizeof(struct ftrec_v8_12);
            break;

          case 13:
            len = ftheader2.count*sizeof(struct ftrec_v8_13);
            break;

          case 14:
            len = ftheader2.count*sizeof(struct ftrec_v8_14);
            break;

          default:
            fterr_errx(1, "Unrecognized PDU version: %d aggregation %d.",
              ftheader2.version, ftheader2.aggregation);
            break;
        } /* switch */
        break;

      default:
        fterr_errx(1, "Unrecognized PDU version: %d.", ftheader2.version);
        break;

    } /* switch */

    ftpdu.bused = len + sizeof (ftheader);

    /* read in the rest of the PDU */
    if ((rlen = fread(ftpdu.buf+sizeof (ftheader), len, 1, stdin) != 1)) {
      fterr_errx(1, "fread(): failed - expecting to read %d bytes.", len);
      goto done;
    }

    /* copy in the read header */
    bcopy(&ftheader, &ftpdu.buf, sizeof (ftheader));

    /* verify integrity, get version */
    if (ftpdu_verify(&ftpdu) < 0) {
      fterr_warnx("ftpdu_verify(): failed.");
      goto done;
    }

    /* first flow or no configured destination? */
    if (!opt->ftv.set) {

      bcopy(&ftpdu.ftv, &opt->ftv, sizeof ftpdu.ftv);
      opt->ftv.set = 1;

    } else {

      /* translation among v8 aggregation methods not possible */
      if ((opt->ftv.d_version == 8) &&
        ((opt->ftv.agg_method != ftpdu.ftv.agg_method)
        || (opt->ftv.agg_version != ftpdu.ftv.agg_version))) {

        fterr_errx(1, "Unexpected PDU: oagg=%d agg=%d over=%d ver=%d",
          opt->ftv.agg_method, ftpdu.ftv.agg_method,
          opt->ftv.agg_version, ftpdu.ftv.agg_version);
      }

      if (opt->ftv.d_version != ftpdu.ftv.d_version)
        xlate = ftrec_xlate_func(&ftpdu.ftv, &opt->ftv);

    }

    /* verify sequence # */
    if (ftpdu_check_seq(&ftpdu, &ftseq) < 0) {

      fterr_warnx("ftpdu_seq_check(): expecting=%lu received=%lu lost=%lu",
        (u_long)ftseq.seq_exp,
        (u_long)ftseq.seq_rcv,
        (u_long)ftseq.seq_lost);

    }

    /* decode */
    ftpdu.ftd.byte_order = opt->ftset.byte_order;

    n = fts3rec_pdu_decode(&ftpdu);

    /* update the exporter stats */
    opt->records += n;

    /* write */
    for (i = 0, offset = 0; i < ftpdu.ftd.count; 
      ++i, offset += ftpdu.ftd.rec_size) {

      if (xlate) {

        xlate(ftpdu.ftd.buf+offset, &xl_rec);
        out_rec = (char*)&xl_rec;

      } else {

        out_rec = (char*)ftpdu.ftd.buf+offset;

      }

      if ((n = ftio_write(ftio, out_rec)) < 0)
        fterr_errx(1, "ftio_write(): failed");

    }

  } /* while */

done:

  return ret;

} /* format4 */

void usage(void) {

  fprintf(stderr, "Usage: flow-import [-h] [-b big|little] [-d debug_level] [-f format]\n");
  fprintf(stderr, "       [-m mask_fields] [-V pdu_version] [-z z_level]\n");


  fprintf(stderr, "\n%s version %s: built by %s\n", PACKAGE, VERSION, FT_PROG_BUILD);

} /* usage */

