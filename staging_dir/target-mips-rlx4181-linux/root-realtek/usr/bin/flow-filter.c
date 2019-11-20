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
 *      $Id: flow-filter.c,v 1.29 2004/11/03 02:06:35 maf Exp $
 */

#include "ftconfig.h"
#include <ftlib.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/uio.h>
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
#include "acl2.h"
#include "aclyacc.h"

/*
 * TODO
 * extended ACL's
 */

int debug;
int ip_net_only;

struct acl_list acl_list;

int yyparse (void);
void usage(void);
void yyerror(const char *msg);

extern FILE *yyin;
extern char *yytext;

int main(int argc, char **argv)
{
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftio ftio_in, ftio_out;
  struct ftset ftset;
  struct ftver ftv;
  struct ftprof ftp;
  u_int32 time_start, time_end, exaddr;
  int i, ret;
  char *acl_fname, *acl_std_src_name, *acl_std_dst_name;
  char *acl_std_nexthop_name;
  char *acl_ext_name, *str, *strm;
  int acl_std_src_index, acl_std_src_index2;
  int acl_std_dst_index, acl_std_dst_index2;
  int acl_std_nexthop_index, acl_std_nexthop_index2;
  int acl_ext_index, acl_ext_index2;
  struct acl_ip_ext_entry tmp_ext;
  int keep_input_time;
  int filter_input, filter_output, filter_srcport, filter_dstport;
  int filter_prot, filter_srcas, filter_dstas, filter_tos, filter_tcp_flags;
  int filter_exaddr;
  char in_tbl[65536], out_tbl[65536], src_tbl[65536], dst_tbl[65536];
  char srcas_tbl[65536], dstas_tbl[65536], tos_tbl[65536];
  char tcp_flags_tbl[65536];
  char prot_tbl[256];
  u_char tos_mask, tos, tcp_flags_mask, tcp_flags;
  u_int64 total_flows, xflag;
  char *rec;
  int first_match = 0;

  /* init fterr */
  fterr_setid(argv[0]);

  bzero(&ftv, sizeof ftv);

  /* defaults + no compression */
  ftset_init(&ftset, 0);

  /* init */
  bzero(&acl_list, sizeof acl_list);
  acl_fname = acl_std_src_name = acl_std_dst_name = (char*)0L;
  acl_std_nexthop_name = (char*)0L;
  acl_ext_name = (char*)0L;
  acl_std_src_index = acl_std_dst_index = -1;
  acl_std_nexthop_index = -1;
  acl_ext_index = -1;
  bzero(&tmp_ext, sizeof tmp_ext);
  total_flows = 0;
  tos_mask = 0xff;
  tcp_flags_mask = 0xff;
  keep_input_time = 0;

  filter_input = filter_output = filter_srcport = filter_dstport = 0;
  filter_prot = filter_srcas = filter_dstas = filter_tos = filter_exaddr = 0;
  filter_tcp_flags = 0;

  while ((i = getopt(argc, argv, "a:A:b:C:d:D:e:E:f:i:I:kop:P:r:S:t:T:x:z:"))
    != -1)
    switch (i) {

    case 'a': /* src AS filter list */
      if (load_lookup(optarg, 65536, srcas_tbl))
        fterr_errx(1, "load_lookup(): failed");

      filter_srcas = 1;
      break;

    case 'A': /* dst AS filter list */
      if (load_lookup(optarg, 65536, dstas_tbl))
        fterr_errx(1, "load_lookup(): failed");

      filter_dstas = 1;
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

    case 'D': /* dst ip standard access list filter */
      acl_std_dst_name = optarg;
      break;

    case 'd': /* debug */
      debug = atoi(optarg);
      break;

    case 'e': /* export IP address */
      filter_exaddr = 1;
      exaddr = scan_ip(optarg);
      break;

    case 'E': /* extended access list filter */
      acl_ext_name = optarg;
      break;

    case 'f': /* acl file name */
      acl_fname = optarg;
      break;

    case 'i': /* input filter interface list */
      if (load_lookup(optarg, 65536, in_tbl))
        fterr_errx(1, "load_lookup(): failed");

      filter_input = 1;
      break;

    case 'I': /* output filter interface list */
      if (load_lookup(optarg, 65536, out_tbl))
        fterr_errx(1, "load_lookup(): failed");

      filter_output = 1;
      break;

    case 'k': /* keep the start/end time from the input */
      keep_input_time = 1;
      break;

    case 'o': /* do logical OR between different statements (first match) */
      first_match = 1;
      break;


    case 'P': /* filter dstport */
      if (load_lookup(optarg, 65536, dst_tbl))
        fterr_errx(1, "load_lookup(): failed");

      filter_dstport = 1;
      break;

    case 'p': /* filter srcport */
      if (load_lookup(optarg, 65536, src_tbl))
        fterr_errx(1, "load_lookup(): failed");

      filter_srcport = 1;
      break;

    case 'r': /* filter protocol */
      if (load_lookup(optarg, 256, prot_tbl))
        fterr_errx(1, "load_lookup(): failed");

      filter_prot = 1;
      break;

    case 'S': /* src ip standard access list filter */
      acl_std_src_name = optarg;
      break;

    case 't': /* ToS Filter */
      if (!(str = malloc(strlen(optarg+1))))
        fterr_err(1, "malloc()");

      strcpy(str, optarg);

      /* search for mask option */
      if ((strm = index(str, '/'))) {
        ++strm;
        tos_mask = (u_char)strtol(strm, (char**)0L, 0);
        --strm;
        *strm = 0;
      }

      if (load_lookup(str, 65536, tos_tbl)) {
        free(str);
        fterr_errx(1, "load_lookup(): failed");
      }

      free(str);
      filter_tos = 1;
      break;

    case 'T': /* tcp flags filter */
      if (!(str = malloc(strlen(optarg+1))))
        fterr_err(1, "malloc()");

      strcpy(str, optarg);

      /* search for mask option */
      if ((strm = index(str, '/'))) {
        ++strm;
        tcp_flags_mask = (u_char)strtol(strm, (char**)0L, 0);
        --strm;
        *strm = 0;
      }

      if (load_lookup(str, 65536, tcp_flags_tbl)) {
        free(str);
        fterr_errx(1, "load_lookup(): failed");
      }

      free(str);
      filter_tcp_flags = 1;
      break;

    case 'x': /* nexthop ip standard access list filter */
      acl_std_nexthop_name = optarg;
      break;

    case 'h': /* help */
    case '?':
      usage();
      exit (0);
      break;

    case 'z': /* compress level */
      ftset.z_level = atoi(optarg);
      if ((ftset.z_level < 0) || (ftset.z_level > 9))
        fterr_errx(1, "Compression level must be between 0 and 9");
      break;

    default:
      usage();
      exit (1);
      break;

    } /* switch */

  if (argc - optind)
    fterr_errx(1, "Extra arguments starting with %s.", argv[optind]);

  /* input from stdin */
  if (ftio_init(&ftio_in, 0, FT_IO_FLAG_READ) < 0)
    fterr_errx(1, "ftio_init(): failed");

  ftio_get_ver(&ftio_in, &ftv);
  ftv.s_version = FT_IO_SVERSION;

  xflag = 0;
  if (filter_input) xflag |= FT_XFIELD_INPUT;
  if (filter_output) xflag |= FT_XFIELD_OUTPUT;
  if (filter_srcport) xflag |= FT_XFIELD_SRCPORT;
  if (filter_dstport) xflag |= FT_XFIELD_DSTPORT;
  if (filter_exaddr) xflag |= FT_XFIELD_EXADDR;
  if (filter_prot) xflag |= FT_XFIELD_PROT;
  if (filter_srcas) xflag |= FT_XFIELD_SRC_AS;
  if (filter_dstas) xflag |= FT_XFIELD_DST_AS;
  if (filter_tos) xflag |= FT_XFIELD_TOS;
  if (filter_tcp_flags) xflag |= FT_XFIELD_TCP_FLAGS;
  if (acl_std_nexthop_name) xflag |= FT_XFIELD_NEXTHOP;
  if (acl_std_src_name) xflag |= FT_XFIELD_SRCADDR;
  if (acl_std_dst_name) xflag |= FT_XFIELD_DSTADDR;
  if (acl_ext_name) xflag |= (FT_XFIELD_SRCADDR | FT_XFIELD_DSTADDR);

  if (ftio_check_xfield(&ftio_in, xflag)) {
    fterr_warnx("Flow record missing required field for format.");
    exit (1);
  }

  fts3rec_compute_offsets(&fo, &ftv);

  /* output to stdout */
  if (ftio_init(&ftio_out, 1, FT_IO_FLAG_WRITE |
    ((ftset.z_level) ? FT_IO_FLAG_ZINIT : 0) ) < 0)
    fterr_errx(1, "ftio_init(): failed");

  /* preserve start/end time from input stream? */
  if (keep_input_time) {

    time_start = ftio_get_cap_start(&ftio_in);
    time_end = ftio_get_cap_end(&ftio_in);

    if (time_start && time_end) {

      ftio_set_preloaded(&ftio_out, 1);
      ftio_set_cap_time(&ftio_out, time_start, time_end);

    }

  } /* keep_input_time */

  ftio_set_comment(&ftio_out, ftset.comments);
  ftio_set_byte_order(&ftio_out, ftset.byte_order);
  ftio_set_z_level(&ftio_out, ftset.z_level);
  ftio_set_streaming(&ftio_out, 1);
  ftio_set_debug(&ftio_out, debug);

  if (ftio_set_ver(&ftio_out, &ftv) < 0)
    fterr_errx(1, "ftio_set_ver(): failed");

  /*
   * load acl's
   * XXX add fname check and close
   */
  if ((yyin = fopen(acl_fname ? acl_fname : "flow.acl", "r")))
    while (!feof(yyin))
      yyparse();

  /*
   * normalize masks
   */
  /* XXX TODO */

  if (debug > 5) 
    acl_dump(acl_list);

  if (acl_std_src_name) {
    if ((acl_std_src_index = acl_find(acl_list, acl_std_src_name)) == -1)
      fterr_errx(1, "Couldn't find list %s\n", acl_std_src_name);

    acl_std_src_index2 = acl_list.names[acl_std_src_index].num;

  }

  if (acl_std_dst_name) {
    if ((acl_std_dst_index = acl_find(acl_list, acl_std_dst_name)) == -1)
      fterr_errx(1, "Couldn't find list %s\n", acl_std_dst_name);

    acl_std_dst_index2 = acl_list.names[acl_std_dst_index].num;
  }

  if (acl_ext_name) {
    if ((acl_ext_index = acl_find(acl_list, acl_ext_name)) == -1)
      fterr_errx(1, "Couldn't find list %s\n", acl_ext_name);

    acl_ext_index2 = acl_list.names[acl_ext_index].num;
  }

 if (acl_std_nexthop_name) {
    if ((acl_std_nexthop_index = acl_find(acl_list, acl_std_nexthop_name))
      == -1)
      fterr_errx(1, "Couldn't find list %s\n", acl_std_nexthop_name);

    acl_std_nexthop_index2 = acl_list.names[acl_std_nexthop_index].num;
  }

  /* header first */
  if (ftio_write_header(&ftio_out) < 0)
    fterr_errx(1, "ftio_write_header(): failed");

  /* profile */
  ftprof_start (&ftp);

  /* grab 1 flow */
  while ((rec = ftio_read(&ftio_in))) {

    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));
    cur.exaddr = ((u_int32*)(rec+fo.exaddr));
    cur.nexthop = ((u_int32*)(rec+fo.nexthop));
    cur.input = ((u_int16*)(rec+fo.input));
    cur.output = ((u_int16*)(rec+fo.output));
    cur.srcport = ((u_int16*)(rec+fo.srcport));
    cur.dstport = ((u_int16*)(rec+fo.dstport));
    cur.src_as = ((u_int16*)(rec+fo.src_as));
    cur.dst_as = ((u_int16*)(rec+fo.dst_as));
    cur.prot = ((u_int8*)(rec+fo.prot));
    cur.tcp_flags = ((u_int8*)(rec+fo.tcp_flags));
    cur.tos = ((u_int8*)(rec+fo.tos));

    ++ total_flows;

    /* filter on input interface */
    if (filter_input) {
      if (!in_tbl[*cur.input]) {
        if (!first_match)
          continue;
      } else if (first_match) {
        goto found;
      }
    }

    /* filter on output interface */
    if (filter_output) {
      if (!out_tbl[*cur.output]) {
        if (!first_match)
          continue;
      } else if (first_match) {
        goto found;
      }
    }

    /* filter on src port */
    if (filter_srcport) {
      if (!src_tbl[*cur.srcport]) {
        if (!first_match)
          continue;
      } else if (first_match) {
        goto found;
      }
    }

    /* filter on dst port */
    if (filter_dstport) {
      if (!dst_tbl[*cur.dstport]) {
        if (!first_match)
          continue;
      } else if (first_match) {
        goto found;
      }
    }

    /* filter on exporter addr */
    if (filter_exaddr) {
      if (*cur.exaddr != exaddr) {
        if (!first_match)
          continue;
      } else if (first_match) {
          goto found;
      }
    }


    /* filter on protocol */
    if (filter_prot) {
      if (!prot_tbl[*cur.prot]) {
        if (!first_match)
          continue;
      } else if (first_match) {
        goto found;
      }
    }

    /* filter on ToS */
      if (filter_tos) {
        tos = *cur.tos & tos_mask;
        if (!tos_tbl[tos]) {
          if (!first_match)
            continue;
        } else if (first_match) {
          goto found;
        }
      }

    /* filter on tcp_flags */
      if (filter_tcp_flags && (*cur.prot == IPPROTO_TCP)) {
        tcp_flags = *cur.tcp_flags & tcp_flags_mask;
        if (!tcp_flags_tbl[tcp_flags]) {
          if (!first_match)
            continue;
        } else if (first_match) {
          goto found;
        }
      }

    if (filter_srcas) {
      if (!srcas_tbl[*cur.src_as]) {
        if (!first_match)
          continue;
      } else if (first_match) {
        goto found;
      }
    }

    /* filter on src AS */
    if (filter_dstas) {
      if (!dstas_tbl[*cur.dst_as]) {
        if (!first_match)
          continue;
      } else if (first_match) {
        goto found;
      }
    }

   /* eval flow nexthop addr and nexthop standard acl */
   if (acl_std_nexthop_index != -1) {
      ret = acl_eval_std(acl_list, acl_std_nexthop_index2, *cur.nexthop);
      if (ret == 1) {
        if (!first_match)
          continue;
      } else if (first_match) {
        goto found;
      }
    }

    /* eval flow src addr and source standard acl */
    if (acl_std_src_index != -1) {
      ret = acl_eval_std(acl_list, acl_std_src_index2, *cur.srcaddr);
      if (ret == 1) {
        if (!first_match)
          continue;
      } else if (first_match) {
        goto found;
      }
    }

    /* eval flow dst addr and destination standard acl */
    if (acl_std_dst_index != -1) {
      ret = acl_eval_std(acl_list, acl_std_dst_index2, *cur.dstaddr);
      if (ret == 1) {
        if (!first_match)
          continue;
      } else if (first_match) {
        goto found;
      }
    }

    /* eval flow and extended acl */
    if (acl_ext_index != -1) {

      tmp_ext.protocol = *cur.prot;
      tmp_ext.tos = *cur.tos;

      /* XXX */
      tmp_ext.type = 0;
      tmp_ext.type_code = 0;
      tmp_ext.message = 0;

      tmp_ext.src_addr = *cur.srcaddr;
      tmp_ext.src_port = *cur.srcport;


      tmp_ext.dst_addr = *cur.dstaddr;
      tmp_ext.dst_port = *cur.dstport;

      ret = acl_eval_ext(acl_list, acl_ext_index2, tmp_ext);
      if (ret == 1) {
        if (!first_match)
          continue;
      } else if (first_match) {
        goto found;
      }
    }

    if (first_match) /* No matches yet? next try.. */
      continue;

    /*
     * made it by the filters, write it
     */

found:
    if (ftio_write(&ftio_out, rec) < 0)
      fterr_errx(1, "ftio_write(): failed");

  } /* while more flows to read */

  if (ftio_close(&ftio_in) < 0)
    fterr_errx(1, "ftio_close(): failed");

  if (ftio_close(&ftio_out) < 0)
    fterr_errx(1, "ftio_close(): failed");

  if (debug > 0) {
    ftprof_end (&ftp, total_flows);
    ftprof_print(&ftp, argv[0], stderr);
  }   

  if (debug > 1) {
    acl_dump_std(acl_list, acl_std_src_index);
    acl_dump_std(acl_list, acl_std_dst_index);
    acl_dump_std(acl_list, acl_std_nexthop_index);
  }

  return 0;

} /* main */

void yyerror(const char *msg)
{
  fterr_warnx("%s at '%s'\n", msg, yytext);
}

void usage(void) {

  fprintf(stderr, "Usage: flow-filter [-hko] [-a src_as_filter] [-A dst_as_filter] [-b big|little]\n");
  fprintf(stderr, "       [-C comment] [-D dstaddr_filter_name] [-d debug_level] [-e exaddr]\n");
  fprintf(stderr, "       [-f acl_fname] [-i input_filter] [-I output_filter]  [-p srcport_filter]\n");
  fprintf(stderr, "       [-P dstport_filter] [-r ipprot_filter] [-S srcaddr_filter_name]\n");
  fprintf(stderr, "       [-t tos_filter] [-T tcp_flags_filter] [-x nexthop_filter_name]\n");
  fprintf(stderr, "       [-z z_level]\n");
  fprintf(stderr, "\n%s version %s: built by %s\n", PACKAGE, VERSION, FT_PROG_BUILD);

} /* usage */

