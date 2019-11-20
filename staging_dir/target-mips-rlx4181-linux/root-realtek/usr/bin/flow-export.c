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
 *      $Id: flow-export.c,v 1.26 2004/03/31 03:11:14 maf Exp $
 */

#include "ftconfig.h"
#include <ftlib.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <ctype.h>
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

#ifdef HAVE_MYSQL

#include <mysql.h>

#define DB_DEFAULT_DBHOST "localhost"
#define DB_DEFAULT_DBNAME "netflow"
#define DB_DEFAULT_DBPORT 3306
#define DB_DEFAULT_DBTABLE "raw"
#define DB_DEFAULT_DBUSER "netflow"
#define DB_DEFAULT_DBPWD "netflow"

#endif /* MYSQL */

#ifdef HAVE_PGSQL

#include <libpq-fe.h>

#define DB_DEFAULT_DBHOST "localhost"
#define DB_DEFAULT_DBNAME "netflow"
#define DB_DEFAULT_DBPORT "5432"
#define DB_DEFAULT_DBTABLE "raw"
#define DB_DEFAULT_DBUSER "netflow"
#define DB_DEFAULT_DBPWD "netflow"

#endif /* PGSQL*/

#if HAVE_LL_STRTOUL
  #define strtoull strtoul
#endif /* HAVE_LL_STRTOULL */

#include "ftbuild.h"
#include "pcap.h"
#include "cflowd.h"

#define PRCOMMA\
  if (comma)\
    printf(",");\

struct options {
  char dbaseURI[256];
  u_int32 cflowd_mask;
  u_int64 ft_mask;
  u_long records;
};

struct jump {
    int (*where)(struct ftio *ftio, struct options *opt);
};

int format0(struct ftio *ftio, struct options *opt);
int format1(struct ftio *ftio, struct options *opt);
int format2(struct ftio *ftio, struct options *opt);
int format3(struct ftio *ftio, struct options *opt);
int format4(struct ftio *ftio, struct options *opt);
int format5(struct ftio *ftio, struct options *opt);

int ftxfield_tocflow(u_int64 xfields, u_int32 *cfmask);

int fmt_xfields_val(char *fmt_buf, char *rec, struct fts3rec_offsets *fo,
  u_int64 xfields, int quote);
int fmt_xfields_type(char *buf, u_int64 xfield);

void usage(void);

#define NFORMATS 6 /* nformats - 1 */
struct jump format[] = {{format0}, {format1}, {format2}, {format3},
                        {format4}, {format5}};

int debug;

int main(int argc, char **argv)
{
  int i, format_index, ret, ascii_mask;
  struct ftio ftio;
  struct ftprof ftp;
  struct options opt;

  /* init fterr */
  fterr_setid(argv[0]);

  debug = 0;
  format_index = 0;
  bzero(&opt, sizeof opt);
  ascii_mask = 0;

  opt.cflowd_mask = 0xFFFFFFFFL;
  opt.ft_mask = 0xFFFFFFFFFFFFFFFFLL;

  /* profile */
  ftprof_start (&ftp);

  while ((i = getopt(argc, argv, "h?d:f:m:u:")) != -1)

    switch (i) {

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

    case 'm': /* cflowd mask */
      if (isalpha((int)optarg[0])) {
        ascii_mask = 1;
        if (ftxfield_parse(optarg, &opt.ft_mask) < 0)
          fterr_errx(1, "ftxfield_parse(): failed");
      } else {
        opt.cflowd_mask = strtoul(optarg, (char **)0L, 0);
        opt.ft_mask = strtoull(optarg, (char **)0L, 0);
      }
      break;

    case 'u': /* db URI */
      if (strlen(optarg) >= sizeof (opt.dbaseURI))
        fterr_errx(1, "dbaseURI string too long.");
      strcpy(opt.dbaseURI, optarg);
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

  if ((format_index == 0) && ascii_mask) {
    opt.cflowd_mask = 0;
    if (ftxfield_tocflow(opt.ft_mask, &opt.cflowd_mask) < 0) {
      fterr_errx(1, "ftxfield_tocflow(): failed");
    }
  }

  if (ftio_init(&ftio, 0, FT_IO_FLAG_READ) < 0)
    fterr_errx(1, "ftio_init(): failed");

  ret = format[format_index].where(&ftio, &opt);
      
  if ((!ret) && (debug > 0)) {
    ftprof_end(&ftp, ftio_get_rec_total(&ftio));
    ftprof_print(&ftp, argv[0], stderr);
  }

  fprintf(stderr, "%s: Exported %lu records\n", argv[0], opt.records);

        
  return ret;

} /* main */

/*
 * function: format0
 *
 * export flows in cflowd format
*/
int format0(struct ftio *ftio, struct options *opt)
{
  struct fts3rec_offsets fo;
  struct ftver ftv;
  struct fttime ftt;
  char *rec;
  u_int32 ui32, index, sysUpTime, unix_secs, unix_nsecs, First, Last;
  u_int16 ui16;
  u_int8 ui8;

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  switch (ftv.d_version) {

    case 1:
      opt->cflowd_mask &= CF_INDEX_V1_MASK;
      break;

    case 5:
      opt->cflowd_mask &= CF_INDEX_V5_MASK;
      break;

    case 6:
      opt->cflowd_mask &= CF_INDEX_V6_MASK;
      break;

    case 7:
      opt->cflowd_mask &= CF_INDEX_V7_MASK;
      break;

    case 1005:
      opt->cflowd_mask &= CF_INDEX_V5_MASK;
      break;

    case 8:

      switch (ftv.agg_method) {

        case 1:
          opt->cflowd_mask &= CF_INDEX_V8_1_MASK;
          break;

        case 2:
          opt->cflowd_mask &= CF_INDEX_V8_2_MASK;
          break;

        case 3:
          opt->cflowd_mask &= CF_INDEX_V8_3_MASK;
          break;

        case 4:
          opt->cflowd_mask &= CF_INDEX_V8_4_MASK;
          break;

        case 5:
          opt->cflowd_mask &= CF_INDEX_V8_5_MASK;
          break;

        case 6:
          opt->cflowd_mask &= CF_INDEX_V8_6_MASK;
          break;

        case 7:
          opt->cflowd_mask &= CF_INDEX_V8_7_MASK;
          break;

        case 8:
          opt->cflowd_mask &= CF_INDEX_V8_8_MASK;
          break;

        case 9:
          opt->cflowd_mask &= CF_INDEX_V8_9_MASK;
          break;

        case 10:
          opt->cflowd_mask &= CF_INDEX_V8_10_MASK;
          break;

        case 11:
          opt->cflowd_mask &= CF_INDEX_V8_11_MASK;
          break;

        case 12:
          opt->cflowd_mask &= CF_INDEX_V8_12_MASK;
          break;

        case 13:
          opt->cflowd_mask &= CF_INDEX_V8_13_MASK;
          break;

        case 14:
          opt->cflowd_mask &= CF_INDEX_V8_14_MASK;
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

  /* index */
  index = opt->cflowd_mask;
  index = htonl(index);

  while ((rec = ftio_read(ftio))) {

    fwrite(&index, sizeof (index), 1, stdout);

    if (opt->cflowd_mask & CF_ROUTERMASK) {
       ui32 = *((u_int32*)(rec+fo.exaddr));
       ui32 = htonl(ui32);
       fwrite(&ui32, sizeof (ui32), 1, stdout);
    }

    if (opt->cflowd_mask & CF_SRCIPADDRMASK) {
       ui32 = *((u_int32*)(rec+fo.srcaddr));
       ui32 = htonl(ui32);
       fwrite(&ui32, sizeof (ui32), 1, stdout);
    }

    if (opt->cflowd_mask & CF_DSTIPADDRMASK) {
       ui32 = *((u_int32*)(rec+fo.dstaddr));
       ui32 = htonl(ui32);
       fwrite(&ui32, sizeof (ui32), 1, stdout);
    }

    if (opt->cflowd_mask & CF_INPUTIFINDEXMASK) {
       ui16 = *((u_int16*)(rec+fo.input));
       ui16 = htons(ui16);
       fwrite(&ui16, sizeof (ui16), 1, stdout);
    }

    if (opt->cflowd_mask & CF_OUTPUTIFINDEXMASK) {
       ui16 = *((u_int16*)(rec+fo.output));
       ui16 = htons(ui16);
       fwrite(&ui16, sizeof (ui16), 1, stdout);
    }

    if (opt->cflowd_mask & CF_SRCPORTMASK) {
       ui16 = *((u_int16*)(rec+fo.srcport));
       ui16 = htons(ui16);
       fwrite(&ui16, sizeof (ui16), 1, stdout);
    }

    if (opt->cflowd_mask & CF_DSTPORTMASK) {
       ui16 = *((u_int16*)(rec+fo.dstport));
       ui16 = htons(ui16);
       fwrite(&ui16, sizeof (ui16), 1, stdout);
    }

    if (opt->cflowd_mask & CF_PKTSMASK) {
       ui32 = *((u_int32*)(rec+fo.dPkts));
       ui32 = htonl(ui32);
       fwrite(&ui32, sizeof (ui32), 1, stdout);
    }

    if (opt->cflowd_mask & CF_BYTESMASK) {
       ui32 = *((u_int32*)(rec+fo.dOctets));
       ui32 = htonl(ui32);
       fwrite(&ui32, sizeof (ui32), 1, stdout);
    }

    if (opt->cflowd_mask & CF_IPNEXTHOPMASK) {
       ui32 = *((u_int32*)(rec+fo.nexthop));
       ui32 = htonl(ui32);
       fwrite(&ui32, sizeof (ui32), 1, stdout);
    }

    if (opt->cflowd_mask & CF_STARTTIMEMASK) {
       sysUpTime = *((u_int32*)(rec+fo.sysUpTime));
       unix_secs = *((u_int32*)(rec+fo.unix_secs));
       unix_nsecs = *((u_int32*)(rec+fo.unix_nsecs));
       First = *((u_int32*)(rec+fo.First));
       ftt = ftltime(sysUpTime, unix_secs, unix_nsecs, First);
       ui32 = htonl(ftt.secs);
       fwrite(&ui32, sizeof (ui32), 1, stdout);
    }

    if (opt->cflowd_mask & CF_ENDTIMEMASK) {
       sysUpTime = *((u_int32*)(rec+fo.sysUpTime));
       unix_secs = *((u_int32*)(rec+fo.unix_secs));
       unix_nsecs = *((u_int32*)(rec+fo.unix_nsecs));
       Last = *((u_int32*)(rec+fo.Last));
       ftt = ftltime(sysUpTime, unix_secs, unix_nsecs, Last);
       ui32 = htonl(ftt.secs);
       fwrite(&ui32, sizeof (ui32), 1, stdout);
    }

    if (opt->cflowd_mask & CF_PROTOCOLMASK) {
       ui8 = *((u_int8*)(rec+fo.prot));
       fwrite(&ui8, sizeof (ui8), 1, stdout);
    }

    if (opt->cflowd_mask & CF_TOSMASK) {
       ui8 = *((u_int8*)(rec+fo.tos));
       fwrite(&ui8, sizeof (ui8), 1, stdout);
    }

    if (opt->cflowd_mask & CF_SRCASMASK) {
       ui16 = *((u_int16*)(rec+fo.src_as));
       ui16 = htons(ui16);
       fwrite(&ui16, sizeof (ui16), 1, stdout);
    }

    if (opt->cflowd_mask & CF_DSTASMASK) {
       ui16 = *((u_int16*)(rec+fo.dst_as));
       ui16 = htons(ui16);
       fwrite(&ui16, sizeof (ui16), 1, stdout);
    }

    if (opt->cflowd_mask & CF_SRCMASKLENMASK) {
       ui8 = *((u_int8*)(rec+fo.src_mask));
       fwrite(&ui8, sizeof (ui8), 1, stdout);
    }

    if (opt->cflowd_mask & CF_DSTMASKLENMASK) {
       ui8 = *((u_int8*)(rec+fo.dst_mask));
       fwrite(&ui8, sizeof (ui8), 1, stdout);
    }

    if (opt->cflowd_mask & CF_TCPFLAGSMASK) {
       ui8 = *((u_int8*)(rec+fo.tcp_flags));
       fwrite(&ui8, sizeof (ui8), 1, stdout);
    }

    if (opt->cflowd_mask & CF_INPUTENCAPMASK) {
       ui8 = *((u_int8*)(rec+fo.in_encaps));
       fwrite(&ui8, sizeof (ui8), 1, stdout);
    }

    if (opt->cflowd_mask & CF_OUTPUTENCAPMASK) {
       ui8 = *((u_int8*)(rec+fo.out_encaps));
       fwrite(&ui8, sizeof (ui8), 1, stdout);
    }

    if (opt->cflowd_mask & CF_PEERNEXTHOPMASK) {
       ui32 = *((u_int32*)(rec+fo.peer_nexthop));
       ui32 = htonl(ui32);
       fwrite(&ui32, sizeof (ui32), 1, stdout);
    }

    if (opt->cflowd_mask & CF_ENGINETYPEMASK) {
       ui8 = *((u_int8*)(rec+fo.engine_type));
       fwrite(&ui8, sizeof (ui8), 1, stdout);
    }

    if (opt->cflowd_mask & CF_ENGINEIDMASK) {
       ui8 = *((u_int8*)(rec+fo.engine_id));
       fwrite(&ui8, sizeof (ui8), 1, stdout);
    }

    ++opt->records;

  } /* while */

  return 0;

} /* format 0 */

/*
 * function: format1
 *
 * export flows in pcap format.  Hack to use tcpdump's packet matcher
*/
int format1(struct ftio *ftio, struct options *opt)
{
  struct timeval now;
  struct timezone tz;
  struct fts3rec_all cur;
  struct fts3rec_offsets fo;
  struct ftver ftv;
  struct pcap_file_header pfh;
  struct pcap_packet_header pph;
  struct pcap_data1 pd1;
  struct pcap_data2 pd2;
  struct pcap_data3 pd3;
  struct pcap_data4 pd4;
  int bsize, bsize2, good;
  long thiszone;
  char buf[1024];
  char *rec;

  if (ftio_check_xfield(ftio, FT_XFIELD_TOS | FT_XFIELD_PROT | 
    FT_XFIELD_SRCADDR | FT_XFIELD_DSTADDR | FT_XFIELD_SRCPORT |
    FT_XFIELD_DSTPORT)) {
    fterr_warnx("Flow record missing required field for format.");
    return -1;
  }

  ftio_get_ver(ftio, &ftv);
 
  fts3rec_compute_offsets(&fo, &ftv);

  if (gettimeofday(&now, &tz) < 0) {
    fterr_warnx("gettimeofday() failed");
    return -1;
  }

  bzero(&pfh, sizeof pfh);
  bzero(&pph, sizeof pph);
  bzero(&pd1, sizeof pd1);
  bzero(&pd2, sizeof pd2);
  bzero(&pd3, sizeof pd3);
  bzero(&pd4, sizeof pd4);
  bsize = 0;

  thiszone = tz.tz_minuteswest * -60;

  if (localtime((time_t *)&now.tv_sec)->tm_isdst)
    thiszone += 3600;

  pfh.magic = TCPDUMP_MAGIC;  
  pfh.version_major = TCPDUMP_VERSION_MAJOR;
  pfh.version_minor = TCPDUMP_VERSION_MINOR;
  pfh.thiszone = thiszone;
  pfh.sigfigs = 6;
  pfh.snaplen = 38; /* XXX TODO */
  pfh.linktype = 1;

  if (fwrite(&pfh, sizeof pfh, 1, stdout) != 1) {
    fterr_warnx("pcap header write failed");
    return -1;
  }

  pph.len = 58;
  pph.caplen = 58;

  pd1.eth_prot = 0x0008;
  pd2.version = 0x45;

  bcopy(&pph, buf, sizeof pph);
  bsize += sizeof pph;

  bcopy(&pd1, buf+bsize, sizeof pd1);
  bsize += sizeof pd1;

  while ((rec = ftio_read(ftio))) {

    cur.srcport = ((u_int16*)(rec+fo.srcport));
    cur.dstport = ((u_int16*)(rec+fo.dstport));
    cur.prot = ((u_int8*)(rec+fo.prot));
    cur.tos = ((u_int8*)(rec+fo.tos));
    cur.srcaddr = ((u_int32*)(rec+fo.srcaddr));
    cur.dstaddr = ((u_int32*)(rec+fo.dstaddr));


    pd2.tos = *cur.tos;
    pd2.prot = *cur.prot;
    pd2.srcaddr = *cur.srcaddr;
    pd2.dstaddr = *cur.dstaddr;

#if BYTE_ORDER == LITTLE_ENDIAN
    SWAPINT32(pd2.srcaddr);
    SWAPINT32(pd2.dstaddr);
#endif /* LITTLE_ENDIAN */

    good = 1;

    switch (pd2.prot) {

    case 6:

      pd3.srcport = *cur.srcport;
      pd3.dstport = *cur.dstport;

#if BYTE_ORDER == LITTLE_ENDIAN
      SWAPINT16(pd3.srcport);
      SWAPINT16(pd3.dstport);
#endif /* LITTLE_ENDIAN */

      bcopy(&pd2, buf+bsize, sizeof pd2);
      bcopy(&pd3, buf+bsize+sizeof pd2, sizeof pd3);
      bsize2 = bsize + sizeof pd2 + sizeof pd3;

      break;

    case 17:

      pd4.srcport = *cur.srcport;
      pd4.dstport = *cur.dstport;

#if BYTE_ORDER == LITTLE_ENDIAN
      SWAPINT16(pd4.srcport);
      SWAPINT16(pd4.dstport);
#endif /* LITTLE_ENDIAN */

      bcopy(&pd2, buf+bsize, sizeof pd2);
      bcopy(&pd4, buf+bsize+sizeof pd2, sizeof pd4);
      bsize2 = bsize + sizeof pd2 + sizeof pd4;

      break;

    default:
      good = 0;
      break;

    } /* switch */

    if (good) {
      if (fwrite(&buf, bsize2, 1, stdout) != 1) {
        fterr_warnx("pcap pkt write failed");
        return -1;
      }
    }

    ++opt->records;

  } /* while */

  return 0;
  
} /* format1 */

/*
 * function: format2
 *
 * export flows in ASCII CSV Format
 */
int format2(struct ftio *ftio, struct options *opt)
{
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fmt_buf[1024];
  char *rec;
  int len;

  ftio_get_ver(ftio, &ftv);

  /* remove invalid fields */
  opt->ft_mask &= ftrec_xfield(&ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  fmt_xfields_type(fmt_buf, opt->ft_mask);

  printf("#:%s\n", fmt_buf);

  while ((rec = ftio_read(ftio))) {

    len = fmt_xfields_val(fmt_buf, rec, &fo, opt->ft_mask, 0);

    if (len)
      printf("%s\n", fmt_buf);

    ++opt->records;

  } /* while */

  return 0;
 
} /* format2 */ 

/*
 * function: format3
 *
 * export flows into MySQL Database
 */
int format3(struct ftio *ftio, struct options *opt)
{
#ifdef HAVE_MYSQL
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fields[1024], values[1024], query[3*1024];
  char *rec;
  char *db_host, *db_name, *db_table, *db_user, *db_pwd, *db_tmp, *tmp;
  int db_port;
  int len;

  MYSQL mysql;

  db_host = DB_DEFAULT_DBHOST;
  db_name = DB_DEFAULT_DBNAME;
  db_port = DB_DEFAULT_DBPORT;
  db_user = DB_DEFAULT_DBUSER;
  db_table = DB_DEFAULT_DBTABLE;
  db_pwd = DB_DEFAULT_DBPWD;

  /* parse URI string */

  if (strlen(opt->dbaseURI)) {

    tmp = opt->dbaseURI;

    db_user = strsep(&tmp, ":");
    db_pwd = strsep(&tmp, ":");
    db_host = strsep(&tmp, ":");
    db_tmp = strsep(&tmp, ":");
    db_name = strsep(&tmp, ":");
    db_table = strsep(&tmp, ":");
    db_port = atoi(db_tmp);

    if (!db_user || !db_pwd || !db_host || !db_tmp || !db_name || !db_table) {
      fterr_warnx("Missing field in dbaseURI, expecting user:pwd:host:port:name:table.");
      return -1;
    }

  } /* dbaseURI */

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  /* remove invalid fields */
  opt->ft_mask &= ftrec_xfield(&ftv);

  /* generate the field names once */
  fmt_xfields_type(fields, opt->ft_mask);

  /* open MySQL database */
  if (!(mysql_init(&mysql)))
    fterr_errx(1, "mysql_init(): failed");

  if (mysql_options(&mysql, MYSQL_READ_DEFAULT_GROUP, "simple"))
    fterr_errx(1, "mysql_options(): %s", mysql_error(&mysql));

  if (mysql_real_connect(&mysql, db_host, db_user, db_pwd, 
	db_name, db_port, NULL, 0) == NULL) 
    fterr_errx(1,"mysql_real_connect(): %s\n", mysql_error(&mysql));

  /* foreach flow */
  while ((rec = ftio_read(ftio))) {

    len = fmt_xfields_val(values, rec, &fo, opt->ft_mask, 1);

    /* form SQL query and execute it */
    if (len) {
      strcpy (query, "INSERT INTO ");
      strcat (query, db_table);
      strcat (query, "(");
      strcat (query, fields);
      strcat (query, ") VALUES (");
      strcat (query, values);
      strcat (query, ")");

      if (debug)
        fprintf(stderr, "field=%s\n val=%s\n query=%s\n", fields, values,
          query);

      if (mysql_real_query(&mysql, query, strlen(query)) != 0) 
        fterr_warnx("mysql_real_query(): %s", mysql_error(&mysql));

    }

    ++opt->records;

  } /* while */

  /* close database */
  mysql_close(&mysql);

#else /* MYSQL */

  fterr_warnx("Format not supported");

#endif /* MYSQL */

  return 0;
 
} /* format3 */ 

/*
 * function: format4
 *
 * export flows in wire format
*/
int format4(struct ftio *ftio, struct options *opt)
{
  struct ftver ftv;
  struct ftencode fte;
  char *rec;
  int ret;

  /* initialize encode struct */
  ftencode_init(&fte, 0);

  /* copy version from io stream */
  ftio_get_ver(ftio, &ftv);

  bcopy(&ftv, &fte.ver, sizeof ftv);

  /* foreach flow */
  while ((rec = ftio_read(ftio))) {

retry:

    ret = fts3rec_pdu_encode(&fte, rec);
/*   ret == 0 then send and clear out buffer
 *   ret > 0 then encode another
 *   ret < 0 then this encoding failed, send and clear out buffer
*/
  
    if (ret <= 0) { 
    
      /* convert pdu to network byte order */
#if BYTE_ORDER == LITTLE_ENDIAN
      ftpdu_swap(fte.buf_enc, BYTE_ORDER);
#endif /* BYTE_ORDER == LITTLE_ENDIAN */

      if (fwrite(&fte.buf, fte.buf_size, 1, stdout) != 1)
        fterr_err(1, "fwrite()");

      /* reset encode buffer */
      ftencode_reset(&fte);
 
      /* if ret < 0 then the current record was not encoded */   
      if (ret < 0)
        goto retry;
    }

    ++opt->records;

  }

  /* any left over? */
  if (fte.buf_size) {

    /* convert pdu to network byte order */
    ftpdu_swap(fte.buf_enc, BYTE_ORDER);

    if (fwrite(&fte.buf, fte.buf_size, 1, stdout) != 1)
      fterr_err(1, "fwrite()");

  } /* fte.buf_size */

  return 0;

} /* format4 */

/*
 * function: format5
 *
 * export flows into PostgreSQL Database
 */
int format5(struct ftio *ftio, struct options *opt)
{
#ifdef HAVE_PGSQL
  struct fts3rec_offsets fo;
  struct ftver ftv;
  char fields[1024], values[1024], query[3*1024];
  char *rec;
  char *db_host, *db_name, *db_table, *db_user, *db_pwd, *db_tmp, *tmp;
  char *db_port;
  int len;

  PGconn     *conn;
  PGresult   *res;

  db_host = DB_DEFAULT_DBHOST;
  db_name = DB_DEFAULT_DBNAME;
  db_port = DB_DEFAULT_DBPORT;
  db_user = DB_DEFAULT_DBUSER;
  db_table = DB_DEFAULT_DBTABLE;
  db_pwd = DB_DEFAULT_DBPWD;

  /* parse URI string */

  if (strlen(opt->dbaseURI)) {

    tmp = opt->dbaseURI;

    db_user = strsep(&tmp, ":");
    db_pwd = strsep(&tmp, ":");
    db_host = strsep(&tmp, ":");
    db_port = strsep(&tmp, ":");
    db_name = strsep(&tmp, ":");
    db_table = strsep(&tmp, ":");

    if (!db_user || !db_pwd || !db_host || !db_tmp || !db_name || !db_table) {
      fterr_warnx("Missing field in dbaseURI, expecting user:pwd:host:port:name:table.");
      return -1;
    }

  } /* dbaseURI */

  ftio_get_ver(ftio, &ftv);

  fts3rec_compute_offsets(&fo, &ftv);

  /* remove invalid fields */
  opt->ft_mask &= ftrec_xfield(&ftv);

  /* generate the field names once */
  fmt_xfields_type(fields, opt->ft_mask);

  /* open PostgreSQL database */
  conn = PQsetdbLogin(db_host, db_port, (char *) NULL, (char *) NULL, db_name, db_user, db_pwd);

  if (PQstatus(conn) == CONNECTION_BAD) 
    fterr_errx(1,"PQsetdbLogin(): %s\n", PQerrorMessage(conn));

  /* foreach flow */
  while ((rec = ftio_read(ftio))) {

    len = fmt_xfields_val(values, rec, &fo, opt->ft_mask, 1);

    /* form SQL query and execute it */
    if (len) {
      strcpy (query, "INSERT INTO ");
      strcat (query, db_table);
      strcat (query, "(");
      strcat (query, fields);
      strcat (query, ") VALUES (");
      strcat (query, values);
      strcat (query, ")");

      if (debug)
        fprintf(stderr, "field=%s\n val=%s\n query=%s\n", fields, values,
          query);

      res = PQexec(conn, query);
      if (!res || PQresultStatus(res) != PGRES_COMMAND_OK) {
        PQclear(res);
        fterr_errx(1,"PQexec(): %s\n", PQerrorMessage(conn));
      }

    }

    ++opt->records;

  } /* while */

  /* close database */
  PQfinish(conn);

#else /* PGSQL */

  fterr_warnx("Format not supported");

#endif /* PGSQL */

  return 0;
 
} /* format5 */ 

int fmt_xfields_type(char *buf, u_int64 xfield)
{
  int comma = 0;

  buf[0] = 0;

  if (xfield & FT_XFIELD_UNIX_SECS) {
    strcat(buf, FT_XFIELD_ASC_UNIX_SECS);
    comma = 1;
  }


  if (xfield & FT_XFIELD_UNIX_NSECS) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_UNIX_NSECS);
    comma = 1;
  }

  if (xfield & FT_XFIELD_SYSUPTIME) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_SYSUPTIME);
    comma = 1;
  }

  if (xfield & FT_XFIELD_EXADDR) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_EXADDR);
    comma = 1;
  }

  if (xfield & FT_XFIELD_DFLOWS) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_DFLOWS);
    comma = 1;
  }

  if (xfield & FT_XFIELD_DPKTS) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_DPKTS);
    comma = 1;
  }

  if (xfield & FT_XFIELD_DOCTETS) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_DOCTETS);
    comma = 1;
  }

  if (xfield & FT_XFIELD_FIRST) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_FIRST);
    comma = 1;
  }

  if (xfield & FT_XFIELD_LAST) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_LAST);
    comma = 1;
  }

  if (xfield & FT_XFIELD_ENGINE_TYPE) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_ENGINE_TYPE);
    comma = 1;
  }

  if (xfield & FT_XFIELD_ENGINE_ID) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_ENGINE_ID);
    comma = 1;
  }

  if (xfield & FT_XFIELD_SRCADDR) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_SRCADDR);
    comma = 1;
  }

  if (xfield & FT_XFIELD_DSTADDR) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_DSTADDR);
    comma = 1;
  }

  if (xfield & FT_XFIELD_NEXTHOP) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_NEXTHOP);
    comma = 1;
  }

  if (xfield & FT_XFIELD_INPUT) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_INPUT);
    comma = 1;
  }

  if (xfield & FT_XFIELD_OUTPUT) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_OUTPUT);
    comma = 1;
  }

  if (xfield & FT_XFIELD_SRCPORT) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_SRCPORT);
    comma = 1;
  }

  if (xfield & FT_XFIELD_DSTPORT) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_DSTPORT);
    comma = 1;
  }

  if (xfield & FT_XFIELD_PROT) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_PROT);
    comma = 1;
  }

  if (xfield & FT_XFIELD_TOS) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_TOS);
    comma = 1;
  }

  if (xfield & FT_XFIELD_TCP_FLAGS) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_TCP_FLAGS);
    comma = 1;
  }

  if (xfield & FT_XFIELD_SRC_MASK) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_SRC_MASK);
    comma = 1;
  }

  if (xfield & FT_XFIELD_DST_MASK) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_DST_MASK);
    comma = 1;
  }

  if (xfield & FT_XFIELD_SRC_AS) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_SRC_AS);
    comma = 1;
  }

  if (xfield & FT_XFIELD_DST_AS) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_DST_AS);
    comma = 1;
  }

  if (xfield & FT_XFIELD_IN_ENCAPS) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_IN_ENCAPS);
    comma = 1;
  }

  if (xfield & FT_XFIELD_OUT_ENCAPS) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_OUT_ENCAPS);
    comma = 1;
  }

  if (xfield & FT_XFIELD_PEER_NEXTHOP) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_NEXTHOP);
    comma = 1;
  }

  if (xfield & FT_XFIELD_ROUTER_SC) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_ROUTER_SC);
    comma = 1;
  }

  if (xfield & FT_XFIELD_EXTRA_PKTS) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_EXTRA_PKTS);
    comma = 1;
  }

  if (xfield & FT_XFIELD_MARKED_TOS) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_MARKED_TOS);
    comma = 1;
  }

  if (xfield & FT_XFIELD_SRC_TAG) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_SRC_TAG);
    comma = 1;
  }

  if (xfield & FT_XFIELD_DST_TAG) {
    if (comma) strcat(buf, ",");
    strcat(buf, FT_XFIELD_ASC_DST_TAG);
    comma = 1;
  }

  return strlen(buf);

} /* fmt_xfields_type */


int fmt_xfields_val(char *fmt_buf, char *rec, struct fts3rec_offsets *fo,
  u_int64 xfields, int quote)
{
  int comma, len;

  fmt_buf[0] = 0;

  len = comma = 0;

  if (xfields & FT_XFIELD_UNIX_SECS) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint32(fmt_buf+len, *((u_int32*)(rec+fo->unix_secs)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_UNIX_NSECS) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint32(fmt_buf+len, *((u_int32*)(rec+fo->unix_nsecs)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_SYSUPTIME) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint32(fmt_buf+len, *((u_int32*)(rec+fo->sysUpTime)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_EXADDR) {
    if (comma) fmt_buf[len++] = ',';
    if (quote) fmt_buf[len++] = '"';
    len += fmt_ipv4(fmt_buf+len, *((u_int32*)(rec+fo->exaddr)),
      FMT_JUST_LEFT);
    if (quote) fmt_buf[len++] = '"';
    comma = 1;
  }

  if (xfields & FT_XFIELD_DFLOWS) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint32(fmt_buf+len, *((u_int32*)(rec+fo->dFlows)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_DPKTS) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint32(fmt_buf+len, *((u_int32*)(rec+fo->dPkts)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_DOCTETS) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint32(fmt_buf+len, *((u_int32*)(rec+fo->dOctets)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_FIRST) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint32(fmt_buf+len, *((u_int32*)(rec+fo->First)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_LAST) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint32(fmt_buf+len, *((u_int32*)(rec+fo->Last)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_ENGINE_TYPE) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint8(fmt_buf+len, *((u_int8*)(rec+fo->engine_type)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_ENGINE_ID) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint8(fmt_buf+len, *((u_int8*)(rec+fo->engine_id)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_SRCADDR) {
    if (comma) fmt_buf[len++] = ',';
    if (quote) fmt_buf[len++] = '"';
    len += fmt_ipv4(fmt_buf+len, *((u_int32*)(rec+fo->srcaddr)),
      FMT_JUST_LEFT);
    if (quote) fmt_buf[len++] = '"';
    comma = 1;
  }

  if (xfields & FT_XFIELD_DSTADDR) {
    if (comma) fmt_buf[len++] = ',';
    if (quote) fmt_buf[len++] = '"';
    len += fmt_ipv4(fmt_buf+len, *((u_int32*)(rec+fo->dstaddr)),
      FMT_JUST_LEFT);
    if (quote) fmt_buf[len++] = '"';
    comma = 1;
  }

  if (xfields & FT_XFIELD_NEXTHOP) {
    if (comma) fmt_buf[len++] = ',';
    if (quote) fmt_buf[len++] = '"';
    len += fmt_ipv4(fmt_buf+len, *((u_int32*)(rec+fo->nexthop)),
      FMT_JUST_LEFT);
    if (quote) fmt_buf[len++] = '"';
    comma = 1;
  }

  if (xfields & FT_XFIELD_INPUT) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint16(fmt_buf+len, *((u_int16*)(rec+fo->input)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_OUTPUT) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint16(fmt_buf+len, *((u_int16*)(rec+fo->output)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_SRCPORT) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint16(fmt_buf+len, *((u_int16*)(rec+fo->srcport)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_DSTPORT) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint16(fmt_buf+len, *((u_int16*)(rec+fo->dstport)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_PROT) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint8(fmt_buf+len, *((u_int8*)(rec+fo->prot)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_TOS) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint8(fmt_buf+len, *((u_int8*)(rec+fo->tos)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_TCP_FLAGS) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint8(fmt_buf+len, *((u_int8*)(rec+fo->tcp_flags)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_SRC_MASK) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint8(fmt_buf+len, *((u_int8*)(rec+fo->src_mask)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_DST_MASK) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint8(fmt_buf+len, *((u_int8*)(rec+fo->dst_mask)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_SRC_AS) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint16(fmt_buf+len, *((u_int16*)(rec+fo->src_as)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_DST_AS) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint16(fmt_buf+len, *((u_int16*)(rec+fo->dst_as)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_IN_ENCAPS) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint8(fmt_buf+len, *((u_int8*)(rec+fo->in_encaps)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_OUT_ENCAPS) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint8(fmt_buf+len, *((u_int8*)(rec+fo->out_encaps)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_PEER_NEXTHOP) {
    if (comma) fmt_buf[len++] = ',';
    if (quote) fmt_buf[len++] = '"';
    len += fmt_ipv4(fmt_buf+len, *((u_int32*)(rec+fo->peer_nexthop)),
      FMT_JUST_LEFT);
    if (quote) fmt_buf[len++] = '"';
    comma = 1;
  }

  if (xfields & FT_XFIELD_ROUTER_SC) {
    if (comma) fmt_buf[len++] = ',';
    if (quote) fmt_buf[len++] = '"';
    len += fmt_ipv4(fmt_buf+len, *((u_int32*)(rec+fo->router_sc)),
      FMT_JUST_LEFT);
    if (quote) fmt_buf[len++] = '"';
    comma = 1;
  }

  if (xfields & FT_XFIELD_MARKED_TOS) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint8(fmt_buf+len, *((u_int8*)(rec+fo->marked_tos)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_EXTRA_PKTS) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint32(fmt_buf+len, *((u_int32*)(rec+fo->extra_pkts)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_SRC_TAG) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint32(fmt_buf+len, *((u_int32*)(rec+fo->src_tag)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  if (xfields & FT_XFIELD_DST_TAG) {
    if (comma) fmt_buf[len++] = ',';
    len += fmt_uint32(fmt_buf+len, *((u_int32*)(rec+fo->dst_tag)),
      FMT_JUST_LEFT);
    comma = 1;
  }

  /* if ended on a quote string may not be null terminated */
  fmt_buf[len++] = 0;

  return strlen(fmt_buf);

} /* fmt_xfields_val */

void usage(void) {

  fprintf(stderr, "Usage: flow-export [-h] [-d debug_level] [-f format] [-m mask_fields] -u [database URI]\n");
  fprintf(stderr, "\n%s version %s: built by %s\n", PACKAGE, VERSION, FT_PROG_BUILD);


} /* usage */

/*
 * function ftxfield_tocflow
 *    
 * convert flow-tools xfield bits to cflowd bits
 *
 * returns 0:  ok
 *         <0  fail
 */
int ftxfield_tocflow(u_int64 xfields, u_int32 *cfmask)
{
  int droptime;

  droptime = 0;

  if (xfields & FT_XFIELD_EXADDR) {
    xfields &= ~FT_XFIELD_EXADDR;
    *cfmask |= CF_ROUTERMASK;
  }

  if (xfields & FT_XFIELD_SRCADDR) {
    xfields &= ~FT_XFIELD_SRCADDR;
    *cfmask |= CF_SRCIPADDRMASK;
  }

  if (xfields & FT_XFIELD_DSTADDR) {
    xfields &= ~FT_XFIELD_DSTADDR;
    *cfmask |= CF_DSTIPADDRMASK;
  }

  if (xfields & FT_XFIELD_INPUT) {
    xfields &= ~FT_XFIELD_INPUT;
    *cfmask |= CF_INPUTIFINDEXMASK;
  }

  if (xfields & FT_XFIELD_OUTPUT) {
    xfields &= ~FT_XFIELD_OUTPUT;
    *cfmask |= CF_OUTPUTIFINDEXMASK;
  }

  if (xfields & FT_XFIELD_SRCPORT) {
    xfields &= ~FT_XFIELD_SRCPORT;
    *cfmask |= CF_SRCPORTMASK;
  }

  if (xfields & FT_XFIELD_DSTPORT) {
    xfields &= ~FT_XFIELD_DSTPORT;
    *cfmask |= CF_DSTPORTMASK;
  }

  if (xfields & FT_XFIELD_DPKTS) {
    xfields &= ~FT_XFIELD_DPKTS;
    *cfmask |= CF_PKTSMASK;
  }

  if (xfields & FT_XFIELD_DOCTETS) {
    xfields &= ~FT_XFIELD_DOCTETS;
    *cfmask |= CF_BYTESMASK;
  }

  if (xfields & FT_XFIELD_NEXTHOP) {
    xfields &= ~FT_XFIELD_NEXTHOP;
    *cfmask |= CF_IPNEXTHOPMASK;
  }

  if ((xfields &
    (FT_XFIELD_UNIX_SECS|FT_XFIELD_UNIX_NSECS|FT_XFIELD_SYSUPTIME|
    FT_XFIELD_FIRST)) == 
    (FT_XFIELD_UNIX_SECS|FT_XFIELD_UNIX_NSECS|FT_XFIELD_SYSUPTIME|
    FT_XFIELD_FIRST)) {
    xfields &= ~FT_XFIELD_NEXTHOP;
    droptime = 1;
    *cfmask |= CF_STARTTIMEMASK;
  }

  if ((xfields &
    (FT_XFIELD_UNIX_SECS|FT_XFIELD_UNIX_NSECS|FT_XFIELD_SYSUPTIME|
    FT_XFIELD_LAST)) == 
    (FT_XFIELD_UNIX_SECS|FT_XFIELD_UNIX_NSECS|FT_XFIELD_SYSUPTIME|
    FT_XFIELD_LAST)) {
    xfields &= ~FT_XFIELD_NEXTHOP;
    droptime = 1;
    *cfmask |= CF_ENDTIMEMASK;
  }

  if (droptime)
    xfields &= ~(FT_XFIELD_UNIX_SECS|FT_XFIELD_UNIX_NSECS|FT_XFIELD_SYSUPTIME);

  if (xfields & FT_XFIELD_PROT) {
    xfields &= ~FT_XFIELD_PROT;
    *cfmask |= CF_PROTOCOLMASK;
  }

  if (xfields & FT_XFIELD_TOS) {
    xfields &= ~FT_XFIELD_TOS;
    *cfmask |= CF_TOSMASK;
  }

  if (xfields & FT_XFIELD_SRC_AS) {
    xfields &= ~FT_XFIELD_SRC_AS;
    *cfmask |= CF_SRCASMASK;
  }

  if (xfields & FT_XFIELD_DST_AS) {
    xfields &= ~FT_XFIELD_DST_AS;
    *cfmask |= CF_DSTASMASK;
  }

  if (xfields & FT_XFIELD_SRC_MASK) {
    xfields &= ~FT_XFIELD_SRC_MASK;
    *cfmask |= CF_SRCMASKLENMASK;
  }

  if (xfields & FT_XFIELD_DST_MASK) {
    xfields &= ~FT_XFIELD_DST_MASK;
    *cfmask |= CF_DSTMASKLENMASK;
  }

  if (xfields & FT_XFIELD_TCP_FLAGS) {
    xfields &= ~FT_XFIELD_TCP_FLAGS ;
    *cfmask |= CF_TCPFLAGSMASK;
  }

  if (xfields & FT_XFIELD_IN_ENCAPS) {
    xfields &= ~FT_XFIELD_IN_ENCAPS ;
    *cfmask |= CF_INPUTENCAPMASK;
  }

  if (xfields & FT_XFIELD_OUT_ENCAPS) {
    xfields &= ~FT_XFIELD_OUT_ENCAPS ;
    *cfmask |= CF_OUTPUTENCAPMASK;
  }

  if (xfields & FT_XFIELD_PEER_NEXTHOP) {
    xfields &= ~FT_XFIELD_PEER_NEXTHOP ;
    *cfmask |= CF_PEERNEXTHOPMASK;
  }

  if (xfields & FT_XFIELD_ENGINE_TYPE) {
    xfields &= ~FT_XFIELD_ENGINE_TYPE ;
    *cfmask |= CF_ENGINETYPEMASK;
  }

  if (xfields & FT_XFIELD_ENGINE_ID) {
    xfields &= ~FT_XFIELD_ENGINE_ID ;
    *cfmask |= CF_ENGINEIDMASK;
  }

  if (xfields) {
    fterr_warnx("xfields contains fields after conversion to cflow mask 0x%llX",
      xfields);
    return -1;
  }

  return 0;
 
} /* ftxfield_tocflow */

