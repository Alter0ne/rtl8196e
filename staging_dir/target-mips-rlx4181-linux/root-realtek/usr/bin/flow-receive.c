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
 *      $Id: flow-receive.c,v 1.54 2005/05/10 15:52:39 maf Exp $
 */

#include "ftconfig.h"
#include <ftlib.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
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
void sig_quit(int);

#define SELECT_TIMEOUT 1   /* 1 second */

int main(int argc, char **argv)
{
#ifdef IP_ADD_MEMBERSHIP
  struct sockaddr_in tmp_addr;
  struct ip_mreq mr;
#ifdef IP_ADD_SOURCE_MEMBERSHIP
  struct ip_mreq_source mrs;
#endif
#endif
  struct timeval tv;
  struct tm *tm;
  struct stat stat_buf;
  struct ftio ftio;
  struct ftset ftset;
  struct ftpdu ftpdu;
  struct ftnet ftnet;
  struct ftpeeri ftpi;
  struct ftver ftv;
  struct ftchash *ftch;
  struct ftchash_rec_exp ftch_recexp, *ftch_recexpp;
  struct fts3rec_offsets fo;
  time_t now, time_startup;
  int i, n, offset, out_fd, out_fd_plain, one;
  unsigned int v1, v2;
  fd_set rfd;
  char *out_fname;
  u_int32 nflows, time_start, time_end;
  u_int32 flows_corrupt, flows_lost, flows_reset;
  u_int32 hash;
  char fmt_src_ip[32], fmt_dst_ip[32], fmt_dst_port[32];
  char xl_rec[FT_IO_MAXREC], *out_rec;
  int stat_interval, stat_next;

  time_startup = time((time_t)0L);

  out_fname = (char*)0L;
  nflows = 0;
  out_fd_plain = 0;
  out_fd = -1;
  ftset.z_level = 0;
  done = 0;
  bzero(&tv, sizeof tv);
  bzero(&ftnet, sizeof ftnet);
  bzero(&ftv, sizeof ftv);
  bzero(&ftpdu, sizeof ftpdu);
  bzero(&ftch_recexp, sizeof ftch_recexp);
  flows_corrupt = flows_lost = flows_reset = 0;
  stat_interval = 0;
  stat_next = -1;

  /* init fterr */
  fterr_setid(argv[0]);

  /* defaults + no compression */
  ftset_init(&ftset, 0);

  /* listen for PDU's */
  ftnet.loc_addr.sin_family = AF_INET;
  ftnet.loc_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  ftnet.loc_addr.sin_port = htons(FT_PORT);

  while ((i = getopt(argc, argv, "b:C:d:h?o:S:V:z:")) != -1)

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

    case 'h': /* help */
    case '?':
      usage();
      exit (0);
      break;

    case 'o': /* output filename */
      out_fname = optarg;
      break;

    case 'S': /* stat interval */
      stat_interval = atoi(optarg);
      if ((stat_interval < 0) || (stat_interval > 60))
        fterr_errx(1, "Stat interval must be between 0 and 60.");
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
      break;

    } /* switch */

  /* number of unprocessed options */
  n = argc - optind;

  /* loc_ip/rem_ip/port */
  if (n == 1) {

    ftpi = scan_peeri(argv[optind]);

    ftnet.rem_ip = ftpi.rem_ip;
    ftnet.loc_ip = ftpi.loc_ip;

    if (ftpi.dst_port)
      ftnet.dst_port = ftpi.dst_port;
    else
      ftnet.dst_port = FT_PORT;

    ftnet.loc_addr.sin_addr.s_addr = htonl(ftpi.loc_ip);
    ftnet.loc_addr.sin_port = htons(ftnet.dst_port);

  } else if (n == 0) {
    /* defaults */
  } else {
    fterr_errx(1, "Illegal option: %s", argv[optind+1]);
  }

  /*
   * configure signal handlers
   */

  if (mysignal(SIGQUIT, sig_quit) == SIG_ERR)
    fterr_err(1, "signal(SIGQUIT)");

  if (mysignal(SIGHUP, sig_quit) == SIG_ERR)
    fterr_err(1, "signal(SIGHUP)");

  if (mysignal(SIGINT, sig_quit) == SIG_ERR)
    fterr_err(1, "signal(SIGINT)");

  if (mysignal(SIGTERM, sig_quit) == SIG_ERR)
    fterr_err(1, "signal(SIGTERM)");

  /* get hostname */
  if (gethostname((char*)&ftset.hnbuf, (int)FT_HOSTNAME_LEN-1) == -1)
    fterr_err(1, "gethostname()");

  ftset.hnbuf[FT_HOSTNAME_LEN-1] = 0;

  if ((ftnet.fd = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
    fterr_err(1, "socket()");

  if (bigsockbuf(ftnet.fd, SO_RCVBUF, FT_SO_RCV_BUFSIZE) < 0)
    fterr_err(1, "bigsockbuf()");

/* multicast capable? */
#ifdef IP_ADD_MEMBERSHIP
  
  if (IN_CLASSD(ftpi.rem_ip)) {

    /* source is the first arg now */
    ftnet.rem_ip = ftpi.loc_ip;
    ftnet.loc_ip = ftpi.rem_ip;

    /* socket API usually requires INADDR_ANY
     * and s/g/port identifier does not have a source interface field
     * to use here
     */
    bzero(&tmp_addr, sizeof tmp_addr);
    tmp_addr.sin_family = AF_INET;
    tmp_addr.sin_port = htons(ftnet.dst_port);

    one = 1;

    /* Multicast streams may have multiple receivers */
    if (setsockopt(ftnet.fd, SOL_SOCKET, SO_REUSEADDR, (char *)&one,
      sizeof(one)) < 0)
      fterr_err(1, "setsockopt(SO_REUSEADDR)");

    if (bind(ftnet.fd, (struct sockaddr*)&tmp_addr,
      sizeof(struct sockaddr)) < 0)
      fterr_err(1, "bind(mcast-rcv)");

#ifdef IP_ADD_SOURCE_MEMBERSHIP

    /* ssm address? */
    if (IN_CLASSD_SSM(ftpi.rem_ip)) {

      mrs.imr_sourceaddr.s_addr = htonl(ftpi.loc_ip);
      mrs.imr_multiaddr.s_addr = htonl(ftpi.rem_ip);
      mrs.imr_interface.s_addr = INADDR_ANY;

      if (setsockopt(ftnet.fd, IPPROTO_IP, IP_ADD_SOURCE_MEMBERSHIP,
        (char*)&mrs, sizeof(mrs)) < 0)
        fterr_err(1, "setsockopt(IP_ADD_SOURCE_MEMBERSHIP)");

    }

    goto mcast_done;
#endif /* IP_ADD_SOURCE_MEMBERSHIP */

    mr.imr_multiaddr.s_addr = htonl(ftpi.rem_ip);
    mr.imr_interface.s_addr = INADDR_ANY;
    
    if (setsockopt(ftnet.fd, IPPROTO_IP, IP_ADD_MEMBERSHIP,
      (char *)&mr, sizeof(mr)) < 0)
      fterr_err(1, "setsockopt(IP_ADD_MEMBERSHIP)");


  } else { /* is a multicast group */

    /* unicast bind -- multicast support */
    if (bind(ftnet.fd, (struct sockaddr*)&ftnet.loc_addr,
      sizeof(ftnet.loc_addr)) < 0)
      fterr_err(1, "bind()");

  } /* not multicast group */

#ifdef IP_ADD_SOURCE_MEMBERSHIP
mcast_done:
#endif /* IP_ADD_SOURCE_MEMBERSHIP */

#else /* IP_ADD_MEMBERSHIP */

  /* unicast bind -- no multicast support */
  if (bind(ftnet.fd, (struct sockaddr*)&ftnet.loc_addr,
    sizeof(ftnet.loc_addr)) < 0)
    fterr_err(1, "bind()");

#endif /* IP_ADD_MEMBERSHIP */

#ifdef IP_RECVDSTADDR
  one = 1;
  /* return the destination IP address */
  if (setsockopt(ftnet.fd, IPPROTO_IP, IP_RECVDSTADDR, (char *)&one,
    sizeof(one)) < 0)
    fterr_err(1, "setsockopt(IP_RECVDSTADDR)");
#else
#ifdef IP_PKTINFO
  one = 1;
  /* return the destination IP address */
  if (setsockopt(ftnet.fd, IPPROTO_IP, IP_PKTINFO, (char *)&one,
    sizeof(one)) < 0)
    fterr_err(1, "setsockopt(IP_PKTINFO)");
#endif /* else */
#endif /* IP_RECVDSTADDR */

  /* if out_fname is not set, then use stdout */
  if (out_fname) {

    if ((out_fd = open(out_fname,  O_WRONLY|O_CREAT|O_TRUNC, 0644)) == -1)
      fterr_err(1, "open(%s)", out_fname);

    if (fstat(out_fd, &stat_buf) == -1)
      fterr_err(1, "fstat(%s)", out_fname);
      
    /* is this a plain file? */
    if (!stat_buf.st_rdev)
      out_fd_plain = 1;
    
  } else
    out_fd = 1;


  /* output to out_fd */
  if (ftio_init(&ftio, out_fd, FT_IO_FLAG_NO_SWAP | FT_IO_FLAG_WRITE |
    ((ftset.z_level) ? FT_IO_FLAG_ZINIT : 0) ) < 0)
    fterr_errx(1, "ftio_init(): failed");

  time_start = (u_int32)time((time_t)0L);

  ftio_set_comment(&ftio, ftset.comments);
  ftio_set_cap_hostname(&ftio, ftset.hnbuf);
  ftio_set_byte_order(&ftio, ftset.byte_order);
  ftio_set_z_level(&ftio, ftset.z_level);
  if (out_fd_plain)
    ftio_set_cap_time(&ftio, time_start, 0);
  else
    ftio_set_cap_time_start(&ftio, time_start);
  ftio_set_debug(&ftio, debug);
  ftio_set_streaming(&ftio, 1);

/*  ftio_map_load(&ftio, FT_FILE_MAP, ftnet.rem_ip); */

  /* header must be full size on initial write */
  if (out_fd_plain) {
    ftio_set_flows_count(&ftio, nflows);
    ftio_set_corrupt(&ftio, flows_corrupt);
    ftio_set_lost(&ftio, flows_lost);
    ftio_set_reset(&ftio, flows_reset);
    ftio_set_flows_count(&ftio, nflows);
  }

  /* if version set on command line, write out header here */
  if (ftv.set) {

    if (ftio_set_ver(&ftio, &ftv) < 0)
      fterr_errx(1, "ftio_set_ver(): failed");

    /* need offsets for filter later */
    fts3rec_compute_offsets(&fo, &ftv);

    /* header first */
    if (ftio_write_header(&ftio) < 0)
      fterr_errx(1, "ftio_write_header()");

  }

  /* init hash table for demuxing exporters */
  if (!(ftch = ftchash_new(256, sizeof (struct ftchash_rec_exp), 12, 1)))
    fterr_errx(1, "ftchash_new(): failed");

  /* init msg block */
  ftnet.iov[0].iov_len = sizeof ftpdu.buf;
  ftnet.iov[0].iov_base = (char*)&ftpdu.buf;
  ftnet.msg.msg_iov = (struct iovec*)&ftnet.iov;
  ftnet.msg.msg_iovlen = 1;
  ftnet.msg.msg_name = &ftnet.rem_addr;
  ftnet.msg.msg_namelen = sizeof ftnet.rem_addr;
  ftnet.msg.msg_control = &ftnet.msgip;
  ftnet.msg.msg_controllen = sizeof ftnet.msgip;

  /* default timeout waiting for an active fd */
  tv.tv_sec = SELECT_TIMEOUT;

  while (1) {

    FD_ZERO (&rfd);
    FD_SET (ftnet.fd, &rfd);

    if (select (ftnet.fd+1, &rfd, (fd_set *)0, (fd_set *)0, &tv) < 0) {
      if (errno == EINTR) {
        FD_ZERO (&rfd);
      } else {
        fterr_err(1, "select()");
      }
    }

    now = time((time_t)0L);

    /* reset */
    bzero (&tv, sizeof tv);
    tv.tv_sec = SELECT_TIMEOUT;

    if (stat_interval) {

      tm = localtime (&now);

      /*
       * note there is an obscure race condition here if this
       * code is not reached at least every stat_interval*60 seconds
       * where up to 1 hour of STAT lines would not show up.
       * This is highly unlikely and not handled.
       */

      if ((tm->tm_min == stat_next) || (stat_next == -1)) {

        ftchash_first(ftch);

        while ((ftch_recexpp = ftchash_foreach(ftch))) {

          fmt_ipv4(fmt_src_ip, ftch_recexpp->src_ip, FMT_JUST_LEFT);
          fmt_ipv4(fmt_dst_ip, ftch_recexpp->dst_ip, FMT_JUST_LEFT);

          fterr_info(
            "STAT: now=%lu startup=%lu src_ip=%s dst_ip=%s d_ver=%d pkts=%lu flows=%lu lost=%lu reset=%lu",
            (unsigned long)now, (unsigned long)time_startup,
            fmt_src_ip, fmt_dst_ip,
            ftch_recexpp->d_version, (u_long)ftch_recexpp->packets,
            (u_long)ftch_recexpp->flows, (u_long)ftch_recexpp->lost,
            (u_long)ftch_recexpp->reset);

        }

        stat_next = (tm->tm_min + (stat_interval - tm->tm_min % stat_interval))
          % 60;

      }

    } /* stat_inverval */

    if (done) {
      fterr_info("Cleaning up");
      break;
    }

    if (FD_ISSET(ftnet.fd, &rfd)) {

restart_recvmsg:

      if ((ftpdu.bused = recvmsg(ftnet.fd,
        (struct msghdr*)&ftnet.msg, 0)) < 0) {

        if (errno == EAGAIN)
          goto restart_recvmsg;

        fterr_err(1, "recvmsg()");

      }

#ifdef IP_RECVDSTADDR
      /* got destination IP back? */
      if ((ftnet.msgip.hdr.cmsg_level == IPPROTO_IP) &&
          (ftnet.msgip.hdr.cmsg_type == IP_RECVDSTADDR)) {
          ftnet.loc_addr.sin_addr.s_addr = ftnet.msgip.ip.s_addr;
      } else {
        ftnet.loc_addr.sin_addr.s_addr = 0;
      }
#else
#ifdef IP_PKTINFO
      if ((ftnet.msgip.hdr.cmsg_level == IPPROTO_IP) &&
          (ftnet.msgip.hdr.cmsg_type == IP_PKTINFO)) {
          ftnet.loc_addr.sin_addr.s_addr = ftnet.msgip.pktinfo.ipi_addr.s_addr;
      } else {
        ftnet.loc_addr.sin_addr.s_addr = 0;
      }
#else
      ftnet.loc_addr.sin_addr.s_addr = 0;
#endif
#endif /* IP_RECVDSTADDR */

      /* fill in hash key */
      ftch_recexp.src_ip = htonl(ftnet.rem_addr.sin_addr.s_addr);
      ftch_recexp.dst_ip = htonl(ftnet.loc_addr.sin_addr.s_addr);
      ftch_recexp.dst_port = ftnet.dst_port;

      /* verify integrity, get version */
      if (ftpdu_verify(&ftpdu) < 0) {
        fmt_ipv4(fmt_src_ip, ftch_recexp.src_ip, FMT_JUST_LEFT);
        fterr_warnx("ftpdu_verify(): src_ip=%s failed.", fmt_src_ip);
        flows_corrupt ++;
        goto skip1;
      }

      /* rest of hash key */
      ftch_recexp.d_version = ftpdu.ftv.d_version;

      /* if exporter src IP has been configured then make sure it matches */
      if (ftnet.rem_ip && (ftnet.rem_ip != ftch_recexp.src_ip)) {
        fmt_ipv4(fmt_src_ip, ftch_recexp.src_ip, FMT_JUST_LEFT);
        fterr_warnx("Unexpected PDU: src_ip=%s not configured", fmt_src_ip);
        flows_corrupt ++;
        goto skip1;
      }

      /* first flow or no configured destination version? */
      if (!ftv.set) {

        /* set the version information in the io stream */
        if (ftio_set_ver(&ftio, &ftpdu.ftv) < 0)
          fterr_errx(1, "ftio_set_ver(): failed");

        /* copy to compare next time */
        bcopy(&ftpdu.ftv, &ftv, sizeof ftv);

        /* flag struct as configured */
        ftv.set = 1;

        /* need offsets for filter later */
        fts3rec_compute_offsets(&fo, &ftv);

        /* header first */
        if (ftio_write_header(&ftio) < 0)
          fterr_errx(1, "ftio_write_header(): failed");

      } else {

        /* translation to/from v8 not possible */
        if (((ftv.d_version == 8) && (ftpdu.ftv.d_version != 8)) ||
            ((ftv.d_version != 8) && (ftpdu.ftv.d_version == 8))) {
          fmt_ipv4(fmt_src_ip, ftch_recexp.dst_ip, FMT_JUST_LEFT);
          fterr_warnx("Unexpected PDU: src_ip=%s no v8 translation",
            fmt_src_ip);
          ++flows_corrupt;
          goto skip1;
        }

        /* translation among v8 aggregation methods not possible */
        if ((ftv.d_version == 8) && ((ftv.agg_method != ftpdu.ftv.agg_method)
          || (ftv.agg_version != ftpdu.ftv.agg_version))) {
          fmt_ipv4(fmt_src_ip, ftch_recexp.dst_ip, FMT_JUST_LEFT);
          fterr_warnx(
            "Unexpected PDU: src_ip=%s multi v8 oagg=%d agg=%d over=%d ver=%d",
            fmt_src_ip, (int)ftv.agg_method, (int)ftpdu.ftv.agg_method,
            (int)ftv.agg_version, (int)ftpdu.ftv.agg_version);
          ++flows_corrupt;
          goto skip1;
        }

      } /* version processing */

      /* compute 8 bit hash */
      hash = (ftch_recexp.src_ip & 0xFF);
      hash ^= (ftch_recexp.src_ip>>24);
      hash ^= (ftch_recexp.dst_ip & 0xFF);
      hash ^= (ftch_recexp.dst_ip>>24);
      hash ^= (ftch_recexp.d_version & 0xFF);

      if (!(ftch_recexpp = ftchash_update(ftch, &ftch_recexp, hash)))
        fterr_errx(1, "ftch_update(): failed");

      /* if the packet count is 0, then this is a new entry */
      if (ftch_recexpp->packets == 0) {

        fmt_ipv4(fmt_src_ip, ftch_recexp.src_ip, FMT_JUST_LEFT);
        fmt_ipv4(fmt_dst_ip, ftch_recexp.dst_ip, FMT_JUST_LEFT);
        fterr_info("New exporter: time=%lu src_ip=%s dst_ip=%s d_version=%d",
          (u_long)now, fmt_src_ip, fmt_dst_ip, (int)ftpdu.ftv.d_version);

        /* set translation function */
        if (ftch_recexp.d_version != ftv.d_version)
          ftch_recexpp->xlate = ftrec_xlate_func(&ftpdu.ftv, &ftv);

      }

      /* verify sequence number */
      if (ftpdu_check_seq(&ftpdu, &(ftch_recexpp->ftseq)) < 0) {
        fmt_ipv4(fmt_src_ip, ftch_recexp.src_ip, FMT_JUST_LEFT);
        fmt_ipv4(fmt_dst_ip, ftch_recexp.dst_ip, FMT_JUST_LEFT);
        fmt_uint16(fmt_dst_port, ftch_recexp.dst_port, FMT_JUST_LEFT);
        fterr_warnx(
          "ftpdu_seq_check(): src_ip=%s dst_ip=%s d_version=%d expecting=%lu received=%lu lost=%lu",
          fmt_src_ip, fmt_dst_ip, (int)ftpdu.ftv.d_version,
          (u_long)ftch_recexpp->ftseq.seq_exp,
          (u_long)ftch_recexpp->ftseq.seq_rcv,
          (u_long)ftch_recexpp->ftseq.seq_lost);

        /* only count these lost if "lost" is a reasonable number */
        if (ftch_recexpp->ftseq.seq_lost < FT_SEQ_RESET) {
          flows_lost += ftch_recexpp->ftseq.seq_lost;
          ftch_recexpp->lost += ftch_recexpp->ftseq.seq_lost;
        } else {
          flows_reset ++;
          ftch_recexpp->reset ++;
        }
      }


      /* decode */
      ftpdu.ftd.byte_order = ftset.byte_order;
      ftpdu.ftd.exporter_ip = ftch_recexp.src_ip;  
      n = fts3rec_pdu_decode(&ftpdu);

      /* update the exporter stats */
      ftch_recexpp->packets ++;
      ftch_recexpp->flows += n;

      /* write decoded flows */
      for (i = 0, offset = 0; i < n; ++i, offset += ftpdu.ftd.rec_size) {

       /* translate version? */
        if (ftch_recexpp->xlate) {

          ftch_recexpp->xlate(ftpdu.ftd.buf+offset, &xl_rec);

          out_rec = (char*)&xl_rec;

        } else {

          out_rec = (char*)ftpdu.ftd.buf+offset;

        }

        ++nflows;

        if (ftio_write(&ftio, out_rec) < 0)
          fterr_errx(1, "ftio_write(): failed");

      } /* for */

skip1:
      continue;

    } /* if FD_ISSET */

  } /* while 1 */

  /* rewrite header with updated info */
  if (out_fd_plain) {
  
    time_end = (u_int32)time((time_t)0L);

    ftio_set_cap_time(&ftio, time_start, time_end);
    ftio_set_flows_count(&ftio, nflows);
    ftio_set_streaming(&ftio, 0);
    ftio_set_corrupt(&ftio, flows_corrupt);
    ftio_set_lost(&ftio, flows_lost);
    ftio_set_reset(&ftio, flows_reset);

    if (ftio_write_header(&ftio) < 0)
      fterr_errx(1, "ftio_write_header(): failed");

  }

  /* close stream */
  if (ftio_close(&ftio) < 0)
    fterr_errx(1, "ftio_close(): failed");

  /* close input */
  close (ftnet.fd);

  return 0;

} /* main */

void sig_quit(int sig)
{
  done = 1;
} /* sig_quit */

void usage(void) {

  fprintf(stderr, "Usage: flow-receive [-h] [-b big|little] [-C comment]\n");
  fprintf(stderr, "       [-d debug_level] [-o output_file] [-S stat_interval]\n");
  fprintf(stderr, "       [-f filter_name] [-F filter_definition]\n");
  fprintf(stderr, "       [-t tag_fname] [-T tag_active] [-V pdu_version] [-z z_level]\n");
  fprintf(stderr, "       localip/remoteip/port\n");
  fprintf(stderr, "\n%s version %s: built by %s\n", PACKAGE, VERSION, FT_PROG_BUILD);

} /* usage */

