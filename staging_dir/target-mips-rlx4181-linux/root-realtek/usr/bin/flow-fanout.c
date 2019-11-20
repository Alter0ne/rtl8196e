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
 *      $Id: flow-fanout.c,v 1.44 2005/05/10 15:52:52 maf Exp $
 */

#include "ftconfig.h"
#include <ftlib.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <netinet/in_systm.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netinet/udp.h>
#include <arpa/inet.h>
#include <errno.h>
#include <syslog.h>
#include <signal.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <time.h>

#if HAVE_STRINGS_H
 #include <strings.h>
#endif

#if HAVE_STRING_H
  #include <string.h>
#endif

#include "ftbuild.h"

#define FANOUT_PIDFILE    "/var/run/flow-fanout.pid"

#define SELECT_TIMEOUT 5   /* 5 seconds */


int debug;
char *pidfile;

void usage(void);

struct peer {
  int fd;
  struct sockaddr_in loc_addr; /* us */
  struct sockaddr_in rem_addr; /* them */
  int port;
  int ttl;
};

void fterr_exit_handler(int code);

int sig_quit_flag, sig_hup_flag, sig_term_flag, reload_flag;
void sig_quit(int), sig_hup(int), sig_term(int);
pid_t pid;
u_int16 listen_port;

void pdu_xmit(int npeers, int tx_delay, int src_ip_spoof, int hdr_len,
  u_int32 *send_nobufs, struct ip *ip_hdr, struct udphdr *udp_hdr,
  struct ftencode *fte, struct peer *peers, struct ftnet *ftnet);

int main(int argc, char **argv)
{
  struct sockaddr_in tmp_addr;
#ifdef IP_ADD_MEMBERSHIP
  struct ip_mreq mr;
#ifdef IP_ADD_SOURCE_MEMBERSHIP
  struct ip_mreq_source mrs;
#endif
#endif
  struct timeval tv;
  struct tm *tm;
  time_t now, time_startup;
  fd_set rfd;
  struct ip *ip_hdr;
  struct udphdr *udp_hdr;
  struct ftpeeri ftpi;
  struct ftpdu ftpdu;
  struct ftver ftv;
  struct ftnet ftnet;
  struct ftchash *ftch;
  struct ftchash_rec_exp ftch_recexp, *ftch_recexpp;
  struct ftencode fte;
  struct ftfil ftfil;
  struct ftfil_def *ftfd;
  struct fts3rec_offsets fo;
  struct ftset ftset;
  struct ftvar ftvar;
  struct peer *peers;
  struct ftipmask ftipmask;
  u_int32 flows_corrupt, flows_lost, flows_reset, hash, privacy_mask;
  u_int32 send_nobufs;
  unsigned int v1, v2;
  char fmt_src_ip[32], fmt_dst_ip[32], fmt_dst_port[32];
  char xl_rec[FT_IO_MAXREC], *out_rec;
  char *filter_fname, *filter_active;
  int i, n, detach, one, ret, offset, hdr_len;
  int npeers, tx_delay;
  int stat_interval, stat_next, src_ip_spoof;

  time_startup = time((time_t)0L);

  /* init fterr */
  fterr_setid(argv[0]);
  fterr_setexit(fterr_exit_handler);

  bzero(&ftpdu, sizeof ftpdu);
  bzero(&ftv, sizeof ftv);
  bzero(&ftnet, sizeof ftnet);
  bzero(&ftch_recexp, sizeof ftch_recexp);
  bzero(&tv, sizeof tv);
  bzero(&ftfil, sizeof ftfil);
  bzero(&ftvar, sizeof ftvar);
  stat_interval = 0;
  stat_next = -1;
  src_ip_spoof = 0; /* no */
  hdr_len = 0;
  send_nobufs = 0;
  reload_flag = 1; /* yes */

  /* init var binding */
  if (ftvar_new(&ftvar) < 0)
    fterr_errx(1, "ftvar_new(): failed");

  filter_fname = FT_PATH_CFG_FILTER;
  filter_active = (char*)0L;
  ftfd = (struct ftfil_def*)0L;

  /* pidfile */
  pidfile = FANOUT_PIDFILE;

  /* by default do not mask any src/dst ip addr bits */
  privacy_mask = 0xFFFFFFFF;

  /* defaults + default compression */
  ftset_init(&ftset, Z_DEFAULT_COMPRESSION);

  flows_corrupt = flows_lost = flows_reset = 0;

  debug = 0;
  tx_delay = 0;
  detach = 1;

  while ((i = getopt(argc, argv, "A:d:Df:F:hm:p:sS:v:V:x:")) != -1)
    switch (i) {

    case 'A': /* AS substitution */
      ftset.as_sub = atoi(optarg);
      break;

    case 'd': /* debug */
      debug = atoi(optarg);
      break;

    case 'D': /* detach */
      detach = 0;
      break;

    case 'f': /* filter fname */
      filter_fname = optarg;
      break;
  
    case 'F': /* filter active */
      filter_active = optarg;
      break;

    case 'h': /* help */
      usage();
      exit (0);
      break;

    case 'm': /* privacy mask */
      privacy_mask = scan_ip(optarg);
      break;

    case 'p': /* pidfile */
      if ((optarg[0] == 0) || ((optarg[0] == '-') && (optarg[1] == 0)))
        pidfile = (char*)0L;
      else
        pidfile = optarg;
      break;

    case 's': /* source ip preserve */
      src_ip_spoof = 1;
      break;

    case 'S': /* stat interval */
      stat_interval = atoi(optarg);
      if ((stat_interval < 0) || (stat_interval > 60))
        fterr_errx(1, "Stat interval must be between 0 and 60.");
      break;

    case 'v': /* variable */
      if (ftvar_pset(&ftvar, optarg) < 0)
        fterr_errx(1, "ftvar_pset(%s): failed", optarg);
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
      } else {
        fterr_errx(1, "Version scan failed");
      }
      break;

    case 'x': /* transmit delay */
      tx_delay = atoi(optarg);
      break;

    default:
      usage();
      exit (1);
      break;

    } /* switch */

  /* initialize encode struct */
  ftencode_init(&fte, (src_ip_spoof) ? FT_ENC_FLAGS_IPHDR : 0);

  /* if src ip spoofing, initialize the IP/UDP header */
  if (src_ip_spoof) {

    ip_hdr = (struct ip*)&fte.buf;
    udp_hdr = (struct udphdr*)((char*)&fte.buf + sizeof (*ip_hdr));

    ip_hdr->ip_hl = 5;
    ip_hdr->ip_v = 4;
    ip_hdr->ip_p = 17; /* UDP */
    hdr_len = FT_ENC_IPHDR_LEN;

  }

  /* initialize encoder version */
  if (ftv.set)
    bcopy(&ftv, &fte.ver, sizeof ftv);

  /* allocate argc - optind peer entries */
  npeers = argc - optind - 1;

  if (npeers < 1)
    fterr_errx(1, "Must define at least source and one destination.");

  /* pre-scan so write_pidfile has the port */
  ftpi = scan_peeri(argv[optind]);

  listen_port = (ftpi.dst_port) ? ftpi.dst_port : FT_PORT;

  if (mysignal(SIGQUIT, sig_quit) == SIG_ERR)
    fterr_err(1, "signal(SIGQUIT)");

  if (mysignal(SIGTERM, sig_term) == SIG_ERR)
    fterr_err(1, "signal(SIGTERM)");

  if (mysignal(SIGHUP, sig_hup) == SIG_ERR)
    fterr_err(1, "signal(SIGHUP)");

  /* daemonize */
  if (detach) {  
    if ((pid = fork()) == -1) {
      fterr_err(1, "fork()");
    } else if (pid) {
      if (pidfile)
        write_pidfile(pid, pidfile, listen_port);
      exit (0); /* parent */
    }
   
    chdir ("/"); 
    umask(0022);
    setsid();
    for (n = 0; n < 16; ++n) /* XXX dynamically get NOFILE */
      close (n);
    
    /* enable syslog */
    fterr_setsyslog(1, LOG_PID|LOG_NDELAY, LOG_LOCAL6);

    /* disable stderr */
    fterr_setfile(0, (void*)0L);

  }

  if (!(peers = (struct peer*)malloc(npeers * sizeof (struct peer))))
    fterr_err(1, "malloc()");

  /* zero out malloc'd memory */
  bzero(peers, npeers*sizeof(struct peer));

  /* pick off destinations, fill in peer entries */
  for (i = optind+1, n = 0; i < argc; ++i, ++n) {

    /* parse loc_ip/rem_ip/port/ttl */
    ftpi = scan_peeri(argv[i]);

    /* default UDP destination port is FT_PORT */
    peers[n].rem_addr.sin_port = (ftpi.dst_port) ? htons(ftpi.dst_port)
      : htons(FT_PORT);

    peers[n].loc_addr.sin_family = AF_INET;
    peers[n].rem_addr.sin_family = AF_INET;
    peers[n].ttl = ftpi.ttl;

    /* default ttl to 255 if this is unicast */
    if (!(IN_CLASSD(ftpi.rem_ip)) && (!ftpi.ttl))
      peers[n].ttl = 255;

    /* reverse remote and local if multicast */
    if ((n == 0) && (IN_CLASSD(ftpi.rem_ip))) {

      peers[n].loc_addr.sin_addr.s_addr = htonl(ftpi.rem_ip);
      peers[n].rem_addr.sin_addr.s_addr = htonl(ftpi.loc_ip);

    } else {

      peers[n].rem_addr.sin_addr.s_addr = htonl(ftpi.rem_ip);
      peers[n].loc_addr.sin_addr.s_addr = htonl(ftpi.loc_ip);

    }

    /* if preserving source IP address then these are raw sockets */
    if (src_ip_spoof) {

      if ((peers[n].fd = socket(AF_INET, SOCK_RAW, IPPROTO_RAW)) < 0)
        fterr_err(1, "socket()");

/* see Stevens Unix Network Programming Volume 1 2nd edition page 657 */
#ifdef IP_HDRINCL

      one = 1;

      if (setsockopt(peers[n].fd, IPPROTO_IP, IP_HDRINCL,
        &one, sizeof(one)) < 0) {
        fterr_err(1, "setsockopt(IP_HDRINCL)");
      }

#endif /* IP_HDRINCL */

    } else { /* normal UDP */

      if ((peers[n].fd = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
        fterr_err(1, "socket()");

    }

    if (bigsockbuf(peers[n].fd, SO_SNDBUF, FT_SO_SND_BUFSIZE) < 0)
      fterr_err(1, "bigsockbuf()");

      

#ifdef IP_ADD_MEMBERSHIP
  
    /* multicast destination? */
    if ((!src_ip_spoof) &&
        (IN_CLASSD(ntohl(peers[n].rem_addr.sin_addr.s_addr)))) {
   
      u_char ttl = peers[n].ttl;
  
      /* set the ttl */
      if (setsockopt(peers[n].fd, IPPROTO_IP, IP_MULTICAST_TTL,
        (char*)&ttl, sizeof(ttl)) < 0) {
        fterr_err(1, "setsockopt(IP_MULTICAST_TTL=%d)", ttl);
      }

    } /* multicast */
     
#endif /* IP_ADD_MEMBERSHIP */
   
    if (!src_ip_spoof) {
 
      if (bind(peers[n].fd, (struct sockaddr*)&peers[n].loc_addr,
        sizeof(struct sockaddr)) < 0)
        fterr_err(1, "bind(xmit)");

    }

    if (connect(peers[n].fd, (struct sockaddr*)&peers[n].rem_addr,
      sizeof(struct sockaddr)) < 0)
      fterr_err(1, "connect(xmit)");

  } /* for each destination */

  /* first arg is the listener, scan it and store away in ftnet */
  ftpi = scan_peeri(argv[optind]);
  
  ftnet.rem_ip = ftpi.rem_ip;
  ftnet.loc_ip = ftpi.loc_ip;

  /* default UDP listen port is FT_PORT */
  ftnet.dst_port = (ftpi.dst_port) ? ftpi.dst_port : FT_PORT;
 
  ftnet.loc_addr.sin_family = AF_INET;
  ftnet.loc_addr.sin_addr.s_addr = htonl(ftpi.loc_ip);
  ftnet.loc_addr.sin_port = htons(ftnet.dst_port);

  /* socket to receive flow pdu exports */
  if ((ftnet.fd = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
    fterr_err(1, "socket()");

  if (bigsockbuf(ftnet.fd, SO_RCVBUF, FT_SO_RCV_BUFSIZE) < 0)
    fterr_err(1, "bigsockbuf()");


  /*
   * setup to receive flows
   */

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
      fterr_err(1, "bind(loc_addr)");

  } /* not multicast group */

#ifdef IP_ADD_SOURCE_MEMBERSHIP
mcast_done:
#endif /* IP_ADD_SOURCE_MEMBERSHIP */

#else /* IP_ADD_MEMBERSHIP */

  /* unicast bind -- no multicast support */
  if (bind(ftnet.fd, (struct sockaddr*)&ftnet.loc_addr,
    sizeof(ftnet.loc_addr)) < 0)
    fterr_err(1, "bind(loc_addr)");

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

  /* setup for ftrec_mask_ip */
  if (privacy_mask != 0xFFFFFFFF)
    ftrec_compute_mask(&ftipmask, privacy_mask, privacy_mask,
      ftset.byte_order);

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
            "STAT: now=%lu startup=%lu src_ip=%s dst_ip=%s d_ver=%d pkts=%lu flows=%lu lost=%lu reset=%lu filter_drops=%lu send_nobufs=%lu",
            (unsigned long)now, (unsigned long)time_startup,
            fmt_src_ip, fmt_dst_ip,
            ftch_recexpp->d_version, (u_long)ftch_recexpp->packets,
            (u_long)ftch_recexpp->flows, (u_long)ftch_recexpp->lost,
            (u_long)ftch_recexpp->reset, (u_long)ftch_recexpp->filtered_flows,
            (u_long)send_nobufs);

        }

        stat_next = (tm->tm_min + (stat_interval - tm->tm_min % stat_interval))
          % 60;

      }

    } /* stat_inverval */

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

        /* copy to compare next time */
        bcopy(&ftpdu.ftv, &ftv, sizeof ftv);

        /* flag struct as configured */
        ftv.set = 1;

        /* configure encoder version */
        bcopy(&ftv, &fte.ver, sizeof ftv);

        /* need offsets for filter later */
        fts3rec_compute_offsets(&fo, &ftv);

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

      /* load filters? */
      if (reload_flag && ftv.set) {

        if (filter_active) {

          /* not first time through, then free previous filters */
          if (ftfd) {
            ftfil_free(&ftfil);
            fterr_info("Loading filters.");
          }

          if (ftfil_load(&ftfil, &ftvar, filter_fname))
            fterr_errx(1, "ftfil_load(%s): failed", filter_fname);

          if (!(ftfd = ftfil_def_find(&ftfil, filter_active)))
            fterr_errx(1, "ftfil_def_find(%s): failed", filter_active);
      
          if (ftfil_def_test_xfields(ftfd, ftrec_xfield(&ftv)))
            fterr_errx(1, "Filter references a field not in flow.");
        
        } /* filter_active */

        reload_flag = 0;

      }


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
      ftpdu.ftd.as_sub = ftset.as_sub;
      ftpdu.ftd.exporter_ip = ftch_recexp.src_ip;  
      n = fts3rec_pdu_decode(&ftpdu);

      /* update the exporter stats */
      ftch_recexpp->packets ++;
        ftch_recexpp->flows += n;

      /* write decoded flows */
      for (i = 0, offset = 0; i < n; ++i, offset += ftpdu.ftd.rec_size) {

        /* simple data privacy */ 
        if (privacy_mask != 0xFFFFFFFF)
          ftrec_mask_ip(ftpdu.ftd.buf+offset, &ftpdu.ftv, &ftipmask);

        /* translate version? */
        if (ftch_recexpp->xlate) {

          ftch_recexpp->xlate(ftpdu.ftd.buf+offset, &xl_rec);

          out_rec = (char*)&xl_rec;

        } else {

          out_rec = (char*)ftpdu.ftd.buf+offset;

        }

        /* filter? */
        if (ftfd)
          if (ftfil_def_eval(ftfd, out_rec, &fo) == FT_FIL_MODE_DENY) {
            ++ftch_recexpp->filtered_flows;
            continue;
          }


retry:
        ret = fts3rec_pdu_encode(&fte, out_rec);
      
        /*   ret == 0 then send and clear out buffer
         *   ret > 0 then can encode another
         *   ret < 0 then this encoding failed, send and clear out buffer
         */

        /* need to transmit? */
        if (ret < 0) {

          pdu_xmit(npeers, tx_delay, src_ip_spoof, hdr_len, &send_nobufs,
            ip_hdr, udp_hdr, &fte, peers, &ftnet);

        } /* ret < 0 */

        /* if ret < 0 then the current record was not encoded */
        if (ret < 0)
          goto retry;

      } /* for each flow */
    
      /* any encoded flows that have not been transmitted */
      if (fte.buf_size) {

        pdu_xmit(npeers, tx_delay, src_ip_spoof, hdr_len, &send_nobufs,
          ip_hdr, udp_hdr, &fte, peers, &ftnet);
    
      } /* fte.buf_size */
    

    } /* if FD_ISSET */

skip1:

    if (sig_quit_flag) {
      fterr_info("SIGQUIT");
      break;
    }

    if (sig_term_flag) {
      fterr_info("SIGTERM");
      break;
    }

    if (sig_hup_flag) {
      fterr_info("SIGHUP");
      sig_hup_flag = 0;
    }

  } /* while 1 */

  if (pidfile)
    unlink_pidfile(pid, pidfile, listen_port);

  for (n = 0; n < npeers; ++n)
    close(peers[0].fd);

  return 0;

} /* main */

void sig_quit(int sig)
{
  sig_quit_flag = 1;
}

void sig_term(int sig)
{
  sig_term_flag = 1;
}

void sig_hup(int sig)
{
  sig_hup_flag = 1;
  reload_flag = 1;
}

void fterr_exit_handler(int code)
{
  if (pid)
    if (pidfile)
      unlink_pidfile(pid, pidfile, listen_port);
  exit (code);
} /* fterr_exit_handler */
 
void pdu_xmit(int npeers, int tx_delay, int src_ip_spoof, int hdr_len,
  u_int32 *send_nobufs, struct ip *ip_hdr, struct udphdr *udp_hdr,
  struct ftencode *fte, struct peer *peers, struct ftnet *ftnet)
{
  int j, sum;

  /* convert pdu to network byte order */
#if BYTE_ORDER == LITTLE_ENDIAN
  ftpdu_swap(fte->buf_enc, BYTE_ORDER);
#endif /* BYTE_ORDER == LITTLE_ENDIAN */

  /* do this once for all destinations */
  ftencode_sum_data(fte);

  for (j = 0; j < npeers; ++j) {

    if (src_ip_spoof) {

/* see Stevens Unix Network Programming Volume 1 2nd edition page 657 */
/* conditional from <simon@limmat.switch.ch> rawsend.c */
#if defined (__linux__) || (defined (__OpenBSD__) && (OpenBSD > 199702))
      ip_hdr->ip_len = htons(FT_ENC_IPHDR_LEN+fte->buf_size);
#else
      ip_hdr->ip_len = FT_ENC_IPHDR_LEN+fte->buf_size;
#endif
      ip_hdr->ip_ttl = peers[j].ttl;
      /* use transmit source if loc_addr is not specified */
      if (!peers[j].loc_addr.sin_addr.s_addr)
        ip_hdr->ip_src.s_addr = ftnet->rem_addr.sin_addr.s_addr;
      else
        ip_hdr->ip_src.s_addr = peers[j].loc_addr.sin_addr.s_addr;
      ip_hdr->ip_dst.s_addr = peers[j].rem_addr.sin_addr.s_addr;

      udp_hdr->uh_sport = htons(7999+j);
      udp_hdr->uh_dport = peers[j].rem_addr.sin_port;
      udp_hdr->uh_ulen = htons(fte->buf_size+8);
      udp_hdr->uh_sum = 0;

      sum = fte->d_sum;
      sum += udp_cksum(ip_hdr, udp_hdr, fte->buf_size+8);

      sum = (sum >> 16) + (sum & 0xffff);
      sum += (sum >> 16);
      udp_hdr->uh_sum = ~sum;

    }
 
again:
 
    if (send(peers[j].fd, (char*)&fte->buf, fte->buf_size+hdr_len, 0) < 0) {

      /* always complete a send, drop flows in the kernel on receive if
         overloaded */
      if (errno == ENOBUFS) {
        ++ *send_nobufs;
        usleep(1);
        goto again;
      }

      if (errno != ECONNREFUSED)
        fterr_warn("send(j=%d)", j);

    }

    if (tx_delay)
      usleep((unsigned)tx_delay);

  } /* foreach peer to send to */

  /* reset encode buffer */
  ftencode_reset(fte);

} /* pdu_xmit */

void usage(void)
{
  fprintf(stderr, "usage: flow-fanout [-hDs] [-A AS0_substitution] [-d debug_level]\n");
  fprintf(stderr, "       [-m privacy_mask] [-p pidfile] [-S stat_interval] [-V pdu_version]\n");
  fprintf(stderr, "       [-x] xmit_delay] localip/remoteip/port localip/remoteip/port ...\n");
  fprintf(stderr, "\n%s version %s: built by %s\n", PACKAGE, VERSION, FT_PROG_BUILD);

} /* usage */
