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
 *      $Id: flow-send.c,v 1.27 2004/03/31 03:15:41 maf Exp $
 */

#include "ftconfig.h"
#include <ftlib.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
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

int debug;

void usage(void);

void pdu_xmit(int tx_delay, int src_ip_spoof, int hdr_len,
  struct ip *ip_hdr, struct udphdr *udp_hdr,
  int sock, struct ftencode *fte, struct ftpeeri *ftpi);

int main(int argc, char **argv)
{
  struct sockaddr_in loc_addr, rem_addr;
  struct ip *ip_hdr;
  struct udphdr *udp_hdr;
  struct ftio ftio;
  struct ftprof ftp;
  struct ftver ftv, ftv2;
  struct ftencode fte;
  struct ftpeeri ftpi;
  struct ftipmask ftipmask;
  void (*xlate)(void *in_rec, void *out_rec);
  char xl_rec[FT_IO_MAXREC], *out_rec;
  u_int32 privacy_mask;
  unsigned int v1, v2, one;
  int i, n, ret, tx_delay, udp_sock;
  int src_ip_spoof, hdr_len = 0;
  void *rec;

  /* init fterr */
  fterr_setid(argv[0]);

  tx_delay = 0;
  bzero(&loc_addr, sizeof (struct sockaddr_in));
  bzero(&rem_addr, sizeof (struct sockaddr_in));
  bzero(&ftv, sizeof ftv);
  privacy_mask = 0xFFFFFFFF;
  src_ip_spoof = 0; /* no */

  /* profile */
  ftprof_start (&ftp);

  while ((i = getopt(argc, argv, "d:h?m:sV:x:")) != -1)
    switch (i) {

    case 'd': /* debug */
      debug = atoi(optarg);
      break;

    case 'h': /* help */
    case '?':
      usage();
      exit (0);
      break;

    case 'm': /* privacy mask */
      privacy_mask = scan_ip(optarg);
      break;

    case 's': /* source ip preserve */
      src_ip_spoof = 1;
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

    case 'x': /* transmitter delay */
      tx_delay = atoi(optarg);
      break;

    default:
      usage();
      exit (1);
      break;

    } /* switch */

  /* expect IP address/port option */
  if ((argc - optind) != 1) {
    usage();
    exit (1);
  }

  /* get localip/rem_ip/port/ttl */
  ftpi = scan_peeri(argv[optind]);

  /* default ttl to 255 if this is unicast */
  if (!(IN_CLASSD(ftpi.rem_ip)) && (!ftpi.ttl))
    ftpi.ttl = 255;

  rem_addr.sin_addr.s_addr = htonl(ftpi.rem_ip);
  rem_addr.sin_family = AF_INET;
 
  if (ftpi.dst_port) 
    rem_addr.sin_port = htons(ftpi.dst_port);
  else
    rem_addr.sin_port = htons(FT_PORT);

  loc_addr.sin_addr.s_addr = htonl(ftpi.loc_ip);
  loc_addr.sin_family = AF_INET;

    /* if preserving source IP address then these are raw sockets */
    if (src_ip_spoof) {
    
      if ((udp_sock = socket(AF_INET, SOCK_RAW, IPPROTO_RAW)) < 0)
        fterr_err(1, "socket()");
  
/* see Stevens Unix Network Programming Volume 1 2nd edition page 657 */
#ifdef IP_HDRINCL
  
      one = 1;
  
      if (setsockopt(udp_sock, IPPROTO_IP, IP_HDRINCL,
        &one, sizeof(one)) < 0) {
        fterr_err(1, "setsockopt(IP_HDRINCL)");
      }

#endif /* IP_HDRINCL */

  } else {

    /* normal UDP */
    if ((udp_sock = socket(AF_INET, SOCK_DGRAM, 0)) < 0)
      fterr_err(1, "socket()");

  }

  if (bigsockbuf(udp_sock, SO_SNDBUF, FT_SO_SND_BUFSIZE) < 0)
    fterr_err(1, "bigsockbuf()");

#ifdef IP_ADD_MEMBERSHIP

  /* multicast destination? */
  if ((!src_ip_spoof) &&
     (IN_CLASSD(ntohl(rem_addr.sin_addr.s_addr)))) {
 
    u_char ttl = ftpi.ttl;

    /* set the ttl */
    if (setsockopt(udp_sock, IPPROTO_IP, IP_MULTICAST_TTL,
      (char*)&ttl, sizeof(ttl)) < 0) {
      fterr_err(1, "setsockopt(IP_MULTICAST_TTL=%d)", ttl);
    }

  } /* mcast */

#endif /* IP_ADD_MEMBERSHIP */

  if (!src_ip_spoof) {

    if (bind(udp_sock, (struct sockaddr*)&loc_addr, sizeof loc_addr) < 0)
      fterr_err(1, "bind()");

  }
    
  if (connect(udp_sock, (struct sockaddr*)&rem_addr, sizeof rem_addr) < 0)
    fterr_err(1, "connect()");

  /* read from stdin */
  if (ftio_init(&ftio, 0, FT_IO_FLAG_READ) < 0)
    fterr_errx(1, "ftio_init(): failed");

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

  /* copy version from io stream */
  ftio_get_ver(&ftio, &ftv2);

  if (ftv.set)
    bcopy(&ftv, &fte.ver, sizeof ftv);
  else
    bcopy(&ftv2, &fte.ver, sizeof ftv2);

  /* translation? */
  if (fte.ver.d_version != ftv2.d_version) {

    if (!(xlate = ftrec_xlate_func(&ftv2, &fte.ver)))
      fterr_errx(1, "ftrec_xlate_func(): failed");

  } else {

    xlate = (void*)0L;

  }

  /* setup for ftrec_mask_ip */
  if (privacy_mask != 0xFFFFFFFF)
    ftrec_compute_mask(&ftipmask, privacy_mask, privacy_mask,
      ftio.fth.byte_order);

  while ((rec = ftio_read(&ftio))) {

    /* simple privacy */
    if (privacy_mask != 0xFFFFFFFF)   
      ftrec_mask_ip(rec, &fte.ver, &ftipmask);

    /* translate? */
    if (xlate) {

      xlate(rec, &xl_rec);
      out_rec = (char*)&xl_rec;

    } else {

      out_rec = rec;

    }

retry:

    ret = fts3rec_pdu_encode(&fte, out_rec);

/*   ret == 0 then send and clear out buffer
 *   ret > 0 then encode another
 *   ret < 0 then this encoding failed, send and clear out buffer
*/

    if (ret <= 0) {

      pdu_xmit(tx_delay, src_ip_spoof, hdr_len, ip_hdr, udp_hdr, udp_sock,
        &fte, &ftpi);

      /* if ret < 0 then the current record was not encoded */
      if (ret < 0)
        goto retry;

    }
  }

  /* any left over? */
  if (fte.buf_size) {

    pdu_xmit(tx_delay, src_ip_spoof, hdr_len, ip_hdr, udp_hdr, udp_sock,
      &fte, &ftpi);

  } /* fte.buf_size */
  
  if (debug > 0) {
    ftprof_end(&ftp, ftio_get_rec_total(&ftio));
    ftprof_print(&ftp, argv[0], stderr);
  }

  return ret;

} /* main */


void pdu_xmit(int tx_delay, int src_ip_spoof, int hdr_len,
  struct ip *ip_hdr, struct udphdr *udp_hdr,
  int sock, struct ftencode *fte, struct ftpeeri *ftpi)
{
  int sum;

  /* convert pdu to network byte order */
#if BYTE_ORDER == LITTLE_ENDIAN
  ftpdu_swap(fte->buf_enc, BYTE_ORDER);
#endif /* BYTE_ORDER == LITTLE_ENDIAN */

  /* do this once */
  ftencode_sum_data(fte);

  if (src_ip_spoof) {

/* see Stevens Unix Network Programming Volume 1 2nd edition page 657 */
/* conditional from <simon@limmat.switch.ch> rawsend.c */
#if defined (__linux__) || (defined (__OpenBSD__) && (OpenBSD > 199702))
    ip_hdr->ip_len = htons(FT_ENC_IPHDR_LEN+fte->buf_size);
#else
    ip_hdr->ip_len = FT_ENC_IPHDR_LEN+fte->buf_size;
#endif
    ip_hdr->ip_ttl = ftpi->ttl;
    ip_hdr->ip_src.s_addr = htonl(ftpi->loc_ip);
    ip_hdr->ip_dst.s_addr = htonl(ftpi->rem_ip);

    udp_hdr->uh_sport = htons(7999);
    udp_hdr->uh_dport = htons(ftpi->dst_port);
    udp_hdr->uh_ulen = htons(fte->buf_size+8);
    udp_hdr->uh_sum = 0;

    sum = fte->d_sum;
    sum += udp_cksum(ip_hdr, udp_hdr, fte->buf_size+8);

    sum = (sum >> 16) + (sum & 0xffff);
    sum += (sum >> 16);
    udp_hdr->uh_sum = ~sum;

  }
 
again:
 
  if (send(sock, (char*)&fte->buf, fte->buf_size+hdr_len, 0) < 0) {

    /* always complete a send, drop flows in the kernel on receive if
       overloaded */
    if (errno == ENOBUFS) {
      usleep(1);
      goto again;
    }

    if (errno != ECONNREFUSED)
      fterr_warn("send()");

  }

  if (tx_delay)
    usleep((unsigned)tx_delay);

  /* reset encode buffer */
  ftencode_reset(fte);

} /* pdu_xmit */

void usage(void) {

  fprintf(stderr, "Usage: flow-send [-h] [-d debug_level] [-x xmit_delay] [-V pdu_version]\n");
  fprintf(stderr, "       localip/remoteip/port\n");
  fprintf(stderr, "\n%s version %s: built by %s\n", PACKAGE, VERSION, FT_PROG_BUILD);

} /* usage */

