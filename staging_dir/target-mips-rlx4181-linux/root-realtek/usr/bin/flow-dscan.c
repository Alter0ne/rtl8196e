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
 *      $Id: flow-dscan.c,v 1.29 2005/05/10 15:53:11 maf Exp $
 */

#include "ftconfig.h"
#include <ftlib.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <fcntl.h>
#include <signal.h>
#include <syslog.h>

#if HAVE_STRINGS_H
 #include <strings.h>
#endif

#if HAVE_STRING_H
  #include <string.h>
#endif

#include "ftbuild.h"
#include "flow-dscan.h"

/*
 * Detect network scan's
 */

/* XXX TODO
 *
 * trigger alarm on # of flows/second
 *
 * need to max out on # of records allocated.  If hit, then run the ager
 * more often/more agressive timeout.
 *
 * FUTURE:
 *  take advantage of need to only allocate strcuture of the same
 *  size by having malloc allocate chunks at a time and maintaining
 *  a freelist.
 * 
 *  most of the dst ports are 53 and 80.  Could save a bunch of memory
 *  by only allocating a bit1024 struct when it's not a list of well
 *  known ports -- tradoff for a few cpu cycles..
 *
 *  possibly trigger the ager with an alarm instead of by flow traffic
 *
 *  if the dst ip list gets too long convert to a hash?
 */

int debug;
int sig_hup, sig_usr1;

int load_state(struct dscan_state *ds);
void flow_dump(struct fts3rec_gen *rec);
void clear_suppress(struct dscan_state *ds, int sd);
int load_suppress(struct dscan_state *ds, int sd);
void ager(struct dscan_state *ds, u_int32 total_flows32);
void sig_hup_handler(int sig);
void sig_usr1_handler(int sig);
void usage(void);
int dump_state(struct dscan_state *ds);


int main(int argc, char **argv)
{
  struct ftio ftio;
  struct ftprof ftp;
  struct fts3rec_gen *rec;
  struct fttime ftt;
  int i, match, k, no_detach;
  u_int64 total_flows;
  u_int32 total_flows32;
  struct dscan_state ds;
  struct dscan_rec *drp;
  struct dscan_dst *ddp, *ddp2;
  struct dscan_sup *dsp_src, *dsp_dst;
  u_long hash;
  char fmt_buf1[64], fmt_buf2[64], fmt_buf3[64], fmt_buf4[64];
  int do_dump, do_load;
  int filter_www, filter_mcast, filter_input, filter_output;
  char in_tbl[65536], out_tbl[65536];
  u_int32 trigger_time, trigger_packets, trigger_octets;
  pid_t pid;
  struct tm *tm;

  total_flows = total_flows32 = 0;
  bzero(&ds, sizeof ds);
  ds.ager_timeout = DSCAN_AGER_TIMEOUT;
  ds.statefile = DSCAN_STATEFILE;
  ds.supfile = DSCAN_SUP_FILE;
  ds.dscan_ip_depth = DSCAN_IP_DEPTH;
  ds.dscan_port_trigger = DSCAN_PORT_TRIGGER;
  sig_hup = 0;
  sig_usr1 = 0;
  do_dump = 0;
  do_load = 0;
  filter_www = filter_mcast = filter_input = filter_output = 0;
  trigger_time = trigger_octets = trigger_packets = 0;
  no_detach = 0;

  /* init fterr */
  fterr_setid(argv[0]);

  while ((i = getopt(argc, argv, "d:i:I:D:h?lmps:t:wL:O:P:S:T:bBW")) != -1)

    switch (i) {

    case 'd': /* debug */
      debug = atoi(optarg);
      break;

    case 'i': /* input filter interface list */

      if (load_lookup(optarg, 65536, in_tbl))
        fterr_errx(1, "load_lookup(): failed");

      filter_input = 1;
      break;

    case 'l': /* load state */
      do_load = 1;
      break;

    case 'm': /* multicast filter */
      filter_mcast = 1;
      break;

    case 'I': /* output filter interface list */

      if (load_lookup(optarg, 65536, out_tbl))
        fterr_errx(1, "load_lookup(): failed");

      filter_output = 1;
      break;

    case 'p': /* dump state on exit */
      do_dump = 1;
      break;

    case 's': /* statefile */
      ds.statefile = optarg;
      break;

    case 't': /* ager timeout */
      sscanf(optarg, "%u", &ds.ager_timeout);
      break;

    case 'w': /* filter www inbound (replies) */
      filter_www |= 1;
      break;

    case 'b': /* no detach */
      no_detach = 1;
      break;

    case 'B': /* no detach, use syslog */
      no_detach = 2;
      break;

    case 'D':
      sscanf(optarg, "%u", &ds.dscan_ip_depth);
      break;

    case 'L': /* suppress file */
      ds.supfile = optarg;
      break;

    case 'O': /* excessive octets trigger */
      sscanf(optarg, "%u", (unsigned int*)&trigger_octets);
      break;

    case 'P': /* excessive packets trigger */
      sscanf(optarg, "%u", (unsigned int*)&trigger_packets);
      break;

    case 'S': /* port scan trigger */
      sscanf(optarg, "%u", &ds.dscan_port_trigger);
      break;

    case 'T': /* excessive flow time trigger */
      sscanf(optarg, "%u", (unsigned int*)&trigger_time);
      break;

    case 'W': /* filter www outbound */
      filter_www |= 2;
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

  /* daemonize */
  if (!no_detach) {
    if ((pid = fork()) == -1) {
      fterr_err(1, "fork()");
    } else if (pid)
      exit (0); /* parent */

    umask(0022);
    setsid();
    for (i = 1; i < 16; ++i) /* XXX dynamically get NOFILE */
      close (i);

    /* enable syslog */
    fterr_setsyslog(1, LOG_PID|LOG_NDELAY, LOG_LOCAL6);
  
    /* disable stderr */
    fterr_setfile(0, (void*)0L);

  } else {
    if (no_detach == 2) {
      /* enable syslog */
      fterr_setsyslog(1, LOG_PID|LOG_NDELAY, LOG_LOCAL6);
  
      /* disable stderr */
      fterr_setfile(0, (void*)0L);
    }
  }

  /* profile */
  ftprof_start (&ftp);

  /* configure handlers */
  if (mysignal(SIGHUP, sig_hup_handler) == SIG_ERR)
    fterr_err(1, "signal(SIGHUP)");

  if (mysignal(SIGUSR1, sig_usr1_handler) == SIG_ERR)
    fterr_err(1, "signal(SIGUSR1)");

  /* initialize scanner hash table */
  for (i = 0; i < DSCAN_HASHSIZE; ++i) {
    FT_SLIST_INIT(&ds.hash_scan[i]);
  }

  /* initialize suppress hash table */
  for (i = 0; i < DSCAN_HASHSIZE; ++i) {
    FT_SLIST_INIT(&ds.hash_sup_src[i]);
  }

  for (i = 0; i < DSCAN_HASHSIZE; ++i) {
    FT_SLIST_INIT(&ds.hash_sup_dst[i]);
  }

  /* load state file */
  if (do_load) {

    if (load_state(&ds))
      fterr_errx(1, "load_state(): failed");

#ifdef XXX
    ds.statefile = "/var/tmp/ds.state2";
    dump_state(&ds);
    exit(0);
#endif

  }

  if (load_suppress(&ds, 0))
    fterr_errx(1, "load_suppress(0): failed");

  if (load_suppress(&ds, 1))
    fterr_errx(1, "load_suppress(1): failed");

  /* read from stdin */
  if (ftio_init(&ftio, 0, FT_IO_FLAG_READ) < 0)
    fterr_errx(1, "ftio_init(): failed");

  /* check for records scan can understand */
  if (ftio_check_generic(&ftio) < 0)
    fterr_errx(1, "Unsupported export version");

  while ((rec = ftio_read(&ftio))) {

    if (sig_usr1) {
      if (debug)
        fterr_info("signal: USR1");
      dump_state(&ds);
      sig_usr1 = 0;
    } /* sig_usr1 */

    if (sig_hup) {
      if (debug)
        fterr_info("signal: USR1");
      clear_suppress(&ds, 0);
      clear_suppress(&ds, 1);
      load_suppress(&ds, 0);
      load_suppress(&ds, 1);
      sig_hup = 0;
    } /* sig_hup */

    ++total_flows;
    ++total_flows32;

    /* filter on input interface */
    if (filter_input) 
      if (!in_tbl[rec->input])
        goto skip2;

    /* filter on output interface */
    if (filter_output)
      if (!out_tbl[rec->output])
        goto skip2;

    /* filter multicast? */
    if (filter_mcast) {

      if ((rec->srcaddr >= 0xe0000000) && (rec->srcaddr <= 0xefffffff))
        goto skip2;

      if ((rec->dstaddr >= 0xe0000000) && (rec->dstaddr <= 0xefffffff))
        goto skip2;

    } /* filter_mcast */

    /* calculate hash based on src ip */
    hash = DSCAN_HASHFUNC(rec->srcaddr);

    if ((filter_www & 1) && (rec->srcport == 80) && (rec->prot == 6) &&
      (rec->dstport > 1023))
      goto skip2;

    if ((filter_www & 2) && (rec->dstport == 80) && (rec->prot == 6) &&
      (rec->srcport > 1023))
      goto skip2;

    /* evaluate this src ip and the suppress list */
    match = 0;
    FT_SLIST_FOREACH(dsp_src, &ds.hash_sup_src[hash], chain) {
      if (dsp_src->ip == rec->srcaddr) {
        if (!(dsp_src->flags & DSCAN_SUP_SRCPORT))
          goto skipsrc;
        if (dsp_src->srcport != rec->srcport)
          goto sup1;
skipsrc:
        if (!(dsp_src->flags & DSCAN_SUP_DSTPORT))
          goto skipdst;
        if (dsp_src->dstport != rec->dstport)
          goto sup1;
skipdst:
        if (!(dsp_src->flags & DSCAN_SUP_PROTOCOL))
          goto skip2;
        if (dsp_src->protocol != rec->prot)
          goto sup1;
        goto skip2;

      }
    }

sup1:

    /* evaluate this dst ip and the suppress list */
    match = 0;
    FT_SLIST_FOREACH(dsp_dst, &ds.hash_sup_dst[hash], chain) {
      if (dsp_dst->ip == rec->dstaddr) {
        if (!(dsp_dst->flags & DSCAN_SUP_SRCPORT))
          goto skipsrc1;
        if (dsp_dst->srcport != rec->srcport)
          goto sup;
skipsrc1:
        if (!(dsp_dst->flags & DSCAN_SUP_DSTPORT))
          goto skipdst1;
        if (dsp_dst->dstport != rec->dstport)
          goto sup;
skipdst1:
        if (!(dsp_dst->flags & DSCAN_SUP_PROTOCOL))
          goto skip2;
        if (dsp_dst->protocol != rec->prot)
          goto sup;
        goto skip2;

      }
    }

sup:

    /* evaluate the triggers */
    if (trigger_octets && (rec->dOctets > trigger_octets)) {
      flow_dump(rec);
    }

    if (trigger_packets && (rec->dPkts > trigger_packets)) {
      flow_dump(rec);
    }

    if (trigger_time && ((rec->Last - rec->First) > trigger_time)) {
      flow_dump(rec);
    }

    match = 0;
    FT_SLIST_FOREACH(drp, &ds.hash_scan[hash], chain) {
      if (drp->ip_src == rec->srcaddr) {
        match = 1;
        break;
      }
    }

    if (match) { /* src ip match */


      /* previous host or scan report then skip */
      if (drp->flags)
        goto skip2;

      /* if rec exists with this dest ip */
      FT_STAILQ_FOREACH(ddp, &drp->dlhead, chain) {
        if (ddp->ip_dst == rec->dstaddr) {
          ddp->ip_time = total_flows32;
          if (rec->dstport < 1024) {

            bit1024_store((int)rec->dstport, &ddp->portmap);

            if ((k = bit1024_count(&ddp->portmap)) >= ds.dscan_port_trigger) {

              fmt_ipv4(fmt_buf1, rec->srcaddr, FMT_JUST_LEFT);
              fmt_ipv4(fmt_buf2, rec->dstaddr, FMT_JUST_LEFT);

              ftt = ftltime(rec->sysUpTime, rec->unix_secs, rec->unix_nsecs,
                rec->First);
              tm = localtime((time_t*)&ftt.secs);

              fmt_uint32(fmt_buf3, ftt.secs, FMT_JUST_LEFT);

              snprintf(fmt_buf4, 64, "%-2.2d%-2.2d.%-2.2d:%-2.2d:%-2.2d.%-3lu ",
                 (int)tm->tm_mon+1, (int)tm->tm_mday, (int)tm->tm_hour,
                 (int)tm->tm_min, (int)tm->tm_sec, (u_long)ftt.msecs);

              fterr_info("port scan: src=%s dst=%s ts=%s start=%s",
                fmt_buf1, fmt_buf2, fmt_buf3, fmt_buf4);
              bzero(&ddp->portmap, sizeof ddp->portmap);
              drp->flags |= DSCAN_FLAGS_PORTSCAN;
            }
          }
          goto skip2;
        }
      }

      /* no rec exists with this dst ip */

      if (drp->depth >= ds.dscan_ip_depth) {

        fmt_ipv4(fmt_buf1, rec->srcaddr, FMT_JUST_LEFT);

        ftt = ftltime(rec->sysUpTime, rec->unix_secs, rec->unix_nsecs,
          rec->First);
        tm = localtime((time_t*)&ftt.secs);

        fmt_uint32(fmt_buf3, ftt.secs, FMT_JUST_LEFT);

        snprintf(fmt_buf4, 64, "%-2.2d%-2.2d.%-2.2d:%-2.2d:%-2.2d.%-3lu ",
         (int)tm->tm_mon+1, (int)tm->tm_mday, (int)tm->tm_hour,
         (int)tm->tm_min, (int)tm->tm_sec, (u_long)ftt.msecs);

        fterr_info( "host scan: ip=%s ts=%s start=%s", 
           fmt_buf1, fmt_buf3, fmt_buf4);

        drp->depth = 0;
        drp->flags |= DSCAN_FLAGS_HOSTSCAN;

        ddp = drp->dlhead.stqh_first;
        while (ddp != NULL) {
          ddp2 = ddp->chain.stqe_next;
          free(ddp);
          ++ds.stat_free;
          ++ds.stat_free_dst;
          ddp = ddp2;
        }
        FT_STAILQ_INIT(&drp->dlhead);
      } else {

        if (!(ddp = malloc(sizeof (struct dscan_dst))))
          fterr_err(1, "malloc(dscan_dst)");

        bzero(ddp, sizeof (struct dscan_dst));
        ds.stat_malloc++;
        ds.stat_malloc_dst++;
        drp->depth ++;

        FT_STAILQ_INSERT_TAIL(&drp->dlhead, ddp, chain);

        ddp->ip_dst = rec->dstaddr;
        ddp->ip_time = total_flows32;
        if (rec->dstport < 1024) {
          bit1024_store((int)rec->dstport, &ddp->portmap);
        }
      }

    } else { /* no src ip match */

      if (!(drp = malloc(sizeof (struct dscan_rec))))
        fterr_err(1, "malloc(dscan_rec)");

      ds.stat_malloc++;
      ds.stat_malloc_rec++;
      bzero(drp, sizeof (struct dscan_rec));
      drp->ip_src = rec->srcaddr;
      drp->depth = 1;
      FT_SLIST_INSERT_HEAD(&ds.hash_scan[hash], drp, chain);

      FT_STAILQ_INIT(&drp->dlhead);

      if (!(ddp = malloc(sizeof (struct dscan_dst))))
        fterr_err(1, "malloc(dscan_dst)");

      bzero(ddp, sizeof (struct dscan_dst));
      ds.stat_malloc++;
      ds.stat_malloc_dst++;

      FT_STAILQ_INSERT_TAIL(&drp->dlhead, ddp, chain);

      ddp->ip_dst = rec->dstaddr;
      ddp->ip_time = total_flows32;
      if (rec->dstport < 1024) {
        bit1024_store((int)rec->dstport, &ddp->portmap);
      }

    }

    /*
     * ager
     */

    if (ds.ager_timeout && (!(total_flows % 1000)))
      ager(&ds, total_flows32);

skip2:
    continue;

  } /* while rec */

  if (debug > 0) {
    ftprof_end(&ftp, ftio_get_rec_total(&ftio));
    ftprof_print(&ftp, argv[0], stderr);
  }

  if (do_dump)
    dump_state(&ds);

  return 0;
    

} /* main */

int dump_state(struct dscan_state *ds)
{
  FILE *FP;
  int i;
  char fmt_buf1[64];
  struct dscan_rec *drp;
  struct dscan_dst *ddp;

  if (!(FP = fopen(ds->statefile, "w"))) {
    fterr_warnx("fopen(%s): failed");
    return -1;
  }

  /* dump data structures */
  for (i = 0; i < DSCAN_HASHSIZE; ++i) {
    FT_SLIST_FOREACH(drp, &ds->hash_scan[i], chain) {
      fmt_ipv4(fmt_buf1, drp->ip_src, FMT_JUST_LEFT);
      fprintf(FP, "H %d %d %d %s\n", i, (int)drp->depth, (int)drp->flags,
        fmt_buf1);
      FT_STAILQ_FOREACH(ddp, &drp->dlhead, chain) {
        fmt_ipv4(fmt_buf1, ddp->ip_dst, FMT_JUST_LEFT);
        fprintf(FP, "I  %u %s\n", ddp->ip_time, fmt_buf1);
        bit1024_print(FP, &ddp->portmap); 
      }
    }
  }
  fprintf(FP, ".\n");

  if (fclose(FP)) {
    fterr_warnx("fclose(%s): failed", ds->statefile);
    return -1;
  }

  return 0;

} /* dump_state */

int load_state(struct dscan_state *ds)
{
  FILE *FP;
  int done;
  struct dscan_rec *drp;
  struct dscan_dst *ddp;
  char type;
  char buf[1024];
  u_int32 ip, hash, depth, flags, h, time, nports, i, j;

  if (!(FP = fopen(ds->statefile, "r")))
    fterr_errx(1, "fopen(%s): failed", ds->statefile);

  done = 0;

  while (!done) {

    if (fscanf(FP, "%c", &type) == EOF) {
      fterr_warnx("fscanf(): EOF");
      break;
    }

    switch (type) {

      case '.':
        done = 1;
        break;

      case 'H': /* host - hash depth flags srcIP */
        fscanf(FP, "%u %u %u %64s", (unsigned*)&hash, (unsigned*)&depth,
          (unsigned*)&flags, buf);
        ip = scan_ip(buf);
        h = DSCAN_HASHFUNC(ip);
        /* create the record */

        if (!(drp = malloc(sizeof (struct dscan_rec))))
          fterr_err(1, "malloc(dscan_rec)");

        ds->stat_malloc++;
        ds->stat_malloc_rec++;
        bzero(drp, sizeof (struct dscan_rec));
        drp->ip_src = ip;
        drp->depth = depth;
        drp->flags = flags;
        FT_SLIST_INSERT_HEAD(&ds->hash_scan[hash], drp, chain);

        /* init destination list */
        FT_STAILQ_INIT(&drp->dlhead);
        break;

      case 'I': /* include - time dstIP */
        fscanf(FP, "%u %15s", &time, buf);
        ip = scan_ip(buf);

        if (!(ddp = malloc(sizeof (struct dscan_dst))))
          fterr_err(1, "malloc(dscan_dst)");

        bzero(ddp, sizeof (struct dscan_dst));
        ds->stat_malloc++;
        ds->stat_malloc_dst++;

        FT_STAILQ_INSERT_TAIL(&drp->dlhead, ddp, chain);

        ddp->ip_dst = ip;
        ddp->ip_time = time;
        break;

      case 'P': /* portmap - nports portlist */
        fscanf(FP, "%u", &nports);
        for (i = 0; i < nports; ++i) {
          fscanf(FP, "%u", &j);
          if (j < 1024) {
            bit1024_store((int)j, &ddp->portmap);
          }
        }
        break;

      case '\n': /* ignore */
        break;

      default:
        fterr_warnx("Unknown record type: %c", type);
        return -1;
        break;

    } /* switch */
  } /* while */

  fclose (FP);
  return 0;
} /* load_state */

void sig_usr1_handler(int sig)
{
  sig_usr1 = 1;
} /* sig_usr1_handler */

void sig_hup_handler(int sig)
{
  sig_hup = 1;
} /* sig_usr1_handler */

void ager(struct dscan_state *ds, u_int32 total_flows32)
{
  static int ager_i;
  int i, work, picky_gcc;
  struct dscan_rec *drp, *odrp, *drp2;
  struct dscan_dst *ddp, *oddp, *ddp2;
  u_int32 i32;

  work = 0;
  for (i = ager_i; i < DSCAN_HASHSIZE; ++i) {
    odrp = (void*)0L;
    drp = ds->hash_scan[i].slh_first;
    while (drp) {
      work += 2;
      oddp = (void*)0L;
      ddp = drp->dlhead.stqh_first;
      while (ddp) {
        ++work;
        if (ddp->ip_time > total_flows32)
          i32 = 4294967295U - ddp->ip_time + total_flows32 + 1;
        else 
          i32 = total_flows32 - ddp->ip_time;
        if (i32 > ds->ager_timeout) { 
          if (debug > 5)
            fterr_info( "ager: remove max=%u i32=%u",
              ds->ager_timeout, i32);
          --drp->depth;
          if (oddp) {
            oddp->chain.stqe_next = ddp->chain.stqe_next;
            if (!ddp->chain.stqe_next) /* tail update */
              drp->dlhead.stqh_last = &oddp->chain.stqe_next;
          }
          else {
            drp->dlhead.stqh_first = ddp->chain.stqe_next;
            if (!ddp->chain.stqe_next) /* tail update */
              drp->dlhead.stqh_last = &ddp->chain.stqe_next;
          }
          ddp2 = ddp;
          ddp = ddp->chain.stqe_next;
          free (ddp2);
          ++ds->stat_free;
          ++ds->stat_free_dst;
          ++ds->stat_aged_ip;
          continue;
        }
        oddp = ddp;
        ddp = ddp->chain.stqe_next;
      } /* while ddp */
            /*  if they're all gone, delete the record itself */
      if (!drp->depth) {
        if (debug > 5)
          fterr_info("ager: remove record");
        if (odrp)
          odrp->chain.sle_next = drp->chain.sle_next;
        else
          ds->hash_scan[i].slh_first = drp->chain.sle_next;
        
        drp2 = drp;
        drp = drp->chain.sle_next;
        free (drp2);
        ++ds->stat_free;
        ++ds->stat_free_rec;
        ++ds->stat_aged_dsr;
        continue;
      } 
  
      odrp = drp;
      drp = drp->chain.sle_next;
  
    } /* while */
  
    /* simple work queue */
    if (++work > DSCAN_AGER_WORK) {
      if (debug > 2) {
        fterr_info("ager: work=%d malloc=%u free=%u aged_ip=%u aged_dsr=%u",
          work, ds->stat_malloc, ds->stat_free, ds->stat_aged_ip,
          ds->stat_aged_dsr);
        fterr_info("ager: malloc_dst=%u malloc_rec=%u free_dst=%u free_rec=%u",
          ds->stat_malloc_dst, ds->stat_malloc_rec, ds->stat_free_dst,
          ds->stat_free_rec);
      }
      ager_i = i++;
      goto skip3;
    }
  } /* for hash */

  ager_i = 0;
  fterr_info("ager: reset hash run");

skip3:
  picky_gcc = 1;

} /* ager */

int load_suppress(struct dscan_state *ds, int sd)
{
  char buf1[1024], *c1, *c2, *c3, *c4, *c;
  struct dscan_sup *dsp;
  int match;
  FILE *FP;
  u_int32 prot, srcport, dstport;
  u_int32 ip, hash;
  char *fname;

  fterr_info("load_suppress %d", sd);

  if (!(fname = (char*)malloc(strlen(ds->supfile)+5)))
    fterr_err(1, "malloc()");

  strcpy(fname, ds->supfile);

  if (sd == 0) {
    strcat(fname, ".src");
  } else if (sd == 1) {
    strcat(fname, ".dst");
  }

  if (!(FP = fopen(fname, "r")))
    fterr_errx(1, "fopen(%s): failed", fname);

  while (1) {

    if (!fgets(buf1, 1024, FP))
      break;

    /* skip whitespace */
    for (c = buf1; *c && ((*c == ' ') || (*c == '\t')); ++c);
    c1 = c;

    if (*c1 == '#') 
      continue;

    if (*c1 == '\n')
      continue;

    for (; *c && ((*c != ' ') && (*c != '\t')); ++c);
    for (; *c && ((*c == ' ') || (*c == '\t')); ++c);
    c2 = c;

    for (; *c && ((*c != ' ') && (*c != '\t')); ++c);
    for (; *c && ((*c == ' ') || (*c == '\t')); ++c);
    c3 = c;

    for (; *c && ((*c != ' ') && (*c != '\t')); ++c);
    for (; *c && ((*c == ' ') || (*c == '\t')); ++c);
    c4 = c;

    if ((!*c1) || (!*c2) || (!*c3) || (!*c4)) {
      fterr_warnx("suppress parser: syntax error: %s", buf1);
      continue;
    }

    if (debug > 5)
      fterr_info( "suppress parser: c1=%s c2=%s c3=%s c4=%s",
      c1, c2, c3, c4);

    ip = scan_ip(c1);
    sscanf(c2, "%u", &prot);
    sscanf(c3, "%u", &srcport);
    sscanf(c4, "%u", &dstport);

    hash = DSCAN_HASHFUNC(ip);


    if (sd == 0) {

      match = 0;
      FT_SLIST_FOREACH(dsp, &ds->hash_sup_src[hash], chain) {
        if (dsp->ip == ip) {
          match = 1;
          break;
        }
      }
  
      if (match) {
        fterr_warnx("suppress parser: dup ip: %s", buf1);
        continue;
      }
  
      if (*c1 == '-') {
        fterr_warnx("suppress parser: no src ip wildcard");
        continue;
      }
  
      if (!(dsp = malloc(sizeof (struct dscan_sup))))
        fterr_err(1, "malloc(dscan_sup)");
  
      bzero(dsp, sizeof (struct dscan_sup));
      dsp->ip = ip;
      dsp->srcport = srcport;
      dsp->dstport = dstport;
      dsp->protocol = prot;
  
      if (*c2 != '-')
        dsp->flags &= DSCAN_SUP_PROTOCOL;
      if (*c3 != '-')
        dsp->flags &= DSCAN_SUP_SRCPORT;
      if (*c4 != '-')
        dsp->flags &= DSCAN_SUP_DSTPORT;
  
      
      FT_SLIST_INSERT_HEAD(&ds->hash_sup_src[hash], dsp, chain);

    } else if (sd == 1) {

      match = 0;
      FT_SLIST_FOREACH(dsp, &ds->hash_sup_dst[hash], chain) {
        if (dsp->ip == ip) {
          match = 1;
          break;
        }
      }
  
      if (match) {
        fterr_warnx("suppress parser: dup ip: %s", buf1);
        continue;
      }
  
      if (*c1 == '-') {
        fterr_warnx("suppress parser: no src ip wildcard");
        continue;
      }
  
      if (!(dsp = malloc(sizeof (struct dscan_sup))))
        fterr_err(1, "malloc(dscan_sup)");
  
      bzero(dsp, sizeof (struct dscan_sup));
      dsp->ip = ip;
      dsp->srcport = srcport;
      dsp->dstport = dstport;
      dsp->protocol = prot;
  
      if (*c2 != '-')
        dsp->flags &= DSCAN_SUP_PROTOCOL;
      if (*c3 != '-')
        dsp->flags &= DSCAN_SUP_SRCPORT;
      if (*c4 != '-')
        dsp->flags &= DSCAN_SUP_DSTPORT;
  
      
      FT_SLIST_INSERT_HEAD(&ds->hash_sup_dst[hash], dsp, chain);

    }
      
  } /* while 1 */

  fclose(FP);
  free (fname);
  return 0;
} /* load_suppress */

void clear_suppress(struct dscan_state *ds, int sd)
{
  int i;
  struct dscan_sup *dsp_src, *dsp_dst;

  if (sd == 0) {
    for (i = 0; i < DSCAN_HASHSIZE; ++i) {
      while (ds->hash_sup_src[i].slh_first != NULL) {
        dsp_src = ds->hash_sup_src[i].slh_first;
        FT_SLIST_REMOVE_HEAD(&ds->hash_sup_src[i], chain);
        free (dsp_src);
      } /* while */
    } /* for */
  } /* if */

  if (sd == 1) {
    for (i = 0; i < DSCAN_HASHSIZE; ++i) {
      while (ds->hash_sup_dst[i].slh_first != NULL) {
        dsp_dst = ds->hash_sup_dst[i].slh_first;
        FT_SLIST_REMOVE_HEAD(&ds->hash_sup_dst[i], chain);
        free (dsp_dst);
      } /* while */
    } /* for */
  } /* if */

} /* clear_suppress */


void flow_dump(struct fts3rec_gen *rec)
{
  struct fttime ftt;
  char fmt_buf1[64], fmt_buf2[64];
  struct tm *tm;
  static int header;

  if (!header) {
    fterr_info( "Start             End                Sif SrcIPaddress      SrcP  DIf DstIPaddress      DstP   P Fl       Pkts     Octets");
    header = 1;
  }

  ftt = ftltime(rec->sysUpTime, rec->unix_secs, rec->unix_nsecs, rec->First);
  tm = localtime((time_t*)&ftt.secs);

  fterr_info( "%-2.2d%-2.2d.%-2.2d:%-2.2d:%-2.2d.%-3lu ",
    (int)tm->tm_mon+1, (int)tm->tm_mday, (int)tm->tm_hour,
    (int)tm->tm_min, (int)tm->tm_sec, (u_long)ftt.msecs);

  ftt = ftltime(rec->sysUpTime, rec->unix_secs, rec->unix_nsecs, rec->Last);
  tm = localtime((time_t*)&ftt.secs);

  fterr_info( "%-2.2d%-2.2d.%-2.2d:%-2.2d:%-2.2d.%-3lu ",
    (int)tm->tm_mon+1, (int)tm->tm_mday, (int)tm->tm_hour,
    (int)tm->tm_min, (int)tm->tm_sec, (u_long)ftt.msecs);

  /* other info */
  fmt_ipv4(fmt_buf1, rec->srcaddr, FMT_PAD_RIGHT);
  fmt_ipv4(fmt_buf2, rec->dstaddr, FMT_PAD_RIGHT);

  fterr_info( "%4d %-15.15s %6d %4d %-15.15s %6d %3d %2d %10lu %10lu",
    (int)rec->input, fmt_buf1, (int)rec->srcport, 
    (int)rec->output, fmt_buf2, (int)rec->dstport,
    (int)rec->prot, 
    (int)rec->tcp_flags & 0x7,
    (u_long)rec->dPkts, 
    (u_long)rec->dOctets);

} /* flow_dump */

void usage(void) {

  fprintf(stderr, "Usage: flow-dscan [-bBhlmpwW] [-d debug_level] [-D iplist_depth]\n");
  fprintf(stderr, "       [-s state_file] [-i input_filter] [-L suppress_list]\n");
  fprintf(stderr, "       [-o output_filter] [-O excessive_octets] [-P excessive_flows]\n");
  fprintf(stderr, "       [-S port_scan_trigger]  [-t ager_timeout]\n");
  fprintf(stderr, "Signals:\n");
  fprintf(stderr, "   SIGHUP  - reload suppress list\n");
  fprintf(stderr, "   SIGUSR1 - dump state\n");
  fprintf(stderr, "\n%s version %s: built by %s\n", PACKAGE, VERSION, FT_PROG_BUILD);

} /* usage */

