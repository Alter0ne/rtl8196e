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
 *      $Id: flow-capture.c,v 1.78 2004/01/05 17:56:50 maf Exp $
 */

#include "ftconfig.h"
#include <ftlib.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/socket.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <syslog.h>

#if HAVE_STRINGS_H
 #include <strings.h>
#endif

#if HAVE_STRING_H
  #include <string.h>
#endif

#include "ftbuild.h"

#ifdef HAVE_LIBWRAP
#include <tcpd.h>
#endif /* HAVE_LIBWRAP */

void fterr_exit_handler(int code);

#define CAPTURE_PIDFILE    "/var/run/flow-capture.pid"

#define SELECT_TIMEOUT 1   /* 1 second */

struct client_rec {
  int fd;
  struct sockaddr_in addr;
  int flags;
  time_t conn_time;
  struct ftio ftio;
  FT_LIST_ENTRY (client_rec) chain;
};

struct client {
  FT_LIST_HEAD(clienth, client_rec) list;
  struct sockaddr_in addr;
  int fd;
  int enabled; /* listen socket enabled? */
  int max; /* max connections */
  int active; /* active connections */
#ifdef HAVE_LIBWRAP
  struct request_info tcpd;
#endif /* HAVE_LIBWRAP */
};

struct rotate {
  double next;    /* time of next rotation */
  int    cur;     /* current rotation # */
  int    n;       /* number per day */
};

struct file {
  int fd;                   /* file descriptor */
  char name[MAXPATHLEN+1];  /* name */
  char nname[MAXPATHLEN+1]; /* new name */
  time_t time;
  struct ftver ftv;
  off_t nbytes;
  u_int32 hdr_nflows;
  u_int32 hdr_flows_corrupt;
  u_int32 hdr_flows_lost;
  u_int32 hdr_flows_reset;
};

int debug;
int sig_pipe_flag, sig_quit_flag, sig_hup_flag, sig_chld_flag, sig_term_flag;
int reload_flag;
void sig_pipe(int);
void sig_quit(int);
void sig_hup(int);
void sig_chld(int);
void sig_term(int);
pid_t pid;
char *pidfile;
struct ftnet ftnet;

void usage(void);
int calc_rotate (int next, double *trotate, int *cur);
double doubletime(void);

int main(argc, argv)   
int argc;
char **argv;
{
#ifdef IP_ADD_MEMBERSHIP
  struct ip_mreq mr;
#ifdef IP_ADD_SOURCE_MEMBERSHIP
  struct ip_mreq_source mrs;
#endif
#endif   
  fd_set rfd;
  struct sockaddr_in tmp_addr;
  struct timeval tv;
  struct tm *tm;
  struct ftset ftset;
  struct ftfile_entries fte;
  struct client client;
  struct ftio ftio;
  struct ftpdu ftpdu;
  struct ftpeeri ftpi;
  struct ftver ftv;
  struct ftchash *ftch;
  struct ftchash_rec_exp ftch_recexp, *ftch_recexpp;
  struct rotate rot;
  struct file cap_file;
  struct fttag fttag;
  struct ftfil ftfil;
  struct ftxlate ftxlate;
  struct fttag_def *ftd;
  struct ftfil_def *ftfd;
  struct ftvar ftvar;
  struct ftxlate_def *ftxd;
  struct fts3rec_offsets fo;
  struct client_rec *client_rec, *client_rec2;
  pid_t child_pid;
  time_t tt_now, time_startup;
  double now;
  char work_dir[MAXPATHLEN+1], post_rotate_exec[MAXPATHLEN+1];
  int i, n, tmp_len, enable_unlink, offset, detach, nest, one, max_fd;
  unsigned int v1, v2;
  u_int32 hash;
  char fmt_src_ip[32], fmt_dst_ip[32], fmt_dst_port[32];
  char xl_rec[FT_IO_MAXREC], *out_rec;
  char *tag_fname, *tag_active;
  char *filter_fname, *filter_active;
  char *xlate_fname, *xlate_active;
  int stat_interval, stat_next, child_status;
  int v_flag;
  int preserve_umask;

  time_startup = time((time_t)0L);

  bzero (&rot, sizeof rot);
  bzero (&cap_file, sizeof cap_file);
  bzero (&ftnet, sizeof ftnet);
  bzero (&tv, sizeof tv);
  bzero (&work_dir, sizeof work_dir);
  bzero (&post_rotate_exec, sizeof post_rotate_exec);
  bzero (&fte, sizeof fte);
  bzero (&client, sizeof client);
  bzero (&ftpdu, sizeof ftpdu);
  bzero (&ftv, sizeof ftv);
  bzero (&fttag, sizeof fttag);
  bzero (&ftfil, sizeof ftfil);
  bzero (&ftxlate, sizeof ftxlate);
  bzero (&ftvar, sizeof ftvar);

  FT_LIST_INIT(&client.list);
  stat_interval = 0;
  stat_next = -1;
  v_flag = 0;
  reload_flag = 1;
  preserve_umask = 0;

  tag_fname = FT_PATH_CFG_TAG;
  tag_active = (char*)0L;

  filter_fname = FT_PATH_CFG_FILTER;
  filter_active = (char*)0L;

  xlate_fname = FT_PATH_CFG_XLATE;
  xlate_active = (char*)0L;

  ftfd = (struct ftfil_def*)0L;
  ftd = (struct fttag_def*)0L;
  ftxd = (struct ftxlate_def*)0L;

  /* init fterr */
  fterr_setid(argv[0]);
  fterr_setexit(fterr_exit_handler);

  /* init var binding */
  if (ftvar_new(&ftvar) < 0)
    fterr_errx(1, "ftvar_new(): failed");

  /* defaults + default compression */
  ftset_init(&ftset, Z_DEFAULT_COMPRESSION);

  /* default timeout waiting for an active fd */
  tv.tv_sec = SELECT_TIMEOUT;

  /* listen for PDU's */
  ftnet.loc_addr.sin_family = AF_INET;
  ftnet.loc_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  ftnet.loc_addr.sin_port = htons(FT_PORT);

  /* listen for clients */
  client.addr.sin_family = AF_INET;
  client.addr.sin_addr.s_addr = htonl(INADDR_ANY);
  client.addr.sin_port = htons(FT_PORT);

  /* default detach from parent */
  detach = 1;

  /* default 95 rotations per day, or every 15 minutes */
  rot.n = 95;

  /* no files initially open */
  cap_file.fd = -1;

  /* year/month/day nesting */
  nest = 3;

  /* pidfile */
  pidfile = CAPTURE_PIDFILE;

  while ((i = getopt(argc, argv,
    "b:c:C:d:De:E:f:F:hn:N:p:S:t:T:uv:V:w:x:X:z:R:")) != -1)
  
    switch (i) {

    case 'b': /* output byte order */
      if (!strcasecmp(optarg, "little"))
        ftset.byte_order = FT_HEADER_LITTLE_ENDIAN;
      else if (!strcasecmp(optarg, "big"))
        ftset.byte_order = FT_HEADER_BIG_ENDIAN;
      else 
        fterr_errx(1, "expecting \"big\" or \"little\" at -b");
      break;

    case 'c': /* client enable */
      client.max = atoi(optarg);
      break;

    case 'C': /* comment field */
      ftset.comments = optarg;
      break;
  
    case 'D': /* daemonize */
      detach = 0;
      pidfile = (char*)0L;
      break;

    case 'e': /* expire */
      fte.max_files = atoi(optarg);
      if (fte.max_files)
        fte.expiring = 1;
      break;
  
    case 'E': /* expire bytes */
      if ((fte.max_bytes = scan_size(optarg)) == -1)
        fterr_errx(1, "scan_size(): failed");
      if (fte.max_bytes)
        fte.expiring = 1;
      break;

    case 'f': /* filter fname */
      filter_fname = optarg;
      break;
  
    case 'F': /* filter active */
      filter_active = optarg;
      break;
        
    case 'd': /* debug */
      debug = atoi(optarg);
      break;

    case 'h': /* help */
      usage();
      exit (0);
      break;

    case 'n': /* # rotations / day */
      rot.n = atoi(optarg);
      /* no more than 1 rotation per minute */
      if (rot.n > (1440-1))
        fterr_errx(1, "rotations limited to every 1 minute");
      break;

    case 'N': /* nesting level */
      nest = atoi(optarg);
      if ((nest < -3) || (nest > 3))
        fterr_errx(1, "-3 <= nesting level <= 3");
      break;

    case 'p': /* pidfile */
      if ((optarg[0] == 0) || ((optarg[0] == '-') && (optarg[1] == 0)))
        pidfile = (char*)0L;
      else
        pidfile = optarg;
      break;

    case 'R': /* Post rotate exec */
      if (strlen(optarg) > MAXPATHLEN)
        fterr_errx(1, "Post rotate argument too long");
      strcpy(post_rotate_exec,optarg);
      break;

    case 'S': /* stat interval */
      stat_interval = atoi(optarg);
      if ((stat_interval < 0) || (stat_interval > 60))
        fterr_errx(1, "Stat interval must be between 0 and 60.");
      break;

    case 't': /* tag filename */
      tag_fname = optarg;
      break;

    case 'T': /* active tags */
      tag_active = optarg;
      /* required for fttag_eval() */
      ftv.s_version = FT_IO_SVERSION;
      ftv.d_version = 1005;
      ftv.set = 1;
      break;

    case 'i': /* preserve inherited umask */
      preserve_umask = 1;
      break;

    case 'v': /* variable */
      if (ftvar_pset(&ftvar, optarg) < 0)
        fterr_errx(1, "ftvar_pset(%s): failed", optarg);
      break;
      
    case 'V': /* PDU version */
      v_flag = 1;
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
        
    case 'w': /* working directory */
      if (strlen(optarg) > (MAXPATHLEN))
        fterr_errx(1, "Pathname too long");
      strcpy(work_dir, optarg);
      break;

    case 'x': /* xlate file name */
      xlate_fname = optarg;
      break;
  
    case 'X': /* xlate definition name */
      xlate_active = optarg;
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

  /* disable pidfile if not forking */
  if (!detach)
    pidfile = (char*)0L;

  if ((argc - optind) != 1)
    fterr_errx(1, "Specify localip/remoteip/port.");

  /* tagging forces v1005 */
  if (v_flag && tag_active && (ftv.d_version != 1005))
    fterr_errx(1, "Must be v1005 with tagging.");

  if (!work_dir[0])
    fterr_errx(1, "Specify workdir with -w.");

  ftpi = scan_peeri(argv[optind]);

  ftnet.rem_ip = ftpi.rem_ip;
  ftnet.loc_ip = ftpi.loc_ip;

  if (ftpi.dst_port)
    ftnet.dst_port = ftpi.dst_port;
  else
    ftnet.dst_port = FT_PORT;

  ftnet.loc_addr.sin_addr.s_addr = htonl(ftpi.loc_ip);
  ftnet.loc_addr.sin_port = htons(ftnet.dst_port);
  client.addr.sin_port = htons(ftnet.dst_port);

  /* if debugging is enabled do not unlink any files when aging */
  if (debug) 
    enable_unlink = 0;
  else
    enable_unlink = 1;

  /* daemonize */
  if (detach) {
    if ((pid = fork()) == -1) {
      fterr_err(1, "fork()");
    } else if (pid) {
      if (pidfile)
        write_pidfile(pid, pidfile, ftnet.dst_port);
      exit (0); /* parent */
    }

    if (!preserve_umask)
      umask(0022);

    setsid();

    for (n = 0; n < 16; ++n)
      close (n);

    /* enable syslog */
    fterr_setsyslog(1, LOG_PID|LOG_NDELAY, LOG_LOCAL6);

    /* disable stderr */
    fterr_setfile(0, (void*)0L);

  } 

  /*
   * configure signal handlers
   */

  if (mysignal(SIGPIPE, sig_pipe) == SIG_ERR)
    fterr_err(1, "signal(SIGPIPE)");

  if (mysignal(SIGHUP, sig_hup) == SIG_ERR)
    fterr_err(1, "signal(SIGHUP)");

  if (mysignal(SIGQUIT, sig_quit) == SIG_ERR)
    fterr_err(1, "signal(SIGQUIT)");

  if (mysignal(SIGTERM, sig_term) == SIG_ERR)
    fterr_err(1, "signal(SIGTERM)");

  if (mysignal(SIGCHLD, sig_chld) == SIG_ERR)
    fterr_err(1, "signal(SIGCHLD)");

  /* sandbox */
  if (chdir(work_dir) == -1)
    fterr_err(1, "chdir(%s)", work_dir);

  /*
   * load directory entries into the file ager
   */
  if (fte.expiring)
    if (ftfile_loaddir(&fte, ".", FT_FILE_SORT|FT_FILE_INIT|FT_FILE_CHECKNAMES))
      fterr_errx(1, "ftfile_scandir(): failed");

  /* debugging gets a dump of the ager */
  if (debug)
    ftfile_dump(&fte);

  /* run the ager once now */
  if (fte.expiring)
    if (ftfile_expire(&fte, enable_unlink, (u_int32)0))
      fterr_errx(1, "ftfile_export(): failed");

  /* get hostname */
  if (gethostname((char*)&ftset.hnbuf, FT_HOSTNAME_LEN-1) == -1)
    fterr_err(1, "gethostname()");

  /* ensure null terminated */
  ftset.hnbuf[FT_HOSTNAME_LEN-1] = 0;

  /* socket to receive flow pdu exports */
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
      fterr_err(1, "bind(%s)", inet_ntoa(tmp_addr.sin_addr));

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
      fterr_err(1, "bind(%s)", inet_ntoa(ftnet.loc_addr.sin_addr));

  } /* not multicast group */

#ifdef IP_ADD_SOURCE_MEMBERSHIP
mcast_done:
#endif /* IP_ADD_SOURCE_MEMBERSHIP */

#else /* IP_ADD_MEMBERSHIP */

  /* unicast bind -- no multicast support */
  if (bind(ftnet.fd, (struct sockaddr*)&ftnet.loc_addr,
    sizeof(ftnet.loc_addr)) < 0)
    fterr_err(1, "bind(%s)", inet_ntoa(ftnet.loc_addr.sin_addr)));

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

  while (1) {

    FD_ZERO (&rfd);
    FD_SET (ftnet.fd, &rfd);

    if (client.enabled && client.max) {
      FD_SET (client.fd, &rfd);
      max_fd = (client.fd > ftnet.fd) ? client.fd : ftnet.fd;
    } else {
      max_fd = ftnet.fd;
    }

    if (select (max_fd+1, &rfd, (fd_set *)0, (fd_set *)0, &tv) < 0)  {
      if (errno == EINTR) {
        FD_ZERO (&rfd);
      } else {
        fterr_err(1, "select()");
      }
    }

    bzero (&tv, sizeof tv);
    tv.tv_sec = SELECT_TIMEOUT;

    tt_now = now = doubletime();

    /* new TCP client connection ? */
    if ((client.max) && (FD_ISSET(client.fd, &rfd))) {

      /* too many clients? */
      if (client.active >= client.max) {
        /* bye */
        i = accept(client.fd, (struct sockaddr*)&tmp_addr,
          (socklen_t*)&tmp_len);
        close(i);
        fterr_warnx("Maximum clients exceeded, rejecting connection from %s",
        inet_ntoa(tmp_addr.sin_addr));
        goto skip_client;
      }

      /* allocate a new client record, fail gracefully */
      if (!(client_rec = (struct client_rec*)malloc(sizeof *client_rec))) {
        fterr_warn("malloc()");
        /* bye */
        tmp_len = sizeof (struct sockaddr_in);
        i = accept(client.fd, (struct sockaddr*)&client_rec->addr,
          (socklen_t*)&tmp_len);
        close(i);
        goto skip_client;
      }

      bzero(client_rec, sizeof *client_rec);

      /* link in the new record */
      FT_LIST_INSERT_HEAD(&client.list, client_rec, chain);

      tmp_len = sizeof (struct sockaddr_in);
      /* accept() the connection */
      if ((client_rec->fd = accept(client.fd,
        (struct sockaddr*)&client_rec->addr, (socklen_t*)&tmp_len)) < 0) {
        fterr_warn("accept()");
        FT_LIST_REMOVE(client_rec, chain);
        free(client_rec);
        goto skip_client;
      }

#ifdef HAVE_LIBWRAP

      request_init(&client.tcpd, RQ_DAEMON, "flow-capture-client", RQ_FILE,
        client_rec->fd, NULL);

      fromhost(&client.tcpd);

      if (!hosts_access(&client.tcpd)) {

        fterr_warnx("client %s refused by libwrap",
          inet_ntoa(client_rec->addr.sin_addr));

        close(client_rec->fd);

        FT_LIST_REMOVE(client_rec, chain);
        free(client_rec);

        goto skip_client;

      }

#endif /* HAVE_LIBWRAP */
      
      if (bigsockbuf(client_rec->fd, SO_SNDBUF, FT_SO_SND_BUFSIZE) < 0)
        fterr_warn("bigsockbuf()");

      /* log it */
      client_rec->conn_time = tt_now;
      fterr_info("client connect: ip=%s time=%lu",
        inet_ntoa(client_rec->addr.sin_addr),
        (unsigned long)client_rec->conn_time);

      /* ftio_init the stream */
      if (ftio_init(&client_rec->ftio, client_rec->fd,
        FT_IO_FLAG_NO_SWAP | FT_IO_FLAG_WRITE) < 0) {
        fterr_warnx("ftio_init(): failed for client");
        close(client_rec->fd);
        FT_LIST_REMOVE(client_rec, chain);
        free(client_rec);
        goto skip_client;
      }

      /* set the version information in the io stream */
      if (ftio_set_ver(&client_rec->ftio, &ftv) < 0)
        fterr_errx(1, "ftio_set_ver(): failed");

      ftio_set_comment(&client_rec->ftio, ftset.comments);
      ftio_set_cap_hostname(&client_rec->ftio, ftset.hnbuf);
      ftio_set_byte_order(&client_rec->ftio, ftset.byte_order);
      ftio_set_cap_time(&client_rec->ftio, cap_file.time, 0);
      ftio_set_debug(&client_rec->ftio, debug);

      /* header first */
      if ((n = ftio_write_header(&client_rec->ftio)) < 0) {
        fterr_warnx("ftio_write_header(): failed for client");
        ftio_close(&client_rec->ftio);
        FT_LIST_REMOVE(client_rec, chain);
        free(client_rec);
        goto skip_client;
      }

      ++client.active;

    } /* new TCP client */

skip_client:

    /* stake the zombies */

    if (sig_chld_flag) {

      /* not again */
      sig_chld_flag = 0;

      while (1) {

        child_pid = wait3(&child_status, WNOHANG, 0);

        /* no more dead children? */
        if (!child_pid)
          break;

        /* no more dead children? */
        if ((child_pid == -1) && (errno == ECHILD))
          break;

        if (WIFEXITED(child_status)) {

          if (WEXITSTATUS(child_status))
            fterr_warnx("Child %d exit_status=%d", (int)child_pid,
              WEXITSTATUS(child_status));

        } else if (WIFSIGNALED(child_status)) {

          fterr_warnx("Child %d signal=%d", (int)child_pid,
            WTERMSIG(child_status));

        } else {

          fterr_warnx("PID %d exited status=%d", (int)child_pid,
            child_status);

        }

      } /* buffy */

    } /* sig_chld_flag */

    if (stat_interval) {

      tm = localtime (&tt_now);

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
            "STAT: now=%lu startup=%lu src_ip=%s dst_ip=%s d_ver=%d pkts=%lu flows=%lu lost=%lu reset=%lu filter_drops=%lu",
            (unsigned long)tt_now, (unsigned long)time_startup,
            fmt_src_ip, fmt_dst_ip,
            ftch_recexpp->d_version, (u_long)ftch_recexpp->packets,
            (u_long)ftch_recexpp->flows, (u_long)ftch_recexpp->lost,
            (u_long)ftch_recexpp->reset, (u_long)ftch_recexpp->filtered_flows);

        }

        stat_next = (tm->tm_min + (stat_interval - tm->tm_min % stat_interval))
          % 60;

      }

    } /* stat_inverval */

    /* flag for work later on */
    ftpdu.ftd.count = 0;
   
    /* PDU ready */
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
        ++cap_file.hdr_flows_corrupt;
        goto skip_pdu_decode;
      }
    
      /* rest of hash key */
      ftch_recexp.d_version = ftpdu.ftv.d_version;

      /* if exporter src IP has been configured then make sure it matches */
      if (ftnet.rem_ip && (ftnet.rem_ip != ftch_recexp.src_ip)) {
        fmt_ipv4(fmt_src_ip, ftch_recexp.src_ip, FMT_JUST_LEFT);
        fterr_warnx("Unexpected PDU: src_ip=%s not configured", fmt_src_ip);
        ++cap_file.hdr_flows_corrupt;
        goto skip_pdu_decode;
      }

      /* first flow or no configured destination version? */
      if (!ftv.set) {
        
        /* copy to compare next time */
        bcopy(&ftpdu.ftv, &ftv, sizeof ftv);
 
        /* flag struct as configured */
        ftv.set = 1;
 
      } else {

        /* translation to/from v8 not possible */
        if (((ftv.d_version == 8) && (ftpdu.ftv.d_version != 8)) ||
            ((ftv.d_version != 8) && (ftpdu.ftv.d_version == 8))) {
          fmt_ipv4(fmt_src_ip, ftch_recexp.dst_ip, FMT_JUST_LEFT);
          fterr_warnx("Unexpected PDU: src_ip=%s no v8 translation",
            fmt_src_ip);
          cap_file.hdr_flows_corrupt ++;
          goto skip_pdu_decode;
        }
 
        /* translation among v8 aggregation methods not possible */
        if ((ftv.d_version == 8) && ((ftv.agg_method != ftpdu.ftv.agg_method)
          || (ftv.agg_version != ftpdu.ftv.agg_version))) {
          fmt_ipv4(fmt_src_ip, ftch_recexp.dst_ip, FMT_JUST_LEFT);
          fterr_warnx(
            "Unexpected PDU: src_ip=%s multi v8 oagg=%d agg=%d over=%d ver=%d",
            fmt_src_ip, (int)ftv.agg_method, (int)ftpdu.ftv.agg_method,
            (int)ftv.agg_version, (int)ftpdu.ftv.agg_version);
          cap_file.hdr_flows_corrupt ++;
          goto skip_pdu_decode;
        }
      } /* version processing */

      /* compute 8 bit hash */
      hash = (ftch_recexp.src_ip & 0xFF);
      hash ^= (ftch_recexp.src_ip>>24);
      hash ^= (ftch_recexp.dst_ip & 0xFF);
      hash ^= (ftch_recexp.dst_ip>>24);
      hash ^= (ftch_recexp.d_version & 0xFF);

      /* get/create hash table entry */
      if (!(ftch_recexpp = ftchash_update(ftch, &ftch_recexp, hash)))
        fterr_errx(1, "ftch_update(): failed");

      /* if the packet count is 0, then this is a new entry */
      if (ftch_recexpp->packets == 0) {

        fmt_ipv4(fmt_src_ip, ftch_recexp.src_ip, FMT_JUST_LEFT);
        fmt_ipv4(fmt_dst_ip, ftch_recexp.dst_ip, FMT_JUST_LEFT);
        fterr_info("New exporter: time=%lu src_ip=%s dst_ip=%s d_version=%d",
          (u_long)tt_now, fmt_src_ip, fmt_dst_ip, (int)ftpdu.ftv.d_version);

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
          cap_file.hdr_flows_lost += ftch_recexpp->ftseq.seq_lost;
          ftch_recexpp->lost += ftch_recexpp->ftseq.seq_lost;
        } else {
          cap_file.hdr_flows_reset ++;
          ftch_recexpp->reset ++;
        }
      }

      /* decode the pdu */
      ftpdu.ftd.byte_order = ftset.byte_order;
      ftpdu.ftd.exporter_ip = ftch_recexp.src_ip;
      n = fts3rec_pdu_decode(&ftpdu);

      /* update the exporter stats */
      ftch_recexpp->packets ++;
      ftch_recexpp->flows += n;

    } /* PDU on receive buffer */

skip_pdu_decode:

    /* no current file and pdu version has been set -> create file */
    if ((cap_file.fd == -1) && (ftv.d_version)) {

      /* calculate the current rotation and next rotate time */
      if (calc_rotate(rot.n, &rot.next, &rot.cur) == -1)
        fterr_errx(1, "calc_rotate(): failed");

      /* remember when file was created */
      cap_file.time = (u_int32)tt_now;

      /* remember the version encoded in the filename */
      bcopy(&ftv, &cap_file.ftv, sizeof cap_file.ftv);

      /* construct the capture file name */
      ftfile_pathname(cap_file.name, MAXPATHLEN, nest, cap_file.ftv, 0,
        cap_file.time);

      /* create directory path for file */
      if (ftfile_mkpath(cap_file.time, nest) < 0)
        fterr_err(1, "ftfile_mkpath(%s)", cap_file.name);

      /* create/open the capture file */
      if ((cap_file.fd = open(cap_file.name, O_WRONLY|O_CREAT|O_TRUNC, 0644))
        == -1)
        fterr_err(1, "open(%s)", cap_file.name);

      /* initialize the IO stream */
      if (ftio_init(&ftio, cap_file.fd, FT_IO_FLAG_NO_SWAP | FT_IO_FLAG_WRITE |
        ((ftset.z_level) ? FT_IO_FLAG_ZINIT : 0) ) < 0)
        fterr_errx(1, "ftio_init(): failed");

      /* set the version information in the io stream */
      if (ftio_set_ver(&ftio, &ftv) < 0)
        fterr_errx(1, "ftio_set_ver(): failed");

      /* need offsets for filter later */
      fts3rec_compute_offsets(&fo, &ftv);

      ftio_set_comment(&ftio, ftset.comments);
      ftio_set_cap_hostname(&ftio, ftset.hnbuf);
      ftio_set_byte_order(&ftio, ftset.byte_order);
      ftio_set_z_level(&ftio, ftset.z_level);
      ftio_set_cap_time(&ftio, cap_file.time, 0);
      ftio_set_debug(&ftio, debug);
      ftio_set_corrupt(&ftio, cap_file.hdr_flows_corrupt);
      ftio_set_lost(&ftio, cap_file.hdr_flows_lost);
      ftio_set_reset(&ftio, cap_file.hdr_flows_reset);
      ftio_set_flows_count(&ftio, cap_file.hdr_nflows);

/*      ftio_map_load(&ftio, FT_MAP_FILE, ftnet.rem_ip); */

      /* header first */
      if ((n = ftio_write_header(&ftio)) < 0)
        fterr_errx(1, "ftio_write_header(): failed");
      else
        cap_file.nbytes = n;

    } /* create capture file and init new io stream */

    /* load filters and tags? */
    if (reload_flag && ftv.set) {

      /* load tags */
      if (tag_active) {
  
        /* not first time through, then free previous tags */
        if (ftd) {
          fttag_free(&fttag);
          fterr_info("Reloading tags.");
        }
    
        if (fttag_load(&fttag, &ftvar, tag_fname) < 0)
          fterr_errx(1, "fttag_load(): failed");
    
        if (!(ftd = fttag_def_find(&fttag, tag_active)))
          fterr_errx(1, "fttag_load(): failed");
    
      } /* tag_active */
    
      /* load filters */
      if (filter_active) {
  
        /* not first time through, then free previous filters */
        if (ftfd) {
          ftfil_free(&ftfil);
          fterr_info("Reloading filters.");
        }
        
        if (ftfil_load(&ftfil, &ftvar, filter_fname))
          fterr_errx(1, "ftfil_load(%s): failed", filter_fname);
            
        if (!(ftfd = ftfil_def_find(&ftfil, filter_active)))
          fterr_errx(1, "ftfil_def_find(%s): failed", filter_active);

        if (ftfil_def_test_xfields(ftfd, ftrec_xfield(&ftv)))
          fterr_errx(1, "Filter references a field not in flow.");
    
      } /* filter_active */

      /* load translations */
      if (xlate_active) {
  
        /* not first time through, then free previous translations */
        if (ftxd) {
          ftxlate_free(&ftxlate);
          fterr_info("Reloading translations.");
        }
        
        if (ftxlate_load(&ftxlate, &ftvar, xlate_fname))
          fterr_errx(1, "ftxlate_load(%s): failed", xlate_fname);
            
        if (!(ftxd = ftxlate_def_find(&ftxlate, xlate_active)))
          fterr_errx(1, "ftlate_def_find(%s): failed", xlate_active);

        if (ftxlate_def_test_xfields(ftxd, ftrec_xfield(&ftv)))
          fterr_errx(1, "Xlate references a field not in flow.");
    
      } /* xlate_active */

      reload_flag = 0;

    } /* reload_flag */

    /* if the decode buffer has entries write them out */
    for (i = 0, offset = 0; i < ftpdu.ftd.count;
      ++i, offset += ftpdu.ftd.rec_size) {

      /* translate version? */
      if (ftch_recexpp->xlate) {
 
        ftch_recexpp->xlate(ftpdu.ftd.buf+offset, &xl_rec);

        out_rec = (char*)&xl_rec;

        /* tagging? */
        if (tag_active)
          fttag_def_eval(ftd, (struct fts3rec_v1005*)out_rec);

      } else {

        out_rec = (char*)ftpdu.ftd.buf+offset;

      }

      /* filter? */
      if (ftfd)
        if (ftfil_def_eval(ftfd, out_rec, &fo) == FT_FIL_MODE_DENY) {
          ++ftch_recexpp->filtered_flows;
          continue; 
        }

      /* xlate? */
      if (ftxd)
        if (ftxlate_def_eval(ftxd, out_rec, &fo) != 0)
          fterr_errx(1, "ftxlate_def_eval(): failed.");

      /* update # of flows stored in capture file */
      cap_file.hdr_nflows ++;

      if ((n = ftio_write(&ftio, out_rec)) < 0)
        fterr_errx(1, "ftio_write(): failed");

      /* write to clients */
      FT_LIST_FOREACH(client_rec, &client.list, chain) {

        if ((n = ftio_write(&client_rec->ftio, out_rec)) < 0) {

          fterr_info("Killed client: ip=%s, dtime=%lu",
            inet_ntoa(client_rec->addr.sin_addr),
            (unsigned long)tt_now - client_rec->conn_time);

          ftio_close(&client_rec->ftio);
          client_rec2 = client_rec;
          client_rec = client_rec->chain.le_next;
          FT_LIST_REMOVE(client_rec2, chain);
          free(client_rec2);
          --client.active;

          if (!client_rec)
            break;

        } /* ftio_write */

      } /* foreach client */

      /* update # of bytes stored in capture file */
      cap_file.nbytes += n;

    } /* foreach entry in decode buffer */

    /*
     * time for a new file ?
     */
    if ((now > rot.next) || sig_term_flag || sig_quit_flag || sig_hup_flag) {

      if (sig_hup_flag)
        fterr_info("SIGHUP");

      sig_hup_flag = 0; /* re-arm */
            
      if (cap_file.fd != -1) {

        ftio_set_cap_time(&ftio, cap_file.time, (u_int32)tt_now);
        ftio_set_corrupt(&ftio, cap_file.hdr_flows_corrupt);
        ftio_set_lost(&ftio, cap_file.hdr_flows_lost);
        ftio_set_reset(&ftio, cap_file.hdr_flows_reset);
        ftio_set_flows_count(&ftio, cap_file.hdr_nflows);

        /* re-write header first */
        if (ftio_write_header(&ftio) < 0)
          fterr_errx(1, "ftio_write_header(): failed");

        if ((n = ftio_close(&ftio)) < 0)
          fterr_errx(1, "ftio_close(): failed");

        cap_file.nbytes += n;

        /* construct final version of capture filename */
        ftfile_pathname(cap_file.nname, MAXPATHLEN, nest, cap_file.ftv, 1,
          cap_file.time);
        
        /* rename working to final */
        if (rename(cap_file.name, cap_file.nname) == -1)
          fterr_err(1, "rename(%s,%s)", cap_file.name, cap_file.nname);

        /* add it to the ager */
        if (fte.expiring)
          if (ftfile_add_tail(&fte, cap_file.nname, cap_file.nbytes,
            cap_file.time))
            fterr_errx(1, "ftfile_add_tail(%s): failed", cap_file.name);

        /* debugging gets a dump of the ager */
        if (debug)
          ftfile_dump(&fte);

	/* Do the post rotate exec */
        if (post_rotate_exec[0]) {

          if ((n = fork()) == -1) {

            fterr_err(1, "fork()");

          } else if (!n) { /* child */

            n = execl(post_rotate_exec, post_rotate_exec, cap_file.nname,
                NULL);

            if (n == -1) 
              fterr_err(1, "exec(%s)", post_rotate_exec);

            _exit(0);
          } /* child */
        } /* post rotate exec */
        
	/* reset */
        bzero(&cap_file, sizeof cap_file);

        /* invalidate file descriptor */
        cap_file.fd = -1;

        /* had enough */
        if (sig_quit_flag || sig_term_flag)
          goto main_exit;
      } /* file open */

    } /* time for new file */

    /* also need to check sig_quit if no file has been processed yet */
    if (sig_quit_flag || sig_term_flag)
      goto main_exit;

  /*
   * If client attachments are enabled, and the flow version has been
   * determined setup a listener, but only once.
   */
  if (ftv.set && client.max && !client.enabled) {
    
    /* socket for listen */
    if ((client.fd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
      fterr_err(1, "socket()");

    one = 1;

    if (setsockopt(client.fd, SOL_SOCKET, SO_REUSEADDR, (char*)&one,
      sizeof (one)))
      fterr_err(1, "setsockopt(SO_REUSEADDR)");
      
    if (bind(client.fd, (struct sockaddr*)&client.addr, sizeof (client.addr))
      < 0)
      fterr_err(1, "bind(%s)", inet_ntoa(client.addr.sin_addr));
      
    /* listen for new TCP connections */
    if (listen(client.fd, 5) < 0)
      fterr_err(1, "listen()");

    /* non blocking */
    if (fcntl(client.fd, F_SETFL, O_NONBLOCK) < 0)
      fterr_err(1, "fcntl()");

    client.enabled = 1;

  } /* clients enabled */


    if (!(cap_file.hdr_nflows % 1001)) {

      if (fte.expiring)
        if (ftfile_expire(&fte, enable_unlink, cap_file.nbytes))
          fterr_errx(1, "ftfile_expire(): failed");

    } /* ager run? */

  } /* while 1 */

main_exit:

  if (pidfile)
    unlink_pidfile(pid, pidfile, ftnet.dst_port);

  if (sig_quit_flag)
    fterr_info("SIGQUIT");

  if (sig_term_flag)
    fterr_info("SIGTERM");

  /* free storage allocated to file entries */
  if (fte.expiring)
    ftfile_free(&fte);

  return 0;

} /* main */

void sig_pipe(int signo)
{
  sig_pipe_flag = 1;
}
    
void sig_hup(int signo)
{
  sig_hup_flag = 1;
  reload_flag = 1;
}
    
void sig_quit(int signo)
{  
  sig_quit_flag = 1;
}

void sig_term(int signo)
{  
  sig_term_flag = 1;
}

void sig_chld(int signo)
{  
  sig_chld_flag = 1;
}

/* doubletime - like time(2), but returns a double (with fractional seconds)
 * This was inspired by the Time::HiRes perl module.  E.g.:
 *    $ perl -MTime::HiRes -le 'print scalar Time::HiRes::time'
 * See "perl/CPAN/authors/id/D/DE/DEWEG/Time-HiRes-01.20.tar.gz".
 * - Dave Plonka <plonka@doit.wisc.edu>
 */
double doubletime(void) {
   double now;
   struct timeval tv_now;
   if (-1 == gettimeofday(&tv_now, (struct timezone *)0))
      return -1;
   now = tv_now.tv_sec + (tv_now.tv_usec / 1000000.0);
   return now;
}

int calc_rotate (int next, double *trotate, int *cur)
{
  double now;       /* current time */
  time_t tt_now;    /* current time */
  double irotate;   /* interval of next in seconds */
  time_t morning;   /* start of the day */
  struct tm *tm1;
        
  irotate = 86400. / (next+1);
       
  *cur = 0;
  
  if (-1 == (tt_now = now = doubletime()))
    return -1;
   
  /* calculate start of the day */
  if (!(tm1 = localtime (&tt_now)))
     return -1;

  tm1->tm_sec = 0;
  tm1->tm_min = 0;
  tm1->tm_hour = 0;
 
  if ((morning = mktime(tm1)) == -1)
    return -1;
  
  if (next)
    *trotate = morning + irotate;
  else  
    *trotate = morning + 86400.;
  
  while (now > *trotate) {
  
    ++ *cur;
    *trotate += irotate;
  
  }

  return 0;

} /* calc_rotate */

void fterr_exit_handler(int code)
{
  if (pid && pidfile)
    unlink_pidfile(pid, pidfile, ftnet.dst_port);
  exit (code);
} /* fterr_exit_handler */

void usage(void) {

  fprintf(stderr, "Usage: flow-capture [-hu] [-b big|little]\n");
  fprintf(stderr, "       [-C comment] [-c flow_clients] [-d debug_level] [-D daemonize]\n");
  fprintf(stderr, "       [-e expire_count] [-E expire_size[bKMG]] [-n rotations]\n");
  fprintf(stderr, "       [-N nesting_level] [-p pidfile ] [-R rotate_program]\n");
  fprintf(stderr, "       [-S stat_interval] [-t tag_fname] [-T tag_active] [-V pdu_version]\n");
  fprintf(stderr, "       [-z z_level] [-x xlate_fname] [-X xlate_active]\n");
  fprintf(stderr, "       -w workdir localip/remoteip/port\n");
  fprintf(stderr, "Signals:\n");
  fprintf(stderr, "   SIGHUP  - close and rotate current file\n");
  fprintf(stderr, "   SIGQUIT - close current file and exit\n");
fprintf(stderr, "\n%s version %s: built by %s\n", PACKAGE, VERSION, FT_PROG_BUILD);

} /* usage */ 

