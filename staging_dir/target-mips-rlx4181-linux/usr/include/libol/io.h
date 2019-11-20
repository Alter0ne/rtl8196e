/***************************************************************************
 *
 * Copyright (c) 1998-1999 Niels Möller
 * Copyright (c) 1999 BalaBit Computing
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * $Id: io.h,v 1.11 2001/08/26 21:28:18 bazsi Exp $
 *
 ***************************************************************************/

#ifndef __IO_H_INCLUDED
#define __IO_H_INCLUDED

#include "abstract_io.h"
#include "resource.h"
#include "queue.h"

#include <time.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

#define CLASS_DECLARE
#include "io.h.x"
#undef CLASS_DECLARE

/* A closed function with a file descriptor as argument */
/* CLASS:
   (class
     (name fd_callback)
     (vars
       (f indirect-method int "int fd")))
*/

#define FD_CALLBACK(c, fd) ((c)->f(&(c), (fd)))

/* Close callbacks are called with a reason as argument. */

/* End of file while reading.
 * Or when a closed write_buffer has been flushed successfully. */
/* FIXME: Should we use separate codes for these two events? */
#define CLOSE_EOF 1

/* EPIPE when writing */
#define CLOSE_BROKEN_PIPE 2

#define CLOSE_WRITE_FAILED 3

/* #define CLOSE_READ_FAILED 4 */

#define CLOSE_POLL_FAILED 5

#define CLOSE_PROTOCOL_FAILURE 6

/* CLASS:
   (class
     (name close_callback)
     (vars
       (f method int "int reason")))
*/

#define CLOSE_CALLBACK(c, r) ((c)->f((c), (r)))

/* CLASS:
   (class
     (name nonblocking_fd)
     (super resource)
     (vars
       (next object nonblocking_fd)
       (fd simple int)
       (fname string)

       ; User's close callback
       (to_be_closed simple int)
       (close_reason simple int)
       (close_callback object close_callback)

       ; Called before poll
       (prepare method void)

       (want_read simple int)
       ; Called if poll indicates that data can be read. 
       (read method void)

       (want_write simple int)
       ; Called if poll indicates that data can be written.
       (write method void)

       ; (close_now simple int)
       (really_close method void)))
*/

#define PREPARE_FD(fd) ((fd)->prepare((fd)))
#define READ_FD(fd) ((fd)->read((fd)))
#define WRITE_FD(fd) ((fd)->write((fd)))
#define REALLY_CLOSE_FD(fd) ((fd)->really_close((fd)))

/* CLASS:
   (class
     (name io_fd)
     (super nonblocking_fd)
     (vars
       (fsync simple int)
       ; Reading 
       (handler object read_handler)
       ; Writing 
       (buffer object abstract_buffer)))
*/

/* Passed to the listen callback, and to other functions and commands
 * dealing with addresses. */
/* CLASS:
   (class
     (name address_info)
     (vars
       (family simple int)
       (convert2sockaddr method "int" "int" "struct sockaddr *")
       (bind_socket method "int" "int")
       (connect_socket method "int" "int")))
*/

#define ADDRESS2SOCKADDR(a, s, sa) ((a)->convert2sockaddr(a, s, sa))
#define ADDRESS_BIND(a, f) ((a)->bind_socket)(a, f)
#define ADDRESS_CONNECT(a, f) ((a)->connect_socket(a, f))

/* CLASS:
   (class
     (name unix_address_info)
     (super address_info)
     (vars
       (path string)))
*/

/* CLASS:
   (class
     (name inet_address_info)
     (super address_info)
     (vars
       (ip string)
       (sa simple "struct sockaddr_in")
       (port simple UINT32)))
*/

/* CLASS:
   (class
     (name fd_listen_callback)
     (vars
       (f method int int "struct address_info *")))
*/

#define FD_LISTEN_CALLBACK(c, fd, a) ((c)->f((c), (fd), (a)))

/* CLASS:
   (class
     (name listen_fd)
     (super nonblocking_fd)
     (vars
       (callback object fd_listen_callback)))
*/

/* CLASS:
   (class
     (name connect_fd)
     (super nonblocking_fd)
     (vars
       (callback object fd_callback)))
*/

/* CLASS:
   (class
     (name callback)
     (vars
       (f method void)))
*/

#define CALLBACK(c) ((c)->f(c))

/* CLASS:
   (class
     (name io_backend)
     (vars
       (reloading simple int)
       ; Linked list of fds. 
       (files object nonblocking_fd)
       ; Callouts
       (callouts special-struct "struct ol_queue" do_mark_callouts do_free_callouts)))
*/

struct callout;

void init_backend(struct io_backend *b);

int io_iter(struct io_backend *b);
void io_run(struct io_backend *b);

int blocking_read(int fd, struct read_handler *r);

int get_inaddr(struct sockaddr_in	* addr,
	       const char		* host,
	       const char		* service,
	       const char		* protocol);

int tcp_addr(struct sockaddr_in *sin,
	     UINT32 length,
	     UINT8 *addr,
	     UINT32 port);

/* int open_inet_socket(int socktype, int proto, struct sockaddr_in *sa); */

struct inet_address_info *make_inet_address_c(const char *host,
                                         const char *port);

struct inet_address_info *make_inet_address(struct ol_string *host,
                                       UINT32 port);

struct unix_address_info *make_unix_address(struct ol_string *path);
struct unix_address_info *make_unix_address_c(const char *path);

struct address_info *sockaddr2address_info(size_t addr_len,
                                           struct sockaddr *addr);


int write_raw(int fd, UINT32 length, UINT8 *data);
int write_raw_with_poll(int fd, UINT32 length, UINT8 *data);

void io_set_nonblocking(int fd);
void io_set_close_on_exec(int fd);
void io_init_fd(int fd);

int io_open_socket(int family, int socktype, int proto, struct address_info *local);

struct connect_fd *io_connect(struct io_backend *b,
			      int fd,
			      struct address_info *remote,
			      struct fd_callback *f);


struct listen_fd *io_listen(struct io_backend *b,
			    int fd,
			    struct fd_listen_callback *callback);


struct io_fd *make_io_fd(struct io_backend *backend, int fd,
	struct ol_string *fname);

void init_file(struct io_backend *b, struct nonblocking_fd *f, int fd,
	struct ol_string *fname);

struct io_fd *io_read_write(struct io_fd *fd,
			    struct read_handler *read_callback,
			    struct abstract_buffer *buffer,
			    struct close_callback *close_callback);

struct io_fd *io_read(struct io_fd *fd,
		      struct read_handler *read_callback,
		      struct close_callback *close_callback);

struct io_fd *io_write(struct io_fd *fd,
		       struct abstract_buffer *buffer,
		       struct close_callback *close_callback);

struct callout *io_callout(struct io_backend *backend, time_t timeout, struct callback *callout);
void io_callout_set_timeout(struct callout *co, time_t timeout);
void io_callout_set_drop(struct callout *co, int drop);
void io_callout_flush(struct io_backend *backend);

#define closekill_fd(fd, reason) close_fd(fd, reason), kill_fd(fd)

int reopen_fd(struct nonblocking_fd *fd);

/* Marks a file for close, without touching the close_Reason field. */
void kill_fd(struct nonblocking_fd *fd);

void close_fd(struct nonblocking_fd *fd, int reason);


#endif /* LSH_IO_H_INCLUDED */
