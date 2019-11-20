/*	$OpenBSD: svc_auth.h,v 1.2 1997/09/21 10:46:16 niklas Exp $	*/
/*	$NetBSD: svc_auth.h,v 1.4 1994/10/26 00:57:07 cgd Exp $	*/

/*
 * Sun RPC is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify Sun RPC without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * SUN RPC IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * Sun RPC is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY SUN RPC
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 *
 *	from: @(#)svc_auth.h 1.6 86/07/16 SMI
 *	@(#)svc_auth.h	2.1 88/07/29 4.0 RPCSRC
 */

/*
 * svc_auth.h, Service side of rpc authentication.
 *
 * Copyright (C) 1984, Sun Microsystems, Inc.
 */

#ifndef _RPC_SVCAUTH_H
#define _RPC_SVCAUTH_H

/*
 * Interface to server-side authentication flavors.
 */
typedef struct {
	struct svc_auth_ops {
		int   (*svc_ah_wrap)();
		int   (*svc_ah_unwrap)();
		int   (*svc_ah_destroy)();
	} *svc_ah_ops;
	caddr_t svc_ah_private;
} SVCAUTH;

#define SVCAUTH_WRAP(auth, xdrs, xfunc, xwhere) \
     ((*((auth)->svc_ah_ops->svc_ah_wrap))(auth, xdrs, xfunc, xwhere))
#define SVCAUTH_UNWRAP(auth, xdrs, xfunc, xwhere) \
     ((*((auth)->svc_ah_ops->svc_ah_unwrap))(auth, xdrs, xfunc, xwhere))
#define SVCAUTH_DESTROY(auth) \
     ((*((auth)->svc_ah_ops->svc_ah_destroy))(auth))

/*
 * Approved way of getting principal of caller
 */
char *svcauth_gss_get_principal	__P((SVCAUTH *auth));

/*
 * Approved way of setting server principal
 */
bool_t
svcauth_gss_set_svc_name(char *principal, char *mechanism, u_int req_time,
			 u_int program, u_int version);

/*
 * Server side authenticator
 */
__BEGIN_DECLS
extern enum auth_stat _authenticate();
__END_DECLS

#endif /* !_RPC_SVCAUTH_H */
