/*
    cgi.h - Routines for CGI programming
    Copyright (c) 1996-8,2007,8  Martin Schulze <joey@infodrom.org>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software Foundation
    Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#ifndef _CGI_H_
#define _CGI_H_

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef struct var_s {
	char	*name,
		*value;
} s_var;

typedef struct cookie_s {
	char	*version,
		*name,
		*value,
		*path,
		*domain;
} s_cookie;

typedef struct file_s {
	char	*name,
		*type,
		*filename,
		*tmpfile;
} s_file;

typedef struct cgi_s {
	s_var **vars;
	s_cookie **cookies;
	s_file **files;
} s_cgi;

/* cgiSetHeader
 * 
 *  Sets additional HTTP header lines to be printed with cgiHeader
 */
int cgiSetHeader (const char *name, const char *value);

/* cgiSetType
 * 
 *  Sets result type for HTTP
 */
int cgiSetType (const char *type);

/* cgiHeader
 * 
 *  Prints a valid CGI Header (Content-type...) etc.
 */
void cgiHeader ();

/* cgiDebug
 * 
 *  Set/unsets debugging
 */
void cgiDebug (int level, int where);

/* cgiInit
 *
 *  Reads in variables set via POST or stdin, reads HTTP Cookies.
 */
s_cgi *cgiInit ();

/* cgiGetValue
 *
 *  Returns the value of the specified variable or NULL if it's empty
 *  or doesn't exist.
 */
char *cgiGetValue (s_cgi *parms, const char *name);

/* cgiGetVariables
 *
 *  Returns the names of all form variables.
 */
char **cgiGetVariables (s_cgi *parms);

/* cgiRedirect
 *
 *  Provides a valid redirect for web pages.
 */
void cgiRedirect (const char *url);

/* cgiGetCookie
 *
 *  Returns the cookie referenced by the given name or NULL if it
 *  doesn't exist or is empty.
 */
s_cookie *cgiGetCookie (s_cgi *parms, const char *name);

/* cgiGetCookies
 *
 * Returns a list of names of all cookies.
 */
char **cgiGetCookies (s_cgi *parms);

/* cgiGetFiles
 *
 * Returns a list of names of all files.
 */
char **cgiGetFiles (s_cgi *parms);

/* cgiGetFile
 *
 * Return data structure for CGI file variable
 */
s_file *cgiGetFile (s_cgi *parms, const char *name);

/* cgiFreeList
 *
 * Frees a list as returned by cgiGetVariables() and cgiGetCookies()
 */
void cgiFreeList (char **list);

/* cgiFree
 *
 * Frees the internal data structures
 */
void cgiFree (s_cgi *parms);

/* cgiEscape
 *
 * Escapes <&> in a string
 */
char *cgiEscape (char *string);

#ifdef __cplusplus
extern }
#endif

#ifdef __cplusplus
} /* end of extern "C" */
#endif

#endif /* _CGI_H_ */
