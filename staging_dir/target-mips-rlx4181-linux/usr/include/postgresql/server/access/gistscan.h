/*-------------------------------------------------------------------------
 *
 * gistscan.h
 *	  routines defined in access/gist/gistscan.c
 *
 *
 * Portions Copyright (c) 1996-2010, PostgreSQL Global Development Group
 * Portions Copyright (c) 1994, Regents of the University of California
 *
 * $PostgreSQL: pgsql/src/include/access/gistscan.h,v 1.35 2010/01/02 16:58:00 momjian Exp $
 *
 *-------------------------------------------------------------------------
 */
#ifndef GISTSCAN_H
#define GISTSCAN_H

#include "fmgr.h"

extern Datum gistbeginscan(PG_FUNCTION_ARGS);
extern Datum gistrescan(PG_FUNCTION_ARGS);
extern Datum gistmarkpos(PG_FUNCTION_ARGS);
extern Datum gistrestrpos(PG_FUNCTION_ARGS);
extern Datum gistendscan(PG_FUNCTION_ARGS);

#endif   /* GISTSCAN_H */
