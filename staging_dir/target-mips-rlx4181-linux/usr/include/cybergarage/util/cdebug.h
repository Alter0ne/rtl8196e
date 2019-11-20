/******************************************************************
*
*	CyberLink for C
*
*	Copyright (C) Satoshi Konno 2005
*
*	File: cdebug.h
*
*	Revision:
*
*	06/06/05
*		- first revision
*
******************************************************************/

#ifndef _CG_UTIL_CDEBUG_H_
#define _CG_UTIL_CDEBUG_H_

#include <cybergarage/typedef.h>

#ifdef  __cplusplus
extern "C" {
#endif

/****************************************
* Function
****************************************/

BOOL cg_debug_ison();
void cg_debug_on();
void cg_debug_off();

#ifdef  __cplusplus
}
#endif

#endif
