/******************************************************************
*
*	CyberUtil for C
*
*	Copyright (C) Satoshi Konno 2005
*
*	File: cmutex.h
*
*	Revision:
*
*	01/17/05
*		- first revision
*
******************************************************************/

#ifndef _CG_UTIL_CMUTEX_H_
#define _CG_UTIL_CMUTEX_H_

#include <cybergarage/typedef.h>

#if defined(WIN32) && !defined(ITRON)
#include <winsock2.h>
#elif defined(BTRON)
#include <btron/taskcomm.h>
#elif defined(ITRON)
#include <kernel.h>
#elif defined(TENGINE) && !defined(PROCESS_BASE)
#include <tk/tkernel.h>
#elif defined(TENGINE) && defined(PROCESS_BASE)
#include <btron/taskcomm.h>
#else
#include <pthread.h>
#endif

#ifdef  __cplusplus
extern "C" {
#endif

/****************************************
* Data Type
****************************************/

typedef struct _CgMutex {
#if defined(WIN32) && !defined(ITRON)
	HANDLE	mutexID;
#elif defined(BTRON)
	WERR	mutexID;
#elif defined(ITRON)
	ER_ID	mutexID;
#elif defined(TENGINE) && !defined(PROCESS_BASE)
	ID mutexID;
#elif defined(TENGINE) && defined(PROCESS_BASE)
	WERR	mutexID;
#else
	pthread_mutex_t mutexID;
#endif
} CgMutex;

/****************************************
* Function
****************************************/

CgMutex *cg_mutex_new();
BOOL cg_mutex_delete(CgMutex *mutex);
BOOL cg_mutex_lock(CgMutex *mutex);
BOOL cg_mutex_unlock(CgMutex *mutex);

#ifdef  __cplusplus
}
#endif

#endif
