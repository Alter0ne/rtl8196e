/* $Id: synce_log.h 2453 2006-06-04 19:15:58Z oleavr $ */
#ifndef __synce_log_h__
#define __synce_log_h__

#include "synce.h"

#define SYNCE_LOG_LEVEL_LOWEST    0

#define SYNCE_LOG_LEVEL_ERROR     1
#define SYNCE_LOG_LEVEL_WARNING   2
#define SYNCE_LOG_LEVEL_INFO      3
#define SYNCE_LOG_LEVEL_DEBUG     4
#define SYNCE_LOG_LEVEL_TRACE     SYNCE_LOG_LEVEL_DEBUG

#define SYNCE_LOG_LEVEL_HIGHEST   5
#define SYNCE_LOG_LEVEL_DEFAULT   2

#ifdef __cplusplus
extern "C"
{
#endif

void synce_log_set_level(int level);
void synce_log_use_syslog();

void _synce_log(int level, const char* file, int line, const char* format, ...);

#define synce_trace(format, args...) \
	_synce_log(SYNCE_LOG_LEVEL_TRACE,__PRETTY_FUNCTION__, __LINE__, format, ##args)

#define synce_debug(format, args...) \
	_synce_log(SYNCE_LOG_LEVEL_DEBUG,__PRETTY_FUNCTION__, __LINE__, format, ##args)

#define synce_info(format, args...) \
	_synce_log(SYNCE_LOG_LEVEL_INFO,__PRETTY_FUNCTION__, __LINE__, format, ##args)

#define synce_warning(format, args...) \
	_synce_log(SYNCE_LOG_LEVEL_WARNING,__PRETTY_FUNCTION__, __LINE__, format, ##args)

#define synce_warning_unless(cond, format, args...) \
	if (!(cond)) \
	_synce_log(SYNCE_LOG_LEVEL_WARNING,__PRETTY_FUNCTION__, __LINE__, format, ##args)

#define synce_error(format, args...) \
	_synce_log(SYNCE_LOG_LEVEL_ERROR,__PRETTY_FUNCTION__, __LINE__, format, ##args)

void _synce_log_wstr(int level, const char* file, int line, const char* name, const WCHAR* wstr);

#define synce_trace_wstr(wstr) \
	_synce_log_wstr(SYNCE_LOG_LEVEL_TRACE,__PRETTY_FUNCTION__, __LINE__, #wstr, wstr)

#ifdef __cplusplus
}
#endif


#endif

