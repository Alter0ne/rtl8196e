/* $Id: synce.h.in 3372 2008-04-11 09:47:16Z mark_ellis $ */
#ifndef __synce_h__
#define __synce_h__

/*
 * Include some files
 */
#include <sys/types.h>
#include <time.h>
#include <stdint.h>

#ifndef __cplusplus
#include <stdbool.h>
#endif

#ifndef NULL
#include <stdlib.h>
#endif


/*
 * Get data types for compatibility with Microsoft Windows
 */
#include "synce_types.h"


/*
 * Functions provided by libsynce
 */

#ifdef __cplusplus
extern "C"
{
namespace synce
{
#endif

/*
 * TIME_FIELDS, FILETIME and DATE conversion
 */

void time_fields_from_filetime(const FILETIME* filetime, TIME_FIELDS* timeFields);
bool time_fields_to_filetime(const TIME_FIELDS* timeFields, FILETIME* filetime);

void filetime_from_unix_time(time_t unix_time, FILETIME *filetime);
time_t filetime_to_unix_time(const FILETIME *filetime); /* deprecated! use time_fields_from_filetime() */

#define DATE_TIMEVALUEONLY   1
#define DATE_DATEVALUEONLY   2

bool date_from_tm(struct tm* pTm, DATE *pDateOut);
bool date_to_tm(DATE dateIn, DWORD dwFlags, struct tm* pTm);


/*
 * Wide string handling
 */

char* wstr_to_ascii(LPCWSTR unicode);

char* wstr_to_utf8(LPCWSTR unicode);

char* wstr_to_current(LPCWSTR unicode);

LPWSTR wstr_from_ascii(const char* ascii);

LPWSTR wstr_from_utf8(const char* utf8);

LPWSTR wstr_from_current(const char* utf8);

void wstr_free_string(void* str);

size_t wstrlen(LPCWSTR unicode);

LPWSTR wstrcpy(LPWSTR dest, LPCWSTR src);

bool wstr_append(LPWSTR dest, LPCWSTR src, size_t max_dest_length);

bool wstr_equal(LPWSTR a, LPWSTR b);

LPWSTR wstrdup(LPCWSTR string);

/*
 * Old function names
 */

#define wstr_strlen(a)    wstrlen(a)
#define wstr_strcpy(a,b)  wstrcpy(a,b)


/*
 * Endian conversions
 */

#if !defined(htole32) || !defined(htole16) || !defined(letoh16) || !defined(letoh32)
#undef htole16
#undef htole32
#undef letoh16
#undef letoh32

/* define host-to-little-endian and little-endian-to-host macros */
#if 1

/* byte swapping */
#if 1
#include <byteswap.h>
#else
#define IMPLEMENT_BSWAP_XX  1
uint16_t bswap_16(uint16_t x);
uint32_t bswap_32(uint32_t x);
#endif

/* Use bswap_xx */

#define htole16(x)		bswap_16(x)
#define htole32(x)		bswap_32(x)
#define letoh16(x)    bswap_16(x)
#define letoh32(x)    bswap_32(x)

#else

/* Empty macros */

#define htole16(x)		(x)
#define htole32(x)		(x)
#define letoh16(x)    (x)
#define letoh32(x)    (x)

#endif
#endif


/*
 * Configuration directory and file name stuff
 */

bool synce_get_directory(char** path);
bool synce_get_subdirectory(const char* name, char** directory);
bool synce_get_connection_filename(char** filename);
bool synce_set_connection_filename(const char* filename);
bool synce_set_default_connection_filename();
bool synce_get_script_directory(char** directory);

/*
 * Error handling
 */

const char* synce_strerror(DWORD error);


/*
 * Info from the ~/.synce/active_connection file
 * written by (v)dccm, or obtained from odccm
 */

typedef struct _SynceInfo
{
  pid_t dccm_pid;
  char* ip;
  char* password;
  int key;
  int os_version;
  int os_minor;
  int build_number;
  int processor_type;
  int partner_id_1;
  int partner_id_2;
  char* name;
  char* os_name;
  char* model;
  char* transport;
  int fd;
} SynceInfo;

SynceInfo* synce_info_new(const char* device_name);
void synce_info_destroy(SynceInfo* info);


#ifdef __cplusplus
}
}
#endif


#endif

