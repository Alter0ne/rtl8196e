/* $Id: synce_types.h 3434 2008-05-04 11:01:57Z mark_ellis $ */
#ifndef __synce_types_h__
#define __synce_types_h__

#ifndef __synce_h__
#error Do not include this file directly, use synce.h
#endif


/*
 * Simple types
 */

typedef void      VOID;

typedef uint8_t   BYTE;
typedef BYTE      BOOLEAN;

typedef int16_t   CSHORT;

typedef uint16_t  WORD;
typedef uint16_t  USHORT;
typedef uint16_t  WCHAR;
typedef WCHAR     TCHAR;

typedef int32_t   LONG;
typedef int32_t   HKEY;
typedef int32_t   REGSAM;

typedef uint32_t  DWORD;
typedef uint32_t  UINT;
typedef uint32_t  ULONG;
typedef uint32_t  HWND;
typedef uint32_t  BOOL;

typedef int64_t LONGLONG;
typedef uint64_t  ULARGE_INTEGER;


/* XXX: sizeof(double) must be 8 */
typedef double    DATE;

/*
 * Pointer types
 */

typedef void*   LPVOID;
typedef char*   LPSTR;
typedef BYTE*   LPBYTE;
typedef WORD*   LPWORD;
typedef WCHAR*  LPWSTR;
typedef HKEY*   PHKEY;
typedef DWORD*  LPDWORD;
typedef LONG*   PLONG;
typedef ULARGE_INTEGER*         PULARGE_INTEGER ;
typedef ULARGE_INTEGER*         LPULARGE_INTEGER;



/*
 * Const pointer types
 */

typedef const void*   LPCVOID;
typedef const char*   LPCSTR;
typedef const WCHAR*  LPCWSTR;
typedef const char*   LPCTSTR ;


/*
 * Misc types
 */

typedef struct _TIME_FIELDS
{
  CSHORT Year;          /* Specifies a value from 1601 on. */
  CSHORT Month;         /* Specifies a value from 1 to 12. */
  CSHORT Day;           /* Specifies a value from 1 to 31. */
  CSHORT Hour;          /* Specifies a value from 0 to 23. */
  CSHORT Minute;        /* Specifies a value from 0 to 59. */
  CSHORT Second;        /* Specifies a value from 0 to 59. */
  CSHORT Milliseconds;  /* Specifies a value from 0 to 999. */
  CSHORT Weekday;       /* Specifies a value from 0 to 6 (Sunday to Saturday). */
} TIME_FIELDS, *PTIME_FIELDS;

typedef union _LARGE_INTEGER {
  struct {
    DWORD LowPart;
    LONG HighPart;
  } u;
  LONGLONG QuadPart;
} LARGE_INTEGER, *PLARGE_INTEGER;

typedef struct _FILETIME
{
  DWORD dwLowDateTime;
  DWORD dwHighDateTime;
} FILETIME, *PFILETIME, *LPFILETIME;

/* A handle  is usually a void*, but we must guarantee 32-bit! */
typedef uint32_t  HANDLE;

#define INVALID_HANDLE_VALUE ((HANDLE)-1)

/* HRESULT must be a signed integer if the FAILED() macro should work */
typedef int32_t  HRESULT;

#define STDAPI HRESULT

/*
 * Some error codes (HRESULTs)
 */

#define E_ABORT         0x80004004
#define E_ACCESSDENIED  0x80070005
#define E_FAIL          0x80004005
#define E_HANDLE        0x80070006
#define E_OUTOFMEMORY   0x8007000E
#define E_INVALIDARG    0x80070057
#define E_NOINTERFACE   0x80004002
#define E_NOTIMPL       0x80004001
#define E_OUTOFMEMORY   0x8007000E
#define E_PENDING       0x8000000A
#define E_POINTER       0x80004003
#define E_UNEXPECTED    0x8000FFFF
#define S_FALSE         0x00000001
#define S_OK            0x00000000

#define SUCCEEDED(x) ((x)>=0)
#define FAILED(x) ((x)<0)

/*
 * System Errors
 */

#include <synce_sys_error.h>

/*
 * Other macros
 */

#define MAX_PATH  260

#ifndef FALSE
#define FALSE false
#endif

#ifndef TRUE
#define TRUE true
#endif

#endif

