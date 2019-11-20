/* include/asterisk/autoconfig.h.  Generated from autoconfig.h.in by configure.  */
/* include/asterisk/autoconfig.h.in.  Generated from configure.ac by autoheader.  */

#ifndef ASTERISK_AUTOCONFIG_H
#define ASTERISK_AUTOCONFIG_H

#include "asterisk/buildopts.h"



/* Define to 1 if internal poll should be used. */
/* #undef AST_POLL_COMPAT */

/* Define to 1 if your system's implementation of mutexes supports comparison
   of a mutex to its initializer. */
/* #undef CAN_COMPARE_MUTEX_TO_INIT_VALUE */

/* Define to 1 if the `closedir' function returns void instead of `int'. */
#define CLOSEDIR_VOID 1

/* Some configure tests will unexpectedly fail if configure is run by a
   non-root user. These may be able to be tested at runtime. */
/* #undef CONFIGURE_RAN_AS_ROOT */

/* Define to one of `_getb67', `GETB67', `getb67' for Cray-2 and Cray-YMP
   systems. This function is required for `alloca.c' support on those systems.
   */
/* #undef CRAY_STACKSEG_END */

/* Define to 1 if using `alloca.c'. */
/* #undef C_ALLOCA */

/* Use the FreeRADIUS-client library */
/* #undef FREERADIUS_CLIENT */

/* Define to 1 if you have the `acos' function. */
#define HAVE_ACOS 1

/* Define to 1 if you have the `acosl' function. */
/* #undef HAVE_ACOSL */

/* Define to 1 if you have `alloca', as a function or macro. */
#define HAVE_ALLOCA 1

/* Define to 1 if you have <alloca.h> and it should be used (not on Ultrix).
   */
#define HAVE_ALLOCA_H 1

/* Define to 1 if you have the Advanced Linux Sound Architecture library. */
#define HAVE_ALSA 1

/* Define to 1 if you have the <arpa/inet.h> header file. */
#define HAVE_ARPA_INET_H 1

/* Define to 1 if you have the <arpa/nameser.h> header file. */
#define HAVE_ARPA_NAMESER_H 1

/* Define to 1 if you have the `asin' function. */
#define HAVE_ASIN 1

/* Define to 1 if you have the `asinl' function. */
/* #undef HAVE_ASINL */

/* Define to 1 if you have the `asprintf' function. */
#define HAVE_ASPRINTF 1

/* Define to 1 if you have the `atan' function. */
#define HAVE_ATAN 1

/* Define to 1 if you have the `atan2' function. */
#define HAVE_ATAN2 1

/* Define to 1 if you have the `atan2l' function. */
/* #undef HAVE_ATAN2L */

/* Define to 1 if you have the `atanl' function. */
/* #undef HAVE_ATANL */

/* Define to 1 if you have the `atexit' function. */
#define HAVE_ATEXIT 1

/* Define to 1 if your GCC C compiler supports the 'always_inline' attribute.
   */
#define HAVE_ATTRIBUTE_always_inline 1

/* Define to 1 if your GCC C compiler supports the 'const' attribute. */
#define HAVE_ATTRIBUTE_const 1

/* Define to 1 if your GCC C compiler supports the 'deprecated' attribute. */
#define HAVE_ATTRIBUTE_deprecated 1

/* Define to 1 if your GCC C compiler supports the 'malloc' attribute. */
#define HAVE_ATTRIBUTE_malloc 1

/* Define to 1 if your GCC C compiler supports the 'pure' attribute. */
#define HAVE_ATTRIBUTE_pure 1

/* Define to 1 if your GCC C compiler supports the 'sentinel' attribute. */
#define HAVE_ATTRIBUTE_sentinel 1

/* Define to 1 if your GCC C compiler supports the 'unused' attribute. */
#define HAVE_ATTRIBUTE_unused 1

/* Define to 1 if your GCC C compiler supports the 'warn_unused_result'
   attribute. */
#define HAVE_ATTRIBUTE_warn_unused_result 1

/* Define to 1 if your GCC C compiler supports the 'weak' attribute. */
/* #undef HAVE_ATTRIBUTE_weak */

/* Define to 1 if your GCC C compiler supports the 'weak_import' attribute. */
/* #undef HAVE_ATTRIBUTE_weak_import */

/* Define to 1 if your GCC C compiler supports the 'weakref' attribute. */
/* #undef HAVE_ATTRIBUTE_weakref */

/* Define to 1 if you have the Debug symbol decoding library. */
/* #undef HAVE_BFD */

/* Define to 1 if you have the Stack Backtrace library. */
/* #undef HAVE_BKTR */

/* Define to 1 if you have the Bluetooth library. */
/* #undef HAVE_BLUETOOTH */

/* Define to 1 if byteswap.h macros are available. */
#define HAVE_BYTESWAP_H 1

/* Define to 1 if you have the POSIX 1.e capabilities library. */
/* #undef HAVE_CAP */

/* Define to 1 if you have the `ceil' function. */
#define HAVE_CEIL 1

/* Define to 1 if you have the `ceill' function. */
/* #undef HAVE_CEILL */

/* Define to 1 if your system has a working `chown' function. */
/* #undef HAVE_CHOWN */

/* Define to 1 if you have the `closefrom' function. */
/* #undef HAVE_CLOSEFROM */

/* Define to 1 if you have the Corosync library. */
/* #undef HAVE_COROSYNC */

/* Define to 1 if COROSYNC has the A callback only in corosync 1.x feature. */
/* #undef HAVE_COROSYNC_CFG_STATE_TRACK */

/* Define to 1 if you have the `cos' function. */
#define HAVE_COS 1

/* Define to 1 if you have the `cosl' function. */
/* #undef HAVE_COSL */

/* Define to 1 if you have the OpenSSL Cryptography library. */
#define HAVE_CRYPTO 1

/* Define to 1 if you have a functional curl library. */
/* #undef HAVE_CURL */

/* Define to 1 if you have the curses library. */
/* #undef HAVE_CURSES */

/* Define if your system has the DAHDI headers. */
/* #undef HAVE_DAHDI */

/* Define if your system has the DAHDI_ECHOCANCEL_FAX_MODE headers. */
/* #undef HAVE_DAHDI_ECHOCANCEL_FAX_MODE */

/* Define if your system has the DAHDI_HALF_FULL headers. */
/* #undef HAVE_DAHDI_HALF_FULL */

/* Define if your system has the DAHDI_LINEREVERSE_VMWI headers. */
/* #undef HAVE_DAHDI_LINEREVERSE_VMWI */

/* Define DAHDI headers version */
/* #undef HAVE_DAHDI_VERSION */

/* Define to 1 if your system has /dev/urandom. */
/* #undef HAVE_DEV_URANDOM */

/* Define to 1 if you have the <dirent.h> header file, and it defines `DIR'.
   */
#define HAVE_DIRENT_H 1

/* Define to 1 if your system has the dladdr() GNU extension */
#define HAVE_DLADDR 1

/* Define to 1 if you don't have `vprintf' but do have `_doprnt.' */
/* #undef HAVE_DOPRNT */

/* Define to 1 if you have the `dup2' function. */
#define HAVE_DUP2 1

/* Define to 1 if you have the `eaccess' function. */
/* #undef HAVE_EACCESS */

/* Define to 1 if you have the `endpwent' function. */
#define HAVE_ENDPWENT 1

/* Define to 1 if you have the `euidaccess' function. */
/* #undef HAVE_EUIDACCESS */

/* Define to 1 if you have the `exp' function. */
#define HAVE_EXP 1

/* Define to 1 if you have the `exp10' function. */
/* #undef HAVE_EXP10 */

/* Define to 1 if you have the `exp10l' function. */
/* #undef HAVE_EXP10L */

/* Define to 1 if you have the `exp2' function. */
#define HAVE_EXP2 1

/* Define to 1 if you have the `exp2l' function. */
/* #undef HAVE_EXP2L */

/* Define to 1 if you have the `expl' function. */
/* #undef HAVE_EXPL */

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the Ffmpeg and avcodec library. */
/* #undef HAVE_FFMPEG */

/* Define to 1 if you have the `ffsll' function. */
#define HAVE_FFSLL 1

/* Define to 1 if you have the `floor' function. */
#define HAVE_FLOOR 1

/* Define to 1 if you have the `floorl' function. */
/* #undef HAVE_FLOORL */

/* Define to 1 if you have the `fmod' function. */
#define HAVE_FMOD 1

/* Define to 1 if you have the `fmodl' function. */
/* #undef HAVE_FMODL */

/* Define to 1 if you have the `fopencookie' function. */
#define HAVE_FOPENCOOKIE 1

/* Define to 1 if you have the `fork' function. */
#define HAVE_FORK 1

/* Define to 1 if you have the FreeTDS library. */
/* #undef HAVE_FREETDS */

/* Define to 1 if fseeko (and presumably ftello) exists and is declared. */
#define HAVE_FSEEKO 1

/* Define to 1 if you have the `ftruncate' function. */
#define HAVE_FTRUNCATE 1

/* Define to 1 if you have the `funopen' function. */
/* #undef HAVE_FUNOPEN */

/* Define to 1 if your GCC C compiler provides atomic operations. */
/* #undef HAVE_GCC_ATOMICS */

/* Define to 1 if you have the `getcwd' function. */
#define HAVE_GETCWD 1

/* Define to 1 if you have the `gethostbyname' function. */
#define HAVE_GETHOSTBYNAME 1

/* Define to 1 if your system has gethostbyname_r with 5 arguments. */
/* #undef HAVE_GETHOSTBYNAME_R_5 */

/* Define to 1 if your system has gethostbyname_r with 6 arguments. */
#define HAVE_GETHOSTBYNAME_R_6 1

/* Define to 1 if you have the `gethostname' function. */
#define HAVE_GETHOSTNAME 1

/* Define if your system has the GETIFADDRS headers. */
#define HAVE_GETIFADDRS 1

/* Define to 1 if you have the `getloadavg' function. */
/* #undef HAVE_GETLOADAVG */

/* Define to 1 if you have the `getpagesize' function. */
#define HAVE_GETPAGESIZE 1

/* Define to 1 if you have the `getpeereid' function. */
/* #undef HAVE_GETPEEREID */

/* Define to 1 if you have the `gettimeofday' function. */
#define HAVE_GETTIMEOFDAY 1

/* Define to 1 if you have the `glob' function. */
#define HAVE_GLOB 1

/* Define if your system has the GLOB_BRACE headers. */
#define HAVE_GLOB_BRACE 1

/* Define if your system has the GLOB_NOMAGIC headers. */
#define HAVE_GLOB_NOMAGIC 1

/* Define if your system has the GMIME libraries. */
/* #undef HAVE_GMIME */

/* Define to indicate the GSM library */
/* #undef HAVE_GSM */

/* Define to indicate that gsm.h is in gsm/gsm.h */
/* #undef HAVE_GSM_GSM_HEADER */

/* Define to indicate that gsm.h has no prefix for its location */
#define HAVE_GSM_HEADER 1

/* Define if your system has the GTK2 libraries. */
/* #undef HAVE_GTK2 */

/* Define to 1 if you have the Hoard Memory Allocator library. */
/* #undef HAVE_HOARD */

/* Define to 1 if you have the `htonll' function. */
/* #undef HAVE_HTONLL */

/* Define to 1 if you have the iCal library. */
/* #undef HAVE_ICAL */

/* Define to 1 if you have the Iconv library. */
/* #undef HAVE_ICONV */

/* Define to 1 if you have the Iksemel Jabber library. */
/* #undef HAVE_IKSEMEL */

/* Define if your system has the ILBC libraries. */
/* #undef HAVE_ILBC */

/* Define if your system has the UW IMAP Toolkit c-client library. */
/* #undef HAVE_IMAP_TK */

/* Define if your system has the UW IMAP Toolkit c-client library version 2006
   or greater. */
/* #undef HAVE_IMAP_TK2006 */

/* Define to 1 if you have the `inet_aton' function. */
#define HAVE_INET_ATON 1

/* Define to 1 if you have the `inet_ntoa' function. */
#define HAVE_INET_NTOA 1

/* Define to 1 if you have the inotify support library. */
#define HAVE_INOTIFY 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the iODBC library. */
/* #undef HAVE_IODBC */

/* Define to 1 if you have the `ioperm' function. */
#define HAVE_IOPERM 1

/* Define if your system has the IP_MTU_DISCOVER headers. */
#define HAVE_IP_MTU_DISCOVER 1

/* Define to 1 if you have the `isascii' function. */
#define HAVE_ISASCII 1

/* Define to 1 if you have the ISDN4Linux library. */
/* #undef HAVE_ISDNNET */

/* Define to 1 if you have the Jack Audio Connection Kit library. */
/* #undef HAVE_JACK */

/* Define to 1 if you have the `kevent64' function. */
/* #undef HAVE_KEVENT64 */

/* Define to 1 if you have the kqueue support library. */
/* #undef HAVE_KQUEUE */

/* Define to 1 if you have the OpenLDAP library. */
#define HAVE_LDAP 1

/* Define if your system has the LIBEDIT libraries. */
/* #undef HAVE_LIBEDIT */

/* Define to 1 if you have the <libintl.h> header file. */
/* #undef HAVE_LIBINTL_H */

/* Define to 1 if you have the `m' library (-lm). */
#define HAVE_LIBM 1

/* Define if your system has the LIBXML2 libraries. */
/* #undef HAVE_LIBXML2 */

/* Define to 1 if you have the <limits.h> header file. */
#define HAVE_LIMITS_H 1

/* Define to 1 if your system has linux/compiler.h. */
/* #undef HAVE_LINUX_COMPILER_H */

/* Define to 1 if limits.h includes a LLONG_MAX definition. */
#define HAVE_LLONG_MAX 1

/* Define to 1 if you have the <locale.h> header file. */
#define HAVE_LOCALE_H 1

/* Define to 1 if your system defines the locale_t type in locale.h */
/* #undef HAVE_LOCALE_T_IN_LOCALE_H */

/* Define to 1 if your system defines the locale_t type in xlocale.h */
/* #undef HAVE_LOCALE_T_IN_XLOCALE_H */

/* Define to 1 if you have the `log' function. */
#define HAVE_LOG 1

/* Define to 1 if you have the `log10' function. */
#define HAVE_LOG10 1

/* Define to 1 if you have the `log10l' function. */
/* #undef HAVE_LOG10L */

/* Define to 1 if you have the `log2' function. */
#define HAVE_LOG2 1

/* Define to 1 if you have the `log2l' function. */
/* #undef HAVE_LOG2L */

/* Define to 1 if you have the `logl' function. */
/* #undef HAVE_LOGL */

/* Define to 1 if the type `long double' works and has more range or precision
   than `double'. */
/* #undef HAVE_LONG_DOUBLE_WIDER */

/* Define to 1 if you have the libtool library. */
#define HAVE_LTDL 1

/* Define to 1 if you have the Lua library. */
/* #undef HAVE_LUA */

/* Define to 1 if you have the <malloc.h> header file. */
#define HAVE_MALLOC_H 1

/* Define to 1 if you have the `memchr' function. */
#define HAVE_MEMCHR 1

/* Define to 1 if you have the `memmove' function. */
#define HAVE_MEMMOVE 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `memset' function. */
#define HAVE_MEMSET 1

/* Define to 1 if you have the mISDN user library. */
/* #undef HAVE_MISDN */

/* Define if your system has the MISDN_FAC_ERROR headers. */
/* #undef HAVE_MISDN_FAC_ERROR */

/* Define if your system has the MISDN_FAC_RESULT headers. */
/* #undef HAVE_MISDN_FAC_RESULT */

/* Define to 1 if you have the `mkdir' function. */
#define HAVE_MKDIR 1

/* Define to 1 if you have the `mkdtemp' function. */
#define HAVE_MKDTEMP 1

/* Define to 1 if you have a working `mmap' system call. */
/* #undef HAVE_MMAP */

/* Define if your system has the MSG_NOSIGNAL headers. */
#define HAVE_MSG_NOSIGNAL 1

/* Define to 1 if you have the `munmap' function. */
#define HAVE_MUNMAP 1

/* Define if your system has the MYSQLCLIENT libraries. */
/* #undef HAVE_MYSQLCLIENT */

/* Define to 1 if you have the Network Broadcast Sound library. */
/* #undef HAVE_NBS */

/* Define to 1 if you have the ncurses library. */
#define HAVE_NCURSES 1

/* Define to 1 if you have the <ndir.h> header file, and it defines `DIR'. */
/* #undef HAVE_NDIR_H */

/* Define if your system has the NEON libraries. */
/* #undef HAVE_NEON */

/* Define if your system has the NEON29 libraries. */
/* #undef HAVE_NEON29 */

/* Define to 1 if you have the <netdb.h> header file. */
#define HAVE_NETDB_H 1

/* Define to 1 if you have the <netinet/in.h> header file. */
#define HAVE_NETINET_IN_H 1

/* Define if your system has the NETSNMP libraries. */
/* #undef HAVE_NETSNMP */

/* Define to 1 if you have the `newlocale' function. */
/* #undef HAVE_NEWLOCALE */

/* Define to 1 if you have the newt library. */
/* #undef HAVE_NEWT */

/* Define to 1 if you have the `ntohll' function. */
/* #undef HAVE_NTOHLL */

/* Define to 1 if your C library can safely print NULL to string formats. */
/* #undef HAVE_NULLSAFE_PRINTF */

/* Define to 1 if your ODBC library has wide (Unicode) types. */
#define HAVE_ODBC_WCHAR 1

/* Define to 1 if you have the OGG library. */
/* #undef HAVE_OGG */

/* Define if your system has the OpenH323 libraries. */
/* #undef HAVE_OPENH323 */

/* Define to 1 if you have the MFR2 library. */
/* #undef HAVE_OPENR2 */

/* Define to 1 if you have the OpenSSL Secure Sockets Layer library. */
#define HAVE_OPENSSL 1

/* Define to 1 if CRYPTO has the OpenSSL SRTP Extension Support feature. */
#define HAVE_OPENSSL_SRTP 1

/* Define this to indicate the ${OSPTK_DESCRIP} library */
/* #undef HAVE_OSPTK */

/* Define to 1 if you have the Open Sound System library. */
#define HAVE_OSS 1

/* Define to 1 if OSX atomic operations are supported. */
/* #undef HAVE_OSX_ATOMICS */

/* Define to 1 if your system defines the file flag O_EVTONLY in fcntl.h */
/* #undef HAVE_O_EVTONLY */

/* Define to 1 if your system defines the file flag O_SYMLINK in fcntl.h */
/* #undef HAVE_O_SYMLINK */

/* Define to indicate the PostgreSQL library */
/* #undef HAVE_PGSQL */

/* Define to indicate presence of the pg_encoding_to_char API. */
/* #undef HAVE_PGSQL_pg_encoding_to_char */

/* Define to 1 if your system defines IP_PKTINFO. */
#define HAVE_PKTINFO 1

/* Define to 1 if you have the popt library. */
#define HAVE_POPT 1

/* Define to 1 if you have the PortAudio library. */
/* #undef HAVE_PORTAUDIO */

/* Define to 1 if you have the `pow' function. */
/* #undef HAVE_POW */

/* Define to 1 if you have the `powl' function. */
/* #undef HAVE_POWL */

/* Define to 1 if you have the `ppoll' function. */
#define HAVE_PPOLL 1

/* Define to 1 if you have the ISDN PRI library. */
/* #undef HAVE_PRI */

/* Define to 1 if PRI has the ISDN PRI advice of charge supplementary service
   events feature. */
/* #undef HAVE_PRI_AOC_EVENTS */

/* Define to 1 if PRI has the ISDN PRI call hold feature. */
/* #undef HAVE_PRI_CALL_HOLD */

/* Define to 1 if PRI has the ISDN PRI call rerouting and call deflection
   feature. */
/* #undef HAVE_PRI_CALL_REROUTING */

/* Define to 1 if PRI has the ISDN PRI call waiting supplementary service
   feature. */
/* #undef HAVE_PRI_CALL_WAITING */

/* Define to 1 if PRI has the ISDN PRI call completion supplementary service
   feature. */
/* #undef HAVE_PRI_CCSS */

/* Define to 1 if PRI has the ISDN PRI Date/time ie send policy feature. */
/* #undef HAVE_PRI_DATETIME_SEND */

/* Define to 1 if PRI has the ISDN PRI user display text IE contents during
   call feature. */
/* #undef HAVE_PRI_DISPLAY_TEXT */

/* Define to 1 if PRI has the ISDN PRI hangup fix feature. */
/* #undef HAVE_PRI_HANGUP_FIX */

/* Define to 1 if PRI has the ISDN PRI set_inbanddisconnect feature. */
/* #undef HAVE_PRI_INBANDDISCONNECT */

/* Define to 1 if PRI has the ISDN Layer 2 persistence option feature. */
/* #undef HAVE_PRI_L2_PERSISTENCE */

/* Define to 1 if PRI has the ISDN PRI Malicious Call ID feature. */
/* #undef HAVE_PRI_MCID */

/* Define to 1 if PRI has the ISDN PRI Message Waiting Indication feature. */
/* #undef HAVE_PRI_MWI */

/* Define to 1 if PRI has the ISDN PRI Message Waiting Indication (Fixed)
   feature. */
/* #undef HAVE_PRI_MWI_V2 */

/* Define to 1 if PRI has the ISDN progress with cause feature. */
/* #undef HAVE_PRI_PROG_W_CAUSE */

/* Define to 1 if PRI has the ISDN reverse charge feature. */
/* #undef HAVE_PRI_REVERSE_CHARGE */

/* Define to 1 if PRI has the ISDN service messages feature. */
/* #undef HAVE_PRI_SERVICE_MESSAGES */

/* Define to 1 if PRI has the ISDN PRI progress inband ie in SETUP ACK
   feature. */
/* #undef HAVE_PRI_SETUP_ACK_INBAND */

/* Define to 1 if PRI has the ISDN PRI keypad facility in SETUP feature. */
/* #undef HAVE_PRI_SETUP_KEYPAD */

/* Define to 1 if PRI has the ISDN PRI subaddressing feature. */
/* #undef HAVE_PRI_SUBADDR */

/* Define to 1 if PRI has the ISDN PRI call transfer supplementary service
   feature. */
/* #undef HAVE_PRI_TRANSFER */

/* Define if you have POSIX threads libraries and header files. */
#define HAVE_PTHREAD 1

/* Define to 1 if your system defines PTHREAD_MUTEX_RECURSIVE_NP in pthread.h
   */
#define HAVE_PTHREAD_MUTEX_RECURSIVE_NP 1

/* Define if your system has the PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
   headers. */
#define HAVE_PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP 1

/* Define if your system has the PTHREAD_RWLOCK_INITIALIZER headers. */
#define HAVE_PTHREAD_RWLOCK_INITIALIZER 1

/* Define to 1 if your system defines PTHREAD_RWLOCK_PREFER_WRITER_NP in
   pthread.h */
#define HAVE_PTHREAD_RWLOCK_PREFER_WRITER_NP 1

/* Define if your system has pthread_rwlock_timedwrlock() */
#define HAVE_PTHREAD_RWLOCK_TIMEDWRLOCK 1

/* Define to 1 if the system has the type `ptrdiff_t'. */
#define HAVE_PTRDIFF_T 1

/* Define to 1 if you have the `putenv' function. */
#define HAVE_PUTENV 1

/* Define if your system has the PWLib libraries. */
/* #undef HAVE_PWLIB */

/* Define to 1 if you have the Radius Client library. */
/* #undef HAVE_RADIUS */

/* Define to 1 if you have the `regcomp' function. */
#define HAVE_REGCOMP 1

/* Define to 1 if you have the `remainder' function. */
#define HAVE_REMAINDER 1

/* Define to 1 if you have the `remainderl' function. */
/* #undef HAVE_REMAINDERL */

/* Define to 1 if you have the LIBRESAMPLE library. */
/* #undef HAVE_RESAMPLE */

/* Define to 1 if your system has the close resolver function. */
/* #undef HAVE_RES_CLOSE */

/* Define to 1 if your system has the ndestroy resolver function. */
/* #undef HAVE_RES_NDESTROY */

/* Define to 1 if your system has the re-entrant resolver functions. */
/* #undef HAVE_RES_NINIT */

/* Define to 1 if you have the `re_comp' function. */
#define HAVE_RE_COMP 1

/* Define to 1 if you have the `rint' function. */
#define HAVE_RINT 1

/* Define to 1 if you have the `rintl' function. */
/* #undef HAVE_RINTL */

/* Define to 1 if you have the `round' function. */
#define HAVE_ROUND 1

/* Define to 1 if you have the `roundf' function. */
#define HAVE_ROUNDF 1

/* Define to 1 if you have the `roundl' function. */
/* #undef HAVE_ROUNDL */

/* Define to 1 if your system has /sbin/launchd. */
/* #undef HAVE_SBIN_LAUNCHD */

/* Define if your system has the SDL libraries. */
/* #undef HAVE_SDL */

/* Define to 1 if you have the Sdl Image library. */
/* #undef HAVE_SDL_IMAGE */

/* Define to 1 if you have the `select' function. */
#define HAVE_SELECT 1

/* Define to 1 if you have the `setenv' function. */
#define HAVE_SETENV 1

/* Define to 1 if you have the `sin' function. */
#define HAVE_SIN 1

/* Define to 1 if you have the `sinl' function. */
/* #undef HAVE_SINL */

/* Define to 1 if you have the `socket' function. */
#define HAVE_SOCKET 1

/* Define to 1 if your system has soxmix application. */
/* #undef HAVE_SOXMIX */

/* Define if your system has the SO_NOSIGPIPE headers. */
/* #undef HAVE_SO_NOSIGPIPE */

/* Define if your system has the SPANDSP headers. */
/* #undef HAVE_SPANDSP */

/* Define to 1 if you have the Speex library. */
#define HAVE_SPEEX 1

/* Define to 1 if you have the SpeexDSP library. */
/* #undef HAVE_SPEEXDSP */

/* Define to 1 if you have the speex_preprocess_ctl library. */
/* #undef HAVE_SPEEX_PREPROCESS */

/* Define to 1 if you have the SQLite library. */
/* #undef HAVE_SQLITE */

/* Define to 1 if you have the SQLite library. */
#define HAVE_SQLITE3 1

/* Define to 1 if you have the `sqrtl' function. */
/* #undef HAVE_SQRTL */

/* Define to 1 if you have the Secure RTP library. */
/* #undef HAVE_SRTP */

/* Define to 1 if SRTP has the SRTP Library Shutdown Function feature. */
/* #undef HAVE_SRTP_SHUTDOWN */

/* Define to 1 if you have the ISDN SS7 library. */
/* #undef HAVE_SS7 */

/* Define to 1 if `stat' has the bug that it succeeds when given the
   zero-length file name argument. */
/* #undef HAVE_STAT_EMPTY_STRING_BUG */

/* Define to 1 if stdbool.h conforms to C99. */
#define HAVE_STDBOOL_H 1

/* Define to 1 if you have the <stddef.h> header file. */
#define HAVE_STDDEF_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strcasecmp' function. */
#define HAVE_STRCASECMP 1

/* Define to 1 if you have the `strcasestr' function. */
#define HAVE_STRCASESTR 1

/* Define to 1 if you have the `strchr' function. */
#define HAVE_STRCHR 1

/* Define to 1 if you have the `strcoll' function and it is properly defined.
   */
/* #undef HAVE_STRCOLL */

/* Define to 1 if you have the `strcspn' function. */
#define HAVE_STRCSPN 1

/* Define to 1 if you have the `strdup' function. */
#define HAVE_STRDUP 1

/* Define to 1 if you have the `strerror' function. */
#define HAVE_STRERROR 1

/* Define to 1 if you have the `strftime' function. */
#define HAVE_STRFTIME 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strlcat' function. */
#define HAVE_STRLCAT 1

/* Define to 1 if you have the `strlcpy' function. */
#define HAVE_STRLCPY 1

/* Define to 1 if you have the `strncasecmp' function. */
#define HAVE_STRNCASECMP 1

/* Define to 1 if you have the `strndup' function. */
#define HAVE_STRNDUP 1

/* Define to 1 if you have the `strnlen' function. */
#define HAVE_STRNLEN 1

/* Define to 1 if you have the `strrchr' function. */
#define HAVE_STRRCHR 1

/* Define to 1 if you have the `strsep' function. */
#define HAVE_STRSEP 1

/* Define to 1 if you have the `strspn' function. */
#define HAVE_STRSPN 1

/* Define to 1 if you have the `strstr' function. */
#define HAVE_STRSTR 1

/* Define to 1 if you have the `strtod' function. */
/* #undef HAVE_STRTOD */

/* Define to 1 if you have the `strtol' function. */
#define HAVE_STRTOL 1

/* Define to 1 if you have the `strtold' function. */
#define HAVE_STRTOLD 1

/* Define to 1 if you have the `strtoq' function. */
#define HAVE_STRTOQ 1

/* Define to 1 if `ifr_ifru.ifru_hwaddr' is a member of `struct ifreq'. */
#define HAVE_STRUCT_IFREQ_IFR_IFRU_IFRU_HWADDR 1

/* Define to 1 if `uid' is a member of `struct sockpeercred'. */
/* #undef HAVE_STRUCT_SOCKPEERCRED_UID */

/* Define to 1 if `st_blksize' is a member of `struct stat'. */
#define HAVE_STRUCT_STAT_ST_BLKSIZE 1

/* Define to 1 if `st_mtim' is a member of `struct stat'. */
#define HAVE_STRUCT_STAT_ST_MTIM 1

/* Define to 1 if `st_mtimensec' is a member of `struct stat'. */
/* #undef HAVE_STRUCT_STAT_ST_MTIMENSEC */

/* Define to 1 if `st_mtimespec' is a member of `struct stat'. */
/* #undef HAVE_STRUCT_STAT_ST_MTIMESPEC */

/* Define to 1 if `cr_uid' is a member of `struct ucred'. */
/* #undef HAVE_STRUCT_UCRED_CR_UID */

/* Define to 1 if `uid' is a member of `struct ucred'. */
#define HAVE_STRUCT_UCRED_UID 1

/* Define to 1 if you have the mISDN Supplemental Services library. */
/* #undef HAVE_SUPPSERV */

/* Define to 1 if you have the `swapctl' function. */
/* #undef HAVE_SWAPCTL */

/* Define to 1 if you have the `sysctl' function. */
#define HAVE_SYSCTL 1

/* Define to 1 if your system has sysinfo support */
#define HAVE_SYSINFO 1

/* Define if your system has the SYSLOG_FACILITY_LOG_AUTH headers. */
#define HAVE_SYSLOG_FACILITY_LOG_AUTH 1

/* Define if your system has the SYSLOG_FACILITY_LOG_AUTHPRIV headers. */
#define HAVE_SYSLOG_FACILITY_LOG_AUTHPRIV 1

/* Define if your system has the SYSLOG_FACILITY_LOG_CRON headers. */
#define HAVE_SYSLOG_FACILITY_LOG_CRON 1

/* Define if your system has the SYSLOG_FACILITY_LOG_DAEMON headers. */
#define HAVE_SYSLOG_FACILITY_LOG_DAEMON 1

/* Define if your system has the SYSLOG_FACILITY_LOG_FTP headers. */
#define HAVE_SYSLOG_FACILITY_LOG_FTP 1

/* Define if your system has the SYSLOG_FACILITY_LOG_KERN headers. */
#define HAVE_SYSLOG_FACILITY_LOG_KERN 1

/* Define if your system has the SYSLOG_FACILITY_LOG_LPR headers. */
#define HAVE_SYSLOG_FACILITY_LOG_LPR 1

/* Define if your system has the SYSLOG_FACILITY_LOG_MAIL headers. */
#define HAVE_SYSLOG_FACILITY_LOG_MAIL 1

/* Define if your system has the SYSLOG_FACILITY_LOG_NEWS headers. */
#define HAVE_SYSLOG_FACILITY_LOG_NEWS 1

/* Define if your system has the SYSLOG_FACILITY_LOG_SYSLOG headers. */
#define HAVE_SYSLOG_FACILITY_LOG_SYSLOG 1

/* Define if your system has the SYSLOG_FACILITY_LOG_UUCP headers. */
#define HAVE_SYSLOG_FACILITY_LOG_UUCP 1

/* Define to 1 if you have the <syslog.h> header file. */
#define HAVE_SYSLOG_H 1

/* Define to 1 if you have the <sys/dir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_DIR_H */

/* Define to 1 if your sys/endian.h header file provides the bswap16 macro. */
/* #undef HAVE_SYS_ENDIAN_BSWAP16 */

/* Define to 1 if your sys/endian.h header file provides the __swap16 macro.
   */
/* #undef HAVE_SYS_ENDIAN_SWAP16 */

/* Define to 1 if you have the <sys/event.h> header file. */
/* #undef HAVE_SYS_EVENT_H */

/* Define to 1 if you have the <sys/file.h> header file. */
#define HAVE_SYS_FILE_H 1

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#define HAVE_SYS_IOCTL_H 1

/* Define to 1 if you have the <sys/io.h> header file. */
/* #undef HAVE_SYS_IO_H */

/* Define to 1 if you have the <sys/ndir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_NDIR_H */

/* Define to 1 if you have the <sys/param.h> header file. */
#define HAVE_SYS_PARAM_H 1

/* Define to 1 if your system has working sys/poll.h */
#define HAVE_SYS_POLL_H 1

/* Define to 1 if you have the <sys/select.h> header file. */
#define HAVE_SYS_SELECT_H 1

/* Define to 1 if you have the <sys/socket.h> header file. */
#define HAVE_SYS_SOCKET_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/thr.h> header file. */
/* #undef HAVE_SYS_THR_H */

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have <sys/wait.h> that is POSIX.1 compatible. */
#define HAVE_SYS_WAIT_H 1

/* Define to 1 if you have the `tan' function. */
#define HAVE_TAN 1

/* Define to 1 if you have the `tanl' function. */
/* #undef HAVE_TANL */

/* Define to 1 if you have the Termcap library. */
/* #undef HAVE_TERMCAP */

/* Define to 1 if you have the <termios.h> header file. */
#define HAVE_TERMIOS_H 1

/* Define if your system has the TIMERFD headers. */
#define HAVE_TIMERFD 1

/* Define to 1 if your system defines timersub. */
#define HAVE_TIMERSUB 1

/* Define to 1 if you have the Term Info library. */
/* #undef HAVE_TINFO */

/* Define to 1 if you have the tonezone library. */
/* #undef HAVE_TONEZONE */

/* Define to 1 if you have the `trunc' function. */
#define HAVE_TRUNC 1

/* Define to 1 if you have the `truncl' function. */
/* #undef HAVE_TRUNCL */

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the unixODBC library. */
#define HAVE_UNIXODBC 1

/* Define to 1 if you have the `unsetenv' function. */
#define HAVE_UNSETENV 1

/* Define to 1 if you have the `utime' function. */
#define HAVE_UTIME 1

/* Define to 1 if you have the <utime.h> header file. */
#define HAVE_UTIME_H 1

/* Define to 1 if `utime(file, NULL)' sets file's timestamp to the present. */
#define HAVE_UTIME_NULL 1

/* Define to 1 if you have the UUID library. */
#define HAVE_UUID 1

/* Define to 1 if your system can support larger than default select bitmasks.
   */
/* #undef HAVE_VARIABLE_FDSET */

/* Define to 1 if you have the `vasprintf' function. */
#define HAVE_VASPRINTF 1

/* Define to 1 if you have the `vfork' function. */
#define HAVE_VFORK 1

/* Define to 1 if you have the <vfork.h> header file. */
/* #undef HAVE_VFORK_H */

/* Define to 1 if your system has linux/videodev.h. */
/* #undef HAVE_VIDEODEV_H */

/* Define to 1 if you have the Vorbis library. */
/* #undef HAVE_VORBIS */

/* Define if your system has OV_CALLBACKS_NOCLOSE declared. */
#define HAVE_VORBIS_OPEN_CALLBACKS 1

/* Define if your system has the VoiceTronix API libraries. */
/* #undef HAVE_VPB */

/* Define to 1 if you have the `vprintf' function. */
#define HAVE_VPRINTF 1

/* Define to 1 if you have the <winsock2.h> header file. */
/* #undef HAVE_WINSOCK2_H */

/* Define to 1 if you have the <winsock.h> header file. */
/* #undef HAVE_WINSOCK_H */

/* Define to 1 if `fork' works. */
#define HAVE_WORKING_FORK 1

/* Define to 1 if `vfork' works. */
#define HAVE_WORKING_VFORK 1

/* Define to 1 if you have the X11 library. */
/* #undef HAVE_X11 */

/* Define to the version of the X11 library. */
/* #undef HAVE_X11_VERSION */

/* Define to 1 if you have the <xlocale.h> header file. */
/* #undef HAVE_XLOCALE_H */

/* Define to 1 if you have the zlib compression library. */
#define HAVE_ZLIB 1

/* Define to 1 if the system has the type `_Bool'. */
#define HAVE__BOOL 1

/* Defined if libcurl supports AsynchDNS */
/* #undef LIBCURL_FEATURE_ASYNCHDNS */

/* Defined if libcurl supports IDN */
/* #undef LIBCURL_FEATURE_IDN */

/* Defined if libcurl supports IPv6 */
/* #undef LIBCURL_FEATURE_IPV6 */

/* Defined if libcurl supports KRB4 */
/* #undef LIBCURL_FEATURE_KRB4 */

/* Defined if libcurl supports libz */
/* #undef LIBCURL_FEATURE_LIBZ */

/* Defined if libcurl supports NTLM */
/* #undef LIBCURL_FEATURE_NTLM */

/* Defined if libcurl supports SSL */
/* #undef LIBCURL_FEATURE_SSL */

/* Defined if libcurl supports SSPI */
/* #undef LIBCURL_FEATURE_SSPI */

/* Defined if libcurl supports DICT */
/* #undef LIBCURL_PROTOCOL_DICT */

/* Defined if libcurl supports FILE */
/* #undef LIBCURL_PROTOCOL_FILE */

/* Defined if libcurl supports FTP */
/* #undef LIBCURL_PROTOCOL_FTP */

/* Defined if libcurl supports FTPS */
/* #undef LIBCURL_PROTOCOL_FTPS */

/* Defined if libcurl supports HTTP */
/* #undef LIBCURL_PROTOCOL_HTTP */

/* Defined if libcurl supports HTTPS */
/* #undef LIBCURL_PROTOCOL_HTTPS */

/* Defined if libcurl supports LDAP */
/* #undef LIBCURL_PROTOCOL_LDAP */

/* Defined if libcurl supports TELNET */
/* #undef LIBCURL_PROTOCOL_TELNET */

/* Defined if libcurl supports TFTP */
/* #undef LIBCURL_PROTOCOL_TFTP */

/* Define to 1 if `lstat' dereferences a symlink specified with a trailing
   slash. */
#define LSTAT_FOLLOWS_SLASHED_SYMLINK 1

/* Build chan_misdn for mISDN 1.2 or later. */
/* #undef MISDN_1_2 */

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "https://issues.asterisk.org"

/* Define to the full name of this package. */
#define PACKAGE_NAME "asterisk"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "asterisk trunk"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "asterisk"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "trunk"

/* Define to necessary symbol if this constant uses a non-standard name on
   your system. */
/* #undef PTHREAD_CREATE_JOINABLE */

/* Define if your system needs braces around PTHREAD_ONCE_INIT */
/* #undef PTHREAD_ONCE_INIT_NEEDS_BRACES */

/* Define as the return type of signal handlers (`int' or `void'). */
#define RETSIGTYPE void

/* Define to the type of arg 1 for `select'. */
#define SELECT_TYPE_ARG1 int

/* Define to the type of args 2, 3 and 4 for `select'. */
#define SELECT_TYPE_ARG234 (fd_set *)

/* Define to the type of arg 5 for `select'. */
#define SELECT_TYPE_ARG5 (struct timeval *)

/* The size of `char *', as computed by sizeof. */
#define SIZEOF_CHAR_P 4

/* The size of `fd_set.fds_bits', as computed by sizeof. */
#define SIZEOF_FD_SET_FDS_BITS 4

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 4

/* The size of `long long', as computed by sizeof. */
#define SIZEOF_LONG_LONG 8

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at runtime.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown */
/* #undef STACK_DIRECTION */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#define TIME_WITH_SYS_TIME 1

/* Define to 1 if your <sys/time.h> declares `struct tm'. */
/* #undef TM_IN_SYS_TIME */

/* Define to a type of the same size as fd_set.fds_bits[[0]] */
#define TYPEOF_FD_SET_FDS_BITS int

/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# define _ALL_SOURCE 1
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif
/* Enable threading extensions on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# define _POSIX_PTHREAD_SEMANTICS 1
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# define _TANDEM_SOURCE 1
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# define __EXTENSIONS__ 1
#endif


/* Define to 1 if running on Darwin. */
/* #undef _DARWIN_UNLIMITED_SELECT */

/* Enable large inode numbers on Mac OS X 10.5.  */
#ifndef _DARWIN_USE_64_BIT_INODE
# define _DARWIN_USE_64_BIT_INODE 1
#endif

/* Number of bits in a file offset, on hosts where this is settable. */
#define _FILE_OFFSET_BITS 64

/* Define to 1 to make fseeko visible on some hosts (e.g. glibc 2.2). */
/* #undef _LARGEFILE_SOURCE */

/* Define for large files, on AIX-style hosts. */
/* #undef _LARGE_FILES */

/* Define to 1 if on MINIX. */
/* #undef _MINIX */

/* Define to 2 if the system does not provide POSIX.1 features except with
   this defined. */
/* #undef _POSIX_1_SOURCE */

/* Define to 1 if you need to in order for `stat' and other things to work. */
/* #undef _POSIX_SOURCE */

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define curl_free() as free() if our version of curl lacks curl_free. */
/* #undef curl_free */

/* Define to `int' if <sys/types.h> doesn't define. */
/* #undef gid_t */

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif

/* Define to `int' if <sys/types.h> does not define. */
/* #undef mode_t */

/* Define to `long int' if <sys/types.h> does not define. */
/* #undef off_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef pid_t */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */

/* Define to `int' if <sys/types.h> doesn't define. */
/* #undef uid_t */

/* Define as `fork' if `vfork' does not work. */
/* #undef vfork */

/* Define to empty if the keyword `volatile' does not work. Warning: valid
   code using `volatile' can become incorrect without. Disable with care. */
/* #undef volatile */

#endif

