/* x86_64-pc-linux-gnu/config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.in by autoheader.  */


#define GHBN_R_SOLARIS  2
#define GHBN_R_AIX      3
#define GHBN_R_GLIBC    4


/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* Assumed cache-line size (in bytes) */
#define ASSUMED_CACHE_LINE_SIZE 64

/* Define the brk() argument type. */
#define BRK_ARG_TYPE void *

/* Define the brk() return type. */
#define BRK_RET_TYPE int

/* Define if the code model is small (code fits below 2Gb) */
/* #undef CODE_MODEL_SMALL */

/* Define if you need to include rpc/types.h to get INADDR_LOOPBACK defined */
/* #undef DEF_INADDR_LOOPBACK_IN_RPC_TYPES_H */

/* Define if you need to include winsock2.h to get INADDR_LOOPBACK defined */
/* #undef DEF_INADDR_LOOPBACK_IN_WINSOCK2_H */

/* Define if you want to disable vfork. */
/* #undef DISABLE_VFORK */

/* Define to 1 if your processor stores the words in a double in middle-endian
   format (like some ARMs). */
/* #undef DOUBLE_MIDDLE_ENDIAN */

/* Define if sbrk()/brk() wrappers can track malloc()s core memory use */
/* #undef ERTS_BRK_WRAPPERS_CAN_TRACK_MALLOC */

/* Define to override the default number of write_concurrency locks */
/* #undef ERTS_DB_HASH_LOCK_CNT */

/* The only reason ERTS_EMU_CMDLINE_FLAGS exists is to force modification of
   config.h when the emulator command line flags are modified by configure */
#define ERTS_EMU_CMDLINE_FLAGS " -O2 -I/home/alter0ne/rtk_openwrt_sdk/staging_dir/host/include -D_GNU_SOURCE -I/home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_21.0/erts/x86_64-pc-linux-gnu   -fno-tree-copyrename -I/home/alter0ne/rtk_openwrt_sdk/staging_dir/host/include -D_GNU_SOURCE -g -I/home/alter0ne/rtk_openwrt_sdk/staging_dir/host/include -D_GNU_SOURCE -I/home/alter0ne/rtk_openwrt_sdk/build_dir/host/otp_src_21.0/erts/x86_64-pc-linux-gnu     -DUSE_THREADS -D_THREAD_SAFE -D_REENTRANT -DPOSIX_THREADS -D_POSIX_THREAD_SAFE_FUNCTIONS  -Werror=undef -Werror=implicit -Werror=return-type  -Wall -Wstrict-prototypes -Wmissing-prototypes -Wdeclaration-after-statement"

/* Define if you have kernel poll and want to use it */
#define ERTS_ENABLE_KERNEL_POLL 1

/* Define as 1 if you want to enable microstate accounting, 2 if you want
   extra states */
#define ERTS_ENABLE_MSACC 1

/* Define > 0 if big-endian < 0 if little-endian, or 0 if unknown */
#define ERTS_ENDIANNESS -1

/* Define if OS monotonic clock is corrected */
#define ERTS_HAVE_CORRECTED_OS_MONOTONIC_TIME 1

/* Define if you have a low resolution OS monotonic clock */
/* #undef ERTS_HAVE_LOW_RESOLUTION_OS_MONOTONIC_LOW */

/* Define if dlopen() needs to be called before first call to dlerror() */
/* #undef ERTS_NEED_DLOPEN_BEFORE_DLERROR */

/* Type qualifier restrict */
#define ERTS_RESTRICT restrict

/* Save compile time? */
#define ERTS_SAVED_COMPILE_TIME 1

/* Define if structure alignment is enough for allocators. If not defined,
   64-bit alignment will be forced. */
#define ERTS_STRUCTURE_ALIGNED_ALLOC 1

/* Define if poll() should be used instead of select() */
#define ERTS_USE_POLL 1

/* Define if __after_morecore_hook can track malloc()s core memory use. */
/* #undef ERTS___AFTER_MORECORE_HOOK_CAN_TRACK_MALLOC */

/* Define if bigendian */
/* #undef ETHR_BIGENDIAN */

/* Define if gcc wont let you clobber ebx with cmpxchg8b and position
   independent code */
/* #undef ETHR_CMPXCHG8B_PIC_NO_CLOBBER_EBX */

/* Define if you get a register shortage with cmpxchg8b and position
   independent code */
/* #undef ETHR_CMPXCHG8B_REGISTER_SHORTAGE */

/* Define if you want to disable native ethread implementations */
/* #undef ETHR_DISABLE_NATIVE_IMPLS */

/* Define if you want to force usage of pthread rwlocks */
/* #undef ETHR_FORCE_PTHREAD_RWLOCK */

/* Define if you use a gcc that supports the double word cmpxchg instruction
   */
#define ETHR_GCC_HAVE_DW_CMPXCHG_ASM_SUPPORT 1

/* Define if you use a gcc that supports -msse2 and understand sse2 specific
   asm statements */
/* #undef ETHR_GCC_HAVE_SSE2_ASM_SUPPORT */

/* Define if you have a clock_gettime() with a monotonic clock */
#define ETHR_HAVE_CLOCK_GETTIME_MONOTONIC 1

/* Define if you have all ethread defines */
#define ETHR_HAVE_ETHREAD_DEFINES 1

/* Define as a boolean indicating whether you have a gcc compatible compiler
   capable of generating the ARM DMB instruction, and are compiling for an ARM
   processor with ARM DMB instruction support, or not */
#define ETHR_HAVE_GCC_ASM_ARM_DMB_INSTRUCTION 0

/* Define as a boolean indicating whether you have a gcc __atomic builtins or
   not */
#define ETHR_HAVE_GCC___ATOMIC_BUILTINS 1

/* Define if you have a monotonic gethrtime() */
/* #undef ETHR_HAVE_GETHRTIME */

/* Define if you have libatomic_ops atomic operations */
/* #undef ETHR_HAVE_LIBATOMIC_OPS */

/* Define if you have a linux futex implementation. */
#define ETHR_HAVE_LINUX_FUTEX 1

/* Define if you have a mach clock_get_time() with a monotonic clock */
/* #undef ETHR_HAVE_MACH_CLOCK_GET_TIME */

/* Define if the pthread.h header file is in pthread/mit directory. */
/* #undef ETHR_HAVE_MIT_PTHREAD_H */

/* Define if you have the pthread_attr_setguardsize function. */
#define ETHR_HAVE_PTHREAD_ATTR_SETGUARDSIZE 1

/* Define if pthread_cond_timedwait() can be used with a monotonic clock */
#define ETHR_HAVE_PTHREAD_COND_TIMEDWAIT_MONOTONIC 1

/* Define if you have ibm style pthread_getname_np */
/* #undef ETHR_HAVE_PTHREAD_GETNAME_NP_2 */

/* Define if you have linux style pthread_getname_np */
#define ETHR_HAVE_PTHREAD_GETNAME_NP_3 1

/* Define if you have the <pthread.h> header file. */
#define ETHR_HAVE_PTHREAD_H 1

/* Define if you have the pthread_rwlockattr_setkind_np() function. */
#define ETHR_HAVE_PTHREAD_RWLOCKATTR_SETKIND_NP 1

/* Define if you have the PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP rwlock
   attribute. */
#define ETHR_HAVE_PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP 1

/* Define if you have darwin style pthread_setname_np */
/* #undef ETHR_HAVE_PTHREAD_SETNAME_NP_1 */

/* Define if you have linux style pthread_setname_np */
#define ETHR_HAVE_PTHREAD_SETNAME_NP_2 1

/* Define if you have bsd style pthread_set_name_np */
/* #undef ETHR_HAVE_PTHREAD_SET_NAME_NP_2 */

/* Define if you have the pthread_spin_lock function. */
#define ETHR_HAVE_PTHREAD_SPIN_LOCK 1

/* Define if you have the pthread_yield() function. */
#define ETHR_HAVE_PTHREAD_YIELD 1

/* Define if you have the <sched.h> header file. */
#define ETHR_HAVE_SCHED_H 1

/* Define if you have the sched_yield() function. */
#define ETHR_HAVE_SCHED_YIELD 1

/* Define if you have the <sys/time.h> header file. */
#define ETHR_HAVE_SYS_TIME_H 1

/* Define if you can use PTHREAD_STACK_MIN */
#define ETHR_HAVE_USABLE_PTHREAD_STACK_MIN 1

/* Define if you have _InterlockedAnd() */
/* #undef ETHR_HAVE__INTERLOCKEDAND */

/* Define if you have _InterlockedAnd64() */
/* #undef ETHR_HAVE__INTERLOCKEDAND64 */

/* Define if you have _InterlockedCompareExchange() */
/* #undef ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE */

/* Define if you have _InterlockedCompareExchange128() */
/* #undef ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE128 */

/* Define if you have _InterlockedCompareExchange64() */
/* #undef ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE64 */

/* Define if you have _InterlockedCompareExchange64_acq() */
/* #undef ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE64_ACQ */

/* Define if you have _InterlockedCompareExchange64_rel() */
/* #undef ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE64_REL */

/* Define if you have _InterlockedCompareExchange_acq() */
/* #undef ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE_ACQ */

/* Define if you have _InterlockedCompareExchange_rel() */
/* #undef ETHR_HAVE__INTERLOCKEDCOMPAREEXCHANGE_REL */

/* Define if you have _InterlockedDecrement() */
/* #undef ETHR_HAVE__INTERLOCKEDDECREMENT */

/* Define if you have _InterlockedDecrement64() */
/* #undef ETHR_HAVE__INTERLOCKEDDECREMENT64 */

/* Define if you have _InterlockedDecrement64_rel() */
/* #undef ETHR_HAVE__INTERLOCKEDDECREMENT64_REL */

/* Define if you have _InterlockedDecrement_rel() */
/* #undef ETHR_HAVE__INTERLOCKEDDECREMENT_REL */

/* Define if you have _InterlockedExchange() */
/* #undef ETHR_HAVE__INTERLOCKEDEXCHANGE */

/* Define if you have _InterlockedExchange64() */
/* #undef ETHR_HAVE__INTERLOCKEDEXCHANGE64 */

/* Define if you have _InterlockedExchangeAdd() */
/* #undef ETHR_HAVE__INTERLOCKEDEXCHANGEADD */

/* Define if you have _InterlockedExchangeAdd64() */
/* #undef ETHR_HAVE__INTERLOCKEDEXCHANGEADD64 */

/* Define if you have _InterlockedExchangeAdd64_acq() */
/* #undef ETHR_HAVE__INTERLOCKEDEXCHANGEADD64_ACQ */

/* Define if you have _InterlockedExchangeAdd_acq() */
/* #undef ETHR_HAVE__INTERLOCKEDEXCHANGEADD_ACQ */

/* Define if you have _InterlockedIncrement() */
/* #undef ETHR_HAVE__INTERLOCKEDINCREMENT */

/* Define if you have _InterlockedIncrement64() */
/* #undef ETHR_HAVE__INTERLOCKEDINCREMENT64 */

/* Define if you have _InterlockedIncrement64_acq() */
/* #undef ETHR_HAVE__INTERLOCKEDINCREMENT64_ACQ */

/* Define if you have _InterlockedIncrement_acq() */
/* #undef ETHR_HAVE__INTERLOCKEDINCREMENT_ACQ */

/* Define if you have _InterlockedOr() */
/* #undef ETHR_HAVE__INTERLOCKEDOR */

/* Define if you have _InterlockedOr64() */
/* #undef ETHR_HAVE__INTERLOCKEDOR64 */

/* Define as a bitmask corresponding to the word sizes that
   __atomic_add_fetch() can handle on your system */
#define ETHR_HAVE___atomic_add_fetch 12

/* Define as a bitmask corresponding to the word sizes that
   __atomic_compare_exchange_n() can handle on your system */
#define ETHR_HAVE___atomic_compare_exchange_n 12

/* Define as a bitmask corresponding to the word sizes that
   __atomic_fetch_and() can handle on your system */
#define ETHR_HAVE___atomic_fetch_and 12

/* Define as a bitmask corresponding to the word sizes that
   __atomic_fetch_or() can handle on your system */
#define ETHR_HAVE___atomic_fetch_or 12

/* Define as a bitmask corresponding to the word sizes that __atomic_load_n()
   can handle on your system */
#define ETHR_HAVE___atomic_load_n 12

/* Define as a bitmask corresponding to the word sizes that __atomic_store_n()
   can handle on your system */
#define ETHR_HAVE___atomic_store_n 12

/* Define as a bitmask corresponding to the word sizes that
   __sync_add_and_fetch() can handle on your system */
#define ETHR_HAVE___sync_add_and_fetch 12

/* Define as a bitmask corresponding to the word sizes that
   __sync_fetch_and_and() can handle on your system */
#define ETHR_HAVE___sync_fetch_and_and 12

/* Define as a bitmask corresponding to the word sizes that
   __sync_fetch_and_or() can handle on your system */
#define ETHR_HAVE___sync_fetch_and_or 12

/* Define as a bitmask corresponding to the word sizes that
   __sync_synchronize() can handle on your system */
#define ETHR_HAVE___sync_synchronize ~0

/* Define as a bitmask corresponding to the word sizes that
   __sync_val_compare_and_swap() can handle on your system */
#define ETHR_HAVE___sync_val_compare_and_swap 12

/* Define if you want to modify the default stack size */
/* #undef ETHR_MODIFIED_DEFAULT_STACK_SIZE */

/* Define to the monotonic clock id to use */
#define ETHR_MONOTONIC_CLOCK_ID CLOCK_MONOTONIC

/* Define if you need the <nptl/pthread.h> header file. */
/* #undef ETHR_NEED_NPTL_PTHREAD_H */

/* Define if you prefer gcc native ethread implementations */
/* #undef ETHR_PREFER_GCC_NATIVE_IMPLS */

/* Define if you prefer libatomic_ops native ethread implementations */
/* #undef ETHR_PREFER_LIBATOMIC_OPS_NATIVE_IMPLS */

/* Define if you have pthreads */
#define ETHR_PTHREADS 1

/* Define if pthread_yield() returns an int. */
#define ETHR_PTHREAD_YIELD_RET_INT 1

/* Define if sched_yield() returns an int. */
#define ETHR_SCHED_YIELD_RET_INT 1

/* Define to the size of AO_t if libatomic_ops is used */
/* #undef ETHR_SIZEOF_AO_T */

/* Define to the size of int */
#define ETHR_SIZEOF_INT 4

/* Define to the size of long */
#define ETHR_SIZEOF_LONG 8

/* Define to the size of long long */
#define ETHR_SIZEOF_LONG_LONG 8

/* Define to the size of pointers */
#define ETHR_SIZEOF_PTR 8

/* Define to the size of __int128_t */
#define ETHR_SIZEOF___INT128_T 16

/* Define to the size of __int64 */
#define ETHR_SIZEOF___INT64 0

/* Define if you want to enable check for native ethread implementations */
#define ETHR_SMP_REQUIRE_NATIVE_IMPLS 1

/* Define if only run in Sparc PSO, or TSO mode */
/* #undef ETHR_SPARC_PSO */

/* Define if run in Sparc RMO, PSO, or TSO mode */
/* #undef ETHR_SPARC_RMO */

/* Define if only run in Sparc TSO mode */
/* #undef ETHR_SPARC_TSO */

/* Define if you can safely include both <sys/time.h> and <time.h>. */
#define ETHR_TIME_WITH_SYS_TIME 1

/* Define as a boolean indicating whether you trust gcc's __atomic_* builtins
   memory barrier implementations, or not */
#define ETHR_TRUST_GCC_ATOMIC_BUILTINS_MEMORY_BARRIERS 0

/* Define if you have win32 threads */
/* #undef ETHR_WIN32_THREADS */

/* Define if x86/x86_64 out of order instructions should be synchronized */
/* #undef ETHR_X86_OUT_OF_ORDER */

/* Define to 1 if you have the <arpa/nameser.h> header file. */
#define HAVE_ARPA_NAMESER_H 1

/* Define to 1 if you have the `brk' function. */
#define HAVE_BRK 1

/* define if clock_gettime() works for getting thread time */
#define HAVE_CLOCK_GETTIME_CPU_TIME /**/

/* Define if you have clock_gettime(CLOCK_MONOTONIC_RAW, _) */
#define HAVE_CLOCK_GETTIME_MONOTONIC_RAW 1

/* Define to 1 if you have the `clock_get_attributes' function. */
/* #undef HAVE_CLOCK_GET_ATTRIBUTES */

/* Define to 1 if you have the `closefrom' function. */
/* #undef HAVE_CLOSEFROM */

/* Define if you have a decl of fread that conflicts with int fread */
#define HAVE_CONFLICTING_FREAD_DECLARATION 1

/* Define if you have a putenv() that stores a copy of the key-value pair */
/* #undef HAVE_COPYING_PUTENV */

/* Define if you have cpuset_getaffinity/cpuset_setaffinity */
/* #undef HAVE_CPUSET_xETAFFINITY */

/* Define to 1 if you have the declaration of `getrlimit', and to 0 if you
   don't. */
#define HAVE_DECL_GETRLIMIT 1

/* Define to 1 if you have the declaration of `IN6ADDR_ANY_INIT', and to 0 if
   you don't. */
#define HAVE_DECL_IN6ADDR_ANY_INIT 1

/* Define to 1 if you have the declaration of `IN6ADDR_LOOPBACK_INIT', and to
   0 if you don't. */
#define HAVE_DECL_IN6ADDR_LOOPBACK_INIT 1

/* Define to 1 if you have the declaration of `IPV6_V6ONLY', and to 0 if you
   don't. */
#define HAVE_DECL_IPV6_V6ONLY 1

/* Define to 1 if you have the declaration of `posix2time', and to 0 if you
   don't. */
#define HAVE_DECL_POSIX2TIME 0

/* Define to 1 if you have the declaration of `RLIMIT_STACK', and to 0 if you
   don't. */
#define HAVE_DECL_RLIMIT_STACK 1

/* Define to 1 if you have the declaration of `SCTPS_BOUND', and to 0 if you
   don't. */
/* #undef HAVE_DECL_SCTPS_BOUND */

/* Define to 1 if you have the declaration of `SCTPS_COOKIE_ECHOED', and to 0
   if you don't. */
/* #undef HAVE_DECL_SCTPS_COOKIE_ECHOED */

/* Define to 1 if you have the declaration of `SCTPS_COOKIE_WAIT', and to 0 if
   you don't. */
/* #undef HAVE_DECL_SCTPS_COOKIE_WAIT */

/* Define to 1 if you have the declaration of `SCTPS_ESTABLISHED', and to 0 if
   you don't. */
/* #undef HAVE_DECL_SCTPS_ESTABLISHED */

/* Define to 1 if you have the declaration of `SCTPS_IDLE', and to 0 if you
   don't. */
/* #undef HAVE_DECL_SCTPS_IDLE */

/* Define to 1 if you have the declaration of `SCTPS_LISTEN', and to 0 if you
   don't. */
/* #undef HAVE_DECL_SCTPS_LISTEN */

/* Define to 1 if you have the declaration of `SCTPS_SHUTDOWN_ACK_SENT', and
   to 0 if you don't. */
/* #undef HAVE_DECL_SCTPS_SHUTDOWN_ACK_SENT */

/* Define to 1 if you have the declaration of `SCTPS_SHUTDOWN_PENDING', and to
   0 if you don't. */
/* #undef HAVE_DECL_SCTPS_SHUTDOWN_PENDING */

/* Define to 1 if you have the declaration of `SCTPS_SHUTDOWN_RECEIVED', and
   to 0 if you don't. */
/* #undef HAVE_DECL_SCTPS_SHUTDOWN_RECEIVED */

/* Define to 1 if you have the declaration of `SCTPS_SHUTDOWN_SENT', and to 0
   if you don't. */
/* #undef HAVE_DECL_SCTPS_SHUTDOWN_SENT */

/* Define to 1 if you have the declaration of `SCTP_ABORT', and to 0 if you
   don't. */
/* #undef HAVE_DECL_SCTP_ABORT */

/* Define to 1 if you have the declaration of `SCTP_ADDR_CONFIRMED', and to 0
   if you don't. */
/* #undef HAVE_DECL_SCTP_ADDR_CONFIRMED */

/* Define to 1 if you have the declaration of `SCTP_ADDR_OVER', and to 0 if
   you don't. */
/* #undef HAVE_DECL_SCTP_ADDR_OVER */

/* Define to 1 if you have the declaration of `SCTP_BOUND', and to 0 if you
   don't. */
/* #undef HAVE_DECL_SCTP_BOUND */

/* Define to 1 if you have the declaration of `SCTP_CLOSED', and to 0 if you
   don't. */
/* #undef HAVE_DECL_SCTP_CLOSED */

/* Define to 1 if you have the declaration of `SCTP_COOKIE_ECHOED', and to 0
   if you don't. */
/* #undef HAVE_DECL_SCTP_COOKIE_ECHOED */

/* Define to 1 if you have the declaration of `SCTP_COOKIE_WAIT', and to 0 if
   you don't. */
/* #undef HAVE_DECL_SCTP_COOKIE_WAIT */

/* Define to 1 if you have the declaration of `SCTP_DELAYED_ACK_TIME', and to
   0 if you don't. */
/* #undef HAVE_DECL_SCTP_DELAYED_ACK_TIME */

/* Define to 1 if you have the declaration of `SCTP_EMPTY', and to 0 if you
   don't. */
/* #undef HAVE_DECL_SCTP_EMPTY */

/* Define to 1 if you have the declaration of `SCTP_EOF', and to 0 if you
   don't. */
/* #undef HAVE_DECL_SCTP_EOF */

/* Define to 1 if you have the declaration of `SCTP_ESTABLISHED', and to 0 if
   you don't. */
/* #undef HAVE_DECL_SCTP_ESTABLISHED */

/* Define to 1 if you have the declaration of `SCTP_LISTEN', and to 0 if you
   don't. */
/* #undef HAVE_DECL_SCTP_LISTEN */

/* Define to 1 if you have the declaration of `SCTP_SENDALL', and to 0 if you
   don't. */
/* #undef HAVE_DECL_SCTP_SENDALL */

/* Define to 1 if you have the declaration of `SCTP_SHUTDOWN_ACK_SENT', and to
   0 if you don't. */
/* #undef HAVE_DECL_SCTP_SHUTDOWN_ACK_SENT */

/* Define to 1 if you have the declaration of `SCTP_SHUTDOWN_PENDING', and to
   0 if you don't. */
/* #undef HAVE_DECL_SCTP_SHUTDOWN_PENDING */

/* Define to 1 if you have the declaration of `SCTP_SHUTDOWN_RECEIVED', and to
   0 if you don't. */
/* #undef HAVE_DECL_SCTP_SHUTDOWN_RECEIVED */

/* Define to 1 if you have the declaration of `SCTP_SHUTDOWN_SENT', and to 0
   if you don't. */
/* #undef HAVE_DECL_SCTP_SHUTDOWN_SENT */

/* Define to 1 if you have the declaration of `SCTP_UNCONFIRMED', and to 0 if
   you don't. */
/* #undef HAVE_DECL_SCTP_UNCONFIRMED */

/* Define to 1 if you have the declaration of `SCTP_UNORDERED', and to 0 if
   you don't. */
/* #undef HAVE_DECL_SCTP_UNORDERED */

/* Define to 1 if you have the declaration of `setrlimit', and to 0 if you
   don't. */
#define HAVE_DECL_SETRLIMIT 1

/* Define to 1 if you have the declaration of `time2posix', and to 0 if you
   don't. */
#define HAVE_DECL_TIME2POSIX 0

/* Define to 1 if you have the <dirent.h> header file, and it defines `DIR'.
   */
#define HAVE_DIRENT_H 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the `dlopen' function. */
#define HAVE_DLOPEN 1

/* Define to 1 if you don't have `vprintf' but do have `_doprnt.' */
/* #undef HAVE_DOPRNT */

/* Define if you have the 'end' symbol */
#define HAVE_END_SYMBOL 1

/* Define if you have a working fallocate() */
#define HAVE_FALLOCATE 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the `fdatasync' function. */
#define HAVE_FDATASYNC 1

/* Define to 1 if you have the `finite' function. */
#define HAVE_FINITE 1

/* Define to 1 if you have the `flockfile' function. */
#define HAVE_FLOCKFILE 1

/* Define to 1 if you have the `fork' function. */
#define HAVE_FORK 1

/* Define to 1 if you have the `fpsetmask' function. */
/* #undef HAVE_FPSETMASK */

/* Define to 1 if you have the `fstat' function. */
#define HAVE_FSTAT 1

/* Define if you have fwrite_unlocked */
#define HAVE_FWRITE_UNLOCKED 1

/* Define to 1 if you have a good `getaddrinfo' function. */
#define HAVE_GETADDRINFO 1

/* Define to 1 if you have the `gethostbyname2' function. */
#define HAVE_GETHOSTBYNAME2 1

/* Define to flavour of gethostbyname_r */
#define HAVE_GETHOSTBYNAME_R GHBN_R_GLIBC

/* Define to 1 if you have the `gethrtime' function. */
/* #undef HAVE_GETHRTIME */

/* define if gethrvtime() works and uses ioctl() to /proc/self */
/* #undef HAVE_GETHRVTIME_PROCFS_IOCTL */

/* Define to 1 if you have the `getifaddrs' function. */
#define HAVE_GETIFADDRS 1

/* Define to 1 if you have the `getipnodebyaddr' function. */
/* #undef HAVE_GETIPNODEBYADDR */

/* Define to 1 if you have the `getipnodebyname' function. */
/* #undef HAVE_GETIPNODEBYNAME */

/* Define to 1 if you have a good `getnameinfo' function. */
#define HAVE_GETNAMEINFO 1

/* Define to 1 if you have the `getrusage' function. */
#define HAVE_GETRUSAGE 1

/* Define to 1 if you have the `gettimeofday' function. */
#define HAVE_GETTIMEOFDAY 1

/* Define to 1 if you have the `gmtime_r' function. */
#define HAVE_GMTIME_R 1

/* Define to 1 if you have the <ieeefp.h> header file. */
/* #undef HAVE_IEEEFP_H */

/* Define to 1 if you have the `ieee_handler' function. */
/* #undef HAVE_IEEE_HANDLER */

/* Define to 1 if you have the <ifaddrs.h> header file. */
#define HAVE_IFADDRS_H 1

/* Define if ipv6 is present */
#define HAVE_IN6 1

/* Define to 1 if you have the variable in6addr_any declared. */
#define HAVE_IN6ADDR_ANY 1

/* Define to 1 if you have the variable in6addr_loopback declared. */
#define HAVE_IN6ADDR_LOOPBACK 1

/* Define to 1 if you have the `inet_pton' function. */
#define HAVE_INET_PTON 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Early linux used in_addr6 instead of in6_addr, define if you have this */
/* #undef HAVE_IN_ADDR6_STRUCT */

/* Define to 1 if you have the `isfinite' function. */
/* #undef HAVE_ISFINITE */

/* Define to 1 if you have the `isinf' function. */
#define HAVE_ISINF 1

/* Define to 1 if you have the `isnan' function. */
#define HAVE_ISNAN 1

/* Define if you have kstat */
/* #undef HAVE_KSTAT */

/* Define to 1 if you have the <langinfo.h> header file. */
#define HAVE_LANGINFO_H 1

/* Define to 1 if you have the `dl' library (-ldl). */
#define HAVE_LIBDL 1

/* Define to 1 if you have the `dlpi' library (-ldlpi). */
/* #undef HAVE_LIBDLPI */

/* Define to 1 if you have the <libdlpi.h> header file. */
/* #undef HAVE_LIBDLPI_H */

/* Define to 1 if you have the `inet' library (-linet). */
/* #undef HAVE_LIBINET */

/* Define to 1 if you have the `m' library (-lm). */
#define HAVE_LIBM 1

/* Define to 1 if you have the `sctp' library (-lsctp). */
/* #undef HAVE_LIBSCTP */

/* Define to 1 if you have the `util' library (-lutil). */
#define HAVE_LIBUTIL 1

/* Define to 1 if you have the <libutil.h> header file. */
/* #undef HAVE_LIBUTIL_H */

/* Define to 1 if you have the `z' library (-lz). */
#define HAVE_LIBZ 1

/* Define to 1 if you have the <limits.h> header file. */
#define HAVE_LIMITS_H 1

/* Define to 1 if you have the <linux/falloc.h> header file. */
#define HAVE_LINUX_FALLOC_H 1

/* Define to 1 if you have the `localtime_r' function. */
#define HAVE_LOCALTIME_R 1

/* Define to 1 if you have the `log2' function. */
#define HAVE_LOG2 1

/* Define to 1 if you have the <lttng/tracepoint-event.h> header file. */
/* #undef HAVE_LTTNG_TRACEPOINT_EVENT_H */

/* Define to 1 if you have the <lttng/tracepoint.h> header file. */
/* #undef HAVE_LTTNG_TRACEPOINT_H */

/* Define to 1 if you have the <malloc.h> header file. */
#define HAVE_MALLOC_H 1

/* Define to 1 if you have the `mallopt' function. */
#define HAVE_MALLOPT 1

/* Define to 1 if you have the `memcpy' function. */
#define HAVE_MEMCPY 1

/* Define to 1 if you have the `memmove' function. */
#define HAVE_MEMMOVE 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define if the pthread.h header file is in pthread/mit directory. */
/* #undef HAVE_MIT_PTHREAD_H */

/* Define to 1 if you have the `mlockall' function. */
#define HAVE_MLOCKALL 1

/* Define to 1 if you have the `mmap' function. */
#define HAVE_MMAP 1

/* Define if you have a monotonic erts_os_hrtime() implementation */
/* #undef HAVE_MONOTONIC_ERTS_SYS_HRTIME */

/* Define to 1 if you have the `mprotect' function. */
#define HAVE_MPROTECT 1

/* Define to 1 if you have the `mremap' function. */
#define HAVE_MREMAP 1

/* Define if setsockopt() accepts multicast options */
#define HAVE_MULTICAST_SUPPORT 1

/* Define to 1 if you have the <ndir.h> header file, and it defines `DIR'. */
/* #undef HAVE_NDIR_H */

/* Define to 1 if you have the <netpacket/packet.h> header file. */
#define HAVE_NETPACKET_PACKET_H 1

/* Define to 1 if you have the <net/errno.h> header file. */
/* #undef HAVE_NET_ERRNO_H */

/* Define to 1 if you have the <net/if_dl.h> header file. */
/* #undef HAVE_NET_IF_DL_H */

/* Define to 1 if you have the `nl_langinfo' function. */
#define HAVE_NL_LANGINFO 1

/* Define if you don't have a definition of INADDR_LOOPBACK */
/* #undef HAVE_NO_INADDR_LOOPBACK */

/* Define to 1 if you have the `openpty' function. */
#define HAVE_OPENPTY 1

/* Define to 1 if you have the `poll' function. */
#define HAVE_POLL 1

/* Define to 1 if you have the <poll.h> header file. */
#define HAVE_POLL_H 1

/* Define to 1 if you have the `posix2time' function. */
/* #undef HAVE_POSIX2TIME */

/* Define to 1 if you have the `posix_fadvise' function. */
#define HAVE_POSIX_FADVISE 1

/* Define if you have a working posix_fallocate() */
#define HAVE_POSIX_FALLOCATE /**/

/* Define to 1 if you have the `posix_memalign' function. */
#define HAVE_POSIX_MEMALIGN 1

/* Define to 1 if you have the `ppoll' function. */
#define HAVE_PPOLL 1

/* Define to 1 if you have the `pread' function. */
#define HAVE_PREAD 1

/* Define if you have processor_bind functionality */
/* #undef HAVE_PROCESSOR_BIND */

/* Define if you have pset functionality */
/* #undef HAVE_PSET */

/* Define if you have the <pthread.h> header file. */
#define HAVE_PTHREAD_H 1

/* Define to 1 if you have the <pty.h> header file. */
#define HAVE_PTY_H 1

/* Define if you have putc_unlocked */
#define HAVE_PUTC_UNLOCKED 1

/* Define to 1 if you have the `pwrite' function. */
#define HAVE_PWRITE 1

/* Define to 1 if you have the `res_gethostbyname' function. */
/* #undef HAVE_RES_GETHOSTBYNAME */

/* Define to 1 if you have the `sbrk' function. */
#define HAVE_SBRK 1

/* Define to 1 if you have the <sched.h> header file. */
#define HAVE_SCHED_H 1

/* Define if you have sched_getaffinity/sched_setaffinity */
#define HAVE_SCHED_xETAFFINITY 1

/* Define to 1 if you have the `sctp_bindx' function. */
/* #undef HAVE_SCTP_BINDX */

/* Define to 1 if you have the `sctp_freeladdrs' function. */
/* #undef HAVE_SCTP_FREELADDRS */

/* Define to 1 if you have the `sctp_freepaddrs' function. */
/* #undef HAVE_SCTP_FREEPADDRS */

/* Define to 1 if you have the `sctp_getladdrs' function. */
/* #undef HAVE_SCTP_GETLADDRS */

/* Define to 1 if you have the `sctp_getpaddrs' function. */
/* #undef HAVE_SCTP_GETPADDRS */

/* Define to 1 if you have the <netinet/sctp.h> header file */
/* #undef HAVE_SCTP_H */

/* Define to 1 if you have the `sctp_peeloff' function. */
/* #undef HAVE_SCTP_PEELOFF */

/* Define to 1 if you have the <sdkddkver.h> header file. */
/* #undef HAVE_SDKDDKVER_H */

/* Define to 1 if you have the `sendfile' function. */
#define HAVE_SENDFILE 1

/* Define to 1 if you have the `sendfilev' function. */
/* #undef HAVE_SENDFILEV */

/* Define to 1 if you have the `setlocale' function. */
#define HAVE_SETLOCALE 1

/* Define to 1 if you have the `setns' function. */
#define HAVE_SETNS 1

/* Define to 1 if you have the <setns.h> header file. */
/* #undef HAVE_SETNS_H */

/* Define to 1 if you have the `setsid' function. */
#define HAVE_SETSID 1

/* Define if we have socklen_t */
#define HAVE_SOCKLEN_T 1

/* define if you have the Solaris/ultrasparc /dev/perfmon interface */
/* #undef HAVE_SOLARIS_SPARC_PERFMON */

/* Define if you have SO_BSDCOMPAT flag on sockets */
#define HAVE_SO_BSDCOMPAT /**/

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strerror' function. */
#define HAVE_STRERROR 1

/* Define to 1 if you have the `strerror_r' function. */
#define HAVE_STRERROR_R 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strlcat' function. */
/* #undef HAVE_STRLCAT */

/* Define to 1 if you have the `strlcpy' function. */
/* #undef HAVE_STRLCPY */

/* Define to 1 if you have the `strncasecmp' function. */
#define HAVE_STRNCASECMP 1

/* Define to 1 if `ifr_enaddr' is a member of `struct ifreq'. */
/* #undef HAVE_STRUCT_IFREQ_IFR_ENADDR */

/* Define to 1 if `ifr_hwaddr' is a member of `struct ifreq'. */
#define HAVE_STRUCT_IFREQ_IFR_HWADDR 1

/* Define to 1 if `spp_flags' is a member of `struct sctp_paddrparams'. */
/* #undef HAVE_STRUCT_SCTP_PADDRPARAMS_SPP_FLAGS */

/* Define to 1 if `spp_pathmtu' is a member of `struct sctp_paddrparams'. */
/* #undef HAVE_STRUCT_SCTP_PADDRPARAMS_SPP_PATHMTU */

/* Define to 1 if `spp_sackdelay' is a member of `struct sctp_paddrparams'. */
/* #undef HAVE_STRUCT_SCTP_PADDRPARAMS_SPP_SACKDELAY */

/* Define to 1 if `sre_data' is a member of `struct sctp_remote_error'. */
/* #undef HAVE_STRUCT_SCTP_REMOTE_ERROR_SRE_DATA */

/* Define to 1 if `ssf_data' is a member of `struct sctp_send_failed'. */
/* #undef HAVE_STRUCT_SCTP_SEND_FAILED_SSF_DATA */

/* Define to 1 if you have the <syslog.h> header file. */
#define HAVE_SYSLOG_H 1

/* Define if you have systemd daemon */
/* #undef HAVE_SYSTEMD_DAEMON */

/* Define to 1 if you have the <systemd/sd-daemon.h> header file. */
/* #undef HAVE_SYSTEMD_SD_DAEMON_H */

/* Define if you have <sys/devpoll.h> header file. */
/* #undef HAVE_SYS_DEVPOLL_H */

/* Define to 1 if you have the <sys/dir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_DIR_H */

/* Define if you have the <sys/epoll.h> header file. */
#define HAVE_SYS_EPOLL_H 1

/* Define if you have <sys/event.h> header file. */
/* #undef HAVE_SYS_EVENT_H */

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#define HAVE_SYS_IOCTL_H 1

/* Define to 1 if you have the <sys/ndir.h> header file, and it defines `DIR'.
   */
/* #undef HAVE_SYS_NDIR_H */

/* Define to 1 if you have the <sys/resource.h> header file */
#define HAVE_SYS_RESOURCE_H 1

/* Define to 1 if you have the <sys/sdt.h> header file. */
/* #undef HAVE_SYS_SDT_H */

/* Define to 1 if you have the <sys/socketio.h> header file. */
/* #undef HAVE_SYS_SOCKETIO_H */

/* Define to 1 if you have the <sys/socket.h> header file. */
#define HAVE_SYS_SOCKET_H 1

/* Define to 1 if you have the <sys/sockio.h> header file. */
/* #undef HAVE_SYS_SOCKIO_H */

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/stropts.h> header file. */
#define HAVE_SYS_STROPTS_H 1

/* Define to 1 if you have the <sys/sysctl.h> header file. */
#define HAVE_SYS_SYSCTL_H 1

/* Define to 1 if you have the <sys/timerfd.h> header file. */
#define HAVE_SYS_TIMERFD_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <sys/uio.h> header file. */
#define HAVE_SYS_UIO_H 1

/* Define to 1 if you have the <sys/un.h> header file. */
#define HAVE_SYS_UN_H 1

/* Define to 1 if you have <sys/wait.h> that is POSIX.1 compatible. */
#define HAVE_SYS_WAIT_H 1

/* Define if termcap functions exists */
#define HAVE_TERMCAP 1

/* Define to 1 if you have the `time2posix' function. */
/* #undef HAVE_TIME2POSIX */

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the <util.h> header file. */
/* #undef HAVE_UTIL_H */

/* Define to 1 if you have the <utmp.h> header file. */
#define HAVE_UTMP_H 1

/* Define to 1 if you have the `vfork' function. */
#define HAVE_VFORK 1

/* Define to 1 if you have the <vfork.h> header file. */
/* #undef HAVE_VFORK_H */

/* Define to 1 if you have the `vprintf' function. */
#define HAVE_VPRINTF 1

/* Define to 1 if you have a `wcwidth' function. */
#define HAVE_WCWIDTH 1

/* Define to 1 if you have the <windows.h> header file. */
/* #undef HAVE_WINDOWS_H */

/* Define to 1 if you have the <winsock2.h> header file. */
/* #undef HAVE_WINSOCK2_H */

/* Define to 1 if `fork' works. */
#define HAVE_WORKING_FORK 1

/* Define if you have a working posix_openpt implementation */
#define HAVE_WORKING_POSIX_OPENPT 1

/* Define to 1 if `vfork' works. */
#define HAVE_WORKING_VFORK 1

/* Define to 1 if you have the `writev' function. */
#define HAVE_WRITEV 1

/* Define to 1 if you have the <ws2tcpip.h> header file. */
/* #undef HAVE_WS2TCPIP_H */

/* Define if your zlib version defines inflateGetDictionary. */
#define HAVE_ZLIB_INFLATEGETDICTIONARY 1

/* Define to 1 if you have the `_brk' function. */
/* #undef HAVE__BRK */

/* Define if you have the '_end' symbol */
#define HAVE__END_SYMBOL 1

/* Define to 1 if you have the `_sbrk' function. */
/* #undef HAVE__SBRK */

/* Define to 1 if you have the `__brk' function. */
/* #undef HAVE___BRK */

/* Define to 1 if you have the `__sbrk' function. */
#define HAVE___SBRK 1

/* Define to enable HiPE */
/* #undef HIPE */

/* Define to monotonic clock id to use */
#define HRTIME_CLOCK_ID CLOCK_MONOTONIC

/* Define as a string of monotonic clock id to use */
#define HRTIME_CLOCK_ID_STR "CLOCK_MONOTONIC"

/* define if h_errno is declared (in some way) in a system header file */
#define H_ERRNO_DECLARED 1

/* Define to monotonic clock id to use */
#define MONOTONIC_CLOCK_ID CLOCK_MONOTONIC

/* Define as a string of monotonic clock id to use */
#define MONOTONIC_CLOCK_ID_STR "CLOCK_MONOTONIC"

/* Define if netdb.h needs struct sockaddr_in ans in.h CAN be included before
   */
#define NETDB_H_NEEDS_IN_H 1

/* Define if floating points exceptions are non-existing/not reliable */
#define NO_FPE_SIGNALS /**/

/* Defined if no found C compiler can handle jump tables */
/* #undef NO_JUMP_TABLE */

/* Define if you dont have salen */
#define NO_SA_LEN 1

/* Define if you want to implement erts_os_monotonic_time() using
   clock_gettime() */
#define OS_MONOTONIC_TIME_USING_CLOCK_GETTIME 1

/* Define if you want to implement erts_os_monotonic_time() using gethrtime()
   */
/* #undef OS_MONOTONIC_TIME_USING_GETHRTIME */

/* Define if you want to implement erts_os_monotonic_time() using mach
   clock_get_time() */
/* #undef OS_MONOTONIC_TIME_USING_MACH_CLOCK_GET_TIME */

/* Define if you want to implement erts_os_monotonic_time() using times() */
/* #undef OS_MONOTONIC_TIME_USING_TIMES */

/* Define if you want to implement erts_os_system_time() using gettimeofday()
   */
/* #undef OS_SYSTEM_TIME_GETTIMEOFDAY */

/* Define if you want to implement erts_os_system_time() using clock_gettime()
   */
#define OS_SYSTEM_TIME_USING_CLOCK_GETTIME 1

/* Define if you want to implement erts_os_system_time() using mach
   clock_get_time() */
/* #undef OS_SYSTEM_TIME_USING_MACH_CLOCK_GET_TIME */

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT ""

/* Define to the full name of this package. */
#define PACKAGE_NAME ""

/* Define to the full name and version of this package. */
#define PACKAGE_STRING ""

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME ""

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION ""

/* Define if you wish to redefine FD_SETSIZE to be able to select on more fd
   */
/* #undef REDEFINE_FD_SETSIZE */

/* Define as the return type of signal handlers (`int' or `void'). */
#define RETSIGTYPE void

/* Define the sbrk() argument type. */
#define SBRK_ARG_TYPE intptr_t

/* Define the sbrk() return type. */
#define SBRK_RET_TYPE void *

/* Define if building a sharing-preserving emulator */
/* #undef SHCOPY */

/* The size of `AO_t', as computed by sizeof. */
/* #undef SIZEOF_AO_T */

/* The size of `char', as computed by sizeof. */
#define SIZEOF_CHAR 1

/* The size of `int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 8

/* The size of `long long', as computed by sizeof. */
#define SIZEOF_LONG_LONG 8

/* The size of `off_t', as computed by sizeof. */
#define SIZEOF_OFF_T 8

/* The size of `short', as computed by sizeof. */
#define SIZEOF_SHORT 2

/* The size of `size_t', as computed by sizeof. */
#define SIZEOF_SIZE_T 8

/* The size of `time_t', as computed by sizeof. */
#define SIZEOF_TIME_T 8

/* The size of `void *', as computed by sizeof. */
#define SIZEOF_VOID_P 8

/* The size of `__int128_t', as computed by sizeof. */
#define SIZEOF___INT128_T 16

/* The size of `__int64', as computed by sizeof. */
#define SIZEOF___INT64 0

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* define if the variable sys_errlist is declared in a system header file */
#define SYS_ERRLIST_DECLARED /**/

/* Define if you want to implement erts_os_hrtime() using clock_gettime() */
/* #undef SYS_HRTIME_USING_CLOCK_GETTIME */

/* Define if you want to implement erts_os_hrtime() using gethrtime() */
/* #undef SYS_HRTIME_USING_GETHRTIME */

/* Define if you want to implement erts_os_hrtime() using mach
   clock_get_time() */
/* #undef SYS_HRTIME_USING_MACH_CLOCK_GET_TIME */

/* Define to 1 if you can safely include both <sys/time.h> and <time.h>. */
#define TIME_WITH_SYS_TIME 1

/* Define to 1 if your <sys/time.h> declares `struct tm'. */
/* #undef TM_IN_SYS_TIME */

/* Define if you want to use dtrace for dynamic tracing */
/* #undef USE_DTRACE */

/* Define if you want to use dynamic tracing */
/* #undef USE_DYNAMIC_TRACE */

/* Define if you want to use lttng for dynamic tracing */
/* #undef USE_LTTNG */

/* Define if you have matherr() function and struct exception type */
/* #undef USE_MATHERR */

/* Define if select() should be used instead of poll() */
/* #undef USE_SELECT */

/* Define if you want to use systemtap for dynamic tracing */
/* #undef USE_SYSTEMTAP */

/* Define to enable VM dynamic trace probes */
/* #undef USE_VM_PROBES */

/* Define to wall clock id to use */
#define WALL_CLOCK_ID CLOCK_REALTIME

/* Define as a string of wall clock id to use */
#define WALL_CLOCK_ID_STR "CLOCK_REALTIME"

/* Define if windows.h includes winsock2.h */
/* #undef WINDOWS_H_INCLUDES_WINSOCK2_H */

/* Define if big-endian */
/* #undef WORDS_BIGENDIAN */

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `long int' if <sys/types.h> does not define. */
/* #undef off_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef pid_t */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */

/* Define as `fork' if `vfork' does not work. */
/* #undef vfork */


/* Redefine in6_addr. XXX this should be moved to the files where it's used? */
#ifdef HAVE_IN_ADDR6_STRUCT
#define in6_addr in_addr6
#endif

/* Define a reasonable default for INADDR_LOOPBACK */
/* XXX this should be moved to the files where it's used? */
#ifdef HAVE_NO_INADDR_LOOPBACK
#define INADDR_LOOPBACK (u_long)0x7F000001
#endif

#ifdef REDEFINE_FD_SETSIZE
#define FD_SETSIZE 1024
#endif
 
#ifdef HAVE_GETHRVTIME_PROCFS_IOCTL
#define HAVE_GETHRVTIME
#endif

#if !defined(HAVE_ISFINITE) && !defined(HAVE_FINITE)
# if defined(HAVE_ISINF) && defined(HAVE_ISNAN)
#  define USE_ISINF_ISNAN
# endif
#endif

#if defined(DEBUG) && !defined(ERTS_ENABLE_LOCK_CHECK)
#define ERTS_ENABLE_LOCK_CHECK 1
#endif

