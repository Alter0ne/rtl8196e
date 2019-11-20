#define PERL_constant_NOTFOUND	1
#define PERL_constant_NOTDEF	2
#define PERL_constant_ISIV	3
#define PERL_constant_ISNO	4
#define PERL_constant_ISNV	5
#define PERL_constant_ISPV	6
#define PERL_constant_ISPVN	7
#define PERL_constant_ISSV	8
#define PERL_constant_ISUNDEF	9
#define PERL_constant_ISUV	10
#define PERL_constant_ISYES	11

#ifndef NVTYPE
typedef double NV; /* 5.6 and later define NVTYPE, and typedef NV to it.  */
#endif
#ifndef aTHX_
#define aTHX_ /* 5.6 or later define this for threading support.  */
#endif
#ifndef pTHX_
#define pTHX_ /* 5.6 or later define this for threading support.  */
#endif

/* This allows slightly more efficient code on !USE_ITHREADS: */
#ifdef USE_ITHREADS
#  define COP_FILE(c)	CopFILE(c)
#  define COP_FILE_F	"s"
#else
#  define COP_FILE(c)	CopFILESV(c)
#  define COP_FILE_F	SVf
#endif

static void
constant_add_symbol(pTHX_  HV *hash, const char *name, I32 namelen, SV *value) {
    HE *he = (HE*) hv_common_key_len(hash, name, namelen, HV_FETCH_LVALUE, NULL,
				     0);
    SV *sv;

    if (!he) {
        Perl_croak(aTHX_  "Couldn't add key '%s' to %%POSIX::",
		   name);
    }
    sv = HeVAL(he);
    if (SvOK(sv) || SvTYPE(sv) == SVt_PVGV) {
	/* Someone has been here before us - have to make a real sub.  */
	newCONSTSUB(hash, name, value);
    } else {
	SvUPGRADE(sv, SVt_RV);
	SvRV_set(sv, value);
	SvROK_on(sv);
	SvREADONLY_on(value);
    }
}



#ifndef SYMBIAN

/* Store a hash of all symbols missing from the package. To avoid trampling on
   the package namespace (uninvited) put each package's hash in our namespace.
   To avoid creating lots of typeblogs and symbol tables for sub-packages, put
   each package's hash into one hash in our namespace.  */

static HV *
get_missing_hash(pTHX) {
    HV *const parent
	= get_hv("ExtUtils::Constant::ProxySubs::Missing", GVf_MULTI);
    /* We could make a hash of hashes directly, but this would confuse anything
	at Perl space that looks at us, and as we're visible in Perl space,
	best to play nice. */
    SV *const *const ref
	= hv_fetch(parent, "POSIX", 5, TRUE);
    HV *new_hv;

    if (!ref)
	return NULL;

    if (SvROK(*ref))
	return (HV*) SvRV(*ref);

    new_hv = newHV();
    SvUPGRADE(*ref, SVt_RV);
    SvRV_set(*ref, (SV *)new_hv);
    SvROK_on(*ref);
    return new_hv;
}

#endif

struct notfound_s {const char *name; I32 namelen;} ;

    static const struct notfound_s values_for_notfound[] =
      {
#ifndef ARG_MAX
        { "ARG_MAX", 7 },
#endif
#ifndef B0
        { "B0", 2 },
#endif
#ifndef B110
        { "B110", 4 },
#endif
#ifndef B1200
        { "B1200", 5 },
#endif
#ifndef B134
        { "B134", 4 },
#endif
#ifndef B150
        { "B150", 4 },
#endif
#ifndef B1800
        { "B1800", 5 },
#endif
#ifndef B19200
        { "B19200", 6 },
#endif
#ifndef B200
        { "B200", 4 },
#endif
#ifndef B2400
        { "B2400", 5 },
#endif
#ifndef B300
        { "B300", 4 },
#endif
#ifndef B38400
        { "B38400", 6 },
#endif
#ifndef B4800
        { "B4800", 5 },
#endif
#ifndef B50
        { "B50", 3 },
#endif
#ifndef B600
        { "B600", 4 },
#endif
#ifndef B75
        { "B75", 3 },
#endif
#ifndef B9600
        { "B9600", 5 },
#endif
#ifndef BRKINT
        { "BRKINT", 6 },
#endif
#ifndef BUFSIZ
        { "BUFSIZ", 6 },
#endif
#ifndef CHAR_BIT
        { "CHAR_BIT", 8 },
#endif
#ifndef CHAR_MAX
        { "CHAR_MAX", 8 },
#endif
#ifndef CHAR_MIN
        { "CHAR_MIN", 8 },
#endif
#ifndef CHILD_MAX
        { "CHILD_MAX", 9 },
#endif
#ifndef CLOCAL
        { "CLOCAL", 6 },
#endif
#ifndef CLOCKS_PER_SEC
        { "CLOCKS_PER_SEC", 14 },
#endif
#ifndef CREAD
        { "CREAD", 5 },
#endif
#ifndef CS5
        { "CS5", 3 },
#endif
#ifndef CS6
        { "CS6", 3 },
#endif
#ifndef CS7
        { "CS7", 3 },
#endif
#ifndef CS8
        { "CS8", 3 },
#endif
#ifndef CSIZE
        { "CSIZE", 5 },
#endif
#ifndef CSTOPB
        { "CSTOPB", 6 },
#endif
#ifndef E2BIG
        { "E2BIG", 5 },
#endif
#ifndef EACCES
        { "EACCES", 6 },
#endif
#ifndef EADDRINUSE
        { "EADDRINUSE", 10 },
#endif
#ifndef EADDRNOTAVAIL
        { "EADDRNOTAVAIL", 13 },
#endif
#ifndef EAFNOSUPPORT
        { "EAFNOSUPPORT", 12 },
#endif
#ifndef EAGAIN
        { "EAGAIN", 6 },
#endif
#ifndef EALREADY
        { "EALREADY", 8 },
#endif
#ifndef EBADF
        { "EBADF", 5 },
#endif
#ifndef EBADMSG
        { "EBADMSG", 7 },
#endif
#ifndef EBUSY
        { "EBUSY", 5 },
#endif
#ifndef ECANCELED
        { "ECANCELED", 9 },
#endif
#ifndef ECHILD
        { "ECHILD", 6 },
#endif
#ifndef ECHO
        { "ECHO", 4 },
#endif
#ifndef ECHOE
        { "ECHOE", 5 },
#endif
#ifndef ECHOK
        { "ECHOK", 5 },
#endif
#ifndef ECHONL
        { "ECHONL", 6 },
#endif
#ifndef ECONNABORTED
        { "ECONNABORTED", 12 },
#endif
#ifndef ECONNREFUSED
        { "ECONNREFUSED", 12 },
#endif
#ifndef ECONNRESET
        { "ECONNRESET", 10 },
#endif
#ifndef EDEADLK
        { "EDEADLK", 7 },
#endif
#ifndef EDESTADDRREQ
        { "EDESTADDRREQ", 12 },
#endif
#ifndef EDOM
        { "EDOM", 4 },
#endif
#ifndef EDQUOT
        { "EDQUOT", 6 },
#endif
#ifndef EEXIST
        { "EEXIST", 6 },
#endif
#ifndef EFAULT
        { "EFAULT", 6 },
#endif
#ifndef EFBIG
        { "EFBIG", 5 },
#endif
#ifndef EHOSTDOWN
        { "EHOSTDOWN", 9 },
#endif
#ifndef EHOSTUNREACH
        { "EHOSTUNREACH", 12 },
#endif
#ifndef EIDRM
        { "EIDRM", 5 },
#endif
#ifndef EILSEQ
        { "EILSEQ", 6 },
#endif
#ifndef EINPROGRESS
        { "EINPROGRESS", 11 },
#endif
#ifndef EINTR
        { "EINTR", 5 },
#endif
#ifndef EINVAL
        { "EINVAL", 6 },
#endif
#ifndef EIO
        { "EIO", 3 },
#endif
#ifndef EISCONN
        { "EISCONN", 7 },
#endif
#ifndef EISDIR
        { "EISDIR", 6 },
#endif
#ifndef ELOOP
        { "ELOOP", 5 },
#endif
#ifndef EMFILE
        { "EMFILE", 6 },
#endif
#ifndef EMLINK
        { "EMLINK", 6 },
#endif
#ifndef EMSGSIZE
        { "EMSGSIZE", 8 },
#endif
#ifndef ENAMETOOLONG
        { "ENAMETOOLONG", 12 },
#endif
#ifndef ENETDOWN
        { "ENETDOWN", 8 },
#endif
#ifndef ENETRESET
        { "ENETRESET", 9 },
#endif
#ifndef ENETUNREACH
        { "ENETUNREACH", 11 },
#endif
#ifndef ENFILE
        { "ENFILE", 6 },
#endif
#ifndef ENOBUFS
        { "ENOBUFS", 7 },
#endif
#ifndef ENODATA
        { "ENODATA", 7 },
#endif
#ifndef ENODEV
        { "ENODEV", 6 },
#endif
#ifndef ENOENT
        { "ENOENT", 6 },
#endif
#ifndef ENOEXEC
        { "ENOEXEC", 7 },
#endif
#ifndef ENOLCK
        { "ENOLCK", 6 },
#endif
#ifndef ENOLINK
        { "ENOLINK", 7 },
#endif
#ifndef ENOMEM
        { "ENOMEM", 6 },
#endif
#ifndef ENOMSG
        { "ENOMSG", 6 },
#endif
#ifndef ENOPROTOOPT
        { "ENOPROTOOPT", 11 },
#endif
#ifndef ENOSPC
        { "ENOSPC", 6 },
#endif
#ifndef ENOSR
        { "ENOSR", 5 },
#endif
#ifndef ENOSTR
        { "ENOSTR", 6 },
#endif
#ifndef ENOSYS
        { "ENOSYS", 6 },
#endif
#ifndef ENOTBLK
        { "ENOTBLK", 7 },
#endif
#ifndef ENOTCONN
        { "ENOTCONN", 8 },
#endif
#ifndef ENOTDIR
        { "ENOTDIR", 7 },
#endif
#ifndef ENOTEMPTY
        { "ENOTEMPTY", 9 },
#endif
#ifndef ENOTRECOVERABLE
        { "ENOTRECOVERABLE", 15 },
#endif
#ifndef ENOTSOCK
        { "ENOTSOCK", 8 },
#endif
#ifndef ENOTSUP
        { "ENOTSUP", 7 },
#endif
#ifndef ENOTTY
        { "ENOTTY", 6 },
#endif
#ifndef ENXIO
        { "ENXIO", 5 },
#endif
#ifndef EOF
        { "EOF", 3 },
#endif
#ifndef EOPNOTSUPP
        { "EOPNOTSUPP", 10 },
#endif
#ifndef EOTHER
        { "EOTHER", 6 },
#endif
#ifndef EOVERFLOW
        { "EOVERFLOW", 9 },
#endif
#ifndef EOWNERDEAD
        { "EOWNERDEAD", 10 },
#endif
#ifndef EPERM
        { "EPERM", 5 },
#endif
#ifndef EPFNOSUPPORT
        { "EPFNOSUPPORT", 12 },
#endif
#ifndef EPIPE
        { "EPIPE", 5 },
#endif
#ifndef EPROCLIM
        { "EPROCLIM", 8 },
#endif
#ifndef EPROTO
        { "EPROTO", 6 },
#endif
#ifndef EPROTONOSUPPORT
        { "EPROTONOSUPPORT", 15 },
#endif
#ifndef EPROTOTYPE
        { "EPROTOTYPE", 10 },
#endif
#ifndef ERANGE
        { "ERANGE", 6 },
#endif
#ifndef EREMOTE
        { "EREMOTE", 7 },
#endif
#ifndef ERESTART
        { "ERESTART", 8 },
#endif
#ifndef EROFS
        { "EROFS", 5 },
#endif
#ifndef ESHUTDOWN
        { "ESHUTDOWN", 9 },
#endif
#ifndef ESOCKTNOSUPPORT
        { "ESOCKTNOSUPPORT", 15 },
#endif
#ifndef ESPIPE
        { "ESPIPE", 6 },
#endif
#ifndef ESRCH
        { "ESRCH", 5 },
#endif
#ifndef ESTALE
        { "ESTALE", 6 },
#endif
#ifndef ETIME
        { "ETIME", 5 },
#endif
#ifndef ETIMEDOUT
        { "ETIMEDOUT", 9 },
#endif
#ifndef ETOOMANYREFS
        { "ETOOMANYREFS", 12 },
#endif
#ifndef ETXTBSY
        { "ETXTBSY", 7 },
#endif
#ifndef EUSERS
        { "EUSERS", 6 },
#endif
#ifndef EWOULDBLOCK
        { "EWOULDBLOCK", 11 },
#endif
#ifndef EXDEV
        { "EXDEV", 5 },
#endif
#ifndef FILENAME_MAX
        { "FILENAME_MAX", 12 },
#endif
#ifndef F_OK
        { "F_OK", 4 },
#endif
#ifndef HUPCL
        { "HUPCL", 5 },
#endif
#ifndef ICANON
        { "ICANON", 6 },
#endif
#ifndef ICRNL
        { "ICRNL", 5 },
#endif
#ifndef IEXTEN
        { "IEXTEN", 6 },
#endif
#ifndef IGNBRK
        { "IGNBRK", 6 },
#endif
#ifndef IGNCR
        { "IGNCR", 5 },
#endif
#ifndef IGNPAR
        { "IGNPAR", 6 },
#endif
#ifndef INLCR
        { "INLCR", 5 },
#endif
#ifndef INPCK
        { "INPCK", 5 },
#endif
#ifndef INT_MAX
        { "INT_MAX", 7 },
#endif
#ifndef INT_MIN
        { "INT_MIN", 7 },
#endif
#ifndef ISIG
        { "ISIG", 4 },
#endif
#ifndef ISTRIP
        { "ISTRIP", 6 },
#endif
#ifndef IXOFF
        { "IXOFF", 5 },
#endif
#ifndef IXON
        { "IXON", 4 },
#endif
#ifndef LC_ALL
        { "LC_ALL", 6 },
#endif
#ifndef LC_COLLATE
        { "LC_COLLATE", 10 },
#endif
#ifndef LC_CTYPE
        { "LC_CTYPE", 8 },
#endif
#ifndef LC_MESSAGES
        { "LC_MESSAGES", 11 },
#endif
#ifndef LC_MONETARY
        { "LC_MONETARY", 11 },
#endif
#ifndef LC_NUMERIC
        { "LC_NUMERIC", 10 },
#endif
#ifndef LC_TIME
        { "LC_TIME", 7 },
#endif
#ifndef LINK_MAX
        { "LINK_MAX", 8 },
#endif
#ifndef LONG_MAX
        { "LONG_MAX", 8 },
#endif
#ifndef LONG_MIN
        { "LONG_MIN", 8 },
#endif
#ifndef L_ctermid
        { "L_ctermid", 9 },
#endif
#ifndef L_cuserid
        { "L_cuserid", 9 },
#endif
#ifndef L_tmpnam
        { "L_tmpnam", 8 },
#endif
#ifndef MAX_CANON
        { "MAX_CANON", 9 },
#endif
#ifndef MAX_INPUT
        { "MAX_INPUT", 9 },
#endif
#ifndef MB_LEN_MAX
        { "MB_LEN_MAX", 10 },
#endif
#ifndef MSG_CTRUNC
        { "MSG_CTRUNC", 10 },
#endif
#ifndef MSG_DONTROUTE
        { "MSG_DONTROUTE", 13 },
#endif
#ifndef MSG_EOR
        { "MSG_EOR", 7 },
#endif
#ifndef MSG_OOB
        { "MSG_OOB", 7 },
#endif
#ifndef MSG_PEEK
        { "MSG_PEEK", 8 },
#endif
#ifndef MSG_TRUNC
        { "MSG_TRUNC", 9 },
#endif
#ifndef MSG_WAITALL
        { "MSG_WAITALL", 11 },
#endif
#ifndef NAME_MAX
        { "NAME_MAX", 8 },
#endif
#ifndef NCCS
        { "NCCS", 4 },
#endif
#ifndef NGROUPS_MAX
        { "NGROUPS_MAX", 11 },
#endif
#ifndef NOFLSH
        { "NOFLSH", 6 },
#endif
#ifndef OPEN_MAX
        { "OPEN_MAX", 8 },
#endif
#ifndef OPOST
        { "OPOST", 5 },
#endif
#ifndef PARENB
        { "PARENB", 6 },
#endif
#ifndef PARMRK
        { "PARMRK", 6 },
#endif
#ifndef PARODD
        { "PARODD", 6 },
#endif
#ifndef PATH_MAX
        { "PATH_MAX", 8 },
#endif
#ifndef PIPE_BUF
        { "PIPE_BUF", 8 },
#endif
#ifndef RAND_MAX
        { "RAND_MAX", 8 },
#endif
#ifndef R_OK
        { "R_OK", 4 },
#endif
#ifndef SCHAR_MAX
        { "SCHAR_MAX", 9 },
#endif
#ifndef SCHAR_MIN
        { "SCHAR_MIN", 9 },
#endif
#ifndef SHRT_MAX
        { "SHRT_MAX", 8 },
#endif
#ifndef SHRT_MIN
        { "SHRT_MIN", 8 },
#endif
#ifndef SIGABRT
        { "SIGABRT", 7 },
#endif
#ifndef SIGALRM
        { "SIGALRM", 7 },
#endif
#ifndef SIGCHLD
        { "SIGCHLD", 7 },
#endif
#ifndef SIGCONT
        { "SIGCONT", 7 },
#endif
#ifndef SIGFPE
        { "SIGFPE", 6 },
#endif
#ifndef SIGHUP
        { "SIGHUP", 6 },
#endif
#ifndef SIGILL
        { "SIGILL", 6 },
#endif
#ifndef SIGINT
        { "SIGINT", 6 },
#endif
#ifndef SIGKILL
        { "SIGKILL", 7 },
#endif
#ifndef SIGPIPE
        { "SIGPIPE", 7 },
#endif
#ifndef SIGQUIT
        { "SIGQUIT", 7 },
#endif
#ifndef SIGSEGV
        { "SIGSEGV", 7 },
#endif
#ifndef SIGSTOP
        { "SIGSTOP", 7 },
#endif
#ifndef SIGTERM
        { "SIGTERM", 7 },
#endif
#ifndef SIGTSTP
        { "SIGTSTP", 7 },
#endif
#ifndef SIGTTIN
        { "SIGTTIN", 7 },
#endif
#ifndef SIGTTOU
        { "SIGTTOU", 7 },
#endif
#ifndef SIGUSR1
        { "SIGUSR1", 7 },
#endif
#ifndef SIGUSR2
        { "SIGUSR2", 7 },
#endif
#ifndef SIG_BLOCK
        { "SIG_BLOCK", 9 },
#endif
#ifndef SIG_SETMASK
        { "SIG_SETMASK", 11 },
#endif
#ifndef SIG_UNBLOCK
        { "SIG_UNBLOCK", 11 },
#endif
#ifndef SSIZE_MAX
        { "SSIZE_MAX", 9 },
#endif
#ifndef SIGBUS
        { "SIGBUS", 6 },
#endif
#ifndef SIGPOLL
        { "SIGPOLL", 7 },
#endif
#ifndef SIGPROF
        { "SIGPROF", 7 },
#endif
#ifndef SIGSYS
        { "SIGSYS", 6 },
#endif
#ifndef SIGTRAP
        { "SIGTRAP", 7 },
#endif
#ifndef SIGURG
        { "SIGURG", 6 },
#endif
#ifndef SIGVTALRM
        { "SIGVTALRM", 9 },
#endif
#ifndef SIGXCPU
        { "SIGXCPU", 7 },
#endif
#ifndef SIGXFSZ
        { "SIGXFSZ", 7 },
#endif
#ifndef STDERR_FILENO
        { "STDERR_FILENO", 13 },
#endif
#ifndef STDIN_FILENO
        { "STDIN_FILENO", 12 },
#endif
#ifndef STDOUT_FILENO
        { "STDOUT_FILENO", 13 },
#endif
#ifndef STREAM_MAX
        { "STREAM_MAX", 10 },
#endif
#ifndef TCIFLUSH
        { "TCIFLUSH", 8 },
#endif
#ifndef TCIOFF
        { "TCIOFF", 6 },
#endif
#ifndef TCIOFLUSH
        { "TCIOFLUSH", 9 },
#endif
#ifndef TCION
        { "TCION", 5 },
#endif
#ifndef TCOFLUSH
        { "TCOFLUSH", 8 },
#endif
#ifndef TCOOFF
        { "TCOOFF", 6 },
#endif
#ifndef TCOON
        { "TCOON", 5 },
#endif
#ifndef TCSADRAIN
        { "TCSADRAIN", 9 },
#endif
#ifndef TCSAFLUSH
        { "TCSAFLUSH", 9 },
#endif
#ifndef TCSANOW
        { "TCSANOW", 7 },
#endif
#ifndef TMP_MAX
        { "TMP_MAX", 7 },
#endif
#ifndef TOSTOP
        { "TOSTOP", 6 },
#endif
#ifndef TZNAME_MAX
        { "TZNAME_MAX", 10 },
#endif
#ifndef VEOF
        { "VEOF", 4 },
#endif
#ifndef VEOL
        { "VEOL", 4 },
#endif
#ifndef VERASE
        { "VERASE", 6 },
#endif
#ifndef VINTR
        { "VINTR", 5 },
#endif
#ifndef VKILL
        { "VKILL", 5 },
#endif
#ifndef VMIN
        { "VMIN", 4 },
#endif
#ifndef VQUIT
        { "VQUIT", 5 },
#endif
#ifndef VSTART
        { "VSTART", 6 },
#endif
#ifndef VSTOP
        { "VSTOP", 5 },
#endif
#ifndef VSUSP
        { "VSUSP", 5 },
#endif
#ifndef VTIME
        { "VTIME", 5 },
#endif
#ifndef WNOHANG
        { "WNOHANG", 7 },
#endif
#ifndef WUNTRACED
        { "WUNTRACED", 9 },
#endif
#ifndef W_OK
        { "W_OK", 4 },
#endif
#ifndef X_OK
        { "X_OK", 4 },
#endif
#ifndef _PC_CHOWN_RESTRICTED
        { "_PC_CHOWN_RESTRICTED", 20 },
#endif
#ifndef _PC_LINK_MAX
        { "_PC_LINK_MAX", 12 },
#endif
#ifndef _PC_MAX_CANON
        { "_PC_MAX_CANON", 13 },
#endif
#ifndef _PC_MAX_INPUT
        { "_PC_MAX_INPUT", 13 },
#endif
#ifndef _PC_NAME_MAX
        { "_PC_NAME_MAX", 12 },
#endif
#ifndef _PC_NO_TRUNC
        { "_PC_NO_TRUNC", 12 },
#endif
#ifndef _PC_PATH_MAX
        { "_PC_PATH_MAX", 12 },
#endif
#ifndef _PC_PIPE_BUF
        { "_PC_PIPE_BUF", 12 },
#endif
#ifndef _PC_VDISABLE
        { "_PC_VDISABLE", 12 },
#endif
#ifndef _SC_ARG_MAX
        { "_SC_ARG_MAX", 11 },
#endif
#ifndef _SC_CHILD_MAX
        { "_SC_CHILD_MAX", 13 },
#endif
#ifndef _SC_CLK_TCK
        { "_SC_CLK_TCK", 11 },
#endif
#ifndef _SC_JOB_CONTROL
        { "_SC_JOB_CONTROL", 15 },
#endif
#ifndef _SC_NGROUPS_MAX
        { "_SC_NGROUPS_MAX", 15 },
#endif
#ifndef _SC_OPEN_MAX
        { "_SC_OPEN_MAX", 12 },
#endif
#ifndef _SC_PAGESIZE
        { "_SC_PAGESIZE", 12 },
#endif
#ifndef _SC_SAVED_IDS
        { "_SC_SAVED_IDS", 13 },
#endif
#ifndef _SC_STREAM_MAX
        { "_SC_STREAM_MAX", 14 },
#endif
#ifndef _SC_TZNAME_MAX
        { "_SC_TZNAME_MAX", 14 },
#endif
#ifndef _SC_VERSION
        { "_SC_VERSION", 11 },
#endif
#ifndef EAI_AGAIN
        { "EAI_AGAIN", 9 },
#endif
#ifndef EAI_BADFLAGS
        { "EAI_BADFLAGS", 12 },
#endif
#ifndef EAI_FAIL
        { "EAI_FAIL", 8 },
#endif
#ifndef EAI_FAMILY
        { "EAI_FAMILY", 10 },
#endif
#ifndef EAI_MEMORY
        { "EAI_MEMORY", 10 },
#endif
#ifndef EAI_NONAME
        { "EAI_NONAME", 10 },
#endif
#ifndef EAI_SERVICE
        { "EAI_SERVICE", 11 },
#endif
#ifndef EAI_SOCKTYPE
        { "EAI_SOCKTYPE", 12 },
#endif
#ifndef EAI_SYSTEM
        { "EAI_SYSTEM", 10 },
#endif
#ifndef EAI_OVERFLOW
        { "EAI_OVERFLOW", 12 },
#endif
#ifndef CLK_TCK
        { "CLK_TCK", 7 },
#endif
#ifndef MB_CUR_MAX
        { "MB_CUR_MAX", 10 },
#endif
#ifndef SIG_DFL
        { "SIG_DFL", 7 },
#endif
#ifndef SIG_ERR
        { "SIG_ERR", 7 },
#endif
#ifndef SIG_IGN
        { "SIG_IGN", 7 },
#endif
#ifndef L_tmpname
        { "L_tmpname", 9 },
#endif
#ifndef NULL
        { "NULL", 4 },
#endif
#ifndef FLT_ROUNDS
        { "FLT_ROUNDS", 10 },
#endif
#if (defined(USE_LONG_DOUBLE) && defined(HUGE_VALL)) || defined(HUGE_VAL)
	/* HUGE_VALL is admittedly non-POSIX but if we are using long doubles
	 * we might as well use long doubles. --jhi */
#else
        { "HUGE_VAL", 8 },
#endif
#ifndef SA_NOCLDSTOP
        { "SA_NOCLDSTOP", 12 },
#endif
#ifndef SA_NOCLDWAIT
        { "SA_NOCLDWAIT", 12 },
#endif
#ifndef SA_NODEFER
        { "SA_NODEFER", 10 },
#endif
#ifndef SA_ONSTACK
        { "SA_ONSTACK", 10 },
#endif
#ifndef SA_RESETHAND
        { "SA_RESETHAND", 12 },
#endif
#ifndef SA_RESTART
        { "SA_RESTART", 10 },
#endif
#ifndef SA_SIGINFO
        { "SA_SIGINFO", 10 },
#endif
#ifndef UCHAR_MAX
        { "UCHAR_MAX", 9 },
#endif
#ifndef UINT_MAX
        { "UINT_MAX", 8 },
#endif
#ifndef ULONG_MAX
        { "ULONG_MAX", 9 },
#endif
#ifndef USHRT_MAX
        { "USHRT_MAX", 9 },
#endif
#ifndef DBL_MAX
        { "DBL_MAX", 7 },
#endif
#ifndef FLT_MAX
        { "FLT_MAX", 7 },
#endif
#ifndef LDBL_MAX
        { "LDBL_MAX", 8 },
#endif
#ifndef LDBL_MIN
        { "LDBL_MIN", 8 },
#endif
#ifndef LDBL_EPSILON
        { "LDBL_EPSILON", 12 },
#endif
#ifndef DBL_EPSILON
        { "DBL_EPSILON", 11 },
#endif
#ifndef DBL_MIN
        { "DBL_MIN", 7 },
#endif
#ifndef FLT_EPSILON
        { "FLT_EPSILON", 11 },
#endif
#ifndef FLT_MIN
        { "FLT_MIN", 7 },
#endif
#ifndef DBL_DIG
        { "DBL_DIG", 7 },
#endif
#ifndef DBL_MANT_DIG
        { "DBL_MANT_DIG", 12 },
#endif
#ifndef DBL_MAX_10_EXP
        { "DBL_MAX_10_EXP", 14 },
#endif
#ifndef DBL_MAX_EXP
        { "DBL_MAX_EXP", 11 },
#endif
#ifndef DBL_MIN_10_EXP
        { "DBL_MIN_10_EXP", 14 },
#endif
#ifndef DBL_MIN_EXP
        { "DBL_MIN_EXP", 11 },
#endif
#ifndef FLT_DIG
        { "FLT_DIG", 7 },
#endif
#ifndef FLT_MANT_DIG
        { "FLT_MANT_DIG", 12 },
#endif
#ifndef FLT_MAX_10_EXP
        { "FLT_MAX_10_EXP", 14 },
#endif
#ifndef FLT_MAX_EXP
        { "FLT_MAX_EXP", 11 },
#endif
#ifndef FLT_MIN_10_EXP
        { "FLT_MIN_10_EXP", 14 },
#endif
#ifndef FLT_MIN_EXP
        { "FLT_MIN_EXP", 11 },
#endif
#ifndef FLT_RADIX
        { "FLT_RADIX", 9 },
#endif
#ifndef LDBL_DIG
        { "LDBL_DIG", 8 },
#endif
#ifndef LDBL_MANT_DIG
        { "LDBL_MANT_DIG", 13 },
#endif
#ifndef LDBL_MAX_10_EXP
        { "LDBL_MAX_10_EXP", 15 },
#endif
#ifndef LDBL_MAX_EXP
        { "LDBL_MAX_EXP", 12 },
#endif
#ifndef LDBL_MIN_10_EXP
        { "LDBL_MIN_10_EXP", 15 },
#endif
#ifndef LDBL_MIN_EXP
        { "LDBL_MIN_EXP", 12 },
#endif
#ifndef SIGRTMAX
        { "SIGRTMAX", 8 },
#endif
#ifndef SIGRTMIN
        { "SIGRTMIN", 8 },
#endif
        { NULL, 0 } };
struct iv_s {const char *name; I32 namelen; IV value;};
struct nv_s {const char *name; I32 namelen; NV value;};
struct uv_s {const char *name; I32 namelen; UV value;};
struct yes_s {const char *name; I32 namelen;};
