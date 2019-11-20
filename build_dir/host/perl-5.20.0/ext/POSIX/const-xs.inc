BOOT:
  {
#ifdef dTHX
    dTHX;
#endif
    HV *symbol_table = get_hv("POSIX::", GV_ADD);

    static const struct iv_s values_for_iv[] =
      {
#ifdef ARG_MAX
        { "ARG_MAX", 7, ARG_MAX },
#endif
#ifdef B0
        { "B0", 2, B0 },
#endif
#ifdef B110
        { "B110", 4, B110 },
#endif
#ifdef B1200
        { "B1200", 5, B1200 },
#endif
#ifdef B134
        { "B134", 4, B134 },
#endif
#ifdef B150
        { "B150", 4, B150 },
#endif
#ifdef B1800
        { "B1800", 5, B1800 },
#endif
#ifdef B19200
        { "B19200", 6, B19200 },
#endif
#ifdef B200
        { "B200", 4, B200 },
#endif
#ifdef B2400
        { "B2400", 5, B2400 },
#endif
#ifdef B300
        { "B300", 4, B300 },
#endif
#ifdef B38400
        { "B38400", 6, B38400 },
#endif
#ifdef B4800
        { "B4800", 5, B4800 },
#endif
#ifdef B50
        { "B50", 3, B50 },
#endif
#ifdef B600
        { "B600", 4, B600 },
#endif
#ifdef B75
        { "B75", 3, B75 },
#endif
#ifdef B9600
        { "B9600", 5, B9600 },
#endif
#ifdef BRKINT
        { "BRKINT", 6, BRKINT },
#endif
#ifdef BUFSIZ
        { "BUFSIZ", 6, BUFSIZ },
#endif
#ifdef CHAR_BIT
        { "CHAR_BIT", 8, CHAR_BIT },
#endif
#ifdef CHAR_MAX
        { "CHAR_MAX", 8, CHAR_MAX },
#endif
#ifdef CHAR_MIN
        { "CHAR_MIN", 8, CHAR_MIN },
#endif
#ifdef CHILD_MAX
        { "CHILD_MAX", 9, CHILD_MAX },
#endif
#ifdef CLOCAL
        { "CLOCAL", 6, CLOCAL },
#endif
#ifdef CLOCKS_PER_SEC
        { "CLOCKS_PER_SEC", 14, CLOCKS_PER_SEC },
#endif
#ifdef CREAD
        { "CREAD", 5, CREAD },
#endif
#ifdef CS5
        { "CS5", 3, CS5 },
#endif
#ifdef CS6
        { "CS6", 3, CS6 },
#endif
#ifdef CS7
        { "CS7", 3, CS7 },
#endif
#ifdef CS8
        { "CS8", 3, CS8 },
#endif
#ifdef CSIZE
        { "CSIZE", 5, CSIZE },
#endif
#ifdef CSTOPB
        { "CSTOPB", 6, CSTOPB },
#endif
#ifdef E2BIG
        { "E2BIG", 5, E2BIG },
#endif
#ifdef EACCES
        { "EACCES", 6, EACCES },
#endif
#ifdef EADDRINUSE
        { "EADDRINUSE", 10, EADDRINUSE },
#endif
#ifdef EADDRNOTAVAIL
        { "EADDRNOTAVAIL", 13, EADDRNOTAVAIL },
#endif
#ifdef EAFNOSUPPORT
        { "EAFNOSUPPORT", 12, EAFNOSUPPORT },
#endif
#ifdef EAGAIN
        { "EAGAIN", 6, EAGAIN },
#endif
#ifdef EALREADY
        { "EALREADY", 8, EALREADY },
#endif
#ifdef EBADF
        { "EBADF", 5, EBADF },
#endif
#ifdef EBADMSG
        { "EBADMSG", 7, EBADMSG },
#endif
#ifdef EBUSY
        { "EBUSY", 5, EBUSY },
#endif
#ifdef ECANCELED
        { "ECANCELED", 9, ECANCELED },
#endif
#ifdef ECHILD
        { "ECHILD", 6, ECHILD },
#endif
#ifdef ECHO
        { "ECHO", 4, ECHO },
#endif
#ifdef ECHOE
        { "ECHOE", 5, ECHOE },
#endif
#ifdef ECHOK
        { "ECHOK", 5, ECHOK },
#endif
#ifdef ECHONL
        { "ECHONL", 6, ECHONL },
#endif
#ifdef ECONNABORTED
        { "ECONNABORTED", 12, ECONNABORTED },
#endif
#ifdef ECONNREFUSED
        { "ECONNREFUSED", 12, ECONNREFUSED },
#endif
#ifdef ECONNRESET
        { "ECONNRESET", 10, ECONNRESET },
#endif
#ifdef EDEADLK
        { "EDEADLK", 7, EDEADLK },
#endif
#ifdef EDESTADDRREQ
        { "EDESTADDRREQ", 12, EDESTADDRREQ },
#endif
#ifdef EDOM
        { "EDOM", 4, EDOM },
#endif
#ifdef EDQUOT
        { "EDQUOT", 6, EDQUOT },
#endif
#ifdef EEXIST
        { "EEXIST", 6, EEXIST },
#endif
#ifdef EFAULT
        { "EFAULT", 6, EFAULT },
#endif
#ifdef EFBIG
        { "EFBIG", 5, EFBIG },
#endif
#ifdef EHOSTDOWN
        { "EHOSTDOWN", 9, EHOSTDOWN },
#endif
#ifdef EHOSTUNREACH
        { "EHOSTUNREACH", 12, EHOSTUNREACH },
#endif
#ifdef EIDRM
        { "EIDRM", 5, EIDRM },
#endif
#ifdef EILSEQ
        { "EILSEQ", 6, EILSEQ },
#endif
#ifdef EINPROGRESS
        { "EINPROGRESS", 11, EINPROGRESS },
#endif
#ifdef EINTR
        { "EINTR", 5, EINTR },
#endif
#ifdef EINVAL
        { "EINVAL", 6, EINVAL },
#endif
#ifdef EIO
        { "EIO", 3, EIO },
#endif
#ifdef EISCONN
        { "EISCONN", 7, EISCONN },
#endif
#ifdef EISDIR
        { "EISDIR", 6, EISDIR },
#endif
#ifdef ELOOP
        { "ELOOP", 5, ELOOP },
#endif
#ifdef EMFILE
        { "EMFILE", 6, EMFILE },
#endif
#ifdef EMLINK
        { "EMLINK", 6, EMLINK },
#endif
#ifdef EMSGSIZE
        { "EMSGSIZE", 8, EMSGSIZE },
#endif
#ifdef ENAMETOOLONG
        { "ENAMETOOLONG", 12, ENAMETOOLONG },
#endif
#ifdef ENETDOWN
        { "ENETDOWN", 8, ENETDOWN },
#endif
#ifdef ENETRESET
        { "ENETRESET", 9, ENETRESET },
#endif
#ifdef ENETUNREACH
        { "ENETUNREACH", 11, ENETUNREACH },
#endif
#ifdef ENFILE
        { "ENFILE", 6, ENFILE },
#endif
#ifdef ENOBUFS
        { "ENOBUFS", 7, ENOBUFS },
#endif
#ifdef ENODATA
        { "ENODATA", 7, ENODATA },
#endif
#ifdef ENODEV
        { "ENODEV", 6, ENODEV },
#endif
#ifdef ENOENT
        { "ENOENT", 6, ENOENT },
#endif
#ifdef ENOEXEC
        { "ENOEXEC", 7, ENOEXEC },
#endif
#ifdef ENOLCK
        { "ENOLCK", 6, ENOLCK },
#endif
#ifdef ENOLINK
        { "ENOLINK", 7, ENOLINK },
#endif
#ifdef ENOMEM
        { "ENOMEM", 6, ENOMEM },
#endif
#ifdef ENOMSG
        { "ENOMSG", 6, ENOMSG },
#endif
#ifdef ENOPROTOOPT
        { "ENOPROTOOPT", 11, ENOPROTOOPT },
#endif
#ifdef ENOSPC
        { "ENOSPC", 6, ENOSPC },
#endif
#ifdef ENOSR
        { "ENOSR", 5, ENOSR },
#endif
#ifdef ENOSTR
        { "ENOSTR", 6, ENOSTR },
#endif
#ifdef ENOSYS
        { "ENOSYS", 6, ENOSYS },
#endif
#ifdef ENOTBLK
        { "ENOTBLK", 7, ENOTBLK },
#endif
#ifdef ENOTCONN
        { "ENOTCONN", 8, ENOTCONN },
#endif
#ifdef ENOTDIR
        { "ENOTDIR", 7, ENOTDIR },
#endif
#ifdef ENOTEMPTY
        { "ENOTEMPTY", 9, ENOTEMPTY },
#endif
#ifdef ENOTRECOVERABLE
        { "ENOTRECOVERABLE", 15, ENOTRECOVERABLE },
#endif
#ifdef ENOTSOCK
        { "ENOTSOCK", 8, ENOTSOCK },
#endif
#ifdef ENOTSUP
        { "ENOTSUP", 7, ENOTSUP },
#endif
#ifdef ENOTTY
        { "ENOTTY", 6, ENOTTY },
#endif
#ifdef ENXIO
        { "ENXIO", 5, ENXIO },
#endif
#ifdef EOF
        { "EOF", 3, EOF },
#endif
#ifdef EOPNOTSUPP
        { "EOPNOTSUPP", 10, EOPNOTSUPP },
#endif
#ifdef EOTHER
        { "EOTHER", 6, EOTHER },
#endif
#ifdef EOVERFLOW
        { "EOVERFLOW", 9, EOVERFLOW },
#endif
#ifdef EOWNERDEAD
        { "EOWNERDEAD", 10, EOWNERDEAD },
#endif
#ifdef EPERM
        { "EPERM", 5, EPERM },
#endif
#ifdef EPFNOSUPPORT
        { "EPFNOSUPPORT", 12, EPFNOSUPPORT },
#endif
#ifdef EPIPE
        { "EPIPE", 5, EPIPE },
#endif
#ifdef EPROCLIM
        { "EPROCLIM", 8, EPROCLIM },
#endif
#ifdef EPROTO
        { "EPROTO", 6, EPROTO },
#endif
#ifdef EPROTONOSUPPORT
        { "EPROTONOSUPPORT", 15, EPROTONOSUPPORT },
#endif
#ifdef EPROTOTYPE
        { "EPROTOTYPE", 10, EPROTOTYPE },
#endif
#ifdef ERANGE
        { "ERANGE", 6, ERANGE },
#endif
#ifdef EREMOTE
        { "EREMOTE", 7, EREMOTE },
#endif
#ifdef ERESTART
        { "ERESTART", 8, ERESTART },
#endif
#ifdef EROFS
        { "EROFS", 5, EROFS },
#endif
#ifdef ESHUTDOWN
        { "ESHUTDOWN", 9, ESHUTDOWN },
#endif
#ifdef ESOCKTNOSUPPORT
        { "ESOCKTNOSUPPORT", 15, ESOCKTNOSUPPORT },
#endif
#ifdef ESPIPE
        { "ESPIPE", 6, ESPIPE },
#endif
#ifdef ESRCH
        { "ESRCH", 5, ESRCH },
#endif
#ifdef ESTALE
        { "ESTALE", 6, ESTALE },
#endif
#ifdef ETIME
        { "ETIME", 5, ETIME },
#endif
#ifdef ETIMEDOUT
        { "ETIMEDOUT", 9, ETIMEDOUT },
#endif
#ifdef ETOOMANYREFS
        { "ETOOMANYREFS", 12, ETOOMANYREFS },
#endif
#ifdef ETXTBSY
        { "ETXTBSY", 7, ETXTBSY },
#endif
#ifdef EUSERS
        { "EUSERS", 6, EUSERS },
#endif
#ifdef EWOULDBLOCK
        { "EWOULDBLOCK", 11, EWOULDBLOCK },
#endif
#ifdef EXDEV
        { "EXDEV", 5, EXDEV },
#endif
#ifdef FILENAME_MAX
        { "FILENAME_MAX", 12, FILENAME_MAX },
#endif
#ifdef F_OK
        { "F_OK", 4, F_OK },
#endif
#ifdef HUPCL
        { "HUPCL", 5, HUPCL },
#endif
#ifdef ICANON
        { "ICANON", 6, ICANON },
#endif
#ifdef ICRNL
        { "ICRNL", 5, ICRNL },
#endif
#ifdef IEXTEN
        { "IEXTEN", 6, IEXTEN },
#endif
#ifdef IGNBRK
        { "IGNBRK", 6, IGNBRK },
#endif
#ifdef IGNCR
        { "IGNCR", 5, IGNCR },
#endif
#ifdef IGNPAR
        { "IGNPAR", 6, IGNPAR },
#endif
#ifdef INLCR
        { "INLCR", 5, INLCR },
#endif
#ifdef INPCK
        { "INPCK", 5, INPCK },
#endif
#ifdef INT_MAX
        { "INT_MAX", 7, INT_MAX },
#endif
#ifdef INT_MIN
        { "INT_MIN", 7, INT_MIN },
#endif
#ifdef ISIG
        { "ISIG", 4, ISIG },
#endif
#ifdef ISTRIP
        { "ISTRIP", 6, ISTRIP },
#endif
#ifdef IXOFF
        { "IXOFF", 5, IXOFF },
#endif
#ifdef IXON
        { "IXON", 4, IXON },
#endif
#ifdef LC_ALL
        { "LC_ALL", 6, LC_ALL },
#endif
#ifdef LC_COLLATE
        { "LC_COLLATE", 10, LC_COLLATE },
#endif
#ifdef LC_CTYPE
        { "LC_CTYPE", 8, LC_CTYPE },
#endif
#ifdef LC_MESSAGES
        { "LC_MESSAGES", 11, LC_MESSAGES },
#endif
#ifdef LC_MONETARY
        { "LC_MONETARY", 11, LC_MONETARY },
#endif
#ifdef LC_NUMERIC
        { "LC_NUMERIC", 10, LC_NUMERIC },
#endif
#ifdef LC_TIME
        { "LC_TIME", 7, LC_TIME },
#endif
#ifdef LINK_MAX
        { "LINK_MAX", 8, LINK_MAX },
#endif
#ifdef LONG_MAX
        { "LONG_MAX", 8, LONG_MAX },
#endif
#ifdef LONG_MIN
        { "LONG_MIN", 8, LONG_MIN },
#endif
#ifdef L_ctermid
        { "L_ctermid", 9, L_ctermid },
#endif
#ifdef L_cuserid
        { "L_cuserid", 9, L_cuserid },
#endif
#ifdef L_tmpnam
        { "L_tmpnam", 8, L_tmpnam },
#endif
#ifdef MAX_CANON
        { "MAX_CANON", 9, MAX_CANON },
#endif
#ifdef MAX_INPUT
        { "MAX_INPUT", 9, MAX_INPUT },
#endif
#ifdef MB_LEN_MAX
        { "MB_LEN_MAX", 10, MB_LEN_MAX },
#endif
#ifdef MSG_CTRUNC
        { "MSG_CTRUNC", 10, MSG_CTRUNC },
#endif
#ifdef MSG_DONTROUTE
        { "MSG_DONTROUTE", 13, MSG_DONTROUTE },
#endif
#ifdef MSG_EOR
        { "MSG_EOR", 7, MSG_EOR },
#endif
#ifdef MSG_OOB
        { "MSG_OOB", 7, MSG_OOB },
#endif
#ifdef MSG_PEEK
        { "MSG_PEEK", 8, MSG_PEEK },
#endif
#ifdef MSG_TRUNC
        { "MSG_TRUNC", 9, MSG_TRUNC },
#endif
#ifdef MSG_WAITALL
        { "MSG_WAITALL", 11, MSG_WAITALL },
#endif
#ifdef NAME_MAX
        { "NAME_MAX", 8, NAME_MAX },
#endif
#ifdef NCCS
        { "NCCS", 4, NCCS },
#endif
#ifdef NGROUPS_MAX
        { "NGROUPS_MAX", 11, NGROUPS_MAX },
#endif
#ifdef NOFLSH
        { "NOFLSH", 6, NOFLSH },
#endif
#ifdef OPEN_MAX
        { "OPEN_MAX", 8, OPEN_MAX },
#endif
#ifdef OPOST
        { "OPOST", 5, OPOST },
#endif
#ifdef PARENB
        { "PARENB", 6, PARENB },
#endif
#ifdef PARMRK
        { "PARMRK", 6, PARMRK },
#endif
#ifdef PARODD
        { "PARODD", 6, PARODD },
#endif
#ifdef PATH_MAX
        { "PATH_MAX", 8, PATH_MAX },
#endif
#ifdef PIPE_BUF
        { "PIPE_BUF", 8, PIPE_BUF },
#endif
#ifdef RAND_MAX
        { "RAND_MAX", 8, RAND_MAX },
#endif
#ifdef R_OK
        { "R_OK", 4, R_OK },
#endif
#ifdef SCHAR_MAX
        { "SCHAR_MAX", 9, SCHAR_MAX },
#endif
#ifdef SCHAR_MIN
        { "SCHAR_MIN", 9, SCHAR_MIN },
#endif
#ifdef SHRT_MAX
        { "SHRT_MAX", 8, SHRT_MAX },
#endif
#ifdef SHRT_MIN
        { "SHRT_MIN", 8, SHRT_MIN },
#endif
#ifdef SIGABRT
        { "SIGABRT", 7, SIGABRT },
#endif
#ifdef SIGALRM
        { "SIGALRM", 7, SIGALRM },
#endif
#ifdef SIGCHLD
        { "SIGCHLD", 7, SIGCHLD },
#endif
#ifdef SIGCONT
        { "SIGCONT", 7, SIGCONT },
#endif
#ifdef SIGFPE
        { "SIGFPE", 6, SIGFPE },
#endif
#ifdef SIGHUP
        { "SIGHUP", 6, SIGHUP },
#endif
#ifdef SIGILL
        { "SIGILL", 6, SIGILL },
#endif
#ifdef SIGINT
        { "SIGINT", 6, SIGINT },
#endif
#ifdef SIGKILL
        { "SIGKILL", 7, SIGKILL },
#endif
#ifdef SIGPIPE
        { "SIGPIPE", 7, SIGPIPE },
#endif
#ifdef SIGQUIT
        { "SIGQUIT", 7, SIGQUIT },
#endif
#ifdef SIGSEGV
        { "SIGSEGV", 7, SIGSEGV },
#endif
#ifdef SIGSTOP
        { "SIGSTOP", 7, SIGSTOP },
#endif
#ifdef SIGTERM
        { "SIGTERM", 7, SIGTERM },
#endif
#ifdef SIGTSTP
        { "SIGTSTP", 7, SIGTSTP },
#endif
#ifdef SIGTTIN
        { "SIGTTIN", 7, SIGTTIN },
#endif
#ifdef SIGTTOU
        { "SIGTTOU", 7, SIGTTOU },
#endif
#ifdef SIGUSR1
        { "SIGUSR1", 7, SIGUSR1 },
#endif
#ifdef SIGUSR2
        { "SIGUSR2", 7, SIGUSR2 },
#endif
#ifdef SIG_BLOCK
        { "SIG_BLOCK", 9, SIG_BLOCK },
#endif
#ifdef SIG_SETMASK
        { "SIG_SETMASK", 11, SIG_SETMASK },
#endif
#ifdef SIG_UNBLOCK
        { "SIG_UNBLOCK", 11, SIG_UNBLOCK },
#endif
#ifdef SSIZE_MAX
        { "SSIZE_MAX", 9, SSIZE_MAX },
#endif
#ifdef SIGBUS
        { "SIGBUS", 6, SIGBUS },
#endif
#ifdef SIGPOLL
        { "SIGPOLL", 7, SIGPOLL },
#endif
#ifdef SIGPROF
        { "SIGPROF", 7, SIGPROF },
#endif
#ifdef SIGSYS
        { "SIGSYS", 6, SIGSYS },
#endif
#ifdef SIGTRAP
        { "SIGTRAP", 7, SIGTRAP },
#endif
#ifdef SIGURG
        { "SIGURG", 6, SIGURG },
#endif
#ifdef SIGVTALRM
        { "SIGVTALRM", 9, SIGVTALRM },
#endif
#ifdef SIGXCPU
        { "SIGXCPU", 7, SIGXCPU },
#endif
#ifdef SIGXFSZ
        { "SIGXFSZ", 7, SIGXFSZ },
#endif
#ifdef STDERR_FILENO
        { "STDERR_FILENO", 13, STDERR_FILENO },
#endif
#ifdef STDIN_FILENO
        { "STDIN_FILENO", 12, STDIN_FILENO },
#endif
#ifdef STDOUT_FILENO
        { "STDOUT_FILENO", 13, STDOUT_FILENO },
#endif
#ifdef STREAM_MAX
        { "STREAM_MAX", 10, STREAM_MAX },
#endif
#ifdef TCIFLUSH
        { "TCIFLUSH", 8, TCIFLUSH },
#endif
#ifdef TCIOFF
        { "TCIOFF", 6, TCIOFF },
#endif
#ifdef TCIOFLUSH
        { "TCIOFLUSH", 9, TCIOFLUSH },
#endif
#ifdef TCION
        { "TCION", 5, TCION },
#endif
#ifdef TCOFLUSH
        { "TCOFLUSH", 8, TCOFLUSH },
#endif
#ifdef TCOOFF
        { "TCOOFF", 6, TCOOFF },
#endif
#ifdef TCOON
        { "TCOON", 5, TCOON },
#endif
#ifdef TCSADRAIN
        { "TCSADRAIN", 9, TCSADRAIN },
#endif
#ifdef TCSAFLUSH
        { "TCSAFLUSH", 9, TCSAFLUSH },
#endif
#ifdef TCSANOW
        { "TCSANOW", 7, TCSANOW },
#endif
#ifdef TMP_MAX
        { "TMP_MAX", 7, TMP_MAX },
#endif
#ifdef TOSTOP
        { "TOSTOP", 6, TOSTOP },
#endif
#ifdef TZNAME_MAX
        { "TZNAME_MAX", 10, TZNAME_MAX },
#endif
#ifdef VEOF
        { "VEOF", 4, VEOF },
#endif
#ifdef VEOL
        { "VEOL", 4, VEOL },
#endif
#ifdef VERASE
        { "VERASE", 6, VERASE },
#endif
#ifdef VINTR
        { "VINTR", 5, VINTR },
#endif
#ifdef VKILL
        { "VKILL", 5, VKILL },
#endif
#ifdef VMIN
        { "VMIN", 4, VMIN },
#endif
#ifdef VQUIT
        { "VQUIT", 5, VQUIT },
#endif
#ifdef VSTART
        { "VSTART", 6, VSTART },
#endif
#ifdef VSTOP
        { "VSTOP", 5, VSTOP },
#endif
#ifdef VSUSP
        { "VSUSP", 5, VSUSP },
#endif
#ifdef VTIME
        { "VTIME", 5, VTIME },
#endif
#ifdef WNOHANG
        { "WNOHANG", 7, WNOHANG },
#endif
#ifdef WUNTRACED
        { "WUNTRACED", 9, WUNTRACED },
#endif
#ifdef W_OK
        { "W_OK", 4, W_OK },
#endif
#ifdef X_OK
        { "X_OK", 4, X_OK },
#endif
#ifdef _PC_CHOWN_RESTRICTED
        { "_PC_CHOWN_RESTRICTED", 20, _PC_CHOWN_RESTRICTED },
#endif
#ifdef _PC_LINK_MAX
        { "_PC_LINK_MAX", 12, _PC_LINK_MAX },
#endif
#ifdef _PC_MAX_CANON
        { "_PC_MAX_CANON", 13, _PC_MAX_CANON },
#endif
#ifdef _PC_MAX_INPUT
        { "_PC_MAX_INPUT", 13, _PC_MAX_INPUT },
#endif
#ifdef _PC_NAME_MAX
        { "_PC_NAME_MAX", 12, _PC_NAME_MAX },
#endif
#ifdef _PC_NO_TRUNC
        { "_PC_NO_TRUNC", 12, _PC_NO_TRUNC },
#endif
#ifdef _PC_PATH_MAX
        { "_PC_PATH_MAX", 12, _PC_PATH_MAX },
#endif
#ifdef _PC_PIPE_BUF
        { "_PC_PIPE_BUF", 12, _PC_PIPE_BUF },
#endif
#ifdef _PC_VDISABLE
        { "_PC_VDISABLE", 12, _PC_VDISABLE },
#endif
#ifdef _SC_ARG_MAX
        { "_SC_ARG_MAX", 11, _SC_ARG_MAX },
#endif
#ifdef _SC_CHILD_MAX
        { "_SC_CHILD_MAX", 13, _SC_CHILD_MAX },
#endif
#ifdef _SC_CLK_TCK
        { "_SC_CLK_TCK", 11, _SC_CLK_TCK },
#endif
#ifdef _SC_JOB_CONTROL
        { "_SC_JOB_CONTROL", 15, _SC_JOB_CONTROL },
#endif
#ifdef _SC_NGROUPS_MAX
        { "_SC_NGROUPS_MAX", 15, _SC_NGROUPS_MAX },
#endif
#ifdef _SC_OPEN_MAX
        { "_SC_OPEN_MAX", 12, _SC_OPEN_MAX },
#endif
#ifdef _SC_PAGESIZE
        { "_SC_PAGESIZE", 12, _SC_PAGESIZE },
#endif
#ifdef _SC_SAVED_IDS
        { "_SC_SAVED_IDS", 13, _SC_SAVED_IDS },
#endif
#ifdef _SC_STREAM_MAX
        { "_SC_STREAM_MAX", 14, _SC_STREAM_MAX },
#endif
#ifdef _SC_TZNAME_MAX
        { "_SC_TZNAME_MAX", 14, _SC_TZNAME_MAX },
#endif
#ifdef _SC_VERSION
        { "_SC_VERSION", 11, _SC_VERSION },
#endif
#ifdef EAI_AGAIN
        { "EAI_AGAIN", 9, EAI_AGAIN },
#endif
#ifdef EAI_BADFLAGS
        { "EAI_BADFLAGS", 12, EAI_BADFLAGS },
#endif
#ifdef EAI_FAIL
        { "EAI_FAIL", 8, EAI_FAIL },
#endif
#ifdef EAI_FAMILY
        { "EAI_FAMILY", 10, EAI_FAMILY },
#endif
#ifdef EAI_MEMORY
        { "EAI_MEMORY", 10, EAI_MEMORY },
#endif
#ifdef EAI_NONAME
        { "EAI_NONAME", 10, EAI_NONAME },
#endif
#ifdef EAI_SERVICE
        { "EAI_SERVICE", 11, EAI_SERVICE },
#endif
#ifdef EAI_SOCKTYPE
        { "EAI_SOCKTYPE", 12, EAI_SOCKTYPE },
#endif
#ifdef EAI_SYSTEM
        { "EAI_SYSTEM", 10, EAI_SYSTEM },
#endif
#ifdef EAI_OVERFLOW
        { "EAI_OVERFLOW", 12, EAI_OVERFLOW },
#endif
#ifdef EXIT_FAILURE
        { "EXIT_FAILURE", 12, EXIT_FAILURE },
#endif
#ifdef EXIT_SUCCESS
        { "EXIT_SUCCESS", 12, EXIT_SUCCESS },
#endif
#ifdef L_tmpname
        { "L_tmpname", 9, L_tmpnam },
#endif
#ifdef NULL
        { "NULL", 4, 0 },
#endif
#ifdef _POSIX_ARG_MAX
        { "_POSIX_ARG_MAX", 14, _POSIX_ARG_MAX },
#endif
#ifdef _POSIX_CHILD_MAX
        { "_POSIX_CHILD_MAX", 16, _POSIX_CHILD_MAX },
#endif
#ifdef _POSIX_CHOWN_RESTRICTED
        { "_POSIX_CHOWN_RESTRICTED", 23, _POSIX_CHOWN_RESTRICTED },
#endif
#ifdef _POSIX_LINK_MAX
        { "_POSIX_LINK_MAX", 15, _POSIX_LINK_MAX },
#endif
#ifdef _POSIX_MAX_CANON
        { "_POSIX_MAX_CANON", 16, _POSIX_MAX_CANON },
#endif
#ifdef _POSIX_MAX_INPUT
        { "_POSIX_MAX_INPUT", 16, _POSIX_MAX_INPUT },
#endif
#ifdef _POSIX_NAME_MAX
        { "_POSIX_NAME_MAX", 15, _POSIX_NAME_MAX },
#endif
#ifdef _POSIX_NGROUPS_MAX
        { "_POSIX_NGROUPS_MAX", 18, _POSIX_NGROUPS_MAX },
#endif
#ifdef _POSIX_NO_TRUNC
        { "_POSIX_NO_TRUNC", 15, _POSIX_NO_TRUNC },
#endif
#ifdef _POSIX_OPEN_MAX
        { "_POSIX_OPEN_MAX", 15, _POSIX_OPEN_MAX },
#endif
#ifdef _POSIX_PATH_MAX
        { "_POSIX_PATH_MAX", 15, _POSIX_PATH_MAX },
#endif
#ifdef _POSIX_PIPE_BUF
        { "_POSIX_PIPE_BUF", 15, _POSIX_PIPE_BUF },
#endif
#ifdef _POSIX_SSIZE_MAX
        { "_POSIX_SSIZE_MAX", 16, _POSIX_SSIZE_MAX },
#endif
#ifdef _POSIX_STREAM_MAX
        { "_POSIX_STREAM_MAX", 17, _POSIX_STREAM_MAX },
#endif
#ifdef _POSIX_TZNAME_MAX
        { "_POSIX_TZNAME_MAX", 17, _POSIX_TZNAME_MAX },
#endif
#ifdef _POSIX_VDISABLE
        { "_POSIX_VDISABLE", 15, _POSIX_VDISABLE },
#endif
#ifdef _POSIX_VERSION
        { "_POSIX_VERSION", 14, _POSIX_VERSION },
#endif
#ifndef EXIT_FAILURE
        /* This is the default value: */
        { "EXIT_FAILURE", 12, 1 },
#endif
#ifndef EXIT_SUCCESS
        /* This is the default value: */
        { "EXIT_SUCCESS", 12, 0 },
#endif
#ifndef _POSIX_JOB_CONTROL
        /* This is the default value: */
        { "_POSIX_JOB_CONTROL", 18, 0 },
#endif
#ifndef _POSIX_SAVED_IDS
        /* This is the default value: */
        { "_POSIX_SAVED_IDS", 16, 0 },
#endif
#ifndef _POSIX_ARG_MAX
        /* This is the default value: */
        { "_POSIX_ARG_MAX", 14, 0 },
#endif
#ifndef _POSIX_CHILD_MAX
        /* This is the default value: */
        { "_POSIX_CHILD_MAX", 16, 0 },
#endif
#ifndef _POSIX_CHOWN_RESTRICTED
        /* This is the default value: */
        { "_POSIX_CHOWN_RESTRICTED", 23, 0 },
#endif
#ifndef _POSIX_LINK_MAX
        /* This is the default value: */
        { "_POSIX_LINK_MAX", 15, 0 },
#endif
#ifndef _POSIX_MAX_CANON
        /* This is the default value: */
        { "_POSIX_MAX_CANON", 16, 0 },
#endif
#ifndef _POSIX_MAX_INPUT
        /* This is the default value: */
        { "_POSIX_MAX_INPUT", 16, 0 },
#endif
#ifndef _POSIX_NAME_MAX
        /* This is the default value: */
        { "_POSIX_NAME_MAX", 15, 0 },
#endif
#ifndef _POSIX_NGROUPS_MAX
        /* This is the default value: */
        { "_POSIX_NGROUPS_MAX", 18, 0 },
#endif
#ifndef _POSIX_NO_TRUNC
        /* This is the default value: */
        { "_POSIX_NO_TRUNC", 15, 0 },
#endif
#ifndef _POSIX_OPEN_MAX
        /* This is the default value: */
        { "_POSIX_OPEN_MAX", 15, 0 },
#endif
#ifndef _POSIX_PATH_MAX
        /* This is the default value: */
        { "_POSIX_PATH_MAX", 15, 0 },
#endif
#ifndef _POSIX_PIPE_BUF
        /* This is the default value: */
        { "_POSIX_PIPE_BUF", 15, 0 },
#endif
#ifndef _POSIX_SSIZE_MAX
        /* This is the default value: */
        { "_POSIX_SSIZE_MAX", 16, 0 },
#endif
#ifndef _POSIX_STREAM_MAX
        /* This is the default value: */
        { "_POSIX_STREAM_MAX", 17, 0 },
#endif
#ifndef _POSIX_TZNAME_MAX
        /* This is the default value: */
        { "_POSIX_TZNAME_MAX", 17, 0 },
#endif
#ifndef _POSIX_VDISABLE
        /* This is the default value: */
        { "_POSIX_VDISABLE", 15, 0 },
#endif
#ifndef _POSIX_VERSION
        /* This is the default value: */
        { "_POSIX_VERSION", 14, 0 },
#endif
        { NULL, 0, 0 } };
	const struct iv_s *value_for_iv = values_for_iv;

    static const struct nv_s values_for_nv[] =
      {
#ifdef DBL_DIG
        { "DBL_DIG", 7, DBL_DIG },
#endif
#ifdef DBL_MANT_DIG
        { "DBL_MANT_DIG", 12, DBL_MANT_DIG },
#endif
#ifdef DBL_MAX_10_EXP
        { "DBL_MAX_10_EXP", 14, DBL_MAX_10_EXP },
#endif
#ifdef DBL_MAX_EXP
        { "DBL_MAX_EXP", 11, DBL_MAX_EXP },
#endif
#ifdef DBL_MIN_10_EXP
        { "DBL_MIN_10_EXP", 14, DBL_MIN_10_EXP },
#endif
#ifdef DBL_MIN_EXP
        { "DBL_MIN_EXP", 11, DBL_MIN_EXP },
#endif
#ifdef FLT_DIG
        { "FLT_DIG", 7, FLT_DIG },
#endif
#ifdef FLT_MANT_DIG
        { "FLT_MANT_DIG", 12, FLT_MANT_DIG },
#endif
#ifdef FLT_MAX_10_EXP
        { "FLT_MAX_10_EXP", 14, FLT_MAX_10_EXP },
#endif
#ifdef FLT_MAX_EXP
        { "FLT_MAX_EXP", 11, FLT_MAX_EXP },
#endif
#ifdef FLT_MIN_10_EXP
        { "FLT_MIN_10_EXP", 14, FLT_MIN_10_EXP },
#endif
#ifdef FLT_MIN_EXP
        { "FLT_MIN_EXP", 11, FLT_MIN_EXP },
#endif
#ifdef FLT_RADIX
        { "FLT_RADIX", 9, FLT_RADIX },
#endif
#ifdef LDBL_DIG
        { "LDBL_DIG", 8, LDBL_DIG },
#endif
#ifdef LDBL_MANT_DIG
        { "LDBL_MANT_DIG", 13, LDBL_MANT_DIG },
#endif
#ifdef LDBL_MAX_10_EXP
        { "LDBL_MAX_10_EXP", 15, LDBL_MAX_10_EXP },
#endif
#ifdef LDBL_MAX_EXP
        { "LDBL_MAX_EXP", 12, LDBL_MAX_EXP },
#endif
#ifdef LDBL_MIN_10_EXP
        { "LDBL_MIN_10_EXP", 15, LDBL_MIN_10_EXP },
#endif
#ifdef LDBL_MIN_EXP
        { "LDBL_MIN_EXP", 12, LDBL_MIN_EXP },
#endif
        { NULL, 0, 0 } };
	const struct nv_s *value_for_nv = values_for_nv;

    static const struct uv_s values_for_uv[] =
      {
#ifdef SA_NOCLDSTOP
        { "SA_NOCLDSTOP", 12, SA_NOCLDSTOP },
#endif
#ifdef SA_NOCLDWAIT
        { "SA_NOCLDWAIT", 12, SA_NOCLDWAIT },
#endif
#ifdef SA_NODEFER
        { "SA_NODEFER", 10, SA_NODEFER },
#endif
#ifdef SA_ONSTACK
        { "SA_ONSTACK", 10, SA_ONSTACK },
#endif
#ifdef SA_RESETHAND
        { "SA_RESETHAND", 12, SA_RESETHAND },
#endif
#ifdef SA_RESTART
        { "SA_RESTART", 10, SA_RESTART },
#endif
#ifdef SA_SIGINFO
        { "SA_SIGINFO", 10, SA_SIGINFO },
#endif
#ifdef UCHAR_MAX
        { "UCHAR_MAX", 9, UCHAR_MAX },
#endif
#ifdef UINT_MAX
        { "UINT_MAX", 8, UINT_MAX },
#endif
#ifdef ULONG_MAX
        { "ULONG_MAX", 9, ULONG_MAX },
#endif
#ifdef USHRT_MAX
        { "USHRT_MAX", 9, USHRT_MAX },
#endif
        { NULL, 0, 0 } };
	const struct uv_s *value_for_uv = values_for_uv;

    static const struct yes_s values_for_yes[] =
      {
#ifdef _POSIX_JOB_CONTROL
        { "_POSIX_JOB_CONTROL", 18 },
#endif
#ifdef _POSIX_SAVED_IDS
        { "_POSIX_SAVED_IDS", 16 },
#endif
        { NULL, 0 } };
	const struct yes_s *value_for_yes = values_for_yes;
        while (value_for_iv->name) {
	    constant_add_symbol(aTHX_  symbol_table, value_for_iv->name,
				value_for_iv->namelen, newSViv(value_for_iv->value));
            ++value_for_iv;
	}
        while (value_for_nv->name) {
	    constant_add_symbol(aTHX_  symbol_table, value_for_nv->name,
				value_for_nv->namelen, newSVnv(value_for_nv->value));
            ++value_for_nv;
	}
        while (value_for_uv->name) {
	    constant_add_symbol(aTHX_  symbol_table, value_for_uv->name,
				value_for_uv->namelen, newSVuv(value_for_uv->value));
            ++value_for_uv;
	}
        while (value_for_yes->name) {
	    constant_add_symbol(aTHX_  symbol_table, value_for_yes->name,
				value_for_yes->namelen, &PL_sv_yes);
            ++value_for_yes;
	}
	if (C_ARRAY_LENGTH(values_for_notfound) > 1) {
#ifndef SYMBIAN
	    HV *const constant_missing = get_missing_hash(aTHX);
#endif
	    const struct notfound_s *value_for_notfound = values_for_notfound;
	    do {

		/* Need to add prototypes, else parsing will vary by platform.  */
		HE *he = (HE*) hv_common_key_len(symbol_table,
						 value_for_notfound->name,
						 value_for_notfound->namelen,
						 HV_FETCH_LVALUE, NULL, 0);
		SV *sv;
#ifndef SYMBIAN
		HEK *hek;
#endif
		if (!he) {
		    Perl_croak(aTHX_ 
			       "Couldn't add key '%s' to %%POSIX::",
			       value_for_notfound->name);
		}
		sv = HeVAL(he);
		if (!SvOK(sv) && SvTYPE(sv) != SVt_PVGV) {
		    /* Nothing was here before, so mark a prototype of ""  */
		    sv_setpvn(sv, "", 0);
		} else if (SvPOK(sv) && SvCUR(sv) == 0) {
		    /* There is already a prototype of "" - do nothing  */
		} else {
		    /* Someone has been here before us - have to make a real
		       typeglob.  */
		    /* It turns out to be incredibly hard to deal with all the
		       corner cases of sub foo (); and reporting errors correctly,
		       so lets cheat a bit.  Start with a constant subroutine  */
		    CV *cv = newCONSTSUB(symbol_table,
					 value_for_notfound->name,
					 &PL_sv_yes);
		    /* and then turn it into a non constant declaration only.  */
		    SvREFCNT_dec(CvXSUBANY(cv).any_ptr);
		    CvCONST_off(cv);
		    CvXSUB(cv) = NULL;
		    CvXSUBANY(cv).any_ptr = NULL;
		}
#ifndef SYMBIAN
		hek = HeKEY_hek(he);
		if (!hv_common(constant_missing, NULL, HEK_KEY(hek),
 			       HEK_LEN(hek), HEK_FLAGS(hek), HV_FETCH_ISSTORE,
			       &PL_sv_yes, HEK_HASH(hek)))
		    Perl_croak(aTHX_  "Couldn't add key '%s' to missing_hash",
			       value_for_notfound->name);
#endif
	    } while ((++value_for_notfound)->name);
	}
#ifdef CLK_TCK
        {
            IV temp0;
            temp0 = CLK_TCK;
	    constant_add_symbol(aTHX_  symbol_table, "CLK_TCK",
				    7, newSViv(temp0));
        }
#endif
#ifdef MB_CUR_MAX
        {
            IV temp0;
            temp0 = MB_CUR_MAX;
	    constant_add_symbol(aTHX_  symbol_table, "MB_CUR_MAX",
				    10, newSViv(temp0));
        }
#endif
#ifdef SIG_DFL
        {
            IV temp0;
            temp0 = PTR2IV(SIG_DFL);
	    constant_add_symbol(aTHX_  symbol_table, "SIG_DFL",
				    7, newSViv(temp0));
        }
#endif
#ifdef SIG_ERR
        {
            IV temp0;
            temp0 = PTR2IV(SIG_ERR);
	    constant_add_symbol(aTHX_  symbol_table, "SIG_ERR",
				    7, newSViv(temp0));
        }
#endif
#ifdef SIG_IGN
        {
            IV temp0;
            temp0 = PTR2IV(SIG_IGN);
	    constant_add_symbol(aTHX_  symbol_table, "SIG_IGN",
				    7, newSViv(temp0));
        }
#endif
#ifdef FLT_ROUNDS
        {
            NV temp0;
            temp0 = FLT_ROUNDS;
	    constant_add_symbol(aTHX_  symbol_table, "FLT_ROUNDS",
				    10, newSVnv(temp0));
        }
#endif
#if (defined(USE_LONG_DOUBLE) && defined(HUGE_VALL)) || defined(HUGE_VAL)
	/* HUGE_VALL is admittedly non-POSIX but if we are using long doubles
	 * we might as well use long doubles. --jhi */
        {
            NV temp0;
            temp0 = 
#if defined(USE_LONG_DOUBLE) && defined(HUGE_VALL)
                   HUGE_VALL
#else
                   HUGE_VAL
#endif
                           ;
	    constant_add_symbol(aTHX_  symbol_table, "HUGE_VAL",
				    8, newSVnv(temp0));
        }
#endif
#ifdef DBL_MAX
        {
            NV temp0;
            temp0 = DBL_MAX;
	    constant_add_symbol(aTHX_  symbol_table, "DBL_MAX",
				    7, newSVnv(temp0));
        }
#endif
#ifdef FLT_MAX
        {
            NV temp0;
            temp0 = FLT_MAX;
	    constant_add_symbol(aTHX_  symbol_table, "FLT_MAX",
				    7, newSVnv(temp0));
        }
#endif
#ifdef LDBL_MAX
        {
            NV temp0;
            temp0 = LDBL_MAX;
	    constant_add_symbol(aTHX_  symbol_table, "LDBL_MAX",
				    8, newSVnv(temp0));
        }
#endif
#ifdef LDBL_MIN
        {
            NV temp0;
            temp0 = LDBL_MIN;
	    constant_add_symbol(aTHX_  symbol_table, "LDBL_MIN",
				    8, newSVnv(temp0));
        }
#endif
#ifdef LDBL_EPSILON
        {
            NV temp0;
            temp0 = LDBL_EPSILON;
	    constant_add_symbol(aTHX_  symbol_table, "LDBL_EPSILON",
				    12, newSVnv(temp0));
        }
#endif
#ifdef DBL_EPSILON
        {
            NV temp0;
            temp0 = DBL_EPSILON;
	    constant_add_symbol(aTHX_  symbol_table, "DBL_EPSILON",
				    11, newSVnv(temp0));
        }
#endif
#ifdef DBL_MIN
        {
            NV temp0;
            temp0 = DBL_MIN;
	    constant_add_symbol(aTHX_  symbol_table, "DBL_MIN",
				    7, newSVnv(temp0));
        }
#endif
#ifdef FLT_EPSILON
        {
            NV temp0;
            temp0 = FLT_EPSILON;
	    constant_add_symbol(aTHX_  symbol_table, "FLT_EPSILON",
				    11, newSVnv(temp0));
        }
#endif
#ifdef FLT_MIN
        {
            NV temp0;
            temp0 = FLT_MIN;
	    constant_add_symbol(aTHX_  symbol_table, "FLT_MIN",
				    7, newSVnv(temp0));
        }
#endif
#ifdef SIGRTMAX
        {
            IV temp0;
            temp0 = SIGRTMAX;
	    constant_add_symbol(aTHX_  symbol_table, "SIGRTMAX",
				    8, newSViv(temp0));
        }
#endif
#ifdef SIGRTMIN
        {
            IV temp0;
            temp0 = SIGRTMIN;
	    constant_add_symbol(aTHX_  symbol_table, "SIGRTMIN",
				    8, newSViv(temp0));
        }
#endif
    /* As we've been creating subroutines, we better invalidate any cached
       methods  */
    mro_method_changed_in(symbol_table);
  }

void
constant(sv)
    INPUT:
	SV *		sv;
    PREINIT:
	const PERL_CONTEXT *cx = caller_cx(0, NULL);
	/* cx is NULL if we've been called from the top level. PL_curcop isn't
	   ideal, but it's much cheaper than other ways of not going SEGV.  */
	const COP *cop = cx ? cx->blk_oldcop : PL_curcop;
    PPCODE:
#ifndef SYMBIAN
	/* It's not obvious how to calculate this at C pre-processor time.
	   However, any compiler optimiser worth its salt should be able to
	   remove the dead code, and hopefully the now-obviously-unused static
	   function too.  */
	HV *constant_missing = (C_ARRAY_LENGTH(values_for_notfound) > 1)
	    ? get_missing_hash(aTHX) : NULL;
	if ((C_ARRAY_LENGTH(values_for_notfound) > 1)
	    ? hv_exists_ent(constant_missing, sv, 0) : 0) {
	    sv = newSVpvf("Your vendor has not defined POSIX macro %" SVf
			  ", used at %" COP_FILE_F " line %d\n", sv,
			  COP_FILE(cop), CopLINE(cop));
	} else
#endif
	{
	    sv = newSVpvf("%"SVf" is not a valid POSIX macro at %"
			  COP_FILE_F " line %d\n", sv, COP_FILE(cop), CopLINE(cop));
	}
	croak_sv(sv_2mortal(sv));
