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
        Perl_croak(aTHX_  "Couldn't add key '%s' to %%Fcntl::",
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
	= hv_fetch(parent, "Fcntl", 5, TRUE);
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
#ifndef DN_ACCESS
        { "DN_ACCESS", 9 },
#endif
#ifndef DN_MODIFY
        { "DN_MODIFY", 9 },
#endif
#ifndef DN_CREATE
        { "DN_CREATE", 9 },
#endif
#ifndef DN_DELETE
        { "DN_DELETE", 9 },
#endif
#ifndef DN_RENAME
        { "DN_RENAME", 9 },
#endif
#ifndef DN_ATTRIB
        { "DN_ATTRIB", 9 },
#endif
#ifndef DN_MULTISHOT
        { "DN_MULTISHOT", 12 },
#endif
#ifndef FAPPEND
        { "FAPPEND", 7 },
#endif
#ifndef FASYNC
        { "FASYNC", 6 },
#endif
#ifndef FCREAT
        { "FCREAT", 6 },
#endif
#ifndef FDEFER
        { "FDEFER", 6 },
#endif
#ifndef FDSYNC
        { "FDSYNC", 6 },
#endif
#ifndef FD_CLOEXEC
        { "FD_CLOEXEC", 10 },
#endif
#ifndef FEXCL
        { "FEXCL", 5 },
#endif
#ifndef FLARGEFILE
        { "FLARGEFILE", 10 },
#endif
#ifndef FNDELAY
        { "FNDELAY", 7 },
#endif
#ifndef FNONBLOCK
        { "FNONBLOCK", 9 },
#endif
#ifndef FRSYNC
        { "FRSYNC", 6 },
#endif
#ifndef FSYNC
        { "FSYNC", 5 },
#endif
#ifndef FTRUNC
        { "FTRUNC", 6 },
#endif
#ifndef F_ALLOCSP
        { "F_ALLOCSP", 9 },
#endif
#ifndef F_ALLOCSP64
        { "F_ALLOCSP64", 11 },
#endif
#ifndef F_COMPAT
        { "F_COMPAT", 8 },
#endif
#ifndef F_DUP2FD
        { "F_DUP2FD", 8 },
#endif
#ifndef F_DUPFD
        { "F_DUPFD", 7 },
#endif
#ifndef F_EXLCK
        { "F_EXLCK", 7 },
#endif
#ifndef F_FREESP
        { "F_FREESP", 8 },
#endif
#ifndef F_FREESP64
        { "F_FREESP64", 10 },
#endif
#ifndef F_FSYNC
        { "F_FSYNC", 7 },
#endif
#ifndef F_FSYNC64
        { "F_FSYNC64", 9 },
#endif
#ifndef F_GETFD
        { "F_GETFD", 7 },
#endif
#ifndef F_GETFL
        { "F_GETFL", 7 },
#endif
#ifndef F_GETLEASE
        { "F_GETLEASE", 10 },
#endif
#ifndef F_GETLK
        { "F_GETLK", 7 },
#endif
#ifndef F_GETLK64
        { "F_GETLK64", 9 },
#endif
#ifndef F_GETOWN
        { "F_GETOWN", 8 },
#endif
#ifndef F_GETSIG
        { "F_GETSIG", 8 },
#endif
#ifndef F_NODNY
        { "F_NODNY", 7 },
#endif
#ifndef F_NOTIFY
        { "F_NOTIFY", 8 },
#endif
#ifndef F_POSIX
        { "F_POSIX", 7 },
#endif
#ifndef F_RDACC
        { "F_RDACC", 7 },
#endif
#ifndef F_RDDNY
        { "F_RDDNY", 7 },
#endif
#ifndef F_RDLCK
        { "F_RDLCK", 7 },
#endif
#ifndef F_RWACC
        { "F_RWACC", 7 },
#endif
#ifndef F_RWDNY
        { "F_RWDNY", 7 },
#endif
#ifndef F_SETFD
        { "F_SETFD", 7 },
#endif
#ifndef F_SETFL
        { "F_SETFL", 7 },
#endif
#ifndef F_SETLEASE
        { "F_SETLEASE", 10 },
#endif
#ifndef F_SETLK
        { "F_SETLK", 7 },
#endif
#ifndef F_SETLK64
        { "F_SETLK64", 9 },
#endif
#ifndef F_SETLKW
        { "F_SETLKW", 8 },
#endif
#ifndef F_SETLKW64
        { "F_SETLKW64", 10 },
#endif
#ifndef F_SETOWN
        { "F_SETOWN", 8 },
#endif
#ifndef F_SETSIG
        { "F_SETSIG", 8 },
#endif
#ifndef F_SHARE
        { "F_SHARE", 7 },
#endif
#ifndef F_SHLCK
        { "F_SHLCK", 7 },
#endif
#ifndef F_UNLCK
        { "F_UNLCK", 7 },
#endif
#ifndef F_UNSHARE
        { "F_UNSHARE", 9 },
#endif
#ifndef F_WRACC
        { "F_WRACC", 7 },
#endif
#ifndef F_WRDNY
        { "F_WRDNY", 7 },
#endif
#ifndef F_WRLCK
        { "F_WRLCK", 7 },
#endif
#ifndef LOCK_MAND
        { "LOCK_MAND", 9 },
#endif
#ifndef LOCK_READ
        { "LOCK_READ", 9 },
#endif
#ifndef LOCK_WRITE
        { "LOCK_WRITE", 10 },
#endif
#ifndef LOCK_RW
        { "LOCK_RW", 7 },
#endif
#ifndef O_ACCMODE
        { "O_ACCMODE", 9 },
#endif
#ifndef O_ALIAS
        { "O_ALIAS", 7 },
#endif
#ifndef O_APPEND
        { "O_APPEND", 8 },
#endif
#ifndef O_ASYNC
        { "O_ASYNC", 7 },
#endif
#ifndef O_BINARY
        { "O_BINARY", 8 },
#endif
#ifndef O_CREAT
        { "O_CREAT", 7 },
#endif
#ifndef O_DEFER
        { "O_DEFER", 7 },
#endif
#ifndef O_DIRECT
        { "O_DIRECT", 8 },
#endif
#ifndef O_DIRECTORY
        { "O_DIRECTORY", 11 },
#endif
#ifndef O_DSYNC
        { "O_DSYNC", 7 },
#endif
#ifndef O_EXCL
        { "O_EXCL", 6 },
#endif
#ifndef O_EXLOCK
        { "O_EXLOCK", 8 },
#endif
#ifndef O_IGNORE_CTTY
        { "O_IGNORE_CTTY", 13 },
#endif
#ifndef O_LARGEFILE
        { "O_LARGEFILE", 11 },
#endif
#ifndef O_NDELAY
        { "O_NDELAY", 8 },
#endif
#ifndef O_NOATIME
        { "O_NOATIME", 9 },
#endif
#ifndef O_NOCTTY
        { "O_NOCTTY", 8 },
#endif
#ifndef O_NOFOLLOW
        { "O_NOFOLLOW", 10 },
#endif
#ifndef O_NOINHERIT
        { "O_NOINHERIT", 11 },
#endif
#ifndef O_NOLINK
        { "O_NOLINK", 8 },
#endif
#ifndef O_NONBLOCK
        { "O_NONBLOCK", 10 },
#endif
#ifndef O_NOTRANS
        { "O_NOTRANS", 9 },
#endif
#ifndef O_RANDOM
        { "O_RANDOM", 8 },
#endif
#ifndef O_RAW
        { "O_RAW", 5 },
#endif
#ifndef O_RDONLY
        { "O_RDONLY", 8 },
#endif
#ifndef O_RDWR
        { "O_RDWR", 6 },
#endif
#ifndef O_RSRC
        { "O_RSRC", 6 },
#endif
#ifndef O_RSYNC
        { "O_RSYNC", 7 },
#endif
#ifndef O_SEQUENTIAL
        { "O_SEQUENTIAL", 12 },
#endif
#ifndef O_SHLOCK
        { "O_SHLOCK", 8 },
#endif
#ifndef O_SYNC
        { "O_SYNC", 6 },
#endif
#ifndef O_TEMPORARY
        { "O_TEMPORARY", 11 },
#endif
#ifndef O_TEXT
        { "O_TEXT", 6 },
#endif
#ifndef O_TRUNC
        { "O_TRUNC", 7 },
#endif
#ifndef O_WRONLY
        { "O_WRONLY", 8 },
#endif
#ifndef S_ENFMT
        { "S_ENFMT", 7 },
#endif
#ifndef S_IEXEC
        { "S_IEXEC", 7 },
#endif
#ifndef S_IFBLK
        { "S_IFBLK", 7 },
#endif
#ifndef S_IFCHR
        { "S_IFCHR", 7 },
#endif
#ifndef S_IFDIR
        { "S_IFDIR", 7 },
#endif
#ifndef S_IFIFO
        { "S_IFIFO", 7 },
#endif
#ifndef S_IFLNK
        { "S_IFLNK", 7 },
#endif
#ifndef S_IFREG
        { "S_IFREG", 7 },
#endif
#ifndef S_IFSOCK
        { "S_IFSOCK", 8 },
#endif
#ifndef S_IFWHT
        { "S_IFWHT", 7 },
#endif
#ifndef S_IREAD
        { "S_IREAD", 7 },
#endif
#ifndef S_IRGRP
        { "S_IRGRP", 7 },
#endif
#ifndef S_IROTH
        { "S_IROTH", 7 },
#endif
#ifndef S_IRUSR
        { "S_IRUSR", 7 },
#endif
#ifndef S_IRWXG
        { "S_IRWXG", 7 },
#endif
#ifndef S_IRWXO
        { "S_IRWXO", 7 },
#endif
#ifndef S_IRWXU
        { "S_IRWXU", 7 },
#endif
#ifndef S_ISGID
        { "S_ISGID", 7 },
#endif
#ifndef S_ISTXT
        { "S_ISTXT", 7 },
#endif
#ifndef S_ISUID
        { "S_ISUID", 7 },
#endif
#ifndef S_ISVTX
        { "S_ISVTX", 7 },
#endif
#ifndef S_IWGRP
        { "S_IWGRP", 7 },
#endif
#ifndef S_IWOTH
        { "S_IWOTH", 7 },
#endif
#ifndef S_IWRITE
        { "S_IWRITE", 8 },
#endif
#ifndef S_IWUSR
        { "S_IWUSR", 7 },
#endif
#ifndef S_IXGRP
        { "S_IXGRP", 7 },
#endif
#ifndef S_IXOTH
        { "S_IXOTH", 7 },
#endif
#ifndef S_IXUSR
        { "S_IXUSR", 7 },
#endif
#ifndef S_IFMT
        { "_S_IFMT", 7 },
#endif
        { NULL, 0 } };
struct iv_s {const char *name; I32 namelen; IV value;};
