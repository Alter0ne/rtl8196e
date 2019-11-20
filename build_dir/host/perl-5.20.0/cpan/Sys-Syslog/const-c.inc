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

static void
constant_add_symbol(pTHX_  HV *hash, const char *name, I32 namelen, SV *value) {
    HE *he = (HE*) hv_common_key_len(hash, name, namelen, HV_FETCH_LVALUE, NULL,
				     0);
    SV *sv;

    if (!he) {
        Perl_croak(aTHX_  "Couldn't add key '%s' to %%Sys::Syslog::",
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
	= hv_fetch(parent, "Sys::Syslog", 11, TRUE);
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
#ifndef LOG_ALERT
        { "LOG_ALERT", 9 },
#endif
#ifndef LOG_CRIT
        { "LOG_CRIT", 8 },
#endif
#ifndef LOG_DEBUG
        { "LOG_DEBUG", 9 },
#endif
#ifndef LOG_EMERG
        { "LOG_EMERG", 9 },
#endif
#ifndef LOG_ERR
        { "LOG_ERR", 7 },
#endif
#ifndef LOG_INFO
        { "LOG_INFO", 8 },
#endif
#ifndef LOG_NOTICE
        { "LOG_NOTICE", 10 },
#endif
#ifndef LOG_WARNING
        { "LOG_WARNING", 11 },
#endif
#ifndef LOG_AUTH
        { "LOG_AUTH", 8 },
#endif
#ifndef LOG_AUTHPRIV
        { "LOG_AUTHPRIV", 12 },
#endif
#ifndef LOG_CRON
        { "LOG_CRON", 8 },
#endif
#ifndef LOG_DAEMON
        { "LOG_DAEMON", 10 },
#endif
#ifndef LOG_FTP
        { "LOG_FTP", 7 },
#endif
#ifndef LOG_KERN
        { "LOG_KERN", 8 },
#endif
#ifndef LOG_LOCAL0
        { "LOG_LOCAL0", 10 },
#endif
#ifndef LOG_LOCAL1
        { "LOG_LOCAL1", 10 },
#endif
#ifndef LOG_LOCAL2
        { "LOG_LOCAL2", 10 },
#endif
#ifndef LOG_LOCAL3
        { "LOG_LOCAL3", 10 },
#endif
#ifndef LOG_LOCAL4
        { "LOG_LOCAL4", 10 },
#endif
#ifndef LOG_LOCAL5
        { "LOG_LOCAL5", 10 },
#endif
#ifndef LOG_LOCAL6
        { "LOG_LOCAL6", 10 },
#endif
#ifndef LOG_LOCAL7
        { "LOG_LOCAL7", 10 },
#endif
#ifndef LOG_LPR
        { "LOG_LPR", 7 },
#endif
#ifndef LOG_MAIL
        { "LOG_MAIL", 8 },
#endif
#ifndef LOG_NEWS
        { "LOG_NEWS", 8 },
#endif
#ifndef LOG_SYSLOG
        { "LOG_SYSLOG", 10 },
#endif
#ifndef LOG_USER
        { "LOG_USER", 8 },
#endif
#ifndef LOG_UUCP
        { "LOG_UUCP", 8 },
#endif
#ifndef LOG_CONS
        { "LOG_CONS", 8 },
#endif
#ifndef LOG_PID
        { "LOG_PID", 7 },
#endif
#ifndef LOG_NDELAY
        { "LOG_NDELAY", 10 },
#endif
#ifndef LOG_NOWAIT
        { "LOG_NOWAIT", 10 },
#endif
#ifndef LOG_ODELAY
        { "LOG_ODELAY", 10 },
#endif
#ifndef LOG_PERROR
        { "LOG_PERROR", 10 },
#endif
#ifndef LOG_FACMASK
        { "LOG_FACMASK", 11 },
#endif
        { NULL, 0 } };
struct iv_s {const char *name; I32 namelen; IV value;};
struct pv_s {const char *name; I32 namelen; const char *value;};
