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
        Perl_croak(aTHX_  "Couldn't add key '%s' to %%File::Glob::",
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
	= hv_fetch(parent, "File::Glob", 10, TRUE);
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
#ifndef GLOB_ABEND
        { "GLOB_ABEND", 10 },
#endif
#ifndef GLOB_ALPHASORT
        { "GLOB_ALPHASORT", 14 },
#endif
#ifndef GLOB_ALTDIRFUNC
        { "GLOB_ALTDIRFUNC", 15 },
#endif
#ifndef GLOB_BRACE
        { "GLOB_BRACE", 10 },
#endif
#ifndef GLOB_ERR
        { "GLOB_ERR", 8 },
#endif
#ifndef GLOB_LIMIT
        { "GLOB_LIMIT", 10 },
#endif
#ifndef GLOB_MARK
        { "GLOB_MARK", 9 },
#endif
#ifndef GLOB_NOCASE
        { "GLOB_NOCASE", 11 },
#endif
#ifndef GLOB_NOCHECK
        { "GLOB_NOCHECK", 12 },
#endif
#ifndef GLOB_NOMAGIC
        { "GLOB_NOMAGIC", 12 },
#endif
#ifndef GLOB_NOSORT
        { "GLOB_NOSORT", 11 },
#endif
#ifndef GLOB_NOSPACE
        { "GLOB_NOSPACE", 12 },
#endif
#ifndef GLOB_QUOTE
        { "GLOB_QUOTE", 10 },
#endif
#ifndef GLOB_TILDE
        { "GLOB_TILDE", 10 },
#endif
        { NULL, 0 } };
struct iv_s {const char *name; I32 namelen; IV value;};
