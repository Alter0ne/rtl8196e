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
        Perl_croak(aTHX_  "Couldn't add key '%s' to %%I18N::Langinfo::",
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
	= hv_fetch(parent, "I18N::Langinfo", 14, TRUE);
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
#ifndef CODESET
        { "CODESET", 7 },
#endif
#if defined(ABDAY_1) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABDAY_1", 7 },
#endif
#if defined(ABDAY_2) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABDAY_2", 7 },
#endif
#if defined(ABDAY_3) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABDAY_3", 7 },
#endif
#if defined(ABDAY_4) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABDAY_4", 7 },
#endif
#if defined(ABDAY_5) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABDAY_5", 7 },
#endif
#if defined(ABDAY_6) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABDAY_6", 7 },
#endif
#if defined(ABDAY_7) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABDAY_7", 7 },
#endif
#if defined(ABMON_1) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABMON_1", 7 },
#endif
#if defined(ABMON_10) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABMON_10", 8 },
#endif
#if defined(ABMON_11) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABMON_11", 8 },
#endif
#if defined(ABMON_12) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABMON_12", 8 },
#endif
#if defined(ABMON_2) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABMON_2", 7 },
#endif
#if defined(ABMON_3) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABMON_3", 7 },
#endif
#if defined(ABMON_4) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABMON_4", 7 },
#endif
#if defined(ABMON_5) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABMON_5", 7 },
#endif
#if defined(ABMON_6) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABMON_6", 7 },
#endif
#if defined(ABMON_7) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABMON_7", 7 },
#endif
#if defined(ABMON_8) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABMON_8", 7 },
#endif
#if defined(ABMON_9) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ABMON_9", 7 },
#endif
#if defined(ALT_DIGITS) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ALT_DIGITS", 10 },
#endif
#if defined(AM_STR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "AM_STR", 6 },
#endif
#if defined(DAY_1) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "DAY_1", 5 },
#endif
#if defined(DAY_2) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "DAY_2", 5 },
#endif
#if defined(DAY_3) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "DAY_3", 5 },
#endif
#if defined(DAY_4) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "DAY_4", 5 },
#endif
#if defined(DAY_5) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "DAY_5", 5 },
#endif
#if defined(DAY_6) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "DAY_6", 5 },
#endif
#if defined(DAY_7) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "DAY_7", 5 },
#endif
#if defined(D_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "D_FMT", 5 },
#endif
#if defined(D_T_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "D_T_FMT", 7 },
#endif
#if defined(ERA) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ERA", 3 },
#endif
#if defined(ERA_D_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ERA_D_FMT", 9 },
#endif
#if defined(ERA_D_T_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ERA_D_T_FMT", 11 },
#endif
#if defined(ERA_T_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "ERA_T_FMT", 9 },
#endif
#if defined(MON_1) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "MON_1", 5 },
#endif
#if defined(MON_10) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "MON_10", 6 },
#endif
#if defined(MON_11) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "MON_11", 6 },
#endif
#if defined(MON_12) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "MON_12", 6 },
#endif
#if defined(MON_2) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "MON_2", 5 },
#endif
#if defined(MON_3) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "MON_3", 5 },
#endif
#if defined(MON_4) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "MON_4", 5 },
#endif
#if defined(MON_5) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "MON_5", 5 },
#endif
#if defined(MON_6) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "MON_6", 5 },
#endif
#if defined(MON_7) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "MON_7", 5 },
#endif
#if defined(MON_8) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "MON_8", 5 },
#endif
#if defined(MON_9) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "MON_9", 5 },
#endif
#if defined(NOEXPR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "NOEXPR", 6 },
#endif
#if defined(NOSTR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "NOSTR", 5 },
#endif
#if defined(PM_STR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "PM_STR", 6 },
#endif
#if defined(T_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "T_FMT", 5 },
#endif
#if defined(T_FMT_AMPM) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "T_FMT_AMPM", 10 },
#endif
#if defined(YESEXPR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "YESEXPR", 7 },
#endif
#if defined(YESSTR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
#else
        { "YESSTR", 6 },
#endif
#if defined(CRNCYSTR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM) && defined(__SVR4_I386_ABI_L1__))
#else
        { "CRNCYSTR", 8 },
#endif
#if defined(THOUSEP) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM) && defined(__SVR4_I386_ABI_L1__))
#else
        { "THOUSEP", 7 },
#endif
#if defined(RADIXCHAR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM) && defined(__SVR4_I386_ABI_L1__))
#else
        { "RADIXCHAR", 9 },
#endif
        { NULL, 0 } };
struct iv_s {const char *name; I32 namelen; IV value;};
