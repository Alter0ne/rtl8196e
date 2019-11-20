BOOT:
  {
#ifdef dTHX
    dTHX;
#endif
    HV *symbol_table = get_hv("Fcntl::", GV_ADD);

    static const struct iv_s values_for_iv[] =
      {
#ifdef DN_ACCESS
        { "DN_ACCESS", 9, DN_ACCESS },
#endif
#ifdef DN_MODIFY
        { "DN_MODIFY", 9, DN_MODIFY },
#endif
#ifdef DN_CREATE
        { "DN_CREATE", 9, DN_CREATE },
#endif
#ifdef DN_DELETE
        { "DN_DELETE", 9, DN_DELETE },
#endif
#ifdef DN_RENAME
        { "DN_RENAME", 9, DN_RENAME },
#endif
#ifdef DN_ATTRIB
        { "DN_ATTRIB", 9, DN_ATTRIB },
#endif
#ifdef DN_MULTISHOT
        { "DN_MULTISHOT", 12, DN_MULTISHOT },
#endif
#ifdef FAPPEND
        { "FAPPEND", 7, FAPPEND },
#endif
#ifdef FASYNC
        { "FASYNC", 6, FASYNC },
#endif
#ifdef FCREAT
        { "FCREAT", 6, FCREAT },
#endif
#ifdef FDEFER
        { "FDEFER", 6, FDEFER },
#endif
#ifdef FDSYNC
        { "FDSYNC", 6, FDSYNC },
#endif
#ifdef FD_CLOEXEC
        { "FD_CLOEXEC", 10, FD_CLOEXEC },
#endif
#ifdef FEXCL
        { "FEXCL", 5, FEXCL },
#endif
#ifdef FLARGEFILE
        { "FLARGEFILE", 10, FLARGEFILE },
#endif
#ifdef FNDELAY
        { "FNDELAY", 7, FNDELAY },
#endif
#ifdef FNONBLOCK
        { "FNONBLOCK", 9, FNONBLOCK },
#endif
#ifdef FRSYNC
        { "FRSYNC", 6, FRSYNC },
#endif
#ifdef FSYNC
        { "FSYNC", 5, FSYNC },
#endif
#ifdef FTRUNC
        { "FTRUNC", 6, FTRUNC },
#endif
#ifdef F_ALLOCSP
        { "F_ALLOCSP", 9, F_ALLOCSP },
#endif
#ifdef F_ALLOCSP64
        { "F_ALLOCSP64", 11, F_ALLOCSP64 },
#endif
#ifdef F_COMPAT
        { "F_COMPAT", 8, F_COMPAT },
#endif
#ifdef F_DUP2FD
        { "F_DUP2FD", 8, F_DUP2FD },
#endif
#ifdef F_DUPFD
        { "F_DUPFD", 7, F_DUPFD },
#endif
#ifdef F_EXLCK
        { "F_EXLCK", 7, F_EXLCK },
#endif
#ifdef F_FREESP
        { "F_FREESP", 8, F_FREESP },
#endif
#ifdef F_FREESP64
        { "F_FREESP64", 10, F_FREESP64 },
#endif
#ifdef F_FSYNC
        { "F_FSYNC", 7, F_FSYNC },
#endif
#ifdef F_FSYNC64
        { "F_FSYNC64", 9, F_FSYNC64 },
#endif
#ifdef F_GETFD
        { "F_GETFD", 7, F_GETFD },
#endif
#ifdef F_GETFL
        { "F_GETFL", 7, F_GETFL },
#endif
#ifdef F_GETLEASE
        { "F_GETLEASE", 10, F_GETLEASE },
#endif
#ifdef F_GETLK
        { "F_GETLK", 7, F_GETLK },
#endif
#ifdef F_GETLK64
        { "F_GETLK64", 9, F_GETLK64 },
#endif
#ifdef F_GETOWN
        { "F_GETOWN", 8, F_GETOWN },
#endif
#ifdef F_GETSIG
        { "F_GETSIG", 8, F_GETSIG },
#endif
#ifdef F_NODNY
        { "F_NODNY", 7, F_NODNY },
#endif
#ifdef F_NOTIFY
        { "F_NOTIFY", 8, F_NOTIFY },
#endif
#ifdef F_POSIX
        { "F_POSIX", 7, F_POSIX },
#endif
#ifdef F_RDACC
        { "F_RDACC", 7, F_RDACC },
#endif
#ifdef F_RDDNY
        { "F_RDDNY", 7, F_RDDNY },
#endif
#ifdef F_RDLCK
        { "F_RDLCK", 7, F_RDLCK },
#endif
#ifdef F_RWACC
        { "F_RWACC", 7, F_RWACC },
#endif
#ifdef F_RWDNY
        { "F_RWDNY", 7, F_RWDNY },
#endif
#ifdef F_SETFD
        { "F_SETFD", 7, F_SETFD },
#endif
#ifdef F_SETFL
        { "F_SETFL", 7, F_SETFL },
#endif
#ifdef F_SETLEASE
        { "F_SETLEASE", 10, F_SETLEASE },
#endif
#ifdef F_SETLK
        { "F_SETLK", 7, F_SETLK },
#endif
#ifdef F_SETLK64
        { "F_SETLK64", 9, F_SETLK64 },
#endif
#ifdef F_SETLKW
        { "F_SETLKW", 8, F_SETLKW },
#endif
#ifdef F_SETLKW64
        { "F_SETLKW64", 10, F_SETLKW64 },
#endif
#ifdef F_SETOWN
        { "F_SETOWN", 8, F_SETOWN },
#endif
#ifdef F_SETSIG
        { "F_SETSIG", 8, F_SETSIG },
#endif
#ifdef F_SHARE
        { "F_SHARE", 7, F_SHARE },
#endif
#ifdef F_SHLCK
        { "F_SHLCK", 7, F_SHLCK },
#endif
#ifdef F_UNLCK
        { "F_UNLCK", 7, F_UNLCK },
#endif
#ifdef F_UNSHARE
        { "F_UNSHARE", 9, F_UNSHARE },
#endif
#ifdef F_WRACC
        { "F_WRACC", 7, F_WRACC },
#endif
#ifdef F_WRDNY
        { "F_WRDNY", 7, F_WRDNY },
#endif
#ifdef F_WRLCK
        { "F_WRLCK", 7, F_WRLCK },
#endif
#ifdef LOCK_MAND
        { "LOCK_MAND", 9, LOCK_MAND },
#endif
#ifdef LOCK_READ
        { "LOCK_READ", 9, LOCK_READ },
#endif
#ifdef LOCK_WRITE
        { "LOCK_WRITE", 10, LOCK_WRITE },
#endif
#ifdef LOCK_RW
        { "LOCK_RW", 7, LOCK_RW },
#endif
#ifdef O_ACCMODE
        { "O_ACCMODE", 9, O_ACCMODE },
#endif
#ifdef O_ALIAS
        { "O_ALIAS", 7, O_ALIAS },
#endif
#ifdef O_APPEND
        { "O_APPEND", 8, O_APPEND },
#endif
#ifdef O_ASYNC
        { "O_ASYNC", 7, O_ASYNC },
#endif
#ifdef O_BINARY
        { "O_BINARY", 8, O_BINARY },
#endif
#ifdef O_CREAT
        { "O_CREAT", 7, O_CREAT },
#endif
#ifdef O_DEFER
        { "O_DEFER", 7, O_DEFER },
#endif
#ifdef O_DIRECT
        { "O_DIRECT", 8, O_DIRECT },
#endif
#ifdef O_DIRECTORY
        { "O_DIRECTORY", 11, O_DIRECTORY },
#endif
#ifdef O_DSYNC
        { "O_DSYNC", 7, O_DSYNC },
#endif
#ifdef O_EXCL
        { "O_EXCL", 6, O_EXCL },
#endif
#ifdef O_EXLOCK
        { "O_EXLOCK", 8, O_EXLOCK },
#endif
#ifdef O_IGNORE_CTTY
        { "O_IGNORE_CTTY", 13, O_IGNORE_CTTY },
#endif
#ifdef O_LARGEFILE
        { "O_LARGEFILE", 11, O_LARGEFILE },
#endif
#ifdef O_NDELAY
        { "O_NDELAY", 8, O_NDELAY },
#endif
#ifdef O_NOATIME
        { "O_NOATIME", 9, O_NOATIME },
#endif
#ifdef O_NOCTTY
        { "O_NOCTTY", 8, O_NOCTTY },
#endif
#ifdef O_NOFOLLOW
        { "O_NOFOLLOW", 10, O_NOFOLLOW },
#endif
#ifdef O_NOINHERIT
        { "O_NOINHERIT", 11, O_NOINHERIT },
#endif
#ifdef O_NOLINK
        { "O_NOLINK", 8, O_NOLINK },
#endif
#ifdef O_NONBLOCK
        { "O_NONBLOCK", 10, O_NONBLOCK },
#endif
#ifdef O_NOTRANS
        { "O_NOTRANS", 9, O_NOTRANS },
#endif
#ifdef O_RANDOM
        { "O_RANDOM", 8, O_RANDOM },
#endif
#ifdef O_RAW
        { "O_RAW", 5, O_RAW },
#endif
#ifdef O_RDONLY
        { "O_RDONLY", 8, O_RDONLY },
#endif
#ifdef O_RDWR
        { "O_RDWR", 6, O_RDWR },
#endif
#ifdef O_RSRC
        { "O_RSRC", 6, O_RSRC },
#endif
#ifdef O_RSYNC
        { "O_RSYNC", 7, O_RSYNC },
#endif
#ifdef O_SEQUENTIAL
        { "O_SEQUENTIAL", 12, O_SEQUENTIAL },
#endif
#ifdef O_SHLOCK
        { "O_SHLOCK", 8, O_SHLOCK },
#endif
#ifdef O_SYNC
        { "O_SYNC", 6, O_SYNC },
#endif
#ifdef O_TEMPORARY
        { "O_TEMPORARY", 11, O_TEMPORARY },
#endif
#ifdef O_TEXT
        { "O_TEXT", 6, O_TEXT },
#endif
#ifdef O_TRUNC
        { "O_TRUNC", 7, O_TRUNC },
#endif
#ifdef O_WRONLY
        { "O_WRONLY", 8, O_WRONLY },
#endif
#ifdef S_ENFMT
        { "S_ENFMT", 7, S_ENFMT },
#endif
#ifdef S_IEXEC
        { "S_IEXEC", 7, S_IEXEC },
#endif
#ifdef S_IFBLK
        { "S_IFBLK", 7, S_IFBLK },
#endif
#ifdef S_IFCHR
        { "S_IFCHR", 7, S_IFCHR },
#endif
#ifdef S_IFDIR
        { "S_IFDIR", 7, S_IFDIR },
#endif
#ifdef S_IFIFO
        { "S_IFIFO", 7, S_IFIFO },
#endif
#ifdef S_IFLNK
        { "S_IFLNK", 7, S_IFLNK },
#endif
#ifdef S_IFREG
        { "S_IFREG", 7, S_IFREG },
#endif
#ifdef S_IFSOCK
        { "S_IFSOCK", 8, S_IFSOCK },
#endif
#ifdef S_IFWHT
        { "S_IFWHT", 7, S_IFWHT },
#endif
#ifdef S_IREAD
        { "S_IREAD", 7, S_IREAD },
#endif
#ifdef S_IRGRP
        { "S_IRGRP", 7, S_IRGRP },
#endif
#ifdef S_IROTH
        { "S_IROTH", 7, S_IROTH },
#endif
#ifdef S_IRUSR
        { "S_IRUSR", 7, S_IRUSR },
#endif
#ifdef S_IRWXG
        { "S_IRWXG", 7, S_IRWXG },
#endif
#ifdef S_IRWXO
        { "S_IRWXO", 7, S_IRWXO },
#endif
#ifdef S_IRWXU
        { "S_IRWXU", 7, S_IRWXU },
#endif
#ifdef S_ISGID
        { "S_ISGID", 7, S_ISGID },
#endif
#ifdef S_ISTXT
        { "S_ISTXT", 7, S_ISTXT },
#endif
#ifdef S_ISUID
        { "S_ISUID", 7, S_ISUID },
#endif
#ifdef S_ISVTX
        { "S_ISVTX", 7, S_ISVTX },
#endif
#ifdef S_IWGRP
        { "S_IWGRP", 7, S_IWGRP },
#endif
#ifdef S_IWOTH
        { "S_IWOTH", 7, S_IWOTH },
#endif
#ifdef S_IWRITE
        { "S_IWRITE", 8, S_IWRITE },
#endif
#ifdef S_IWUSR
        { "S_IWUSR", 7, S_IWUSR },
#endif
#ifdef S_IXGRP
        { "S_IXGRP", 7, S_IXGRP },
#endif
#ifdef S_IXOTH
        { "S_IXOTH", 7, S_IXOTH },
#endif
#ifdef S_IXUSR
        { "S_IXUSR", 7, S_IXUSR },
#endif
#ifdef LOCK_SH
        { "LOCK_SH", 7, LOCK_SH },
#endif
#ifdef LOCK_EX
        { "LOCK_EX", 7, LOCK_EX },
#endif
#ifdef LOCK_NB
        { "LOCK_NB", 7, LOCK_NB },
#endif
#ifdef LOCK_UN
        { "LOCK_UN", 7, LOCK_UN },
#endif
#ifdef SEEK_SET
        { "SEEK_SET", 8, SEEK_SET },
#endif
#ifdef SEEK_CUR
        { "SEEK_CUR", 8, SEEK_CUR },
#endif
#ifdef SEEK_END
        { "SEEK_END", 8, SEEK_END },
#endif
#ifdef S_IFMT
        { "_S_IFMT", 7, S_IFMT },
#endif
#ifndef LOCK_SH
        /* This is the default value: */
        { "LOCK_SH", 7, 1 },
#endif
#ifndef LOCK_EX
        /* This is the default value: */
        { "LOCK_EX", 7, 2 },
#endif
#ifndef LOCK_NB
        /* This is the default value: */
        { "LOCK_NB", 7, 4 },
#endif
#ifndef LOCK_UN
        /* This is the default value: */
        { "LOCK_UN", 7, 8 },
#endif
#ifndef SEEK_SET
        /* This is the default value: */
        { "SEEK_SET", 8, 0 },
#endif
#ifndef SEEK_CUR
        /* This is the default value: */
        { "SEEK_CUR", 8, 1 },
#endif
#ifndef SEEK_END
        /* This is the default value: */
        { "SEEK_END", 8, 2 },
#endif
        { NULL, 0, 0 } };
	const struct iv_s *value_for_iv = values_for_iv;
        while (value_for_iv->name) {
	    constant_add_symbol(aTHX_  symbol_table, value_for_iv->name,
				value_for_iv->namelen, newSViv(value_for_iv->value));
            ++value_for_iv;
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
			       "Couldn't add key '%s' to %%Fcntl::",
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
    /* As we've been creating subroutines, we better invalidate any cached
       methods  */
    mro_method_changed_in(symbol_table);
  }

void
AUTOLOAD()
    PROTOTYPE: DISABLE
    PREINIT:
	SV *sv = newSVpvn_flags(SvPVX(cv), SvCUR(cv), SVs_TEMP | SvUTF8(cv));
	const COP *cop = PL_curcop;
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
	    sv = newSVpvf("Your vendor has not defined Fcntl macro %" SVf
			  ", used at %" COP_FILE_F " line %d\n", sv,
			  COP_FILE(cop), CopLINE(cop));
	} else
#endif
	{
	    sv = newSVpvf("%"SVf" is not a valid Fcntl macro at %"
			  COP_FILE_F " line %d\n", sv, COP_FILE(cop), CopLINE(cop));
	}
	croak_sv(sv_2mortal(sv));
