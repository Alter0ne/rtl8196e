BOOT:
  {
#ifdef dTHX
    dTHX;
#endif
    HV *symbol_table = get_hv("Sys::Syslog::", GV_ADD);

    static const struct iv_s values_for_iv[] =
      {
#ifdef LOG_ALERT
        { "LOG_ALERT", 9, LOG_ALERT },
#endif
#ifdef LOG_CRIT
        { "LOG_CRIT", 8, LOG_CRIT },
#endif
#ifdef LOG_DEBUG
        { "LOG_DEBUG", 9, LOG_DEBUG },
#endif
#ifdef LOG_EMERG
        { "LOG_EMERG", 9, LOG_EMERG },
#endif
#ifdef LOG_ERR
        { "LOG_ERR", 7, LOG_ERR },
#endif
#ifdef LOG_INFO
        { "LOG_INFO", 8, LOG_INFO },
#endif
#ifdef LOG_NOTICE
        { "LOG_NOTICE", 10, LOG_NOTICE },
#endif
#ifdef LOG_WARNING
        { "LOG_WARNING", 11, LOG_WARNING },
#endif
#ifdef LOG_AUTH
        { "LOG_AUTH", 8, LOG_AUTH },
#endif
#ifdef LOG_AUTHPRIV
        { "LOG_AUTHPRIV", 12, LOG_AUTHPRIV },
#endif
#ifdef LOG_CRON
        { "LOG_CRON", 8, LOG_CRON },
#endif
#ifdef LOG_DAEMON
        { "LOG_DAEMON", 10, LOG_DAEMON },
#endif
#ifdef LOG_FTP
        { "LOG_FTP", 7, LOG_FTP },
#endif
#ifdef LOG_KERN
        { "LOG_KERN", 8, LOG_KERN },
#endif
#ifdef LOG_LOCAL0
        { "LOG_LOCAL0", 10, LOG_LOCAL0 },
#endif
#ifdef LOG_LOCAL1
        { "LOG_LOCAL1", 10, LOG_LOCAL1 },
#endif
#ifdef LOG_LOCAL2
        { "LOG_LOCAL2", 10, LOG_LOCAL2 },
#endif
#ifdef LOG_LOCAL3
        { "LOG_LOCAL3", 10, LOG_LOCAL3 },
#endif
#ifdef LOG_LOCAL4
        { "LOG_LOCAL4", 10, LOG_LOCAL4 },
#endif
#ifdef LOG_LOCAL5
        { "LOG_LOCAL5", 10, LOG_LOCAL5 },
#endif
#ifdef LOG_LOCAL6
        { "LOG_LOCAL6", 10, LOG_LOCAL6 },
#endif
#ifdef LOG_LOCAL7
        { "LOG_LOCAL7", 10, LOG_LOCAL7 },
#endif
#ifdef LOG_LPR
        { "LOG_LPR", 7, LOG_LPR },
#endif
#ifdef LOG_MAIL
        { "LOG_MAIL", 8, LOG_MAIL },
#endif
#ifdef LOG_NEWS
        { "LOG_NEWS", 8, LOG_NEWS },
#endif
#ifdef LOG_SYSLOG
        { "LOG_SYSLOG", 10, LOG_SYSLOG },
#endif
#ifdef LOG_USER
        { "LOG_USER", 8, LOG_USER },
#endif
#ifdef LOG_UUCP
        { "LOG_UUCP", 8, LOG_UUCP },
#endif
#ifdef LOG_INSTALL
        { "LOG_INSTALL", 11, LOG_INSTALL },
#endif
#ifdef LOG_LAUNCHD
        { "LOG_LAUNCHD", 11, LOG_LAUNCHD },
#endif
#ifdef LOG_NETINFO
        { "LOG_NETINFO", 11, LOG_NETINFO },
#endif
#ifdef LOG_RAS
        { "LOG_RAS", 7, LOG_RAS },
#endif
#ifdef LOG_REMOTEAUTH
        { "LOG_REMOTEAUTH", 14, LOG_REMOTEAUTH },
#endif
#ifdef LOG_CONSOLE
        { "LOG_CONSOLE", 11, LOG_CONSOLE },
#endif
#ifdef LOG_NTP
        { "LOG_NTP", 7, LOG_NTP },
#endif
#ifdef LOG_SECURITY
        { "LOG_SECURITY", 12, LOG_SECURITY },
#endif
#ifdef LOG_AUDIT
        { "LOG_AUDIT", 9, LOG_AUDIT },
#endif
#ifdef LOG_LFMT
        { "LOG_LFMT", 8, LOG_LFMT },
#endif
#ifdef LOG_CONS
        { "LOG_CONS", 8, LOG_CONS },
#endif
#ifdef LOG_PID
        { "LOG_PID", 7, LOG_PID },
#endif
#ifdef LOG_NDELAY
        { "LOG_NDELAY", 10, LOG_NDELAY },
#endif
#ifdef LOG_NOWAIT
        { "LOG_NOWAIT", 10, LOG_NOWAIT },
#endif
#ifdef LOG_ODELAY
        { "LOG_ODELAY", 10, LOG_ODELAY },
#endif
#ifdef LOG_PERROR
        { "LOG_PERROR", 10, LOG_PERROR },
#endif
#ifdef LOG_FACMASK
        { "LOG_FACMASK", 11, LOG_FACMASK },
#endif
#ifdef LOG_PRIMASK
        { "LOG_PRIMASK", 11, LOG_PRIMASK },
#endif
#ifdef LOG_NFACILITIES
        { "LOG_NFACILITIES", 15, LOG_NFACILITIES },
#endif
#ifndef LOG_INSTALL
        /* This is the default value: */
        { "LOG_INSTALL", 11, LOG_USER },
#endif
#ifndef LOG_LAUNCHD
        /* This is the default value: */
        { "LOG_LAUNCHD", 11, LOG_DAEMON },
#endif
#ifndef LOG_NETINFO
        /* This is the default value: */
        { "LOG_NETINFO", 11, LOG_DAEMON },
#endif
#ifndef LOG_RAS
        /* This is the default value: */
        { "LOG_RAS", 7, LOG_AUTH },
#endif
#ifndef LOG_REMOTEAUTH
        /* This is the default value: */
        { "LOG_REMOTEAUTH", 14, LOG_AUTH },
#endif
#ifndef LOG_CONSOLE
        /* This is the default value: */
        { "LOG_CONSOLE", 11, LOG_USER },
#endif
#ifndef LOG_NTP
        /* This is the default value: */
        { "LOG_NTP", 7, LOG_DAEMON },
#endif
#ifndef LOG_SECURITY
        /* This is the default value: */
        { "LOG_SECURITY", 12, LOG_AUTH },
#endif
#ifndef LOG_AUDIT
        /* This is the default value: */
        { "LOG_AUDIT", 9, LOG_AUTH },
#endif
#ifndef LOG_LFMT
        /* This is the default value: */
        { "LOG_LFMT", 8, LOG_USER },
#endif
#ifndef LOG_PRIMASK
        /* This is the default value: */
        { "LOG_PRIMASK", 11, 7 },
#endif
#ifndef LOG_NFACILITIES
        /* This is the default value: */
        { "LOG_NFACILITIES", 15, 30 },
#endif
        { NULL, 0, 0 } };
	const struct iv_s *value_for_iv = values_for_iv;

    static const struct pv_s values_for_pv[] =
      {
#ifdef _PATH_LOG
        { "_PATH_LOG", 9, _PATH_LOG },
#endif
#ifndef _PATH_LOG
        /* This is the default value: */
        { "_PATH_LOG", 9, "/dev/log" },
#endif
        { NULL, 0, 0 } };
	const struct pv_s *value_for_pv = values_for_pv;
        while (value_for_iv->name) {
	    constant_add_symbol(aTHX_  symbol_table, value_for_iv->name,
				value_for_iv->namelen, newSViv(value_for_iv->value));
            ++value_for_iv;
	}
        while (value_for_pv->name) {
	    constant_add_symbol(aTHX_  symbol_table, value_for_pv->name,
				value_for_pv->namelen, newSVpv(value_for_pv->value, 0));
            ++value_for_pv;
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
			       "Couldn't add key '%s' to %%Sys::Syslog::",
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
constant(sv)
    INPUT:
	SV *		sv;
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
	    sv = newSVpvf("Your vendor has not defined Sys::Syslog macro %" SVf
			  ", used", sv);
	} else
#endif
	{
	    sv = newSVpvf("%"SVf" is not a valid Sys::Syslog macro",
			  sv);
	}
	PUSHs(sv_2mortal(sv));
