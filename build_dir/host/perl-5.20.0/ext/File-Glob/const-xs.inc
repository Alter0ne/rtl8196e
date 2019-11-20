BOOT:
  {
#ifdef dTHX
    dTHX;
#endif
    HV *symbol_table = get_hv("File::Glob::", GV_ADD);

    static const struct iv_s values_for_iv[] =
      {
#ifdef GLOB_ABEND
        { "GLOB_ABEND", 10, GLOB_ABEND },
#endif
#ifdef GLOB_ALPHASORT
        { "GLOB_ALPHASORT", 14, GLOB_ALPHASORT },
#endif
#ifdef GLOB_ALTDIRFUNC
        { "GLOB_ALTDIRFUNC", 15, GLOB_ALTDIRFUNC },
#endif
#ifdef GLOB_BRACE
        { "GLOB_BRACE", 10, GLOB_BRACE },
#endif
#ifdef GLOB_ERR
        { "GLOB_ERR", 8, GLOB_ERR },
#endif
#ifdef GLOB_LIMIT
        { "GLOB_LIMIT", 10, GLOB_LIMIT },
#endif
#ifdef GLOB_MARK
        { "GLOB_MARK", 9, GLOB_MARK },
#endif
#ifdef GLOB_NOCASE
        { "GLOB_NOCASE", 11, GLOB_NOCASE },
#endif
#ifdef GLOB_NOCHECK
        { "GLOB_NOCHECK", 12, GLOB_NOCHECK },
#endif
#ifdef GLOB_NOMAGIC
        { "GLOB_NOMAGIC", 12, GLOB_NOMAGIC },
#endif
#ifdef GLOB_NOSORT
        { "GLOB_NOSORT", 11, GLOB_NOSORT },
#endif
#ifdef GLOB_NOSPACE
        { "GLOB_NOSPACE", 12, GLOB_NOSPACE },
#endif
#ifdef GLOB_QUOTE
        { "GLOB_QUOTE", 10, GLOB_QUOTE },
#endif
#ifdef GLOB_TILDE
        { "GLOB_TILDE", 10, GLOB_TILDE },
#endif
        { "GLOB_CSH", 8, GLOB_BRACE|GLOB_NOMAGIC|GLOB_QUOTE|GLOB_TILDE|GLOB_ALPHASORT },
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
			       "Couldn't add key '%s' to %%File::Glob::",
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
	    sv = newSVpvf("Your vendor has not defined File::Glob macro %" SVf
			  ", used at %" COP_FILE_F " line %d\n", sv,
			  COP_FILE(cop), CopLINE(cop));
	} else
#endif
	{
	    sv = newSVpvf("%"SVf" is not a valid File::Glob macro at %"
			  COP_FILE_F " line %d\n", sv, COP_FILE(cop), CopLINE(cop));
	}
	croak_sv(sv_2mortal(sv));
