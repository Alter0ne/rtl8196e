#
# spec file for quilt - patch management scripts
#

Name:		quilt
Summary:	Scripts for working with series of patches
License:	GPL
Group:		Productivity/Text/Utilities
Version:	0.63
Release:	1
Requires:	coreutils diffutils findutils patch gzip bzip2 perl mktemp gettext
Autoreqprov:	off
Source:		quilt-%{version}.tar.gz
BuildRoot:	%{_tmppath}/%{name}-%{version}-build

%description
The scripts allow to manage a series of patches by keeping
track of the changes each patch makes. Patches can be
applied, un-applied, refreshed, etc.

The scripts are heavily based on Andrew Morton's patch scripts
found at http://userweb.kernel.org/~akpm/stuff/patch-scripts.tar.gz.

Authors:
--------
    Andrew Morton <akpm@digeo.com>
    Andreas Gruenbacher <agruen@suse.de>

%prep
%setup

%build
CFLAGS="$RPM_OPT_FLAGS" \
./configure --prefix=/usr \
	--mandir=%_mandir \
	--docdir=%_docdir/%{name}-%{version}
make RELEASE=%release

%install
rm -rf $RPM_BUILD_ROOT
make install prefix=/usr BUILD_ROOT=$RPM_BUILD_ROOT
%{find_lang} %{name}

%clean
rm -rf $RPM_BUILD_ROOT

%files -f %{name}.lang
%defattr(-, root, root)
/usr/bin/guards
/usr/bin/quilt
/usr/share/quilt/
/usr/share/emacs/
/etc/bash_completion.d/quilt
%config(noreplace) /etc/quilt.quiltrc
%doc %{_mandir}/man1/guards.1*
%doc %{_mandir}/man1/quilt.1*
%doc %{_docdir}/%{name}-%{version}/README
%doc %{_docdir}/%{name}-%{version}/README.MAIL
%doc %{_docdir}/%{name}-%{version}/quilt.pdf

%changelog

* Wed May 07 2014 - upstream
- Update to version 0.63
  + New NEWS file, containing a human-readable changelog
  + Option -E is no longer passed to patch by default
  + Huge performance improvement (e.g., for commands series, applied and unapplied)
  + configure: Add compat symlinks for cp and md5sum
  + Return 2 when there is nothing to do, contrasting with errors (ret=1)
  + Exit with an error when diff's retcode=2 (error) on patch refresh
  + bash_completion: cleanups and performance improvement (Savannah's #27111)
  + test/run: Use perl module Text::ParseWords (+ performance improvement)
  + Add some tests to our testsuite, for a better coverage
  + Fix heuristic for unapplied patches with timestamps
  + Bug fix: Patches emptying files should work now
  + Bug fix: Check for duplicate patch in series (Savannah's #20628)
  + Portability fixes for older Bash and GNU patch

* was a buggy release, with an incomplete tarfile - upstream
- Update to version 0.62

* Sun Dec 08 2013 - upstream
- Update to version 0.61
  + Almost two years of fixes and minor improvements
  + Fix support for ./configure --with-xargs
  + Parameter quoting fixes to many commands
  + Various fixes to the pop, push, refresh and patches commands
  + Translation fixes
  + setup: Many fixes and improvements
  + remove-trailing-ws: Several fixes and improvements
  + remove-trailing-ws: Add a dedicated test case
  + quilt.el: Many fixes and improvements (emacs integration)

* Wed Feb 29 2012 - upstream
- Update to version 0.60
  + BSD compatibility improvements
  + grep: Support file names which include spaces
  + import: Fix import of relative patches
  + mail: Several fixes
  + setup: Support directory and archive names which include spaces
  + backup-files: rewritten from C to bash
  + backup-files: Add a dedicated test case

* Sat Jan 28 2012 - upstream
- Update to version 0.51
  + Fix support for ./configure --docdir
  + Various $TMPDIR fixes
  + mail: Fix delivery address checking
  + mail: CC people in more common patch headers
  + push: Fix bash completion
  + inspect: Complain if wrapper script can't be executed

* Mon Dec 5 2011 - upstream
- Update to version 0.50
  + 34 months of fixes and improvements, too many to list them all
  + Fix detection of the patch version
  + Avoid error messages when building due to missing git-desc file
  + Add support for lzma and xz compression formats
  + import: Fix confusing French translation
  + mail: Stop using =~ for older versions of bash
  + mail: Fix a temporary directory leak
  + revert: Stop using cp -l
  + revert: Add bash completion support
  + setup: Add --fuzz parameter
  + setup: Add support for reverse patches
  + inspect: Fix shell syntax errors
  + Fix error in test case create-delete

* Thu Jan 29 2009 - upstream
- Update to version 0.48
  + fold: Fix bash completion
  + mail: Don't use GNU awk extensions
  + mail: Check for formail
  + setup: Fix for rpm 4.6
  + Fix error in test case import

* Thu Aug 21 2008 - upstream
- Update to version 0.47
  + Change summary not available

* Thu Oct 19 2006 - upstream
- Update to version 0.46
  + Change summary not available

* Mon Apr 24 2006 - upstream
- Update to version 0.45
  + Change summary not available

* Tue Feb 14 2006 - upstream
- Update to version 0.44
  + Change summary not available

* Wed Feb 01 2006 - upstream
- Update to version 0.43
  + Change summary not available

* Tue Jul 26 2005 - upstream
- Update to version 0.42
  + Change summary not available

* Fri Apr 29 2005 - upstream
- Update to version 0.40
  + Change summary not available

* Thu Feb 10 2005 - upstream
- Update to version 0.39
  + Change summary not available

* Sun Oct 17 2004 - upstream
- Update to version 0.37
  + Change summary not available

* Wed Sep 22 2004 - upstream
- Update to version 0.36
  + Change summary not available

* Thu Jul 15 2004 - upstream
- Update to version 0.35
  + Change summary not available

* Thu Jun 10 2004 - upstream
- Update to version 0.34
  + Change summary not available

* Sun Jun 06 2004 - upstream
- Update to version 0.33
  + Change summary not available

* Sat Mar 13 2004 - upstream
- Update to version 0.32
  + Change summary not available

* Wed Jan 28 2004 - upstream
- Update to version 0.30
  + Change summary not available

* Wed Nov 12 2003 - upstream
- Update to version 0.29
  + Change summary not available

* Fri Oct 31 2003 - upstream
- Update to version 0.28
  + Change summary not available

* Tue Oct 28 2003 - upstream
- Update to version 0.27
  + Change summary not available

* Tue Oct 21 2003 - upstream
- Update to version 0.26
  + Change summary not available
