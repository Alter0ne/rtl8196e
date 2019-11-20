Summary:	Xfce developer tools
Name:		xfce4-dev-tools
Version:	4.8.0
Release:	1
License:	GPL
URL:		http://xfce.org/~benny/projects/xfce4-dev-tools/
Source0: 	%{name}-%{version}.tar.gz
Group:		Development/Tools
BuildRoot:	%{_tmppath}/%{name}-root

%description:
This package contains common tools required by Xfce developers and people
that want to build Xfce from SVN. In addition, this package contains the
Xfce developer's handbook.

%prep
%setup -q

%build
%configure
make

%install
rm -rf $RPM_BUILD_ROOT
make install DESTDIR=$RPM_BUILD_ROOT mandir=%{_mandir}

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc AUTHORS COPYING ChangeLog HACKING INSTALL NEWS README
%{_bindir}/
%{_datadir}/
