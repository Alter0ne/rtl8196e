Source-Makefile: feeds/telephony/net/kamailio-3.x/Makefile
Package: kamailio3
Menu: 1
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread +BUILD_NLS:libiconv-full +libncurses +libpthread +libreadline
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: Mature and flexible open source SIP server, v3.3.7
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: Mature and flexible open source SIP server, v3.3.7
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-acc
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-tm
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Accounting module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-alias-db
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-db-sqlite
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Database-backend aliases module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-auth
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Authentication Framework module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-auth-db
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-auth +kamailio3-mod-db-sqlite
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Database-backend authentication module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-auth-diameter
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-sl
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Diameter-backend authentication module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-avpops
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 AVP operation module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-benchmark
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Config benchmark module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-cfgutils
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Config utilities module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-cfg-db
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-db-sqlite
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Load core and module parameters from database module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-cfg-rpc
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Update core and module parameters at runtime via RPC interface module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-ctl
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 BINRPC transport interface module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-db-flatstore
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Fast writing-only text database-backed module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-db-mysql
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +libmysqlclient
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 MySQL database-backend module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-db-postgres
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +libpq
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 PostgreSQL Database-backend module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-db-sqlite
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +libsqlite3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Sqlite DB support module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-db-text
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Text database-backend module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-db-unixodbc
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +unixodbc
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 UnixODBC Database-backend module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-dialog
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-rr +kamailio3-mod-tm
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Dialog support module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-dialplan
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +libpcre
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Dialplan management module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-dispatcher
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Dispatcher module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-diversion
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Diversion header insertion module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-domain
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Multi-domain support module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-domainpolicy
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Domain policy module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-enum
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 ENUM lookup module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-exec
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 External exec module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-group
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Database-backend user-groups module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-h350
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-ldap +libopenldap
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 H.350 module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-htable
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Hash Table module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-imc
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-db-mysql +kamailio3-mod-tm
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 IM conferencing module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-ipops
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 IP and IPv6 operations module module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-kex
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Core extensions module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-lcr
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-tm +libpcre
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Least Cost Routing module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-ldap
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +libopenldap
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 LDAP connector module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-maxfwd
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Max-Forward processor module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-mediaproxy
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-dialog
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Automatic NAT traversal module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-mi-datagram
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Datagram support for Management Interface module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-mi-fifo
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 FIFO support for Management Interface module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-mi-rpc
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 RPC support for Management Interface module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-msilo
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-tm
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 SIP message silo module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-nat-traversal
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-dialog +kamailio3-mod-sl +kamailio3-mod-tm
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 NAT traversal module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-nathelper
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +rtpproxy
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 NAT traversal helper module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-path
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-rr
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 SIP path insertion module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-pdt
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Prefix-to-Domain translator module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-permissions
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Permissions control module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-pike
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Flood detector module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-presence
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-sl +kamailio3-mod-tm +libxml2
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Presence server module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-presence-dialoginfo
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-presence
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Dialog Event presence module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-presence-mwi
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-presence
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Message Waiting Indication presence module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-presence-xml
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-presence +kamailio3-mod-xcap-client
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 XCAP presence module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-pua
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-tm +libxml2
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Presence User Agent module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-pua-bla
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-presence +kamailio3-mod-pua +kamailio3-mod-usrloc
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Bridged Line Appearence PUA module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-pua-dialoginfo
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-dialog +kamailio3-mod-pua
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Dialog Event PUA module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-pua-mi
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-pua
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 PUA Management Interface module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-pua-usrloc
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-pua +kamailio3-mod-usrloc
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 PUA User Location module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-pua-xmpp
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-presence +kamailio3-mod-pua +kamailio3-mod-xmpp
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 PUA XMPP module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-pv
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Pseudo-Variables module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-qos
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-dialog
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 QoS control module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-ratelimit
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Traffic shapping module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-regex
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +libpcre
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Regular Expression module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-registrar
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-usrloc
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 SIP Registrar module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-rls
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-presence +kamailio3-mod-pua +kamailio3-mod-tm +libxml2
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Resource List Server module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-rr
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Record-Route and Route module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-rtimer
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Routing Timer module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-rtpproxy
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-tm
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 RTP proxy module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-sanity
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-sl
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 SIP sanity checks module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-sipcapture
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 SIP capture module module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-siptrace
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 SIP trace module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-siputils
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-sl
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 SIP utilities module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-sl
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Stateless replier module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-sms
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-tm
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 SIP-to-SMS IM gateway module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-speeddial
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Per-user speed-dial controller module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-sqlops
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 SQL operations module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-statistics
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Script statistics module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-sst
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-dialog +kamailio3-mod-sl
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 SIP Session Timer module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-textops
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Text operations module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-tls
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +libopenssl
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 TLS operations module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-tm
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Transaction module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-tmx
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Transaction module extensions module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-uac
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-tm
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 User Agent Client module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-uac-redirect
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-tm
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 User Agent Client redirection module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-uri-db
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Database-backend SIP URI checking module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-userblacklist
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 User blacklists module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-usrloc
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +libxml2
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 User location module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-utils
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +libcurl +libxml2
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Misc utilities module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-xcap-client
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +libcurl
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 XCAP Client module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-xlog
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 Advanced logger module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@

Package: kamailio3-mod-xmpp
Submenu: Telephony
Version: 3.3.7-7
Depends: +libc +USE_EGLIBC:librt +USE_EGLIBC:libpthread kamailio3 +kamailio3-mod-tm +libexpat +libxml2
Menu-Depends: 
Provides: 
Build-Depends: !BUILD_NLS:libiconv !BUILD_NLS:libintl
Section: net
Category: Network
Title: kamailio3 SIP-to-XMPP Gateway module
Maintainer: Jiri Slachta <slachta@cesnet.cz>
Source: kamailio-3.3.7_src.tar.gz
Type: ipkg
Description: This package provides support for  in Kamailio.
http://www.kamailio.org/
Jiri Slachta <slachta@cesnet.cz>
@@


