Embedthis Appweb README
-----------------------

Welcome to the Embedthis Appweb distribution. Appweb is a compact, fast, modular and powerful embedded web server. 

This document contains details regarding the installation procedure for the binary Embedthis Appweb package.  This package contains pre-built stand-alone web server and an embeddable HTTP library with headers.

You can read the documentation for the latest release online at:

    http://www.embedthis.com/products/appweb/doc/product/index.html

This software is copyrighted and distributed under license. Please read the LICENSE.TXT for details.


Table of Contents
-----------------

* System Requirements
* Installation Package Formats
* Development Environment Platform Support
* Installating the Tar Self-Install Image
* Removing Embedthis Appweb
* Apple MAC OS X  Release Details
* Linux Release Details
* Windows Release Details
* Running Appweb
* Building from the Source Code
* License and Copyright Information

System Requirements
-------------------

Operating System Support

    * Windows XP, Vista, Windows-7 (x86,x86_64)
    * Mac OS X 10.4, 10.5 (ppc,x86,x86_64)
    * Linux 2.4, 2.6 (x86,,x86_64arm,ppc,sparc,xscale,mips)
    * VxWorks (x86,arm,ppc,sparc,xscale,mips)
    * Solaris (sparc,x86,x86_64)

To install Appweb, your development system will need at least the following:

    * 50 MB Disk
    * 1 GB RAM

To rebuild Appweb from source code, you will need at least:

    * 80 MB disk space

To deploy Appweb in an embedded environment, you will need at least the 
following:

    * 1MB KB Disk / ROM space
    * 2MB RAM


Installation Package Formats
----------------------------

The Appweb binary installation is published in several formats:

    * Native Apple MAC OS X DMG/PKG format
    * Native Linux RPM format
    * Native Linux Debian format
    * Native Windows installer format
    * Tar format for custom self installs.

All formats are wrapped in a compressed Tar archive with this README.TXT file.  It is recommended that you use one of the native O/S formats if your system supports it. The native formats will typically register the software installation with your system's software manager so that you can query, verify, and easily manage the software on your system. 

The Tar format consists of tar archives that you can manually extract for maximum control over the placement of various product components.

The installation package includes binaries, development libraries, headers and documentation. You can selectively install sub-packages or you can install the complete distribution.

For those looking to build from source code, you will need to download the separate source code distribution. The product documentation describes how to build from source. 

See below for operating system specific installation information.


Development Environment Platform Support
----------------------------------------

If you wish to embed Appweb in your application or build Appweb from source code, you will need to use a C compiler and associated development tools.  Several development environments are supported.  You may choose any of the following:

    * Linux/Unix/Solaris GNU Tools

    * Windows Visual Studio (Visual Studio 2008 (Express) or the Cygwin UNIX 
      emulation tools. See www.cygwin.com.

    * WindRiver Tornado Development Tools


Installating the Tar Self-Install Image
---------------------------------------

This is the procedure to to install the self-install Tar format image:

   1. Uncompress and extract the image by typing

      tar xvf appweb-VERSION-BUILD-DIST-OS-ARCH.FMT.tar.gz

      where FMT is "tar".

   2. Invoke the installation script with administrator privilege

      sudo ./install


Removing Embedthis Appweb
---------------------------

This is the procedure to remove Appweb on all systems except Windows.

   1. Change directory to the installation directory for Appweb. This is 
      usually /etc/appweb

   2. Run the uninstall script in the install directory for Appweb by typing 

      sudo ./uninstall


Apple Mac OS X Release Details
-------------------------------

To install the native MAC OS X DMG image:

   1. Download the DMG image.

   2. Double click on the DMG file image. This will mount a new drive called
      appweb-VERSION. Double click on this drive to display its contents.
 
   3. Double click on the "appweb.mpkg" image.

   4. Answer the questions as prompted.


Linux Release Details
---------------------

The installation images all include an "install" script that will intelligently invoke the appropriate native software package manager such as "rpm" or dpkg as required.

To install a Linux image:

   1. Uncompress and extract the image by typing:

      tar xvfz appweb-VERSION-BUILD-DIST-LINUX-ARCH.FMT.tar.gz

      Where FMT is "rpm" or "deb".

   2. Run the install script which will invoke the native package manager. If
      you are doing a Tar self-install, the install script will complete the
      installation for you.

      sudo ./install


Windows Release Details
-----------------------

To install the Windows Installer image:

   1. Login with administrator privileges. This is necessary as the binary 
      installation needs to install a Windows Service.

   2. Use WinZip or equivalent software to extract the installation image. This
      will extract the README.TXT and setup.exe from the zip archive.

   3. Double click on the setup.exe image to run it.


Removing Appweb on Windows
----------------------------

If you installed the Appweb Windows Installer image, follow these steps to remove Appweb. If you installed the Tar self-install image, you will need to remove Appweb manually. 

   1. Login with administrator privileges

   2. Run the Add / Remove Programs applet from the control panel.

   3. Select the Embedthis Appweb and choose remove.


Running Appweb
----------------

Appweb will start when the Appweb installation procedure completes and on some systems, will launch your browser directed at the Appweb local home page.  Thereafter, Appweb will automatically start if the system is rebooted. If you wish to manually stop Appweb, type:

    /etc/init.d/appweb stop

To restart Appweb:

    /etc/init.d/appweb start

Alternatively, you can run appweb manually:

    cd /etc/appweb
    appweb

To get all the options for Appweb, read the Appweb man page via

    man appweb

or get the command help via:

    appweb --help

Appweb is configured by default to listen on port 7777. To test that it is serving pages, point your browser at http://localhost:7777. 

To tailor the Embedthis Appweb configuration, including the default port, you may wish to edit the appweb.conf configuration file.


Building from the Source Code
-----------------------------

If you wish to rebuild the Appweb product from the distributed source code, please download the source code distribution from the Embedthis web site. Then read "Building from Source" in the Appweb product documentation for an explanation of the necessary steps to successfully rebuild the product.


License and Copyright Information
---------------------------------

This software is supplied under license. Both a GNU and commercial license are available. Please read LICENSE.TXT for details. This documentation and the software described in this document may be used and copied only in accordance with the terms of the accompanying license agreement. Product and technical information in this document is subject to change without notice and does not represent a commitment on the part of Embedthis Software LLC.

Copyright (c) Embedthis Software LLC, 2003-2012. All Rights Reserved.

Embedthis is a trademark of Embedthis Software LLC. Embedthis, Appweb and Ejscript are trademarks of Embedthis Software. All other brand or product names are the trademarks or registered trademarks of their respective holders.


Embedthis Software, LLC.
4616 25th Ave NE, #733
Seattle, WA 98105.
+1 (425) 329-3490
www.embedthis.com
info@embedthis.com
