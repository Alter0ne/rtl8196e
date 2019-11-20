/* warranty.c: warranty routines for bc. */

/*  This file is part of GNU bc.
    Copyright (C) 1991-1994, 1997, 2000, 2003, 2004 Free Software Foundation, Inc.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License , or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; see the file COPYING.  If not, write to
      The Free Software Foundation, Inc.
      51 Franklin Street, Fifth Floor
      Boston, MA 02110-1301  USA

    You may contact the author by:
       e-mail:  philnelson@acm.org
      us-mail:  Philip A. Nelson
                Computer Science Department, 9062
                Western Washington University
                Bellingham, WA 98226-9062
       
*************************************************************************/


#include "bcdefs.h"
#include "proto.h"


/* Print the welcome banner. */

void 
welcome()
{
  printf ("This is free software with ABSOLUTELY NO WARRANTY.\n");
  printf ("For details type `warranty'. \n");
}

/* Print out the version information. */
void
show_bc_version()
{
  printf("%s %s\n%s\n", PACKAGE, VERSION, BC_COPYRIGHT);
}


/* Print out the warranty information. */

void 
warranty(prefix)
     const char *prefix;
{
  printf ("\n%s", prefix);
  show_bc_version ();
  printf ("\n"
"    This program is free software; you can redistribute it and/or modify\n"
"    it under the terms of the GNU General Public License as published by\n"
"    the Free Software Foundation; either version 2 of the License , or\n"
"    (at your option) any later version.\n\n"
"    This program is distributed in the hope that it will be useful,\n"
"    but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
"    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
"    GNU General Public License for more details.\n\n"
"    You should have received a copy of the GNU General Public License\n"
"    along with this program. If not, write to\n\n"
"       The Free Software Foundation, Inc.\n"
"       51 Franklin Street, Fifth Floor\n"
"       Boston, MA 02110-1301  USA\n\n");
}
