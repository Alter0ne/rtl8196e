/*
 * Copyright (C) 2003, 2004, 2005, 2006, 2007
 * Robert Lougher <rob@jamvm.org.uk>.
 *
 * This file is part of JamVM.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2,
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include <fenv.h>

/* Change the x87 FPU precision to double (64-bit) from the extended
   (80-bit) Linux default.  Note, unlike on i386, my testcases pass
   without this.  This is probably because gcc assumes and uses SSE
   by default, not the x87 FPU.  However, keep it in to be safe.
*/

void setDoublePrecision() {
    fenv_t fenv;

    fegetenv(&fenv);
    fenv.__control_word &= ~0x300; /*_FPU_EXTENDED */
    fenv.__control_word |= 0x200; /*_FPU_DOUBLE */
    fesetenv(&fenv);
}

void initialisePlatform() {
    setDoublePrecision();
}
