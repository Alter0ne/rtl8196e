/*
 * Copyright (C) 2011 Robert Lougher <rob@jamvm.org.uk>.
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

#include "jam.h"
#include "symbol.h"

void classlibVMShutdown() {
    /* Execute Shutdown.shutdown() to run any registered shutdown hooks.
       Unlike System.exit() it does not exit the VM after shutdown */

    if(!VMInitialising()) {
        Class *class = findSystemClass(SYMBOL(java_lang_Shutdown));
        if(class != NULL) {
            MethodBlock *mb = findMethod(class, SYMBOL(shutdown),
                                                SYMBOL(___V));
            if(mb != NULL)
                executeStaticMethod(class, mb);
        }

        shutdownVM();
    }
}
