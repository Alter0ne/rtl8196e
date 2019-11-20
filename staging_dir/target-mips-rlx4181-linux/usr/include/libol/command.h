/***************************************************************************
 *
 * Copyright (c) 1998-1999 Niels Möller
 * Copyright (c) 1999 BalaBit Computing
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * $Id: command.h,v 1.1 1999/07/10 13:23:09 bazsi Exp $
 *
 ***************************************************************************/

#ifndef __COMMAND_H_INCLUDED
#define __COMMAND_H_INCLUDED

#include "list.h"
#include "io.h"

#include <stdarg.h>

#define CLASS_DECLARE
#include "command.h.x"
#undef CLASS_DECLARE

/* Continuation based command execution. A command can take one object
 * as argument, and returns one object. */

/* CLASS:
   (class
     (name command_continuation)
     (vars
       (c method int "struct ol_object *result")))
*/

/* CLASS:
   (class
     (name command)
     (vars
       (call method int "struct ol_object *arg"
                        "struct command_continuation *c")))
*/

/* CLASS:
   (class
     (name command_simple)
     (super command)
     (vars
       ;; Like call, but returns the value immediately rather than
       ;; using a continuation function
       (call_simple method "struct ol_object *" "struct ol_object *")))
*/

#define COMMAND_CALL(f, a, c) \
  ((f)->call((f), (struct ol_object *) (a), (c)))
#define COMMAND_RETURN(r, v) \
  ((r) ? ((r)->c((r), (struct ol_object *) (v))) : ST_OK | ST_GOON)
#define COMMAND_SIMPLE(f, a) \
  ((f)->call_simple((f), (struct ol_object *)(a)))

int do_call_simple_command(struct command *s,
			   struct ol_object *arg,
			   struct command_continuation *c);

#define STATIC_COMMAND_SIMPLE(f) \
{ { STATIC_HEADER, do_call_simple_command }, f}

struct command *make_parallell_progn(struct object_list *body);
extern struct command_simple progn_command;

/* CLASS:
   (class
     (name command_frame)
     (super command_continuation)
     (vars
       (up object command_continuation)))
*/

/* Commands that need to collect some arguments before actually doing
 * anything. */

/* The collect_info_n classes keeps track about what to do whith the
 * next argument. As long as we collect arguments without doing
 * anything, the f field in collect_info_n will point to the
 * constructor make_collect_state_n. */
/* CLASS:
   (class
     (name collect_info_4)
     (vars
       (f method "struct ol_object *"
                 "struct ol_object *" "struct ol_object *"
		 "struct ol_object *" "struct ol_object *")
       ;; No next field
       ))
*/

/* CLASS:
   (class
     (name collect_info_3)
     (vars
       (f method  "struct ol_object *"
                  "struct ol_object *" "struct ol_object *"
		  "struct ol_object *")
       (next object collect_info_4)))
*/

/* CLASS:
   (class
     (name collect_info_2)
     (vars
       (f method  "struct ol_object *"
                  "struct ol_object *" "struct ol_object *")
       (next object collect_info_3)))
*/

/* CLASS:
   (class
     (name collect_info_1)
     (super command_simple)
     (vars
       (f method  "struct ol_object *"
                  "struct ol_object *")
       (next object collect_info_2)))
*/

struct ol_object *
do_collect_1(struct command_simple *s, struct ol_object *a);

struct ol_object *
make_collect_state_1(struct collect_info_1 *info,
		     struct ol_object *a);

struct ol_object *
make_collect_state_2(struct collect_info_2 *info,
		     struct ol_object *a,
		     struct ol_object *b);

struct ol_object *
make_collect_state_3(struct collect_info_3 *info,
		     struct ol_object *a,
		     struct ol_object *b,
		     struct ol_object *c);

#define STATIC_COLLECT_1(next) \
{ { { STATIC_HEADER, do_call_simple_command }, do_collect_1}, \
  make_collect_state_1, next }

#define STATIC_COLLECT_2(next) \
{ STATIC_HEADER, make_collect_state_2, next }

#define STATIC_COLLECT_2_FINAL(f) \
{ STATIC_HEADER, f, NULL }

#define STATIC_COLLECT_3(next) \
{ STATIC_HEADER, make_collect_state_3, next }

#define STATIC_COLLECT_3_FINAL(f) \
{ STATIC_HEADER, f, NULL }

extern struct command_simple command_unimplemented;
#define COMMAND_UNIMPLEMENTED (&command_unimplemented.super.super)

struct command command_die_on_null;

/* The CLASS_* macros are used by automatically generated evaluation code */

struct command_continuation *
make_apply(struct command *f, struct command_continuation *c);  
struct ol_object *gaba_apply(struct ol_object *f,
			      struct ol_object *x);

#define CLASS_APPLY gaba_apply

extern struct command_simple command_I;
#define CLASS_VALUE_I (&command_I.super.super)
#define CLASS_APPLY_I_1(x) (x)

extern struct command_simple command_K;
struct command *make_command_K_1(struct ol_object *x);

#define CLASS_VALUE_K (&command_K.super.super)
#define CLASS_APPLY_K_1(x) ((struct ol_object *) make_command_K_1(x))

extern struct collect_info_1 command_S;
/* extern struct collect_info_2 collect_info_S_2; */

struct command *make_command_S_2(struct command *f,
				 struct command *g);

#define CLASS_VALUE_S (&command_S.super.super.super)
#define CLASS_APPLY_S_1(f) (make_collect_state_1(&command_S, (f)))
#define CLASS_APPLY_S_2(f, g) (collect_S_2(NULL, (f), (g)))

extern struct collect_info_1 command_Sp;
extern struct collect_info_2 collect_info_Sp_2;
/* extern struct collect_info_3 collect_info_Sp_3; */

struct command *make_command_Sp_3(struct command *c,
				  struct command *f,
				  struct command *g);

struct ol_object *collect_S_2(struct collect_info_2 *info,
			       struct ol_object *f,
			       struct ol_object *g);
struct ol_object *collect_Sp_3(struct collect_info_3 *info,
				struct ol_object *c,
				struct ol_object *f,
				struct ol_object *g);

#define CLASS_VALUE_Sp (&command_Sp.super.super)
#define CLASS_APPLY_Sp_1(c) (make_collect_state_1(&command_Sp, (c)))
#define CLASS_APPLY_Sp_2(c, f) \
  (make_collect_state_2(&collect_info_Sp_2, (c), (f)))
#define CLASS_APPLY_Sp_3(c, f, g) (collect_Sp_3(NULL, (c), (f), (g)))

extern struct collect_info_1 command_B;
/* extern struct collect_info_2 collect_info_B_2; */

struct command *make_command_B_2(struct command *f,
				 struct command *g);
struct ol_object *collect_B_2(struct collect_info_2 *info,
			       struct ol_object *f,
			       struct ol_object *g);

#define CLASS_VALUE_B (&command_B.super.super)
#define CLASS_APPLY_B_1(f) (make_collect_state_1(&command_B, (f)))
#define CLASS_APPLY_B_2(f, g) (collect_B_2(NULL, (f), (g)))

extern struct collect_info_1 command_Bp;
extern struct collect_info_2 collect_info_Bp_2;
extern struct collect_info_3 collect_info_Bp_3;

struct command *make_command_Bp_3(struct command *c,
				  struct command *f,
				  struct command *g);
struct ol_object *collect_Bp_3(struct collect_info_3 *info,
				struct ol_object *c,
				struct ol_object *f,
				struct ol_object *g);

#define CLASS_VALUE_Bp (&command_Bp.super.super)
#define CLASS_APPLY_Bp_1(c) (make_collect_state_1(&command_Bp, (c)))
#define CLASS_APPLY_Bp_2(c, f) \
  (make_collect_state_2(&collect_info_Bp_2, (c), (f)))
#define CLASS_APPLY_Bp_3(c, f, g) (collect_Bp_3(NULL, (c), (f), (g)))

extern struct collect_info_1 command_C;
/* extern struct collect_info_2 collect_info_C_2; */

struct command *
make_command_C_2(struct command *f,
		 struct ol_object *y);
struct ol_object *
collect_C_2(struct collect_info_2 *info,
	    struct ol_object *f,
	    struct ol_object *y);

#define CLASS_VALUE_C (&command_C.super.super.super)
#define CLASS_APPLY_C_1(f) (make_collect_state_1(&command_C, (f)))
#define CLASS_APPLY_C_2(f, y) (collect_C_2(NULL, (f), (y)))

extern struct collect_info_1 command_Cp;
extern struct collect_info_2 collect_info_Cp_2;
/* extern struct collect_info_3 collect_info_Cp_3; */

struct command *
make_command_Cp_3(struct command *c,
		  struct command *f,
		  struct ol_object *y);
struct ol_object *
collect_Cp_3(struct collect_info_3 *info,
	     struct ol_object *c,
	     struct ol_object *f,
	     struct ol_object *y);

#define CLASS_VALUE_Cp (&command_Cp.super.super)
#define CLASS_APPLY_Cp_1(c) (make_collect_state_1(&command_Cp, (c)))
#define CLASS_APPLY_Cp_2(c, f) \
  (make_collect_state_2(&collect_info_Cp_2, (c), (f)))
#define CLASS_APPLY_Cp_3(c, f, y) (collect_Cp_3(NULL, (c), (f), (y)))
     

#endif
