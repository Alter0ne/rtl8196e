/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 *

 */
#include <stdlib.h>
#include "reg.h"

int ei_reg_setfval(ei_reg *reg, const char *key, double f)
{
  ei_hash *tab;
  ei_reg_obj *obj=NULL;
  
  if (!key || !reg) return -1; /* return EI_BADARG; */
  tab = reg->tab;

  if ((obj=ei_hash_lookup(tab,key))) {
    /* object with same name already exists */
    switch (ei_reg_typeof(obj)) {
    case EI_INT:
      break;
    case EI_FLT:
      break;
    case EI_STR:
      if (obj->size > 0) free(obj->val.s);
      break;
    case EI_BIN:
      if (obj->size > 0) free(obj->val.p);
      break;
    default:
      return -1;
      /* return EI_UNKNOWN; */
    }
  }
  else {
    /* object is new */
    if (!(obj=ei_reg_make(reg,EI_FLT))) return -1; /*  return EI_NOMEM; */
    ei_hash_insert(tab,key,obj);
  }

  obj->attr = EI_FLT | EI_DIRTY;
  obj->val.f=f;
  obj->size = 0;

  return 0;
}
