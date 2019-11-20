/* Set current rounding direction.
   Copyright (C) 1997, 2003, 2004, 2005 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1997.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

#include <fenv.h>
#include <fpu_control.h>

int
fesetround (int round)
{
#ifndef __SOFTFP__
      fpu_control_t temp;

      switch (round)
	{
	case FE_TONEAREST:
	case FE_UPWARD:
	case FE_DOWNWARD:
	case FE_TOWARDZERO:
	  _FPU_GETCW (temp);
	  temp = (temp & ~FE_TOWARDZERO) | round;
	  _FPU_SETCW (temp);
	  return 0;
	default:
	  return 1;
	}
#else
  if (round == FE_TONEAREST)
    /* This is the only supported rounding mode for soft-fp.  */
    return 0;
#endif

  /* Unsupported, so fail.  */
  return 1;
}
