/*
 * Copyright (C) 2012 STMicroelectronics, Ltd
 *
 * Author(s): Filippo Arcidiacono  <filippo.arcidiacono@st.com>
 *
 * Licensed under the LGPL v2.1, see the file COPYING.LIB in this tarball
 */

#include <bits/stdio-lock.h>

/* Function wrapper needed for I/O locking macros. */

void IO_lock_unlock (_IO_lock_t *lock)
{
  _IO_lock_unlock (*lock);
}
