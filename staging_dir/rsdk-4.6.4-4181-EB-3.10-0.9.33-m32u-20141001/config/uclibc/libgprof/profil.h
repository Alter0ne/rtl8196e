#ifndef _PROFIL_H
#define _PROFIL_H 
#include <stdio.h>

extern int __profil (unsigned short int *__sample_buffer, size_t __size,
                     size_t __offset, unsigned int __scale);

extern int __profile_frequency (void);
extern int __fxprintf (FILE *fp, const char *fmt, ...);

#endif
