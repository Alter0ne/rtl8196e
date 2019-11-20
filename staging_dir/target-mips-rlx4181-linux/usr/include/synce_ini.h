/* $Id: synce_ini.h 1022 2003-07-26 11:14:39Z twogood $ */
#ifndef __synce_ini_h__
#define __synce_ini_h__

typedef struct _SynceIni SynceIni;

SynceIni* synce_ini_new(const char* filename);
void synce_ini_destroy(SynceIni* ini);

int synce_ini_get_int(SynceIni* ini, const char* section, const char* key);
const char* synce_ini_get_string(SynceIni* ini, const char* section, const char* key);

#endif

