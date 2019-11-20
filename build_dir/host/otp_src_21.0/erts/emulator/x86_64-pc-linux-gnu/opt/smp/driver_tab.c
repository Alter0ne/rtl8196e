#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif /* HAVE_CONFIG_H */
#include <stdio.h>
#include "global.h"


extern ErlDrvEntry inet_driver_entry;
extern ErlDrvEntry ram_file_driver_entry;
extern ErlDrvEntry ttsl_driver_entry;

ErtsStaticDriver driver_tab[] =
{
    {&inet_driver_entry, 0},
    {&ram_file_driver_entry, 0},
    {&ttsl_driver_entry, 0},
    {NULL}
};
void erts_init_static_drivers() {
}
void *erl_tracer_nif_init(void);
void *prim_buffer_nif_init(void);
void *prim_file_nif_init(void);
void *zlib_nif_init(void);
static ErtsStaticNifEntry static_nif_tab[] =
{
    {"erl_tracer", &erl_tracer_nif_init, 0},
    {"prim_buffer", &prim_buffer_nif_init, 0},
    {"prim_file", &prim_file_nif_init, 0},
    {"zlib", &zlib_nif_init, 0},
    {NULL,NULL}
};
ErtsStaticNifEntry* erts_static_nif_get_nif_init(const char *name, int len) {
    ErtsStaticNifEntry* p;
    for (p = static_nif_tab; p->nif_name != NULL; p++)
        if (strncmp(p->nif_name, name, len) == 0 && p->nif_name[len] == 0)
            return p;
    return NULL;
}

int erts_is_static_nif(void *handle) {
    ErtsStaticNifEntry* p;
    for (p = static_nif_tab; p->nif_name != NULL; p++)
        if (((void*)p->nif_init) == handle)
            return 1;
    return 0;
}

