/* $Id: synce_vector_template.h 1504 2004-01-12 09:34:36Z twogood $ */

#include <synce.h>
#include <synce_log.h>
#include <stdlib.h>

#define SYNCE_VECTOR_DECLARE(name, prefix, type) \
 \
struct _ ## name \
{ \
  type* items; \
  size_t used; \
  size_t size; \
}; \
 \
typedef struct _ ## name name; \
 \
/** Create a new vector object */ \
name* prefix ## _new(); \
 \
/** Destroy vector object */ \
void prefix ## _destroy(name* v, bool free_items); \
 \
/** Add an item to vector */ \
name* prefix ## _add(name* v, type value); \
 \
/** Add many items to vector */ \
name* prefix ## _add_many( \
    name* v,  \
    type* values,  \
    size_t count);


#define SYNCE_VECTOR_IMPLEMENT(name, prefix, type) \
 \
static void prefix ## _enlarge(name* v, size_t size) \
{ \
  if (v->size < size) \
  { \
    size_t new_size = v->size ? v->size : 2; \
 \
    while (new_size < size) \
      new_size <<= 1; \
 \
    v->items = realloc(v->items, sizeof(type) * new_size); \
    if (!v->items) \
    { \
      synce_error("Failed to allocate space for %i elements - crashing!", new_size); \
    } \
 \
    v->size  = new_size; \
  } \
} \
 \
name* prefix ## _new() \
{ \
  return (name*)calloc(1, sizeof(name)); \
} \
 \
void prefix ## _destroy(name* v, bool free_items) \
{ \
  if (v) \
  { \
    if (free_items && v->items) \
      free(v->items); \
    free(v); \
  } \
} \
 \
name* prefix ## _add(name* v, type value) \
{ \
  prefix ## _enlarge(v, v->used + 1); \
  v->items[v->used++] = value; \
  return v; \
} \
 \
name* prefix ## _add_many( \
    name* v,  \
    type* values,  \
    size_t count) \
{ \
  unsigned i; \
   \
  prefix ## _enlarge(v, v->used + count); \
 \
  for (i = 0; i < count; i++) \
  { \
    v->items[v->used++] = values[i]; \
  } \
   \
  return v; \
}

