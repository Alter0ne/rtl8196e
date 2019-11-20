/* +++Date last modified: 05-Jul-1997 */

#ifndef HASH__H
#define HASH__H

#include <stddef.h>           /* For size_t     */

typedef unsigned (*SHashFunc)    (const void* key);
typedef int      (*SCompareFunc) (const void* a, const void* b);
typedef void (*SHashTableCallback)(const void* key, const void* data, void* cookie);
typedef void (*SHashTableDataDestroy)(void* data);


unsigned s_str_hash(const void *key);
int s_str_equal(const void* a, const void* b);
int s_str_equal_no_case(const void* a, const void* b);

/*
** A hash table consists of an array of these buckets.  Each bucket
** holds a copy of the key, a pointer to the data associated with the
** key, and a pointer to the next bucket that collided with this one,
** if there was one.
*/

typedef struct _bucket {
    void *key;
    void *data;
    struct _bucket *next;
} bucket;

/*
** This is what you actually declare an instance of to create a table.
** You then call 'construct_table' with the address of this structure,
** and a guess at the size of the table.  Note that more nodes than this
** can be inserted in the table, but performance degrades as this
** happens.  Performance should still be quite adequate until 2 or 3
** times as many nodes have been inserted as the table was created with.
*/

typedef struct _SHashTable {
    size_t size;
    bucket **table;
    SHashFunc hash;
    SCompareFunc equal;
} SHashTable;

/*
** This is used to construct the table.  If it doesn't succeed, it sets
** the table's size to 0, and the pointer to the table to NULL.
*/

SHashTable *s_hash_table_new(SHashFunc hash_func, SCompareFunc compare_func, size_t size);

/*
** Inserts a pointer to 'data' in the table, with a copy of 'key' as its
** key.  Note that this makes a copy of the key, but NOT of the
** associated data.
*/

void *s_hash_table_insert(SHashTable *table, void *key,void *data);

/*
** Returns a pointer to the data associated with a key.  If the key has
** not been inserted in the table, returns NULL.
*/

void *s_hash_table_lookup(SHashTable *table, const void *key);

/*
** Deletes an entry from the table.  Returns a pointer to the data that
** was associated with the key so the calling code can dispose of it
** properly.
*/

void *s_hash_table_remove(SHashTable *table, const void *key);


/*
** Goes through a hash table and calls the function passed to it
** for each node that has been inserted.  The function is passed
** a pointer to the key, and a pointer to the data associated
** with it.
*/

void s_hash_table_foreach(SHashTable *table, SHashTableCallback func, void* cookie);

/*
** Frees a hash table.  For each node that was inserted in the table,
** it calls the function whose address it was passed, with a pointer
** to the data that was in the table.  The function is expected to
** free the data.  Typical usage would be:
** free_table(&table, free);
** if the data placed in the table was dynamically allocated, or:
** free_table(&table, NULL);
** if not.  ( If the parameter passed is NULL, it knows not to call
** any function with the data. )
*/

void s_hash_table_destroy(SHashTable *table, SHashTableDataDestroy func);

#endif /* HASH__H */
