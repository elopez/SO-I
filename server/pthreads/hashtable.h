#ifndef __HASHTABLE_H__
#define __HASHTABLE_H__

typedef void (*VisitorFunc) (void *data, void *extra_data);

typedef void (*HTVisitorFunc) (void *key, void *value, void *extra_data);

/**
 * Functions of this type should return 0 if the objects pointed to by 'a' and
 * 'b' are different, or a value different from 0 if the objects are equal.
 */
typedef int  (*EqualsFunc) (const void *a, const void *b);

/**
 * Opaque structure representing a hash table.
 */
typedef struct _HashTable HashTable;

/**
 * Hash functions
 */
typedef unsigned int (*HashFunc) (const void *key, unsigned int limit);
unsigned int hash_table_ugly_hash(const void *key, unsigned int limit);
unsigned int hash_table_string_hash(const void *key, unsigned int limit);
unsigned int hash_table_int_hash(const void *key, unsigned int limit);

/**
 * Creates a new hash table that uses a default hashing function.
 *
 * 'equals' can be NULL. If NULL is provided, then direct address comparison
 * is used to determine key equality.
 */
HashTable *hash_table_new(EqualsFunc equals);

/**
 * Creates a new hash table that uses an arbitrary hashing function
 *
 * 'equals' can be NULL. If NULL is provided, then direct address comparison
 * is used to determine key equality.
 *
 * 'hash' should be not NULL
 */
HashTable *hash_table_new_extended(EqualsFunc equals, HashFunc hash);

/**
 * Shallow destruction of a hash table.
 *
 * The data items that were pointed to by the table are not freed (neither
 * the keys nor the values). If you want to destroy them you should call
 * first hash_table_foreach to invoke proper clean-up code.
 */
void hash_table_destroy(HashTable * table);

/**
 * Inserts a (key,value) pair into a table.
 *
 * Note that neither 'key' nor 'value' are copied into the table, but just
 * referenced on it, so both the key and value objects must exists for the
 * lifetime of the table.
 *
 * 'key' cannot be NULL.
 *
 * If there was already a value associated for the given key, that value is
 * replaced by the new one. That means, additionally, that there are no
 * duplicated (key,value) pairs in a HashTable.
 */
void hash_table_insert(HashTable * table, void *key, void *value);

/**
 * Removes a (key,value) pair from a table.
 *
 * If the key is unknown, the table is not modified. 'key' cannot be NULL.
 */
void hash_table_remove(HashTable * table, const void *key);

/**
 * Returns the value associated with a key, if the key exists in the table.
 *
 * If the key is unknown, returns NULL. 'key' cannot be NULL.
 */
void *hash_table_lookup(HashTable * table, void *key);

/**
 * Returns the number of (key,value) pairs in a table.
 */
size_t hash_table_size(HashTable * table);

/**
 * Iterates over each (key,value) pair of a table applying the function 'visit'.
 *
 * There is no defined order on the elements. All of them will be visited, but
 * the exact order is not predictable based on the order of insertions/removals.
 */
void hash_table_foreach(HashTable * table, HTVisitorFunc visit,
			void *extra_data);

#endif
