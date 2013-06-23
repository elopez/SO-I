#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "hashtable.h"

#define HASH_TABLE_MIN_SLOTS_NR 8

#define hash_table_must_resize(x)	((x) != NULL && (x)->slots_nr * 2 <= ((x)->elements_count))

/**
 * Structures representing a hash table.
 */

typedef struct _HashTableNode {
	void *key;
	void *value;
	struct _HashTableNode *next;
} HashTableNode;

struct _HashTable {
	HashTableNode **slots;
	unsigned int slots_nr;
	EqualsFunc key_equals;
	size_t elements_count;
	HashFunc hash;
};

static void hash_table_resize(HashTable * table);

static int hash_table_addr_equals(const void *a, const void *b)
{
	return (a == b);
}

/** For hashing strings, see
        http://eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx

    For hashing integers, see
        http://www.concentric.net/~Ttwang/tech/inthash.htm
**/

unsigned int hash_table_ugly_hash(const void *key, unsigned int limit)
{
	return ((unsigned long int)key) % limit;
}

unsigned int hash_table_string_hash(const void *key, unsigned int limit)
{
	unsigned const char *p = key;
	unsigned int h = 2166136261;
	int i;

	for (i = 0; p[i] != '\0'; i++)
		h = (h * 16777619) ^ p[i];

	return h % limit;
}

unsigned int hash_table_int_hash(const void *keyp, unsigned int limit)
{
	int key = *(int *)keyp;
	int c2 = 0x27d4eb2d;	// a prime or an odd constant
	key = (key ^ 61) ^ (key >> 16);
	key = key + (key << 3);
	key = key ^ (key >> 4);
	key = key * c2;
	key = key ^ (key >> 15);
	return key % limit;
}

static HashTable *hash_table_new_internal(EqualsFunc equals, HashFunc hash,
					  unsigned int slots)
{
	HashTable *table = malloc(sizeof(HashTable));
	table->key_equals = (equals != NULL) ? equals : hash_table_addr_equals;
	table->hash = hash;
	table->elements_count = 0;
	table->slots_nr = slots;
	table->slots = calloc(slots, sizeof(HashTableNode *));

	return table;
}

/**
 * Creates a new hash table that uses an arbitrary hashing function
 *
 * 'equals' can be NULL. If NULL is provided, then direct address comparison
 * is used to determine key equality.
 *
 * 'hash' should be not NULL
 */
HashTable *hash_table_new_extended(EqualsFunc equals, HashFunc hash)
{
	assert(hash != NULL);

	return hash_table_new_internal(equals, hash, HASH_TABLE_MIN_SLOTS_NR);
}

/**
 * Creates a new hash table that uses a default hashing function.
 *
 * 'equals' can be NULL. If NULL is provided, then direct address comparison
 * is used to determine key equality.
 */
HashTable *hash_table_new(EqualsFunc equals)
{
	return hash_table_new_internal(equals, hash_table_ugly_hash,
				       HASH_TABLE_MIN_SLOTS_NR);
}

/**
 * Shallow destruction of a hash table.
 *
 * The data items that were pointed to by the table are not freed (neither
 * the keys nor the values). If you want to destroy them you should call
 * first hash_table_foreach to invoke proper clean-up code.
 */

void hash_table_destroy_internal(HashTable * table)
{
	unsigned int i;
	HashTableNode *elem;

	assert(table != NULL);

	for (i = 0; i != table->slots_nr; i++) {
		while (table->slots[i] != NULL) {
			elem = table->slots[i];
			table->slots[i] = elem->next;
			free(elem);
		}
	}
}

void hash_table_destroy(HashTable * table)
{
	assert(table != NULL);

	hash_table_destroy_internal(table);
	free(table);
}

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
void hash_table_insert(HashTable * table, void *key, void *value)
{
	unsigned int hash, updated = 0;
	HashTableNode *elem;

	assert(table != NULL);
	assert(key != NULL);

	if (hash_table_must_resize(table))
		hash_table_resize(table);

	hash = table->hash(key, table->slots_nr);

	if (table->slots[hash] == NULL) {
		/* Create node */
		table->slots[hash] = malloc(sizeof(HashTableNode));
		table->slots[hash]->key = key;
		table->slots[hash]->value = value;
		table->slots[hash]->next = NULL;
	} else {
		/* Collision */

		/* if key exists, update it */
		elem = table->slots[hash];
		do {
			if (table->key_equals(elem->key, key)) {
				elem->value = value;
				updated = 1;
				break;
			}
		} while (elem->next != NULL && ((elem = elem->next) || 1));

		if (!updated) {
			/* Create and attach node, as key wasn't there */
			elem->next = malloc(sizeof(HashTableNode));
			elem->next->key = key;
			elem->next->value = value;
			elem->next->next = NULL;
		}
	}

	table->elements_count++;
}

/**
 * Removes a (key,value) pair from a table.
 *
 * If the key is unknown, the table is not modified. 'key' cannot be NULL.
 */
void hash_table_remove(HashTable * table, const void *key)
{
	unsigned int hash;
	HashTableNode *elem;
	HashTableNode *prevelem = NULL;

	assert(table != NULL);
	assert(key != NULL);

	hash = table->hash(key, table->slots_nr);

	/* key not on table, exit */
	if (table->slots[hash] == NULL)
		return;

	/* key (possibly) on table, delete */
	elem = table->slots[hash];
	do {
		if (table->key_equals(elem->key, key)) {
			/* Fix chain for deletion */
			if (prevelem == NULL)
				table->slots[hash] = elem->next;
			else
				prevelem->next = elem->next;

			/* Delete */
			free(elem);
			table->elements_count--;
			break;
		}
	} while (elem->next != NULL && ((elem = elem->next) || 1));

	/* Element wasn't on the table, nevermind */
}

/**
 * Returns the value associated with a key, if the key exists in the table.
 *
 * If the key is unknown, returns NULL. 'key' cannot be NULL.
 */
void *hash_table_lookup(HashTable * table, void *key)
{
	unsigned int hash;
	HashTableNode *elem;

	assert(table != NULL);
	assert(key != NULL);

	hash = table->hash(key, table->slots_nr);

	/* key not on table, return NULL */
	if (table->slots[hash] == NULL)
		return NULL;

	/* key (possibly) on table, compare keys */
	elem = table->slots[hash];
	do {
		if (table->key_equals(elem->key, key)) {
			return elem->value;
		}
	} while (elem->next != NULL && ((elem = elem->next) || 1));

	/* Element wasn't on the table */
	return NULL;
}

/**
 * Returns the number of (key,value) pairs in a table.
 */
size_t hash_table_size(HashTable * table)
{
	assert(table != NULL);
	return table->elements_count;
}

/**
 * Iterates over each (key,value) pair of a table applying the function 'visit'.
 *
 * There is no defined order on the elements. All of them will be visited, but
 * the exact order is not predictable based on the order of insertions/removals.
 */
void hash_table_foreach(HashTable * table, HTVisitorFunc visit,
			void *extra_data)
{
	int i = 0;
	HashTableNode *node;

	assert(table != NULL);
	assert(visit != NULL);

	for (i = table->slots_nr - 1; i != -1; i--) {
		node = table->slots[i];
		while (node != NULL) {
			visit(node->key, node->value, extra_data);
			node = node->next;
		}
	}
}

/* Resizing magic. */

static void hash_table_resize_helper(void *key, void *value, void *extra_data)
{
	hash_table_insert((HashTable *) extra_data, key, value);
}

static void hash_table_resize(HashTable * table)
{
	assert(table != NULL);

	HashTable *new =
	    hash_table_new_internal(table->key_equals, table->hash,
				    table->slots_nr * 2);

	hash_table_foreach(table, hash_table_resize_helper, new);

	/* As we're given a pointer and we should preserve it not to break
	 * user code, we use _internal which kills all of the internal
	 * elements, and then copy our new struct over the old one. Finally
	 * we free the new struct because it's no longer needed */
	hash_table_destroy_internal(table);

	*table = *new;
	free(new);
}

/* Not generic! string->int only*/
void hash_table_print_buckets(HashTable * table)
{
	int i = 0;
	HashTableNode *node;

	assert(table != NULL);

	for (i = table->slots_nr - 1; i != -1; i--) {
		node = table->slots[i];

		printf("Bucket %d: ", i);

		while (node != NULL) {
			printf("(%s, %d) ", (char *)node->key,
			       *(int *)node->value);
			node = node->next;
		}

		printf("\n");
	}
}
