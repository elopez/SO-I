#ifndef __CLIST_H__
#define __CLIST_H__

/**
 * Opaque structure representing a circular list.
 */
typedef struct _CList CList;

/**
 * CList initializer value (empty list)
 *
 */
#define CLIST_INITIALIZER	((CList *)NULL)

/**
 * Shallow destruction of a hash table.
 *
 * The data items that were pointed to by the table are not freed (neither
 * the keys nor the values). If you want to destroy them you should call
 * first hash_table_foreach to invoke proper clean-up code.
 */
void clist_destroy(CList *list);

/**
 * Inserts a value into the list
 */
CList *clist_insert(CList *list, int value);

CList *clist_next(CList *list);

int clist_data(CList *list);

#endif
