#include <inttypes.h>

#ifndef __CLIST_H__
#define __CLIST_H__

/**
 * Opaque structure representing a circular list.
 */
typedef struct _CList CList;

/**
 * CList initializer value (empty list)
 */
#define CLIST_INITIALIZER		((CList *)NULL)

/**
 * Shallow destruction of a circular list
 */
void clist_destroy(CList *list);

/**
 * Inserts a value into the list.
 * Only values of less than 64 bits are supported
 */
#define clist_insert(list, data)	_clist_insert((list), (uint64_t)(data))

/**
 * Get the next element in a circular list
 */
CList *clist_next(CList *list);

/**
 * Get the value of an element in a circular list
 */
#define clist_data(list, type)		((type) _clist_data((list)))

/**
 * Internal exported symbols for linking. You should use the macros above
 * to call these functions.
 */
CList *_clist_insert(CList *list, uint64_t value);
uint64_t _clist_data(CList *list);
#endif
